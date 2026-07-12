cat("
================================================================
  analyze_team_context_beat_adp.R

  GOAL: Full Beat-ADP battery for the 19 team-context stats that
  survived the persistence screen (analyze_team_context_decay_rate.R),
  tested against ALL FOUR positions -- a team-context stat can matter
  for a position it wasn't obviously 'about' (this project's own
  precedent: QB_Rush_Stanine, tested only at the team level in the
  original project, showed a hint of spillover into RB Beat-ADP). Every
  stat x position combination gets the full three-test battery, not
  just the position it seems most relevant to.

  METHOD: Nested regression (season_ppg ~ RANK + Prior_PPG [+
  candidate]), bust/breakout classification, and pairwise concordance
  among similarly-drafted players -- each via leave-one-season-out
  cross-validation. Team-context stats are decay-weighted per TEAM
  (not per player) through data available as of yr-1, then broadcast
  to every player on that team's roster for the target season -- same
  join-by-team pattern the original team-context project used
  throughout. BH-FDR correction applied across ALL stat x position
  combinations together in one family (same convention as that
  project's analyze_full_battery.R), not scoped per position.

  Uses the ADP file as already team-corrected by apply_team_
  correction_to_adp_file.R -- no need to re-run team correction here,
  Team is already accurate in the loaded ADP pool.

  PARAMETERS IN THIS RUN:
    SEASONS_TO_TEST      -- which target seasons get scored (2013-2025 default)
    DECAY_R              -- exponential decay rate weighting a team's
                             trailing seasons (0.5 default, project-wide)
    LOOKBACK_YEARS        -- how many trailing seasons feed the decay window (4)
    MIN_GAMES             -- games-played floor for a player-season to count (8)
    PAIR_RANK_WINDOWS      -- ADP-pick-distance windows tested for concordance
    QUANTILE_CUTOFF        -- top/bottom share defining breakout/bust (0.25)
================================================================
\n")

if (!requireNamespace("nflreadr", quietly = TRUE)) install.packages("nflreadr", repos = "https://cloud.r-project.org")
library(dplyr)

script_arg <- grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) > 0) setwd(dirname(sub("--file=", "", script_arg)))

source("R/utils_core.R")
source("R/grading_utils.R")
source("R/fetch_player_data.R")
source("R/fetch_team_context_data.R")
source("R/adp_ppg_utils.R")

# ---- CONFIG ---------------------------------------------------
if (!exists("SEASONS_TO_TEST")) SEASONS_TO_TEST <- 2013:2025
if (!exists("DECAY_R")) DECAY_R <- 0.5
if (!exists("LOOKBACK_YEARS")) LOOKBACK_YEARS <- 4
if (!exists("MIN_GAMES")) MIN_GAMES <- 8
if (!exists("PAIR_RANK_WINDOWS")) PAIR_RANK_WINDOWS <- c(6, 12, 18, 24)
if (!exists("QUANTILE_CUTOFF")) QUANTILE_CUTOFF <- 0.25
cat("=== Config: SEASONS_TO_TEST =", paste(range(SEASONS_TO_TEST), collapse="-"),
    "| DECAY_R =", DECAY_R, "| LOOKBACK_YEARS =", LOOKBACK_YEARS,
    "| MIN_GAMES =", MIN_GAMES,
    "| PAIR_RANK_WINDOWS =", paste(PAIR_RANK_WINDOWS, collapse=", "),
    "| QUANTILE_CUTOFF =", QUANTILE_CUTOFF, "===\n")
cat("NOTE: these persist in your R session once set. If a value above wasn't\n")
cat("intended, clear it first: rm(SEASONS_TO_TEST, DECAY_R, LOOKBACK_YEARS, MIN_GAMES, PAIR_RANK_WINDOWS, QUANTILE_CUTOFF)\n\n")
# -----------------------------------------------------------------

compute_auc <- function(scores, actual_binary) {
  n_pos <- sum(actual_binary == 1); n_neg <- sum(actual_binary == 0)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  r <- rank(scores)
  (sum(r[actual_binary == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}
paired_wilcox_p <- function(x, y) {
  d <- x - y; d <- d[!is.na(d)]
  if (length(d) < 4 || all(d == 0)) return(NA_real_)
  tryCatch(suppressWarnings(wilcox.test(x, y, paired = TRUE)$p.value), error = function(e) NA_real_)
}

adp <- load_historic_adp()
full_range <- (min(SEASONS_TO_TEST) - LOOKBACK_YEARS):max(SEASONS_TO_TEST)
message("Fetching player weekly stats for ", min(full_range), "-", max(full_range), "...")
weekly_stats_full <- nflreadr::load_player_stats(clamp_seasons(full_range), summary_level = "week")
if ("season_type" %in% names(weekly_stats_full)) weekly_stats_full <- weekly_stats_full %>% filter(season_type == "REG")
all_seasons_ppg <- compute_season_ppg(weekly_stats_full)

message("Building team-context stats for ", min(full_range), "-", max(full_range),
        " -- one full pbp pull per season, expect this to take a while...")
team_context_by_season <- list()
for (yr in clamp_seasons(full_range)) {
  message("  ", yr, "...")
  team_context_by_season[[as.character(yr)]] <- fetch_team_context_stats(yr)
}
team_context_full <- do.call(rbind, team_context_by_season)

# Only the 19 stats that survived the persistence screen -- excluded:
# Team_Avg_Starting_Field_Pos, Team_Weekly_Points_Variance,
# Team_Weekly_EPA_Variance (all near-zero persistence, in the same
# range as the already-excluded Turnover Margin).
team_context_candidates <- c(
  "Team_4th_Down_Aggressiveness", "Team_TE_Target_Share", "Team_RB_Target_Share",
  "Team_Avg_Plays_Per_Drive", "Team_WR_Target_Share", "Team_aDOT", "Team_PROE",
  "Team_CPOE", "Team_Scoring_Drive_Rate", "Team_TD_Drive_Rate", "Team_EPA_Per_Dropback",
  "Team_Trailing_Snap_Pct", "Team_RZ_Trip_Rate", "Team_Leading_Snap_Pct",
  "Team_Raw_Plays_PG", "Team_Three_And_Out_Rate", "Team_Deep_Attempt_Pct",
  "Team_Deep_Completion_Pct", "Team_OL_Penalty_Rate"
)

prior_ppg_lookup <- all_seasons_ppg %>%
  transmute(Season = season + 1, norm_name, position, Prior_PPG = season_ppg)

#' Build player-level rows for one position, with all team-context
#' candidates decay-weighted PER TEAM (not per player) through data
#' available as of yr-1, then broadcast to every player on that team's
#' roster that season -- same join-by-team pattern used throughout the
#' original team-context project.
build_position_data <- function(pos) {
  rows_by_season <- list()
  for (yr in SEASONS_TO_TEST) {
    end_year <- yr - 1
    this_season_ppg <- all_seasons_ppg %>% filter(season == yr)
    pool <- top150_pool(adp, yr) %>% filter(Pos == pos)
    # Team is already historically accurate -- the loaded ADP file has
    # already been through the multi-tier correction process.
    actual <- this_season_ppg %>% filter(position == pos) %>% select(norm_name, season_ppg)
    prior  <- prior_ppg_lookup %>% filter(Season == yr, position == pos) %>% select(norm_name, Prior_PPG)

    merged <- pool %>%
      inner_join(actual, by = "norm_name") %>%
      left_join(prior, by = "norm_name") %>%
      filter(!is.na(Prior_PPG), !is.na(Team))

    if (nrow(merged) < 5) next

    team_vals <- as.data.frame(setNames(
      lapply(team_context_candidates, function(col) {
        decay_weighted_avg_vec(team_context_full, "Team", col, merged$Team, end_year,
                                year_col = "Season", r = DECAY_R, lookback = LOOKBACK_YEARS)
      }),
      team_context_candidates
    ))
    team_vals$Team <- merged$Team
    # Team isn't a unique key across rows (many players share a team),
    # so join by row position instead of a key -- team_vals was built
    # in the exact same row order as merged.
    merged <- cbind(merged, team_vals[, team_context_candidates, drop = FALSE])
    merged$Season <- yr
    rows_by_season[[as.character(yr)]] <- merged
  }
  do.call(rbind, rows_by_season)
}

run_rmse_cv <- function(data, candidate_col) {
  seasons_present <- sort(unique(data$Season))
  results <- lapply(seasons_present, function(held_out) {
    train <- data %>% filter(Season != held_out, !is.na(.data[[candidate_col]]))
    test  <- data %>% filter(Season == held_out, !is.na(.data[[candidate_col]]))
    if (nrow(train) < 15 || nrow(test) < 5) return(NULL)
    fmla_b <- as.formula(paste("season_ppg ~ RANK + Prior_PPG +", candidate_col))
    model_a <- lm(season_ppg ~ RANK + Prior_PPG, data = train)
    model_b <- lm(fmla_b, data = train)
    rmse_a <- sqrt(mean((test$season_ppg - predict(model_a, test))^2, na.rm = TRUE))
    rmse_b <- sqrt(mean((test$season_ppg - predict(model_b, test))^2, na.rm = TRUE))
    data.frame(RMSE_A = rmse_a, RMSE_B = rmse_b)
  })
  results <- do.call(rbind, results[!sapply(results, is.null)])
  if (is.null(results) || nrow(results) < 4) return(c(RMSE_Improvement_Pct = NA_real_, RMSE_Folds_Won = NA_integer_, RMSE_P = NA_real_))
  c(RMSE_Improvement_Pct = round(100 * mean((results$RMSE_A - results$RMSE_B) / results$RMSE_A), 2),
    RMSE_Folds_Won = sum(results$RMSE_B < results$RMSE_A),
    RMSE_P = paired_wilcox_p(results$RMSE_A, results$RMSE_B))
}

run_auc_cv <- function(data, candidate_col, target_col) {
  seasons_present <- sort(unique(data$Season))
  results <- lapply(seasons_present, function(held_out) {
    train <- data %>% filter(Season != held_out, !is.na(.data[[candidate_col]]))
    test  <- data %>% filter(Season == held_out, !is.na(.data[[candidate_col]]))
    if (nrow(train) < 15 || nrow(test) < 5 || length(unique(train[[target_col]])) < 2) return(NULL)
    fmla_b <- as.formula(paste(target_col, "~ RANK + Prior_PPG +", candidate_col))
    model_b <- glm(fmla_b, data = train, family = binomial)
    pred_b <- suppressWarnings(predict(model_b, newdata = test, type = "response"))
    model_a <- glm(as.formula(paste(target_col, "~ RANK + Prior_PPG")), data = train, family = binomial)
    pred_a <- suppressWarnings(predict(model_a, newdata = test, type = "response"))
    data.frame(AUC_A = compute_auc(pred_a, test[[target_col]]), AUC_B = compute_auc(pred_b, test[[target_col]]))
  })
  results <- do.call(rbind, results[!sapply(results, is.null)])
  if (is.null(results) || nrow(results) < 4) return(c(AUC_B = NA_real_, AUC_Folds_Won = NA_integer_, AUC_P = NA_real_))
  c(AUC_B = round(mean(results$AUC_B, na.rm = TRUE), 3),
    AUC_Folds_Won = sum(results$AUC_B > results$AUC_A, na.rm = TRUE),
    AUC_P = paired_wilcox_p(results$AUC_A, results$AUC_B))
}

run_concordance <- function(data, candidate_col) {
  data <- data %>% filter(!is.na(.data[[candidate_col]]))
  seasons_present <- sort(unique(data$Season))
  if (nrow(data) < 15 || length(seasons_present) < 4) {
    out <- setNames(rep(NA_real_, length(PAIR_RANK_WINDOWS)), paste0("Concordance_Diff_W", PAIR_RANK_WINDOWS))
    return(c(out, Concordance_W12_P = NA_real_))
  }
  data$pred_b <- NA_real_
  fmla_b <- as.formula(paste("season_ppg ~ RANK + Prior_PPG +", candidate_col))
  for (held_out in seasons_present) {
    train <- data %>% filter(Season != held_out)
    if (nrow(train) < 15) next
    model_b <- lm(fmla_b, data = train)
    idx <- which(data$Season == held_out)
    data$pred_b[idx] <- predict(model_b, newdata = data[idx, ])
  }
  all_pairs <- lapply(seasons_present, function(yr) {
    sp <- data %>% filter(Season == yr)
    n <- nrow(sp)
    if (n < 2) return(NULL)
    pairs <- combn(n, 2)
    do.call(rbind, lapply(seq_len(ncol(pairs)), function(k) {
      i <- pairs[1, k]; j <- pairs[2, k]
      p1 <- sp[i, ]; p2 <- sp[j, ]
      if (is.na(p1$pred_b) || is.na(p2$pred_b)) return(NULL)
      data.frame(Season = yr, rank_diff = abs(p1$RANK - p2$RANK),
                 adp_correct    = (p1$RANK < p2$RANK)     == (p1$season_ppg > p2$season_ppg),
                 modelb_correct = (p1$pred_b > p2$pred_b) == (p1$season_ppg > p2$season_ppg))
    }))
  })
  all_pairs_df <- do.call(rbind, all_pairs[!sapply(all_pairs, is.null)])
  if (is.null(all_pairs_df)) {
    out <- setNames(rep(NA_real_, length(PAIR_RANK_WINDOWS)), paste0("Concordance_Diff_W", PAIR_RANK_WINDOWS))
    return(c(out, Concordance_W12_P = NA_real_))
  }
  out <- sapply(PAIR_RANK_WINDOWS, function(w) {
    sub <- all_pairs_df %>% filter(rank_diff <= w)
    round(mean(sub$modelb_correct) - mean(sub$adp_correct), 4)
  })
  names(out) <- paste0("Concordance_Diff_W", PAIR_RANK_WINDOWS)
  w12 <- all_pairs_df %>% filter(rank_diff <= 12)
  season_rates <- w12 %>% group_by(Season) %>% summarise(adp_rate = mean(adp_correct), modelb_rate = mean(modelb_correct), .groups = "drop")
  p_val <- if (nrow(season_rates) >= 4) paired_wilcox_p(season_rates$adp_rate, season_rates$modelb_rate) else NA_real_
  c(out, Concordance_W12_P = p_val)
}

message("Running the full battery: ", length(team_context_candidates), " stats x 4 positions...")
summary_rows <- list()
for (pos in c("QB", "RB", "WR", "TE")) {
  message("Building ", pos, " player-level data...")
  pdata <- build_position_data(pos)
  pdata <- pdata %>%
    group_by(Season) %>%
    mutate(
      expected_ppg = predict(lm(season_ppg ~ RANK + Prior_PPG)),
      residual = season_ppg - expected_ppg,
      is_breakout = as.integer(residual >= quantile(residual, 1 - QUANTILE_CUTOFF)),
      is_bust     = as.integer(residual <= quantile(residual, QUANTILE_CUTOFF))
    ) %>%
    ungroup()
  cat("  ", pos, ": ", nrow(pdata), " player-season rows\n", sep = "")

  for (stat in team_context_candidates) {
    message("  ", pos, " x ", stat, "...")
    rmse_res <- run_rmse_cv(pdata, stat)
    breakout_res <- run_auc_cv(pdata, stat, "is_breakout")
    bust_res <- run_auc_cv(pdata, stat, "is_bust")
    conc_res <- run_concordance(pdata, stat)
    summary_rows[[paste(pos, stat)]] <- c(
      Position = pos, Stat = stat, N_Rows = nrow(pdata),
      RMSE_Improvement_Pct = unname(rmse_res["RMSE_Improvement_Pct"]),
      RMSE_Folds_Won = unname(rmse_res["RMSE_Folds_Won"]),
      RMSE_P = unname(rmse_res["RMSE_P"]),
      Breakout_AUC_B = unname(breakout_res["AUC_B"]),
      Bust_AUC_B = unname(bust_res["AUC_B"]),
      Bust_AUC_P = unname(bust_res["AUC_P"]),
      conc_res
    )
  }
}
summary_df <- as.data.frame(do.call(rbind, summary_rows), stringsAsFactors = FALSE)
rownames(summary_df) <- NULL
numeric_cols <- setdiff(names(summary_df), c("Position", "Stat"))
summary_df[numeric_cols] <- lapply(summary_df[numeric_cols], function(x) as.numeric(as.character(x)))

# BH-FDR across ALL stat x position combinations together, same
# convention as the original project's analyze_full_battery.R.
summary_df$RMSE_Q <- p.adjust(summary_df$RMSE_P, method = "BH")
summary_df$Bust_AUC_Q <- p.adjust(summary_df$Bust_AUC_P, method = "BH")
summary_df$Concordance_W12_Q <- p.adjust(summary_df$Concordance_W12_P, method = "BH")

cat("\n=== TEAM-CONTEXT BATTERY: ", length(team_context_candidates), " stats x 4 positions = ",
    nrow(summary_df), " combinations ===\n", sep = "")
cat("(BH-FDR correction applied across ALL combinations together. Q < 0.10 reasonable, Q < 0.05 stronger.)\n\n")
print(summary_df, row.names = FALSE)

cat("\n=== Combinations surviving Q < 0.10 on ANY metric ===\n")
survivors <- summary_df %>% filter(RMSE_Q < 0.10 | Bust_AUC_Q < 0.10 | Concordance_W12_Q < 0.10)
if (nrow(survivors) > 0) {
  print(survivors %>% select(Position, Stat, RMSE_Improvement_Pct, RMSE_Q, Bust_AUC_B, Bust_AUC_Q,
                              Concordance_Diff_W12, Concordance_W12_Q), row.names = FALSE)
} else {
  cat("None survive at Q < 0.10.\n")
}

dir.create("output", showWarnings = FALSE)
readr::write_csv(summary_df, "output/analyze_team_context_beat_adp_results.csv")
cat("\nFull results written to output/analyze_team_context_beat_adp_results.csv\n")
