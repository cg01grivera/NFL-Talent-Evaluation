cat("
================================================================
  analyze_team_context_confirmatory_battery.R

  GOAL: Gate 7 -- the confirmatory battery (C1-C8, TEAM_CONTEXT_
  PREREGISTRATION_V2.md, post Gate-4 Team_OL_Composite retirement).
  Uses the shared, tested library (R/beat_adp_battery.R) and the fresh
  null biases cached at Gate 6 -- no parallel implementation.

  C1: Team_Rush_Stuff_Rate  x RB   (Negative, lead candidate)
  C2: Team_Sack_Rate_Allowed x QB  (Negative)
  C3: Team_Sack_Rate_Allowed x WR  (Negative)
  C4: Team_Sack_Rate_Allowed x TE  (Negative, weakest/contested)
  C5: Team_QB_Hit_Rate_Allowed x QB (Negative)
  C6: Team_QB_Hit_Rate_Allowed x WR (Negative)
  C7: Team_QB_Hit_Rate_Allowed x TE (Negative, weakest/contested)
  C8: Team_RZ_Trip_Rate x RB       (Positive)

  Deliverables (Gate 7 spec): pooled 2014-2025 bias-corrected estimates
  with direct reads + method-disagreement flags; family matrix per pool
  (|r|>=0.7); boundary-noise margin 0.005; era-split replication
  (2014-2019 vs 2020-2025, same cached bias applied to each half); C8's
  Team_Goal_To_Go_Run_Rate median-split mechanism-consistency check;
  a season-block-bootstrap sensitivity run on C1 (vs. a naive pair-level
  bootstrap that ignores team/season clustering).

  Max claim without full era-split + boundary-margin + no-disagreement-
  flag: 'directionally supported, unconfirmed' (prereg confirmation
  standard, restated here since this script is the first real read
  against it).
================================================================
\n")

if (!requireNamespace("nflreadr", quietly = TRUE)) install.packages("nflreadr", repos = "https://cloud.r-project.org")
library(dplyr)

script_arg <- grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) > 0) setwd(dirname(sub("--file=", "", script_arg)))

source("R/utils_core.R")
source("R/grading_utils.R")
source("R/fetch_player_data.R")
source("R/adp_ppg_utils.R")
source("R/fetch_team_context_data.R")
source("R/beat_adp_battery.R")

# ---- CONFIG ---------------------------------------------------
if (!exists("SEASONS_TO_TEST")) SEASONS_TO_TEST <- 2014:2025
if (!exists("ERA_SPLIT")) ERA_SPLIT <- list(exploratory = 2014:2019, confirmation = 2020:2025)
if (!exists("DECAY_R")) DECAY_R <- 0.5
if (!exists("LOOKBACK_YEARS")) LOOKBACK_YEARS <- 4
if (!exists("MIN_GAMES")) MIN_GAMES <- 8
if (!exists("N_BOOT")) N_BOOT <- 1000
if (!exists("CONF_LEVEL")) CONF_LEVEL <- 0.90
if (!exists("SIGNIFICANCE_WINDOW")) SIGNIFICANCE_WINDOW <- 12
if (!exists("BOUNDARY_NOISE_MARGIN")) BOUNDARY_NOISE_MARGIN <- 0.005
if (!exists("BIAS_PERMUTE_WITHIN")) BIAS_PERMUTE_WITHIN <- "team"
if (!exists("CACHE_PATH")) CACHE_PATH <- "output/concordance_null_bias_cache.csv"
cat("=== Config: SEASONS_TO_TEST =", paste(range(SEASONS_TO_TEST), collapse = "-"),
    "| ERA_SPLIT exploratory =", paste(range(ERA_SPLIT$exploratory), collapse = "-"),
    ", confirmation =", paste(range(ERA_SPLIT$confirmation), collapse = "-"), "\n")
cat("    DECAY_R =", DECAY_R, "| LOOKBACK_YEARS =", LOOKBACK_YEARS, "| MIN_GAMES =", MIN_GAMES,
    "| N_BOOT =", N_BOOT, "| CONF_LEVEL =", CONF_LEVEL, "| SIGNIFICANCE_WINDOW =", SIGNIFICANCE_WINDOW,
    "| BOUNDARY_NOISE_MARGIN =", BOUNDARY_NOISE_MARGIN, "| BIAS_PERMUTE_WITHIN =", BIAS_PERMUTE_WITHIN, "===\n\n")
# -----------------------------------------------------------------

adp <- load_historic_adp()
full_range <- (min(SEASONS_TO_TEST) - LOOKBACK_YEARS):max(SEASONS_TO_TEST)
message("Fetching player weekly stats for ", min(full_range), "-", max(full_range), "...")
weekly_stats_full <- nflreadr::load_player_stats(clamp_seasons(full_range), summary_level = "week")
if ("season_type" %in% names(weekly_stats_full)) weekly_stats_full <- weekly_stats_full %>% filter(season_type == "REG")
all_seasons_ppg <- compute_season_ppg(weekly_stats_full)
prior_ppg_lookup <- all_seasons_ppg %>% transmute(Season = season + 1, norm_name, position, Prior_PPG = season_ppg)

if (exists("GATE7_TEAM_CONTEXT_FULL")) {
  team_context_full <- GATE7_TEAM_CONTEXT_FULL
  cat("Reusing pre-fetched team_context_full (", nrow(team_context_full), "rows )\n\n")
} else {
  message("Fetching team-context stats for ", min(full_range), "-", max(full_range), "...")
  team_context_by_season <- list()
  for (yr in clamp_seasons(full_range)) {
    message("  ", yr, "...")
    team_context_by_season[[as.character(yr)]] <- fetch_team_context_stats(yr)
  }
  team_context_full <- do.call(rbind, team_context_by_season)
}

# Confirmatory candidate list, per position pool (prereg C1-C8)
pool_spec <- list(
  QB = list(candidates = c("Team_Sack_Rate_Allowed", "Team_QB_Hit_Rate_Allowed"), representative = "Team_Sack_Rate_Allowed"),
  RB = list(candidates = c("Team_Rush_Stuff_Rate", "Team_RZ_Trip_Rate"), representative = "Team_Rush_Stuff_Rate"),
  WR = list(candidates = c("Team_Sack_Rate_Allowed", "Team_QB_Hit_Rate_Allowed"), representative = "Team_Sack_Rate_Allowed"),
  TE = list(candidates = c("Team_Sack_Rate_Allowed", "Team_QB_Hit_Rate_Allowed"), representative = "Team_Sack_Rate_Allowed")
)
# C8's mechanism-consistency check needs the median-split variable
# attached to the RB pool specifically (not a confirmatory candidate
# itself -- prereg Rule 15).
rb_extra_candidates <- c(pool_spec$RB$candidates, "Team_Goal_To_Go_Run_Rate")

candidate_direction <- c(
  Team_Rush_Stuff_Rate    = "Negative",
  Team_Sack_Rate_Allowed  = "Negative",
  Team_QB_Hit_Rate_Allowed = "Negative",
  Team_RZ_Trip_Rate       = "Positive"
)
candidate_id <- c(
  "RB.Team_Rush_Stuff_Rate" = "C1", "QB.Team_Sack_Rate_Allowed" = "C2", "WR.Team_Sack_Rate_Allowed" = "C3",
  "TE.Team_Sack_Rate_Allowed" = "C4", "QB.Team_QB_Hit_Rate_Allowed" = "C5", "WR.Team_QB_Hit_Rate_Allowed" = "C6",
  "TE.Team_QB_Hit_Rate_Allowed" = "C7", "RB.Team_RZ_Trip_Rate" = "C8"
)

pool_data <- list()
for (pos in names(pool_spec)) {
  cands <- if (pos == "RB") rb_extra_candidates else pool_spec[[pos]]$candidates
  message("Building ", pos, " player-level data (extra_key_col='Team')...")
  d <- build_position_data(pos, adp, all_seasons_ppg, prior_ppg_lookup, team_context_full,
                            cands, SEASONS_TO_TEST, DECAY_R, LOOKBACK_YEARS, extra_key_col = "Team")
  pool_data[[pos]] <- d
  cat("  ", pos, ": ", nrow(d), " player-season rows\n", sep = "")
}

cat("\n=== Pooled 2014-2025 confirmatory battery (cached Gate 6 biases) ===\n")
pool_results <- list()
for (pos in names(pool_spec)) {
  pool_label <- paste0(pos, "_teamctx")
  message("=== ", pool_label, " ===")
  res <- run_estimation_for_position(
    pool_label, pool_data[[pos]], pool_spec[[pos]]$candidates,
    representative_candidate_col = pool_spec[[pos]]$representative,
    seasons_to_test = SEASONS_TO_TEST, min_games = MIN_GAMES, decay_r = DECAY_R, lookback_years = LOOKBACK_YEARS,
    significance_window = SIGNIFICANCE_WINDOW, n_boot = N_BOOT, conf_level = CONF_LEVEL, skip_rmse = TRUE,
    force_recompute_bias = FALSE, cache_path = CACHE_PATH, permute_within = BIAS_PERMUTE_WITHIN, team_col = "Team",
    run_coverage_spotcheck = FALSE, emit_direct_reads = TRUE
  )
  pool_results[[pos]] <- res
}

# ---- Compile the 8-row confirmatory table ----
confirmatory_rows <- list()
for (pos in names(pool_spec)) {
  df <- pool_results[[pos]]
  for (i in seq_len(nrow(df))) {
    key <- paste0(pos, ".", df$Stat[i])
    confirmatory_rows[[key]] <- data.frame(
      Candidate_Id = candidate_id[[key]], Position = pos, Stat = df$Stat[i],
      Direction_Predicted = candidate_direction[[df$Stat[i]]],
      Concordance_Point_Estimate_BiasCorrected = df$Concordance_Point_Estimate_BiasCorrected[i],
      Concordance_CI_Lower_BiasCorrected = df$Concordance_CI_Lower_BiasCorrected[i],
      Concordance_CI_Upper_BiasCorrected = df$Concordance_CI_Upper_BiasCorrected[i],
      Direct_Read_Mean_Rho = df$Direct_Read_Mean_Rho[i],
      Method_Disagreement_Flag = df$Method_Disagreement_Flag[i],
      stringsAsFactors = FALSE
    )
  }
}
confirmatory_df <- do.call(rbind, confirmatory_rows)
rownames(confirmatory_df) <- NULL
confirmatory_df <- confirmatory_df[order(confirmatory_df$Candidate_Id), ]
cat("\n=== Confirmatory table (pooled, bias-corrected, C1-C8) ===\n")
print(confirmatory_df, row.names = FALSE)

# ---- Family matrix per pool (Rule 7: |r| >= 0.7) ----
cat("\n=== Family matrix per pool ===\n")
family_rows <- list()
for (pos in names(pool_spec)) {
  fam <- compute_candidate_family_matrix(pool_data[[pos]], pool_spec[[pos]]$candidates, threshold = 0.7)
  fam_summary <- summarize_candidate_families(fam)
  fam_summary$Position <- pos
  family_rows[[pos]] <- fam_summary
  cat("  ", pos, ":\n", sep = "")
  print(fam$correlation_matrix)
}
family_df <- do.call(rbind, family_rows)
rownames(family_df) <- NULL

# ---- Era-split replication check (2014-2019 vs 2020-2025), same
# cached bias applied to each era subset ----
cat("\n=== Era-split replication check ===\n")
era_rows <- list()
for (pos in names(pool_spec)) {
  null_bias <- attr(pool_results[[pos]], "null_bias")
  for (cand in pool_spec[[pos]]$candidates) {
    for (era_name in names(ERA_SPLIT)) {
      era_data <- pool_data[[pos]] %>% filter(Season %in% ERA_SPLIT[[era_name]])
      ci <- run_concordance_bootstrap(era_data, cand, significance_window = SIGNIFICANCE_WINDOW,
                                       n_boot = N_BOOT, conf_level = CONF_LEVEL)
      era_rows[[paste(pos, cand, era_name)]] <- data.frame(
        Position = pos, Stat = cand, Era = era_name,
        Point_Estimate_BiasCorrected = round(unname(ci["Point_Estimate"]) - null_bias, 4),
        N_Seasons = unname(ci["N_Seasons"]), stringsAsFactors = FALSE
      )
    }
  }
}
era_df <- do.call(rbind, era_rows)
rownames(era_df) <- NULL
print(era_df, row.names = FALSE)

era_check <- era_df %>%
  group_by(Position, Stat) %>%
  summarise(
    Exploratory_Est = Point_Estimate_BiasCorrected[Era == "exploratory"],
    Confirmation_Est = Point_Estimate_BiasCorrected[Era == "confirmation"],
    .groups = "drop"
  ) %>%
  # "Replication" for the confirmation standard means the POSITIVE
  # concordance finding (real incremental value) shows up in BOTH eras,
  # not merely "both eras happen to agree on some sign" -- a candidate
  # that's negative in both eras is consistently showing NO value, not
  # replicating a confirmed effect.
  mutate(Both_Eras_Positive = Exploratory_Est > 0 & Confirmation_Est > 0)
cat("\n--- Era-split check (does the POSITIVE concordance finding replicate in both eras?) ---\n")
print(era_check, row.names = FALSE)

# ---- C8 mechanism-consistency check: Team_Goal_To_Go_Run_Rate median split ----
cat("\n=== C8 mechanism-consistency check (Team_Goal_To_Go_Run_Rate median split) ===\n")
rb_data <- pool_data$RB
rb_median <- median(rb_data$Team_Goal_To_Go_Run_Rate, na.rm = TRUE)
cat("  Median trailing Team_Goal_To_Go_Run_Rate:", round(rb_median, 4), "\n")
rb_run_heavy <- rb_data %>% filter(Team_Goal_To_Go_Run_Rate >= rb_median)
rb_pass_heavy <- rb_data %>% filter(Team_Goal_To_Go_Run_Rate < rb_median)
cat("  Run-heavy half:", nrow(rb_run_heavy), "rows | Pass-heavy half:", nrow(rb_pass_heavy), "rows\n")
null_bias_rb <- attr(pool_results$RB, "null_bias")
ci_run_heavy <- run_concordance_bootstrap(rb_run_heavy, "Team_RZ_Trip_Rate", significance_window = SIGNIFICANCE_WINDOW, n_boot = N_BOOT, conf_level = CONF_LEVEL)
ci_pass_heavy <- run_concordance_bootstrap(rb_pass_heavy, "Team_RZ_Trip_Rate", significance_window = SIGNIFICANCE_WINDOW, n_boot = N_BOOT, conf_level = CONF_LEVEL)
c8_run_heavy_corrected <- unname(ci_run_heavy["Point_Estimate"]) - null_bias_rb
c8_pass_heavy_corrected <- unname(ci_pass_heavy["Point_Estimate"]) - null_bias_rb
cat(sprintf("  Run-heavy half:  bias-corrected point estimate = %.4f\n", c8_run_heavy_corrected))
cat(sprintf("  Pass-heavy half: bias-corrected point estimate = %.4f\n", c8_pass_heavy_corrected))
mechanism_consistent <- c8_run_heavy_corrected > c8_pass_heavy_corrected
cat("  Mechanism-consistent (effect concentrates in run-heavy half):", mechanism_consistent, "\n")
cat("  NOTE: this check can only QUALIFY C8, never upgrade it, per prereg Rule/C8 text.\n")

# ---- C1 season-block-bootstrap sensitivity run (vs. a naive pair-level
# bootstrap that ignores team/season clustering entirely) ----
cat("\n=== C1 season-block-bootstrap sensitivity (Team_Rush_Stuff_Rate x RB) ===\n")
naive_pair_bootstrap <- function(pairs_df, n_boot = 2000, conf_level = 0.90) {
  # NAIVE, deliberately-wrong-on-purpose comparison: resamples INDIVIDUAL
  # PAIRS with replacement, ignoring season/team clustering entirely --
  # NOT this project's real method (bootstrap_concordance_effect(),
  # which resamples whole SEASON BLOCKS). Built only to show, as a
  # labeled robustness footnote, how much narrower (anti-conservative) a
  # naive pair-level bootstrap would look on a team-clustered candidate.
  n_pairs <- nrow(pairs_df)
  boot_estimates <- replicate(n_boot, {
    idx <- sample(n_pairs, n_pairs, replace = TRUE)
    resampled <- pairs_df[idx, ]
    mean(resampled$modelb_correct) - mean(resampled$modela_correct)
  })
  point_estimate <- mean(pairs_df$modelb_correct) - mean(pairs_df$modela_correct)
  alpha <- 1 - conf_level
  ci <- quantile(boot_estimates, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
  c(Point_Estimate = round(point_estimate, 4), CI_Lower = round(unname(ci[1]), 4), CI_Upper = round(unname(ci[2]), 4))
}
c1_conc <- run_concordance(pool_data$RB, "Team_Rush_Stuff_Rate", significance_window = SIGNIFICANCE_WINDOW, return_pairs = TRUE)
c1_standard <- bootstrap_concordance_effect(c1_conc$pairs, n_boot = N_BOOT, conf_level = CONF_LEVEL)
c1_naive <- naive_pair_bootstrap(c1_conc$pairs, n_boot = N_BOOT, conf_level = CONF_LEVEL)
cat(sprintf("  Standard (season-block):  point=%.4f  CI=[%.4f, %.4f]  width=%.4f\n",
            c1_standard["Point_Estimate"], c1_standard["CI_Lower"], c1_standard["CI_Upper"],
            c1_standard["CI_Upper"] - c1_standard["CI_Lower"]))
cat(sprintf("  Naive (pair-level, WRONG on purpose): point=%.4f  CI=[%.4f, %.4f]  width=%.4f\n",
            c1_naive["Point_Estimate"], c1_naive["CI_Lower"], c1_naive["CI_Upper"],
            c1_naive["CI_Upper"] - c1_naive["CI_Lower"]))
width_ratio <- (c1_naive["CI_Upper"] - c1_naive["CI_Lower"]) / (c1_standard["CI_Upper"] - c1_standard["CI_Lower"])
cat(sprintf("  Naive/Standard CI width ratio: %.3f (%s)\n", width_ratio,
            if (width_ratio < 1) "naive is NARROWER -- confirms season-block bootstrap is the more conservative, correct choice for a team-clustered candidate" else "naive is not narrower in this instance"))

# ---- Final classification against the prereg confirmation standard ----
# SIGN CONVENTION, stated explicitly (a real correction made while writing
# this section -- worth recording since it's easy to get backwards):
# Concordance_Point_Estimate measures whether ADDING the candidate improves
# pairwise ranking accuracy (model_b vs model_a) -- a MAGNITUDE/utility
# question. Because model_b's own fitted coefficient absorbs whatever sign
# the true relationship has, a genuinely informative candidate is expected
# to show POSITIVE bias-corrected concordance REGARDLESS of whether its raw
# correlation with the outcome is positive or negative. So:
#   (a) "excludes zero" checks the CONCORDANCE CI is POSITIVE by the margin
#       (a real, useful signal exists at all) -- not compared against the
#       predicted direction's sign.
#   (b) "predicted direction" is checked against Direct_Read_Mean_Rho (the
#       machinery-free per-season Spearman read), which DOES carry the raw
#       relationship's actual sign -- NOT against concordance's sign.
cat("\n=== Final read against the prereg confirmation standard ===\n")
cat("(a) bias-corrected 90% concordance CI is POSITIVE by >=", BOUNDARY_NOISE_MARGIN, "(real incremental value)\n")
cat("(b) Direct_Read_Mean_Rho sign matches the predicted direction\n")
cat("(c) era-split replication (positive concordance in BOTH eras) | (d) no unresolved method-disagreement flag\n\n")

classify_status <- function(concordance_point_est, concordance_ci_lower, concordance_ci_upper, direct_read_rho,
                             predicted_direction, boundary_margin, both_eras_positive, disagreement_flag) {
  predicted_num <- if (predicted_direction == "Positive") 1 else -1
  direction_match <- !is.na(direct_read_rho) && sign(direct_read_rho) == predicted_num
  excludes_zero_margin <- (concordance_ci_lower - boundary_margin) > 0
  # A CI entirely BELOW zero is a distinct, notable finding (the candidate
  # appears to genuinely HURT ranking accuracy) -- not the same as an
  # inconclusive straddle-zero null, and reported as such rather than
  # collapsed into "null".
  entirely_negative <- concordance_ci_upper < 0
  if (excludes_zero_margin && direction_match && isTRUE(both_eras_positive) && !disagreement_flag) return("CONFIRMED")
  if (excludes_zero_margin && !direction_match) return("CONTRADICTED (direct-read direction opposite prediction, despite real concordance value)")
  if (excludes_zero_margin) return("directionally supported, unconfirmed")
  if (entirely_negative) return("concordance CI entirely negative (candidate appears to hurt ranking accuracy)")
  return("null (CI straddles zero)")
}

final_rows <- list()
for (i in seq_len(nrow(confirmatory_df))) {
  row <- confirmatory_df[i, ]
  era_row <- era_check[era_check$Position == row$Position & era_check$Stat == row$Stat, ]
  status <- classify_status(
    row$Concordance_Point_Estimate_BiasCorrected, row$Concordance_CI_Lower_BiasCorrected,
    row$Concordance_CI_Upper_BiasCorrected, row$Direct_Read_Mean_Rho, row$Direction_Predicted, BOUNDARY_NOISE_MARGIN,
    era_row$Both_Eras_Positive, as.logical(row$Method_Disagreement_Flag)
  )
  final_rows[[i]] <- data.frame(Candidate_Id = row$Candidate_Id, Position = row$Position, Stat = row$Stat,
                                 Concordance_CI_Lower = row$Concordance_CI_Lower_BiasCorrected,
                                 Concordance_CI_Upper = row$Concordance_CI_Upper_BiasCorrected,
                                 Direct_Read_Mean_Rho = row$Direct_Read_Mean_Rho,
                                 Method_Disagreement_Flag = row$Method_Disagreement_Flag,
                                 Status = status, stringsAsFactors = FALSE)
}
final_df <- do.call(rbind, final_rows)
print(final_df, row.names = FALSE)

# ---- Explicit cross-check: does the Gate 6 QB/TE under-coverage flag
# show up as a pattern in these findings? ----
cat("\n=== Cross-check against Gate 6 coverage flags ===\n")
cat("QB_teamctx and TE_teamctx were flagged for below-nominal coverage at Gate 6\n")
cat("(0.780 vs 0.90 nominal, both p=0.009) -- RB_teamctx and WR_teamctx were not.\n\n")
final_df$CI_Width <- round(final_df$Concordance_CI_Upper - final_df$Concordance_CI_Lower, 4)
final_df$Pool_Flagged_At_Gate6 <- final_df$Position %in% c("QB", "TE")
print(final_df %>% select(Candidate_Id, Position, Stat, CI_Width, Status, Pool_Flagged_At_Gate6), row.names = FALSE)

dir.create("output", showWarnings = FALSE)
readr::write_csv(confirmatory_df, "output/analyze_team_context_confirmatory_battery_results.csv")
readr::write_csv(era_df, "output/analyze_team_context_confirmatory_battery_era_split.csv")
readr::write_csv(family_df, "output/analyze_team_context_confirmatory_battery_family_matrix.csv")
readr::write_csv(final_df, "output/analyze_team_context_confirmatory_battery_final_status.csv")
cat("\nWritten: output/analyze_team_context_confirmatory_battery_{results,era_split,family_matrix,final_status}.csv\n")
