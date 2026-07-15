cat("
================================================================
  analyze_team_context_exploratory_screen.R

  GOAL: Gate 8 (TEAM_CONTEXT_REWORK_PLAN_V2.md Section 5) -- the wide
  exploratory net, screened on 2014-2019 ONLY (never the 2020-2025
  confirmation era, and never the confirmatory candidates' own already-
  spent 2014-2025 read from Gate 7). Same machinery as the confirmatory
  battery (shared assembly, team-permuted nulls, family matrix, direct
  reads, boundary margin) -- NOT a parallel implementation.

  OUTPUT IS RANKED EFFECT SIZES BY FAMILY ONLY. No 'significant', no
  'confirmed', no directional claims beyond the numbers themselves.
  Graduation to the 2020-2025 confirmation era requires a NEW,
  separately committed prereg entry per candidate -- this script
  presents candidates and stops; it does not graduate anything itself.

  CANDIDATE LIST (26 stats, all currently-fetched team-context columns
  EXCEPT Team_RZ_Trip_Rate [already confirmatory C8] and Team_Goal_To_
  Go_Run_Rate [constructed for the C8 median-split check only, prereg
  Rule 15 -- not its own candidate]), each x {QB, RB, WR, TE} = 104
  combinations. NOTE on the plan's own '19 Phase-1 stats' count: this
  script uses the concrete, verifiable set of 26 columns actually
  produced by fetch_team_context_stats() today, since the plan's prose
  count doesn't fully reconcile against that live list by exactly 1
  stat and guessing at the discrepancy would be worse than using the
  real, current column set.
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
if (!exists("SEASONS_TO_TEST")) SEASONS_TO_TEST <- 2014:2019   # exploratory era ONLY
if (!exists("DECAY_R")) DECAY_R <- 0.5
if (!exists("LOOKBACK_YEARS")) LOOKBACK_YEARS <- 4
if (!exists("MIN_GAMES")) MIN_GAMES <- 8
if (!exists("N_BOOT")) N_BOOT <- 1000
if (!exists("CONF_LEVEL")) CONF_LEVEL <- 0.90
if (!exists("SIGNIFICANCE_WINDOW")) SIGNIFICANCE_WINDOW <- 12
if (!exists("BOUNDARY_NOISE_MARGIN")) BOUNDARY_NOISE_MARGIN <- 0.005
if (!exists("BIAS_N_META_REPLICATES")) BIAS_N_META_REPLICATES <- 5
if (!exists("BIAS_N_REPLICATES")) BIAS_N_REPLICATES <- 100
if (!exists("BIAS_N_BOOT")) BIAS_N_BOOT <- 500
if (!exists("BIAS_PERMUTE_WITHIN")) BIAS_PERMUTE_WITHIN <- "team"
if (!exists("CACHE_PATH")) CACHE_PATH <- "output/concordance_null_bias_cache.csv"
cat("=== Config: SEASONS_TO_TEST (EXPLORATORY ERA ONLY) =", paste(range(SEASONS_TO_TEST), collapse = "-"), "\n")
cat("    DECAY_R =", DECAY_R, "| LOOKBACK_YEARS =", LOOKBACK_YEARS, "| MIN_GAMES =", MIN_GAMES, "| N_BOOT =", N_BOOT, "\n")
cat("    BIAS_N_META_REPLICATES =", BIAS_N_META_REPLICATES, "| BIAS_N_REPLICATES =", BIAS_N_REPLICATES,
    "| BIAS_N_BOOT =", BIAS_N_BOOT, "| BIAS_PERMUTE_WITHIN =", BIAS_PERMUTE_WITHIN, "===\n\n")
# -----------------------------------------------------------------

adp <- load_historic_adp()
full_range <- (min(SEASONS_TO_TEST) - LOOKBACK_YEARS):max(SEASONS_TO_TEST)
message("Fetching player weekly stats for ", min(full_range), "-", max(full_range), "...")
weekly_stats_full <- nflreadr::load_player_stats(clamp_seasons(full_range), summary_level = "week")
if ("season_type" %in% names(weekly_stats_full)) weekly_stats_full <- weekly_stats_full %>% filter(season_type == "REG")
all_seasons_ppg <- compute_season_ppg(weekly_stats_full)
prior_ppg_lookup <- all_seasons_ppg %>% transmute(Season = season + 1, norm_name, position, Prior_PPG = season_ppg)

if (exists("GATE8_TEAM_CONTEXT_FULL")) {
  team_context_full <- GATE8_TEAM_CONTEXT_FULL
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

all_team_context_cols <- setdiff(names(team_context_full), c("Team", "Season"))
exploratory_candidates <- setdiff(all_team_context_cols, c("Team_RZ_Trip_Rate", "Team_Goal_To_Go_Run_Rate"))
cat("Exploratory candidates (", length(exploratory_candidates), "):\n", sep = "")
cat(paste(" -", exploratory_candidates, collapse = "\n"), "\n\n")

representative_candidate <- list(QB = "Team_Sack_Rate_Allowed", RB = "Team_Rush_Stuff_Rate",
                                  WR = "Team_Sack_Rate_Allowed", TE = "Team_Sack_Rate_Allowed")

pool_data <- list()
for (pos in c("QB", "RB", "WR", "TE")) {
  message("Building ", pos, " player-level data (extra_key_col='Team')...")
  d <- build_position_data(pos, adp, all_seasons_ppg, prior_ppg_lookup, team_context_full,
                            exploratory_candidates, SEASONS_TO_TEST, DECAY_R, LOOKBACK_YEARS, extra_key_col = "Team")
  pool_data[[pos]] <- d
  cat("  ", pos, ": ", nrow(d), " player-season rows\n", sep = "")
}

cat("\n=== Running the exploratory battery: ", length(exploratory_candidates), " stats x 4 positions = ",
    length(exploratory_candidates) * 4, " combinations (2014-2019 ONLY) ===\n", sep = "")
pool_results <- list()
for (pos in c("QB", "RB", "WR", "TE")) {
  pool_label <- paste0(pos, "_teamctx_exploratory")
  message("=== ", pool_label, " ===")
  res <- run_estimation_for_position(
    pool_label, pool_data[[pos]], exploratory_candidates, representative_candidate_col = representative_candidate[[pos]],
    seasons_to_test = SEASONS_TO_TEST, min_games = MIN_GAMES, decay_r = DECAY_R, lookback_years = LOOKBACK_YEARS,
    significance_window = SIGNIFICANCE_WINDOW, n_boot = N_BOOT, conf_level = CONF_LEVEL, skip_rmse = TRUE,
    force_recompute_bias = FALSE,
    bias_n_meta_replicates = BIAS_N_META_REPLICATES, bias_n_replicates = BIAS_N_REPLICATES, bias_n_boot = BIAS_N_BOOT,
    cache_path = CACHE_PATH, permute_within = BIAS_PERMUTE_WITHIN, team_col = "Team",
    run_coverage_spotcheck = FALSE, emit_direct_reads = TRUE
  )
  res$Position <- pos
  pool_results[[pos]] <- res
}

# ---- Family matrix per pool (Rule 7: |r| >= 0.7) ----
cat("\n=== Family matrix per pool ===\n")
family_rows <- list()
for (pos in c("QB", "RB", "WR", "TE")) {
  fam <- compute_candidate_family_matrix(pool_data[[pos]], exploratory_candidates, threshold = 0.7)
  fam_summary <- summarize_candidate_families(fam)
  fam_summary$Position <- pos
  family_rows[[pos]] <- fam_summary
}
family_df <- do.call(rbind, family_rows)
rownames(family_df) <- NULL

# ---- Compile + rank -- EFFECT SIZES ONLY, no confirmatory language ----
all_results <- do.call(rbind, pool_results)
rownames(all_results) <- NULL
all_results <- all_results %>%
  left_join(family_df %>% select(Position, Stat, Family_Id, Family_Size, Family_Members), by = c("Position", "Stat"))

# Boundary-noise margin applied purely as a REPORTING lens (does the CI
# clear zero by the margin) -- explicitly NOT labeled "significant" or
# "confirmed" anywhere, per Gate 8's zero-confirmatory-language rule.
all_results$CI_Clears_Boundary_Margin <- (all_results$Concordance_CI_Lower_BiasCorrected - BOUNDARY_NOISE_MARGIN) > 0
all_results$Abs_Effect_Size <- abs(all_results$Concordance_Point_Estimate_BiasCorrected)
ranked <- all_results %>%
  select(Position, Stat, Family_Id, Family_Size, Concordance_Point_Estimate_BiasCorrected,
         Concordance_CI_Lower_BiasCorrected, Concordance_CI_Upper_BiasCorrected, Abs_Effect_Size,
         Direct_Read_Mean_Rho, Method_Disagreement_Flag, CI_Clears_Boundary_Margin) %>%
  arrange(desc(Abs_Effect_Size))

cat("\n=== EXPLORATORY_ ranked effect sizes (top 20 by |effect size|, 2014-2019 screen ONLY) ===\n")
cat("(Ranked list only -- NOT a significance test, NOT a confirmation. This is discovery-stage output;\n")
cat(" any candidate the user wants to pursue needs its own committed prereg entry before the\n")
cat(" 2020-2025 confirmation era is touched.)\n\n")
print(head(ranked, 20), row.names = FALSE)

dir.create("output", showWarnings = FALSE)
readr::write_csv(ranked, "output/EXPLORATORY_team_context_screen_ranked.csv")
readr::write_csv(all_results, "output/EXPLORATORY_team_context_screen_full_results.csv")
readr::write_csv(family_df, "output/EXPLORATORY_team_context_screen_family_matrix.csv")
cat("\nWritten: output/EXPLORATORY_team_context_screen_{ranked,full_results,family_matrix}.csv\n")
cat("\nGate 8 complete. Per plan: presenting candidates and stopping -- graduation to the\n")
cat("2020-2025 confirmation era requires a NEW, separately committed prereg entry per candidate.\n")
