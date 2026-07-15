cat("
================================================================
  analyze_team_context_null_biases.R

  GOAL: Gate 6 (TEAM_CONTEXT_REWORK_PLAN_V2.md / TEAM_CONTEXT_
  PREREGISTRATION_V2.md Rule 3) -- fresh concordance null biases for the
  four team-context position pools (QB_teamctx, RB_teamctx, WR_teamctx,
  TE_teamctx), using the RESOLVED team-level permutation unit
  (permute_within='team', Rule 5) and the shared, tested library
  (R/beat_adp_battery.R) -- NOT a parallel implementation. This is the
  first script in the Team-Context Rework to touch real player outcome
  data (season_ppg/ADP); everything before Gate 5 was stat-level only.

  METHOD: build_position_data(..., extra_key_col = 'Team') assembles each
  position's real top-150 pool with team-context candidates decay-
  weighted per TEAM (not per player) and broadcast to that team's roster.
  run_estimation_for_position(..., permute_within = 'team') then computes
  a fresh null bias per pool (5 meta x 100 inner x BIAS_N_BOOT=500) and a
  coverage spot-check, writing to the REAL production cache
  (output/concordance_null_bias_cache.csv) under the '_teamctx' labels
  Rule 3 declares. No transfer across pools or modes -- each pool gets
  its OWN bias, keyed by its own representative candidate.

  REPRESENTATIVE CANDIDATE PER POOL (one bias per pool, applied to every
  confirmatory candidate in that pool, per Rule 3 -- not one bias per
  candidate):
    QB_teamctx, WR_teamctx, TE_teamctx -> Team_Sack_Rate_Allowed (C2/C3/C4)
    RB_teamctx                         -> Team_Rush_Stuff_Rate (C1, the
                                           declared lead/strongest candidate)

  PARAMETERS IN THIS RUN:
    SEASONS_TO_TEST   -- confirmatory era, full range per the plan's own
                         config block (2014-2025, NOT the 2020-2025
                         sub-range reserved for exploratory graduates)
    DECAY_R, LOOKBACK_YEARS, MIN_GAMES -- project-wide standard (0.5, 4, 8)
    N_BOOT            -- battery bootstrap replicates (1000, per Rule 3 /
                         Gate 6 spec)
    BIAS_N_META_REPLICATES / BIAS_N_REPLICATES / BIAS_N_BOOT -- 5 / 100 /
                         500, per Gate 6 spec -- NOT cheapened; this feeds
                         the real confirmatory battery's bias correction.
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
if (!exists("DECAY_R")) DECAY_R <- 0.5
if (!exists("LOOKBACK_YEARS")) LOOKBACK_YEARS <- 4
if (!exists("MIN_GAMES")) MIN_GAMES <- 8
if (!exists("N_BOOT")) N_BOOT <- 1000
if (!exists("CONF_LEVEL")) CONF_LEVEL <- 0.90
if (!exists("SIGNIFICANCE_WINDOW")) SIGNIFICANCE_WINDOW <- 12
if (!exists("BIAS_N_META_REPLICATES")) BIAS_N_META_REPLICATES <- 5
if (!exists("BIAS_N_REPLICATES")) BIAS_N_REPLICATES <- 100
if (!exists("BIAS_N_BOOT")) BIAS_N_BOOT <- 500
if (!exists("BIAS_PERMUTE_WITHIN")) BIAS_PERMUTE_WITHIN <- "team"
if (!exists("FORCE_RECOMPUTE_BIAS")) FORCE_RECOMPUTE_BIAS <- FALSE
if (!exists("CACHE_PATH")) CACHE_PATH <- "output/concordance_null_bias_cache.csv"
cat("=== Config: SEASONS_TO_TEST =", paste(range(SEASONS_TO_TEST), collapse = "-"),
    "| DECAY_R =", DECAY_R, "| LOOKBACK_YEARS =", LOOKBACK_YEARS, "| MIN_GAMES =", MIN_GAMES,
    "| N_BOOT =", N_BOOT, "| CONF_LEVEL =", CONF_LEVEL, "| SIGNIFICANCE_WINDOW =", SIGNIFICANCE_WINDOW, "\n")
cat("    BIAS_N_META_REPLICATES =", BIAS_N_META_REPLICATES, "| BIAS_N_REPLICATES =", BIAS_N_REPLICATES,
    "| BIAS_N_BOOT =", BIAS_N_BOOT, "| BIAS_PERMUTE_WITHIN =", BIAS_PERMUTE_WITHIN,
    "| FORCE_RECOMPUTE_BIAS =", FORCE_RECOMPUTE_BIAS, "===\n\n")
# -----------------------------------------------------------------

adp <- load_historic_adp()
full_range <- (min(SEASONS_TO_TEST) - LOOKBACK_YEARS):max(SEASONS_TO_TEST)
message("Fetching player weekly stats for ", min(full_range), "-", max(full_range), "...")
weekly_stats_full <- nflreadr::load_player_stats(clamp_seasons(full_range), summary_level = "week")
if ("season_type" %in% names(weekly_stats_full)) weekly_stats_full <- weekly_stats_full %>% filter(season_type == "REG")
all_seasons_ppg <- compute_season_ppg(weekly_stats_full)
prior_ppg_lookup <- all_seasons_ppg %>% transmute(Season = season + 1, norm_name, position, Prior_PPG = season_ppg)

if (exists("GATE6_TEAM_CONTEXT_FULL")) {
  team_context_full <- GATE6_TEAM_CONTEXT_FULL
  cat("Reusing pre-fetched team_context_full (", nrow(team_context_full), "rows )\n\n")
} else {
  message("Fetching team-context stats for ", min(full_range), "-", max(full_range),
          " -- one full pbp pull per season, expect this to take a while...")
  team_context_by_season <- list()
  for (yr in clamp_seasons(full_range)) {
    message("  ", yr, "...")
    team_context_by_season[[as.character(yr)]] <- fetch_team_context_stats(yr)
  }
  team_context_full <- do.call(rbind, team_context_by_season)
}

pool_spec <- list(
  QB = list(candidates = c("Team_Sack_Rate_Allowed", "Team_QB_Hit_Rate_Allowed"), representative = "Team_Sack_Rate_Allowed"),
  RB = list(candidates = c("Team_Rush_Stuff_Rate", "Team_RZ_Trip_Rate"), representative = "Team_Rush_Stuff_Rate"),
  WR = list(candidates = c("Team_Sack_Rate_Allowed", "Team_QB_Hit_Rate_Allowed"), representative = "Team_Sack_Rate_Allowed"),
  TE = list(candidates = c("Team_Sack_Rate_Allowed", "Team_QB_Hit_Rate_Allowed"), representative = "Team_Sack_Rate_Allowed")
)

pool_data <- list()
for (pos in names(pool_spec)) {
  message("Building ", pos, " player-level data (extra_key_col='Team')...")
  d <- build_position_data(pos, adp, all_seasons_ppg, prior_ppg_lookup, team_context_full,
                            pool_spec[[pos]]$candidates, SEASONS_TO_TEST, DECAY_R, LOOKBACK_YEARS,
                            extra_key_col = "Team")
  pool_data[[pos]] <- d
  cat("  ", pos, ": ", nrow(d), " player-season rows, ", length(unique(d$Season)), " seasons\n", sep = "")
}

cat("\nProceeding to null-bias computation for all 4 pools -- this is the expensive\n")
cat("step (5 meta x", BIAS_N_REPLICATES, "inner x", BIAS_N_BOOT, "boot per pool, plus coverage spot-checks).\n\n")

gate6_results <- list()
for (pos in names(pool_spec)) {
  pool_label <- paste0(pos, "_teamctx")
  message("=== ", pool_label, " ===")
  res <- run_estimation_for_position(
    pool_label, pool_data[[pos]], pool_spec[[pos]]$candidates,
    representative_candidate_col = pool_spec[[pos]]$representative,
    seasons_to_test = SEASONS_TO_TEST, min_games = MIN_GAMES, decay_r = DECAY_R, lookback_years = LOOKBACK_YEARS,
    significance_window = SIGNIFICANCE_WINDOW, n_boot = N_BOOT, conf_level = CONF_LEVEL, skip_rmse = TRUE,
    force_recompute_bias = FORCE_RECOMPUTE_BIAS,
    bias_n_meta_replicates = BIAS_N_META_REPLICATES, bias_n_replicates = BIAS_N_REPLICATES, bias_n_boot = BIAS_N_BOOT,
    cache_path = CACHE_PATH, permute_within = BIAS_PERMUTE_WITHIN, team_col = "Team",
    run_coverage_spotcheck = TRUE, emit_direct_reads = TRUE
  )
  gate6_results[[pos]] <- res
  cov <- attr(res, "coverage_spotcheck")
  cat(sprintf("\n  %s: null_bias = %.5f | coverage = %.3f (%d/%d, p_vs_nominal=%.3f)\n\n",
              pool_label, attr(res, "null_bias"), cov$Coverage_Rate, cov$N_Covered, cov$N_Trials, cov$P_Value_Vs_Nominal))
}

cat("\n=== Gate 6 summary: null biases + coverage, all 4 pools ===\n")
for (pos in names(pool_spec)) {
  cov <- attr(gate6_results[[pos]], "coverage_spotcheck")
  cat(sprintf("  %-4s_teamctx  null_bias=%9.5f  coverage=%.3f (%d/%d)  representative=%s\n",
              pos, attr(gate6_results[[pos]], "null_bias"), cov$Coverage_Rate, cov$N_Covered, cov$N_Trials,
              pool_spec[[pos]]$representative))
}

cat("\nCache entries written to", CACHE_PATH, "-- see the '_teamctx' position labels.\n")
