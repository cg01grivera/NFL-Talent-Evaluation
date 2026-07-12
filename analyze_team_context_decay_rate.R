cat("
================================================================
  analyze_team_context_decay_rate.R

  GOAL: Persistence screen for the newly-built team-level context
  stats (passing efficiency, deep-ball rates, game script, OL
  penalties, 4th-down aggressiveness, weekly variance, drive-level
  outcomes, target share by position) -- does a decay-weighted blend
  of a TEAM's own past seasons predict its own future season's value
  of the same stat. Same cheap first filter as every prior persistence
  screen in this project -- passing it doesn't guarantee real Beat-ADP
  value (this project's own history: QB_Rush_Stanine was the LEAST
  persistent QB stat found anywhere, yet had the most interesting
  concordance result once fully tested), and failing it doesn't
  permanently disqualify a stat either.

  METHOD: Same decay-rate grid as every other persistence screen in
  this project (analyze_decay_rate.R, analyze_player_talent_decay_
  rate.R), keyed by Team instead of player or position, run at a
  single games-threshold (not applicable here -- team-seasons don't
  have a games-played qualifier the way player-seasons do; every team
  plays a full season by definition).

  PARAMETERS IN THIS RUN:
    SEASONS_TO_TEST  -- full range of seasons tested (2012-2025 default,
                        matching this project's ADP data coverage)
    R_GRID           -- decay rates tested per stat (1.0=flat avg down to 0.1)
    LOOKBACK_YEARS    -- trailing seasons feeding the decay window (4)
================================================================
\n")

if (!requireNamespace("nflreadr", quietly = TRUE)) install.packages("nflreadr", repos = "https://cloud.r-project.org")
library(dplyr)

script_arg <- grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) > 0) setwd(dirname(sub("--file=", "", script_arg)))

source("R/utils_core.R")
source("R/grading_utils.R")
source("R/fetch_team_context_data.R")
source("R/decay_test_utils.R")

# ---- CONFIG ---------------------------------------------------
if (!exists("SEASONS_TO_TEST")) SEASONS_TO_TEST <- 2012:2025
if (!exists("R_GRID")) R_GRID <- c(1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
if (!exists("LOOKBACK_YEARS")) LOOKBACK_YEARS <- 4
cat("=== Config: SEASONS_TO_TEST =", paste(range(SEASONS_TO_TEST), collapse="-"),
    "| LOOKBACK_YEARS =", LOOKBACK_YEARS, "===\n")
cat("NOTE: these persist in your R session once set. If a value above wasn't\n")
cat("intended, clear it first: rm(SEASONS_TO_TEST, R_GRID, LOOKBACK_YEARS)\n\n")
# -----------------------------------------------------------------

message("Building team-context stats for ", min(SEASONS_TO_TEST), "-", max(SEASONS_TO_TEST),
        " -- one full pbp pull per season, expect this to take a while...")
team_context_by_season <- list()
for (yr in SEASONS_TO_TEST) {
  message("  ", yr, "...")
  team_context_by_season[[as.character(yr)]] <- fetch_team_context_stats(yr)
}
team_context_full <- do.call(rbind, team_context_by_season)
cat("Total team-season rows:", nrow(team_context_full), "across", length(SEASONS_TO_TEST), "seasons\n\n")

stat_cols <- setdiff(names(team_context_full), c("Team", "Season"))
cat("Testing", length(stat_cols), "team-context stats:\n")
cat(paste(" -", stat_cols, collapse = "\n"), "\n\n")

message("Running the decay-rate grid...")
results <- list()
for (stat in stat_cols) {
  message("  ", stat, "...")
  res <- test_decay_rates(team_context_full, "Team", stat, "Season", R_GRID, LOOKBACK_YEARS)
  best_idx <- which.max(res$correlations)
  results[[stat]] <- data.frame(
    Stat = stat,
    Best_R = if (length(best_idx) > 0) R_GRID[best_idx] else NA,
    Best_Correlation = if (length(best_idx) > 0) round(res$correlations[best_idx], 3) else NA,
    Flat_Avg_Correlation = round(res$correlations[which(R_GRID == 1.0)], 3),
    N_Target_Years = res$n_years,
    N_Pairs = res$n_pairs
  )
}
summary_df <- do.call(rbind, results)
rownames(summary_df) <- NULL
summary_df <- summary_df[order(-summary_df$Best_Correlation), ]

cat("\n=== Team-context persistence, ranked best to worst ===\n")
print(summary_df, row.names = FALSE)

dir.create("output", showWarnings = FALSE)
readr::write_csv(summary_df, "output/analyze_team_context_decay_rate_results.csv")
cat("\nFull results written to output/analyze_team_context_decay_rate_results.csv\n")
