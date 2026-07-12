# ============================================================
# analyze_player_talent_decay_rate.R
# Persistence screen for individual player-talent stats -- the first
# step of the shift from team-context to player-talent evaluation.
# Same decay-rate grid methodology as analyze_decay_rate.R, but keyed
# by player (norm_name) instead of team, and run at TWO minimum-games
# thresholds (4 and 8) side by side, since we deliberately didn't want
# to guess a single cutoff.
#
# Restricted to ADP-pool-relevant player-seasons (top150_pool(), same
# filter every team-context script used) rather than every player who
# ever appeared in a box score -- both for tractability (evaluate_
# decay_rate() scans the full entity pool per target row; thousands of
# career backups/callups would make this far slower than the 32-team
# version ever was) and for relevance (we care about persistence among
# fantasy-relevant players, not the entire league).
#
# MIN_GAMES filters a player-SEASON row out entirely if it falls below
# the threshold -- both as a potential "actual" (current year) test
# point and as potential "history" (predictor) data. A cameo season
# shouldn't count as meaningful evidence either way.
# ============================================================
cat("
================================================================
  analyze_player_talent_decay_rate.R

  GOAL: Persistence screen for individual player-talent stats -- does
  a decay-weighted blend of a player's OWN past seasons predict his own
  future season's value of the same stat. This is the first, cheap
  filter before the expensive Beat-ADP battery -- passing it doesn't
  guarantee real decision value (QB_Rush_Stanine, tested elsewhere,
  proved persistence and Beat-ADP value can point in opposite
  directions), and failing it doesn't permanently disqualify a stat
  either, but it screens out pure noise before spending real compute
  on it.

  METHOD: Same decay-rate grid as analyze_decay_rate.R (team-context
  project), keyed by player (norm_name) instead of team, restricted to
  ADP-pool-relevant player-seasons, run at TWO minimum-games thresholds
  side by side rather than assuming one.

  PARAMETERS IN THIS RUN:
    SEASONS_TO_TEST          -- full range of seasons tested (2013-2025 default)
    R_GRID                    -- decay rates tested per stat (1.0=flat avg down to 0.1)
    LOOKBACK_YEARS            -- trailing seasons feeding the decay window (4)
    MIN_GAMES_THRESHOLDS       -- games-played floors tested side by side (4 and 8)
    MIN_TARGET_YEARS_FOR_MEAN  -- (not used for per-stat results, only for
                                  any future project-wide generalizable-r summary)
    POSITIONS_TO_TEST          -- which positions' stat lists actually run (all 4 default)
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
source("R/decay_test_utils.R")

# ---- CONFIG ---------------------------------------------------
if (!exists("SEASONS_TO_TEST")) SEASONS_TO_TEST <- 2013:2025
if (!exists("R_GRID")) R_GRID <- c(1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
if (!exists("LOOKBACK_YEARS")) LOOKBACK_YEARS <- 4
if (!exists("MIN_GAMES_THRESHOLDS")) MIN_GAMES_THRESHOLDS <- c(4, 8)
if (!exists("MIN_TARGET_YEARS_FOR_MEAN")) MIN_TARGET_YEARS_FOR_MEAN <- 10
if (!exists("POSITIONS_TO_TEST")) POSITIONS_TO_TEST <- c("QB", "RB", "WR", "TE")
cat("=== Config: SEASONS_TO_TEST =", paste(range(SEASONS_TO_TEST), collapse="-"),
    "| LOOKBACK_YEARS =", LOOKBACK_YEARS,
    "| MIN_GAMES_THRESHOLDS =", paste(MIN_GAMES_THRESHOLDS, collapse=", "),
    "| MIN_TARGET_YEARS_FOR_MEAN =", MIN_TARGET_YEARS_FOR_MEAN,
    "| POSITIONS_TO_TEST =", paste(POSITIONS_TO_TEST, collapse=", "), "===\n")
cat("NOTE: these persist in your R session once set. If a value above wasn't\n")
cat("intended, clear it first: rm(SEASONS_TO_TEST, R_GRID, LOOKBACK_YEARS, MIN_GAMES_THRESHOLDS, MIN_TARGET_YEARS_FOR_MEAN, POSITIONS_TO_TEST)\n\n")
# -----------------------------------------------------------------

# evaluate_decay_rate() / test_decay_rates() are sourced from
# R/decay_test_utils.R above -- entity-generic, pointed at "norm_name"
# below instead of "Team".

adp <- load_historic_adp()
message("Fetching player weekly stats for ", min(SEASONS_TO_TEST), "-", max(SEASONS_TO_TEST), "...")
weekly_stats_full <- nflreadr::load_player_stats(clamp_seasons(SEASONS_TO_TEST), summary_level = "week")
if ("season_type" %in% names(weekly_stats_full)) weekly_stats_full <- weekly_stats_full %>% filter(season_type == "REG")

message("Building player-season stats...")
player_season_full <- fetch_player_season_stats(clamp_seasons(SEASONS_TO_TEST), weekly_stats = weekly_stats_full)

message("Fetching red zone/end zone splits (needs raw pbp)...")
player_rz_full <- fetch_player_redzone_endzone_stats(clamp_seasons(SEASONS_TO_TEST))

message("Fetching dropback/rush success rate splits...")
player_success_full <- fetch_player_success_rate_stats(clamp_seasons(SEASONS_TO_TEST))

message("Fetching QB designed-rush/scramble split...")
qb_rush_split_full <- fetch_qb_rush_split_stats(clamp_seasons(SEASONS_TO_TEST))

message("Fetching RB situational rushing stats (gap/goal-to-go/explosive/power/leverage)...")
rush_situational_full <- fetch_player_rush_situational_stats(clamp_seasons(SEASONS_TO_TEST))

message("Fetching NGS rushing stats (RYOE, box counts, time to LOS) -- one season at a time...")
ngs_rushing_by_season <- list()
for (yr in clamp_seasons(SEASONS_TO_TEST)) {
  ngs_rushing_by_season[[as.character(yr)]] <- tryCatch(
    fetch_player_ngs_rushing_stats(yr),
    error = function(e) { message("    NGS fetch failed for ", yr, ": ", conditionMessage(e)); NULL }
  )
}
ngs_rushing_full <- do.call(rbind, ngs_rushing_by_season[!sapply(ngs_rushing_by_season, is.null)])

message("Fetching NGS receiving stats (separation, cushion, YAC over expected) -- one season at a time...")
ngs_receiving_by_season <- list()
for (yr in clamp_seasons(SEASONS_TO_TEST)) {
  ngs_receiving_by_season[[as.character(yr)]] <- tryCatch(
    fetch_player_ngs_receiving_stats(yr),
    error = function(e) { message("    NGS receiving fetch failed for ", yr, ": ", conditionMessage(e)); NULL }
  )
}
ngs_receiving_full <- do.call(rbind, ngs_receiving_by_season[!sapply(ngs_receiving_by_season, is.null)])

# Merge RZ/EZ raw counts into player_season_full and convert to
# per-game rates using THIS table's own Games_Played -- not
# recomputed a second way inside the RZ fetch function itself.
player_season_full <- player_season_full %>%
  left_join(player_rz_full, by = c("Season", "player_id", "Team")) %>%
  left_join(player_success_full, by = c("Season", "player_id", "Team")) %>%
  left_join(qb_rush_split_full, by = c("Season", "player_id", "Team")) %>%
  left_join(rush_situational_full, by = c("Season", "player_id", "Team")) %>%
  left_join(ngs_rushing_full, by = c("Season", "player_id")) %>%
  left_join(ngs_receiving_full, by = c("Season", "player_id")) %>%
  mutate(
    RZ_Targets   = coalesce(RZ_Targets, 0),
    EZ_Targets   = coalesce(EZ_Targets, 0),
    RZ_Rush_Att  = coalesce(RZ_Rush_Att, 0),
    RZ_Pass_Att  = coalesce(RZ_Pass_Att, 0),
    RZ_Completions = coalesce(RZ_Completions, 0),
    Designed_Rush_Att   = coalesce(Designed_Rush_Att, 0),
    Designed_Rush_Yards = coalesce(Designed_Rush_Yards, 0),
    Scramble_Att        = coalesce(Scramble_Att, 0),
    Scramble_Yards      = coalesce(Scramble_Yards, 0),
    Clock_Killing_Rush_Att = coalesce(Clock_Killing_Rush_Att, 0),
    RZ_Targets_PG  = RZ_Targets / Games_Played,
    EZ_Targets_PG  = EZ_Targets / Games_Played,
    RZ_Rush_Att_PG = RZ_Rush_Att / Games_Played,
    RZ_Pass_Att_PG = RZ_Pass_Att / Games_Played,
    Designed_Rush_Att_PG = Designed_Rush_Att / Games_Played,
    Scramble_Att_PG      = Scramble_Att / Games_Played,
    Clock_Killing_Rush_PG = Clock_Killing_Rush_Att / Games_Played,
    # RZ passing efficiency -- NA (not 0) for a QB with zero RZ pass
    # attempts, same reasoning as Dropback_Success_Rate: an undefined
    # rate isn't a true zero.
    RZ_Comp_Pct     = ifelse(RZ_Pass_Att > 0, RZ_Completions / RZ_Pass_Att, NA_real_),
    RZ_EPA_Per_Att  = ifelse(RZ_Pass_Att > 0, RZ_Pass_EPA_Sum / RZ_Pass_Att, NA_real_)
    # Dropback_Success_Rate / Rush_Success_Rate deliberately NOT
    # coalesced to 0 -- unlike RZ/EZ counts (where "no looks" really is
    # a true zero), a player with zero qualifying dropbacks/rushes has
    # no defined success rate at all; NA is the correct value, not 0.
  )

# Sanity check on the join itself, printed every run -- confirmed real
# bug previously here: joining on norm_name (built from two DIFFERENT
# nflverse naming conventions -- pbp's abbreviated "F.Last" vs.
# load_player_stats()'s full-name format) matched almost nothing, and
# coalesce(...,0) silently turned every failed match into a false zero
# rather than an error. This prints the actual nonzero-match rate every
# time so a regression would be caught immediately, not discovered by
# a `-0.003` correlation showing up three steps later.
message("RZ/EZ join check: ", sum(player_season_full$RZ_Targets > 0), " of ", nrow(player_season_full),
        " player-seasons have RZ_Targets > 0 (", round(100 * mean(player_season_full$RZ_Targets > 0), 1),
        "% -- expect a meaningfully positive share for WR/TE especially; a value near 0% means the join is broken again).")

# Restrict to ADP-pool-relevant player-seasons only -- see header note.
message("Restricting to ADP-pool-relevant player-seasons...")
adp_pool_by_season <- lapply(SEASONS_TO_TEST, function(yr) {
  top150_pool(adp, yr) %>% filter(Pos %in% c("QB", "RB", "WR", "TE")) %>%
    transmute(Season = yr, norm_name, Pos)
})
adp_pool_all <- do.call(rbind, adp_pool_by_season)

player_season_relevant <- player_season_full %>%
  inner_join(adp_pool_all, by = c("Season", "norm_name")) %>%
  filter(position == Pos)  # guard against a rare name collision across positions in the same season

# Full stat list, per position -- see conversation record for the full
# reasoning behind each inclusion/exclusion.
stat_specs <- list(
  # QB
  list(name = "QB Rush_Att_PG",        position = "QB", value = "Rush_Att_PG"),
  list(name = "QB RZ_Rush_Att_PG",     position = "QB", value = "RZ_Rush_Att_PG"),
  list(name = "QB Pass_Yards_PG",      position = "QB", value = "Pass_Yards_PG"),
  list(name = "QB Dropbacks_PG",       position = "QB", value = "Dropbacks_PG"),
  list(name = "QB Rush_Yards_PG",      position = "QB", value = "Rush_Yards_PG"),
  list(name = "QB RZ_Pass_Att_PG",     position = "QB", value = "RZ_Pass_Att_PG"),
  list(name = "QB Pass_Yards (raw)",   position = "QB", value = "Pass_Yards"),
  list(name = "QB Rush_Yards (raw)",   position = "QB", value = "Rush_Yards"),
  list(name = "QB Dropbacks (raw)",    position = "QB", value = "Dropbacks"),
  list(name = "QB Rush_Yards_Per_Att", position = "QB", value = "Rush_Yards_Per_Att"),
  list(name = "QB Games_Played",       position = "QB", value = "Games_Played"),
  list(name = "QB CPOE",               position = "QB", value = "CPOE"),
  list(name = "QB ANY_A",              position = "QB", value = "ANY_A"),
  list(name = "QB Completion_Pct",     position = "QB", value = "Completion_Pct"),
  list(name = "QB TD_Rate",            position = "QB", value = "TD_Rate"),
  list(name = "QB INT_Rate",           position = "QB", value = "INT_Rate"),
  list(name = "QB Sack_Rate",          position = "QB", value = "Sack_Rate"),
  list(name = "QB Dropback_EPA",       position = "QB", value = "Dropback_EPA"),
  list(name = "QB Rush_EPA",           position = "QB", value = "Rush_EPA"),
  list(name = "QB EPA_Per_Play",       position = "QB", value = "EPA_Per_Play"),
  list(name = "QB Dropback_Success_Rate", position = "QB", value = "Dropback_Success_Rate"),
  list(name = "QB Rush_Success_Rate",  position = "QB", value = "Rush_Success_Rate"),
  list(name = "QB aDOT",               position = "QB", value = "aDOT"),
  list(name = "QB Pass_First_Downs_PG",position = "QB", value = "Pass_First_Downs_PG"),
  list(name = "QB NY_A",               position = "QB", value = "NY_A"),
  list(name = "QB Designed_Rush_Att_PG", position = "QB", value = "Designed_Rush_Att_PG"),
  list(name = "QB Scramble_Att_PG",    position = "QB", value = "Scramble_Att_PG"),
  list(name = "QB RZ_Comp_Pct",        position = "QB", value = "RZ_Comp_Pct"),
  list(name = "QB RZ_EPA_Per_Att",     position = "QB", value = "RZ_EPA_Per_Att"),
  # RB
  list(name = "RB Touches_PG",         position = "RB", value = "Touches_PG"),
  list(name = "RB Rush_Yards_PG",      position = "RB", value = "Rush_Yards_PG"),
  list(name = "RB Rec_Yards_PG",       position = "RB", value = "Rec_Yards_PG"),
  list(name = "RB Rush_First_Downs_PG",position = "RB", value = "Rush_First_Downs_PG"),
  list(name = "RB Rec_First_Downs_PG", position = "RB", value = "Rec_First_Downs_PG"),
  list(name = "RB Target_Share",       position = "RB", value = "Target_Share"),
  list(name = "RB RZ_Rush_Att_PG",     position = "RB", value = "RZ_Rush_Att_PG"),
  list(name = "RB Rush_Yards (raw)",   position = "RB", value = "Rush_Yards"),
  list(name = "RB Rec_Yards (raw)",    position = "RB", value = "Rec_Yards"),
  list(name = "RB Touches (raw)",      position = "RB", value = "Touches"),
  list(name = "RB Yards_Per_Touch",    position = "RB", value = "Yards_Per_Touch"),
  list(name = "RB Games_Played",       position = "RB", value = "Games_Played"),
  # Already generic, computed for every position, never tested for RB
  list(name = "RB Rush_Success_Rate",  position = "RB", value = "Rush_Success_Rate"),
  list(name = "RB Rush_EPA",           position = "RB", value = "Rush_EPA"),
  list(name = "RB WOPR",               position = "RB", value = "WOPR"),
  list(name = "RB Air_Yards_Share",    position = "RB", value = "Air_Yards_Share"),
  list(name = "RB RZ_Targets_PG",      position = "RB", value = "RZ_Targets_PG"),
  # Cheap new additions from already-confirmed columns
  list(name = "RB Rush_TD_Rate",         position = "RB", value = "Rush_TD_Rate"),
  list(name = "RB Rush_First_Down_Rate", position = "RB", value = "Rush_First_Down_Rate"),
  list(name = "RB Fumble_Rate",          position = "RB", value = "Fumble_Rate"),
  list(name = "RB Receiving_EPA",        position = "RB", value = "Receiving_EPA"),
  list(name = "RB Receiving_aDOT",       position = "RB", value = "Receiving_aDOT"),
  list(name = "RB Yards_Per_Reception",  position = "RB", value = "Yards_Per_Reception"),
  list(name = "RB YAC_Share",            position = "RB", value = "YAC_Share"),
  list(name = "RB Scrimmage_Yards_PG",   position = "RB", value = "Scrimmage_Yards_PG"),
  # New pbp-based situational stats
  list(name = "RB Boundary_Run_Pct",      position = "RB", value = "Boundary_Run_Pct"),
  list(name = "RB Goal_To_Go_Conv_Rate",  position = "RB", value = "Goal_To_Go_Conv_Rate"),
  list(name = "RB Explosive_Run_Rate",    position = "RB", value = "Explosive_Run_Rate"),
  list(name = "RB Stuffed_Run_Rate",      position = "RB", value = "Stuffed_Run_Rate"),
  list(name = "RB Power_Success_Rate",    position = "RB", value = "Power_Success_Rate"),
  list(name = "RB Clock_Killing_Rush_PG", position = "RB", value = "Clock_Killing_Rush_PG"),
  list(name = "RB High_Leverage_EPA",     position = "RB", value = "High_Leverage_EPA"),
  list(name = "RB Garbage_Time_EPA",      position = "RB", value = "Garbage_Time_EPA"),
  # New NGS-based stats
  list(name = "RB RYOE_Per_Att",       position = "RB", value = "RYOE_Per_Att"),
  list(name = "RB RYOE_Pct",           position = "RB", value = "RYOE_Pct"),
  list(name = "RB NGS_Efficiency",     position = "RB", value = "NGS_Efficiency"),
  list(name = "RB Pct_Stacked_Box",    position = "RB", value = "Pct_Stacked_Box"),
  list(name = "RB Avg_Time_To_LOS",    position = "RB", value = "Avg_Time_To_LOS"),
  # WR
  list(name = "WR Target_Share",       position = "WR", value = "Target_Share"),
  list(name = "WR Rec_Yards_PG",       position = "WR", value = "Rec_Yards_PG"),
  list(name = "WR Rec_First_Downs_PG", position = "WR", value = "Rec_First_Downs_PG"),
  list(name = "WR Targets_PG",         position = "WR", value = "Targets_PG"),
  list(name = "WR Air_Yards_Share",    position = "WR", value = "Air_Yards_Share"),
  list(name = "WR WOPR",               position = "WR", value = "WOPR"),
  list(name = "WR RZ_Targets_PG",      position = "WR", value = "RZ_Targets_PG"),
  list(name = "WR EZ_Targets_PG",      position = "WR", value = "EZ_Targets_PG"),
  list(name = "WR Rec_Yards (raw)",    position = "WR", value = "Rec_Yards"),
  list(name = "WR Targets (raw)",      position = "WR", value = "Targets"),
  list(name = "WR Yards_Per_Target",   position = "WR", value = "Yards_Per_Target"),
  list(name = "WR Catch_Rate",         position = "WR", value = "Catch_Rate"),
  list(name = "WR Games_Played",       position = "WR", value = "Games_Played"),
  # Already generic, computed for every position, never tested for WR
  list(name = "WR Receiving_EPA",       position = "WR", value = "Receiving_EPA"),
  list(name = "WR Receiving_aDOT",      position = "WR", value = "Receiving_aDOT"),
  list(name = "WR Yards_Per_Reception", position = "WR", value = "Yards_Per_Reception"),
  list(name = "WR YAC_Share",           position = "WR", value = "YAC_Share"),
  # New NGS receiving stats -- Avg_Separation and YAC_Above_Expectation
  # are the receiving-side analogues of RB's RYOE: attempts to isolate
  # a receiver's own skill (route-running/separation; YAC ability) from
  # team/scheme context.
  list(name = "WR Avg_Separation",         position = "WR", value = "Avg_Separation"),
  list(name = "WR Avg_Cushion",            position = "WR", value = "Avg_Cushion"),
  list(name = "WR Avg_Intended_Air_Yards", position = "WR", value = "Avg_Intended_Air_Yards"),
  list(name = "WR Pct_Share_Intended_Air_Yards", position = "WR", value = "Pct_Share_Intended_Air_Yards"),
  list(name = "WR YAC_Above_Expectation",  position = "WR", value = "YAC_Above_Expectation"),
  # TE (same stat set as WR)
  list(name = "TE Target_Share",       position = "TE", value = "Target_Share"),
  list(name = "TE Rec_Yards_PG",       position = "TE", value = "Rec_Yards_PG"),
  list(name = "TE Rec_First_Downs_PG", position = "TE", value = "Rec_First_Downs_PG"),
  list(name = "TE Targets_PG",         position = "TE", value = "Targets_PG"),
  list(name = "TE Air_Yards_Share",    position = "TE", value = "Air_Yards_Share"),
  list(name = "TE WOPR",               position = "TE", value = "WOPR"),
  list(name = "TE RZ_Targets_PG",      position = "TE", value = "RZ_Targets_PG"),
  list(name = "TE EZ_Targets_PG",      position = "TE", value = "EZ_Targets_PG"),
  list(name = "TE Rec_Yards (raw)",    position = "TE", value = "Rec_Yards"),
  list(name = "TE Targets (raw)",      position = "TE", value = "Targets"),
  list(name = "TE Yards_Per_Target",   position = "TE", value = "Yards_Per_Target"),
  list(name = "TE Catch_Rate",         position = "TE", value = "Catch_Rate"),
  list(name = "TE Games_Played",       position = "TE", value = "Games_Played"),
  # Already generic, computed for every position, never tested for TE
  list(name = "TE Receiving_EPA",       position = "TE", value = "Receiving_EPA"),
  list(name = "TE Receiving_aDOT",      position = "TE", value = "Receiving_aDOT"),
  list(name = "TE Yards_Per_Reception", position = "TE", value = "Yards_Per_Reception"),
  list(name = "TE YAC_Share",           position = "TE", value = "YAC_Share"),
  # New NGS receiving stats
  list(name = "TE Avg_Separation",         position = "TE", value = "Avg_Separation"),
  list(name = "TE Avg_Cushion",            position = "TE", value = "Avg_Cushion"),
  list(name = "TE Avg_Intended_Air_Yards", position = "TE", value = "Avg_Intended_Air_Yards"),
  list(name = "TE Pct_Share_Intended_Air_Yards", position = "TE", value = "Pct_Share_Intended_Air_Yards"),
  list(name = "TE YAC_Above_Expectation",  position = "TE", value = "YAC_Above_Expectation")
)

stat_specs <- Filter(function(s) s$position %in% POSITIONS_TO_TEST, stat_specs)
message("Running the decay-rate grid for ", length(stat_specs), " stats x ", length(MIN_GAMES_THRESHOLDS), " games-thresholds...")
all_results <- list()
for (min_games in MIN_GAMES_THRESHOLDS) {
  filtered <- player_season_relevant %>% filter(Games_Played >= min_games)
  for (spec in stat_specs) {
    message("  [min_games=", min_games, "] ", spec$name, "...")
    pos_data <- filtered %>% filter(position == spec$position)
    res <- test_decay_rates(pos_data, "norm_name", spec$value, "Season", R_GRID, LOOKBACK_YEARS)
    best_idx <- which.max(res$correlations)
    all_results[[paste(min_games, spec$name)]] <- data.frame(
      Min_Games = min_games, Stat = spec$name,
      Best_R = if (length(best_idx) > 0) R_GRID[best_idx] else NA,
      Best_Correlation = if (length(best_idx) > 0) round(res$correlations[best_idx], 3) else NA,
      Flat_Avg_Correlation = round(res$correlations[which(R_GRID == 1.0)], 3),
      N_Target_Years = res$n_years, N_Pairs = res$n_pairs
    )
  }
}
summary_df <- do.call(rbind, all_results)
rownames(summary_df) <- NULL

cat("\n=== Player-talent persistence: 4-game vs. 8-game minimum, side by side ===\n")
if (length(MIN_GAMES_THRESHOLDS) == 2) {
  a <- summary_df %>% filter(Min_Games == MIN_GAMES_THRESHOLDS[1]) %>%
    select(Stat, Best_R, Best_Correlation, N_Target_Years, N_Pairs)
  b <- summary_df %>% filter(Min_Games == MIN_GAMES_THRESHOLDS[2]) %>%
    select(Stat, Best_R, Best_Correlation, N_Target_Years, N_Pairs)
  names(a)[-1] <- paste0(names(a)[-1], "_MinGames", MIN_GAMES_THRESHOLDS[1])
  names(b)[-1] <- paste0(names(b)[-1], "_MinGames", MIN_GAMES_THRESHOLDS[2])
  wide <- merge(a, b, by = "Stat", sort = FALSE)
  print(wide, row.names = FALSE)
} else {
  cat("(More than 2 MIN_GAMES_THRESHOLDS set -- showing long format instead of a side-by-side table.)\n")
  print(summary_df, row.names = FALSE)
}

dir.create("output", showWarnings = FALSE)
readr::write_csv(summary_df, "output/analyze_player_talent_decay_rate_results.csv")
cat("\nFull results written to output/analyze_player_talent_decay_rate_results.csv\n")
