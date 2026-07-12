# ============================================================
# fetch_player_data.R
# Individual player-talent stats -- extracted from nfl_team_grades'
# fetch_nflverse_data.R. All column names below were confirmed directly
# against a live `names(nflreadr::load_player_stats(2023,
# summary_level="week"))` pull, not assumed -- see project history for
# why that discipline matters (play_action, targets, carries all needed
# correction after being guessed rather than checked in the parent
# project).
#
# Requires utils_core.R and grading_utils.R to be sourced first
# (standardize_teams, filter_reg_pbp, clamp_seasons, normalize_player_name).
# ============================================================
library(dplyr)

#' Player-level season stats for talent evaluation: volume (rate AND
#' raw totals -- raw totals capture durability/games-played in a way
#' rates cannot), target_share/air_yards_share/wopr (already provided
#' directly by nflverse, not recomputed), and simple opportunity-
#' independent efficiency (yards/touch, yards/target, catch rate,
#' yards/rush attempt).
#'
#' Deliberately does NOT include a fantasy-points column -- Prior_PPG
#' for any nested-regression baseline should come from compute_
#' season_ppg() (adp_ppg_utils.R), not a second, possibly-inconsistent
#' version built fresh here.
#'
#' @param seasons seasons to fetch
#' @param weekly_stats optional pre-loaded weekly player stats, for
#'   sharing across repeated calls
fetch_player_season_stats <- function(seasons, weekly_stats = NULL) {
  if (is.null(weekly_stats)) seasons <- clamp_seasons(seasons)
  if (length(seasons) == 0) {
    warning("fetch_player_season_stats: no requested seasons are available yet in nflverse data. Returning empty result.")
    return(data.frame())
  }
  if (is.null(weekly_stats)) {
    weekly_stats <- nflreadr::load_player_stats(seasons, summary_level = "week")
  }
  if ("season" %in% names(weekly_stats)) weekly_stats <- weekly_stats %>% filter(season %in% seasons)
  if ("season_type" %in% names(weekly_stats)) weekly_stats <- weekly_stats %>% filter(season_type == "REG")
  weekly_stats$team <- standardize_teams(weekly_stats$team)
  weekly_stats$norm_name <- normalize_player_name(weekly_stats$player_display_name)

  weekly_stats %>%
    filter(position %in% c("QB", "RB", "WR", "TE")) %>%
    group_by(Season = season, norm_name, player_display_name, position, Team = team) %>%
    summarise(
      player_id        = dplyr::first(player_id),
      Games_Played     = n(),
      Pass_Yards       = sum(passing_yards, na.rm = TRUE),
      Pass_Air_Yards_Sum = sum(passing_air_yards, na.rm = TRUE),
      Rush_Yards       = sum(rushing_yards, na.rm = TRUE),
      Rush_TD          = sum(rushing_tds, na.rm = TRUE),
      Rushing_Fumbles      = sum(rushing_fumbles, na.rm = TRUE),
      Rushing_Fumbles_Lost = sum(rushing_fumbles_lost, na.rm = TRUE),
      Rec_Yards        = sum(receiving_yards, na.rm = TRUE),
      Receiving_EPA_Sum      = sum(receiving_epa, na.rm = TRUE),
      Receiving_Air_Yards_Sum = sum(receiving_air_yards, na.rm = TRUE),
      YAC_Sum          = sum(receiving_yards_after_catch, na.rm = TRUE),
      Dropbacks        = sum(attempts, na.rm = TRUE) + sum(sacks_suffered, na.rm = TRUE),
      Pass_EPA_Sum     = sum(passing_epa, na.rm = TRUE),
      Rush_EPA_Sum     = sum(rushing_epa, na.rm = TRUE),
      Rush_Att         = sum(carries, na.rm = TRUE),
      Targets          = sum(targets, na.rm = TRUE),
      Receptions       = sum(receptions, na.rm = TRUE),
      Touches          = sum(carries, na.rm = TRUE) + sum(receptions, na.rm = TRUE),
      Pass_First_Downs = sum(passing_first_downs, na.rm = TRUE),
      Rush_First_Downs = sum(rushing_first_downs, na.rm = TRUE),
      Completions      = sum(completions, na.rm = TRUE),
      Attempts         = sum(attempts, na.rm = TRUE),
      Pass_TD          = sum(passing_tds, na.rm = TRUE),
      Interceptions    = sum(passing_interceptions, na.rm = TRUE),
      Sack_Yards_Lost  = sum(sack_yards_lost, na.rm = TRUE),
      # Attempt-weighted, NOT a naive mean() across weeks -- an
      # unweighted average would let a 2-attempt mop-up week count
      # exactly as much as a 40-attempt full game, understating what a
      # real, volume-weighted season CPOE should look like. Confirmed
      # real issue: this was originally mean(), and it made raw
      # Completion_Pct (which IS naturally volume-weighted, being a
      # season-total ratio) outpersist CPOE on the persistence screen --
      # backwards from what should happen given CPOE is the more
      # refined, context-adjusted stat.
      CPOE             = weighted.mean(passing_cpoe, w = attempts, na.rm = TRUE),
      Rec_First_Downs  = sum(receiving_first_downs, na.rm = TRUE),
      Target_Share     = mean(target_share, na.rm = TRUE),
      Air_Yards_Share  = mean(air_yards_share, na.rm = TRUE),
      WOPR             = mean(wopr, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Pass_Yards_PG        = Pass_Yards / Games_Played,
      Rush_Yards_PG        = Rush_Yards / Games_Played,
      Rec_Yards_PG         = Rec_Yards / Games_Played,
      Dropbacks_PG         = Dropbacks / Games_Played,
      Rush_Att_PG          = Rush_Att / Games_Played,
      Targets_PG           = Targets / Games_Played,
      Touches_PG           = Touches / Games_Played,
      Pass_First_Downs_PG  = Pass_First_Downs / Games_Played,
      Rush_First_Downs_PG  = Rush_First_Downs / Games_Played,
      Rec_First_Downs_PG   = Rec_First_Downs / Games_Played,
      Yards_Per_Touch      = ifelse(Touches > 0, (Rush_Yards + Rec_Yards) / Touches, NA_real_),
      Yards_Per_Target     = ifelse(Targets > 0, Rec_Yards / Targets, NA_real_),
      Catch_Rate           = ifelse(Targets > 0, Receptions / Targets, NA_real_),
      Rush_Yards_Per_Att   = ifelse(Rush_Att > 0, Rush_Yards / Rush_Att, NA_real_),
      # RB-focused additions -- all built from columns already confirmed
      # present, no new guessing.
      Rush_TD_Rate            = ifelse(Rush_Att > 0, Rush_TD / Rush_Att, NA_real_),
      Rush_First_Down_Rate    = ifelse(Rush_Att > 0, Rush_First_Downs / Rush_Att, NA_real_),
      Fumble_Rate             = ifelse(Rush_Att > 0, Rushing_Fumbles / Rush_Att, NA_real_),
      Receiving_EPA           = ifelse(Targets > 0, Receiving_EPA_Sum / Targets, NA_real_),
      Receiving_aDOT          = ifelse(Targets > 0, Receiving_Air_Yards_Sum / Targets, NA_real_),
      Yards_Per_Reception     = ifelse(Receptions > 0, Rec_Yards / Receptions, NA_real_),
      # YAC_Share NOT coalesced/defaulted -- a player with zero receiving
      # yards has an undefined share, not a true zero (same reasoning
      # as Dropback_Success_Rate elsewhere in this project).
      YAC_Share               = ifelse(Rec_Yards > 0, YAC_Sum / Rec_Yards, NA_real_),
      Scrimmage_Yards_PG      = (Rush_Yards + Rec_Yards) / Games_Played,
      aDOT                 = ifelse(Attempts > 0, Pass_Air_Yards_Sum / Attempts, NA_real_),
      Pass_First_Downs_PG  = Pass_First_Downs / Games_Played,
      # NY/A: sack-adjusted yards/attempt WITHOUT the TD/INT terms ANY/A
      # bakes in -- built specifically to test whether ANY/A's weak
      # persistence comes from the sack adjustment or from the TD/INT
      # terms (both individually showed weak standalone persistence
      # already). Denominator is Dropbacks (Attempts + Sacks), same as ANY_A.
      NY_A                 = ifelse(Dropbacks > 0, (Pass_Yards - Sack_Yards_Lost) / Dropbacks, NA_real_),
      Dropback_EPA         = ifelse(Dropbacks > 0, Pass_EPA_Sum / Dropbacks, NA_real_),
      # NOTE: not fully confirmed whether load_player_stats()'s
      # passing_epa includes sack plays' EPA or only completed/incomplete
      # dropback-pass attempts -- Dropbacks (the denominator here)
      # definitely includes sacks. If passing_epa excludes them, this is
      # a slightly generous version of "dropback EPA" (not penalizing
      # sacks in the numerator the way a true dropback-EPA measure
      # should), not an incorrect one. Worth verifying directly if this
      # stat looks promising enough to build further on.
      Rush_EPA             = ifelse(Rush_Att > 0, Rush_EPA_Sum / Rush_Att, NA_real_),
      EPA_Per_Play         = ifelse((Dropbacks + Rush_Att) > 0,
                                     (Pass_EPA_Sum + Rush_EPA_Sum) / (Dropbacks + Rush_Att), NA_real_),
      Completion_Pct       = ifelse(Attempts > 0, Completions / Attempts, NA_real_),
      TD_Rate              = ifelse(Attempts > 0, Pass_TD / Attempts, NA_real_),
      INT_Rate             = ifelse(Attempts > 0, Interceptions / Attempts, NA_real_),
      Sack_Rate            = ifelse(Dropbacks > 0, (Dropbacks - Attempts) / Dropbacks, NA_real_),
      # ANY/A: standard TD/INT/sack-adjusted efficiency formula.
      # Denominator is Attempts + Sacks, which is just Dropbacks by
      # definition (Dropbacks = Attempts + sacks_suffered above).
      ANY_A                = ifelse(Dropbacks > 0,
                                     (Pass_Yards + 20 * Pass_TD - 45 * Interceptions - Sack_Yards_Lost) / Dropbacks,
                                     NA_real_)
    ) %>%
    as.data.frame()
}

#' Player-level red zone (yardline_100 <= 20) and end zone
#' (yardline_100 <= 5) target/attempt counts -- constructed from raw
#' play-by-play since load_player_stats() has no situational splits.
#'
#' Keyed by player_id (nflverse's canonical GSIS ID), NOT norm_name --
#' confirmed real bug in an earlier version of this function: pbp's
#' receiver_player_name/rusher_player_name/passer_player_name use
#' nflverse's abbreviated "F.Last" pbp format (e.g. "T.Hill"), while
#' fetch_player_season_stats() keys on player_display_name's full-name
#' format ("Tyreek Hill") -- normalize_player_name() only strips
#' punctuation/case/suffixes, it doesn't reconcile two different naming
#' CONVENTIONS, so the join silently matched almost nothing and
#' coalesce(...,0) papered over every failed match with a false zero.
#' player_id is the same GSIS scheme in both pbp and load_player_stats(),
#' so joining on it sidesteps the naming-format mismatch entirely rather
#' than patching the name-matching logic.
#'
#' norm_name is still included in the output (for display/debugging),
#' but is NOT part of the join key used to merge this into
#' fetch_player_season_stats()'s output -- see analyze_player_talent_
#' decay_rate.R's join, which uses player_id.
#'
#' Returns RAW COUNTS, not per-game rates -- per-player Games_Played
#' (the correct denominator) lives in fetch_player_season_stats(), not
#' here; rate conversion happens at the join/analysis step, to avoid
#' computing games-played two different, possibly-inconsistent ways in
#' two different functions.
#'
#' @param seasons seasons to fetch
#' @param pbp optional pre-loaded play-by-play, for sharing across calls
fetch_player_redzone_endzone_stats <- function(seasons, pbp = NULL) {
  if (is.null(pbp)) seasons <- clamp_seasons(seasons)
  if (length(seasons) == 0) {
    warning("fetch_player_redzone_endzone_stats: no requested seasons are available yet in nflverse data. Returning empty result.")
    return(data.frame(Season = integer(0), player_id = character(0), Team = character(0),
                       RZ_Targets = numeric(0), EZ_Targets = numeric(0),
                       RZ_Rush_Att = numeric(0), RZ_Pass_Att = numeric(0)))
  }
  if (is.null(pbp)) pbp <- nflreadr::load_pbp(seasons)
  if ("season" %in% names(pbp)) pbp <- pbp %>% filter(season %in% seasons)
  pbp <- filter_reg_pbp(pbp)
  pbp$posteam <- standardize_teams(pbp$posteam)

  rz_targets <- pbp %>%
    filter(play_type == "pass", !is.na(receiver_player_id), !is.na(yardline_100), yardline_100 <= 20) %>%
    count(Season = season, player_id = receiver_player_id, Team = posteam, name = "RZ_Targets")

  ez_targets <- pbp %>%
    filter(play_type == "pass", !is.na(receiver_player_id), !is.na(yardline_100), yardline_100 <= 5) %>%
    count(Season = season, player_id = receiver_player_id, Team = posteam, name = "EZ_Targets")

  rz_rush_att <- pbp %>%
    filter(play_type == "run", !is.na(rusher_player_id), !is.na(yardline_100), yardline_100 <= 20) %>%
    count(Season = season, player_id = rusher_player_id, Team = posteam, name = "RZ_Rush_Att")

  rz_pass_att <- pbp %>%
    filter(play_type == "pass", !is.na(passer_player_id), !is.na(yardline_100), yardline_100 <= 20) %>%
    count(Season = season, player_id = passer_player_id, Team = posteam, name = "RZ_Pass_Att")

  # RZ passing EFFICIENCY, not just volume -- tests whether the same
  # opportunity-beats-efficiency pattern found everywhere else in this
  # project (team-level red zone TD rate vs. trips, QB rush attempts
  # vs. rush EPA) holds for red-zone passing specifically too, rather
  # than assuming it does.
  rz_completions <- pbp %>%
    filter(play_type == "pass", !is.na(passer_player_id), !is.na(yardline_100), yardline_100 <= 20,
           !is.na(complete_pass)) %>%
    group_by(Season = season, player_id = passer_player_id, Team = posteam) %>%
    summarise(RZ_Completions = sum(complete_pass, na.rm = TRUE), .groups = "drop")

  rz_pass_epa <- pbp %>%
    filter(play_type == "pass", !is.na(passer_player_id), !is.na(yardline_100), yardline_100 <= 20,
           !is.na(epa)) %>%
    group_by(Season = season, player_id = passer_player_id, Team = posteam) %>%
    summarise(RZ_Pass_EPA_Sum = sum(epa, na.rm = TRUE), .groups = "drop")

  rz_targets %>%
    full_join(ez_targets,     by = c("Season", "player_id", "Team")) %>%
    full_join(rz_rush_att,    by = c("Season", "player_id", "Team")) %>%
    full_join(rz_pass_att,    by = c("Season", "player_id", "Team")) %>%
    full_join(rz_completions, by = c("Season", "player_id", "Team")) %>%
    full_join(rz_pass_epa,    by = c("Season", "player_id", "Team")) %>%
    mutate(across(c(RZ_Targets, EZ_Targets, RZ_Rush_Att, RZ_Pass_Att, RZ_Completions), ~coalesce(., 0L))) %>%
    # RZ_Pass_EPA_Sum NOT coalesced to 0 -- a QB with zero RZ pass
    # attempts has an undefined RZ EPA, not a true zero (same reasoning
    # as Dropback_Success_Rate elsewhere in this project).
    as.data.frame()
}

#' Player-level dropback/rush success rate -- "success" is a per-play,
#' down-and-distance-relative outcome that only exists at the raw
#' play-by-play level (confirmed: nflreadr::load_pbp()'s own `success`
#' column, not something load_player_stats() provides pre-aggregated).
#'
#' Keyed by player_id from the start (passer_player_id/rusher_player_id)
#' -- NOT normalized name -- same fix already applied to fetch_player_
#' redzone_endzone_stats() after the norm_name join bug there. Dropback
#' definition matches fetch_player_season_stats()' Dropbacks exactly
#' (play_type=="pass" OR sack==1), so these two functions' denominators
#' stay consistent with each other.
#'
#' @param seasons seasons to fetch
#' @param pbp optional pre-loaded play-by-play, for sharing across calls
fetch_player_success_rate_stats <- function(seasons, pbp = NULL) {
  if (is.null(pbp)) seasons <- clamp_seasons(seasons)
  if (length(seasons) == 0) {
    warning("fetch_player_success_rate_stats: no requested seasons are available yet in nflverse data. Returning empty result.")
    return(data.frame(Season = integer(0), player_id = character(0), Team = character(0),
                       Dropback_Success_Rate = numeric(0), Rush_Success_Rate = numeric(0)))
  }
  if (is.null(pbp)) pbp <- nflreadr::load_pbp(seasons)
  if ("season" %in% names(pbp)) pbp <- pbp %>% filter(season %in% seasons)
  pbp <- filter_reg_pbp(pbp)
  pbp$posteam <- standardize_teams(pbp$posteam)

  dropback_success <- pbp %>%
    filter((play_type == "pass" | sack == 1), !is.na(passer_player_id), !is.na(success)) %>%
    group_by(Season = season, player_id = passer_player_id, Team = posteam) %>%
    summarise(Dropback_Success_Rate = mean(success, na.rm = TRUE), .groups = "drop")

  rush_success <- pbp %>%
    filter(play_type == "run", !is.na(rusher_player_id), !is.na(success)) %>%
    group_by(Season = season, player_id = rusher_player_id, Team = posteam) %>%
    summarise(Rush_Success_Rate = mean(success, na.rm = TRUE), .groups = "drop")

  dropback_success %>%
    full_join(rush_success, by = c("Season", "player_id", "Team")) %>%
    as.data.frame()
}

#' QB designed-rush vs. scramble split -- a QB's rush attempts have
#' been tested so far as one blended number, but a coaching-scheme
#' designed run (zone read, sneak, called keeper) and an improvised
#' scramble off a broken passing play are different traits: one
#' reflects scheme trust, the other reflects pocket-escape instinct
#' under pressure. Confirmed real column for this split: pbp's
#' `qb_scramble` flag (verified present before building this).
#'
#' Returns RAW COUNTS/YARDS, not per-game rates -- same convention as
#' fetch_player_redzone_endzone_stats(), rate conversion happens at the
#' join/analysis step using the real per-player Games_Played.
#'
#' @param seasons seasons to fetch
#' @param pbp optional pre-loaded play-by-play, for sharing across calls
fetch_qb_rush_split_stats <- function(seasons, pbp = NULL) {
  if (is.null(pbp)) seasons <- clamp_seasons(seasons)
  if (length(seasons) == 0) {
    warning("fetch_qb_rush_split_stats: no requested seasons are available yet in nflverse data. Returning empty result.")
    return(data.frame(Season = integer(0), player_id = character(0), Team = character(0),
                       Designed_Rush_Att = numeric(0), Designed_Rush_Yards = numeric(0),
                       Scramble_Att = numeric(0), Scramble_Yards = numeric(0)))
  }
  if (is.null(pbp)) pbp <- nflreadr::load_pbp(seasons)
  if ("season" %in% names(pbp)) pbp <- pbp %>% filter(season %in% seasons)
  pbp <- filter_reg_pbp(pbp)
  pbp$posteam <- standardize_teams(pbp$posteam)

  rush_plays <- pbp %>%
    filter(play_type == "run", !is.na(rusher_player_id), !is.na(qb_scramble))

  # qb_scramble == 1 (or TRUE) marks a scramble; R's == coerces logical
  # to numeric safely either way, so this works regardless of which
  # type the column actually is.
  designed <- rush_plays %>%
    filter(qb_scramble == 0) %>%
    group_by(Season = season, player_id = rusher_player_id, Team = posteam) %>%
    summarise(Designed_Rush_Att = n(), Designed_Rush_Yards = sum(yards_gained, na.rm = TRUE), .groups = "drop")

  scramble <- rush_plays %>%
    filter(qb_scramble == 1) %>%
    group_by(Season = season, player_id = rusher_player_id, Team = posteam) %>%
    summarise(Scramble_Att = n(), Scramble_Yards = sum(yards_gained, na.rm = TRUE), .groups = "drop")

  designed %>%
    full_join(scramble, by = c("Season", "player_id", "Team")) %>%
    mutate(across(c(Designed_Rush_Att, Designed_Rush_Yards, Scramble_Att, Scramble_Yards), ~coalesce(., 0))) %>%
    as.data.frame()
}

#' Situational rushing stats requiring play-level granularity -- NOT
#' obtainable from load_player_stats()'s season aggregates, built from
#' raw play-by-play instead. Keyed by player_id (rusher_player_id),
#' same discipline as every other pbp-based function in this project.
#'
#' DEFINITIONS (confirmed against real column values before use):
#'   Boundary_Run_Pct        = run_gap=="end" (outside the tackles) as
#'                             a share of all carries with a recorded gap
#'   Goal_To_Go_Conv_Rate    = of goal_to_go==1 run plays, share where
#'                             yards_gained >= yardline_100 (reached the
#'                             end zone on that specific play -- a
#'                             self-verifying TD definition using only
#'                             already-confirmed columns, not a guessed
#'                             touchdown-flag column name)
#'   Explosive_Run_Rate      = share of carries with yards_gained >= 10
#'   Stuffed_Run_Rate        = share of carries with yards_gained <= 0
#'   Power_Success_Rate      = of 3rd/4th-and-2-or-less run plays, share
#'                             where yards_gained >= ydstogo (first down)
#'                             OR yards_gained >= yardline_100 (TD)
#'   Clock_Killing_Rush_PG   = rush attempts per game in the 4th quarter
#'                             while leading by 4+ points
#'   High_Leverage_EPA       = mean EPA on carries where wp is between
#'                             0.2 and 0.8 (competitive game state)
#'   Garbage_Time_EPA        = mean EPA on carries where wp < 0.05 or
#'                             wp > 0.95 (using the exact threshold
#'                             already proposed and confirmed available)
#'
#' Returns RAW COUNTS for rate-denominator pieces (not yet divided by
#' Games_Played) alongside the pre-computed rates themselves -- rate
#' conversion to per-game where needed happens at the analysis step,
#' same convention as every other fetch function here.
#'
#' @param seasons seasons to fetch
#' @param pbp optional pre-loaded play-by-play, for sharing across calls
fetch_player_rush_situational_stats <- function(seasons, pbp = NULL) {
  if (is.null(pbp)) seasons <- clamp_seasons(seasons)
  if (length(seasons) == 0) {
    warning("fetch_player_rush_situational_stats: no requested seasons are available yet in nflverse data. Returning empty result.")
    return(data.frame(Season = integer(0), player_id = character(0), Team = character(0)))
  }
  if (is.null(pbp)) pbp <- nflreadr::load_pbp(seasons)
  if ("season" %in% names(pbp)) pbp <- pbp %>% filter(season %in% seasons)
  pbp <- filter_reg_pbp(pbp)
  pbp$posteam <- standardize_teams(pbp$posteam)

  runs <- pbp %>% filter(play_type == "run", !is.na(rusher_player_id))

  gap_stats <- runs %>%
    filter(!is.na(run_gap)) %>%
    group_by(Season = season, player_id = rusher_player_id, Team = posteam) %>%
    summarise(Boundary_Runs = sum(run_gap == "end", na.rm = TRUE), Gap_Runs_Total = n(), .groups = "drop") %>%
    mutate(Boundary_Run_Pct = Boundary_Runs / Gap_Runs_Total) %>%
    select(Season, player_id, Team, Boundary_Run_Pct)

  goal_to_go_stats <- runs %>%
    filter(goal_to_go == 1, !is.na(yardline_100), !is.na(yards_gained)) %>%
    group_by(Season = season, player_id = rusher_player_id, Team = posteam) %>%
    summarise(Goal_To_Go_Conv_Rate = mean(yards_gained >= yardline_100, na.rm = TRUE), .groups = "drop")

  explosiveness_stats <- runs %>%
    filter(!is.na(yards_gained)) %>%
    group_by(Season = season, player_id = rusher_player_id, Team = posteam) %>%
    summarise(
      Explosive_Run_Rate = mean(yards_gained >= 10, na.rm = TRUE),
      Stuffed_Run_Rate   = mean(yards_gained <= 0, na.rm = TRUE),
      .groups = "drop"
    )

  power_stats <- runs %>%
    filter(down %in% c(3, 4), ydstogo <= 2, !is.na(yards_gained), !is.na(yardline_100)) %>%
    group_by(Season = season, player_id = rusher_player_id, Team = posteam) %>%
    summarise(Power_Success_Rate = mean(yards_gained >= ydstogo | yards_gained >= yardline_100, na.rm = TRUE), .groups = "drop")

  clock_killing_stats <- runs %>%
    filter(qtr == 4, !is.na(score_differential), score_differential >= 4) %>%
    count(Season = season, player_id = rusher_player_id, Team = posteam, name = "Clock_Killing_Rush_Att")

  leverage_stats <- runs %>%
    filter(!is.na(wp), !is.na(epa)) %>%
    group_by(Season = season, player_id = rusher_player_id, Team = posteam) %>%
    summarise(
      High_Leverage_EPA = mean(epa[wp >= 0.2 & wp <= 0.8], na.rm = TRUE),
      Garbage_Time_EPA  = mean(epa[wp < 0.05 | wp > 0.95], na.rm = TRUE),
      .groups = "drop"
    )

  gap_stats %>%
    full_join(goal_to_go_stats,   by = c("Season", "player_id", "Team")) %>%
    full_join(explosiveness_stats, by = c("Season", "player_id", "Team")) %>%
    full_join(power_stats,        by = c("Season", "player_id", "Team")) %>%
    full_join(clock_killing_stats, by = c("Season", "player_id", "Team")) %>%
    full_join(leverage_stats,     by = c("Season", "player_id", "Team")) %>%
    mutate(Clock_Killing_Rush_Att = coalesce(Clock_Killing_Rush_Att, 0L)) %>%
    as.data.frame()
}

#' Next Gen Stats rushing metrics -- tracking-derived, not obtainable
#' from standard play-by-play at all. Confirmed real columns before use
#' (rush_yards_over_expected, rush_yards_over_expected_per_att,
#' rush_pct_over_expected, efficiency, percent_attempts_gte_eight_
#' defenders, avg_time_to_los).
#'
#' Keyed by player_gsis_id -- NOT verified byte-for-byte identical to
#' the player_id scheme used elsewhere in this project, though both
#' follow the standard NFL GSIS ID convention and are expected to match
#' directly. Worth a direct join-rate spot-check before fully trusting
#' this function's output, same discipline as every new data source
#' introduced here.
#'
#' @param season the season to fetch (NGS data is fetched per-season,
#'   not as a multi-season pull, matching nflreadr's own interface)
fetch_player_ngs_rushing_stats <- function(season) {
  ngs <- nflreadr::load_nextgen_stats(season, stat_type = "rushing")
  # NOTE: week==0 assumed to be nflreadr's season-aggregate row, based
  # on general nflverse convention -- NOT independently verified in
  # this project the way every other column/value used here has been.
  # Worth confirming directly (e.g. table(ngs$week)) before trusting
  # this function's output at the season level.
  ngs %>%
    filter(!is.na(player_gsis_id), week == 0) %>%
    transmute(
      Season = season,
      player_id = player_gsis_id,
      RYOE_Per_Att      = rush_yards_over_expected_per_att,
      RYOE_Pct          = rush_pct_over_expected,
      NGS_Efficiency    = efficiency,
      Pct_Stacked_Box   = percent_attempts_gte_eight_defenders,
      Avg_Time_To_LOS   = avg_time_to_los
    ) %>%
    as.data.frame()
}

#' Next Gen Stats receiving metrics -- tracking-derived, not obtainable
#' from standard play-by-play at all. Confirmed real columns before use
#' (avg_separation, avg_cushion, avg_intended_air_yards,
#' percent_share_of_intended_air_yards, avg_yac_above_expectation).
#'
#' avg_separation and avg_yac_above_expectation are the receiving-side
#' analogues of RB's RYOE -- attempts to isolate a receiver's own skill
#' (route-running/separation ability; ability to generate yards after
#' the catch) from team/scheme context, the same "isolate skill from
#' opportunity" idea that made RYOE worth testing for RB.
#'
#' Same week==0 caveat as fetch_player_ngs_rushing_stats() -- assumed
#' season-aggregate row based on convention, not independently verified.
#'
#' @param season the season to fetch (NGS data is fetched per-season,
#'   not as a multi-season pull, matching nflreadr's own interface)
fetch_player_ngs_receiving_stats <- function(season) {
  ngs <- nflreadr::load_nextgen_stats(season, stat_type = "receiving")
  ngs %>%
    filter(!is.na(player_gsis_id), week == 0) %>%
    transmute(
      Season = season,
      player_id = player_gsis_id,
      Avg_Separation           = avg_separation,
      Avg_Cushion              = avg_cushion,
      Avg_Intended_Air_Yards   = avg_intended_air_yards,
      Pct_Share_Intended_Air_Yards = percent_share_of_intended_air_yards,
      YAC_Above_Expectation    = avg_yac_above_expectation
    ) %>%
    as.data.frame()
}

