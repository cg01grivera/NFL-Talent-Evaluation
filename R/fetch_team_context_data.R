# ============================================================
# fetch_team_context_data.R
# Team-level context stats for the revisited, more comprehensive
# team-context battery -- lives in nfl_talent_eval (not the original
# nfl_team_grades project) specifically to reuse this project's ADP
# infrastructure (FantasyPros-sourced, multi-tier team correction,
# Prior_PPG baseline), per explicit request to test against "our
# updated ADP data."
#
# Every column referenced here was confirmed directly against a live
# names(nflreadr::load_pbp(2023)) pull and, for categorical columns,
# an actual table() of their values -- not assumed. Two stats proposed
# from the original candidate list (Motion Rate, Personnel Usage) were
# CUT ENTIRELY after verification found no corresponding column and no
# reasonable substitute -- not silently approximated.
#
# Requires utils_core.R (standardize_teams, filter_reg_pbp, clamp_seasons)
# to be sourced first.
# ============================================================
library(dplyr)

#' Team-level context stats for one season, built from a single pbp
#' pull. Returns one row per team.
#'
#' DEFINITIONS (confirmed against real column values, not assumed):
#'   Three-and-out rate    = fixed_drive_result=="Punt" AND drive_play_count<=3
#'                            (a punt alone isn't enough -- a long drive
#'                            that eventually punts is not a three-and-out)
#'   Scoring drive rate     = fixed_drive_result %in% c("Touchdown","Field goal")
#'   TD drive rate          = fixed_drive_result=="Touchdown" specifically,
#'                            kept separate from FG rate deliberately
#'   RZ trip rate           = drive_inside20==1 -- a DRIVE-level measure,
#'                            deliberately different from this project's
#'                            earlier PLAY-level yardline_100<=20 approach,
#'                            which could over-count a single possession's
#'                            multiple red-zone snaps as separate trips
#'   Deep attempt/completion % = pass_length=="deep" directly (confirmed
#'                            clean short/deep split, no air-yards
#'                            threshold guessing needed)
#'   OL penalty rate        = penalty_type %in% c("False Start","Offensive
#'                            Holding") specifically -- NOT "Defensive
#'                            Holding", a separate, correctly distinguished
#'                            category confirmed in the raw data
#'   Starting field position = yardline_100 of each drive's FIRST play,
#'                            not drive_start_yard_line (whose exact
#'                            encoding convention isn't confirmed --
#'                            this reuses an already-verified column
#'                            instead of a new guess)
#'   4th down aggressiveness = share of 4th-down plays where the team
#'                            ran or passed instead of punting/kicking a FG
#'
#' TEAM-CONTEXT REWORK V2 ADDITIONS (Gate 2, TEAM_CONTEXT_REWORK_PLAN_V2.md
#' Section 3.1/5) -- every column below re-verified live against a fresh
#' 2023 nflreadr::load_pbp() pull before coding, per this file's own
#' standing convention:
#'   Team_Sack_Rate_Allowed   = sack==1 share of dropbacks (dropback =
#'                            play_type=="pass" | sack==1, the SAME
#'                            denominator as Team_EPA_Per_Dropback above,
#'                            reusing `pass_plays` rather than a second
#'                            dropback definition). Confirmed live: sack
#'                            and qb_hit are both NA-free within this set.
#'   Team_QB_Hit_Rate_Allowed = qb_hit==1 share of the same dropback set.
#'   Team_Rush_Stuff_Rate    = share of DESIGNED rushes (play_type=="run"
#'                            AND qb_scramble==0 -- confirmed live:
#'                            qb_scramble is a clean 0/1 flag with zero NA
#'                            on every play_type=="run" row) with
#'                            yards_gained<=0. Confirmed live: yards_gained
#'                            has zero NA within this designed-rush set.
#'   Team_Shotgun_Rate       = shotgun==1 share of "called" plays
#'                            (play_type %in% c("pass","run")). Confirmed
#'                            live: shotgun is a clean 0/1 flag, zero NA,
#'                            across every play_type.
#'   Team_Goal_To_Go_Pass_Rate,
#'   Team_Goal_To_Go_Run_Rate = pass (resp. run) share of called plays
#'                            (play_type %in% c("pass","run")) where
#'                            goal_to_go==1 -- i.e. this team's OWN
#'                            propensity to pass vs. run once it reaches
#'                            a goal-to-go situation, not a share of all
#'                            offensive snaps. Confirmed live: goal_to_go
#'                            is a clean 0/1 flag, zero NA, and matches
#'                            ydstogo==yardline_100 EXACTLY (no
#'                            discrepancies) in a 2023 pull -- a fully
#'                            trustworthy column, not a guessed proxy.
#'                            These two rates are complementary by
#'                            construction (same numerator play_type
#'                            split, same denominator) and are reported
#'                            as separate named columns because the
#'                            exploratory tier and the C5 mechanism-
#'                            consistency check each reference only one
#'                            of the two.
#'
#' @param season the season to fetch
#' @param pbp optional pre-loaded play-by-play, for sharing across calls
#' @param weekly_stats optional pre-loaded weekly player stats (needed
#'   for target-share-by-position; avoids a second fetch if the caller
#'   already has it)
fetch_team_context_stats <- function(season, pbp = NULL, weekly_stats = NULL) {
  if (is.null(pbp)) pbp <- nflreadr::load_pbp(clamp_seasons(season))
  if ("season" %in% names(pbp)) pbp <- pbp %>% filter(season == !!season)
  pbp <- filter_reg_pbp(pbp)
  pbp$posteam <- standardize_teams(pbp$posteam)

  offense_plays <- pbp %>% filter(!is.na(posteam), posteam != "")

  # ---- Passing efficiency (team-level, not individual QB) ----
  pass_plays <- offense_plays %>% filter(play_type == "pass" | sack == 1)
  passing_stats <- pass_plays %>%
    group_by(Team = posteam) %>%
    summarise(
      Team_EPA_Per_Dropback = mean(epa, na.rm = TRUE),
      Team_CPOE             = mean(cpoe, na.rm = TRUE),
      Team_aDOT             = mean(air_yards, na.rm = TRUE),
      Deep_Attempts         = sum(pass_length == "deep", na.rm = TRUE),
      Deep_Completions      = sum(pass_length == "deep" & complete_pass == 1, na.rm = TRUE),
      Total_Attempts        = sum(play_type == "pass", na.rm = TRUE),
      # Reuses this SAME dropback set (pass_plays) for the OL sack/hit
      # components -- "same denominator as Team_EPA_Per_Dropback" per
      # TEAM_CONTEXT_REWORK_PLAN_V2.md Section 3.1, not a second,
      # possibly-divergent dropback definition.
      Team_Sack_Rate_Allowed   = mean(sack == 1, na.rm = TRUE),
      Team_QB_Hit_Rate_Allowed = mean(qb_hit == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Team_Deep_Attempt_Pct    = ifelse(Total_Attempts > 0, Deep_Attempts / Total_Attempts, NA_real_),
      Team_Deep_Completion_Pct = ifelse(Deep_Attempts > 0, Deep_Completions / Deep_Attempts, NA_real_)
    ) %>%
    select(Team, Team_EPA_Per_Dropback, Team_CPOE, Team_aDOT, Team_Deep_Attempt_Pct, Team_Deep_Completion_Pct,
           Team_Sack_Rate_Allowed, Team_QB_Hit_Rate_Allowed)

  # ---- Game script (leading/trailing snap share) ----
  game_script <- offense_plays %>%
    filter(!is.na(score_differential)) %>%
    group_by(Team = posteam) %>%
    summarise(
      Team_Leading_Snap_Pct  = mean(score_differential > 0, na.rm = TRUE),
      Team_Trailing_Snap_Pct = mean(score_differential < 0, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- OL penalties (False Start + Offensive Holding specifically) ----
  penalty_stats <- offense_plays %>%
    group_by(Team = posteam) %>%
    summarise(
      OL_Penalties  = sum(penalty == 1 & penalty_team == posteam &
                           penalty_type %in% c("False Start", "Offensive Holding"), na.rm = TRUE),
      Total_Plays   = n(),
      .groups = "drop"
    ) %>%
    mutate(Team_OL_Penalty_Rate = ifelse(Total_Plays > 0, OL_Penalties / Total_Plays, NA_real_)) %>%
    select(Team, Team_OL_Penalty_Rate)

  # ---- 4th down aggressiveness ----
  fourth_downs <- offense_plays %>% filter(down == 4, play_type %in% c("run", "pass", "punt", "field_goal"))
  fourth_down_stats <- fourth_downs %>%
    group_by(Team = posteam) %>%
    summarise(
      Team_4th_Down_Aggressiveness = mean(play_type %in% c("run", "pass"), na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Weekly variance (points, EPA) -- a genuinely new dimension,
  # not just another mean-based stat. Computed from the SAME pbp,
  # grouped by week, then the SD taken across weeks per team.
  weekly_epa <- offense_plays %>%
    filter(!is.na(epa)) %>%
    group_by(Team = posteam, week) %>%
    summarise(week_epa = mean(epa, na.rm = TRUE), .groups = "drop")
  weekly_points <- pbp %>%
    filter(!is.na(posteam_score_post)) %>%
    group_by(Team = posteam, week) %>%
    summarise(week_score = max(posteam_score_post, na.rm = TRUE), .groups = "drop")
  variance_stats <- weekly_epa %>%
    group_by(Team) %>%
    summarise(Team_Weekly_EPA_Variance = sd(week_epa, na.rm = TRUE), .groups = "drop") %>%
    full_join(
      weekly_points %>% group_by(Team) %>%
        summarise(Team_Weekly_Points_Variance = sd(week_score, na.rm = TRUE), .groups = "drop"),
      by = "Team"
    )

  # ---- Drive-level metrics (three-and-out, scoring drive rate, TD
  # drive rate, RZ trip rate, plays/drive) -- deduplicated to one row
  # per real drive first, since pbp is play-level and a drive spans
  # many rows.
  drives <- offense_plays %>%
    filter(!is.na(fixed_drive)) %>%
    # game_id included in the grouping deliberately -- fixed_drive is a
    # per-game sequential counter (drive #1, #2, #3...), NOT a globally
    # unique ID across the season. Grouping by (Team, fixed_drive) alone
    # would incorrectly merge "drive #1" from every game that team
    # played that season into a single row.
    group_by(Team = posteam, game_id, fixed_drive) %>%
    summarise(
      drive_result = dplyr::first(fixed_drive_result),
      play_count   = dplyr::first(drive_play_count),
      inside20     = dplyr::first(drive_inside20),
      start_field_pos = dplyr::first(yardline_100),
      .groups = "drop"
    )
  drive_stats <- drives %>%
    group_by(Team) %>%
    summarise(
      Team_Three_And_Out_Rate      = mean(drive_result == "Punt" & play_count <= 3, na.rm = TRUE),
      Team_Scoring_Drive_Rate      = mean(drive_result %in% c("Touchdown", "Field goal"), na.rm = TRUE),
      Team_TD_Drive_Rate           = mean(drive_result == "Touchdown", na.rm = TRUE),
      Team_RZ_Trip_Rate            = mean(inside20 == 1, na.rm = TRUE),
      Team_Avg_Plays_Per_Drive     = mean(play_count, na.rm = TRUE),
      Team_Avg_Starting_Field_Pos  = mean(start_field_pos, na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Target share by receiver position -- joins pbp's
  # receiver_player_id to a player_id -> position lookup built from
  # weekly stats, reusing already-proven infrastructure rather than
  # depending on an unverified position column inside pbp itself.
  if (is.null(weekly_stats)) weekly_stats <- nflreadr::load_player_stats(season, summary_level = "week")
  position_lookup <- weekly_stats %>% distinct(player_id, position)
  targets <- offense_plays %>%
    filter(play_type == "pass", !is.na(receiver_player_id)) %>%
    left_join(position_lookup, by = c("receiver_player_id" = "player_id"))
  target_share_stats <- targets %>%
    group_by(Team = posteam) %>%
    summarise(
      Team_TE_Target_Share = mean(position == "TE", na.rm = TRUE),
      Team_WR_Target_Share = mean(position == "WR", na.rm = TRUE),
      Team_RB_Target_Share = mean(position == "RB", na.rm = TRUE),
      .groups = "drop"
    )

  # ---- PROE and raw pace -- ported directly from nfl_team_grades'
  # fetch_team_pace_proe_stats(), same validated methodology, not
  # reinvented. Re-included here specifically because its prior null
  # result was tested under the old ADP source + RANK-only baseline;
  # Prior_PPG makes the bar HARDER (arguing against re-discovery), but
  # the ADP-source change alone is enough to warrant a fresh look given
  # this project's own recent evidence that source changes can move
  # results meaningfully (RZ_Rush_Att_PG's QB result, for one).
  neutral <- pbp %>%
    filter(play_type %in% c("pass", "run"), !is.na(posteam),
           !is.na(score_differential), abs(score_differential) <= 7,
           !is.na(half_seconds_remaining), half_seconds_remaining > 120)
  raw_plays <- pbp %>%
    filter(play_type %in% c("pass", "run"), !is.na(posteam)) %>%
    count(Team = posteam, name = "raw_plays")
  games_played <- pbp %>%
    filter(!is.na(posteam)) %>%
    distinct(posteam, game_id) %>%
    count(Team = posteam, name = "team_games")
  neutral_binned <- neutral %>%
    filter(!is.na(down), !is.na(ydstogo)) %>%
    mutate(dist_bucket = dplyr::case_when(
      ydstogo <= 3 ~ "short", ydstogo <= 7 ~ "medium", TRUE ~ "long"
    ))
  league_expected <- neutral_binned %>%
    group_by(down, dist_bucket) %>%
    summarise(league_pass_rate = mean(play_type == "pass", na.rm = TRUE), .groups = "drop")
  team_binned_pass_rate <- neutral_binned %>%
    group_by(Team = posteam, down, dist_bucket) %>%
    summarise(plays = n(), team_pass_rate_bucket = mean(play_type == "pass", na.rm = TRUE), .groups = "drop") %>%
    left_join(league_expected, by = c("down", "dist_bucket")) %>%
    mutate(pass_oe_bucket = team_pass_rate_bucket - league_pass_rate)
  pace_stats <- team_binned_pass_rate %>%
    group_by(Team) %>%
    summarise(Team_PROE = weighted.mean(pass_oe_bucket, plays, na.rm = TRUE), .groups = "drop") %>%
    full_join(raw_plays, by = "Team") %>%
    full_join(games_played, by = "Team") %>%
    mutate(Team_Raw_Plays_PG = raw_plays / team_games) %>%
    select(Team, Team_PROE, Team_Raw_Plays_PG)

  # ---- Rush stuff rate (designed rushes only) -- excludes qb_scramble
  # so a QB's own scramble decision-making doesn't get attributed to the
  # OL/run-blocking construct this stat feeds (TEAM_CONTEXT_REWORK_PLAN_
  # V2.md Section 3.1's declared contamination boundary).
  designed_rushes <- offense_plays %>% filter(play_type == "run", qb_scramble == 0)
  rush_stuff_stats <- designed_rushes %>%
    group_by(Team = posteam) %>%
    summarise(
      Stuffed_Rushes = sum(yards_gained <= 0, na.rm = TRUE),
      Total_Rushes   = n(),
      .groups = "drop"
    ) %>%
    mutate(Team_Rush_Stuff_Rate = ifelse(Total_Rushes > 0, Stuffed_Rushes / Total_Rushes, NA_real_)) %>%
    select(Team, Team_Rush_Stuff_Rate)

  # ---- Shotgun rate, over "called" plays (play_type %in% c("pass","run")) --
  # the same real-play denominator convention this file already uses
  # elsewhere (e.g. the pace stats' raw_plays/neutral sets), not every raw
  # pbp row (kickoffs, punts, kneels, etc. carry no real formation signal).
  called_plays <- offense_plays %>% filter(play_type %in% c("pass", "run"))
  shotgun_stats <- called_plays %>%
    group_by(Team = posteam) %>%
    summarise(
      Shotgun_Plays       = sum(shotgun == 1, na.rm = TRUE),
      Total_Called_Plays  = n(),
      .groups = "drop"
    ) %>%
    mutate(Team_Shotgun_Rate = ifelse(Total_Called_Plays > 0, Shotgun_Plays / Total_Called_Plays, NA_real_)) %>%
    select(Team, Team_Shotgun_Rate)

  # ---- Goal-to-go pass/run split -- this team's OWN propensity to pass
  # vs. run once it reaches goal_to_go==1, not a share of all offensive
  # snaps (see the docstring above for the C5 mechanism-consistency use).
  goal_to_go_plays <- offense_plays %>% filter(goal_to_go == 1, play_type %in% c("pass", "run"))
  goal_to_go_stats <- goal_to_go_plays %>%
    group_by(Team = posteam) %>%
    summarise(
      GTG_Pass_Plays  = sum(play_type == "pass", na.rm = TRUE),
      GTG_Run_Plays   = sum(play_type == "run", na.rm = TRUE),
      Total_GTG_Plays = n(),
      .groups = "drop"
    ) %>%
    mutate(
      Team_Goal_To_Go_Pass_Rate = ifelse(Total_GTG_Plays > 0, GTG_Pass_Plays / Total_GTG_Plays, NA_real_),
      Team_Goal_To_Go_Run_Rate  = ifelse(Total_GTG_Plays > 0, GTG_Run_Plays / Total_GTG_Plays, NA_real_)
    ) %>%
    select(Team, Team_Goal_To_Go_Pass_Rate, Team_Goal_To_Go_Run_Rate)

  passing_stats %>%
    full_join(game_script,        by = "Team") %>%
    full_join(penalty_stats,      by = "Team") %>%
    full_join(fourth_down_stats,  by = "Team") %>%
    full_join(variance_stats,     by = "Team") %>%
    full_join(drive_stats,        by = "Team") %>%
    full_join(target_share_stats, by = "Team") %>%
    full_join(pace_stats,         by = "Team") %>%
    full_join(rush_stuff_stats,   by = "Team") %>%
    full_join(shotgun_stats,      by = "Team") %>%
    full_join(goal_to_go_stats,   by = "Team") %>%
    mutate(Season = season) %>%
    as.data.frame()
}
