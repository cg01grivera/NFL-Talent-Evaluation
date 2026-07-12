# ============================================================
# adp_ppg_utils.R
# ADP loading, top-150 pool definition, and season-PPG computation --
# extracted from nfl_team_grades' fantasy_target_analysis.R. Includes
# the norm_name+position join fix (see correct_adp_team()'s comment)
# discovered partway through that project's work -- built in from the
# start here rather than needing to be rediscovered.
#
# REQUIRES: data/historic_adp.csv to exist in this project (same file
# used by nfl_team_grades -- copy it over, see README).
# ============================================================
library(dplyr)

MIN_GAMES_FOR_PPG <- 4     # a player-season needs this many games
                            # before its PPG is trusted
MIN_POOLED_GAMES_FOR_PCTILE <- 8

#' Manual aliases for team codes in the historic ADP file that either
#' aren't standard nflverse codes, or aren't reliably caught by
#' nflreadr::clean_team_abbrs(). Confirmed by inspecting the actual file
#' in the parent project -- these are real, legitimate team references,
#' just not in nflverse's exact format.
adp_team_aliases <- c(
  "JAC" = "JAX", "NEP" = "NE", "NOS" = "NO", "GBP" = "GB", "TBB" = "TB",
  "KCC" = "KC", "SFO" = "SF", "LAR" = "LA", "OAK" = "LV", "SDC" = "LAC",
  "LVR" = "LV", "STL" = "LA", "HU" = "HOU"
)

# "FA"/"FA*" = Free Agent -- a legitimate "no team at ADP time"
# designation, not a data error.
FREE_AGENT_CODES <- c("FA", "FA*")

#' Manual overrides for player names in the historic ADP file that don't
#' match nflverse's player_display_name spelling. Confirmed in the
#' parent project via repeated multi-year match failures, not guessed.
adp_player_name_overrides <- c(
  "Robby Anderson"  = "Robbie Chosen",
  "Stevie Johnson"  = "Steve Johnson",
  "Gabriel Davis"   = "Gabe Davis",
  # Confirmed real name-matching failure: FantasyPros lists him by his
  # nickname ("Hollywood"), nflverse's official player_display_name
  # uses his legal first name. Without this, correct_adp_team() finds
  # no match in ANY season regardless of which team he was actually on
  # that year -- confirmed by his 2020/2022/2023 rows all showing the
  # same unchanged team despite genuinely different real teams those years.
  "Hollywood Brown" = "Marquise Brown",
  # All five below confirmed directly against real weekly stats data
  # (not guessed) before being added -- same discipline as Hollywood
  # Brown above.
  "Trenton Richardson"  = "Trent Richardson",
  "Chris Beanie Wells"  = "Chris Wells",
  "William Fuller V"    = "Will Fuller",
  "Nyheim Miller-Hines" = "Nyheim Hines",
  # Confirmed particularly important: his real 2021 team was PHI, not
  # the "TB" that had been sitting uncorrected in the ADP file --
  # the name mismatch ("Kenny" vs official "Kenneth") was preventing
  # correct_adp_team() from ever running for him, letting a genuinely
  # wrong team code stand uncorrected across multiple real seasons.
  "Kenny Gainwell"      = "Kenneth Gainwell",
  # Confirmed via direct data check: real nflverse name drops "Robert"/
  # "Joshua" in favor of the shorter form actually used in official
  # records.
  "Robert Kelley"       = "Rob Kelley",
  "Joshua Palmer"       = "Josh Palmer"
)

VALID_NFL_TEAMS <- c("ARI","ATL","BAL","BUF","CAR","CHI","CIN","CLE","DAL","DEN","DET","GB",
                      "HOU","IND","JAX","KC","LA","LAC","LV","MIA","MIN","NE","NO","NYG","NYJ",
                      "PHI","PIT","SEA","SF","TB","TEN","WAS")

#' Load and clean the historic ADP file.
load_historic_adp <- function(path = "data/historic_adp.csv") {
  adp <- readr::read_csv(path, show_col_types = FALSE)
  names(adp) <- gsub(" ", "_", names(adp))

  adp <- adp %>% filter(Pos %in% c("QB", "RB", "WR", "TE"))
  adp <- adp %>% filter(!Team %in% FREE_AGENT_CODES)

  adp$Team <- ifelse(adp$Team %in% names(adp_team_aliases), adp_team_aliases[adp$Team], adp$Team)
  adp$Team <- standardize_teams(adp$Team)

  invalid_rows <- adp[!adp$Team %in% VALID_NFL_TEAMS, ]
  if (nrow(invalid_rows) > 0) {
    warning(sprintf(
      "load_historic_adp: dropping %d row(s) with unrecognized team codes (likely source-file data corruption, not a real team). Affected players: %s",
      nrow(invalid_rows),
      paste(sprintf("%s (Team='%s', Year=%s)", invalid_rows$Name, invalid_rows$Team, invalid_rows$Year), collapse = "; ")
    ))
    adp <- adp[adp$Team %in% VALID_NFL_TEAMS, ]
  }

  adp$Name <- ifelse(adp$Name %in% names(adp_player_name_overrides),
                      adp_player_name_overrides[adp$Name], adp$Name)
  adp$norm_name <- normalize_player_name(adp$Name)
  adp
}

#' Top-150-by-ADP pool for one season.
top150_pool <- function(adp, season_yr) {
  adp %>% filter(Year == season_yr, RANK <= 150)
}

#' Compute each player's season half-PPR points-per-game, for EVERY
#' season present in the input data, using only player-seasons with
#' >= min_games games played. Returns one row per player-season
#' (combined across teams if traded, not fragmented per-team), with a
#' `primary_team` column (whichever team he was on for his earliest
#' recorded game that season) attached.
#'
#' Pass in the FULL historical range of weekly player_stats, not just
#' the seasons being tested -- this feeds both a target season's actual
#' PPG and any leave-one-out-style baseline that needs every other
#' year's data.
compute_season_ppg <- function(player_stats, min_games = MIN_GAMES_FOR_PPG) {
  player_stats <- player_stats %>% filter(position %in% c("QB", "RB", "WR", "TE"))
  player_stats$team <- standardize_teams(player_stats$team)

  weekly_norm <- player_stats %>%
    mutate(fantasy_points_half_ppr = fantasy_points + 0.5 * receptions,
           norm_name = normalize_player_name(player_display_name))

  season_totals <- weekly_norm %>%
    group_by(season, norm_name, player_display_name, position) %>%
    summarise(
      games_played = n(),
      season_ppg = mean(fantasy_points_half_ppr, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(games_played >= min_games)

  primary_team <- weekly_norm %>%
    group_by(season, norm_name) %>%
    slice_min(week, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(season, norm_name, primary_team = team)

  season_totals %>% left_join(primary_team, by = c("season", "norm_name"))
}

#' Correct a top150_pool()'s Team column using historically-accurate
#' team assignment, replacing ADP's (sometimes stale/future) Team
#' wherever real play data is available.
#'
#' @param primary_team_lookup a data.frame with norm_name, position,
#'   primary_team columns for the season in question.
correct_adp_team <- function(pool, primary_team_lookup) {
  # Join by norm_name AND position, not norm_name alone -- two different
  # skill-position players can share an exact display name in the same
  # season (confirmed real in the parent project: "David Johnson" 2016,
  # "Chris Thompson" 2017 -- an RB and a same-named player at a
  # different skill position, both active). Joining by norm_name alone
  # would fan out the pool the moment that happens.
  pool %>%
    left_join(primary_team_lookup, by = c("norm_name", "Pos" = "position")) %>%
    mutate(Team = ifelse(!is.na(primary_team), primary_team, Team)) %>%
    select(-primary_team)
}

#' Derive a primary-team lookup directly from raw weekly stats for ONE
#' season. Same earliest-week logic as compute_season_ppg()'s internal
#' primary_team.
#'
#' Restricted to QB/RB/WR/TE -- confirmed real bug without this filter:
#' the name-only matching tier (correct_adp_team_multi_tier()) joins on
#' name alone, with nothing to stop it from matching across into a
#' totally unrelated position if the raw weekly stats include every
#' position (defensive players, kickers, etc.). Concretely: the only
#' "Michael Thomas" in 2021's weekly stats turned out to be an unrelated
#' defensive back, and the real WR Michael Thomas (injury-limited that
#' season) generated no qualifying row at all -- the name-only fallback
#' then matched to the DB instead, silently assigning the wrong team.
#' The strict tier was never affected (it already requires an exact
#' position match), but the lookup table itself needs restricting
#' before ANY tier uses it, not just the strict one.
#'
#' FB (fullback) included alongside the four ADP skill positions --
#' confirmed real regression in the original fix above: Mike Tolbert
#' (and any real fullback) is drafted/valued as "RB" in fantasy/ADP
#' data, but nflverse classifies him as FB. Excluding FB entirely meant
#' fullbacks could never match at ANY tier, not even the name-only
#' fallback -- too aggressive a fix for the cross-position collision
#' problem, which was specifically about unrelated defensive positions,
#' not RB-adjacent ones like FB.
primary_team_from_weekly <- function(weekly_stats_season) {
  weekly_stats_season <- weekly_stats_season %>% filter(position %in% c("QB", "RB", "WR", "TE", "FB"))
  weekly_stats_season$team <- standardize_teams(weekly_stats_season$team)
  weekly_stats_season %>%
    mutate(norm_name = normalize_player_name(player_display_name)) %>%
    group_by(norm_name, position) %>%
    slice_min(week, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(norm_name, position, primary_team = team)
}

#' Same purpose as primary_team_from_weekly(), but sourced from ROSTER
#' data (nflreadr::load_rosters()) instead of game stats -- rosters
#' track contractual/roster affiliation independent of whether a player
#' recorded a single snap. This is the fallback for real, legitimately-
#' drafted players with zero weekly stats that season for a reason that
#' has nothing to do with data quality: a season-long holdout (confirmed
#' real example: Le'Veon Bell, 2018), a season-ending injury before ever
#' playing, or an in-season suspension covering the whole year. A player
#' genuinely out of the league entirely (confirmed real example: Tom
#' Brady's fabricated 2023/2024 ADP rows, both after his real
#' retirement) won't appear on ANY team's roster either, so this
#' correctly leaves those rows unmatched rather than inventing a team
#' for them.
primary_team_from_roster <- function(roster_season) {
  roster_season <- roster_season %>% filter(position %in% c("QB", "RB", "WR", "TE", "FB"))
  roster_season$team <- standardize_teams(roster_season$team)
  roster_season %>%
    mutate(norm_name = normalize_player_name(full_name)) %>%
    group_by(norm_name, position) %>%
    slice_min(week, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(norm_name, position, primary_team = team)
}

#' Multi-tier team correction: tries progressively looser match
#' strategies, in order, and records EXACTLY which one succeeded for
#' every row via Match_Tier -- essential for a file meant to be shared
#' and trusted by other people, not just used internally.
#'
#' Tier 1 (weekly_strict): name AND position both match real weekly
#'   game stats. Safest -- confirms real game participation under the
#'   exact position the ADP file also lists.
#' Tier 2 (weekly_name_only): name matches weekly stats, but NOT at the
#'   listed position -- catches real position-code disagreements
#'   between the ADP source and nflverse (confirmed real example: Ty
#'   Montgomery, drafted as a WR, functionally an RB in fantasy value,
#'   with no "Montgomery" appearing under RB at all in some seasons'
#'   weekly stats). Lower confidence than Tier 1 -- collapses to one
#'   row per name, so a genuine same-name collision across two
#'   different positions (rare, but the reason Tier 1 requires position
#'   at all) could pick the wrong one. Flagged explicitly so this is
#'   reviewable, not silently trusted.
#' Tier 3 (roster_strict): no weekly game data at all (any position),
#'   but the exact name+position appears on a real team roster that
#'   season. Catches genuine holdouts/season-long injuries/suspensions
#'   (confirmed real examples: Le'Veon Bell 2018, Andrew Luck 2017).
#' Tier 4 (roster_name_only): same idea as Tier 2, but against roster
#'   data instead of weekly stats -- lowest confidence, last resort.
#' unmatched: none of the four found anything. Genuinely no real data
#'   to correct against -- Team stays whatever it already was
#'   (confirmed real example: Tom Brady's fabricated 2023/2024 rows,
#'   both after his actual retirement -- correctly unmatched, since he
#'   isn't on any real team's roster or weekly stats those years either).
#'
#' @param pool a data.frame with norm_name, Pos, Team columns (an ADP pool)
#' @param weekly_lookup output of primary_team_from_weekly() for the same season
#' @param roster_lookup output of primary_team_from_roster() for the same season
correct_adp_team_multi_tier <- function(pool, weekly_lookup, roster_lookup) {
  weekly_strict <- weekly_lookup %>% transmute(norm_name, Pos = position, team_t1 = primary_team)
  weekly_name_only <- weekly_lookup %>%
    arrange(norm_name, position) %>%
    group_by(norm_name) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    transmute(norm_name, team_t2 = primary_team)
  roster_strict <- roster_lookup %>% transmute(norm_name, Pos = position, team_t3 = primary_team)
  roster_name_only <- roster_lookup %>%
    arrange(norm_name, position) %>%
    group_by(norm_name) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    transmute(norm_name, team_t4 = primary_team)

  pool %>%
    left_join(weekly_strict,    by = c("norm_name", "Pos")) %>%
    left_join(weekly_name_only, by = "norm_name") %>%
    left_join(roster_strict,    by = c("norm_name", "Pos")) %>%
    left_join(roster_name_only, by = "norm_name") %>%
    mutate(
      Match_Tier = case_when(
        !is.na(team_t1) ~ "weekly_strict",
        !is.na(team_t2) ~ "weekly_name_only",
        !is.na(team_t3) ~ "roster_strict",
        !is.na(team_t4) ~ "roster_name_only",
        TRUE ~ "unmatched"
      ),
      Team = case_when(
        !is.na(team_t1) ~ team_t1,
        !is.na(team_t2) ~ team_t2,
        !is.na(team_t3) ~ team_t3,
        !is.na(team_t4) ~ team_t4,
        TRUE ~ Team
      )
    ) %>%
    select(-team_t1, -team_t2, -team_t3, -team_t4)
}


