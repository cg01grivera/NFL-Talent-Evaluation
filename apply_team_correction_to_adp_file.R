cat("
================================================================
  apply_team_correction_to_adp_file.R

  GOAL: Materialize a MULTI-TIER team correction back into a saved CSV,
  rather than leaving it as something that only happens transiently in
  memory. Tries progressively looser match strategies in order --
  strict name+position against real weekly game stats, then name-only
  against weekly stats (catches position-code disagreements, confirmed
  real example: Ty Montgomery, drafted as a WR, valued as an RB in
  fantasy, absent from some seasons' weekly stats under RB entirely),
  then strict name+position against ROSTER data (catches real players
  with zero game stats for a real reason -- a season-long holdout,
  injury, or suspension, confirmed real examples: Le'Veon Bell 2018,
  Andrew Luck 2017), then name-only against roster data as a last
  resort. Every row records EXACTLY which tier succeeded via Match_Tier
  -- essential for a file meant to be shared and trusted by other
  people, not just used internally.

  WHY EVERY ROW NEEDS THIS, NOT JUST THE PLACEHOLDER ONES: even rows
  that already had a real-looking team code from clean_fantasypros_
  adp.R's suffix parsing are still wrong for any year that isn't the
  player's CURRENT season -- the suffix reflects today's team, stamped
  uniformly onto every one of that player's historical rows (confirmed:
  this is why even a player's genuinely active, real old seasons show
  his current team, not his team that year). So this runs the
  correction on the WHOLE file, not just the placeholder rows.

  OUTPUT: Team_Raw (whatever came out of clean_fantasypros_adp.R, kept
  for comparison), Team (the actual corrected value), a Corrected flag
  (TRUE if the two differ -- NOT a reliable success/failure signal on
  its own: a player who never changed teams his whole career, e.g.
  Travis Kelce, correctly shows Corrected=FALSE even when matching
  worked perfectly), and Match_Tier (weekly_strict / weekly_name_only /
  roster_strict / roster_name_only / unmatched) -- the actually
  reliable signal for whether, and how confidently, each row's team was
  determined.

  A REAL LIMITATION, not a bug: rows with no matching real game OR
  roster data that year (confirmed earlier: the raw file's fabricated-
  looking placeholder rows for retired players in years after they
  actually stopped playing) will NOT get corrected -- there's no real
  team to correct them WITH. These stay at whatever Team_Raw already
  was. This is expected, not something this script can fix, since
  there's genuinely no real data to correct against.

  A FINAL, EXPLICIT MANUAL OVERRIDE PASS runs after the four automated
  tiers, for a small, human-confirmed list of exceptions the automation
  genuinely cannot resolve on its own. Two different kinds, not the
  same thing: a real team code, for players confirmed still rostered
  that season despite generating no stats/roster match (commonly:
  preseason injuries that may not appear in an earliest-week snapshot);
  or a deliberate NA, for players confirmed genuinely out of the league
  that season. NA here is a real, honest signal, not an error -- see
  load_historic_adp()'s VALID_NFL_TEAMS gate, which correctly drops any
  row with an invalid team code (NA included), excluding that player-
  season from every downstream analysis rather than leaving a guessed
  placeholder standing. See manual_team_overrides below to add entries
  -- keep this list small and confirmed, not a place for guesses.

  PARAMETERS:
    CLEANED_ADP_PATH   -- the cleaned ADP file (output of clean_fantasypros_adp.R)
    TEAM_CORRECTED_ADP_PATH  -- where the fully team-corrected CSV gets written
================================================================
\n")

library(dplyr)

source("R/utils_core.R")
source("R/grading_utils.R")
source("R/fetch_player_data.R")
source("R/adp_ppg_utils.R")

if (!exists("CLEANED_ADP_PATH")) CLEANED_ADP_PATH <- "data/historic_adp.csv"
if (!exists("TEAM_CORRECTED_ADP_PATH")) TEAM_CORRECTED_ADP_PATH <- "data/historic_adp_team_corrected.csv"
cat("=== Config: CLEANED_ADP_PATH =", CLEANED_ADP_PATH, "| TEAM_CORRECTED_ADP_PATH =", TEAM_CORRECTED_ADP_PATH, "===\n\n")
cat("NOTE: these persist in your R session once set. If a value above wasn't\n")
cat("intended (e.g. left over from a DIFFERENT script's run), clear it first:\n")
cat("rm(CLEANED_ADP_PATH, TEAM_CORRECTED_ADP_PATH)\n\n")

adp_raw <- load_historic_adp(CLEANED_ADP_PATH)
adp_raw$Team_Raw <- adp_raw$Team  # preserve the pre-correction value for comparison
years <- sort(unique(adp_raw$Year))
cat("Loaded", nrow(adp_raw), "rows across", length(years), "seasons (", min(years), "-", max(years), ")\n\n")

corrected_by_year <- list()
for (yr in years) {
  message("Correcting team assignments for ", yr, " (multi-tier: weekly stats, then roster data)...")
  weekly_yr <- nflreadr::load_player_stats(yr, summary_level = "week")
  if ("season_type" %in% names(weekly_yr)) weekly_yr <- weekly_yr %>% filter(season_type == "REG")
  primary_team_weekly_yr <- primary_team_from_weekly(weekly_yr)

  roster_yr <- nflreadr::load_rosters(yr)
  primary_team_roster_yr <- primary_team_from_roster(roster_yr)

  pool_yr <- adp_raw %>% filter(Year == yr)
  corrected_yr <- correct_adp_team_multi_tier(pool_yr, primary_team_weekly_yr, primary_team_roster_yr)
  corrected_by_year[[as.character(yr)]] <- corrected_yr
}

corrected_full <- do.call(rbind, corrected_by_year) %>%
  mutate(Corrected = Team_Raw != Team) %>%
  select(Year, Name, Pos, Pos_Rk, Team_Raw, Team, Corrected, Match_Tier, RANK, norm_name) %>%
  arrange(Year, RANK)

# ---- Manual overrides: applied LAST, after the automated multi-tier
# cascade. Two different kinds of case, NOT the same thing -- confirmed
# via direct human research, not automated inference:
#
#   Override_Team = a real team code -- for players who were genuinely
#     ROSTERED that season but the pipeline still couldn't find them
#     (commonly: injured/IR players who may not appear cleanly in the
#     earliest-week roster snapshot). We KNOW their real team, so we
#     supply it directly rather than leaving a wrong placeholder or
#     losing real information to a blanket NA.
#
#   Override_Team = NA -- for players CONFIRMED genuinely out of the
#     league that season (no real team exists, full stop). Using NA
#     here rather than a placeholder is a deliberate, honest signal --
#     see load_historic_adp()'s VALID_NFL_TEAMS gate, which drops any
#     row with an invalid team code (NA included) rather than erroring,
#     so this correctly excludes the player-season from every
#     downstream analysis, same as if the row never existed.
#
# Keep this list SMALL and confirmed -- every entry here should be
# something a human actually checked, not a guess. Add via (norm_name,
# Year) since that's the stable join key already used everywhere else.
# Base R data.frame, not tribble() -- tribble() needs the tibble
# package loaded explicitly, not a confirmed dependency elsewhere in
# this project.
manual_team_overrides <- data.frame(
  norm_name = c("jahvidbest", "terrellowens", "jordynelson", "kevinwhite", "kelvinbenjamin"),
  Year = c(2012, 2012, 2015, 2015, 2015),
  Override_Team = c(NA_character_, NA_character_, "GB", "CHI", "CAR"),
  stringsAsFactors = FALSE
)
# Add more rows here as you confirm them -- e.g. Dustin Keller (2013,
# MIA), Tim Hightower (2012, WAS), Marcus Lattimore (drafted but never
# played a real NFL down -- likely a genuine NA case, not a team).
# Each addition should be based on a real check, not an assumption.

# Has_Override is the disambiguating signal, NOT Override_Team itself --
# after the join, a row with NO override at all and a row deliberately
# overridden to NA (a confirmed out-of-league case) would otherwise
# both show Override_Team = NA, with no way to tell them apart. Has_
# Override is TRUE only for rows the join actually matched, regardless
# of what the override value is.
manual_team_overrides$Has_Override <- TRUE

corrected_full <- corrected_full %>%
  left_join(manual_team_overrides, by = c("norm_name", "Year")) %>%
  mutate(
    Match_Tier = ifelse(!is.na(Has_Override), "manual_override", Match_Tier),
    Team = ifelse(!is.na(Has_Override), Override_Team, Team)
  ) %>%
  select(-Override_Team, -Has_Override)

n_manual <- sum(corrected_full$Match_Tier == "manual_override", na.rm = TRUE)
cat("Manual overrides applied:", n_manual, "row(s) -- see manual_team_overrides in this script.\n\n")

n_corrected <- sum(corrected_full$Corrected, na.rm = TRUE)
tier_counts <- table(corrected_full$Match_Tier)
cat("\n=== Correction summary ===\n")
cat("Rows where team CHANGED after correction: ", n_corrected, "/", nrow(corrected_full),
    " (", round(100 * n_corrected / nrow(corrected_full), 1), "%)\n", sep = "")
cat("(This number alone is NOT a reliable success/failure signal -- a player who never\n")
cat(" changed teams his whole career will correctly show 0 changes even when matching\n")
cat(" worked perfectly. Use the Match_Tier breakdown below instead.)\n\n")
cat("=== Match_Tier breakdown (how each row's team was actually determined) ===\n")
for (tier_name in c("weekly_strict", "weekly_name_only", "roster_strict", "roster_name_only", "unmatched")) {
  n <- if (tier_name %in% names(tier_counts)) tier_counts[[tier_name]] else 0
  cat(sprintf("  %-20s %6d  (%5.1f%%)\n", tier_name, n, 100 * n / nrow(corrected_full)))
}
cat("\nweekly_strict = highest confidence (real game stats, exact position match).\n")
cat("weekly_name_only/roster_name_only = lower confidence (collapsed across positions --\n")
cat(" a genuine same-name collision at two different positions could pick the wrong one;\n")
cat(" review these specifically before fully trusting them). unmatched = genuinely no\n")
cat(" data to correct against (fabricated placeholder rows, or a still-unresolved gap).\n\n")

dir.create(dirname(TEAM_CORRECTED_ADP_PATH), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(corrected_full, TEAM_CORRECTED_ADP_PATH)
cat("Fully team-corrected file written to", TEAM_CORRECTED_ADP_PATH, "\n")

cat("\n=== Spot-check starter: a few well-known team-changers, all years ===\n")
cat("(Add your own norm_name checks here -- normalize_player_name() strips\n")
cat(" spaces/punctuation/case/suffixes, e.g. 'Peyton Manning' -> 'peytonmanning')\n\n")
spot_check_names <- c("peytonmanning", "tombrady", "drewbrees")
print(corrected_full %>% filter(norm_name %in% spot_check_names) %>% arrange(norm_name, Year),
      n = 100)
