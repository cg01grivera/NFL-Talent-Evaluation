cat("
================================================================
  clean_fantasypros_adp.R

  GOAL: Reshape the raw FantasyPros historic ADP export (Year, Rank,
  'Player (Bye)', POS, per-platform columns, AVG) into this project's
  native ADP format (Year, Name, Pos, Team, RANK) so it drops straight
  into load_historic_adp() unmodified.

  THREE THINGS THIS FIXES:
  1. Player names carry a CURRENT-team + bye-week suffix baked in
     ('Aaron Rodgers   PIT (4)') that's wrong for any non-current year
     -- stripped out here, since correct_adp_team() already overwrites
     Team downstream with the real historically-accurate value from
     actual game logs regardless of what we seed it with.
  2. Position and position-rank are combined in one cell ('QB11') --
     split into Pos (letters) and Pos_Rk (digits) here.
  3. Players with no current team (retired/inactive as of file
     generation -- confirmed this is a PER-PLAYER property, not
     per-row: e.g. Tom Brady has no suffix even in his own genuinely
     active 2012-2022 rows) get a placeholder valid team code seeded
     in, so load_historic_adp()'s valid-team-code gate doesn't drop
     their real historical rows entirely before correct_adp_team() can
     fix them.

  NOTE ON A DATA QUALITY ISSUE, not fixed here because it doesn't need
  to be: the raw file contains fabricated-looking rows for retired
  players in years after they actually stopped playing (confirmed:
  Brady shows a plausible QB34/rank-283 in 2023, a year after his real
  retirement) -- almost certainly a rectangularity placeholder the
  file's generator inserts, not real ADP. NOT specially filtered here
  because every downstream analyze script already inner_join()s against
  real season outcomes (compute_season_ppg()) -- a fabricated row for a
  season the player didn't actually play has nothing to join against
  and drops out on its own, the same safety net already relied on
  elsewhere in this project.

  PARAMETERS:
    RAW_ADP_PATH   -- path to the raw FantasyPros export
    CLEANED_ADP_PATH  -- where the cleaned, native-format CSV gets written
    PLACEHOLDER_TEAM -- valid team code seeded for suffix-less rows,
                        fully expected to be overwritten by
                        correct_adp_team() wherever real season data
                        exists
================================================================
\n")

library(dplyr)

if (!exists("RAW_ADP_PATH")) RAW_ADP_PATH <- "data/FantasyProsHistoricADP.csv"
if (!exists("CLEANED_ADP_PATH")) CLEANED_ADP_PATH <- "data/historic_adp.csv"
if (!exists("PLACEHOLDER_TEAM")) PLACEHOLDER_TEAM <- "ARI"  # arbitrary but valid -- see header note
cat("=== Config: RAW_ADP_PATH =", RAW_ADP_PATH, "| CLEANED_ADP_PATH =", CLEANED_ADP_PATH,
    "| PLACEHOLDER_TEAM =", PLACEHOLDER_TEAM, "===\n\n")
cat("NOTE: these persist in your R session once set. If a value above wasn't\n")
cat("intended (e.g. left over from a DIFFERENT script's run), clear it first:\n")
cat("rm(RAW_ADP_PATH, CLEANED_ADP_PATH, PLACEHOLDER_TEAM)\n\n")

raw <- readr::read_csv(RAW_ADP_PATH, show_col_types = FALSE)
cat("Raw rows read:", nrow(raw), "\n")

# Player name parsing: "Name   TEAM (BYE)" -- non-greedy up to the
# double-space separator, so suffixes like "Jr."/"Sr."/"II" stay part
# of the name (confirmed against real examples: "Odell Beckham Jr.",
# "Patrick Mahomes II" both parse correctly with this pattern).
#
# CONFIRMED REAL SECOND FORMAT: a small, scattered number of rows
# (roughly 1-7 per year, 2012-2019 only, zero from 2020 on -- not a
# clean year-based split, just inconsistent source data) have a team
# code with NO bye-week parenthetical at all: "Matthew Stafford   LAR"
# instead of "Matthew Stafford   LAR (8)". The original single pattern
# required the bye-week parens to match anything, so these rows fell
# through entirely -- the team code ended up stuck inside Name instead
# of being extracted, corrupting the name field itself, not just
# leaving Team uncorrected. Two patterns tried in order below, first
# match wins.
name_pattern_with_bye <- "^(.*?)\\s{2,}([A-Z]{2,4})\\s+\\(\\s*[0-9]+\\s*\\)\\s*$"
name_pattern_team_only <- "^(.*?)\\s{2,}([A-Z]{2,4})\\s*$"

parsed <- raw %>%
  mutate(
    has_bye_suffix  = grepl(name_pattern_with_bye, `Player (Bye)`),
    has_team_only   = !has_bye_suffix & grepl(name_pattern_team_only, `Player (Bye)`),
    has_suffix      = has_bye_suffix | has_team_only,
    Name = case_when(
      has_bye_suffix ~ sub(name_pattern_with_bye, "\\1", `Player (Bye)`),
      has_team_only  ~ sub(name_pattern_team_only, "\\1", `Player (Bye)`),
      TRUE ~ `Player (Bye)`
    ),
    Team = case_when(
      has_bye_suffix ~ sub(name_pattern_with_bye, "\\2", `Player (Bye)`),
      has_team_only  ~ sub(name_pattern_team_only, "\\2", `Player (Bye)`),
      TRUE ~ NA_character_
    ),
    # POS = letters (position) + digits (position rank), e.g. "QB11".
    # A handful of rows (confirmed: 21 in the raw file, all defensive
    # positions like "CB"/"S"/"DT" with no trailing number) don't match
    # this pattern at all -- Pos_Rk is simply NA for those, and they get
    # filtered out below regardless since they're not QB/RB/WR/TE.
    Pos = sub("^([A-Za-z]+).*$", "\\1", POS),
    Pos_Rk = suppressWarnings(as.integer(sub("^[A-Za-z]+([0-9]*)$", "\\1", POS))),
    RANK = Rank
  )

cat("Rows with an extractable current-team suffix:", sum(parsed$has_suffix), "/", nrow(parsed), "\n")

cleaned <- parsed %>%
  filter(Pos %in% c("QB", "RB", "WR", "TE")) %>%
  mutate(Team = ifelse(is.na(Team), PLACEHOLDER_TEAM, Team)) %>%
  select(Year, Name, Pos, Pos_Rk, Team, RANK) %>%
  arrange(Year, RANK)

cat("Rows after filtering to QB/RB/WR/TE:", nrow(cleaned), "\n")
cat("Rows seeded with placeholder team (no real suffix available):",
    sum(parsed$Pos %in% c("QB","RB","WR","TE") & is.na(parsed$Team)), "\n")
cat("(These rely entirely on correct_adp_team() downstream to get a real team --\n")
cat(" confirm that function still runs on this file before trusting Team for any row.)\n\n")

dir.create(dirname(CLEANED_ADP_PATH), showWarnings = FALSE, recursive = TRUE)
readr::write_csv(cleaned, CLEANED_ADP_PATH)
cat("Cleaned ADP file written to", CLEANED_ADP_PATH, "\n")

cat("\n=== Spot-check: a few rows with each name-parsing case ===\n")
print(cleaned %>% filter(Year == 2025) %>% slice_head(n = 5))
