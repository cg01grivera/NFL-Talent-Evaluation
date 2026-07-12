cat("
================================================================
  diagnose_uncorrected_team_rows.R

  GOAL: Quantify how many rows in the team-corrected ADP file have NO
  match at ANY tier of the multi-tier correction (weekly stats or
  roster data, strict or name-only -- see apply_team_correction_to_
  adp_file.R for the full cascade), via Match_Tier == 'unmatched' --
  NOT Team_Raw == Team, which was a confirmed real bug in an earlier
  version of this script: any player who never changed teams his whole
  career, e.g. Travis Kelce, Josh Allen, Justin Jefferson, correctly
  shows 'unchanged' even when matching worked perfectly, since his
  current-team suffix already happened to equal his historical team
  every year -- that's not a failure, and treating it as one flooded
  the flagged list with false positives.

  Of the genuinely unmatched rows, this flags which are likely REAL
  holdout/season-long-injury/suspension cases (confirmed real examples:
  Le'Veon Bell 2018, Andrew Luck 2017, Deshaun Watson 2021 -- all
  legitimate, important rostered players with zero real game OR roster
  data matching that specific season) versus likely-fabricated
  placeholder rows for genuinely retired/out-of-league players
  (confirmed real example: Tom Brady's 2023/2024 rows, both well after
  his actual retirement, matching nothing at any tier since he isn't on
  any real team's roster those years either).

  Also separately surfaces rows matched via a LOWER-CONFIDENCE tier
  (weekly_name_only / roster_name_only) -- these got a team, so they
  won't show up in the 'unmatched' list, but the name-only match
  strategy collapses across positions and carries real collision risk
  (a genuine same-name-different-position collision could silently pick
  the wrong team), so they deserve their own explicit review pass for a
  file meant to be shared and trusted by other people.

  PARAMETERS:
    TEAM_CORRECTED_ADP_PATH        -- the team-corrected ADP file to diagnose
    RANK_THRESHOLD     -- only rows at or above this ADP rank get
                          flagged as 'worth a human look' (default 100
                          -- a rank-283 fabricated Brady row shouldn't
                          show up here, a rank-2 Bell row should)
================================================================
\n")

library(dplyr)

if (!exists("TEAM_CORRECTED_ADP_PATH")) TEAM_CORRECTED_ADP_PATH <- "data/historic_adp_team_corrected.csv"
if (!exists("RANK_THRESHOLD")) RANK_THRESHOLD <- 100
cat("=== Config: TEAM_CORRECTED_ADP_PATH =", TEAM_CORRECTED_ADP_PATH, "| RANK_THRESHOLD =", RANK_THRESHOLD, "===\n\n")
cat("NOTE: these persist in your R session once set. If a value above wasn't\n")
cat("intended (e.g. left over from a DIFFERENT script's run), clear it first:\n")
cat("rm(TEAM_CORRECTED_ADP_PATH, RANK_THRESHOLD)\n\n")

corrected <- readr::read_csv(TEAM_CORRECTED_ADP_PATH, show_col_types = FALSE)

still_uncorrected <- corrected %>% filter(Match_Tier == "unmatched")
cat("Total rows:", nrow(corrected), "\n")
cat("Rows with NO match found at ANY tier (weekly stats or roster data, strict or\n")
cat(" name-only) -- genuinely uncorrected:", nrow(still_uncorrected),
    " (", round(100 * nrow(still_uncorrected) / nrow(corrected), 1), "%)\n\n", sep = "")

# The ones actually worth a human look: high enough ADP rank that a
# real person clearly drafted them seriously that year. Confirmed real
# example this threshold should catch: Le'Veon Bell, 2018, RANK ~2.
flagged <- still_uncorrected %>%
  filter(RANK <= RANK_THRESHOLD) %>%
  arrange(Year, RANK)

cat("Of those, rows at or above RANK", RANK_THRESHOLD, "(worth a human look, not likely fabricated noise):",
    nrow(flagged), "\n\n")

cat("=== Every flagged row -- check each against your own knowledge of that season ===\n")
print(flagged %>% select(Year, Name, Pos, RANK, Team_Raw), n = 200)

dir.create("output", showWarnings = FALSE)
readr::write_csv(flagged, "output/flagged_uncorrected_high_rank_rows.csv")
cat("\nFull flagged list written to output/flagged_uncorrected_high_rank_rows.csv\n")
cat("\nNOTE: this list will include BOTH real holdout/injury cases (like Bell) AND any\n")
cat("high-ADP player who, for some other reason, has no matching weekly-stats row that\n")
cat("season (e.g. a data-source gap). Each one needs a quick human check, not an\n")
cat("assumption either way -- that's exactly why this is surfaced rather than auto-fixed.\n")

# name_only tier rows are NOT unmatched -- they got a team, but via a
# lower-confidence path (collapsed across positions, so a genuine
# same-name collision at two different positions could pick the wrong
# one). Worth a separate, explicit review pass for a file meant to be
# shared, distinct from the fully-unmatched list above.
name_only_matches <- corrected %>%
  filter(Match_Tier %in% c("weekly_name_only", "roster_name_only"), RANK <= RANK_THRESHOLD) %>%
  arrange(Year, RANK)
cat("\n=== Rows matched via a lower-confidence name-only tier (RANK <=", RANK_THRESHOLD,
    ") -- got a team, but worth a second look given the collision risk ===\n")
if (nrow(name_only_matches) > 0) {
  print(name_only_matches %>% select(Year, Name, Pos, RANK, Team_Raw, Team, Match_Tier), n = 200)
  readr::write_csv(name_only_matches, "output/name_only_tier_matches_high_rank.csv")
  cat("\nWritten to output/name_only_tier_matches_high_rank.csv\n")
} else {
  cat("None at this rank threshold.\n")
}
