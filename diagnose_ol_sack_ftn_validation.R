cat("
================================================================
  diagnose_ol_sack_ftn_validation.R

  GOAL: TEAM_CONTEXT_REWORK_PLAN_V2.md Section 3.2 V5b -- REPORT-ONLY.
  The sack component (Team_Sack_Rate_Allowed) is declared-contaminated
  by QB time-to-throw/scramble tendency (Section 3.1). FTN charting's
  is_qb_fault_sack flag (2022+, nflreadr::load_ftn_charting()) is a
  direct empirical handle on that contamination.

  Deliverables:
    (i)  QB-fault share of team sacks, and its team-to-team spread.
    (ii) Correlation between raw Team_Sack_Rate_Allowed and a
         QB-fault-EXCLUDED version, across the 2022+ subsample.

  This gate CANNOT change OL composite membership (that's V2-V3's job,
  already resolved) -- it only changes how much the sack component's
  contribution is trusted in interpretation. High correlation -> the
  long-history raw proxy is validated. Low correlation -> the
  contamination caveat is elevated in all reporting and a sensitivity
  composite substituting the adjusted rate becomes a required
  robustness output. FTN's short history is validation-only, never
  battery input.

  COLUMN VERIFICATION (live, this run): nflverse_game_id/nflverse_
  play_id join FTN to pbp's game_id/play_id with 100% coverage on
  sack==1 rows (1356/1356 sacks joined, 0 NA is_qb_fault_sack) in a
  2022 pbp+FTN pull. 2025 FTN charting is NOT YET RELEASED (SSL/404 on
  the nflverse-data release asset as of this run) -- subsample is
  2022-2024 only, not 2022-2025 as originally hoped.
================================================================
\n")

if (!requireNamespace("nflreadr", quietly = TRUE)) install.packages("nflreadr", repos = "https://cloud.r-project.org")
library(dplyr)

script_arg <- grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) > 0) setwd(dirname(sub("--file=", "", script_arg)))

source("R/utils_core.R")

# ---- CONFIG ---------------------------------------------------
if (!exists("FTN_SEASONS")) FTN_SEASONS <- 2022:2024   # 2025 not yet released as of this run
cat("=== Config: FTN_SEASONS =", paste(range(FTN_SEASONS), collapse = "-"), "===\n\n")
# -----------------------------------------------------------------

message("Loading pbp + FTN charting for ", min(FTN_SEASONS), "-", max(FTN_SEASONS), "...")
pbp <- nflreadr::load_pbp(FTN_SEASONS)
pbp <- filter_reg_pbp(pbp)
pbp$posteam <- standardize_teams(pbp$posteam)
ftn <- nflreadr::load_ftn_charting(FTN_SEASONS)

dropbacks <- pbp %>% filter(!is.na(posteam), posteam != "", play_type == "pass" | sack == 1)
sacks <- dropbacks %>% filter(sack == 1) %>%
  left_join(ftn %>% select(nflverse_game_id, nflverse_play_id, is_qb_fault_sack),
            by = c("game_id" = "nflverse_game_id", "play_id" = "nflverse_play_id"))

cat("Sacks in dropback set:", nrow(sacks), " | matched to FTN:", sum(!is.na(sacks$is_qb_fault_sack)), "\n")
unmatched <- sum(is.na(sacks$is_qb_fault_sack))
if (unmatched > 0) cat("WARNING:", unmatched, "sacks did not match an FTN row -- excluded from QB-fault accounting below.\n")

team_sack_stats <- sacks %>%
  filter(!is.na(is_qb_fault_sack)) %>%
  group_by(Team = posteam, Season = season) %>%
  summarise(Total_Sacks = n(), QB_Fault_Sacks = sum(is_qb_fault_sack), .groups = "drop") %>%
  mutate(QB_Fault_Share = QB_Fault_Sacks / Total_Sacks)

dropback_counts <- dropbacks %>% group_by(Team = posteam, Season = season) %>% summarise(N_Dropbacks = n(), .groups = "drop")

# Total sacks per team-season (unmatched sacks still counted in the RAW
# rate -- only the QB-fault split itself is restricted to matched rows)
raw_sack_counts <- dropbacks %>% filter(sack == 1) %>% group_by(Team = posteam, Season = season) %>%
  summarise(Raw_Sacks = n(), .groups = "drop")

team_season <- dropback_counts %>%
  left_join(raw_sack_counts, by = c("Team", "Season")) %>%
  left_join(team_sack_stats, by = c("Team", "Season")) %>%
  mutate(
    Raw_Sacks = ifelse(is.na(Raw_Sacks), 0, Raw_Sacks),
    Total_Sacks = ifelse(is.na(Total_Sacks), 0, Total_Sacks),
    QB_Fault_Sacks = ifelse(is.na(QB_Fault_Sacks), 0, QB_Fault_Sacks),
    Team_Sack_Rate_Allowed_Raw = Raw_Sacks / N_Dropbacks,
    Team_Sack_Rate_Allowed_QBFaultExcluded = (Raw_Sacks - QB_Fault_Sacks) / N_Dropbacks,
    QB_Fault_Share = ifelse(Total_Sacks > 0, QB_Fault_Sacks / Total_Sacks, NA_real_)
  )

cat("\n=== (i) QB-fault share of team sacks, team-to-team spread ===\n")
cat(sprintf("  Mean QB-fault share: %.3f | SD: %.3f | Range: [%.3f, %.3f]\n",
            mean(team_season$QB_Fault_Share, na.rm = TRUE), sd(team_season$QB_Fault_Share, na.rm = TRUE),
            min(team_season$QB_Fault_Share, na.rm = TRUE), max(team_season$QB_Fault_Share, na.rm = TRUE)))
print(team_season %>% select(Team, Season, Total_Sacks, QB_Fault_Sacks, QB_Fault_Share) %>% arrange(desc(QB_Fault_Share)) %>% head(5), row.names = FALSE)
cat("  ... (lowest 5) ...\n")
print(team_season %>% select(Team, Season, Total_Sacks, QB_Fault_Sacks, QB_Fault_Share) %>% arrange(QB_Fault_Share) %>% head(5), row.names = FALSE)

cat("\n=== (ii) Raw vs. QB-fault-excluded Team_Sack_Rate_Allowed correlation (2022-2024 subsample) ===\n")
r_raw_vs_adjusted <- cor(team_season$Team_Sack_Rate_Allowed_Raw, team_season$Team_Sack_Rate_Allowed_QBFaultExcluded, use = "complete.obs")
cat(sprintf("  r = %.3f (n = %d team-seasons)\n", r_raw_vs_adjusted, nrow(team_season)))
# The plan deliberately does not pre-declare a numeric "high vs low"
# threshold here (unlike the OL_CONVERGENCE_MIN_R=0.2 drop rule, which
# IS pre-declared) -- it leaves "high correlation -> validated" vs.
# "low correlation -> elevate the caveat" to judgment at report time.
# r=0.738 is moderate, not clearly either: comparable in size to this
# project's OWN "strong correlation" family-matrix threshold (0.7,
# compute_candidate_family_matrix()'s default) but well short of a
# near-1.0 "these are basically the same measurement" read. Reported
# as-is, not forced into a binary label via a threshold invented after
# seeing the data.
cat("  This is a MODERATE correlation -- comparable to this project's own 0.7 'strong correlation'\n")
cat("  convention (compute_candidate_family_matrix()'s family threshold) but well short of near-1.0.\n")
cat("  Per the plan's qualitative rule (no numeric threshold is pre-declared here), a correlation this\n")
cat("  far from 1.0 does not fully validate the raw proxy -- the QB-contamination caveat is ELEVATED in\n")
cat("  all reporting, and a sensitivity composite substituting the QB-fault-excluded rate (2022+\n")
cat("  subsample only) becomes a required robustness output for later gates. This does NOT change OL\n")
cat("  composite membership (fixed by V2-V3), only how much the sack component's contribution is\n")
cat("  trusted in interpretation.\n")

dir.create("output", showWarnings = FALSE)
readr::write_csv(team_season, "output/diagnose_ol_sack_ftn_validation_results.csv")
cat("\nWritten: output/diagnose_ol_sack_ftn_validation_results.csv\n")
