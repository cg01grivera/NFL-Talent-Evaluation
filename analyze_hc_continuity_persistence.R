cat("
================================================================
  analyze_hc_continuity_persistence.R

  GOAL: TEAM_CONTEXT_REWORK_PLAN_V2.md Section 5b (Stage 0) / prereg
  Rule 14 -- a MEASUREMENT STUDY, not a beat-ADP test. Are team stats
  franchise properties or coach properties? Splits each team stat's
  year-over-year persistence by head-coach continuity vs. change,
  scheme family vs. roster family. Produces NO beat-ADP claims and
  cannot modify any committed prediction -- report-only, and per the
  plan, all follow-on coach-keying work (playcaller tables, franchise-
  vs-coach forecasting) is OUT OF SCOPE and not built here even if a
  result looks inviting.

  METHOD: head coach per team-game from nflreadr::load_schedules()
  (home_coach/away_coach, verified live: 0 NA across 2012-2025 REG
  games). Primary HC per team-season = the coach with the most starts
  that season (handles in-season firings without guessing). HC-
  continuity flag = same primary HC as the team's PRIOR season.
  Per-stat year-over-year (lag-1, undecayed) correlation is computed
  SEPARATELY for continuity-transition seasons vs. change-transition
  seasons.

  MISCLASSIFICATION NOTE (stated in the plan): HC-change is a noisy
  proxy for playcaller-change (OCs churn under stable HCs; some HCs
  call plays themselves). This noise ATTENUATES the measured gap
  toward zero -- it can hide a real coach effect but cannot fabricate
  one -- so a large observed gap is trustworthy and a null is only
  suggestive.
================================================================
\n")

if (!requireNamespace("nflreadr", quietly = TRUE)) install.packages("nflreadr", repos = "https://cloud.r-project.org")
library(dplyr)

script_arg <- grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) > 0) setwd(dirname(sub("--file=", "", script_arg)))

source("R/utils_core.R")
source("R/fetch_team_context_data.R")

# ---- CONFIG ---------------------------------------------------
if (!exists("SEASONS_TO_TEST")) SEASONS_TO_TEST <- 2012:2025
if (!exists("MIN_N_PER_GROUP")) MIN_N_PER_GROUP <- 10   # minimum transitions in EITHER group to report a correlation at all
cat("=== Config: SEASONS_TO_TEST =", paste(range(SEASONS_TO_TEST), collapse = "-"),
    "| MIN_N_PER_GROUP =", MIN_N_PER_GROUP, "===\n\n")
# -----------------------------------------------------------------

if (exists("GATE4_TEAM_CONTEXT_FULL")) {
  team_context_full <- GATE4_TEAM_CONTEXT_FULL
  cat("Reusing pre-fetched team_context_full (", nrow(team_context_full), "rows )\n\n")
} else {
  message("Building team-context stats for ", min(SEASONS_TO_TEST), "-", max(SEASONS_TO_TEST), "...")
  team_context_by_season <- list()
  for (yr in SEASONS_TO_TEST) {
    message("  ", yr, "...")
    team_context_by_season[[as.character(yr)]] <- fetch_team_context_stats(yr)
  }
  team_context_full <- do.call(rbind, team_context_by_season)
}

message("Loading schedules for head-coach data...")
schedules <- nflreadr::load_schedules(SEASONS_TO_TEST) %>% filter(game_type == "REG")
schedules$home_team <- standardize_teams(schedules$home_team)
schedules$away_team <- standardize_teams(schedules$away_team)

coach_long <- rbind(
  schedules %>% transmute(Season = season, Team = home_team, Coach = home_coach),
  schedules %>% transmute(Season = season, Team = away_team, Coach = away_coach)
)
# Primary HC per team-season = most-started coach that season (handles
# in-season firings without guessing which one "counts").
primary_coach <- coach_long %>%
  count(Season, Team, Coach, name = "N_Starts") %>%
  group_by(Season, Team) %>%
  slice_max(N_Starts, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Season, Team, Coach)

hc_continuity <- primary_coach %>%
  left_join(
    primary_coach %>% mutate(Season = Season + 1) %>% rename(Prior_Coach = Coach),
    by = c("Season", "Team")
  ) %>%
  mutate(HC_Continuity = ifelse(is.na(Prior_Coach), NA, Coach == Prior_Coach))

cat("HC-continuity flag built for", sum(!is.na(hc_continuity$HC_Continuity)), "team-season transitions\n")
cat("  Continuity seasons:", sum(hc_continuity$HC_Continuity, na.rm = TRUE), "\n")
cat("  Change seasons:    ", sum(!hc_continuity$HC_Continuity, na.rm = TRUE), "\n\n")

# ---- Per-stat year-over-year (lag-1, undecayed) correlation, split by
# HC continuity vs. change ----
stat_cols <- setdiff(names(team_context_full), c("Team", "Season"))
scheme_family <- c("Team_PROE", "Team_Raw_Plays_PG", "Team_Shotgun_Rate",
                    "Team_Goal_To_Go_Pass_Rate", "Team_Goal_To_Go_Run_Rate",
                    "Team_4th_Down_Aggressiveness")
roster_family <- c("Team_Sack_Rate_Allowed", "Team_QB_Hit_Rate_Allowed", "Team_Rush_Stuff_Rate",
                    "Team_OL_Penalty_Rate", "Team_EPA_Per_Dropback", "Team_CPOE",
                    "Team_TE_Target_Share", "Team_WR_Target_Share", "Team_RB_Target_Share")

compute_split_persistence <- function(stat) {
  this_yr <- team_context_full %>% select(Team, Season, val_t = all_of(stat))
  next_yr <- team_context_full %>% select(Team, Season, val_tplus1 = all_of(stat)) %>%
    mutate(Season = Season - 1)
  paired <- inner_join(this_yr, next_yr, by = c("Team", "Season")) %>%
    # HC_Continuity is keyed by the LATER season (Season+1 in `paired`'s
    # own indexing) -- was THIS transition into the following season a
    # continuity or change year.
    inner_join(hc_continuity %>% mutate(Season = Season - 1), by = c("Team", "Season")) %>%
    filter(!is.na(HC_Continuity))

  cont <- paired %>% filter(HC_Continuity)
  chg  <- paired %>% filter(!HC_Continuity)
  r_cont <- if (nrow(cont) >= MIN_N_PER_GROUP) cor(cont$val_t, cont$val_tplus1, use = "complete.obs") else NA_real_
  r_chg  <- if (nrow(chg)  >= MIN_N_PER_GROUP) cor(chg$val_t,  chg$val_tplus1,  use = "complete.obs") else NA_real_
  data.frame(Stat = stat,
             Family = if (stat %in% scheme_family) "scheme" else if (stat %in% roster_family) "roster" else "other",
             R_Continuity = round(r_cont, 3), N_Continuity = nrow(cont),
             R_Change = round(r_chg, 3), N_Change = nrow(chg),
             Persistence_Drop = round(r_cont - r_chg, 3))
}

message("Computing HC-continuity-split persistence for ", length(stat_cols), " stats...")
hc_results <- do.call(rbind, lapply(stat_cols, compute_split_persistence))
hc_results <- hc_results[order(-hc_results$Persistence_Drop), ]
rownames(hc_results) <- NULL

cat("\n=== HC-continuity-split persistence, ranked by drop (continuity minus change) ===\n")
print(hc_results, row.names = FALSE)

cat("\n=== By family ===\n")
cat("Scheme family mean persistence drop:", round(mean(hc_results$Persistence_Drop[hc_results$Family == "scheme"], na.rm = TRUE), 3), "\n")
cat("Roster family mean persistence drop:", round(mean(hc_results$Persistence_Drop[hc_results$Family == "roster"], na.rm = TRUE), 3), "\n")

cat("\nThis is a report-only measurement study (Stage 0). It produces NO beat-ADP claims and\n")
cat("does not modify any committed prediction. Follow-on coach-keying work (playcaller\n")
cat("tables, franchise-vs-coach forecasting) is explicitly OUT OF SCOPE for this plan.\n")

dir.create("output", showWarnings = FALSE)
readr::write_csv(hc_results, "output/analyze_hc_continuity_persistence_results.csv")
cat("\nWritten: output/analyze_hc_continuity_persistence_results.csv\n")
