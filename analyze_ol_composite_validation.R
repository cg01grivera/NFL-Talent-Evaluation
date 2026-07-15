cat("
================================================================
  analyze_ol_composite_validation.R

  GOAL: Gate 4 (TEAM_CONTEXT_REWORK_PLAN_V2.md Section 3.2) validation
  ladder for Team_OL_Composite -- resolves TEAM_CONTEXT_PREREGISTRATION_
  V2.md Rule 9 (composite membership). Stat-level only: no player
  outcomes (season_ppg/ADP) are touched anywhere in this script.

  V2 Persistence: the composite itself must clear the same decay-
  weighted year-over-year self-correlation gate (>= 0.30) as any raw
  stat -- same engine (R/decay_test_utils.R), same entity (Team).

  V3 Convergent validity: each component should positively correlate
  with the composite-of-the-OTHER-THREE (all oriented higher=better).
  Declared drop rule: correlation < 0.2 means that component is
  measuring something else and is dropped from the composite.

  V4 Predictive stability: the composite in year t should predict each
  component's OWN year t+1 value at least as well as that component's
  own year t value does -- the whole point of aggregating four noisy
  proxies is to average out one-year contamination noise, so if a
  single component beats the composite at predicting its own future,
  the composite isn't doing its job.

  Composite definition (FIXED IN ADVANCE, never fit to outcomes): per
  season, z-score each surviving component across teams, orient to
  higher = better line (all four raw components are lower = better),
  take the unweighted mean.
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
if (!exists("GATE_R")) GATE_R <- 0.5           # the project's standard decay rate -- the GATE decision
                                                 # is read at this fixed r, not a grid-searched "best r",
                                                 # to avoid r-cherry-picking (no goal-seeking, CLAUDE.md #5)
if (!exists("PERSISTENCE_GATE")) PERSISTENCE_GATE <- 0.30
if (!exists("OL_CONVERGENCE_MIN_R")) OL_CONVERGENCE_MIN_R <- 0.2
OL_COMPONENTS <- c("Team_Sack_Rate_Allowed", "Team_QB_Hit_Rate_Allowed", "Team_Rush_Stuff_Rate", "Team_OL_Penalty_Rate")
cat("=== Config: SEASONS_TO_TEST =", paste(range(SEASONS_TO_TEST), collapse = "-"),
    "| LOOKBACK_YEARS =", LOOKBACK_YEARS, "| GATE_R =", GATE_R,
    "| PERSISTENCE_GATE =", PERSISTENCE_GATE, "| OL_CONVERGENCE_MIN_R =", OL_CONVERGENCE_MIN_R, "===\n")
cat("OL_COMPONENTS:", paste(OL_COMPONENTS, collapse = ", "), "\n\n")
# -----------------------------------------------------------------

# ---- Data: reuse a cached fetch if the caller already built one
# (GATE4_TEAM_CONTEXT_FULL), else fetch fresh -- this script never
# touches season_ppg/ADP, only fetch_team_context_stats()' output.
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

missing_cols <- setdiff(OL_COMPONENTS, names(team_context_full))
if (length(missing_cols) > 0) stop("Missing OL component columns: ", paste(missing_cols, collapse = ", "))

# ---- z-score every raw component up front (needed for V3 regardless
# of final composite membership) ----
z_cols <- paste0("z_", OL_COMPONENTS)
team_context_full <- team_context_full %>%
  group_by(Season) %>%
  mutate(across(all_of(OL_COMPONENTS), ~ -1 * zscore(.x), .names = "z_{.col}")) %>%
  ungroup() %>%
  as.data.frame()

cat("=== V3: Convergent validity (component vs. composite-of-the-others) ===\n")
v3_rows <- list()
for (comp in OL_COMPONENTS) {
  others <- setdiff(OL_COMPONENTS, comp)
  other_z_cols <- paste0("z_", others)
  composite_of_others <- rowMeans(as.matrix(team_context_full[, other_z_cols]), na.rm = TRUE)
  r <- cor(team_context_full[[paste0("z_", comp)]], composite_of_others, use = "complete.obs")
  v3_rows[[comp]] <- data.frame(Component = comp, Correlation_Vs_CompositeOfOthers = round(r, 3),
                                 Dropped = r < OL_CONVERGENCE_MIN_R)
  cat(sprintf("  %-28s r vs. composite-of-others = %6.3f  (%s)\n", comp, round(r, 3),
              if (r < OL_CONVERGENCE_MIN_R) "DROP -- below 0.2 threshold" else "keep"))
}
v3_df <- do.call(rbind, v3_rows)
rownames(v3_df) <- NULL

# ---- Composite construction, from the V3 SURVIVORS only (fixed
# definition, never fit to outcomes). V2/V4 below are read on THIS
# composite, not a pre-drop 4-component one -- V3's drop decision must
# feed into V2/V4, not be tested alongside a component it already
# rejected. ----
surviving_components <- OL_COMPONENTS[!v3_df$Dropped]
cat("\nComposite built from V3 survivors only:", paste(surviving_components, collapse = ", "), "\n")
surviving_z_cols <- paste0("z_", surviving_components)
team_context_full$Team_OL_Composite <- rowMeans(as.matrix(team_context_full[, surviving_z_cols, drop = FALSE]), na.rm = TRUE)

cat("\n=== V2: Composite persistence (survivors-only composite) ===\n")
comp_res <- test_decay_rates(team_context_full, "Team", "Team_OL_Composite", "Season", R_GRID, LOOKBACK_YEARS)
r_gate_idx <- which(R_GRID == GATE_R)
comp_gate_corr <- round(comp_res$correlations[r_gate_idx], 3)
comp_best_idx <- which.max(comp_res$correlations)
cat(sprintf("  Team_OL_Composite: r=%.1f correlation = %.3f (gate >= %.2f: %s) | best-in-grid r=%.1f -> %.3f | N_pairs=%d, N_years=%d\n",
            GATE_R, comp_gate_corr, PERSISTENCE_GATE, if (!is.na(comp_gate_corr) && comp_gate_corr >= PERSISTENCE_GATE) "PASS" else "FAIL",
            R_GRID[comp_best_idx], round(comp_res$correlations[comp_best_idx], 3), comp_res$n_pairs, comp_res$n_years))

cat("\n=== V4: Predictive stability (composite_t -> component_t+1, vs. component_t -> component_t+1), V3 survivors only ===\n")
v4_rows <- list()
for (comp in surviving_components) {
  this_yr <- team_context_full %>% select(Team, Season, comp_t = all_of(comp), Team_OL_Composite)
  next_yr <- team_context_full %>% select(Team, Season, comp_tplus1 = all_of(comp)) %>%
    mutate(Season = Season - 1)
  paired <- inner_join(this_yr, next_yr, by = c("Team", "Season"))
  r_composite <- cor(paired$Team_OL_Composite, paired$comp_tplus1, use = "complete.obs")
  r_own <- cor(paired$comp_t, paired$comp_tplus1, use = "complete.obs")
  v4_rows[[comp]] <- data.frame(
    Component = comp, R_Composite_Predicts_TPlus1 = round(r_composite, 3),
    R_Own_Predicts_TPlus1 = round(r_own, 3),
    Composite_At_Least_As_Good = abs(r_composite) >= abs(r_own), N_Pairs = nrow(paired)
  )
  cat(sprintf("  %-28s |composite->t+1| = %.3f vs |own->t+1| = %.3f  (%s, n=%d)\n",
              comp, abs(round(r_composite, 3)), abs(round(r_own, 3)),
              if (abs(r_composite) >= abs(r_own)) "composite at least as good" else "SINGLE COMPONENT BEATS COMPOSITE",
              nrow(paired)))
}
v4_df <- do.call(rbind, v4_rows)
rownames(v4_df) <- NULL

cat("\n=== Gate 4 / Rule 9 resolution summary ===\n")
cat("Components surviving V3 (convergent validity):", paste(surviving_components, collapse = ", "), "\n")
cat("Composite V2 persistence gate:", if (!is.na(comp_gate_corr) && comp_gate_corr >= PERSISTENCE_GATE) "PASS" else "FAIL",
    sprintf("(r=%.1f corr = %.3f, gate = %.2f)\n", GATE_R, comp_gate_corr, PERSISTENCE_GATE))
cat("V4 predictive stability:", if (all(v4_df$Composite_At_Least_As_Good)) "PASS for all components" else "FAILS for at least one component -- reconsider composite before the battery, per plan", "\n")

dir.create("output", showWarnings = FALSE)
readr::write_csv(v3_df, "output/analyze_ol_composite_v3_convergent_validity.csv")
readr::write_csv(v4_df, "output/analyze_ol_composite_v4_predictive_stability.csv")
cat("\nWritten: output/analyze_ol_composite_v3_convergent_validity.csv, output/analyze_ol_composite_v4_predictive_stability.csv\n")
