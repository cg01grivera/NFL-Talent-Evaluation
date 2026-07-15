################################################################
# diagnose_wr_te_anomaly_D0_D1_D2.R
#
# Runs the first three (cheapest) rungs of the WR/TE opportunity-
# direction anomaly ladder, per WR_TE_ANOMALY_RESOLUTION_PLAN.md /
# the wr-te-anomaly-diagnosis skill:
#
#   D0 - decisive WR coverage spot-check (n=200, not another n=50)
#   D1 - column-identity audit (is TE's Target_Share/Targets_PG/WOPR
#        raw-0.0000 triple three distinct columns, or one column
#        reaching the model under three names?)
#   D2 - collinearity gradient (does a candidate's R^2 against
#        RANK+Prior_PPG predict how negative its raw concordance
#        point estimate is? This is H1's observational signature.)
#
# D3 (the collinearity-matched null) is deliberately NOT run here. Per
# the plan's own decision tree, D3 is a substantial library change
# (a new null-generation mode) gated on reviewing D0-D2 first, and per
# the skill's "stop at gates" rule, this script stops after D2 and
# reports rather than proceeding unattended.
#
# WHY THIS SOURCES THE FULL BATTERY SCRIPT: wr_data/te_data/
# wr_te_candidates must come from build_position_data(), the one
# shared assembly path (section 1.2 -- no parallel local assembly,
# ever, per this project's worst historical incident). Sourcing
# analyze_estimation_battery.R in full is the only way to get those
# objects without duplicating its logic. This re-runs QB/RB too, but
# since nothing in this session invalidates the null-bias cache, all
# four positions' bias steps should be cache hits and the whole thing
# should take a few minutes, not the ~2 hours the original run took.
# Watch for "Using CACHED" (expected) vs "No matching cache entry"
# (unexpected -- means something changed and this will take much
# longer) in the console as it runs.
################################################################

source("analyze_estimation_battery.R")

cat("\n\n################################################################\n")
cat("# WR/TE ANOMALY DIAGNOSIS: D0, D1, D2\n")
cat("# (D3, the matched null, is NOT run here -- gated on review of these first)\n")
cat("################################################################\n\n")

# ================================================================
# D0 -- decisive WR coverage spot-check
#
# Two n=50 spot-checks already exist for this exact config (0.80 from
# the original N_BOOT=500 run, 0.86 from the N_BOOT=1000 re-run). A
# two-proportion test between them came back p=0.59 -- statistically
# indistinguishable. Running a THIRD n=50 check would tell us nothing
# new; n=200 here is what actually has a chance of resolving it.
#
# pos = "WR" (not a diagnostic-only label) deliberately, so this reuses
# the REAL cached WR null bias via a cache HIT (read-only -- the cache
# is only ever written on a miss, confirmed in R/beat_adp_battery.R
# before relying on this) rather than computing a fresh bias under a
# throwaway label, which would take ~10-20 minutes for no reason.
# ================================================================
cat("=== D0: WR coverage spot-check at n=200 ===\n")
d0_result <- run_estimation_for_position(
  "WR", wr_data, candidates = c("Target_Share"),
  representative_candidate_col = "Target_Share",
  seasons_to_test = SEASONS_TO_TEST, min_games = MIN_GAMES, decay_r = DECAY_R, lookback_years = LOOKBACK_YEARS,
  n_boot = N_BOOT, conf_level = CONF_LEVEL, run_coverage_spotcheck = TRUE,
  coverage_n_replicates = 200,
  cache_path = "output/concordance_null_bias_cache.csv"
)
d0_cov <- attr(d0_result, "coverage_spotcheck")
cat("\nD0 RESULT: coverage =", round(d0_cov$Coverage_Rate, 3),
    " (", d0_cov$N_Covered, "/", d0_cov$N_Trials, ")",
    " one-sided p vs nominal ", d0_cov$Nominal_Conf_Level, " = ", signif(d0_cov$P_Value_Vs_Nominal, 3), "\n", sep = "")
if (d0_cov$Coverage_Rate >= 0.88 && d0_cov$P_Value_Vs_Nominal >= 0.10) {
  cat("D0 READ: coverage ~nominal -- plan's PASS criterion met. H3 (bootstrap miscalibration) closed as an N_BOOT=500 artifact.\n")
} else if (d0_cov$P_Value_Vs_Nominal < 0.10) {
  cat("D0 READ: still significantly below nominal -- plan's FAIL criterion met. Per the plan: stop, calibration investigation precedes everything else.\n")
} else {
  cat("D0 READ: AMBIGUOUS by the plan's own binary criterion (neither clearly resolved nor clearly still broken). Report this explicitly -- do not round to either side. D1/D2 below do not depend on bootstrap calibration (they run on pre-bootstrap candidate columns and raw point estimates), so proceeding to review them in parallel is reasonable even if D0 stays unresolved -- but do not treat D0 as closed.\n")
}

# ================================================================
# D1 -- column-identity audit (WR and TE)
# ================================================================
cat("\n=== D1: column-identity audit ===\n")
wr_d1 <- audit_candidate_column_identity(wr_data, wr_te_candidates)
te_d1 <- audit_candidate_column_identity(te_data, wr_te_candidates)

cat("\n--- WR identical/near-identical pairs (|r| >= 0.999) ---\n")
if (nrow(wr_d1$identical_pairs) > 0) print(wr_d1$identical_pairs, row.names = FALSE) else cat("None.\n")

cat("\n--- TE identical/near-identical pairs (|r| >= 0.999) ---\n")
if (nrow(te_d1$identical_pairs) > 0) print(te_d1$identical_pairs, row.names = FALSE) else cat("None.\n")

cat("\n--- TE column summary: the Target_Share/Targets_PG/WOPR raw-0.0000 triple, specifically ---\n")
print(te_d1$column_summary[te_d1$column_summary$Stat %in% c("Target_Share", "Targets_PG", "WOPR"), ], row.names = FALSE)
te_triple_pairs <- te_d1$identical_pairs[
  (te_d1$identical_pairs$Stat_A %in% c("Target_Share", "Targets_PG", "WOPR")) &
  (te_d1$identical_pairs$Stat_B %in% c("Target_Share", "Targets_PG", "WOPR")), ]
if (nrow(te_triple_pairs) > 0 && any(te_triple_pairs$Values_Identical)) {
  cat("\nD1 READ (TE triple): at least one pair has IDENTICAL values -- construction bug. Fix before anything else; do not proceed to D2 interpretation for TE until resolved.\n")
} else if (nrow(te_triple_pairs) > 0) {
  cat("\nD1 READ (TE triple): perfectly correlated but NOT identical values -- consistent with the small-pool quantization explanation, not a construction bug. Still worth noting for D2/H1 (perfect correlation with each other doesn't mean perfect correlation with RANK/Prior_PPG).\n")
} else {
  cat("\nD1 READ (TE triple): not even flagged as near-identical to each other -- the raw-0.0000 coincidence needs a different explanation than shared/duplicated columns.\n")
}

# ================================================================
# D2 -- collinearity gradient (WR)
# ================================================================
cat("\n=== D2: collinearity gradient (WR) ===\n")
wr_d2 <- compute_candidate_collinearity(wr_data, wr_te_candidates)
wr_raw <- all_results[all_results$Position == "WR", c("Stat", "Concordance_Point_Estimate")]
d2_joined <- merge(wr_d2, wr_raw, by = "Stat")
d2_joined <- d2_joined[order(-d2_joined$R_Squared), ]
cat("\n--- WR candidates ranked by R^2 vs (RANK, Prior_PPG), raw concordance alongside ---\n")
print(d2_joined, row.names = FALSE)

d2_fit <- lm(Concordance_Point_Estimate ~ R_Squared, data = d2_joined)
slope <- coef(d2_fit)["R_Squared"]
pval <- summary(d2_fit)$coefficients["R_Squared", "Pr(>|t|)"]
cat("\nSlope of raw concordance point estimate on R_Squared:", round(slope, 4), " (p =", signif(pval, 3), ")\n")
if (slope < 0 && pval < 0.10) {
  cat("D2 READ: negative, meaningful slope -- H1's observational signature is present. Plan says: proceed to D3 (do not build D3 from this script -- report first).\n")
} else {
  cat("D2 READ: flat or non-negative slope -- H1 is weakened by this observational test. Per the plan: tell the user H1 is weakened and H2 is promoted before continuing to D3.\n")
}

cat("\n\n################################################################\n")
cat("# STOP HERE per the wr-te-anomaly-diagnosis skill's gate discipline.\n")
cat("# D3 (the collinearity-matched null) is a substantial library change\n")
cat("# and is gated on review of D0/D1/D2 above -- report these results\n")
cat("# before any D3 code is written.\n")
cat("################################################################\n")
