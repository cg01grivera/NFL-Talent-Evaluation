################################################################
# diagnose_wr_te_anomaly_D4.R
#
# Runs D4 (machinery-free direct signal reads) on real WR data, per
# WR_TE_ANOMALY_RESOLUTION_PLAN.md / the wr-te-anomaly-diagnosis skill.
#
# CONTEXT: D3 (the collinearity-matched null) did NOT confirm H1's
# proposed mechanism under a properly-powered synthetic test -- see
# test_beat_adp_battery.R's D3 block. This does not mean the real D2
# pattern (WR candidates' collinearity with RANK+Prior_PPG correlates
# with how negative their raw concordance estimate is) isn't real --
# only that H1's specific proposed EXPLANATION for it didn't survive
# its own designed test. D4 checks H2 (real signal / market
# mispricing) directly, with no bootstrap, no permutation, no bias
# correction -- just: does the model consistently learn a negative
# coefficient on these candidates across LOSO folds, and does the raw
# candidate correlate negatively with the RANK+Prior_PPG residual in
# most individual seasons?
#
# WHY THIS SOURCES THE FULL BATTERY SCRIPT: same reasoning as the
# D0/D1/D2 runner -- wr_data must come from build_position_data(), the
# one shared assembly path. Should be fast (cache hits expected).
################################################################

source("analyze_estimation_battery.R")

cat("\n\n################################################################\n")
cat("# WR/TE ANOMALY DIAGNOSIS: D4 (direct signal reads, H2 corroboration)\n")
cat("################################################################\n\n")

# Candidates to check: the family-1 representative (Target_Share, the
# most collinear/most negative candidate in D2), plus the two named
# exceptions flagged after D2 (Avg_Intended_Air_Yards, YAC_Share --
# both LOW collinearity but still notably negative, which H1 alone
# doesn't explain), plus Games_Played as the explicit control (should
# show NO consistent pattern either way).
d4_candidates <- c("Target_Share", "Avg_Intended_Air_Yards", "YAC_Share", "Games_Played")

d4_results <- lapply(d4_candidates, function(cand) {
  res <- run_concordance(wr_data, cand, significance_window = 12, return_fold_coefs = TRUE)
  fold_summary <- summarize_fold_coefficients(attr(res, "fold_coefficients"))
  signal_read <- compute_candidate_signal_read(wr_data, cand)
  list(candidate = cand, fold_summary = fold_summary, signal_read = signal_read)
})
names(d4_results) <- d4_candidates

cat("=== D4a: fold-coefficient distributions (LOSO folds are NOT independent --\n")
cat("magnitude and the per-season Spearman below matter more than raw sign-count; see\n")
cat("summarize_fold_coefficients()'s docstring) ===\n\n")
for (r in d4_results) {
  fs <- r$fold_summary
  cat(sprintf("%-25s N_Folds=%d  N_Negative=%d  Pct_Negative=%.2f  Mean=%.4f  SD=%.4f\n",
              r$candidate, fs$N_Folds, fs$N_Negative, fs$Pct_Negative, fs$Mean, fs$SD))
}

cat("\n=== D4b: per-season Spearman(candidate, RANK+Prior_PPG residual) ===\n\n")
for (r in d4_results) {
  sr <- r$signal_read
  cat(sprintf("%-25s N_Seasons_Negative=%d  N_Seasons_Positive=%d  Pct_Negative=%.2f  Mean_Rho=%.4f\n",
              r$candidate, sr$n_seasons_negative, sr$n_seasons_positive, sr$pct_negative, sr$mean_rho))
}

cat("\n--- Per-season detail, Target_Share (family-1 representative) ---\n")
print(d4_results[["Target_Share"]]$signal_read$by_season, row.names = FALSE)

cat("\n=== READ ===\n")
cat("Per the plan: 'both negative and stable across seasons -> H2 real; coefficients\n")
cat("centered at zero -> degradation is variance, not signal.' Compare Target_Share against\n")
cat("Games_Played (the explicit control, expected to show no consistent pattern either way).\n")
cat("Avg_Intended_Air_Yards / YAC_Share are the named exceptions from D2 -- low collinearity\n")
cat("but still notably negative raw; if THESE also show consistent negative signal here,\n")
cat("that's evidence they share whatever real mechanism (if any) is driving Target_Share,\n")
cat("rather than being an unrelated artifact.\n")

cat("\n\n################################################################\n")
cat("# STOP HERE. This is a direct data read, not a decision -- review\n")
cat("# before drawing conclusions about H2 or moving to D5/D6.\n")
cat("################################################################\n")
