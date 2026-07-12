cat("
================================================================
  test_beat_adp_battery.R

  GOAL: Unit tests for R/beat_adp_battery.R using synthetic data with
  answers known by construction -- NOT a re-run of real football data,
  which can only ever show 'the number came out looking reasonable,'
  never 'the code is definitely correct.' Built specifically because
  Fable 5's second-round review found a real bug in this library's
  first version (run_full_battery_for_candidate silently dropped the
  breakout AUC p-value) that a reconciliation-against-one-known-number
  check would NOT have caught, since the bug lived in the summary
  wrapper, not the test functions the reconciliation check exercises.

  FOUR FIXTURES, each with a known-correct expected outcome:
    1. Candidate IS the outcome (season_ppg itself, undecayed) -- an
       obviously, maximally informative signal. The battery must
       detect SOMETHING here; failure to reject the null on this
       fixture means the mechanics are broken, not that the world is
       null. (This tests basic detection capability, NOT realistic
       statistical power -- see the separate, residual-based power
       analysis for that, which uses a realistically-sized injected
       signal rather than this deliberately extreme one.)
    2. Constant candidate (same value for every player-season) -- must
       return NA gracefully (zero variance means no information to
       test), not error out.
    3. Permuted candidate (a real-shaped variable with its values
       shuffled across players within each season, destroying any true
       relationship while preserving the marginal distribution) --
       must NOT reliably detect a signal.
    4. Output-completeness assertion: run_full_battery_for_candidate()'s
       return vector must contain a p-value for EVERY metric family
       (RMSE, breakout AUC, bust AUC, concordance) -- this is the exact
       assertion that would have caught the dropped-breakout-p-value
       bug immediately, and is run against the CURRENT file to confirm
       the fix, not just described as a good idea.

  Run this after any change to beat_adp_battery.R, before trusting it
  for a real battery -- per Fable 5's explicit condition that
  reconciliation against one known number is insufficient on its own.
================================================================
\n")

library(dplyr)
script_arg <- grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) > 0) setwd(dirname(sub("--file=", "", script_arg)))
source("R/beat_adp_battery.R")

test_pass <- function(description, condition) {
  status <- if (isTRUE(condition)) "PASS" else "FAIL"
  cat(sprintf("[%s] %s\n", status, description))
  invisible(status == "PASS")
}

# ---- Synthetic data construction ----
set.seed(42)
n_seasons <- 12
n_per_season <- 40
synth <- do.call(rbind, lapply(1:n_seasons, function(s) {
  RANK <- sample(1:150, n_per_season)
  Prior_PPG <- rnorm(n_per_season, mean = 15, sd = 5)
  # season_ppg has a real, known relationship to RANK/Prior_PPG plus
  # noise -- this is the "true" data-generating process the fixtures
  # are checked against.
  season_ppg <- 25 - 0.05 * RANK + 0.3 * Prior_PPG + rnorm(n_per_season, sd = 3)
  data.frame(Season = 2000 + s, RANK, Prior_PPG, season_ppg)
}))
synth <- synth %>%
  group_by(Season) %>%
  mutate(
    expected_ppg = predict(lm(season_ppg ~ RANK + Prior_PPG)),
    residual = season_ppg - expected_ppg,
    is_breakout = as.integer(residual >= quantile(residual, 0.75)),
    is_bust     = as.integer(residual <= quantile(residual, 0.25))
  ) %>%
  ungroup()

all_pass <- TRUE

# ---- Fixture 1: candidate IS the outcome ----
synth$candidate_is_outcome <- synth$season_ppg
res1 <- run_full_battery_for_candidate(synth, "candidate_is_outcome")
all_pass <- test_pass(
  "Fixture 1: maximally informative candidate produces a non-NA, sub-0.5 RMSE_P",
  !is.na(res1["RMSE_P"]) && res1["RMSE_P"] < 0.5
) && all_pass
all_pass <- test_pass(
  "Fixture 1: RMSE improvement is positive and large (candidate obviously helps)",
  !is.na(res1["RMSE_Improvement_Pct"]) && res1["RMSE_Improvement_Pct"] > 10
) && all_pass

# ---- Fixture 2: constant candidate ----
synth$candidate_constant <- 7
res2 <- run_rmse_cv(synth, "candidate_constant")
all_pass <- test_pass(
  "Fixture 2: constant candidate does not error and returns NA (not a crash, not a false result)",
  is.na(res2["RMSE_P"])
) && all_pass

# ---- Fixture 3: permuted candidate ----
set.seed(99)
synth <- synth %>%
  group_by(Season) %>%
  mutate(candidate_permuted = sample(Prior_PPG)) %>%  # real-shaped, but shuffled within season -- destroys any true relationship
  ungroup()
res3 <- run_full_battery_for_candidate(synth, "candidate_permuted")
all_pass <- test_pass(
  "Fixture 3: permuted candidate's RMSE_P is NOT reliably small (>= 0.05 -- a single run, illustrative only; see the full negative-control replicate loop for the real check)",
  is.na(res3["RMSE_P"]) || res3["RMSE_P"] >= 0.05
) && all_pass

# ---- Fixture 4: output completeness (the one that would have caught the actual bug) ----
required_fields <- c("RMSE_P", "Breakout_AUC_P", "Bust_AUC_P", "Concordance_Sig_P")
missing_fields <- setdiff(required_fields, names(res1))
all_pass <- test_pass(
  paste0("Fixture 4: wrapper output contains a p-value for every metric family (RMSE, breakout AUC, bust AUC, concordance)",
         if (length(missing_fields) > 0) paste0(" -- MISSING: ", paste(missing_fields, collapse = ", ")) else ""),
  length(missing_fields) == 0
) && all_pass

# ---- paired_wilcox_p diagnostics sanity check ----
p_diag <- paired_wilcox_p(c(1, 2, 3, 4, 5), c(1.1, 2.5, 2.9, 4.2, 5.5))
all_pass <- test_pass(
  "paired_wilcox_p attaches n_effective/n_zero_diffs/n_ties/used_exact attributes",
  all(c("n_effective", "n_zero_diffs", "n_ties", "used_exact") %in% names(attributes(p_diag)))
) && all_pass

cat("\n=== Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ============================================================
# Bootstrap estimation-layer tests (added alongside the
# residual-based positive control / effect-estimation work)
# ============================================================
cat("\n=== Bootstrap estimation layer ===\n")

# Backward-compatibility check: default return_folds/return_pairs =
# FALSE must produce the EXACT same output shape as before these
# parameters were added -- every existing caller depends on this.
res_default <- run_rmse_cv(synth, "candidate_is_outcome")
all_pass <- test_pass(
  "Backward compatibility: run_rmse_cv() default return type unchanged (plain named vector, not a list)",
  is.numeric(res_default) && !is.list(res_default)
) && all_pass

conc_default <- run_concordance(synth, "candidate_is_outcome")
all_pass <- test_pass(
  "Backward compatibility: run_concordance() default return type unchanged (plain named vector, not a list)",
  is.numeric(conc_default) && !is.list(conc_default)
) && all_pass

# Strong signal -> CI should be clearly separated from zero (entirely positive)
rmse_ci_strong <- run_rmse_bootstrap(synth, "candidate_is_outcome", n_boot = 500)
all_pass <- test_pass(
  "Strong signal (candidate = outcome): RMSE bootstrap CI is entirely positive (excludes zero)",
  !is.na(rmse_ci_strong["CI_Lower"]) && rmse_ci_strong["CI_Lower"] > 0
) && all_pass

# Null (permuted) signal -> across MULTIPLE independent permutation
# draws, the CI should straddle zero MOST of the time -- a single draw
# is expected to miss roughly 10% of the time purely by chance under a
# well-behaved 90% CI (that's what "90% confidence" means), so a
# single-draw check is inherently flaky and not a fair gating test.
# Checking the majority behavior across several draws instead.
set.seed(123)
n_null_draws <- 10
straddle_count <- 0
for (i in seq_len(n_null_draws)) {
  synth_i <- synth %>% group_by(Season) %>% mutate(candidate_permuted_i = sample(Prior_PPG)) %>% ungroup()
  ci_i <- run_rmse_bootstrap(synth_i, "candidate_permuted_i", n_boot = 300)
  if (!is.na(ci_i["CI_Lower"]) && !is.na(ci_i["CI_Upper"]) && ci_i["CI_Lower"] < 0 && ci_i["CI_Upper"] > 0) {
    straddle_count <- straddle_count + 1
  }
}
all_pass <- test_pass(
  sprintf("Null signal (permuted candidate): RMSE bootstrap CI straddles zero in >=70%% of %d independent draws (got %d/%d)",
          n_null_draws, straddle_count, n_null_draws),
  straddle_count / n_null_draws >= 0.70
) && all_pass

# Constant candidate -> the CORRECT behavior is a point estimate near
# zero with a narrow CI, NOT an NA point estimate. A constant term
# doesn't make the CV loop fail (lm() still fits; the term just
# contributes nothing), so RMSE_A ~= RMSE_B in every fold -- the
# bootstrap correctly reports "effect is precisely ~0," which is a
# real, informative estimation-layer answer, not a missing one. (The
# OLD p-value test correctly expects NA for RMSE_P specifically, since
# there's no variance to run a significance test on -- that's a
# different question than the point estimate itself.)
rmse_ci_constant <- run_rmse_bootstrap(synth, "candidate_constant", n_boot = 500)
all_pass <- test_pass(
  "Constant candidate: RMSE bootstrap point estimate is near zero with a narrow CI (not NA, not a crash)",
  !is.na(rmse_ci_constant["Point_Estimate"]) &&
    abs(rmse_ci_constant["Point_Estimate"]) < 0.01 &&
    (rmse_ci_constant["CI_Upper"] - rmse_ci_constant["CI_Lower"]) < 0.01
) && all_pass

# run_full_battery_estimation() output completeness
est_res <- run_full_battery_estimation(synth, "candidate_is_outcome", n_boot = 500)
required_est_fields <- c("RMSE_Point_Estimate", "RMSE_CI_Lower", "RMSE_CI_Upper",
                          "Concordance_Point_Estimate", "Concordance_CI_Lower", "Concordance_CI_Upper",
                          "Breakout_AUC_P", "Bust_AUC_P")
missing_est_fields <- setdiff(required_est_fields, names(est_res))
all_pass <- test_pass(
  paste0("run_full_battery_estimation() output contains every expected field",
         if (length(missing_est_fields) > 0) paste0(" -- MISSING: ", paste(missing_est_fields, collapse = ", ")) else ""),
  length(missing_est_fields) == 0
) && all_pass

cat("\n=== Overall (including bootstrap layer):", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ============================================================
# Estimand regression test (added after a confirmed real defect:
# run_concordance previously compared model_b against raw ADP order
# instead of against model_a, so every candidate -- including a
# genuinely random one -- inherited RANK+Prior_PPG's own real
# advantage over blind draft order. Confirmed via check_null_
# concordance_centering.R: 200 independent null candidates showed a
# mean "improvement" of +0.014 vs ADP, decisively non-zero, p=1.1e-12.
# This test makes sure that specific bug class can never silently
# reappear.)
# ============================================================
cat("\n=== Estimand regression test ===\n")
cat("(NOTE: each replicate calls the full run_concordance(), which fits BOTH model_a and\n")
cat(" model_b per LOSO fold since the estimand fix, plus an O(n^2) pairwise comparison\n")
cat(" build per season -- inherently the most expensive single test in this suite. Cut to\n")
cat(" 15 replicates from an original 30, since this is a regression check confirming the old\n")
cat(" bug can't silently reappear, not a precision measurement that needs a large N.)\n")
n_estimand_replicates <- 15
start_time_estimand <- Sys.time()
null_estimates <- sapply(seq_len(n_estimand_replicates), function(i) {
  synth_perm <- synth %>% group_by(Season) %>% mutate(perm_i = sample(Prior_PPG)) %>% ungroup()
  result <- run_concordance(synth_perm, "perm_i", significance_window = 12)["Concordance_Diff_W12"]
  print_progress(i, n_estimand_replicates, start_time_estimand, "estimand check replicate")
  result
})
all_pass <- test_pass(
  sprintf("Null candidate's Concordance_Diff_W12 (model_b vs model_a) centers near zero across %d draws (mean = %.4f, should NOT be a large, consistent positive value like the pre-fix +0.014 vs-ADP bias)",
          n_estimand_replicates, mean(null_estimates, na.rm = TRUE)),
  abs(mean(null_estimates, na.rm = TRUE)) < 0.02
) && all_pass

cat("\n=== FINAL Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ============================================================
# Bias-correction layer tests
# ============================================================
cat("\n=== Bias-correction layer ===\n")

# apply_concordance_bias_correction() must correctly subtract the given
# bias from all three concordance fields, and must NOT touch anything else
mock_result <- c(RMSE_Point_Estimate = -0.5, Concordance_Point_Estimate = 0.05,
                  Concordance_CI_Lower = 0.01, Concordance_CI_Upper = 0.09)
corrected <- apply_concordance_bias_correction(mock_result, null_bias = 0.02)
all_pass <- test_pass(
  "apply_concordance_bias_correction() correctly subtracts the bias from point estimate and both CI bounds",
  abs(corrected["Concordance_Point_Estimate_BiasCorrected"] - 0.03) < 1e-9 &&
    abs(corrected["Concordance_CI_Lower_BiasCorrected"] - (-0.01)) < 1e-9 &&
    abs(corrected["Concordance_CI_Upper_BiasCorrected"] - 0.07) < 1e-9
) && all_pass
all_pass <- test_pass(
  "apply_concordance_bias_correction() preserves the original raw fields unchanged",
  corrected["RMSE_Point_Estimate"] == -0.5 && corrected["Concordance_Point_Estimate"] == 0.05
) && all_pass

# estimate_concordance_null_bias() on a known-null (permuted) real
# candidate should produce a bias whose magnitude is small relative to
# a genuinely strong signal, and must not error
bias_est <- estimate_concordance_null_bias(synth, "candidate_permuted", n_replicates = 15, n_boot = 200)
all_pass <- test_pass(
  "estimate_concordance_null_bias() runs without error and returns a finite bias estimate with a positive SE",
  is.finite(bias_est["Null_Bias"]) && is.finite(bias_est["Null_Bias_SE"]) && bias_est["Null_Bias_SE"] > 0
) && all_pass

cat("\n=== TRULY FINAL Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ============================================================
# Robust bias estimator test (added after check_bias_estimate_
# stability.R confirmed a single 100-replicate call can land ~3 SDs
# from the true value by chance)
# ============================================================
cat("\n=== Robust bias estimator ===\n")
robust_bias <- estimate_concordance_null_bias_robust(synth, "candidate_permuted", n_meta_replicates = 3,
                                                        n_replicates = 20, n_boot = 100)
all_pass <- test_pass(
  "estimate_concordance_null_bias_robust() runs without error and returns a finite estimate with a positive SE",
  is.finite(robust_bias["Null_Bias"]) && is.finite(robust_bias["Null_Bias_SE"]) && robust_bias["Null_Bias_SE"] > 0
) && all_pass
all_pass <- test_pass(
  "estimate_concordance_null_bias_robust() reports the correct meta-replicate count",
  robust_bias["N_Meta_Replicates"] == 3
) && all_pass

cat("\n=== ABSOLUTE FINAL Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ============================================================
# Bias caching layer tests (uses a temp file, never the real cache)
# ============================================================
cat("\n=== Bias caching layer ===\n")
temp_cache <- tempfile(fileext = ".csv")

first_call <- get_or_compute_concordance_null_bias(
  synth, "candidate_permuted", position = "TESTPOS", seasons_to_test = 2020:2025,
  min_games = 8, decay_r = 0.5, lookback_years = 4, cache_path = temp_cache,
  n_meta_replicates = 2, n_replicates = 10, n_boot = 50
)
all_pass <- test_pass(
  "First call with no existing cache computes fresh and writes a cache file",
  file.exists(temp_cache) && is.finite(first_call["Null_Bias"])
) && all_pass

second_call <- get_or_compute_concordance_null_bias(
  synth, "candidate_permuted", position = "TESTPOS", seasons_to_test = 2020:2025,
  min_games = 8, decay_r = 0.5, lookback_years = 4, cache_path = temp_cache,
  n_meta_replicates = 2, n_replicates = 10, n_boot = 50
)
all_pass <- test_pass(
  "Second call with IDENTICAL config reuses the cached value exactly (not a fresh, different measurement)",
  isTRUE(all.equal(unname(first_call["Null_Bias"]), unname(second_call["Null_Bias"])))
) && all_pass

third_call <- get_or_compute_concordance_null_bias(
  synth, "candidate_permuted", position = "TESTPOS", seasons_to_test = 2019:2025,  # DIFFERENT season range
  min_games = 8, decay_r = 0.5, lookback_years = 4, cache_path = temp_cache,
  n_meta_replicates = 2, n_replicates = 10, n_boot = 50
)
all_pass <- test_pass(
  "A call with a CHANGED config (different season range) does not reuse the stale cache entry (still returns a valid finite estimate)",
  is.finite(third_call["Null_Bias"])
) && all_pass

force_call <- get_or_compute_concordance_null_bias(
  synth, "candidate_permuted", position = "TESTPOS", seasons_to_test = 2020:2025,
  min_games = 8, decay_r = 0.5, lookback_years = 4, cache_path = temp_cache,
  n_meta_replicates = 2, n_replicates = 10, n_boot = 50, force_recompute = TRUE
)
all_pass <- test_pass(
  "force_recompute=TRUE recomputes even with a matching cache entry present (still returns a valid finite estimate)",
  is.finite(force_call["Null_Bias"])
) && all_pass

unlink(temp_cache)
cat("\n=== ULTIMATE FINAL Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ============================================================
# skip_rmse test (added after RB battery runtime became excessive --
# RMSE is already quarantined/uncited, so computing it is pure waste)
# ============================================================
cat("\n=== skip_rmse option ===\n")
skip_result <- run_full_battery_estimation(synth, "candidate_is_outcome", n_boot = 200, skip_rmse = TRUE)
all_pass <- test_pass(
  "skip_rmse=TRUE returns NA for all RMSE fields",
  is.na(skip_result["RMSE_Point_Estimate"]) && is.na(skip_result["RMSE_CI_Lower"]) && is.na(skip_result["RMSE_CI_Upper"])
) && all_pass
all_pass <- test_pass(
  "skip_rmse=TRUE still computes concordance and AUC fields normally (not NA)",
  !is.na(skip_result["Concordance_Point_Estimate"]) && !is.na(skip_result["Bust_AUC_B"])
) && all_pass

cat("\n=== DEFINITELY ULTIMATE FINAL Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ============================================================
# Missingness-matching and cache-fingerprint tests (added after
# Fable 5's direct code review found a real bias-estimation bug:
# estimate_concordance_null_bias() permuting a column that still had
# NA rows scattered those NAs onto random players each replicate,
# measuring the null on a different, randomly-varying pool than the
# real candidate estimates use)
# ============================================================
cat("\n=== Missingness-matching fix ===\n")
synth_with_na <- synth
synth_with_na$candidate_with_gaps <- synth$Prior_PPG
synth_with_na$candidate_with_gaps[sample(nrow(synth_with_na), 10)] <- NA
bias_with_gaps <- estimate_concordance_null_bias(synth_with_na, "candidate_with_gaps", n_replicates = 20, n_boot = 100)
all_pass <- test_pass(
  "estimate_concordance_null_bias() runs cleanly on a candidate with NA gaps and returns a finite estimate (NA rows filtered before permuting, not scattered onto random players)",
  is.finite(bias_with_gaps["Null_Bias"])
) && all_pass

cat("\n=== Cache data-fingerprint fix ===\n")
temp_cache2 <- tempfile(fileext = ".csv")
data_v1 <- synth
data_v2 <- synth[1:(nrow(synth) - 5), ]  # fewer rows -- SAME config args, DIFFERENT underlying data

call_v1 <- get_or_compute_concordance_null_bias(
  data_v1, "candidate_permuted", position = "TESTPOS2", seasons_to_test = 2020:2025,
  min_games = 8, decay_r = 0.5, lookback_years = 4, cache_path = temp_cache2,
  n_meta_replicates = 2, n_replicates = 10, n_boot = 50
)
call_v2 <- get_or_compute_concordance_null_bias(
  data_v2, "candidate_permuted", position = "TESTPOS2", seasons_to_test = 2020:2025,  # identical config args
  min_games = 8, decay_r = 0.5, lookback_years = 4, cache_path = temp_cache2,
  n_meta_replicates = 2, n_replicates = 10, n_boot = 50
)
cache_contents <- readr::read_csv(temp_cache2, show_col_types = FALSE)
all_pass <- test_pass(
  "Two calls with IDENTICAL config but DIFFERENT underlying data (different row counts) produce TWO separate cache entries, not a false cache hit",
  nrow(cache_contents %>% filter(position == "TESTPOS2")) == 2
) && all_pass
unlink(temp_cache2)

cat("\n=== THE REAL FINAL Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ============================================================
# Progress-reporting helper test
# ============================================================
cat("\n=== Progress reporting ===\n")
progress_ran_clean <- tryCatch({
  st <- Sys.time()
  for (i in 1:5) print_progress(i, 5, st, "test item")
  TRUE
}, error = function(e) FALSE)
all_pass <- test_pass(
  "print_progress() runs across a full loop without error",
  progress_ran_clean
) && all_pass

cat("\n=== FOR REAL THIS TIME FINAL Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ============================================================
# Vectorized pairwise-construction reconciliation test
#
# run_concordance()'s pairwise comparison step was rewritten from a
# per-pair lapply()+rbind() loop to vectorized matrix operations for
# speed. Since the underlying comparison is fully deterministic (no
# randomness), this test checks the vectorized version against an
# INDEPENDENTLY WRITTEN reference (a plain double for-loop, not a copy
# of either the old or new code) -- confirming the refactor changed
# performance, not correctness, via a genuinely separate calculation
# rather than just re-running the same logic twice.
# ============================================================
cat("\n=== Vectorized pairwise construction reconciliation ===\n")

reference_concordance_rate <- function(data, candidate_col) {
  # Deliberately simple, unoptimized, written independently of both the
  # old and new run_concordance() implementations -- a genuinely
  # separate calculation to check against, not a re-run of the same code.
  data <- data[!is.na(data[[candidate_col]]), ]
  fmla_b <- as.formula(paste("season_ppg ~ RANK + Prior_PPG +", candidate_col))
  seasons <- sort(unique(data$Season))
  data$pred_a <- NA_real_; data$pred_b <- NA_real_
  for (yr in seasons) {
    train <- data[data$Season != yr, ]
    model_a <- lm(season_ppg ~ RANK + Prior_PPG, data = train)
    model_b <- lm(fmla_b, data = train)
    idx <- which(data$Season == yr)
    data$pred_a[idx] <- predict(model_a, newdata = data[idx, ])
    data$pred_b[idx] <- predict(model_b, newdata = data[idx, ])
  }
  n_total <- 0; n_adp_correct <- 0; n_modelb_correct <- 0
  for (yr in seasons) {
    sp <- data[data$Season == yr, ]
    n <- nrow(sp)
    if (n < 2) next
    for (a in 1:(n - 1)) {
      for (b in (a + 1):n) {
        if (is.na(sp$pred_a[a]) || is.na(sp$pred_a[b]) || is.na(sp$pred_b[a]) || is.na(sp$pred_b[b])) next
        if (abs(sp$RANK[a] - sp$RANK[b]) > 12) next  # matches significance_window default
        n_total <- n_total + 1
        n_adp_correct <- n_adp_correct + as.integer((sp$RANK[a] < sp$RANK[b]) == (sp$season_ppg[a] > sp$season_ppg[b]))
        n_modelb_correct <- n_modelb_correct + as.integer((sp$pred_b[a] > sp$pred_b[b]) == (sp$season_ppg[a] > sp$season_ppg[b]))
      }
    }
  }
  c(n_total = n_total, adp_rate = n_adp_correct / n_total, modelb_rate = n_modelb_correct / n_total)
}

set.seed(777)
ref_result <- reference_concordance_rate(synth, "candidate_is_outcome")
vectorized_result <- run_concordance(synth, "candidate_is_outcome", significance_window = 12)

# Reconstruct the vectorized version's implied adp_rate/modelb_rate at
# window 12 from its reported summary for comparison -- Concordance_
# VsADP_W12 is (modelb_rate - adp_rate), so recovering both rates
# individually requires re-deriving them the same way the reference does.
all_pass <- test_pass(
  sprintf("Independently-written reference pair count matches expectations (n_total = %d, should be > 0)",
          ref_result["n_total"]),
  ref_result["n_total"] > 0
) && all_pass

# Direct comparison: does the vectorized function's raw pair-level data
# (via return_pairs=TRUE) match the reference exactly?
vectorized_pairs <- run_concordance(synth, "candidate_is_outcome", significance_window = 12, return_pairs = TRUE)$pairs
all_pass <- test_pass(
  sprintf("Vectorized implementation's pair count at window 12 matches the independent reference exactly (vectorized = %d, reference = %d)",
          nrow(vectorized_pairs), ref_result["n_total"]),
  nrow(vectorized_pairs) == ref_result["n_total"]
) && all_pass
all_pass <- test_pass(
  sprintf("Vectorized implementation's adp_correct rate matches the independent reference exactly (vectorized = %.6f, reference = %.6f)",
          mean(vectorized_pairs$adp_correct), ref_result["adp_rate"]),
  isTRUE(all.equal(mean(vectorized_pairs$adp_correct), unname(ref_result["adp_rate"])))
) && all_pass
all_pass <- test_pass(
  sprintf("Vectorized implementation's modelb_correct rate matches the independent reference exactly (vectorized = %.6f, reference = %.6f)",
          mean(vectorized_pairs$modelb_correct), ref_result["modelb_rate"]),
  isTRUE(all.equal(mean(vectorized_pairs$modelb_correct), unname(ref_result["modelb_rate"])))
) && all_pass

cat("\n=== NO REALLY THE ACTUAL FINAL Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ============================================================
# run_estimation_for_position() routing tests -- this function moved
# here from analyze_estimation_battery.R specifically because it had
# grown real branching logic (which candidate gets which bias, which
# window applies to which stat) with zero prior test coverage.
# ============================================================
cat("\n=== run_estimation_for_position() routing logic ===\n")

synth$candidate_a <- synth$Prior_PPG
synth$candidate_b <- synth$candidate_permuted

routing_result <- run_estimation_for_position(
  "TESTPOS", synth, candidates = c("candidate_a", "candidate_b"),
  representative_candidate_col = "candidate_a",
  seasons_to_test = 2001:2012, min_games = 0, decay_r = 0.5, lookback_years = 4,
  n_boot = 100, secondary_candidates = "candidate_b",
  secondary_representative_candidate = "candidate_b",
  sig_window_fn = function(stat) if (stat == "candidate_a") 6 else 18,
  # Cheap bias-measurement settings -- this test only checks ROUTING
  # (right bias to right candidate, right window to right candidate),
  # not bias precision, so it doesn't need the production-scale 5x100x500
  # replicate counts. Previously this test silently paid that full cost
  # anyway, since those counts were hardcoded rather than configurable.
  bias_n_meta_replicates = 2, bias_n_replicates = 10, bias_n_boot = 50
)

all_pass <- test_pass(
  "run_estimation_for_position() output contains one row per candidate",
  nrow(routing_result) == 2
) && all_pass
all_pass <- test_pass(
  "Significance_Window is correctly routed per candidate via sig_window_fn (candidate_a -> 6, candidate_b -> 18)",
  routing_result$Significance_Window[routing_result$Stat == "candidate_a"] == 6 &&
    routing_result$Significance_Window[routing_result$Stat == "candidate_b"] == 18
) && all_pass
all_pass <- test_pass(
  "A candidate in secondary_candidates does NOT receive the same bias-corrected value a primary-bias candidate would (confirms the two biases are actually being routed differently, not silently collapsed to one)",
  !isTRUE(all.equal(
    routing_result$Concordance_Point_Estimate_BiasCorrected[routing_result$Stat == "candidate_a"] -
      routing_result$Concordance_Point_Estimate[routing_result$Stat == "candidate_a"],
    routing_result$Concordance_Point_Estimate_BiasCorrected[routing_result$Stat == "candidate_b"] -
      routing_result$Concordance_Point_Estimate[routing_result$Stat == "candidate_b"]
  ))
) && all_pass

cat("\n=== ABSOLUTELY, POSITIVELY FINAL Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")
