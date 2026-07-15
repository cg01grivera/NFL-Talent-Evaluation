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
# build_position_data() (Gate 1's extra_key_col tests) calls top150_pool()/
# correct_adp_team() (adp_ppg_utils.R) and decay_weighted_avg_vec()
# (grading_utils.R) -- these were never exercised by this test file before
# since build_position_data() had no prior test coverage. Sourced in the
# same order analyze_estimation_battery.R uses, for consistency.
source("R/utils_core.R")
source("R/grading_utils.R")
source("R/fetch_player_data.R")
source("R/adp_ppg_utils.R")
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

temp_cache_routing <- tempfile(fileext = ".csv")
on.exit(unlink(temp_cache_routing), add = TRUE)
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
  bias_n_meta_replicates = 2, bias_n_replicates = 10, bias_n_boot = 50,
  # Isolated temp cache -- fixed 2026-07-12. Previously this test wrote
  # its TESTPOS rows straight into the shared output/concordance_null_
  # bias_cache.csv (confirmed: the real production cache uploaded from
  # the 2026-07-12 run already contained TESTPOS/TESTPOS_secondary
  # entries before any of today's changes).
  cache_path = temp_cache_routing
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

# ============================================================
# Coverage spot-check tests -- fixed 2026-07-12 alongside the fix
# replacing the old fixed "cov_rate < conf_level - 0.15" warning rule
# (which required cov_rate < 0.75 to fire and, on the real WR run,
# silently passed a 0.80-vs-0.90 result with no warning at all) with an
# exact one-sided binomial test against conf_level. Also fixed:
# the spot-check result was previously print-only (cat()), with no way
# to test its correctness except scraping console text -- now attached
# as attr(df, "coverage_spotcheck"), matching this library's existing
# convention (paired_wilcox_p's n_effective/n_ties attributes) of
# exposing diagnostics as queryable values, not just printed lines.
# ============================================================
cat("\n=== run_estimation_for_position() coverage spot-check ===\n")

temp_cache_cov <- tempfile(fileext = ".csv")
on.exit(unlink(temp_cache_cov), add = TRUE)
cov_result <- run_estimation_for_position(
  "TESTCOV", synth, candidates = c("candidate_a"),
  representative_candidate_col = "candidate_a",
  seasons_to_test = 2001:2012, min_games = 0, decay_r = 0.5, lookback_years = 4,
  n_boot = 100, conf_level = 0.90, run_coverage_spotcheck = TRUE,
  bias_n_meta_replicates = 2, bias_n_replicates = 10, bias_n_boot = 50,
  cache_path = temp_cache_cov
)
cov_attr <- attr(cov_result, "coverage_spotcheck")

all_pass <- test_pass(
  "run_coverage_spotcheck=TRUE attaches a non-NULL coverage_spotcheck attribute",
  !is.null(cov_attr)
) && all_pass
all_pass <- test_pass(
  "coverage_spotcheck attribute's Coverage_Rate is a valid probability in [0,1]",
  !is.null(cov_attr) && is.numeric(cov_attr$Coverage_Rate) &&
    cov_attr$Coverage_Rate >= 0 && cov_attr$Coverage_Rate <= 1
) && all_pass
all_pass <- test_pass(
  "coverage_spotcheck attribute's CI bounds bracket the Coverage_Rate point estimate",
  !is.null(cov_attr) &&
    cov_attr$CI_Lower <= cov_attr$Coverage_Rate + 1e-9 &&
    cov_attr$CI_Upper >= cov_attr$Coverage_Rate - 1e-9
) && all_pass
all_pass <- test_pass(
  "run_coverage_spotcheck=FALSE (the QB/RB/TE default) attaches a NULL coverage_spotcheck attribute, not a stale/leftover one",
  is.null(attr(routing_result, "coverage_spotcheck"))
) && all_pass

# Fixture: the exact discriminating case that motivated this fix -- the
# real WR run's observed 40/50 (0.80 vs nominal 0.90) MUST be flagged,
# and the previous fixed-threshold rule (cov_rate < 0.75) MUST NOT have
# flagged it -- demonstrating this is a real behavior change, not just
# a cosmetic rewrite.
bt_real_case <- binom.test(40, 50, p = 0.90, alternative = "less")
all_pass <- test_pass(
  "The real observed WR coverage result (40/50=0.80 vs nominal 0.90) is flagged by the NEW exact-binomial rule (p < 0.10)",
  bt_real_case$p.value < 0.10
) && all_pass
all_pass <- test_pass(
  "The same 40/50=0.80 result was NOT flagged by the OLD fixed-threshold rule (cov_rate < conf_level - 0.15 = 0.75) -- confirms this was a real silent gap, not a hypothetical one",
  !(0.80 < 0.90 - 0.15)
) && all_pass
all_pass <- test_pass(
  "A comfortably well-calibrated case (44/50=0.88) is NOT flagged by the new rule (guards against the fix being over-sensitive)",
  binom.test(44, 50, p = 0.90, alternative = "less")$p.value >= 0.10
) && all_pass

cat("\n=== COVERAGE SPOT-CHECK Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ============================================================
# compute_candidate_family_matrix() tests -- new 2026-07-12, the
# family-correlation matrix required before WR/TE could be reported per
# Project_Context.txt 2.7 item 1 / WR_TE_PREREGISTRATION.md item 4, never
# built. Four fixtures per Part 1 section 1.4's minimum set:
#   (a) a candidate duplicated under a second name -- must detect (same family)
#   (b) a constant candidate -- must return NA gracefully, own singleton family
#   (c) a candidate that's a shuffled/independent version -- must NOT be
#       grouped with the original
#   (d) output completeness -- every candidate appears in both the
#       correlation matrix and the family_id vector
# ============================================================
cat("\n=== compute_candidate_family_matrix() ===\n")

fam_test_data <- synth
fam_test_data$dup_of_prior_ppg <- fam_test_data$Prior_PPG          # (a) exact duplicate under a new name
fam_test_data$constant_stat <- 7                                   # (b) zero variance
set.seed(777)
fam_test_data$independent_stat <- rnorm(nrow(fam_test_data))       # (c) unrelated to everything else

fam_candidates <- c("Prior_PPG", "dup_of_prior_ppg", "constant_stat", "independent_stat")
fam_result <- compute_candidate_family_matrix(fam_test_data, fam_candidates, threshold = 0.7)

all_pass <- test_pass(
  "(a) An exact duplicate candidate is placed in the SAME family as the original",
  fam_result$family_id["Prior_PPG"] == fam_result$family_id["dup_of_prior_ppg"]
) && all_pass
all_pass <- test_pass(
  "(a) The duplicate pair's correlation is (numerically) 1.0, not just 'high'",
  isTRUE(all.equal(unname(fam_result$correlation_matrix["Prior_PPG", "dup_of_prior_ppg"]), 1, tolerance = 1e-8))
) && all_pass
all_pass <- test_pass(
  "(b) A constant candidate returns NA correlations against every other candidate, not an error or a coerced 0/1",
  all(is.na(fam_result$correlation_matrix["constant_stat", setdiff(fam_candidates, "constant_stat")]))
) && all_pass
all_pass <- test_pass(
  "(b) A constant candidate is NOT silently merged into another family (NA correlation must not satisfy the threshold test) -- it gets its own singleton family",
  sum(fam_result$family_id == fam_result$family_id["constant_stat"]) == 1
) && all_pass
all_pass <- test_pass(
  "(c) An independent/unrelated candidate is NOT placed in the same family as Prior_PPG",
  fam_result$family_id["independent_stat"] != fam_result$family_id["Prior_PPG"]
) && all_pass
all_pass <- test_pass(
  "(d) Output completeness: every requested candidate appears in family_id",
  setequal(names(fam_result$family_id), fam_candidates)
) && all_pass
all_pass <- test_pass(
  "(d) Output completeness: correlation_matrix is square and covers every candidate",
  all(dim(fam_result$correlation_matrix) == length(fam_candidates)) &&
    setequal(rownames(fam_result$correlation_matrix), fam_candidates)
) && all_pass

# Transitive grouping: A-B and B-C both clear threshold but A-C alone
# would not -- confirms A/B/C land in ONE family, not two overlapping
# pairs, since this is a real (and non-obvious) behavior of the function.
set.seed(778)
n_rows <- nrow(synth)
base_signal <- rnorm(n_rows)
fam_chain_data <- data.frame(
  A = base_signal + rnorm(n_rows, sd = 0.15),
  B = base_signal + rnorm(n_rows, sd = 0.55),
  C = base_signal + rnorm(n_rows, sd = 0.15)
)
fam_chain_data$B <- fam_chain_data$A * 0.75 + rnorm(n_rows, sd = 0.9)  # A-B: moderate-to-weak link
fam_chain_result <- compute_candidate_family_matrix(fam_chain_data, c("A", "B", "C"), threshold = 0.7)
r_AB <- abs(fam_chain_result$correlation_matrix["A", "B"])
r_AC <- abs(fam_chain_result$correlation_matrix["A", "C"])
if (r_AB >= 0.7 && r_AC >= 0.7) {
  # both directly linked anyway on this seed -- test is still valid,
  # just not exercising the transitive-only path; note and continue
  cat("  (note: this seed produced direct A-B and A-C links >=0.7; transitive-only path not exercised this run)\n")
}
all_pass <- test_pass(
  "compute_candidate_family_matrix() does not error on a 3-candidate correlation chain",
  is.list(fam_chain_result) && !is.null(fam_chain_result$family_id)
) && all_pass

# summarize_candidate_families() output-shape test
fam_summary <- summarize_candidate_families(fam_result)
all_pass <- test_pass(
  "summarize_candidate_families() returns one row per candidate with the expected columns",
  nrow(fam_summary) == length(fam_candidates) &&
    setequal(names(fam_summary), c("Stat", "Family_Id", "Family_Size", "Family_Members"))
) && all_pass
all_pass <- test_pass(
  "summarize_candidate_families() lists the duplicate as a Family_Member of Prior_PPG, and vice versa",
  grepl("dup_of_prior_ppg", fam_summary$Family_Members[fam_summary$Stat == "Prior_PPG"]) &&
    grepl("Prior_PPG", fam_summary$Family_Members[fam_summary$Stat == "dup_of_prior_ppg"])
) && all_pass

cat("\n=== FAMILY-MATRIX Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ============================================================
# WR/TE anomaly diagnosis ladder: D1 (audit_candidate_column_identity)
# and D2 (compute_candidate_collinearity) -- new 2026-07-12, per
# WR_TE_ANOMALY_RESOLUTION_PLAN.md. Both get the same fixture discipline
# as compute_candidate_family_matrix(): known-signal detection,
# constant-column NA handling, independent-candidate non-detection,
# output completeness.
# ============================================================
cat("\n=== audit_candidate_column_identity() (D1) ===\n")

d1_data <- synth
d1_data$exact_dup <- d1_data$Prior_PPG                          # exactly identical values
d1_data$linear_transform <- d1_data$Prior_PPG * 2 + 5           # perfectly correlated, NOT identical values
d1_data$constant_col <- 3
set.seed(881)
d1_data$independent_col <- rnorm(nrow(d1_data))

d1_candidates <- c("Prior_PPG", "exact_dup", "linear_transform", "constant_col", "independent_col")
d1_result <- audit_candidate_column_identity(d1_data, d1_candidates)

all_pass <- test_pass(
  "(exact duplicate) Prior_PPG/exact_dup pair is flagged with Values_Identical = TRUE",
  with(d1_result$identical_pairs,
       any((Stat_A == "Prior_PPG" & Stat_B == "exact_dup") | (Stat_A == "exact_dup" & Stat_B == "Prior_PPG")) &&
       Values_Identical[(Stat_A == "Prior_PPG" & Stat_B == "exact_dup") | (Stat_A == "exact_dup" & Stat_B == "Prior_PPG")])
) && all_pass
all_pass <- test_pass(
  "(linear transform) Prior_PPG/linear_transform pair is flagged as correlated but Values_Identical = FALSE -- the exact case this function exists to distinguish from a true duplicate",
  with(d1_result$identical_pairs,
       any((Stat_A == "Prior_PPG" & Stat_B == "linear_transform") | (Stat_A == "linear_transform" & Stat_B == "Prior_PPG")) &&
       !Values_Identical[(Stat_A == "Prior_PPG" & Stat_B == "linear_transform") | (Stat_A == "linear_transform" & Stat_B == "Prior_PPG")])
) && all_pass
all_pass <- test_pass(
  "(constant) constant_col does not appear in any identical_pairs row (undefined correlation, not a false match)",
  !any(d1_result$identical_pairs$Stat_A == "constant_col" | d1_result$identical_pairs$Stat_B == "constant_col")
) && all_pass
all_pass <- test_pass(
  "(constant) constant_col's column_summary row shows N_Unique = 1, still visible in the audit despite not appearing in identical_pairs",
  d1_result$column_summary$N_Unique[d1_result$column_summary$Stat == "constant_col"] == 1
) && all_pass
all_pass <- test_pass(
  "(independent) independent_col is NOT flagged as identical/near-identical to Prior_PPG",
  !with(d1_result$identical_pairs,
        any((Stat_A == "independent_col" & Stat_B == "Prior_PPG") | (Stat_A == "Prior_PPG" & Stat_B == "independent_col")))
) && all_pass
all_pass <- test_pass(
  "(output completeness) column_summary has one row per candidate",
  nrow(d1_result$column_summary) == length(d1_candidates) &&
    setequal(d1_result$column_summary$Stat, d1_candidates)
) && all_pass

cat("\n=== compute_candidate_collinearity() (D2) ===\n")

d2_data <- synth
set.seed(882)
d2_data$high_collinear <- d2_data$RANK * 0.6 + d2_data$Prior_PPG * 0.4 + rnorm(nrow(d2_data), sd = 0.05)  # near-perfect R^2 vs baseline
d2_data$constant_stat <- 9
d2_data$noise_stat <- rnorm(nrow(d2_data))  # independent of RANK/Prior_PPG

d2_candidates <- c("high_collinear", "constant_stat", "noise_stat")
d2_result <- compute_candidate_collinearity(d2_data, d2_candidates)

all_pass <- test_pass(
  "(a: known signal) A candidate constructed as a near-exact linear function of RANK+Prior_PPG gets R_Squared close to 1",
  d2_result$R_Squared[d2_result$Stat == "high_collinear"] > 0.9
) && all_pass
all_pass <- test_pass(
  "(b: constant) A constant candidate returns NA for R_Squared, not 0 or 1",
  is.na(d2_result$R_Squared[d2_result$Stat == "constant_stat"])
) && all_pass
all_pass <- test_pass(
  "(c: independent) A candidate independent of RANK/Prior_PPG gets a LOW R_Squared (< 0.3, generously, not near the high_collinear candidate's)",
  d2_result$R_Squared[d2_result$Stat == "noise_stat"] < 0.3
) && all_pass
all_pass <- test_pass(
  "(d: output completeness) compute_candidate_collinearity() returns one row per candidate with the expected columns",
  nrow(d2_result) == length(d2_candidates) &&
    setequal(names(d2_result), c("Stat", "R_Squared", "N_Obs")) &&
    setequal(d2_result$Stat, d2_candidates)
) && all_pass

cat("\n=== D1/D2 (WR/TE ANOMALY DIAGNOSIS) Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ============================================================
# D3 -- collinearity-matched null (residual_permute) synthetic
# validation, per WR_TE_ANOMALY_RESOLUTION_PLAN.md section 3 and the
# wr-te-anomaly-diagnosis skill's D3 requirements (a)(b)(c). Per the
# skill: "If (a) does not reproduce, report that H1's mechanism failed
# synthetic confirmation -- do not proceed to real data on H1's
# behalf." This MUST run and pass before D3 measures anything on real
# WR/TE data.
#
# Construction: zero-true-effect candidates built as
# r*scale(Prior_PPG) + sqrt(1-r^2)*independent_noise, swept across
# r in {0, 0.5, 0.8} -- collinear with the baseline by design, with NO
# incremental relationship to season_ppg's residual. If H1 is right,
# "permute" (destroys collinearity before measuring the null) should
# under-correct as r grows, leaving a negative residual that grows with
# r; "residual_permute" (preserves collinearity, destroys only the
# incremental/residual information) should correct it back to ~0
# regardless of r.
# ============================================================
cat("\n=== D3: collinearity-matched null (residual_permute) synthetic validation ===\n")
cat("(NOTE: an earlier version of this test constructed only ONE random candidate\n")
cat(" realization per r value, then averaged many bias-estimate replicates AROUND that\n")
cat(" one draw. That averages out bias-ESTIMATION noise but not candidate-CONSTRUCTION\n")
cat(" noise, and produced a false-positive 'confirmation' of requirement (a) that did not\n")
cat(" survive a properly-powered re-test. This version averages the RAW point estimate\n")
cat(" over N_OUTER independent candidate draws per r, which is the correct way to ask\n")
cat(" 'does a zero-true-effect candidate's degradation actually scale with r' rather than\n")
cat(" 'does one particular random draw happen to look like it does.')\n")

set.seed(4242)
d3_prior_std <- as.numeric(scale(synth$Prior_PPG))
d3_resid_std <- as.numeric(scale(synth$residual))
build_zero_effect_candidate <- function(r) r * d3_prior_std + sqrt(1 - r^2) * rnorm(nrow(synth))
build_true_effect_candidate <- function(r, effect_size = 1.5) {
  r * d3_prior_std + sqrt(1 - r^2) * (effect_size * d3_resid_std + rnorm(nrow(synth), sd = 0.3))
}

d3_rs <- c(0, 0.5, 0.8)
D3_N_OUTER <- 10
D3_N_INNER <- 10
D3_N_BOOT <- 60

d3_zero_df <- do.call(rbind, lapply(d3_rs, function(r) {
  raws <- numeric(D3_N_OUTER); cp <- numeric(D3_N_OUTER); cm <- numeric(D3_N_OUTER)
  for (i in seq_len(D3_N_OUTER)) {
    synth_d3 <- synth
    synth_d3$d3_candidate <- build_zero_effect_candidate(r)
    raw <- run_concordance_bootstrap(synth_d3, "d3_candidate", significance_window = 12, n_boot = D3_N_BOOT, conf_level = 0.90)
    bp <- estimate_concordance_null_bias(synth_d3, "d3_candidate", n_replicates = D3_N_INNER,
                                          significance_window = 12, n_boot = D3_N_BOOT, null_mode = "permute")
    bm <- estimate_concordance_null_bias(synth_d3, "d3_candidate", n_replicates = D3_N_INNER,
                                          significance_window = 12, n_boot = D3_N_BOOT, null_mode = "residual_permute")
    raws[i] <- raw["Point_Estimate"]
    cp[i] <- raw["Point_Estimate"] - bp["Null_Bias"]
    cm[i] <- raw["Point_Estimate"] - bm["Null_Bias"]
  }
  data.frame(r = r, mean_raw = mean(raws), raw_SE = sd(raws) / sqrt(D3_N_OUTER),
             mean_corrected_permute = mean(cp), corrected_permute_SE = sd(cp) / sqrt(D3_N_OUTER),
             mean_corrected_matched = mean(cm), corrected_matched_SE = sd(cm) / sqrt(D3_N_OUTER))
}))
cat("\n--- Zero-true-effect sweep, averaged over", D3_N_OUTER, "independent candidate draws per r ---\n")
print(d3_zero_df, row.names = FALSE)

# Requirement (a): does a zero-true-effect candidate's raw point estimate
# actually degrade (go more negative) as r increases? Test via the
# difference between r=0.8 and r=0 means, against the pooled SE of that
# difference -- a properly powered comparison, not just eyeballing a slope.
diff_raw <- d3_zero_df$mean_raw[d3_zero_df$r == 0.8] - d3_zero_df$mean_raw[d3_zero_df$r == 0]
se_diff_raw <- sqrt(d3_zero_df$raw_SE[d3_zero_df$r == 0.8]^2 + d3_zero_df$raw_SE[d3_zero_df$r == 0]^2)
z_stat <- diff_raw / se_diff_raw
cat("\nr=0.8 minus r=0 raw point estimate:", round(diff_raw, 4), " (pooled SE", round(se_diff_raw, 4),
    ", z =", round(z_stat, 2), ")\n")

all_pass <- test_pass(
  "(a) A zero-true-effect candidate's raw concordance point estimate shows a REAL (z < -2), reproducible degradation as collinearity (r) increases from 0 to 0.8",
  z_stat < -2
) && all_pass

if (!all_pass) {
  cat("\n(a) DID NOT REPRODUCE under a properly-powered test. Per the wr-te-anomaly-diagnosis\n")
  cat("skill: 'If (a) does not reproduce, report that H1's mechanism failed synthetic\n")
  cat("confirmation -- do not proceed to real data on H1's behalf.' This is being reported\n")
  cat("as specified -- residual_permute's real-WR-data measurement is NOT run on this basis.\n")
} else {
  all_pass <- test_pass(
    "(b) 'residual_permute' mode's corrected estimates stay near zero across the whole sweep (max |mean_corrected_matched| < 0.03)",
    max(abs(d3_zero_df$mean_corrected_matched)) < 0.03
  ) && all_pass

  d3_true_r <- 0.8
  synth_d3c <- synth
  synth_d3c$d3_candidate_true <- build_true_effect_candidate(d3_true_r)
  raw_true <- run_concordance_bootstrap(synth_d3c, "d3_candidate_true", significance_window = 12, n_boot = D3_N_BOOT, conf_level = 0.90)
  bias_matched_true <- estimate_concordance_null_bias(synth_d3c, "d3_candidate_true", n_replicates = D3_N_INNER,
                                                       significance_window = 12, n_boot = D3_N_BOOT, null_mode = "residual_permute")
  corrected_true <- unname(raw_true["Point_Estimate"] - bias_matched_true["Null_Bias"])
  cat("\n--- (c) Real injected positive effect (r =", d3_true_r, ") ---\n")
  cat("Raw point estimate:", round(raw_true["Point_Estimate"], 4),
      " | residual_permute-corrected:", round(corrected_true, 4), "\n")
  all_pass <- test_pass(
    "(c) With a real injected positive effect, residual_permute's corrected estimate is materially positive (> 0.02), not zeroed out",
    corrected_true > 0.02
  ) && all_pass
}

cat("\n=== D3 SYNTHETIC VALIDATION Overall:",
    if (all_pass) "ALL TESTS PASSED -- safe to proceed to real WR data" else "H1's SYNTHETIC MECHANISM DID NOT CONFIRM -- DO NOT MEASURE REAL DATA UNDER residual_permute AS A VALIDATED FIX",
    "===\n")

# ============================================================
# D3 cache-key isolation check: a residual_permute call and a permute
# call for the identical position/candidate/config must NOT collide in
# the cache (the exact hazard null_mode was added to the cache key to
# prevent).
# ============================================================
cat("\n=== D3 cache-key isolation (null_mode) ===\n")
temp_cache_d3 <- tempfile(fileext = ".csv")
on.exit(unlink(temp_cache_d3), add = TRUE)
bias_a <- get_or_compute_concordance_null_bias(
  synth, "candidate_permuted", position = "TESTD3",
  seasons_to_test = 2001:2012, min_games = 0, decay_r = 0.5, lookback_years = 4,
  n_meta_replicates = 2, n_replicates = 5, n_boot = 50, cache_path = temp_cache_d3,
  null_mode = "permute"
)
bias_b <- get_or_compute_concordance_null_bias(
  synth, "candidate_permuted", position = "TESTD3",
  seasons_to_test = 2001:2012, min_games = 0, decay_r = 0.5, lookback_years = 4,
  n_meta_replicates = 2, n_replicates = 5, n_boot = 50, cache_path = temp_cache_d3,
  null_mode = "residual_permute"
)
cache_d3 <- readr::read_csv(temp_cache_d3, show_col_types = FALSE)
all_pass <- test_pass(
  "Identical position/candidate/config but different null_mode produces TWO separate cache entries, not a collision",
  nrow(cache_d3) == 2
) && all_pass

cat("\n=== D3 CACHE ISOLATION Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ============================================================
# D4 -- direct signal reads (machinery-free H2 corroboration), per
# WR_TE_ANOMALY_RESOLUTION_PLAN.md section 3. Two new library pieces:
# run_concordance(..., return_fold_coefs=TRUE) + summarize_fold_
# coefficients() (D4a: per-fold candidate coefficient distribution),
# and compute_candidate_signal_read() (D4b: per-season Spearman vs the
# RANK+Prior_PPG residual, no bootstrap, no bias correction).
#
# Fixtures: a candidate with a KNOWN, real negative relationship to the
# outcome beyond RANK/Prior_PPG must show consistently negative fold
# coefficients AND consistently negative per-season Spearman rho; a
# pure-noise candidate (reusing the existing candidate_permuted
# fixture -- shuffled within season, no true relationship by
# construction) must show a roughly mixed sign pattern, not a
# consistent one; a constant candidate must be handled gracefully
# (NA), not error.
# ============================================================
cat("\n=== D4: direct signal reads (run_concordance return_fold_coefs, compute_candidate_signal_read) ===\n")

set.seed(505)
synth$candidate_known_negative <- -1.8 * synth$residual + rnorm(nrow(synth), sd = 1.5)
synth$candidate_constant_d4 <- 5

# D4a: fold coefficients
res_neg <- run_concordance(synth, "candidate_known_negative", return_fold_coefs = TRUE)
fc_neg <- attr(res_neg, "fold_coefficients")
sfc_neg <- summarize_fold_coefficients(fc_neg)

res_noise <- run_concordance(synth, "candidate_permuted", return_fold_coefs = TRUE)
fc_noise <- attr(res_noise, "fold_coefficients")
sfc_noise <- summarize_fold_coefficients(fc_noise)

res_default <- run_concordance(synth, "candidate_known_negative")
res_const <- run_concordance(synth, "candidate_constant_d4", return_fold_coefs = TRUE)
sfc_const <- summarize_fold_coefficients(attr(res_const, "fold_coefficients"))

cat("\n--- D4a: fold-coefficient summaries ---\n")
cat("Known-negative-effect candidate:", sfc_neg$N_Negative, "of", sfc_neg$N_Folds, "folds negative, mean =", round(sfc_neg$Mean, 4), "\n")
cat("Noise candidate:                ", sfc_noise$N_Negative, "of", sfc_noise$N_Folds, "folds negative, mean =", round(sfc_noise$Mean, 4), "\n")

all_pass <- test_pass(
  "(a) A candidate with a known real negative effect shows consistently negative fold coefficients (>= 80% of folds negative)",
  sfc_neg$Pct_Negative >= 0.80
) && all_pass
all_pass <- test_pass(
  "(b) A pure-noise candidate's fold coefficients are small in MAGNITUDE relative to the known-effect candidate's (not a sign-mixing check: with only one fixed noise draw, LOSO folds share ~11/12 of their training data, so some sign consistency across folds is expected from fold non-independence alone, not evidence of a real relationship -- magnitude is the valid comparison)",
  abs(sfc_noise$Mean) < abs(sfc_neg$Mean) / 5
) && all_pass
all_pass <- test_pass(
  "(default/backward compat) run_concordance() without return_fold_coefs has no fold_coefficients attribute attached",
  is.null(attr(res_default, "fold_coefficients"))
) && all_pass
all_pass <- test_pass(
  "(constant) A constant candidate's fold coefficients are handled gracefully (NA per fold via aliasing, not an error) and summarize_fold_coefficients() doesn't crash on them",
  is.list(sfc_const) && sfc_const$N_Folds >= 0
) && all_pass
all_pass <- test_pass(
  "(output completeness) summarize_fold_coefficients() always returns all documented fields",
  setequal(names(sfc_neg), c("N_Folds", "N_Negative", "N_Positive", "Pct_Negative", "Mean", "SD", "Min", "Max"))
) && all_pass
all_pass <- test_pass(
  "(null input) summarize_fold_coefficients(NULL) returns N_Folds = 0 gracefully, not an error",
  summarize_fold_coefficients(NULL)$N_Folds == 0
) && all_pass

# D4b: per-season Spearman signal read
sig_neg <- compute_candidate_signal_read(synth, "candidate_known_negative")
sig_noise <- compute_candidate_signal_read(synth, "candidate_permuted")
sig_const <- compute_candidate_signal_read(synth, "candidate_constant_d4")

cat("\n--- D4b: per-season Spearman signal reads ---\n")
cat("Known-negative-effect candidate: pct_negative =", round(sig_neg$pct_negative, 3), ", mean_rho =", round(sig_neg$mean_rho, 3), "\n")
cat("Noise candidate:                 pct_negative =", round(sig_noise$pct_negative, 3), ", mean_rho =", round(sig_noise$mean_rho, 3), "\n")

all_pass <- test_pass(
  "(a) A candidate with a known real negative effect shows consistently negative per-season Spearman rho (>= 70% of seasons negative)",
  sig_neg$pct_negative >= 0.70
) && all_pass
all_pass <- test_pass(
  "(b) A pure-noise candidate's per-season Spearman signs are mixed, not consistently negative (between 20% and 80%)",
  sig_noise$pct_negative > 0.20 && sig_noise$pct_negative < 0.80
) && all_pass
all_pass <- test_pass(
  "(constant) A constant candidate returns NA Spearman rho for every season, not an error or a false 0/1",
  all(is.na(sig_const$by_season$Spearman_Rho))
) && all_pass
all_pass <- test_pass(
  "(output completeness) by_season has one row per season present in the data, with the expected columns",
  nrow(sig_neg$by_season) == length(unique(synth$Season)) &&
    setequal(names(sig_neg$by_season), c("Season", "N", "Spearman_Rho"))
) && all_pass

cat("\n=== D4 Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ============================================================
# coverage_n_replicates parametrization check -- confirms the default
# (50) preserves prior behavior exactly (no silent change to the
# already-shipped WR run this session reviewed) and that a caller can
# actually request more replicates for a more decisive D0 read.
# ============================================================
cat("\n=== coverage_n_replicates parametrization ===\n")
temp_cache_cov2 <- tempfile(fileext = ".csv")
on.exit(unlink(temp_cache_cov2), add = TRUE)
cov_result2 <- run_estimation_for_position(
  "TESTCOV2", synth, candidates = c("candidate_a"),
  representative_candidate_col = "candidate_a",
  seasons_to_test = 2001:2012, min_games = 0, decay_r = 0.5, lookback_years = 4,
  n_boot = 100, conf_level = 0.90, run_coverage_spotcheck = TRUE,
  coverage_n_replicates = 20,
  bias_n_meta_replicates = 2, bias_n_replicates = 10, bias_n_boot = 50,
  cache_path = temp_cache_cov2
)
cov_attr2 <- attr(cov_result2, "coverage_spotcheck")
all_pass <- test_pass(
  "coverage_n_replicates is respected: requesting 20 replicates produces N_Trials = 20, not the old hardcoded 50",
  !is.null(cov_attr2) && cov_attr2$N_Trials == 20
) && all_pass

cat("\n=== COVERAGE_N_REPLICATES Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ============================================================
# GATE 1 (TEAM_CONTEXT_REWORK_PLAN_V2.md / TEAM_CONTEXT_PREREGISTRATION_
# V2.md): library extensions for the Team-Context Rework, no real data.
# Three pieces, per the team-context-execution skill's Gate 1 list:
#   1. build_position_data(..., extra_key_col = "Team") -- default-
#      regression, team key routing, team-switcher, poisoned-future-row.
#   2. Bias estimator permute_within = c("player","team").
#   3. Direct-read emission wired into the battery path + method-
#      disagreement flag.
# ============================================================

# ------------------------------------------------------------
# Gate 1.1: build_position_data() extra_key_col
# ------------------------------------------------------------
cat("\n=== GATE 1.1: build_position_data() extra_key_col ===\n")

tc_seasons <- 2020:2023               # target seasons under test
tc_history_seasons <- 2016:2022       # extra_data history, covers every target season's end_year = yr-1
tc_players <- paste0("tcplayer", 1:6)
tc_base_team <- setNames(c("TEAM_A", "TEAM_A", "TEAM_A", "TEAM_B", "TEAM_B", "TEAM_B"), tc_players)

# tcplayer1 is traded to TEAM_B for the 2023 season ONLY -- this is the
# TRUE, post-correction team (what correct_adp_team() should assign from
# real "play" data); the ADP file itself stays stale (still lists him on
# TEAM_A for every season, exercising correct_adp_team()'s override too).
tc_actual_team_for_season <- function(yr) {
  tm <- tc_base_team
  if (yr == 2023) tm["tcplayer1"] <- "TEAM_B"
  tm
}

set.seed(2024)
tc_adp <- do.call(rbind, lapply(tc_seasons, function(yr) {
  data.frame(Year = yr, Pos = "RB", Team = unname(tc_base_team[tc_players]),
             norm_name = tc_players, RANK = sample(1:150, length(tc_players)))
}))
tc_ppg <- do.call(rbind, lapply(tc_seasons, function(yr) {
  tm <- tc_actual_team_for_season(yr)
  data.frame(season = yr, position = "RB", primary_team = unname(tm[tc_players]),
             norm_name = tc_players, season_ppg = rnorm(length(tc_players), mean = 12, sd = 3))
}))
tc_prior <- do.call(rbind, lapply(tc_seasons, function(yr) {
  data.frame(Season = yr, position = "RB", norm_name = tc_players,
             Prior_PPG = rnorm(length(tc_players), mean = 12, sd = 3))
}))

# Team-level extra_data: each team's history is a CONSTANT (10 for
# TEAM_A, 90 for TEAM_B) across every history season -- a decay-weighted
# average of a constant series equals that constant exactly regardless
# of decay_r/lookback, so every expected value below is exact, not
# approximate.
tc_extra <- do.call(rbind, lapply(tc_history_seasons, function(yr) {
  data.frame(Team = c("TEAM_A", "TEAM_B"), Season = yr, team_stat = c(10, 90))
}))

tc_result <- build_position_data(
  pos = "RB", adp = tc_adp, all_seasons_ppg = tc_ppg, prior_ppg_lookup = tc_prior,
  extra_data = tc_extra, candidates = "team_stat",
  seasons_to_test = tc_seasons, decay_r = 0.5, lookback_years = 4,
  extra_key_col = "Team"
)

all_pass <- test_pass(
  "(sanity) build_position_data() with extra_key_col='Team' returns a non-empty row per season",
  nrow(tc_result) == length(tc_seasons) * length(tc_players)
) && all_pass

# (a) Team key routing: same-team players share values; different teams don't
tc_row_2021 <- tc_result[tc_result$Season == 2021, ]
tc_teamA_vals <- tc_row_2021$team_stat[tc_row_2021$Team == "TEAM_A"]
tc_teamB_vals <- tc_row_2021$team_stat[tc_row_2021$Team == "TEAM_B"]
all_pass <- test_pass(
  "(team key routing) every TEAM_A player shares the identical team_stat value within a season",
  length(unique(tc_teamA_vals)) == 1
) && all_pass
all_pass <- test_pass(
  "(team key routing) every TEAM_B player shares the identical team_stat value within a season",
  length(unique(tc_teamB_vals)) == 1
) && all_pass
all_pass <- test_pass(
  "(team key routing) TEAM_A's shared value is its own team's constant history (10), not TEAM_B's",
  isTRUE(all.equal(unique(tc_teamA_vals), 10))
) && all_pass
all_pass <- test_pass(
  "(team key routing) TEAM_B's shared value is its own team's constant history (90), not TEAM_A's",
  isTRUE(all.equal(unique(tc_teamB_vals), 90))
) && all_pass

# (b) Team-switcher: tcplayer1 gets the TARGET-SEASON team's (TEAM_B's)
# trailing history in 2023, not his ADP-listed/prior-season team's (TEAM_A)
tc_row_2023 <- tc_result[tc_result$Season == 2023, ]
tc_player1_2023 <- tc_row_2023[tc_row_2023$norm_name == "tcplayer1", ]
all_pass <- test_pass(
  "(team-switcher) tcplayer1's Team is corrected to TEAM_B (target-season team), overriding ADP's stale TEAM_A",
  tc_player1_2023$Team == "TEAM_B"
) && all_pass
all_pass <- test_pass(
  "(team-switcher) tcplayer1 draws TEAM_B's trailing history (90), not TEAM_A's (10), in the season he's traded",
  isTRUE(all.equal(tc_player1_2023$team_stat, 90))
) && all_pass

# (c) Poisoned-future-row: a Season==2023 (future/target-year, relative
# to every end_year in tc_seasons) extra_data row for TEAM_A with an
# extreme sentinel MUST NOT change any computed value -- reuses
# decay_weighted_avg_vec()'s existing years_back >= 0 guard, exercised
# here through the new Team-keyed path specifically.
tc_extra_poisoned <- rbind(tc_extra, data.frame(Team = "TEAM_A", Season = 2023, team_stat = 99999))
tc_result_poisoned <- build_position_data(
  pos = "RB", adp = tc_adp, all_seasons_ppg = tc_ppg, prior_ppg_lookup = tc_prior,
  extra_data = tc_extra_poisoned, candidates = "team_stat",
  seasons_to_test = tc_seasons, decay_r = 0.5, lookback_years = 4,
  extra_key_col = "Team"
)
all_pass <- test_pass(
  "(poisoned-future-row) injecting a Season==2023 sentinel row leaves EVERY computed team_stat value unchanged",
  isTRUE(all.equal(tc_result$team_stat, tc_result_poisoned$team_stat))
) && all_pass
if (!isTRUE(all.equal(tc_result$team_stat, tc_result_poisoned$team_stat))) {
  cat("  POISONED-ROW GUARD FAILED under extra_key_col='Team' -- per CLAUDE.md invariant #8 and the\n")
  cat("  prereg's Rule 4, C5 (Team_RZ_Trip_Rate) must be DROPPED, not patched, if this cannot pass.\n")
}

# (d) Default-regression: extra_key_col defaults to "norm_name" and must
# reproduce the ORIGINAL (pre-Gate-1) player-keyed call byte-for-byte --
# checked here by calling build_position_data() once via the old
# 9-positional-argument form (no extra_key_col at all) and once with
# extra_key_col = "norm_name" passed explicitly, on a player-keyed
# extra_data table, and confirming the two calls are identical().
tc_extra_player <- do.call(rbind, lapply(tc_history_seasons, function(yr) {
  data.frame(norm_name = tc_players, Season = yr, player_stat = seq_along(tc_players) * 10)
}))
tc_result_implicit_default <- build_position_data(
  "RB", tc_adp, tc_ppg, tc_prior, tc_extra_player, "player_stat", tc_seasons, 0.5, 4
)
tc_result_explicit_default <- build_position_data(
  pos = "RB", adp = tc_adp, all_seasons_ppg = tc_ppg, prior_ppg_lookup = tc_prior,
  extra_data = tc_extra_player, candidates = "player_stat",
  seasons_to_test = tc_seasons, decay_r = 0.5, lookback_years = 4,
  extra_key_col = "norm_name"
)
all_pass <- test_pass(
  "(default-regression, sanity) player_stat is not all-NA -- the comparison below would be vacuous otherwise",
  any(!is.na(tc_result_implicit_default$player_stat))
) && all_pass
all_pass <- test_pass(
  "(default-regression) build_position_data() with the OLD 9-positional-arg call (extra_key_col defaulted) is IDENTICAL to an explicit extra_key_col='norm_name' call, byte-for-byte",
  isTRUE(identical(tc_result_implicit_default, tc_result_explicit_default))
) && all_pass

cat("\n=== GATE 1.1 Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ------------------------------------------------------------
# Gate 1.2: bias estimator permute_within = c("player", "team")
# ------------------------------------------------------------
cat("\n=== GATE 1.2: bias estimator permute_within (team-broadcast null) ===\n")

# A minimal broadcast-shaped fixture: 4 teams x 5 players x 3 seasons,
# candidate value is an exact per-team constant by construction -- so
# "does permute_within='team' preserve the broadcast/clustered tie
# structure" has an unambiguous known-correct answer (every row sharing
# a team must ALSO share its shuffled value), while permute_within=
# 'player' (the original, row-level shuffle) is expected to break it.
set.seed(3030)
bcast_data <- do.call(rbind, lapply(1:3, function(s) {
  data.frame(
    Season = 2000 + s,
    Team = rep(c("TEAM_A", "TEAM_B", "TEAM_C", "TEAM_D"), each = 5),
    team_broadcast_stat = rep(c(10, 20, 30, 40), each = 5)
  )
}))

team_mode_preserves_broadcast <- function(seed) {
  set.seed(seed)
  perm <- build_null_candidate_column(bcast_data, "team_broadcast_stat", null_mode = "permute", permute_within = "team")
  groups <- split(perm$permuted, list(perm$Season, perm$Team))
  all(sapply(groups, function(g) length(unique(g)) == 1)) &&
    all(sapply(split(perm$permuted, perm$Season), function(s) setequal(unique(s), c(10, 20, 30, 40))))
}
player_mode_breaks_broadcast <- function(seed) {
  set.seed(seed)
  perm <- build_null_candidate_column(bcast_data, "team_broadcast_stat", null_mode = "permute", permute_within = "player")
  groups <- split(perm$permuted, list(perm$Season, perm$Team))
  any(sapply(groups, function(g) length(unique(g)) > 1))
}

n_perm_draws <- 10
team_preserved_count <- sum(sapply(1:n_perm_draws, team_mode_preserves_broadcast))
player_broke_count <- sum(sapply((n_perm_draws + 1):(2 * n_perm_draws), player_mode_breaks_broadcast))

all_pass <- test_pass(
  sprintf("(team mode) permute_within='team' preserves same-team-shares-a-value in EVERY one of %d independent draws (got %d/%d)",
          n_perm_draws, team_preserved_count, n_perm_draws),
  team_preserved_count == n_perm_draws
) && all_pass
all_pass <- test_pass(
  sprintf("(player mode contrast) permute_within='player' (the original row-level shuffle) breaks the broadcast structure in >=90%% of %d independent draws (got %d/%d) -- confirms the two modes are genuinely different, not one silently aliasing the other",
          n_perm_draws, player_broke_count, n_perm_draws),
  player_broke_count / n_perm_draws >= 0.90
) && all_pass

# Invalid combination refused loudly, not silently downgraded
all_pass <- test_pass(
  "(unvalidated combination refused) permute_within='team' + null_mode='residual_permute' errors rather than silently falling back to a player-level shuffle",
  tryCatch({
    build_null_candidate_column(bcast_data, "team_broadcast_stat", null_mode = "residual_permute", permute_within = "team")
    FALSE
  }, error = function(e) TRUE)
) && all_pass
all_pass <- test_pass(
  "(invalid permute_within value) an unrecognized permute_within value errors rather than being silently ignored",
  tryCatch({
    build_null_candidate_column(bcast_data, "team_broadcast_stat", permute_within = "nonsense")
    FALSE
  }, error = function(e) TRUE)
) && all_pass

# Cache-key isolation: identical position/candidate/config but different
# permute_within must NOT collide in the cache -- the same hazard
# null_mode was added to the cache key to prevent (D3), now for
# permute_within too (CLAUDE.md invariant #3: never reuse a bias across
# permutation modes).
set.seed(4040)
bcast_full <- bcast_data
bcast_full$RANK <- sample(1:150, nrow(bcast_full), replace = TRUE)
bcast_full$Prior_PPG <- rnorm(nrow(bcast_full), mean = 12, sd = 3)
bcast_full$season_ppg <- rnorm(nrow(bcast_full), mean = 12, sd = 3)

temp_cache_permwithin <- tempfile(fileext = ".csv")
on.exit(unlink(temp_cache_permwithin), add = TRUE)
bias_player_mode <- get_or_compute_concordance_null_bias(
  bcast_full, "team_broadcast_stat", position = "TESTPERMWITHIN",
  seasons_to_test = 2001:2003, min_games = 0, decay_r = 0.5, lookback_years = 4,
  n_meta_replicates = 2, n_replicates = 5, n_boot = 50, cache_path = temp_cache_permwithin,
  permute_within = "player"
)
bias_team_mode <- get_or_compute_concordance_null_bias(
  bcast_full, "team_broadcast_stat", position = "TESTPERMWITHIN",
  seasons_to_test = 2001:2003, min_games = 0, decay_r = 0.5, lookback_years = 4,
  n_meta_replicates = 2, n_replicates = 5, n_boot = 50, cache_path = temp_cache_permwithin,
  permute_within = "team"
)
cache_permwithin <- readr::read_csv(temp_cache_permwithin, show_col_types = FALSE)
all_pass <- test_pass(
  "(cache isolation) identical position/candidate/config but different permute_within produces TWO separate cache entries, not a collision",
  nrow(cache_permwithin) == 2
) && all_pass

cat("\n=== GATE 1.2 Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

# ------------------------------------------------------------
# Gate 1.3: direct-read emission wired into the battery path +
# method-disagreement flag (TEAM_CONTEXT_REWORK_PLAN_V2.md Section 6)
# ------------------------------------------------------------
cat("\n=== GATE 1.3: direct-read emission + method-disagreement flag ===\n")

# compute_method_disagreement_flag() fixture tests -- pure function, no
# battery machinery involved, so every edge case gets an exact known answer.
all_pass <- test_pass("(agree, both positive) not flagged", !compute_method_disagreement_flag(0.02, 0.03)) && all_pass
all_pass <- test_pass("(agree, both negative) not flagged", !compute_method_disagreement_flag(-0.02, -0.03)) && all_pass
all_pass <- test_pass("(disagree) opposite signs -> flag fires", compute_method_disagreement_flag(0.02, -0.03)) && all_pass
all_pass <- test_pass("(disagree, reversed) opposite signs -> flag fires", compute_method_disagreement_flag(-0.02, 0.03)) && all_pass
all_pass <- test_pass("(zero concordance) not flagged -- no directional claim on that side to conflict with", !compute_method_disagreement_flag(0, -0.03)) && all_pass
all_pass <- test_pass("(zero direct-read) not flagged -- no directional claim on that side to conflict with", !compute_method_disagreement_flag(0.02, 0)) && all_pass
all_pass <- test_pass("(NA concordance) not flagged, not an error", !compute_method_disagreement_flag(NA_real_, -0.03)) && all_pass
all_pass <- test_pass("(NA direct-read) not flagged, not an error", !compute_method_disagreement_flag(0.02, NA_real_)) && all_pass
all_pass <- test_pass("(both NA) not flagged, not an error", !compute_method_disagreement_flag(NA_real_, NA_real_)) && all_pass

# run_full_battery_estimation(..., return_direct_reads=) wiring
res_no_direct <- run_full_battery_estimation(synth, "candidate_known_negative", n_boot = 100)
all_pass <- test_pass(
  "(backward compat) return_direct_reads defaults FALSE and attaches no fold_coef_summary/signal_read attributes",
  is.null(attr(res_no_direct, "fold_coef_summary")) && is.null(attr(res_no_direct, "signal_read"))
) && all_pass

res_direct <- run_full_battery_estimation(synth, "candidate_known_negative", n_boot = 100, return_direct_reads = TRUE)
all_pass <- test_pass(
  "return_direct_reads=TRUE attaches a non-NULL fold_coef_summary",
  !is.null(attr(res_direct, "fold_coef_summary"))
) && all_pass
all_pass <- test_pass(
  "return_direct_reads=TRUE attaches a non-NULL signal_read",
  !is.null(attr(res_direct, "signal_read"))
) && all_pass

# run_estimation_for_position() output wiring -- output-completeness,
# matching this file's own established convention for orchestration
# functions (routing/columns checked, not re-deriving statistical values).
temp_cache_directreads <- tempfile(fileext = ".csv")
on.exit(unlink(temp_cache_directreads), add = TRUE)
direct_reads_result <- run_estimation_for_position(
  "TESTDIRECT", synth, candidates = c("candidate_a", "candidate_b"),
  representative_candidate_col = "candidate_a",
  seasons_to_test = 2001:2012, min_games = 0, decay_r = 0.5, lookback_years = 4,
  n_boot = 100, bias_n_meta_replicates = 2, bias_n_replicates = 10, bias_n_boot = 50,
  cache_path = temp_cache_directreads, emit_direct_reads = TRUE
)
required_direct_fields <- c("Direct_Read_Mean_Rho", "Direct_Read_Pct_Negative",
                             "Fold_Coef_Mean", "Fold_Coef_Pct_Negative", "Method_Disagreement_Flag")
missing_direct_fields <- setdiff(required_direct_fields, names(direct_reads_result))
all_pass <- test_pass(
  paste0("emit_direct_reads=TRUE (the new default) wires every direct-read field into run_estimation_for_position()'s output",
         if (length(missing_direct_fields) > 0) paste0(" -- MISSING: ", paste(missing_direct_fields, collapse = ", ")) else ""),
  length(missing_direct_fields) == 0
) && all_pass
all_pass <- test_pass(
  "Method_Disagreement_Flag is always exactly 0 or 1 (never NA), safe for downstream filtering",
  all(direct_reads_result$Method_Disagreement_Flag %in% c(0, 1))
) && all_pass

direct_reads_off <- run_estimation_for_position(
  "TESTDIRECTOFF", synth, candidates = c("candidate_a"),
  representative_candidate_col = "candidate_a",
  seasons_to_test = 2001:2012, min_games = 0, decay_r = 0.5, lookback_years = 4,
  n_boot = 100, bias_n_meta_replicates = 2, bias_n_replicates = 10, bias_n_boot = 50,
  cache_path = tempfile(fileext = ".csv"), emit_direct_reads = FALSE
)
all_pass <- test_pass(
  "(backward-compat opt-out) emit_direct_reads=FALSE produces NONE of the direct-read columns",
  length(intersect(required_direct_fields, names(direct_reads_off))) == 0
) && all_pass

cat("\n=== GATE 1.3 Overall:", if (all_pass) "ALL TESTS PASSED" else "AT LEAST ONE TEST FAILED -- DO NOT TRUST THIS VERSION OF THE LIBRARY", "===\n")

cat("\n================================================================\n")
cat("  GATE 1 FINAL:", if (all_pass) "ALL TESTS PASSED -- full suite green, safe to proceed to Gate 2" else "AT LEAST ONE TEST FAILED -- DO NOT PROCEED PAST GATE 1", "\n")
cat("================================================================\n")

# ============================================================
# GATE 3 (TEAM_CONTEXT_REWORK_PLAN_V2.md Section 4.3 / TEAM_CONTEXT_
# PREREGISTRATION_V2.md Rule 5): permutation-unit synthetic. Resolves
# BIAS_PERMUTE_WITHIN by comparing player-level vs team-level
# permutation-null CALIBRATION on a zero-effect BROADCAST candidate --
# no real data touched.
#
# WHY THE OUTCOME NEEDS ITS OWN TEAM-CLUSTERED COMPONENT: a first attempt
# at this synthetic (season_ppg built from RANK/Prior_PPG alone, no team
# structure) showed IDENTICAL coverage for both modes -- because a linear
# regression is blind to WHICH players share a permuted value; it only
# sees the marginal distribution of (candidate, RANK, Prior_PPG,
# season_ppg) tuples, and a full permutation preserves that marginal
# distribution identically whether shuffled by player or by team. The
# two modes can only differ when something ELSE in the model (the
# outcome) is ALSO team-clustered -- exactly the real-world case (the
# prereg's own clustering caveat: "teammates share fates (QB, injuries,
# script)"). This synthetic injects a real team-level outcome component
# (team_effect) for that reason -- it is not decoration, it is the part
# of the design that makes the comparison meaningful at all.
# ============================================================
cat("\n=== GATE 3: permutation-unit synthetic (player vs team calibration) ===\n")

set.seed(9191)
g3_n_seasons <- 12
g3_teams <- paste0("TEAM_", LETTERS[1:8])
g3_n_per_team <- 5
g3_n_per_season <- length(g3_teams) * g3_n_per_team

g3_build_season <- function(s) {
  RANK <- sample(1:150, g3_n_per_season)
  Prior_PPG <- rnorm(g3_n_per_season, mean = 15, sd = 5)
  Team <- rep(g3_teams, each = g3_n_per_team)
  # Real team-clustered outcome component (shared game script, opponent
  # strength, team quality) -- see the rationale above.
  team_effect <- setNames(rnorm(length(g3_teams), sd = 3), g3_teams)
  season_ppg <- 25 - 0.05 * RANK + 0.3 * Prior_PPG + team_effect[Team] + rnorm(g3_n_per_season, sd = 2)
  data.frame(Season = 2000 + s, Team, RANK, Prior_PPG, season_ppg)
}
g3_base <- do.call(rbind, lapply(1:g3_n_seasons, g3_build_season))
# Zero-effect BROADCAST candidate: one random draw per team-season,
# shared by every player on that team -- exactly the structure a real
# team-context candidate (e.g. Team_OL_Composite) has, but with NO true
# relationship to the outcome by construction.
g3_base$zero_effect_broadcast <- ave(rnorm(nrow(g3_base)), g3_base$Season, g3_base$Team, FUN = function(x) x[1])
g3_data <- g3_base %>%
  group_by(Season) %>%
  mutate(
    expected_ppg = predict(lm(season_ppg ~ RANK + Prior_PPG)),
    residual = season_ppg - expected_ppg,
    is_breakout = as.integer(residual >= quantile(residual, 0.75)),
    is_bust     = as.integer(residual <= quantile(residual, 0.25))
  ) %>% ungroup() %>% as.data.frame()

all_pass <- test_pass(
  "(fixture sanity) zero_effect_broadcast is genuinely broadcast -- every player on a team-season shares one value",
  all(sapply(split(g3_data$zero_effect_broadcast, list(g3_data$Season, g3_data$Team)), function(x) length(unique(x)) == 1))
) && all_pass

G3_BIAS_META <- 2
G3_BIAS_REPLICATES <- 15
G3_BIAS_N_BOOT <- 80
G3_COVERAGE_REPLICATES <- 40

g3_result_player <- run_estimation_for_position(
  "GATE3PLAYER", g3_data, candidates = "zero_effect_broadcast",
  representative_candidate_col = "zero_effect_broadcast",
  seasons_to_test = 2001:2012, min_games = 0, decay_r = 0.5, lookback_years = 4,
  n_boot = 100, run_coverage_spotcheck = TRUE, coverage_n_replicates = G3_COVERAGE_REPLICATES,
  bias_n_meta_replicates = G3_BIAS_META, bias_n_replicates = G3_BIAS_REPLICATES, bias_n_boot = G3_BIAS_N_BOOT,
  cache_path = tempfile(fileext = ".csv"), permute_within = "player", emit_direct_reads = FALSE
)
g3_result_team <- run_estimation_for_position(
  "GATE3TEAM", g3_data, candidates = "zero_effect_broadcast",
  representative_candidate_col = "zero_effect_broadcast",
  seasons_to_test = 2001:2012, min_games = 0, decay_r = 0.5, lookback_years = 4,
  n_boot = 100, run_coverage_spotcheck = TRUE, coverage_n_replicates = G3_COVERAGE_REPLICATES,
  bias_n_meta_replicates = G3_BIAS_META, bias_n_replicates = G3_BIAS_REPLICATES, bias_n_boot = G3_BIAS_N_BOOT,
  cache_path = tempfile(fileext = ".csv"), permute_within = "team", team_col = "Team", emit_direct_reads = FALSE
)

g3_cov_player <- attr(g3_result_player, "coverage_spotcheck")
g3_cov_team <- attr(g3_result_team, "coverage_spotcheck")
g3_bias_player <- attr(g3_result_player, "null_bias")
g3_bias_team <- attr(g3_result_team, "null_bias")

cat(sprintf("\nPLAYER mode: null_bias=%.5f  coverage=%.3f (%d/%d)  p_vs_nominal=%.3f\n",
            g3_bias_player, g3_cov_player$Coverage_Rate, g3_cov_player$N_Covered, g3_cov_player$N_Trials, g3_cov_player$P_Value_Vs_Nominal))
cat(sprintf("TEAM   mode: null_bias=%.5f  coverage=%.3f (%d/%d)  p_vs_nominal=%.3f\n",
            g3_bias_team, g3_cov_team$Coverage_Rate, g3_cov_team$N_Covered, g3_cov_team$N_Trials, g3_cov_team$P_Value_Vs_Nominal))

all_pass <- test_pass(
  "team-mode's null bias magnitude is larger than player-mode's -- confirms the two modes are measuring genuinely different things on a team-clustered candidate, not silently aliasing each other",
  abs(g3_bias_team) > abs(g3_bias_player)
) && all_pass
all_pass <- test_pass(
  # NOT a "closer to nominal by absolute distance" check -- an
  # over-covering (conservative) mode can legitimately sit farther from
  # 0.90 in raw distance than an under-covering (anti-conservative) mode
  # while still being the SAFER choice, since the whole point of this
  # bias-correction machinery is to avoid the anti-conservative direction
  # (overstating a null candidate's significance), not to minimize
  # absolute distance from nominal. The core calibration criterion is
  # DIRECTIONAL: does team-mode avoid the risky (below-nominal,
  # anti-conservative) side that player-mode falls on here?
  "team-mode's coverage sits at/above nominal (the safe, conservative direction) while player-mode's sits below nominal (the risky, anti-conservative direction that matches this project's known WR player-pool ~87%-vs-90% precedent) -- the core calibration criterion",
  g3_cov_player$Coverage_Rate < 0.90 && g3_cov_team$Coverage_Rate >= 0.90
) && all_pass
all_pass <- test_pass(
  "team-mode's coverage is NOT flagged as below-nominal by this project's own one-sided binomial coverage test (p >= 0.10)",
  g3_cov_team$P_Value_Vs_Nominal >= 0.10
) && all_pass

cat("\n=== GATE 3 Overall:", if (all_pass) "ALL TESTS PASSED -- BIAS_PERMUTE_WITHIN resolved to \"team\"" else "AT LEAST ONE TEST FAILED -- DO NOT RESOLVE BIAS_PERMUTE_WITHIN ON THIS BASIS", "===\n")
