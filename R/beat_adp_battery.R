# ============================================================
# beat_adp_battery.R
# Shared, single-copy implementation of the three-test Beat-ADP
# battery (RMSE nested regression, bust/breakout AUC, pairwise
# concordance) plus their shared helper functions.
#
# WHY THIS FILE EXISTS: every analyze_*_beat_adp.R script previously
# carried its OWN copy-pasted version of these five functions. Defect
# #8 (logged in program_review_packet.md) was exactly this pattern:
# a MIN_GAMES-style filter existed in one script's copy and was
# accidentally omitted from another's, producing a stable, reproducible
# discrepancy between two scripts that should have agreed. Consolidating
# into one sourced file makes that specific class of bug structurally
# impossible going forward -- there is now only one copy to get right
# or wrong, not several copies that can silently drift apart.
#
# Every future analyze_*_beat_adp.R script should source this file and
# call these functions directly, rather than defining its own version.
# ============================================================
library(dplyr)

#' Prints a throttled progress line: percent complete, elapsed time,
#' and estimated time remaining -- for any long-running loop in this
#' project. Throttled to roughly every 10% (plus always the first and
#' last iteration) so long loops (100+ replicates) don't flood the
#' console, while short loops (a handful of candidates) print every
#' step since there's no flooding risk.
#'
#' @param i current iteration (1-indexed)
#' @param n total iterations
#' @param start_time the Sys.time() captured before the loop began
#' @param label a short label for what's running (e.g. "QB candidate", "bias replicate")
print_progress <- function(i, n, start_time, label = "") {
  throttle_every <- max(1, ceiling(n / 10))
  if (i != 1 && i != n && i %% throttle_every != 0) return(invisible(NULL))
  elapsed_secs <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  pct <- round(100 * i / n)
  avg_per_iter <- elapsed_secs / i
  eta_secs <- avg_per_iter * (n - i)
  fmt <- function(secs) {
    if (secs < 60) sprintf("%.0fs", secs)
    else if (secs < 3600) sprintf("%.1fmin", secs / 60)
    else sprintf("%.1fhr", secs / 3600)
  }
  message(sprintf("    [%3d%%] %s %d/%d -- elapsed: %s, ETA: %s remaining",
                   pct, label, i, n, fmt(elapsed_secs), fmt(eta_secs)))
}

#' Manual AUC via the Mann-Whitney U equivalence.
compute_auc <- function(scores, actual_binary) {
  n_pos <- sum(actual_binary == 1); n_neg <- sum(actual_binary == 0)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  r <- rank(scores)
  (sum(r[actual_binary == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

#' Paired Wilcoxon signed-rank p-value, defensively wrapped, with
#' diagnostics attached as attributes (does not change how the return
#' value behaves in arithmetic/comparisons -- existing callers that
#' just use the numeric p-value are unaffected).
#'
#' PAIRING UNIT, confirmed and documented explicitly per Fable 5's
#' program review (Concern 2): every caller of this function passes
#' SEASON-LEVEL paired values (one pair per held-out season -- fold-
#' level RMSE_A/RMSE_B, fold-level AUC_A/AUC_B, or season-level
#' aggregated concordance rates), NEVER player-level pairs. This means
#' n is the number of seasons tested (typically 10-14), not the number
#' of players -- the anti-conservative risk from non-independent
#' player-level pairs does NOT apply, but the small-n power limitation
#' is real (see the power-analysis harness planned in response to the
#' same review).
#'
#' DIAGNOSTICS (added per Fable 5's second-round review): the previous
#' version wrapped wilcox.test() in suppressWarnings(), which silently
#' hid the exact information needed to diagnose whether a battery of
#' many near-identical q=1.0 results reflects a genuinely uninformative
#' null or a test that is structurally incapable of producing a small
#' p-value at this n (e.g. falling back to the normal approximation due
#' to ties, when the exact signed-rank test could have resolved
#' further). attr(result, "n_effective"), attr(result, "n_ties"), and
#' attr(result, "used_exact") expose this instead of hiding it.
#'
#' EFFECTIVE-N FIX: the signed-rank test drops zero differences
#' internally before computing ranks -- the previous guard checked
#' length(d) < 4 on the RAW differences, meaning e.g. 5 pairs with 3
#' zeros would pass the guard while having an effective n of 2. The
#' guard now checks the POST-zero-removal count directly.
paired_wilcox_p <- function(x, y) {
  d <- x - y; d <- d[!is.na(d)]
  d_nonzero <- d[d != 0]
  if (length(d_nonzero) < 4) return(NA_real_)
  n_ties <- sum(duplicated(abs(d_nonzero)) | duplicated(abs(d_nonzero), fromLast = TRUE))
  can_use_exact <- n_ties == 0 && length(d_nonzero) < 50  # matches wilcox.test()'s own exact-test eligibility rule
  result <- tryCatch({
    w <- suppressWarnings(wilcox.test(x, y, paired = TRUE, exact = if (can_use_exact) TRUE else FALSE))
    p <- w$p.value
    attr(p, "n_effective") <- length(d_nonzero)
    attr(p, "n_zero_diffs") <- sum(d == 0)
    attr(p, "n_ties") <- n_ties
    attr(p, "used_exact") <- can_use_exact
    p
  }, error = function(e) NA_real_)
  result
}

#' Nested-regression RMSE test via leave-one-season-out cross-validation.
#' Model A: season_ppg ~ RANK + Prior_PPG. Model B: same + candidate.
#'
#' @param data player-level data with Season, RANK, Prior_PPG, season_ppg,
#'   and the named candidate column
#' @param candidate_col name of the candidate stat column to test
#' @param min_train minimum training-fold size to attempt a fit (default 15)
#' @param min_test minimum test-fold size to attempt a fit (default 5)
#' @param return_folds if TRUE, returns list(summary=<the usual named
#'   vector>, folds=<per-season RMSE_A/RMSE_B data.frame>) instead of
#'   just the summary vector. Default FALSE preserves the exact
#'   original return type/behavior for every existing caller (analyze
#'   scripts, test_beat_adp_battery.R) -- added specifically so
#'   bootstrap_rmse_effect() can resample the real fold-level results
#'   without re-running the CV loop from scratch.
run_rmse_cv <- function(data, candidate_col, min_train = 15, min_test = 5, return_folds = FALSE) {
  seasons_present <- sort(unique(data$Season))
  results <- lapply(seasons_present, function(held_out) {
    train <- data %>% filter(Season != held_out, !is.na(.data[[candidate_col]]))
    test  <- data %>% filter(Season == held_out, !is.na(.data[[candidate_col]]))
    if (nrow(train) < min_train || nrow(test) < min_test) return(NULL)
    fmla_b <- as.formula(paste("season_ppg ~ RANK + Prior_PPG +", candidate_col))
    model_a <- lm(season_ppg ~ RANK + Prior_PPG, data = train)
    model_b <- lm(fmla_b, data = train)
    rmse_a <- sqrt(mean((test$season_ppg - predict(model_a, test))^2, na.rm = TRUE))
    rmse_b <- sqrt(mean((test$season_ppg - predict(model_b, test))^2, na.rm = TRUE))
    data.frame(RMSE_A = rmse_a, RMSE_B = rmse_b)
  })
  results <- do.call(rbind, results[!sapply(results, is.null)])
  if (is.null(results) || nrow(results) < 4) {
    summary_vec <- c(RMSE_Improvement_Pct = NA_real_, RMSE_Folds_Won = NA_integer_, RMSE_P = NA_real_)
    return(if (return_folds) list(summary = summary_vec, folds = results) else summary_vec)
  }
  summary_vec <- c(RMSE_Improvement_Pct = round(100 * mean((results$RMSE_A - results$RMSE_B) / results$RMSE_A), 2),
    RMSE_Folds_Won = sum(results$RMSE_B < results$RMSE_A),
    RMSE_P = paired_wilcox_p(results$RMSE_A, results$RMSE_B))
  if (return_folds) list(summary = summary_vec, folds = results) else summary_vec
}

#' Bust/breakout classification AUC test via leave-one-season-out
#' cross-validation.
#'
#' @param target_col name of the binary outcome column (e.g. is_breakout, is_bust)
run_auc_cv <- function(data, candidate_col, target_col, min_train = 15, min_test = 5) {
  seasons_present <- sort(unique(data$Season))
  results <- lapply(seasons_present, function(held_out) {
    train <- data %>% filter(Season != held_out, !is.na(.data[[candidate_col]]))
    test  <- data %>% filter(Season == held_out, !is.na(.data[[candidate_col]]))
    if (nrow(train) < min_train || nrow(test) < min_test || length(unique(train[[target_col]])) < 2) return(NULL)
    fmla_b <- as.formula(paste(target_col, "~ RANK + Prior_PPG +", candidate_col))
    model_b <- glm(fmla_b, data = train, family = binomial)
    pred_b <- suppressWarnings(predict(model_b, newdata = test, type = "response"))
    model_a <- glm(as.formula(paste(target_col, "~ RANK + Prior_PPG")), data = train, family = binomial)
    pred_a <- suppressWarnings(predict(model_a, newdata = test, type = "response"))
    data.frame(AUC_A = compute_auc(pred_a, test[[target_col]]), AUC_B = compute_auc(pred_b, test[[target_col]]))
  })
  results <- do.call(rbind, results[!sapply(results, is.null)])
  if (is.null(results) || nrow(results) < 4) return(c(AUC_B = NA_real_, AUC_Folds_Won = NA_integer_, AUC_P = NA_real_))
  c(AUC_B = round(mean(results$AUC_B, na.rm = TRUE), 3),
    AUC_Folds_Won = sum(results$AUC_B > results$AUC_A, na.rm = TRUE),
    AUC_P = paired_wilcox_p(results$AUC_A, results$AUC_B))
}

#' Pairwise concordance across ADP-pick windows, LOSO-fitted.
#'
#' ESTIMAND FIX (confirmed real, project-wide defect via check_null_
#' concordance_centering.R): this function previously compared
#' model_b's (RANK+Prior_PPG+candidate) pairwise accuracy against RAW
#' ADP RANK ORDER ALONE, never against a model_a (RANK+Prior_PPG,
#' WITHOUT the candidate) fitted the same way. Since RANK+Prior_PPG
#' alone already beats raw ADP rank (that is the entire reason
#' Prior_PPG was added as a baseline term in the first place), EVERY
#' candidate -- including a genuinely random, permuted one -- inherited
#' credit for that pre-existing baseline advantage. Confirmed directly:
#' 200 independent null (permuted) candidates showed a mean "improvement"
#' of +0.014 (95% CI [0.010, 0.018], t=7.6, p=1.1e-12) -- decisively
#' non-zero for a stat with zero true information content. This means
#' EVERY concordance result this project has ever reported, across
#' every position and every phase, measured "does the whole model beat
#' blind draft order" rather than "does THIS candidate add anything
#' beyond RANK+Prior_PPG" -- a fundamentally different and much weaker
#' claim than what was being reported. Every prior concordance finding
#' in this project's record should be treated as unconfirmed until
#' re-run against the corrected estimand below.
#'
#' model_a is now fit in every fold, identically to model_b's fitting
#' procedure, and modela_correct (NOT adp_correct) is the comparison
#' baseline for the candidate's incremental value. adp_correct and
#' adp_rate are RETAINED in the output (renamed nowhere) since "does
#' the whole model beat ADP" remains a separate, legitimate question --
#' it just was never the question this function was supposed to be
#' answering about individual candidates, and both quantities are now
#' clearly distinguished in the return value.
#'
#' @param pair_rank_windows ADP-pick-distance windows to test (explicit
#'   parameter with a default, NOT a global variable this function
#'   silently depends on -- a prior version of this function across
#'   several scripts read PAIR_RANK_WINDOWS from the calling
#'   environment implicitly; making it an explicit parameter removes
#'   that implicit-dependency risk, the same root-cause class as
#'   defect #8)
#' @param significance_window which single window's season-level rates
#'   get the paired-Wilcoxon significance test (default 12, matching
#'   every prior script's convention)
#' @param return_pairs if TRUE, returns list(summary=<the usual named
#'   vector>, pairs=<the full player-pair-level all_pairs_df>) instead
#'   of just the summary vector. Default FALSE preserves the exact
#'   original return type/behavior for every existing caller -- added
#'   specifically so bootstrap_concordance_effect() can resample real
#'   pair-level results, clustered by season, without re-fitting the
#'   LOSO models from scratch.
run_concordance <- function(data, candidate_col, pair_rank_windows = c(6, 12, 18, 24),
                             significance_window = 12, min_train = 15, return_pairs = FALSE) {
  data <- data %>% filter(!is.na(.data[[candidate_col]]))
  seasons_present <- sort(unique(data$Season))
  if (nrow(data) < 15 || length(seasons_present) < 4) {
    out <- setNames(rep(NA_real_, length(pair_rank_windows)), paste0("Concordance_Diff_W", pair_rank_windows))
    summary_vec <- c(out, Concordance_Sig_P = NA_real_)
    return(if (return_pairs) list(summary = summary_vec, pairs = NULL) else summary_vec)
  }
  data$pred_a <- NA_real_
  data$pred_b <- NA_real_
  fmla_b <- as.formula(paste("season_ppg ~ RANK + Prior_PPG +", candidate_col))
  for (held_out in seasons_present) {
    train <- data %>% filter(Season != held_out)
    if (nrow(train) < min_train) next
    model_a <- lm(season_ppg ~ RANK + Prior_PPG, data = train)
    model_b <- lm(fmla_b, data = train)
    idx <- which(data$Season == held_out)
    data$pred_a[idx] <- predict(model_a, newdata = data[idx, ])
    data$pred_b[idx] <- predict(model_b, newdata = data[idx, ])
  }
  # VECTORIZED pairwise construction (replaces a per-pair lapply()+
  # rbind() loop -- a known-slow R pattern where each rbind copies the
  # whole growing table). This computes the EXACT SAME deterministic
  # comparisons via matrix operations instead of a loop -- no
  # randomness is involved in this step, so the output is mathematically
  # identical to the old loop, not an approximation. Verified directly
  # via test_beat_adp_battery.R's old-vs-new reconciliation test, not
  # just asserted.
  all_pairs <- lapply(seasons_present, function(yr) {
    sp <- data %>% filter(Season == yr)
    n <- nrow(sp)
    if (n < 2) return(NULL)
    RANK <- sp$RANK; ppg <- sp$season_ppg; pred_a <- sp$pred_a; pred_b <- sp$pred_b
    valid <- !is.na(pred_a) & !is.na(pred_b)
    upper_idx <- which(upper.tri(matrix(0, n, n)), arr.ind = TRUE)
    i <- upper_idx[, 1]; j <- upper_idx[, 2]
    keep <- valid[i] & valid[j]
    i <- i[keep]; j <- j[keep]
    if (length(i) == 0) return(NULL)
    data.frame(
      Season = yr,
      rank_diff = abs(RANK[i] - RANK[j]),
      adp_correct    = (RANK[i] < RANK[j])     == (ppg[i] > ppg[j]),
      modela_correct = (pred_a[i] > pred_a[j]) == (ppg[i] > ppg[j]),
      modelb_correct = (pred_b[i] > pred_b[j]) == (ppg[i] > ppg[j])
    )
  })
  all_pairs_df <- do.call(rbind, all_pairs[!sapply(all_pairs, is.null)])
  if (is.null(all_pairs_df)) {
    out <- setNames(rep(NA_real_, length(pair_rank_windows)), paste0("Concordance_Diff_W", pair_rank_windows))
    summary_vec <- c(out, Concordance_Sig_P = NA_real_)
    return(if (return_pairs) list(summary = summary_vec, pairs = NULL) else summary_vec)
  }
  # Concordance_Diff_W* is now (model_b - model_a): the candidate's OWN
  # incremental value, not (model_b - ADP). Concordance_Vs_ADP_W* is
  # kept as a SEPARATE, clearly-labeled quantity for the legitimate but
  # different question of whether the whole model beats blind ADP.
  out <- sapply(pair_rank_windows, function(w) {
    sub <- all_pairs_df %>% filter(rank_diff <= w)
    round(mean(sub$modelb_correct) - mean(sub$modela_correct), 4)
  })
  names(out) <- paste0("Concordance_Diff_W", pair_rank_windows)
  out_vs_adp <- sapply(pair_rank_windows, function(w) {
    sub <- all_pairs_df %>% filter(rank_diff <= w)
    round(mean(sub$modelb_correct) - mean(sub$adp_correct), 4)
  })
  names(out_vs_adp) <- paste0("Concordance_VsADP_W", pair_rank_windows)
  sig_pairs <- all_pairs_df %>% filter(rank_diff <= significance_window)
  season_rates <- sig_pairs %>% group_by(Season) %>%
    summarise(adp_rate = mean(adp_correct), modela_rate = mean(modela_correct),
              modelb_rate = mean(modelb_correct), .groups = "drop")
  p_val <- if (nrow(season_rates) >= 4) paired_wilcox_p(season_rates$modela_rate, season_rates$modelb_rate) else NA_real_
  summary_vec <- c(out, out_vs_adp, Concordance_Sig_P = p_val)
  if (return_pairs) list(summary = summary_vec, pairs = sig_pairs) else summary_vec
}

#' Runs all three tests for one candidate and returns a single named
#' vector row -- the standard per-candidate summary used by every
#' battery script. Centralizing this loop body too, not just the three
#' test functions individually, since the assembly logic itself was
#' also duplicated across scripts.
run_full_battery_for_candidate <- function(data, candidate_col, pair_rank_windows = c(6, 12, 18, 24)) {
  rmse_res <- run_rmse_cv(data, candidate_col)
  breakout_res <- run_auc_cv(data, candidate_col, "is_breakout")
  bust_res <- run_auc_cv(data, candidate_col, "is_bust")
  conc_res <- run_concordance(data, candidate_col, pair_rank_windows = pair_rank_windows)
  c(
    RMSE_Improvement_Pct = unname(rmse_res["RMSE_Improvement_Pct"]),
    RMSE_Folds_Won = unname(rmse_res["RMSE_Folds_Won"]),
    RMSE_P = unname(rmse_res["RMSE_P"]),
    Breakout_AUC_B = unname(breakout_res["AUC_B"]),
    Breakout_AUC_Folds_Won = unname(breakout_res["AUC_Folds_Won"]),
    Breakout_AUC_P = unname(breakout_res["AUC_P"]),
    Bust_AUC_B = unname(bust_res["AUC_B"]),
    Bust_AUC_Folds_Won = unname(bust_res["AUC_Folds_Won"]),
    Bust_AUC_P = unname(bust_res["AUC_P"]),
    conc_res
  )
}

# ============================================================
# EFFECT-ESTIMATION LAYER (bootstrap confidence intervals)
#
# Added in response to Fable 5's round-3 finding: the positive-control
# simulation showed this battery's p-value-based tests only reliably
# detect (>=80% of replicates) an effect size of ~20% of residual
# variance explained at n~12 seasons -- far larger than any real
# finding this project has produced. A hypothesis test at this n can
# mostly only say "not detectably different from zero," regardless of
# whether a smaller true effect exists. An interval instead reports
# WHERE the true effect plausibly lies, which is informative even when
# it straddles zero (it bounds how large an edge could exist), and is
# the more decision-useful answer for a real drafter (see the worked
# draft-scenario discussion in this project's conversation record).
#
# These bootstrap over the ALREADY-COMPUTED fold-level/pair-level
# results (via run_rmse_cv(..., return_folds=TRUE) and run_concordance
# (..., return_pairs=TRUE)) rather than refitting models on every
# resample -- the expensive LOSO-CV fitting happens once, for real; the
# bootstrap only resamples WHICH seasons contribute to the aggregate.
# Resampling is done at the SEASON level in both cases (with-
# replacement resampling of whole seasons, not individual player rows
# or pairs), preserving the same season-clustering discipline the
# original p-value tests were built around.
# ============================================================

#' Bootstrap confidence interval for the RMSE improvement effect size.
#'
#' @param fold_results the $folds data.frame from run_rmse_cv(..., return_folds=TRUE)
#' @param n_boot number of bootstrap replicates (default 2000)
#' @param conf_level confidence level (default 0.90, matching this
#'   project's established "reasonable bar" convention elsewhere)
bootstrap_rmse_effect <- function(fold_results, n_boot = 2000, conf_level = 0.90) {
  if (is.null(fold_results) || nrow(fold_results) < 4) {
    return(c(Point_Estimate = NA_real_, CI_Lower = NA_real_, CI_Upper = NA_real_, N_Seasons = NA_integer_))
  }
  n_seasons <- nrow(fold_results)
  boot_estimates <- replicate(n_boot, {
    resampled <- fold_results[sample(n_seasons, n_seasons, replace = TRUE), ]
    100 * mean((resampled$RMSE_A - resampled$RMSE_B) / resampled$RMSE_A)
  })
  point_estimate <- 100 * mean((fold_results$RMSE_A - fold_results$RMSE_B) / fold_results$RMSE_A)
  alpha <- 1 - conf_level
  ci <- quantile(boot_estimates, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
  c(Point_Estimate = round(point_estimate, 3), CI_Lower = round(unname(ci[1]), 3),
    CI_Upper = round(unname(ci[2]), 3), N_Seasons = n_seasons)
}

#' Bootstrap confidence interval for the concordance improvement effect
#' size, at a single ADP-pick window. Resamples SEASONS (with
#' replacement), pulling in every real player-pair belonging to each
#' drawn season -- preserves season-level clustering while using more
#' of the actual player-pair resolution than the original one-rate-per-
#' season design.
#'
#' ESTIMAND FIX: bootstraps (model_b - model_a), the candidate's own
#' incremental value, matching run_concordance()'s corrected summary
#' computation -- NOT (model_b - ADP), which every candidate trivially
#' wins via RANK+Prior_PPG's own real advantage over blind draft order
#' (confirmed via check_null_concordance_centering.R: 200 independent
#' null candidates showed a mean "improvement" of +0.014 against ADP,
#' decisively non-zero, p=1.1e-12). Using adp_correct here would have
#' silently reintroduced the exact same bug into the bootstrap CI layer
#' even after run_concordance()'s own summary was fixed.
#'
#' @param pairs_df the $pairs data.frame from run_concordance(..., return_pairs=TRUE)
bootstrap_concordance_effect <- function(pairs_df, n_boot = 2000, conf_level = 0.90) {
  if (is.null(pairs_df) || nrow(pairs_df) < 15) {
    return(c(Point_Estimate = NA_real_, CI_Lower = NA_real_, CI_Upper = NA_real_, N_Seasons = NA_integer_))
  }
  seasons <- unique(pairs_df$Season)
  n_seasons <- length(seasons)
  boot_estimates <- replicate(n_boot, {
    drawn_seasons <- sample(seasons, n_seasons, replace = TRUE)
    resampled_pairs <- do.call(rbind, lapply(drawn_seasons, function(s) pairs_df[pairs_df$Season == s, ]))
    mean(resampled_pairs$modelb_correct) - mean(resampled_pairs$modela_correct)
  })
  point_estimate <- mean(pairs_df$modelb_correct) - mean(pairs_df$modela_correct)
  alpha <- 1 - conf_level
  ci <- quantile(boot_estimates, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
  c(Point_Estimate = round(point_estimate, 4), CI_Lower = round(unname(ci[1]), 4),
    CI_Upper = round(unname(ci[2]), 4), N_Seasons = n_seasons)
}

#' Convenience wrapper: runs the LOSO-CV RMSE test once, then
#' bootstraps the resulting fold-level results directly -- no
#' re-fitting.
run_rmse_bootstrap <- function(data, candidate_col, n_boot = 2000, conf_level = 0.90, min_train = 15, min_test = 5) {
  cv_result <- run_rmse_cv(data, candidate_col, min_train = min_train, min_test = min_test, return_folds = TRUE)
  bootstrap_rmse_effect(cv_result$folds, n_boot = n_boot, conf_level = conf_level)
}

#' Convenience wrapper: runs the LOSO-CV concordance test once (at the
#' given significance_window, which becomes the window the interval is
#' reported for), then bootstraps the resulting pair-level results
#' directly -- no re-fitting.
run_concordance_bootstrap <- function(data, candidate_col, significance_window = 12, n_boot = 2000,
                                       conf_level = 0.90, min_train = 15) {
  cv_result <- run_concordance(data, candidate_col, significance_window = significance_window,
                                min_train = min_train, return_pairs = TRUE)
  bootstrap_concordance_effect(cv_result$pairs, n_boot = n_boot, conf_level = conf_level)
}

#' Estimation-based battery: reports point estimate + confidence
#' interval for RMSE and concordance (the two metrics where FDR-
#' corrected significance testing was shown to be badly underpowered
#' at this project's real sample sizes), and keeps the AUC
#' bust/breakout tests as p-value-based classification (a genuinely
#' binary question -- did this player break out or not -- where a
#' hypothesis test remains the right tool, unlike RMSE/concordance
#' which are asking a magnitude question).
#'
#' This does NOT replace run_full_battery_for_candidate() -- both are
#' kept. This is a separate, complementary summary for the same
#' candidate, not a corrected version of the p-value one.
#'
#' @param skip_rmse if TRUE, does not compute the RMSE bootstrap at
#'   all (returns NA for all RMSE fields). RMSE's bootstrap CIs were
#'   confirmed SEVERELY MISCALIBRATED (coverage 0.52-0.66 against a
#'   nominal 0.90, both positions -- validate_bootstrap_coverage.R) and
#'   are already excluded from all trusted reporting -- computing them
#'   anyway is pure wasted runtime, roughly half the per-candidate cost
#'   of this function, for a number nobody is allowed to cite. Default
#'   FALSE preserves old behavior for any caller that still wants the
#'   (quarantined, for-the-record-only) RMSE numbers.
run_full_battery_estimation <- function(data, candidate_col, significance_window = 12,
                                          n_boot = 2000, conf_level = 0.90, skip_rmse = FALSE) {
  rmse_ci <- if (skip_rmse) {
    c(Point_Estimate = NA_real_, CI_Lower = NA_real_, CI_Upper = NA_real_, N_Seasons = NA_integer_)
  } else {
    run_rmse_bootstrap(data, candidate_col, n_boot = n_boot, conf_level = conf_level)
  }
  conc_ci <- run_concordance_bootstrap(data, candidate_col, significance_window = significance_window,
                                        n_boot = n_boot, conf_level = conf_level)
  breakout_res <- run_auc_cv(data, candidate_col, "is_breakout")
  bust_res <- run_auc_cv(data, candidate_col, "is_bust")
  c(
    RMSE_Point_Estimate = unname(rmse_ci["Point_Estimate"]),
    RMSE_CI_Lower = unname(rmse_ci["CI_Lower"]),
    RMSE_CI_Upper = unname(rmse_ci["CI_Upper"]),
    Concordance_Point_Estimate = unname(conc_ci["Point_Estimate"]),
    Concordance_CI_Lower = unname(conc_ci["CI_Lower"]),
    Concordance_CI_Upper = unname(conc_ci["CI_Upper"]),
    N_Seasons = unname(conc_ci["N_Seasons"]),
    Breakout_AUC_B = unname(breakout_res["AUC_B"]),
    Breakout_AUC_P = unname(breakout_res["AUC_P"]),
    Bust_AUC_B = unname(bust_res["AUC_B"]),
    Bust_AUC_P = unname(bust_res["AUC_P"])
  )
}

# ============================================================
# BIAS-CORRECTION LAYER
#
# Added after diagnose_null_bias_mechanism.R confirmed the concordance
# estimand carries a real, position-specific, non-zero bias even after
# the estimand fix -- QB +0.0156, RB -0.0096, neither explainable by
# season-trend leakage through the permutation null (season-demeaning
# barely moved either number). Critically, diagnostic (ii) confirmed
# this is a LOCATION problem, not a WIDTH problem: shifting each null
# replicate's CI by its own population mean restored ~90% coverage for
# both positions (QB 0.87, RB 0.91). This means the bootstrap's actual
# precision was never broken -- every candidate's raw estimate just
# sits a known, measurable, constant distance off-center, and
# correcting for it is a reporting subtraction, not new machinery.
#
# The bias is estimated ONCE per position from a representative real
# candidate (NOT re-measured per candidate) -- this mirrors exactly
# how it was validated. Note this is a documented simplification, not
# a certainty: Fable 5 flagged that NGS-derived candidates (shorter,
# 7-9 season history) may have their own, different-behaved null and
# should get a candidate-specific bias check before being fully
# trusted under this correction -- that check is a planned follow-up,
# not yet built.
# ============================================================

#' Builds player-level rows for one position -- the SINGLE, shared
#' implementation of a pattern that had been re-implemented separately
#' in at least two scripts (check_bias_estimate_stability.R and
#' analyze_estimation_battery.R), with a confirmed real divergence
#' between them: one filtered rows to non-NA in the representative
#' candidate at the end, one did not. That divergence caused a real,
#' hard-to-diagnose bias-estimation bug (the null was measured on a
#' randomly-varying pool instead of the fixed, systematic pool the real
#' candidate estimates actually use). Consolidating here closes this
#' specific recurrence channel for every future position (WR/TE) and
#' every future script -- per Fable 5's diagnosis, the round-2 library
#' consolidation was correct but drawn too narrowly around the TEST
#' functions, leaving data assembly duplicated per script as an open
#' channel for exactly this kind of silent divergence.
#'
#' @param pos position label to filter the ADP pool to (e.g. "QB")
#' @param adp the loaded historic ADP data.frame
#' @param all_seasons_ppg full season_ppg table across all needed years
#' @param prior_ppg_lookup precomputed Prior_PPG lookup (Season = season+1)
#' @param extra_data position-specific merged data table (already
#'   joined with whatever fetch functions this position needs) to pull
#'   decay-weighted candidate values from
#' @param candidates character vector of candidate column names present
#'   in extra_data -- pass a single column name for a single-candidate
#'   use case (e.g. a bias-estimation diagnostic); this replaces both
#'   the single-candidate and multi-candidate versions that previously
#'   existed as separate, divergence-prone copies
#' @param seasons_to_test target seasons to build rows for
#' @param decay_r, lookback_years decay-weighting parameters
build_position_data <- function(pos, adp, all_seasons_ppg, prior_ppg_lookup, extra_data, candidates,
                                 seasons_to_test, decay_r, lookback_years) {
  rows_by_season <- list()
  for (yr in seasons_to_test) {
    end_year <- yr - 1
    this_season_ppg <- all_seasons_ppg %>% filter(season == yr)
    pool <- top150_pool(adp, yr) %>% filter(Pos == pos)
    pool <- correct_adp_team(pool, this_season_ppg %>% select(norm_name, position, primary_team))
    actual <- this_season_ppg %>% filter(position == pos) %>% select(norm_name, season_ppg)
    prior  <- prior_ppg_lookup %>% filter(Season == yr, position == pos) %>% select(norm_name, Prior_PPG)
    merged <- pool %>% inner_join(actual, by = "norm_name") %>% left_join(prior, by = "norm_name") %>%
      filter(!is.na(Prior_PPG))
    if (nrow(merged) < 5) next
    candidate_vals <- as.data.frame(setNames(
      lapply(candidates, function(col) {
        decay_weighted_avg_vec(extra_data, "norm_name", col, merged$norm_name, end_year,
                                year_col = "Season", r = decay_r, lookback = lookback_years)
      }), candidates
    ))
    candidate_vals$norm_name <- merged$norm_name
    merged <- merged %>% left_join(candidate_vals, by = "norm_name")
    merged$Season <- yr
    rows_by_season[[as.character(yr)]] <- merged
  }
  do.call(rbind, rows_by_season) %>%
    group_by(Season) %>%
    mutate(
      expected_ppg = predict(lm(season_ppg ~ RANK + Prior_PPG)),
      residual = season_ppg - expected_ppg,
      is_breakout = as.integer(residual >= quantile(residual, 0.75)),
      is_bust     = as.integer(residual <= quantile(residual, 0.25))
    ) %>%
    ungroup()
  # NOTE: deliberately NOT filtering !is.na() on any specific candidate
  # here -- this table may carry several candidates with different
  # missingness patterns (e.g. NGS candidates at 7-9 seasons vs. 12 for
  # standard stats). Each consumer (a battery test, a bias estimator)
  # filters to its OWN candidate's non-NA rows at the point of use --
  # this is exactly what closes the defect from this incident, since
  # estimate_concordance_null_bias() now does this filtering itself
  # rather than depending on the caller to have done it upstream.
}
#'
#' Estimates a position's concordance null bias by permuting a real,
#' representative candidate within season many times and averaging the
#' resulting point estimates. This is the SAME procedure used to
#' discover and validate the bias in diagnose_null_bias_mechanism.R --
#' calling this function reproduces that measurement, not a new method.
#'
#' MISSINGNESS-MATCHING FIX (confirmed real defect, found via Fable 5's
#' direct code review): this function now filters to
#' !is.na(representative_candidate_col) BEFORE permuting, matching
#' EXACTLY how run_concordance() filters the real candidate before
#' testing it. Without this, if a caller passes data still containing
#' NA rows for the representative candidate, permutation scatters those
#' NAs onto random players every replicate -- meaning the null gets
#' measured on a randomly-varying pool, while the REAL candidate
#' estimate (via run_concordance's own !is.na() filter) is always
#' measured on the fixed, systematic pool of players who actually have
#' that stat. Since the bias is a property of POOL STRUCTURE (margin
#' compression among close-ADP pairs, who's in the training folds), a
#' null measured on a randomly-varying pool answers a different
#' question than the real estimate does, and there's no reason its
#' answer should match. Confirmed real-world consequence: measured at
#' +0.00346 (badly biased-low, on a random-composition pool) versus the
#' validated +0.0106 (correct, on the real, fixed-composition QB
#' rushing-history pool) -- a difference (~0.007) comparable in size to
#' the actual effects this correction exists to isolate. Filtering here,
#' in the shared function, closes this for every future caller
#' (including WR/TE) rather than requiring each script to remember its
#' own filter -- the exact repeat-defect pattern this project keeps
#' re-discovering when a fix is applied per-script instead of at the
#' shared-library boundary.
#'
#' @param data player-level data for one position
#' @param representative_candidate_col a real candidate column to
#'   permute (does not need to be a "good" stat -- permutation destroys
#'   its real relationship regardless; this checks the MACHINERY's
#'   bias, not this candidate's properties)
#' @param n_replicates number of independent permutations (100 default,
#'   matching the originally validated diagnostic)
#' @param n_boot bootstrap draws per replicate (500 default)
estimate_concordance_null_bias <- function(data, representative_candidate_col, n_replicates = 100,
                                            significance_window = 12, n_boot = 500, conf_level = 0.90) {
  data <- data %>% filter(!is.na(.data[[representative_candidate_col]]))
  point_ests <- numeric(n_replicates)
  start_time <- Sys.time()
  for (i in seq_len(n_replicates)) {
    data_perm <- data %>% group_by(Season) %>%
      mutate(permuted = sample(.data[[representative_candidate_col]])) %>% ungroup()
    ci <- run_concordance_bootstrap(data_perm, "permuted", significance_window = significance_window,
                                     n_boot = n_boot, conf_level = conf_level)
    point_ests[i] <- ci["Point_Estimate"]
    print_progress(i, n_replicates, start_time, "bias replicate")
  }
  valid <- point_ests[!is.na(point_ests)]
  c(Null_Bias = mean(valid), Null_Bias_SE = sd(valid) / sqrt(length(valid)), N_Replicates = length(valid))
}

#' Robust version of estimate_concordance_null_bias(): a single 100-
#' replicate call was confirmed, via check_bias_estimate_stability.R,
#' to occasionally land ~3 SDs from the true value purely by chance --
#' the underlying procedure's OWN reported SE was accurate (matched the
#' real across-run spread almost exactly), but that doesn't stop any
#' one run from being an unlucky draw. This wraps
#' estimate_concordance_null_bias() in n_meta_replicates independent
#' calls and averages them, the same way the stability check itself
#' resolved the QB +0.0156-vs-+0.0020 discrepancy (the average of 10
#' independent runs, +0.0106, is far more trustworthy than either
#' single measurement). This should be the STANDARD way any future
#' script estimates a bias -- not a one-off diagnostic tool.
#'
#' @param n_meta_replicates independent full bias estimates to average
#'   (5 default -- a real runtime tradeoff; 10 was used for the
#'   original stability confirmation, 5 is a documented compromise for
#'   routine use, not a re-derivation of that validated number)
estimate_concordance_null_bias_robust <- function(data, representative_candidate_col, n_meta_replicates = 5,
                                                   n_replicates = 100, significance_window = 12,
                                                   n_boot = 500, conf_level = 0.90) {
  meta_estimates <- numeric(n_meta_replicates)
  start_time <- Sys.time()
  for (i in seq_len(n_meta_replicates)) {
    message("  meta-replicate ", i, " of ", n_meta_replicates, " (each running ", n_replicates, " inner replicates)...")
    res <- estimate_concordance_null_bias(data, representative_candidate_col, n_replicates = n_replicates,
                                           significance_window = significance_window, n_boot = n_boot,
                                           conf_level = conf_level)
    meta_estimates[i] <- res["Null_Bias"]
    print_progress(i, n_meta_replicates, start_time, "meta-replicate")
  }
  c(Null_Bias = mean(meta_estimates), Null_Bias_SE = sd(meta_estimates) / sqrt(n_meta_replicates),
    N_Meta_Replicates = n_meta_replicates, Meta_SD = sd(meta_estimates))
}

#' Cached wrapper around estimate_concordance_null_bias_robust().
#'
#' WHY CACHING IS SAFE HERE: the bias is a property of the METHOD
#' (how the model-a-vs-model-b comparison behaves at a given sample
#' size and pool shape), not of which exact players happened to be
#' good in a specific day's data pull -- unlike a real candidate's own
#' effect, which should genuinely be re-measured whenever fresh data
#' is available. Re-running the full 5x100-replicate procedure on
#' every single battery invocation is expensive and, once the
#' underlying setup is unchanged, mostly redundant.
#'
#' SAFETY: the cache is keyed on every setting that could plausibly
#' change what the bias actually is (position, representative
#' candidate, season range, games floor, decay parameters, and the
#' estimation settings themselves). If ANY of these differ from what's
#' on record, the cache is treated as a miss and a fresh estimate is
#' computed and saved -- a stale bias is never silently reused for a
#' setup it wasn't measured under.
#'
#' @param cache_path CSV file to read/write cached estimates (created if missing)
#' @param position a label for the cache key (e.g. "QB", "RB")
#' @param force_recompute if TRUE, ignores any existing cache entry and
#'   recomputes + overwrites it -- use this periodically (e.g. once a
#'   season, or if nflverse data is suspected to have meaningfully
#'   revised) even when the config hasn't changed, since this is a
#'   measurement of the real world's data, not a pure constant.
get_or_compute_concordance_null_bias <- function(data, representative_candidate_col, position,
                                                  seasons_to_test, min_games, decay_r, lookback_years,
                                                  cache_path = "output/concordance_null_bias_cache.csv",
                                                  n_meta_replicates = 5, n_replicates = 100,
                                                  significance_window = 12, n_boot = 500, conf_level = 0.90,
                                                  force_recompute = FALSE) {
  # DATA FINGERPRINT (added after a confirmed real incident): the cache
  # key previously included only CONFIG parameters, not anything about
  # the data itself. Two structurally different build_position_data()
  # implementations across two scripts produced different pools (one
  # correctly filtered to non-NA rows, one not) under IDENTICAL config
  # parameters -- meaning a bias measured on one build could be, and
  # was, silently served to the other. nrow(data) and the non-NA count
  # of the representative candidate column are cheap, sufficient
  # fingerprints to catch this specific failure class: if either
  # differs from what's on record, this is treated as a cache miss.
  n_rows <- nrow(data)
  n_nonNA <- sum(!is.na(data[[representative_candidate_col]]))

  cache_key <- paste(position, representative_candidate_col,
                      min(seasons_to_test), max(seasons_to_test), min_games, decay_r, lookback_years,
                      n_meta_replicates, n_replicates, significance_window, n_boot, conf_level,
                      n_rows, n_nonNA, sep = "|")

  cache <- if (file.exists(cache_path)) {
    tryCatch(readr::read_csv(cache_path, show_col_types = FALSE), error = function(e) NULL)
  } else NULL

  if (!force_recompute && !is.null(cache) && cache_key %in% cache$cache_key) {
    row <- cache[cache$cache_key == cache_key, ][1, ]
    message("  Using CACHED ", position, " null bias (computed ", row$computed_date, "): ",
            round(row$null_bias, 5), " -- pass force_recompute=TRUE to override.")
    return(c(Null_Bias = row$null_bias, Null_Bias_SE = row$null_bias_se,
             N_Meta_Replicates = row$n_meta_replicates, Meta_SD = row$meta_sd))
  }

  message("  No matching cache entry (or force_recompute=TRUE) -- computing fresh ", position, " null bias",
          " (data fingerprint: ", n_rows, " rows, ", n_nonNA, " non-NA)...")
  fresh <- estimate_concordance_null_bias_robust(data, representative_candidate_col, n_meta_replicates,
                                                  n_replicates, significance_window, n_boot, conf_level)
  new_row <- data.frame(cache_key = cache_key, position = position,
                         representative_candidate = representative_candidate_col,
                         null_bias = unname(fresh["Null_Bias"]), null_bias_se = unname(fresh["Null_Bias_SE"]),
                         n_meta_replicates = unname(fresh["N_Meta_Replicates"]), meta_sd = unname(fresh["Meta_SD"]),
                         n_rows = n_rows, n_nonNA = n_nonNA, computed_date = as.character(Sys.Date()))
  cache_updated <- if (is.null(cache)) new_row else rbind(cache[cache$cache_key != cache_key, ], new_row)
  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(cache_updated, cache_path)
  fresh
}

#' Battery-running loop for one position -- the SINGLE, shared
#' implementation used by ALL FOUR positions (QB, RB, WR, TE), rather
#' than a per-script copy. Moved here from analyze_estimation_battery.R
#' specifically so it's covered by test_beat_adp_battery.R -- it had
#' grown real branching logic (which candidate gets which bias, which
#' window applies to which stat) while living only in an analysis
#' script with zero automated test coverage, exactly the kind of
#' complex-but-untested orchestration code this project has been
#' burned by before.
#'
#' Converted to EXPLICIT parameters rather than relying on ~9 implicit
#' global variables from the calling script (SEASONS_TO_TEST,
#' MIN_GAMES, etc.) -- matching how every other function in this
#' library already works, and making this function properly testable
#' in isolation rather than requiring a caller to first set up a pile
#' of globals correctly.
#'
#' @param secondary_candidates optional character vector of candidates
#'   needing a SEPARATE null bias (e.g. shorter-history NGS-derived
#'   stats) -- NULL (default) means every candidate uses the single
#'   representative_candidate_col bias.
#' @param secondary_representative_candidate the representative column
#'   for measuring the secondary bias -- required if secondary_candidates is set.
#' @param sig_window_fn optional function(stat) -> window, for positions
#'   needing different significance windows per candidate -- NULL
#'   (default) uses the single significance_window for every candidate.
#' @param run_coverage_spotcheck if TRUE, runs a quick coverage check
#'   before the main loop -- for positions with an unusually large pool
#'   where validated calibration from other positions should not be
#'   assumed to transfer automatically.
run_estimation_for_position <- function(pos, data, candidates, representative_candidate_col,
                                         seasons_to_test, min_games, decay_r, lookback_years,
                                         significance_window = 12, n_boot = 500, conf_level = 0.90,
                                         skip_rmse = TRUE, force_recompute_bias = FALSE,
                                         secondary_candidates = NULL, secondary_representative_candidate = NULL,
                                         sig_window_fn = NULL, run_coverage_spotcheck = FALSE,
                                         bias_n_meta_replicates = 5, bias_n_replicates = 100, bias_n_boot = 500,
                                         cache_path = "output/concordance_null_bias_cache.csv") {
  # bias_n_meta_replicates/bias_n_replicates/bias_n_boot control the
  # cost of the BIAS MEASUREMENT step specifically -- previously
  # hardcoded to production-scale values (5 meta x 100 inner x 500
  # boot = 500,000+ total bootstrap operations) regardless of what a
  # caller wanted, meaning a routing-logic test that only cares whether
  # the RIGHT bias gets applied to the RIGHT candidate had no way to
  # ask for a cheap, fast version -- it silently paid the full
  # production cost every time. Defaults preserve exact prior behavior
  # for the real battery; a test can now pass small values instead.
  #
  # cache_path added 2026-07-12: previously hardcoded (via
  # get_or_compute_concordance_null_bias()'s own default) to the shared
  # production cache with no way for a caller of THIS function to
  # override it -- meaning every test exercising run_estimation_for_
  # position() wrote synthetic TESTPOS-style rows straight into
  # output/concordance_null_bias_cache.csv. Confirmed real: the
  # production cache uploaded from the actual 2026-07-12 run already
  # contained TESTPOS/TESTPOS_secondary entries before any of today's
  # changes. Default preserves exact prior behavior for real battery
  # runs; tests now pass an isolated temp path instead.
  message("  Getting ", pos, "'s concordance null bias (cached if config unchanged, representative candidate: ",
          representative_candidate_col, ")...")
  bias_est <- get_or_compute_concordance_null_bias(
    data, representative_candidate_col, position = pos,
    seasons_to_test = seasons_to_test, min_games = min_games, decay_r = decay_r, lookback_years = lookback_years,
    n_meta_replicates = bias_n_meta_replicates, n_replicates = bias_n_replicates, significance_window = significance_window,
    n_boot = bias_n_boot, conf_level = conf_level, force_recompute = force_recompute_bias, cache_path = cache_path
  )
  null_bias <- unname(bias_est["Null_Bias"])
  cat("  ", pos, " null bias =", round(null_bias, 5), "(SE =", round(bias_est["Null_Bias_SE"], 5),
      ", from", bias_est["N_Meta_Replicates"], "meta-replicates, meta-SD =", round(bias_est["Meta_SD"], 5), ")\n")

  secondary_bias <- NULL
  if (!is.null(secondary_candidates)) {
    message("  Getting ", pos, "'s SECONDARY concordance null bias (shorter-history candidates, representative: ",
            secondary_representative_candidate, ")...")
    secondary_bias_est <- get_or_compute_concordance_null_bias(
      data, secondary_representative_candidate, position = paste0(pos, "_secondary"),
      seasons_to_test = seasons_to_test, min_games = min_games, decay_r = decay_r, lookback_years = lookback_years,
      n_meta_replicates = bias_n_meta_replicates, n_replicates = bias_n_replicates, significance_window = significance_window,
      n_boot = bias_n_boot, conf_level = conf_level, force_recompute = force_recompute_bias, cache_path = cache_path
    )
    secondary_bias <- unname(secondary_bias_est["Null_Bias"])
    cat("  ", pos, " secondary bias =", round(secondary_bias, 5), "\n")
  }

  if (run_coverage_spotcheck) {
    message("  Running ", pos, " coverage spot-check (large-pool positions shouldn't assume validated\n",
            "  calibration from other positions transfers automatically)...")
    n_cov <- 50
    covered <- logical(n_cov)
    start_cov <- Sys.time()
    for (i in seq_len(n_cov)) {
      perm <- data %>% group_by(Season) %>% mutate(permuted = sample(.data[[representative_candidate_col]])) %>% ungroup()
      ci <- run_concordance_bootstrap(perm, "permuted", significance_window = significance_window, n_boot = 300, conf_level = conf_level)
      covered[i] <- !is.na(ci["CI_Lower"]) && !is.na(ci["CI_Upper"]) &&
        (ci["CI_Lower"] - null_bias) <= 0 && (ci["CI_Upper"] - null_bias) >= 0
      print_progress(i, n_cov, start_cov, paste0(pos, " coverage replicate"))
    }
    n_covered <- sum(covered, na.rm = TRUE)
    cov_rate <- n_covered / n_cov
    # Exact (Clopper-Pearson) CI and a one-sided exact binomial test
    # against the nominal conf_level, in place of both (a) a bare point
    # estimate with no uncertainty reported (section 1.7: "report
    # distributions, not adjectives") and (b) the previous fixed
    # cov_rate < conf_level - 0.15 rule, which required cov_rate < 0.75 to
    # fire at all and said nothing about the actual observed WR result
    # (40/50 = 0.80). A normal-approximation CI was tried first and its
    # two-sided 95% upper bound (0.911) still would have missed this case
    # -- exact/one-sided is the correct tool at n=50, not a cosmetic
    # change. p < 0.10 (not 0.05) is deliberately less stringent than a
    # confirmatory threshold: this is a spot-check meant to flag "go run
    # the real coverage harness," not a standalone confirmatory test.
    bt <- binom.test(n_covered, n_cov, p = conf_level, alternative = "less")
    cat("  ", pos, " coverage spot-check:", round(cov_rate, 3),
        " (", n_covered, "/", n_cov, ", exact 95% CI [",
        round(bt$conf.int[1], 3), ",", round(bt$conf.int[2], 3), "]",
        ", one-sided p vs nominal ", conf_level, " = ", signif(bt$p.value, 3),
        ")\n", sep = "")
    if (bt$p.value < 0.10) {
      cat("  WARNING:", pos, "coverage spot-check (", n_covered, "/", n_cov,
          "=", round(cov_rate, 3), ") is significantly below the nominal",
          conf_level, "target (one-sided p =", signif(bt$p.value, 3), ") --",
          "treat", pos, "intervals with extra caution and do not treat this",
          "as resolved without a full validate_bootstrap_coverage.R re-run.\n")
    }
  }

  coverage_spotcheck_result <- NULL
  if (run_coverage_spotcheck) {
    coverage_spotcheck_result <- list(
      Position = pos, N_Covered = n_covered, N_Trials = n_cov,
      Coverage_Rate = cov_rate, CI_Lower = unname(bt$conf.int[1]),
      CI_Upper = unname(bt$conf.int[2]), P_Value_Vs_Nominal = bt$p.value,
      Nominal_Conf_Level = conf_level, Flagged = bt$p.value < 0.10
    )
  }

  rows <- list()
  n_candidates <- length(candidates)
  start_time <- Sys.time()
  for (idx in seq_along(candidates)) {
    stat <- candidates[idx]
    message("  ", pos, " x ", stat, "...")
    sig_window <- if (is.null(sig_window_fn)) significance_window else sig_window_fn(stat)
    res <- run_full_battery_estimation(data, stat, significance_window = sig_window,
                                        n_boot = n_boot, conf_level = conf_level, skip_rmse = skip_rmse)
    bias_to_use <- if (!is.null(secondary_candidates) && stat %in% secondary_candidates) secondary_bias else null_bias
    res <- apply_concordance_bias_correction(res, bias_to_use)
    rows[[stat]] <- c(Position = pos, Stat = stat, Significance_Window = sig_window, res)
    print_progress(idx, n_candidates, start_time, paste0(pos, " candidate"))
  }
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  rownames(df) <- NULL
  numeric_cols <- setdiff(names(df), c("Position", "Stat"))
  df[numeric_cols] <- lapply(df[numeric_cols], function(x) as.numeric(as.character(x)))
  attr(df, "null_bias") <- null_bias
  attr(df, "coverage_spotcheck") <- coverage_spotcheck_result
  df
}

#' Applies a measured null-bias correction to a run_full_battery_
#' estimation() result's concordance fields, adding BIAS-CORRECTED
#' columns alongside the original RAW ones -- never replacing the raw
#' numbers, so the correction is always visible and auditable rather
#' than silently substituted.
#'
#' @param battery_result the named vector from run_full_battery_estimation()
#' @param null_bias the position's measured bias (from estimate_concordance_null_bias())
apply_concordance_bias_correction <- function(battery_result, null_bias) {
  c(battery_result,
    Concordance_Point_Estimate_BiasCorrected = unname(battery_result["Concordance_Point_Estimate"]) - null_bias,
    Concordance_CI_Lower_BiasCorrected = unname(battery_result["Concordance_CI_Lower"]) - null_bias,
    Concordance_CI_Upper_BiasCorrected = unname(battery_result["Concordance_CI_Upper"]) - null_bias)
}

#' Groups a position's candidate stats into correlated "families" based
#' on their raw pairwise correlation in the player-season data -- fixed
#' 2026-07-12. This is the family-correlation matrix required by
#' Project_Context.txt section 2.7 item 1 and WR_TE_PREREGISTRATION.md
#' item 4 ("Build the family-correlation matrix for WR/TE from the
#' start (not retrofitted)") before WR/TE findings could be reported --
#' it was never implemented, and the real run's reporting listed e.g.
#' QB's five rushing-volume candidates as five separate findings with no
#' indication they were correlated measurements of essentially one
#' signal (per section 1.7: "Results are reported by candidate family
#' ... not as raw counts of individually-significant candidates").
#'
#' ESTIMAND: this groups candidates by correlation of their RAW STAT
#' VALUES in the underlying player-season data (e.g. Rush_Yards_PG vs
#' Rush_Yards), NOT by correlation of their concordance effect sizes or
#' p-values across seasons -- the latter would require refitting every
#' candidate across many resampled datasets to get a sampling
#' distribution to correlate, which this project's current
#' infrastructure does not produce. This is a real, deliberate scope
#' limit: two candidates could plausibly be family-linked by shared
#' mechanism without their raw values correlating strongly (e.g. if one
#' is a rate and the other a red-zone-specific subset), and this
#' function will not catch that. It groups by "these are numerically
#' redundant measurements," not by "these share a hypothesized cause."
#'
#' NULL CANDIDATE BEHAVIOR: a candidate uncorrelated with everything
#' else forms its own singleton family (family size 1) -- this is the
#' expected, correct output for an independent candidate, not a failure.
#'
#' @param data the position's player-season data frame (e.g. qb_data)
#' @param candidates character vector of candidate column names present in data
#' @param method correlation method passed to stats::cor (default "spearman",
#'   robust to the mix of count-like and rate-like stats in these candidate
#'   lists without requiring a distributional assumption)
#' @param threshold |r| at or above which two candidates are placed in the
#'   same family (default 0.7 -- a conventional "strong correlation" cutoff;
#'   this is a reporting-grouping choice, not a statistical test, and is
#'   exposed as a parameter rather than hardcoded so it can be revisited)
#' @return a list with:
#'   - correlation_matrix: candidates x candidates correlation matrix
#'     (NA for any pair involving a constant column -- cor() cannot define
#'     a correlation with zero variance, and this is surfaced as NA rather
#'     than erroring or silently coercing to 0)
#'   - family_id: named integer vector, one family id per candidate,
#'     assigned via connected components on the |r| >= threshold graph
#'     (transitive: if A-B and B-C both clear threshold, A/B/C are one
#'     family even if A-C alone would not have cleared it -- documented
#'     here since it is easy to assume otherwise)
compute_candidate_family_matrix <- function(data, candidates, method = "spearman", threshold = 0.7) {
  missing_cols <- setdiff(candidates, names(data))
  if (length(missing_cols) > 0) {
    stop("compute_candidate_family_matrix: candidate column(s) not found in data: ",
         paste(missing_cols, collapse = ", "))
  }
  mat <- as.matrix(data[, candidates, drop = FALSE])
  storage.mode(mat) <- "double"
  corr <- suppressWarnings(cor(mat, method = method, use = "pairwise.complete.obs"))
  # cor() returns NA for any column with zero variance, rather than
  # erroring -- but it does NOT reliably return NA for a fully-missing
  # (all-NA) column in every R version, so make that case explicit too.
  all_na_cols <- candidates[sapply(seq_along(candidates), function(i) all(is.na(mat[, i])))]
  if (length(all_na_cols) > 0) {
    corr[all_na_cols, ] <- NA
    corr[, all_na_cols] <- NA
  }

  n <- length(candidates)
  adjacency <- matrix(FALSE, n, n, dimnames = list(candidates, candidates))
  adjacency[!is.na(corr) & abs(corr) >= threshold] <- TRUE
  diag(adjacency) <- TRUE

  family_id <- setNames(rep(NA_integer_, n), candidates)
  current_family <- 0L
  for (i in seq_len(n)) {
    if (!is.na(family_id[i])) next
    current_family <- current_family + 1L
    # breadth-first traversal over the |r| >= threshold graph so
    # transitively-linked candidates land in one family even if not
    # every pair among them individually clears the threshold
    frontier <- i
    visited <- logical(n)
    while (length(frontier) > 0) {
      node <- frontier[1]; frontier <- frontier[-1]
      if (visited[node]) next
      visited[node] <- TRUE
      family_id[node] <- current_family
      neighbors <- which(adjacency[node, ] & !visited)
      frontier <- c(frontier, neighbors)
    }
  }
  list(correlation_matrix = corr, family_id = family_id, method = method, threshold = threshold)
}

#' Prints a compact family-correlation summary for a position's battery
#' results (candidate name, family id, family size, and -- for families
#' of size > 1 -- which other candidates share the family) alongside the
#' bias-corrected concordance table, so a reader sees at a glance which
#' "findings" are actually independent and which are the same signal
#' counted multiple times (section 1.7's "report by family, not raw
#' count of significant candidates").
#'
#' @param family_matrix_result the list returned by compute_candidate_family_matrix()
#' @return a data frame: Stat, Family_Id, Family_Size, Family_Members (comma-joined, excludes self)
summarize_candidate_families <- function(family_matrix_result) {
  fam <- family_matrix_result$family_id
  sizes <- table(fam)
  members_str <- sapply(names(fam), function(stat) {
    this_fam <- fam[stat]
    peers <- setdiff(names(fam)[fam == this_fam], stat)
    if (length(peers) == 0) "(none)" else paste(peers, collapse = ", ")
  })
  data.frame(
    Stat = names(fam),
    Family_Id = unname(fam),
    Family_Size = as.integer(unname(sizes)[match(as.character(fam), names(sizes))]),
    Family_Members = unname(members_str),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
