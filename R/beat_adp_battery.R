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
#' @param return_fold_coefs if TRUE, attaches the candidate's own fitted
#'   coefficient from model_b in EVERY LOSO training fold as
#'   attr(result, "fold_coefficients") (a named vector, Season -> coef).
#'   This is D4a of the WR/TE anomaly resolution ladder
#'   (WR_TE_ANOMALY_RESOLUTION_PLAN.md): a machinery-free corroboration
#'   read for H2 -- consistently negative fold coefficients mean the
#'   model is actively learning a mean-reversion relationship, not that
#'   bootstrap/permutation noise is degrading an otherwise-neutral
#'   candidate. Attached as an attribute (matching this library's
#'   existing diagnostics-as-attributes convention, e.g. paired_wilcox_p,
#'   coverage_spotcheck) rather than changing the return type, so
#'   existing callers with return_fold_coefs=FALSE (the default) see
#'   byte-identical behavior. NULL when a fold's model_b fit doesn't
#'   include the candidate term (e.g. a rank-deficient fit) or when the
#'   function early-returns due to insufficient data -- not silently
#'   dropped from the vector, which would misrepresent how many folds
#'   actually ran.
run_concordance <- function(data, candidate_col, pair_rank_windows = c(6, 12, 18, 24),
                             significance_window = 12, min_train = 15, return_pairs = FALSE,
                             return_fold_coefs = FALSE) {
  data <- data %>% filter(!is.na(.data[[candidate_col]]))
  seasons_present <- sort(unique(data$Season))
  if (nrow(data) < 15 || length(seasons_present) < 4) {
    out <- setNames(rep(NA_real_, length(pair_rank_windows)), paste0("Concordance_Diff_W", pair_rank_windows))
    summary_vec <- c(out, Concordance_Sig_P = NA_real_)
    result <- if (return_pairs) list(summary = summary_vec, pairs = NULL) else summary_vec
    if (return_fold_coefs) attr(result, "fold_coefficients") <- NULL
    return(result)
  }
  data$pred_a <- NA_real_
  data$pred_b <- NA_real_
  fmla_b <- as.formula(paste("season_ppg ~ RANK + Prior_PPG +", candidate_col))
  fold_coefs <- if (return_fold_coefs) numeric(0) else NULL
  for (held_out in seasons_present) {
    train <- data %>% filter(Season != held_out)
    if (nrow(train) < min_train) next
    model_a <- lm(season_ppg ~ RANK + Prior_PPG, data = train)
    model_b <- lm(fmla_b, data = train)
    idx <- which(data$Season == held_out)
    data$pred_a[idx] <- predict(model_a, newdata = data[idx, ])
    data$pred_b[idx] <- predict(model_b, newdata = data[idx, ])
    if (return_fold_coefs) {
      cf <- coef(model_b)
      fold_coefs[as.character(held_out)] <- if (candidate_col %in% names(cf)) unname(cf[candidate_col]) else NA_real_
    }
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
    result <- if (return_pairs) list(summary = summary_vec, pairs = NULL) else summary_vec
    if (return_fold_coefs) attr(result, "fold_coefficients") <- fold_coefs
    return(result)
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
  result <- if (return_pairs) list(summary = summary_vec, pairs = sig_pairs) else summary_vec
  if (return_fold_coefs) attr(result, "fold_coefficients") <- fold_coefs
  result
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
#' @param return_direct_reads if TRUE, attaches attr(result,
#'   "fold_coef_summary") (summarize_fold_coefficients() on a
#'   return_fold_coefs=TRUE concordance fit) and attr(result,
#'   "signal_read") (compute_candidate_signal_read()) -- TEAM_CONTEXT_
#'   REWORK_PLAN_V2.md Section 6's direct-signal-read outputs, wired
#'   into the battery path rather than requiring a separate diagnostic
#'   script call. Attached as attributes, not new named-vector entries,
#'   matching this library's existing diagnostics-as-attributes
#'   convention -- default FALSE preserves byte-identical output for
#'   every existing caller.
run_full_battery_estimation <- function(data, candidate_col, significance_window = 12,
                                          n_boot = 2000, conf_level = 0.90, skip_rmse = FALSE,
                                          return_direct_reads = FALSE) {
  rmse_ci <- if (skip_rmse) {
    c(Point_Estimate = NA_real_, CI_Lower = NA_real_, CI_Upper = NA_real_, N_Seasons = NA_integer_)
  } else {
    run_rmse_bootstrap(data, candidate_col, n_boot = n_boot, conf_level = conf_level)
  }
  conc_ci <- run_concordance_bootstrap(data, candidate_col, significance_window = significance_window,
                                        n_boot = n_boot, conf_level = conf_level)
  breakout_res <- run_auc_cv(data, candidate_col, "is_breakout")
  bust_res <- run_auc_cv(data, candidate_col, "is_bust")
  result <- c(
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
  if (return_direct_reads) {
    fold_res <- run_concordance(data, candidate_col, significance_window = significance_window,
                                 return_fold_coefs = TRUE)
    attr(result, "fold_coef_summary") <- summarize_fold_coefficients(attr(fold_res, "fold_coefficients"))
    attr(result, "signal_read") <- compute_candidate_signal_read(data, candidate_col)
  }
  result
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
#' @param extra_key_col which column of `merged` (the assembled player-
#'   season pool) identifies rows in `extra_data`. Default "norm_name"
#'   reproduces the original player-keyed behavior byte-for-byte -- the
#'   decay lookup is done per player. Team-context callers pass "Team":
#'   `merged$Team` is the player's CORRECTED, historically-accurate team
#'   for the TARGET season yr (via correct_adp_team()'s primary_team
#'   join above, not whatever ADP originally listed), so a traded player
#'   draws his target-season team's trailing history, not his old team's
#'   -- and every player still on the same team in yr shares one team-
#'   level decayed value. The join back onto `merged` always happens by
#'   "norm_name" regardless of extra_key_col, since that's the row-level
#'   identity key of the assembled pool; extra_key_col only changes which
#'   column extra_data is looked up BY.
build_position_data <- function(pos, adp, all_seasons_ppg, prior_ppg_lookup, extra_data, candidates,
                                 seasons_to_test, decay_r, lookback_years, extra_key_col = "norm_name") {
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
        decay_weighted_avg_vec(extra_data, extra_key_col, col, merged[[extra_key_col]], end_year,
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
#' Constructs the synthetic null-candidate column under one of two modes
#' -- factored out of estimate_concordance_null_bias() so both modes
#' share one code path rather than diverging copies (section 1.2).
#'
#' "permute" (the original mode): shuffles the candidate's own values
#' within season. Preserves the candidate's marginal distribution;
#' DESTROYS its correlation with RANK/Prior_PPG. This is what H1 (the
#' WR/TE anomaly's lead hypothesis) says is collinearity-blind: it
#' measures the concordance degradation from adding ORTHOGONAL noise to
#' the model, which is a real measurement for an orthogonal candidate
#' but an underestimate of the degradation for a candidate that is
#' itself collinear with RANK/Prior_PPG (a real WR receiving-volume
#' stat, for instance).
#'
#' "residual_permute" (D3's matched null): fits
#' candidate ~ baseline_cols WITHIN EACH SEASON, permutes the
#' RESIDUALS within season, adds back that season's fitted component.
#' The resulting synthetic candidate carries the real candidate's exact
#' collinearity with baseline_cols (same fitted component every
#' replicate) while its incremental, baseline-orthogonal information is
#' destroyed (the residual is what's permuted). If H1 is correct, this
#' null should measure a LARGER degradation for collinear candidates
#' than "permute" does, and the resulting bias correction should
#' re-center corrected estimates near zero where "permute" left them
#' negative.
#'
#' PER-SEASON FIT FALLBACK: a season with too few rows for a stable
#' baseline_cols fit (fewer than length(baseline_cols)+2 observations,
#' or a singular fit) falls back to a fit on all OTHER seasons' data
#' instead of erroring or silently zero-filling -- this is disclosed
#' via a one-time message per call, not silent, per section 1.5's
#' "assert, don't assume."
#'
#' @param data the position's player-season data frame, already filtered
#'   to non-NA representative_candidate_col rows
#' @param representative_candidate_col candidate column to build a null for
#' @param null_mode "permute" (default, original behavior) or "residual_permute" (D3)
#' @param baseline_cols predictors for residual_permute's per-season fit
#'   (default c("RANK","Prior_PPG"), matching model_a/model_b's shared term)
#' @param permute_within "player" (default, original behavior: every row
#'   is shuffled independently within season) or "team" (TEAM_CONTEXT_
#'   REWORK_PLAN_V2.md Section 4.3): shuffles the TEAM -> VALUE mapping
#'   within season and re-broadcasts it to every row sharing that team,
#'   preserving the clustered tie structure a real broadcast (team-level)
#'   candidate has -- a player-level shuffle would scatter each row
#'   independently and destroy that clustering, understating the real
#'   candidate's tie structure relative to its null. Only supported under
#'   null_mode = "permute"; combining with "residual_permute" is refused
#'   rather than silently falling back to a player-level shuffle, since
#'   that combination has not been validated by the Gate 3 synthetic.
#' @param team_col column identifying each row's team (default "Team"),
#'   only consulted when permute_within = "team"
#' @return data with a new "permuted" column (name kept for backward
#'   compatibility with callers regardless of which mode built it)
build_null_candidate_column <- function(data, representative_candidate_col, null_mode = "permute",
                                         baseline_cols = c("RANK", "Prior_PPG"),
                                         permute_within = "player", team_col = "Team") {
  if (!permute_within %in% c("player", "team")) {
    stop("build_null_candidate_column: permute_within must be 'player' or 'team', got: ", permute_within)
  }
  if (null_mode == "permute") {
    if (permute_within == "team") {
      return(data %>% group_by(Season) %>%
               mutate(permuted = {
                 vals <- .data[[representative_candidate_col]]
                 teams <- .data[[team_col]]
                 first_row_per_team <- !duplicated(teams)
                 shuffled_vals <- sample(vals[first_row_per_team])
                 lookup <- setNames(shuffled_vals, teams[first_row_per_team])
                 unname(lookup[as.character(teams)])
               }) %>% ungroup())
    }
    return(data %>% group_by(Season) %>%
             mutate(permuted = sample(.data[[representative_candidate_col]])) %>% ungroup())
  }
  if (null_mode != "residual_permute") {
    stop("build_null_candidate_column: null_mode must be 'permute' or 'residual_permute', got: ", null_mode)
  }
  if (permute_within == "team") {
    stop("build_null_candidate_column: permute_within = 'team' is only supported under null_mode = 'permute' -- ",
         "the team-broadcast + residual-permute combination is unvalidated and refused rather than silently ",
         "falling back to a player-level shuffle.")
  }

  fmla <- as.formula(paste(representative_candidate_col, "~", paste(baseline_cols, collapse = " + ")))
  min_obs <- length(baseline_cols) + 2
  fallback_warned <- FALSE

  data$permuted <- NA_real_
  for (s in unique(data$Season)) {
    idx <- which(data$Season == s)
    sub <- data[idx, , drop = FALSE]
    fit <- tryCatch({
      if (nrow(sub) < min_obs) stop("too few observations")
      lm(fmla, data = sub)
    }, error = function(e) NULL)

    if (is.null(fit)) {
      if (!fallback_warned) {
        message("  build_null_candidate_column: season ", s, " (and possibly others) had too few rows",
                " for its own residual_permute fit -- falling back to a fit on all OTHER seasons' data.",
                " (this message prints once per call)")
        fallback_warned <- TRUE
      }
      fit <- lm(fmla, data = data[-idx, , drop = FALSE])
    }

    fitted_vals <- predict(fit, newdata = sub)
    resid_vals <- sub[[representative_candidate_col]] - fitted_vals
    data$permuted[idx] <- fitted_vals + sample(resid_vals)
  }
  data
}

estimate_concordance_null_bias <- function(data, representative_candidate_col, n_replicates = 100,
                                            significance_window = 12, n_boot = 500, conf_level = 0.90,
                                            null_mode = "permute", baseline_cols = c("RANK", "Prior_PPG"),
                                            permute_within = "player", team_col = "Team") {
  data <- data %>% filter(!is.na(.data[[representative_candidate_col]]))
  point_ests <- numeric(n_replicates)
  start_time <- Sys.time()
  for (i in seq_len(n_replicates)) {
    data_perm <- build_null_candidate_column(data, representative_candidate_col, null_mode, baseline_cols,
                                              permute_within = permute_within, team_col = team_col)
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
                                                   n_boot = 500, conf_level = 0.90,
                                                   null_mode = "permute", baseline_cols = c("RANK", "Prior_PPG"),
                                                   permute_within = "player", team_col = "Team") {
  meta_estimates <- numeric(n_meta_replicates)
  start_time <- Sys.time()
  for (i in seq_len(n_meta_replicates)) {
    message("  meta-replicate ", i, " of ", n_meta_replicates, " (each running ", n_replicates, " inner replicates)...")
    res <- estimate_concordance_null_bias(data, representative_candidate_col, n_replicates = n_replicates,
                                           significance_window = significance_window, n_boot = n_boot,
                                           conf_level = conf_level, null_mode = null_mode, baseline_cols = baseline_cols,
                                           permute_within = permute_within, team_col = team_col)
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
                                                  force_recompute = FALSE,
                                                  null_mode = "permute", baseline_cols = c("RANK", "Prior_PPG"),
                                                  permute_within = "player", team_col = "Team") {
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

  # null_mode/baseline_cols added to the cache key 2026-07-12 (D3, the
  # WR/TE anomaly matched-null): without this, a residual_permute
  # measurement and an orthogonal-permute measurement for the exact
  # same position/candidate/config would share one cache_key and the
  # second call would silently reuse whichever was computed first --
  # exactly the stale-cache hazard this cache's own docstring warns
  # about, just via a new parameter instead of a new build script.
  # permute_within added for the Team-Context Rework (TEAM_CONTEXT_
  # REWORK_PLAN_V2.md Section 4.3): a player-level and a team-level
  # permutation null are DIFFERENT MEASUREMENTS of the same candidate,
  # not interchangeable cache hits, per CLAUDE.md's cache-hygiene
  # invariant ("never reuse a bias across ... permutation modes").
  cache_key <- paste(position, representative_candidate_col,
                      min(seasons_to_test), max(seasons_to_test), min_games, decay_r, lookback_years,
                      n_meta_replicates, n_replicates, significance_window, n_boot, conf_level,
                      n_rows, n_nonNA, null_mode, paste(baseline_cols, collapse = "+"), permute_within, sep = "|")

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
          " (data fingerprint: ", n_rows, " rows, ", n_nonNA, " non-NA, null_mode=", null_mode,
          ", permute_within=", permute_within, ")...")
  fresh <- estimate_concordance_null_bias_robust(data, representative_candidate_col, n_meta_replicates,
                                                  n_replicates, significance_window, n_boot, conf_level,
                                                  null_mode = null_mode, baseline_cols = baseline_cols,
                                                  permute_within = permute_within, team_col = team_col)
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
#' @param permute_within "player" (default) or "team" (TEAM_CONTEXT_
#'   REWORK_PLAN_V2.md Section 4.3) -- passed through to both the
#'   primary/secondary bias estimation and the coverage spot-check's own
#'   permutation, so a team-context run's null is measured the same way
#'   throughout, never a player-level null silently applied to a
#'   broadcast candidate.
#' @param team_col column identifying each row's team (default "Team"),
#'   only consulted when permute_within = "team".
#' @param emit_direct_reads if TRUE (default, matching the prereg's
#'   EMIT_DIRECT_READS config flag), each candidate's output row gains
#'   fold-coefficient and per-season-Spearman direct-read summaries plus
#'   a Method_Disagreement_Flag (TEAM_CONTEXT_REWORK_PLAN_V2.md Section
#'   6, the YAC_Share lesson) -- see compute_method_disagreement_flag().
run_estimation_for_position <- function(pos, data, candidates, representative_candidate_col,
                                         seasons_to_test, min_games, decay_r, lookback_years,
                                         significance_window = 12, n_boot = 500, conf_level = 0.90,
                                         skip_rmse = TRUE, force_recompute_bias = FALSE,
                                         secondary_candidates = NULL, secondary_representative_candidate = NULL,
                                         sig_window_fn = NULL, run_coverage_spotcheck = FALSE,
                                         coverage_n_replicates = 50,
                                         bias_n_meta_replicates = 5, bias_n_replicates = 100, bias_n_boot = 500,
                                         cache_path = "output/concordance_null_bias_cache.csv",
                                         permute_within = "player", team_col = "Team",
                                         emit_direct_reads = TRUE) {
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
    n_boot = bias_n_boot, conf_level = conf_level, force_recompute = force_recompute_bias, cache_path = cache_path,
    permute_within = permute_within, team_col = team_col
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
      n_boot = bias_n_boot, conf_level = conf_level, force_recompute = force_recompute_bias, cache_path = cache_path,
      permute_within = permute_within, team_col = team_col
    )
    secondary_bias <- unname(secondary_bias_est["Null_Bias"])
    cat("  ", pos, " secondary bias =", round(secondary_bias, 5), "\n")
  }

  if (run_coverage_spotcheck) {
    message("  Running ", pos, " coverage spot-check (large-pool positions shouldn't assume validated\n",
            "  calibration from other positions transfers automatically)...")
    n_cov <- coverage_n_replicates
    covered <- logical(n_cov)
    start_cov <- Sys.time()
    for (i in seq_len(n_cov)) {
      # Routed through build_null_candidate_column() (rather than an
      # inline sample()) so a team-context spot-check exercises the SAME
      # permute_within = "team" broadcast-preserving shuffle the actual
      # bias measurement above used -- not a player-level shuffle that
      # would silently answer a different calibration question.
      perm <- build_null_candidate_column(data, representative_candidate_col, null_mode = "permute",
                                           permute_within = permute_within, team_col = team_col)
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
                                        n_boot = n_boot, conf_level = conf_level, skip_rmse = skip_rmse,
                                        return_direct_reads = emit_direct_reads)
    # Direct-read attributes must be pulled off BEFORE apply_concordance_
    # bias_correction() below, since that function's own c(battery_result,
    # ...) construction does not preserve attributes (same reason
    # run_full_battery_estimation() attaches them as attributes in the
    # first place, per its own docstring -- they'd otherwise be silently
    # dropped one call later).
    fold_summary <- if (emit_direct_reads) attr(res, "fold_coef_summary") else NULL
    signal_read <- if (emit_direct_reads) attr(res, "signal_read") else NULL
    bias_to_use <- if (!is.null(secondary_candidates) && stat %in% secondary_candidates) secondary_bias else null_bias
    res <- apply_concordance_bias_correction(res, bias_to_use)
    if (emit_direct_reads) {
      disagreement <- compute_method_disagreement_flag(
        unname(res["Concordance_Point_Estimate_BiasCorrected"]), signal_read$mean_rho
      )
      res <- c(res,
               Direct_Read_Mean_Rho = signal_read$mean_rho,
               Direct_Read_Pct_Negative = signal_read$pct_negative,
               Fold_Coef_Mean = fold_summary$Mean,
               Fold_Coef_Pct_Negative = fold_summary$Pct_Negative,
               Method_Disagreement_Flag = as.numeric(disagreement))
    }
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

#' D1 of the WR/TE anomaly resolution ladder (WR_TE_ANOMALY_RESOLUTION_PLAN.md):
#' audits a position's candidate columns for exact or near-exact
#' duplication after build_position_data()'s decay-weighting/join, and
#' reports per-column variance/NA/uniqueness -- specifically built to
#' resolve TE's Target_Share/Targets_PG/WOPR raw-concordance-exactly-
#' 0.0000 triple: are these genuinely distinct (small-pool quantization)
#' or literally the same vector reaching the model under three names
#' (a construction bug)?
#'
#' Reuses compute_candidate_family_matrix() for the pairwise correlation
#' matrix rather than recomputing it -- per section 1.2, single source
#' of truth -- and adds what correlation alone cannot tell you:
#' correlation = 1 is consistent with either identical values OR
#' different values related by an exact linear transform (y = a*x + b,
#' a != 1). Both make two "candidates" functionally redundant for
#' concordance purposes, but only the former is a construction bug;
#' this function distinguishes them explicitly rather than collapsing
#' both into one "near-duplicate" bucket.
#'
#' NULL/EDGE-CASE BEHAVIOR: a constant column has undefined correlation
#' with everything (NA, via compute_candidate_family_matrix()) and is
#' reported with N_Unique = 1 so it's visible in the audit table even
#' though it can't appear in the identical-pairs list.
#'
#' @param data the position's player-season data frame
#' @param candidates character vector of candidate column names
#' @param identity_threshold |r| at or above which a pair is flagged for
#'   identity inspection (default 0.999 -- deliberately stricter than
#'   compute_candidate_family_matrix()'s default 0.7 family threshold;
#'   D1 is asking "are these secretly the same column", not "are these
#'   related enough to report together")
#' @return list with:
#'   - column_summary: data frame of Stat, Variance, N_NA, N_Unique
#'   - identical_pairs: data frame of Stat_A, Stat_B, Correlation,
#'     Values_Identical (TRUE = construction bug candidate; FALSE = a
#'     genuine but perfectly-collinear pair -- still relevant to D2/H1,
#'     not a bug by itself)
audit_candidate_column_identity <- function(data, candidates, identity_threshold = 0.999) {
  missing_cols <- setdiff(candidates, names(data))
  if (length(missing_cols) > 0) {
    stop("audit_candidate_column_identity: candidate column(s) not found in data: ",
         paste(missing_cols, collapse = ", "))
  }

  column_summary <- do.call(rbind, lapply(candidates, function(stat) {
    v <- data[[stat]]
    data.frame(
      Stat = stat,
      Variance = if (all(is.na(v))) NA_real_ else stats::var(v, na.rm = TRUE),
      N_NA = sum(is.na(v)),
      N_Unique = length(unique(v[!is.na(v)])),
      stringsAsFactors = FALSE
    )
  }))
  rownames(column_summary) <- NULL

  fam <- compute_candidate_family_matrix(data, candidates, threshold = identity_threshold)
  corr <- fam$correlation_matrix
  n <- length(candidates)
  pairs <- list()
  for (i in seq_len(n - 1)) {
    for (j in seq((i + 1), n)) {
      r <- corr[i, j]
      if (!is.na(r) && abs(r) >= identity_threshold) {
        stat_a <- candidates[i]; stat_b <- candidates[j]
        va <- data[[stat_a]]; vb <- data[[stat_b]]
        complete <- !is.na(va) & !is.na(vb)
        values_identical <- isTRUE(all.equal(va[complete], vb[complete], tolerance = 1e-8))
        pairs[[length(pairs) + 1]] <- data.frame(
          Stat_A = stat_a, Stat_B = stat_b, Correlation = unname(r),
          Values_Identical = values_identical, stringsAsFactors = FALSE
        )
      }
    }
  }
  identical_pairs <- if (length(pairs) > 0) do.call(rbind, pairs) else
    data.frame(Stat_A = character(0), Stat_B = character(0),
               Correlation = numeric(0), Values_Identical = logical(0))

  list(column_summary = column_summary, identical_pairs = identical_pairs)
}

#' D2 of the WR/TE anomaly resolution ladder: for each candidate,
#' computes how much of its variance is already explained by the same
#' baseline (RANK, Prior_PPG) that model_a and model_b both include --
#' i.e. how collinear each candidate is with the model's existing
#' predictors, independent of any concordance/bootstrap machinery. This
#' is the observational signature H1 predicts: if the permutation null
#' is collinearity-blind (see estimate_concordance_null_bias()'s
#' docstring), candidates with high R² here should show the most
#' negative bias-corrected concordance, and low-R² candidates should
#' not.
#'
#' NULL CANDIDATE BEHAVIOR: a constant candidate has an undefined R²
#' (regressing a constant on anything is degenerate) and returns NA,
#' not 0 or 1 -- 0 would wrongly claim "no relationship" when the
#' question doesn't apply, and 1 would wrongly claim total collinearity.
#'
#' @param data the position's player-season data frame
#' @param candidates character vector of candidate column names
#' @param baseline_cols predictors to regress each candidate against
#'   (default c("RANK","Prior_PPG"), matching model_a/model_b's shared term)
#' @return data frame: Stat, R_Squared, N_Obs (rows used for that candidate's fit)
compute_candidate_collinearity <- function(data, candidates, baseline_cols = c("RANK", "Prior_PPG")) {
  missing_cols <- setdiff(c(candidates, baseline_cols), names(data))
  if (length(missing_cols) > 0) {
    stop("compute_candidate_collinearity: column(s) not found in data: ",
         paste(missing_cols, collapse = ", "))
  }
  rows <- lapply(candidates, function(stat) {
    sub <- data[, c(stat, baseline_cols), drop = FALSE]
    sub <- sub[stats::complete.cases(sub), , drop = FALSE]
    n_obs <- nrow(sub)
    if (n_obs < (length(baseline_cols) + 2) || all(sub[[stat]] == sub[[stat]][1])) {
      return(data.frame(Stat = stat, R_Squared = NA_real_, N_Obs = n_obs, stringsAsFactors = FALSE))
    }
    fmla <- as.formula(paste(stat, "~", paste(baseline_cols, collapse = " + ")))
    fit <- tryCatch(lm(fmla, data = sub), error = function(e) NULL)
    r2 <- if (is.null(fit)) NA_real_ else summary(fit)$r.squared
    data.frame(Stat = stat, R_Squared = r2, N_Obs = n_obs, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Summarizes a run_concordance(..., return_fold_coefs=TRUE) result's
#' fold_coefficients attribute -- D4a of the WR/TE anomaly resolution
#' ladder. Consistently negative fold coefficients across seasons mean
#' the model is actively learning a mean-reversion relationship (H2);
#' coefficients scattered around zero mean the concordance degradation
#' seen elsewhere is variance, not a learned signal.
#'
#' CAVEAT ON READING "CONSISTENCY": LOSO folds are NOT independent of
#' each other -- any two folds share all but one season's worth of
#' training data (e.g. 11 of 12 seasons for this project's typical
#' pool). A real, non-zero incidental relationship in the data --
#' including one that isn't a genuine population-level effect, just a
#' feature of this particular sample -- will tend to show up with a
#' consistent SIGN across most folds simply because the folds are
#' mostly the same data, not because it's a robust, reproducible
#' effect. Sign-consistency across folds is therefore weaker evidence
#' than it looks; MAGNITUDE relative to a known baseline (or, better,
#' consistency ACROSS INDEPENDENT SEASONS via compute_candidate_signal_
#' read()'s per-season Spearman, where each season's noise realization
#' really is independent) is the more trustworthy read. Confirmed via
#' test_beat_adp_battery.R: a single fixed noise draw showed 11 of 12
#' folds same-signed but at ~1/30th the magnitude of a genuine effect --
#' sign-consistency alone would have misread that as a real relationship.
#'
#' @param fold_coefficients a named numeric vector (Season -> coefficient),
#'   e.g. attr(run_concordance(..., return_fold_coefs=TRUE), "fold_coefficients")
#' @return a list: N_Folds, N_Negative, N_Positive, Pct_Negative, Mean,
#'   SD, Min, Max -- or all NA (N_Folds=0) if fold_coefficients is NULL
#'   or empty, not an error
summarize_fold_coefficients <- function(fold_coefficients) {
  if (is.null(fold_coefficients) || length(fold_coefficients) == 0) {
    return(list(N_Folds = 0, N_Negative = NA_integer_, N_Positive = NA_integer_,
                Pct_Negative = NA_real_, Mean = NA_real_, SD = NA_real_,
                Min = NA_real_, Max = NA_real_))
  }
  valid <- fold_coefficients[!is.na(fold_coefficients)]
  n <- length(valid)
  list(
    N_Folds = n,
    N_Negative = sum(valid < 0),
    N_Positive = sum(valid > 0),
    Pct_Negative = if (n > 0) sum(valid < 0) / n else NA_real_,
    Mean = if (n > 0) mean(valid) else NA_real_,
    SD = if (n > 1) sd(valid) else NA_real_,
    Min = if (n > 0) min(valid) else NA_real_,
    Max = if (n > 0) max(valid) else NA_real_
  )
}

#' D4b of the WR/TE anomaly resolution ladder: per-season Spearman
#' correlation between a candidate and the residual from
#' season_ppg ~ baseline_cols -- a raw-data read with NO bootstrap, NO
#' permutation, NO bias correction. If a candidate has real incremental
#' signal beyond RANK/Prior_PPG, its per-season correlation with what's
#' LEFT OVER after RANK/Prior_PPG should be consistently signed across
#' seasons; if the concordance-level degradation is pure variance/noise,
#' these per-season correlations should show no consistent sign.
#'
#' DESIGN CHOICE, stated explicitly: the residual is computed from ONE
#' baseline fit pooled across all seasons (not fit per-season) -- this
#' is deliberately the simplest possible construction, matching D4's
#' "machinery-free" purpose. A per-season baseline fit is a reasonable
#' alternative but is not what this function does; do not assume it.
#'
#' NULL/EDGE-CASE BEHAVIOR: a season with fewer than 3 non-NA rows for
#' the candidate, or a constant candidate within that season, returns
#' NA for that season's Spearman rho (correlation is undefined/unstable
#' below that), not an error and not a 0 that would misrepresent "no
#' relationship" as a real measurement.
#'
#' @param data the position's player-season data frame
#' @param candidate_col candidate column to correlate against the residual
#' @param baseline_cols predictors for the pooled residual fit (default
#'   c("RANK","Prior_PPG"), matching model_a's term)
#' @return list with:
#'   - by_season: data frame of Season, N, Spearman_Rho
#'   - n_seasons_negative, n_seasons_positive, pct_negative, mean_rho
compute_candidate_signal_read <- function(data, candidate_col, baseline_cols = c("RANK", "Prior_PPG")) {
  data <- data %>% filter(!is.na(.data[[candidate_col]]))
  fmla <- as.formula(paste("season_ppg ~", paste(baseline_cols, collapse = " + ")))
  fit <- lm(fmla, data = data)
  data$.resid <- data$season_ppg - predict(fit, newdata = data)

  by_season <- data %>%
    group_by(Season) %>%
    summarise(
      N = dplyr::n(),
      Spearman_Rho = if (dplyr::n() >= 3 && stats::var(.data[[candidate_col]]) > 0)
        suppressWarnings(cor(.data[[candidate_col]], .data[[".resid"]], method = "spearman")) else NA_real_,
      .groups = "drop"
    )

  valid_rho <- by_season$Spearman_Rho[!is.na(by_season$Spearman_Rho)]
  list(
    by_season = as.data.frame(by_season),
    n_seasons_negative = sum(valid_rho < 0),
    n_seasons_positive = sum(valid_rho > 0),
    pct_negative = if (length(valid_rho) > 0) sum(valid_rho < 0) / length(valid_rho) else NA_real_,
    mean_rho = if (length(valid_rho) > 0) mean(valid_rho) else NA_real_
  )
}

#' Method-disagreement flag between concordance's bias-corrected sign
#' and the direct per-season-Spearman signal read's sign -- TEAM_CONTEXT_
#' REWORK_PLAN_V2.md Section 6, the YAC_Share lesson: two independent
#' measurement approaches (bootstrap concordance vs. a machinery-free
#' raw-data correlation) disagreeing on DIRECTION means the candidate
#' cannot be reported as supported in either direction until reconciled,
#' regardless of how confident either measurement looks on its own.
#'
#' "Nominally non-trivial" (the plan's phrase) is operationalized here as
#' both signs being DEFINED (non-NA) and NON-ZERO -- a flag can only fire
#' when there are two actual, opposite directional claims to disagree
#' with each other. A zero or NA on either side means one side has no
#' directional claim at all, so there is nothing to conflict with.
#'
#' @param concordance_estimate the BIAS-CORRECTED concordance point
#'   estimate (Concordance_Point_Estimate_BiasCorrected), not the raw one
#'   -- the corrected number is what gets reported/compared against zero
#' @param direct_read_mean_rho compute_candidate_signal_read()'s mean_rho
#' @return TRUE if flagged, FALSE otherwise -- never NA, so this is
#'   always safe to use directly as a data.frame column or filter
compute_method_disagreement_flag <- function(concordance_estimate, direct_read_mean_rho) {
  conc_sign <- sign(concordance_estimate)
  read_sign <- sign(direct_read_mean_rho)
  isTRUE(!is.na(conc_sign) && !is.na(read_sign) && conc_sign != 0 && read_sign != 0 && conc_sign != read_sign)
}
