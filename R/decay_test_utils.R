# ============================================================
# decay_test_utils.R
# The persistence-testing engine: does a decay-weighted blend of a
# player's PAST seasons predict his FUTURE season's value of the same
# stat. Entity-generic (works on any entity_col -- team, player name,
# whatever); promoted here into its own shared file rather than left
# duplicated inside each analyze script, since this project will reuse
# it repeatedly as new candidate stats get proposed.
# ============================================================

evaluate_decay_rate <- function(long_data, entity_col, value_col, year_col, r, lookback = 4) {
  years <- sort(unique(long_data[[year_col]]))
  results <- list()
  for (target_year in years) {
    prior_years <- (target_year - lookback):(target_year - 1)
    candidates <- long_data[long_data[[year_col]] == target_year, ]
    for (i in seq_len(nrow(candidates))) {
      ent <- candidates[[entity_col]][i]
      actual <- candidates[[value_col]][i]
      if (is.na(actual)) next
      hist <- long_data[long_data[[entity_col]] == ent & long_data[[year_col]] %in% prior_years, ]
      hist <- hist[!is.na(hist[[value_col]]), ]
      if (nrow(hist) == 0) next
      k <- target_year - hist[[year_col]]
      w <- r^(k - 1)
      predictor <- sum(w * hist[[value_col]]) / sum(w)
      results[[length(results) + 1]] <- data.frame(entity = ent, target_year = target_year,
                                                      predictor = predictor, actual = actual)
    }
  }
  do.call(rbind, results)
}

#' Test a grid of decay rates for one stat, returning correlations plus
#' sample-size diagnostics (which rows qualify never depends on r, only
#' the weighting within an already-qualifying row does, so these are
#' computed once rather than per grid value).
test_decay_rates <- function(long_data, entity_col, value_col, year_col, r_grid, lookback = 4) {
  cors <- sapply(r_grid, function(r) {
    df <- evaluate_decay_rate(long_data, entity_col, value_col, year_col, r, lookback)
    if (is.null(df) || nrow(df) < 10) return(NA)
    cor(df$predictor, df$actual, use = "complete.obs")
  })
  diag_df <- evaluate_decay_rate(long_data, entity_col, value_col, year_col, r = r_grid[1], lookback = lookback)
  n_pairs <- if (is.null(diag_df)) 0L else nrow(diag_df)
  n_years <- if (is.null(diag_df)) 0L else length(unique(diag_df$target_year))
  list(correlations = cors, n_pairs = n_pairs, n_years = n_years)
}
