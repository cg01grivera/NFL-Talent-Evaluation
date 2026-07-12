# ============================================================
# grading_utils.R
# Statistical primitives reused from nfl_team_grades: decay-weighted
# averaging (the core year-over-year weighting scheme this whole
# project's persistence testing depends on) and player-name
# normalization (for matching the same player across data sources that
# format names differently). zscore/sub_grade also included since
# they'll be needed once this project moves from persistence/Beat-ADP
# testing to building an actual talent-composite score, the same
# z-score -> sum -> re-z-score -> stanine pipeline nfl_team_grades uses.
# ============================================================

#' Z-score a numeric vector against itself (i.e. against the pool/group)
zscore <- function(x) {
  (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
}

#' Convert a z-score into a 1-9 "stanine" scale
stanine <- function(z) {
  pmin(9, pmax(1, round(2 * z + 5)))
}

#' Map a stanine (1-9) to a letter grade
letter_grade <- function(stan) {
  dplyr::case_when(
    is.na(stan)  ~ NA_character_,
    stan >= 9 ~ "Elite",
    stan >= 8 ~ "Great",
    stan >= 7 ~ "Good",
    stan >= 6 ~ "Above Average",
    stan >= 5 ~ "Average",
    stan >= 4 ~ "Below Average",
    stan >= 3 ~ "Bad",
    stan >= 2 ~ "Very Bad",
    stan >= 0 ~ "Terrible",
    TRUE ~ NA_character_
  )
}

#' Take a matrix/data.frame of per-metric z-scores (one column per
#' metric, one row per player) and run the second stage of the
#' pipeline: sum the z-scores, re-z-score that sum across the pool,
#' convert to stanine, grade. Not yet used by anything in this project
#' (persistence/Beat-ADP testing doesn't need it), but will be once a
#' final talent-composite score gets built.
sub_grade <- function(z_matrix, suffix = "") {
  z_matrix <- as.matrix(z_matrix)
  z_sum <- rowSums(z_matrix, na.rm = TRUE)
  z_tot <- zscore(z_sum)
  stan  <- stanine(z_tot)
  grade <- letter_grade(stan)

  out <- data.frame(z_sum = z_sum, z_tot = z_tot, stanine = stan, grade = grade)
  names(out) <- paste0(names(out), suffix)
  out
}

#' Project-wide standard weighting scheme: exponential decay with a HARD
#' cutoff, not an infinite tail. Weight for k years back (k=0 = most
#' recent) is r^k for k < lookback, and exactly ZERO for k >= lookback.
#' r=0.5, lookback=4 were validated (in nfl_team_grades) via a decay-
#' rate grid search against real persistence across team/QB stats --
#' kept as the default here too, though this project's own persistence
#' screen (analyze_player_talent_decay_rate.R) tests the full grid
#' fresh for every player-talent stat rather than assuming the old
#' project's result transfers.
#'
#' @param values raw metric values
#' @param years_back how many years before the target season each value
#'   is from (0 = the most recent/target-adjacent season)
decay_weighted_avg <- function(values, years_back, r = 0.5, lookback = 4) {
  # years_back >= 0 is required, not just years_back < lookback --
  # without it, a row from a year AFTER end_year (years_back negative)
  # still passes the < lookback check and gets weighted by r^years_back,
  # which for a negative exponent is a MUCH LARGER weight than any
  # legitimate in-window year. Confirmed real bug in the parent project
  # (nfl_team_grades) when a shared multi-year table was reused across
  # several target years for efficiency -- fixed here from the start
  # rather than re-discovered.
  keep <- years_back >= 0 & years_back < lookback & !is.na(values)
  if (!any(keep)) return(NA_real_)
  w <- r ^ years_back[keep]
  sum(w * values[keep]) / sum(w)
}

#' Vectorized wrapper: compute decay_weighted_avg for every value in
#' `group_vals` (e.g. every player), returning a numeric vector in the
#' same order.
decay_weighted_avg_vec <- function(df, group_col, value_col, group_vals, end_year,
                                    year_col = "Year", r = 0.5, lookback = 4) {
  vapply(group_vals, function(gv) {
    sub <- df[df[[group_col]] == gv & !is.na(df[[group_col]]), ]
    if (nrow(sub) == 0) return(NA_real_)
    years_back <- end_year - sub[[year_col]]
    decay_weighted_avg(sub[[value_col]], years_back, r, lookback)
  }, numeric(1))
}

#' Normalize a player name for fuzzy matching across data sources that
#' may format the same person differently (full name vs. abbreviated,
#' with/without suffixes, inconsistent spacing/punctuation). Strips
#' whitespace, periods, common suffixes, and case.
normalize_player_name <- function(name) {
  suffixes <- c("Jr", "Sr", "II", "III", "IV", "V")
  vapply(name, function(n) {
    if (is.na(n) || trimws(n) == "") return(NA_character_)
    parts <- strsplit(trimws(n), "\\s+")[[1]]
    parts <- gsub("\\.", "", parts)
    parts <- gsub("['\u2019]", "", parts)  # strip straight and curly apostrophes
    parts <- parts[!parts %in% suffixes]
    tolower(paste(parts, collapse = ""))
  }, character(1), USE.NAMES = FALSE)
}
