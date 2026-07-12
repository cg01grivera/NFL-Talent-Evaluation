# ============================================================
# utils_core.R
# Small, generic utilities shared by every fetch/analysis script in
# this project. Extracted from nfl_team_grades' fetch_nflverse_data.R
# -- these three functions are needed by nearly everything else here,
# unchanged from that project.
# ============================================================
if (!requireNamespace("nflreadr", quietly = TRUE)) {
  message("Installing nflreadr from CRAN...")
  install.packages("nflreadr", repos = "https://cloud.r-project.org")
}
library(dplyr)

#' Standardize team abbreviations to their CURRENT location/code.
#' nflverse's raw data keeps the abbreviation as it was at the time
#' (e.g. "OAK" for pre-2020 Raiders seasons, "SD" for pre-2017 Chargers,
#' "STL" for pre-2016 Rams) rather than rewriting history -- so every
#' team-column from every load_*() call needs to pass through this
#' before being used as a join/group key, or older seasons for a
#' relocated franchise will silently fail to match and get dropped with
#' no error.
standardize_teams <- function(x) {
  if (!requireNamespace("nflreadr", quietly = TRUE)) return(x)
  out <- x
  non_na <- !is.na(x)
  out[non_na] <- nflreadr::clean_team_abbrs(x[non_na])
  out
}

#' Filter play-by-play down to regular season only. Without this,
#' playoff games get silently pooled into "season" stats for any player
#' whose team made the postseason.
filter_reg_pbp <- function(pbp) {
  if ("season_type" %in% names(pbp)) pbp %>% filter(season_type == "REG") else pbp
}
filter_reg_sched <- function(sched) {
  if ("game_type" %in% names(sched)) sched %>% filter(game_type == "REG") else sched
}

#' Clamp a requested season range down to seasons nflverse actually has
#' data for. load_pbp()/load_schedules() hard-error if asked for a
#' season beyond nflreadr::most_recent_season() -- this turns that into
#' a graceful "no data for that season" instead.
clamp_seasons <- function(seasons) {
  if (requireNamespace("nflreadr", quietly = TRUE)) {
    latest <- nflreadr::most_recent_season()
    seasons <- seasons[seasons <= latest]
  }
  seasons
}
