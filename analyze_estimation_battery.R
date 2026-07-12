cat("
================================================================
  analyze_estimation_battery.R

  GOAL: Apply the validated bootstrap estimation layer (R/beat_adp_
  battery.R's run_full_battery_estimation, confirmed by test_beat_adp_
  battery.R) to QB, RB, WR, and TE -- ALL FOUR POSITIONS IN ONE SCRIPT,
  deliberately, per this project's own decision after the
  build_position_data() divergence incident to never again maintain
  parallel, similar-but-not-identical battery scripts per position.
  WR/TE additionally use a secondary, shorter-history null bias for
  their NGS-derived candidates and per-candidate significance windows
  drawn from WR_TE_PREREGISTRATION.md -- both handled via optional,
  backward-compatible parameters on the SAME shared run_estimation_
  for_position() function QB/RB already use, not a forked copy.
  battery.R's run_full_battery_estimation, confirmed by test_beat_adp_
  battery.R) to the REAL QB and RB candidate lists already established
  in analyze_qb_talent_beat_adp.R / analyze_rb_talent_beat_adp.R --
  the first real-data test of the estimation reframing, directly
  comparable against those scripts' existing p-value-based results.

  WHY THIS EXISTS: the positive-control simulation found this
  project's p-value-based tests only reliably detect (>=80% of
  replicates) an effect size of ~20% of residual variance at n~12
  seasons -- far larger than any real finding this project has
  produced. Rather than keep reporting 'not significant' (which this
  battery mostly can't help but say, regardless of the true effect
  size), this reports a POINT ESTIMATE and CONFIDENCE INTERVAL for the
  RMSE and concordance metrics -- informative even when the interval
  straddles zero, since it bounds how large a real effect COULD
  plausibly be. Bust/breakout AUC is kept as p-value-based (a genuinely
  binary question), unchanged.

  Uses the EXACT SAME candidate lists and player-level data
  construction as the existing p-value batteries -- this is a
  different way of summarizing the same underlying model fits, not a
  different dataset or different candidates.

  PARAMETERS IN THIS RUN:
    SEASONS_TO_TEST  -- which target seasons get scored (2014-2025 default)
    DECAY_R          -- exponential decay rate (0.5 default, project-wide)
    LOOKBACK_YEARS    -- trailing seasons feeding the decay window (4)
    MIN_GAMES         -- games-played floor for a player-season to count (8)
    N_BOOT            -- bootstrap replicates per candidate (1000 default --
                          lower than the 2000 used in the illustrative
                          control script, a runtime tradeoff given this
                          runs across ~40 candidates, not a handful.
                          FIXED 2026-07-12: this comment has said 1000
                          default since this script's first version, but
                          the config block below silently set 500 -- a
                          doc/code mismatch that was never caught because
                          nothing diffed the two. The 2026-07-12 QB-RB-WR-TE
                          run below actually ran at 500, and WR's own
                          coverage spot-check on that run came back 0.80
                          against a nominal 0.90 -- consistent with, though
                          not proven to be caused by, running at half the
                          documented replicate count. Restored to 1000 to
                          match this comment's long-standing stated intent.
                          A fresh WR coverage spot-check at N_BOOT=1000 is
                          the discriminating test that would confirm or
                          rule out N_BOOT as the mechanism -- see Task list.)
    CONF_LEVEL        -- confidence level for intervals (0.90 default)
    SIGNIFICANCE_WINDOW -- which ADP-pick window the concordance
                          interval is reported at (12 default)
    FORCE_RECOMPUTE_BIAS -- if TRUE, ignores any cached null-bias entry
                          and recomputes fresh for every position (FALSE
                          default). Promoted into this explicit config
                          block 2026-07-12 -- it was previously read via
                          four separate scattered `if (exists(...))`
                          checks inline at each run_estimation_for_position()
                          call site, the exact implicit-global config
                          pattern Part 1 section 1.3 flags as a hazard,
                          and it was never printed in the effective-config
                          line even though it silently controls whether a
                          multi-minute bias computation runs or a cache
                          hit is used.
================================================================
\n")

if (!requireNamespace("nflreadr", quietly = TRUE)) install.packages("nflreadr", repos = "https://cloud.r-project.org")
library(dplyr)

script_arg <- grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) > 0) setwd(dirname(sub("--file=", "", script_arg)))

source("R/utils_core.R")
source("R/grading_utils.R")
source("R/fetch_player_data.R")
source("R/adp_ppg_utils.R")
source("R/beat_adp_battery.R")

# ---- CONFIG ---------------------------------------------------
if (!exists("SEASONS_TO_TEST")) SEASONS_TO_TEST <- 2014:2025
if (!exists("DECAY_R")) DECAY_R <- 0.5
if (!exists("LOOKBACK_YEARS")) LOOKBACK_YEARS <- 4
if (!exists("MIN_GAMES")) MIN_GAMES <- 8
if (!exists("N_BOOT")) N_BOOT <- 1000
if (!exists("SKIP_RMSE")) SKIP_RMSE <- TRUE
if (!exists("CONF_LEVEL")) CONF_LEVEL <- 0.90
if (!exists("SIGNIFICANCE_WINDOW")) SIGNIFICANCE_WINDOW <- 12
if (!exists("FORCE_RECOMPUTE_BIAS")) FORCE_RECOMPUTE_BIAS <- FALSE
cat("=== Config: SEASONS_TO_TEST =", paste(range(SEASONS_TO_TEST), collapse="-"),
    "| DECAY_R =", DECAY_R, "| LOOKBACK_YEARS =", LOOKBACK_YEARS, "| MIN_GAMES =", MIN_GAMES,
    "| N_BOOT =", N_BOOT, "| SKIP_RMSE =", SKIP_RMSE,
    "| CONF_LEVEL =", CONF_LEVEL, "| SIGNIFICANCE_WINDOW =", SIGNIFICANCE_WINDOW,
    "| FORCE_RECOMPUTE_BIAS =", FORCE_RECOMPUTE_BIAS, "===\n")
cat("NOTE: these persist in your R session once set. If a value above wasn't\n")
cat("intended, clear it first: rm(SEASONS_TO_TEST, DECAY_R, LOOKBACK_YEARS, MIN_GAMES, N_BOOT, SKIP_RMSE, CONF_LEVEL, SIGNIFICANCE_WINDOW, FORCE_RECOMPUTE_BIAS)\n\n")
# -----------------------------------------------------------------

adp <- load_historic_adp()
full_range <- (min(SEASONS_TO_TEST) - LOOKBACK_YEARS):max(SEASONS_TO_TEST)
message("Fetching player weekly stats for ", min(full_range), "-", max(full_range), "...")
weekly_stats_full <- nflreadr::load_player_stats(clamp_seasons(full_range), summary_level = "week")
if ("season_type" %in% names(weekly_stats_full)) weekly_stats_full <- weekly_stats_full %>% filter(season_type == "REG")
all_seasons_ppg <- compute_season_ppg(weekly_stats_full)
prior_ppg_lookup <- all_seasons_ppg %>% transmute(Season = season + 1, norm_name, position, Prior_PPG = season_ppg)

message("Building player-season talent stats...")
player_season_full <- fetch_player_season_stats(clamp_seasons(full_range), weekly_stats = weekly_stats_full)

message("Fetching red zone/end zone splits...")
player_rz_full <- fetch_player_redzone_endzone_stats(clamp_seasons(full_range))

# run_estimation_for_position() now lives in R/beat_adp_battery.R --
# moved there specifically so it's covered by test_beat_adp_battery.R,
# rather than being complex, untested orchestration logic sitting only
# in this analysis script.

# ============================================================
# QB
# ============================================================
message("Fetching dropback/rush success rate splits...")
player_success_full <- fetch_player_success_rate_stats(clamp_seasons(full_range))
message("Fetching QB designed-rush/scramble split...")
qb_rush_split_full <- fetch_qb_rush_split_stats(clamp_seasons(full_range))

qb_season_full <- player_season_full %>%
  left_join(player_rz_full, by = c("Season", "player_id", "Team")) %>%
  left_join(player_success_full, by = c("Season", "player_id", "Team")) %>%
  left_join(qb_rush_split_full, by = c("Season", "player_id", "Team")) %>%
  mutate(
    RZ_Targets = coalesce(RZ_Targets, 0), EZ_Targets = coalesce(EZ_Targets, 0),
    RZ_Rush_Att = coalesce(RZ_Rush_Att, 0), RZ_Pass_Att = coalesce(RZ_Pass_Att, 0),
    Designed_Rush_Att = coalesce(Designed_Rush_Att, 0), Scramble_Att = coalesce(Scramble_Att, 0),
    RZ_Targets_PG  = RZ_Targets / Games_Played, EZ_Targets_PG = EZ_Targets / Games_Played,
    RZ_Rush_Att_PG = RZ_Rush_Att / Games_Played, RZ_Pass_Att_PG = RZ_Pass_Att / Games_Played,
    Designed_Rush_Att_PG = Designed_Rush_Att / Games_Played,
    Scramble_Att_PG      = Scramble_Att / Games_Played
  ) %>%
  filter(Games_Played >= MIN_GAMES)

qb_candidates <- c(
  "Rush_Att_PG", "Rush_Yards_PG", "Rush_Yards", "RZ_Rush_Att_PG", "Rush_Yards_Per_Att",
  "Pass_Yards_PG", "Dropbacks_PG", "Sack_Rate", "Rush_EPA", "RZ_Pass_Att_PG", "Completion_Pct",
  "Dropback_Success_Rate", "Dropbacks", "CPOE", "Dropback_EPA", "EPA_Per_Play", "Rush_Success_Rate",
  "aDOT", "Pass_First_Downs_PG", "Designed_Rush_Att_PG", "Scramble_Att_PG"
)

message("Building QB player-level data...")
qb_data <- build_position_data("QB", adp, all_seasons_ppg, prior_ppg_lookup, qb_season_full, qb_candidates,
                                SEASONS_TO_TEST, DECAY_R, LOOKBACK_YEARS)
cat("QB rows:", nrow(qb_data), "\n")
message("Running QB estimation battery (", length(qb_candidates), " candidates)...")
qb_results <- run_estimation_for_position("QB", qb_data, qb_candidates, "Rush_Att_PG",
                                           seasons_to_test = SEASONS_TO_TEST, min_games = MIN_GAMES,
                                           decay_r = DECAY_R, lookback_years = LOOKBACK_YEARS,
                                           significance_window = SIGNIFICANCE_WINDOW, n_boot = N_BOOT,
                                           conf_level = CONF_LEVEL, skip_rmse = SKIP_RMSE,
                                           force_recompute_bias = FORCE_RECOMPUTE_BIAS)

# ============================================================
# RB
# ============================================================
message("Fetching RB situational rushing stats...")
rush_situational_full <- fetch_player_rush_situational_stats(clamp_seasons(full_range))
message("Fetching NGS rushing stats -- one season at a time...")
ngs_rushing_by_season <- list()
for (yr in clamp_seasons(full_range)) {
  ngs_rushing_by_season[[as.character(yr)]] <- tryCatch(
    fetch_player_ngs_rushing_stats(yr),
    error = function(e) { message("    NGS fetch failed for ", yr, ": ", conditionMessage(e)); NULL }
  )
}
ngs_rushing_full <- do.call(rbind, ngs_rushing_by_season[!sapply(ngs_rushing_by_season, is.null)])

rb_season_full <- player_season_full %>%
  left_join(player_rz_full, by = c("Season", "player_id", "Team")) %>%
  left_join(rush_situational_full, by = c("Season", "player_id", "Team")) %>%
  left_join(ngs_rushing_full, by = c("Season", "player_id")) %>%
  mutate(
    RZ_Targets  = coalesce(RZ_Targets, 0), RZ_Rush_Att = coalesce(RZ_Rush_Att, 0),
    RZ_Targets_PG  = RZ_Targets / Games_Played, RZ_Rush_Att_PG = RZ_Rush_Att / Games_Played
  ) %>%
  filter(Games_Played >= MIN_GAMES)

rb_candidates <- c(
  "WOPR", "Target_Share", "Rec_First_Downs_PG", "Rec_Yards_PG", "Rec_Yards", "Touches_PG",
  "Rush_Yards_PG", "Scrimmage_Yards_PG", "Avg_Time_To_LOS", "Boundary_Run_Pct",
  "RZ_Targets_PG", "Pct_Stacked_Box", "Yards_Per_Touch", "Touches", "RZ_Rush_Att_PG",
  "Receiving_aDOT", "Rush_Yards", "NGS_Efficiency", "RYOE_Per_Att", "Air_Yards_Share"
)

message("Building RB player-level data...")
rb_data <- build_position_data("RB", adp, all_seasons_ppg, prior_ppg_lookup, rb_season_full, rb_candidates,
                                SEASONS_TO_TEST, DECAY_R, LOOKBACK_YEARS)
cat("RB rows:", nrow(rb_data), "\n")
message("Running RB estimation battery (", length(rb_candidates), " candidates)...")
rb_ngs_candidates <- c("Avg_Time_To_LOS", "Pct_Stacked_Box", "NGS_Efficiency", "RYOE_Per_Att")
rb_results <- run_estimation_for_position("RB", rb_data, rb_candidates, "Rush_Yards_PG",
                                           seasons_to_test = SEASONS_TO_TEST, min_games = MIN_GAMES,
                                           decay_r = DECAY_R, lookback_years = LOOKBACK_YEARS,
                                           significance_window = SIGNIFICANCE_WINDOW, n_boot = N_BOOT,
                                           conf_level = CONF_LEVEL, skip_rmse = SKIP_RMSE,
                                           force_recompute_bias = FORCE_RECOMPUTE_BIAS,
                                           secondary_candidates = rb_ngs_candidates,
                                           secondary_representative_candidate = "Avg_Time_To_LOS")

# ============================================================
# WR / TE
#
# Merged into this SAME script rather than a separate one -- this
# project explicitly decided against parallel battery scripts after
# the build_position_data() divergence incident. WR/TE reuse the same
# generalized run_estimation_for_position() as QB/RB, just passing the
# additional secondary-bias and per-candidate-window arguments that
# QB/RB don't need (and whose defaults leave QB/RB's behavior
# completely unchanged).
# ============================================================
message("Fetching NGS receiving stats -- one season at a time...")
ngs_receiving_by_season <- list()
for (yr in clamp_seasons(full_range)) {
  ngs_receiving_by_season[[as.character(yr)]] <- tryCatch(
    fetch_player_ngs_receiving_stats(yr),
    error = function(e) { message("    NGS receiving fetch failed for ", yr, ": ", conditionMessage(e)); NULL }
  )
}
ngs_receiving_full <- do.call(rbind, ngs_receiving_by_season[!sapply(ngs_receiving_by_season, is.null)])

wr_te_season_full <- player_season_full %>%
  left_join(player_rz_full, by = c("Season", "player_id", "Team")) %>%
  left_join(ngs_receiving_full, by = c("Season", "player_id")) %>%
  mutate(
    RZ_Targets = coalesce(RZ_Targets, 0), EZ_Targets = coalesce(EZ_Targets, 0),
    RZ_Targets_PG = RZ_Targets / Games_Played, EZ_Targets_PG = EZ_Targets / Games_Played
  ) %>%
  filter(Games_Played >= MIN_GAMES)

wr_te_candidates <- c(
  "Target_Share", "Rec_Yards_PG", "Rec_First_Downs_PG", "Targets_PG", "Air_Yards_Share", "WOPR",
  "RZ_Targets_PG", "EZ_Targets_PG", "Rec_Yards", "Targets", "Yards_Per_Target", "Catch_Rate",
  "Games_Played", "Receiving_EPA", "Receiving_aDOT", "Yards_Per_Reception", "YAC_Share",
  "Avg_Separation", "Avg_Cushion", "Avg_Intended_Air_Yards", "Pct_Share_Intended_Air_Yards", "YAC_Above_Expectation"
)
# Predicted direction per WR_TE_PREREGISTRATION.md, transcribed verbatim
# from its Tier A-E tables -- "positive"/"negative" are directional
# commitments made in advance; "uncertain" (Tiers B/C/D/E) means the
# document deliberately declared no direction, so those candidates
# cannot "contradict" a prediction and are excluded from the check below.
# Games_Played is the document's own explicit non-candidate control.
wr_te_predicted_direction <- c(
  Target_Share = "positive", Targets_PG = "positive", Air_Yards_Share = "positive",
  Pct_Share_Intended_Air_Yards = "positive", WOPR = "positive", RZ_Targets_PG = "positive",
  EZ_Targets_PG = "positive", Rec_Yards_PG = "positive", Rec_First_Downs_PG = "positive",
  Rec_Yards = "positive", Targets = "positive",
  Yards_Per_Target = "uncertain", Catch_Rate = "uncertain", Receiving_EPA = "uncertain",
  Yards_Per_Reception = "uncertain",
  Receiving_aDOT = "uncertain", Avg_Intended_Air_Yards = "uncertain", Avg_Cushion = "uncertain",
  YAC_Share = "uncertain",
  Avg_Separation = "uncertain", YAC_Above_Expectation = "uncertain",
  Games_Played = "null_control"
)
# NGS-derived candidates need their OWN null bias (shorter, 9-season
# history vs. 12 for the rest) -- per Fable 5's flagged item, addressed
# here from the start rather than retrofitted.
wr_te_ngs_candidates <- c("Avg_Separation", "Avg_Cushion", "Avg_Intended_Air_Yards",
                          "Pct_Share_Intended_Air_Yards", "YAC_Above_Expectation")
# Per the WR/TE pre-registration: Tier A (opportunity) stats use window
# 6, everything else uses window 12 -- declared in advance, not chosen
# after seeing results.
wr_te_tier_a <- c("Target_Share", "Targets_PG", "Air_Yards_Share", "Pct_Share_Intended_Air_Yards",
                  "WOPR", "RZ_Targets_PG", "EZ_Targets_PG", "Rec_Yards_PG", "Rec_First_Downs_PG",
                  "Rec_Yards", "Targets")
wr_te_sig_window_fn <- function(stat) if (stat %in% wr_te_tier_a) 6 else 12

message("Building WR player-level data...")
wr_data <- build_position_data("WR", adp, all_seasons_ppg, prior_ppg_lookup, wr_te_season_full, wr_te_candidates,
                                SEASONS_TO_TEST, DECAY_R, LOOKBACK_YEARS)
cat("WR rows:", nrow(wr_data), "\n")
message("Running WR estimation battery (", length(wr_te_candidates), " candidates)...")
wr_results <- run_estimation_for_position("WR", wr_data, wr_te_candidates, "Target_Share",
                                           seasons_to_test = SEASONS_TO_TEST, min_games = MIN_GAMES,
                                           decay_r = DECAY_R, lookback_years = LOOKBACK_YEARS,
                                           n_boot = N_BOOT, conf_level = CONF_LEVEL, skip_rmse = SKIP_RMSE,
                                           force_recompute_bias = FORCE_RECOMPUTE_BIAS,
                                           secondary_candidates = wr_te_ngs_candidates,
                                           secondary_representative_candidate = "Avg_Separation",
                                           sig_window_fn = wr_te_sig_window_fn,
                                           run_coverage_spotcheck = TRUE)  # WR's large pool -- don't assume calibration transfers

message("Building TE player-level data...")
te_data <- build_position_data("TE", adp, all_seasons_ppg, prior_ppg_lookup, wr_te_season_full, wr_te_candidates,
                                SEASONS_TO_TEST, DECAY_R, LOOKBACK_YEARS)
cat("TE rows:", nrow(te_data), "\n")
message("Running TE estimation battery (", length(wr_te_candidates), " candidates)...")
te_results <- run_estimation_for_position("TE", te_data, wr_te_candidates, "Target_Share",
                                           seasons_to_test = SEASONS_TO_TEST, min_games = MIN_GAMES,
                                           decay_r = DECAY_R, lookback_years = LOOKBACK_YEARS,
                                           n_boot = N_BOOT, conf_level = CONF_LEVEL, skip_rmse = SKIP_RMSE,
                                           force_recompute_bias = FORCE_RECOMPUTE_BIAS,
                                           secondary_candidates = wr_te_ngs_candidates,
                                           secondary_representative_candidate = "Avg_Separation",
                                           sig_window_fn = wr_te_sig_window_fn)

# ============================================================
# CANDIDATE FAMILY-CORRELATION MATRIX (per position)
#
# Fixed 2026-07-12: this was a required gate item before WR/TE could be
# reported (Project_Context.txt 2.7 item 1 / WR_TE_PREREGISTRATION.md
# item 4: "Build the family-correlation matrix for WR/TE from the start
# (not retrofitted)") and was never built for ANY position, including
# QB, where Project_Context.txt already narrates the rushing-volume
# candidates as "ONE family / likely one signal" without ever having
# computed the actual correlation matrix that claim rests on. Uses
# compute_candidate_family_matrix() from R/beat_adp_battery.R (unit
# tested in test_beat_adp_battery.R), not a bespoke implementation here,
# per section 1.2.
# ============================================================
qb_family <- compute_candidate_family_matrix(qb_data, qb_candidates)
rb_family <- compute_candidate_family_matrix(rb_data, rb_candidates)
wr_family <- compute_candidate_family_matrix(wr_data, wr_te_candidates)
te_family <- compute_candidate_family_matrix(te_data, wr_te_candidates)

cat("\n=== Candidate family-correlation matrix (Spearman |r| >= 0.7 = same family) ===\n")
cat("(Grouping is by RAW STAT correlation in the player-season data, not by correlation of\n")
cat(" concordance effect sizes -- see compute_candidate_family_matrix()'s docstring for why.\n")
cat(" A family of size 1 means that candidate is not strongly correlated with any other\n")
cat(" candidate at this position -- an independent test, not a gap in the grouping.)\n\n")
for (fam_spec in list(list(pos = "QB", fam = qb_family), list(pos = "RB", fam = rb_family),
                       list(pos = "WR", fam = wr_family), list(pos = "TE", fam = te_family))) {
  fam_summary <- summarize_candidate_families(fam_spec$fam) %>% arrange(Family_Id, Stat)
  n_families <- length(unique(fam_summary$Family_Id))
  n_multi <- sum(fam_summary$Family_Size > 1)
  cat(fam_spec$pos, ": ", nrow(fam_summary), " candidates -> ", n_families, " families (",
      n_multi, " candidates share a family with at least one other candidate)\n", sep = "")
  print(fam_summary %>% filter(Family_Size > 1), row.names = FALSE)
  cat("\n")
}

# ============================================================
# COMBINED OUTPUT
# ============================================================
all_results <- rbind(qb_results, rb_results, wr_results, te_results)
all_family_summary <- rbind(
  cbind(Position = "QB", summarize_candidate_families(qb_family)),
  cbind(Position = "RB", summarize_candidate_families(rb_family)),
  cbind(Position = "WR", summarize_candidate_families(wr_family)),
  cbind(Position = "TE", summarize_candidate_families(te_family))
)
all_results <- all_results %>%
  left_join(all_family_summary %>% select(Position, Stat, Family_Id, Family_Size),
            by = c("Position", "Stat"))

# Direction-vs-prediction check (WR/TE only; NA for QB/RB, which predate
# the preregistration convention -- a separately-flagged limitation, not
# something invented by this fix). Per WR_TE_PREREGISTRATION.md rule 5.
all_results$Predicted_Direction <- ifelse(
  all_results$Position %in% c("WR", "TE"),
  wr_te_predicted_direction[all_results$Stat],
  NA_character_
)
all_results$Contradicts_Prediction <- with(all_results,
  !is.na(Predicted_Direction) & Predicted_Direction %in% c("positive", "negative") &
    !is.na(Concordance_Point_Estimate_BiasCorrected) &
    ((Predicted_Direction == "positive" & Concordance_Point_Estimate_BiasCorrected < 0) |
     (Predicted_Direction == "negative" & Concordance_Point_Estimate_BiasCorrected > 0))
)

# BH-FDR on the AUC p-values -- fixed 2026-07-12. This was a roadmap gate
# item for WR/TE (Project_Context.txt section 2.7, item 1: "AUC columns
# FDR-corrected or dropped") that shipped neither way on the actual run:
# raw Breakout_AUC_P/Bust_AUC_P sat in the output CSV uncorrected. Uses
# the SAME per-metric p.adjust(method="BH") convention already
# established in analyze_team_context_beat_adp.R (each metric's p-values
# corrected as its own family across all Position x Stat combinations),
# not a new pooling scheme invented here.
all_results$Breakout_AUC_Q <- p.adjust(all_results$Breakout_AUC_P, method = "BH")
all_results$Bust_AUC_Q <- p.adjust(all_results$Bust_AUC_P, method = "BH")

# Boundary-noise flag on the bias-corrected concordance CI -- fixed
# 2026-07-12. Section 1.7: "An interval clearing zero by <0.005 ... is
# boundary noise, not individually citable." This threshold existed only
# in prose before; the actual "notable_conc" filter below had no margin
# check at all, so e.g. WR's Targets (CI_Lower_BiasCorrected = -0.0002)
# printed in the same table, with no visual distinction, as QB's
# RZ_Rush_Att_PG (CI_Lower_BiasCorrected = +0.035) -- a 175x difference in
# how far each actually clears zero.
BOUNDARY_NOISE_MARGIN <- 0.005
all_results$CI_Margin_From_Zero_BiasCorrected <- pmin(
  abs(all_results$Concordance_CI_Lower_BiasCorrected),
  abs(all_results$Concordance_CI_Upper_BiasCorrected)
)
all_results$Excludes_Zero_BiasCorrected <- !is.na(all_results$Concordance_CI_Lower_BiasCorrected) &
  !is.na(all_results$Concordance_CI_Upper_BiasCorrected) &
  (all_results$Concordance_CI_Lower_BiasCorrected > 0 | all_results$Concordance_CI_Upper_BiasCorrected < 0)
all_results$Boundary_Noise <- all_results$Excludes_Zero_BiasCorrected &
  all_results$CI_Margin_From_Zero_BiasCorrected < BOUNDARY_NOISE_MARGIN

cat("\n=== ESTIMATION BATTERY: point estimates + ", CONF_LEVEL * 100, "% CIs, QB (", length(qb_candidates),
    ") + RB (", length(rb_candidates), ") + WR (", length(wr_te_candidates), ") + TE (", length(wr_te_candidates),
    ") candidates ===\n", sep = "")
cat("(RMSE/Concordance: point estimate + CI, informative even when the interval straddles zero --\n")
cat(" it bounds how large a real effect could plausibly be. Bust/Breakout AUC: still p-value based,\n")
cat(" a genuinely binary classification question, now with BH-FDR Q columns added (per-metric,\n")
cat(" across all", nrow(all_results), "Position x Stat combinations). N_BOOT =", N_BOOT, "replicates per candidate.\n")
cat("REMINDER: this framework excludes every rookie season (Prior_PPG filter) -- costs the least at\n")
cat(" QB and the MOST at WR, where rookie breakouts are common and fantasy-relevant.\n\n")
print(all_results, row.names = FALSE)

# Multiplicity context -- section 1.7: "Every findings table states its
# multiplicity context: 20 candidates x 90% CIs => ~2 expected false
# exclusions under a global null." This was previously stated only in
# the project's own notes, never actually computed and printed alongside
# the real table.
n_total_candidates <- nrow(all_results)
expected_false_exclusions <- n_total_candidates * (1 - CONF_LEVEL)
cat("\n=== Multiplicity context ===\n")
cat(n_total_candidates, " total candidates x ", (1 - CONF_LEVEL) * 100,
    "% expected false-exclusion rate under a global null => ~", round(expected_false_exclusions, 1),
    " expected false exclusions from chance alone, before considering boundary noise.\n", sep = "")

cat("\n=== Candidates whose BIAS-CORRECTED CONCORDANCE interval excludes zero entirely ===\n")
cat("(Bias correction confirmed via diagnose_null_bias_mechanism.R: QB and RB concordance both\n")
cat(" carry a real, position-specific, non-zero null bias that is a pure LOCATION issue, not a\n")
cat(" width/precision issue -- re-centering restored ~90% coverage for both positions. Raw\n")
cat(" values shown alongside for transparency; the corrected columns are the ones to trust.\n")
cat(" Split into ROBUST vs BOUNDARY NOISE below -- margin from zero <", BOUNDARY_NOISE_MARGIN,
    "is boundary noise per section 1.7 and is NOT individually citable, however cleanly it\n")
cat(" appears to clear zero on a first read.)\n")
notable_conc <- all_results %>% filter(Excludes_Zero_BiasCorrected)
robust_conc <- notable_conc %>% filter(!Boundary_Noise) %>% arrange(desc(abs(Concordance_Point_Estimate_BiasCorrected)))
boundary_conc <- notable_conc %>% filter(Boundary_Noise) %>% arrange(desc(abs(Concordance_Point_Estimate_BiasCorrected)))

cat("\n--- ROBUST (margin >=", BOUNDARY_NOISE_MARGIN, ") ---\n")
if (nrow(robust_conc) > 0) {
  print(robust_conc %>% select(Position, Stat, Concordance_Point_Estimate, Concordance_Point_Estimate_BiasCorrected,
                                Concordance_CI_Lower_BiasCorrected, Concordance_CI_Upper_BiasCorrected,
                                CI_Margin_From_Zero_BiasCorrected, Family_Id, Family_Size),
        row.names = FALSE)
  n_robust_families <- length(unique(paste(robust_conc$Position, robust_conc$Family_Id)))
  cat("  ->", nrow(robust_conc), "robust candidate(s) represent", n_robust_families,
      "distinct (position, family) group(s) -- do not count candidates sharing a family as independent confirmations.\n")
} else {
  cat("None.\n")
}
cat("\n--- BOUNDARY NOISE (margin <", BOUNDARY_NOISE_MARGIN, ", NOT individually citable) ---\n")
if (nrow(boundary_conc) > 0) {
  print(boundary_conc %>% select(Position, Stat, Concordance_Point_Estimate, Concordance_Point_Estimate_BiasCorrected,
                                  Concordance_CI_Lower_BiasCorrected, Concordance_CI_Upper_BiasCorrected,
                                  CI_Margin_From_Zero_BiasCorrected, Family_Id, Family_Size),
        row.names = FALSE)
} else {
  cat("None.\n")
}
if (nrow(notable_conc) == 0) {
  cat("None -- every candidate's bias-corrected concordance interval straddles zero.\n")
}

cat("\n=== WR/TE candidates whose direction CONTRADICTS the pre-registration ===\n")
cat("(WR_TE_PREREGISTRATION.md rule 5: 'including explicitly flagging any result that\n")
cat(" contradicts its predicted direction, not just the ones that confirm it.' Applies\n")
cat(" only to candidates with a committed positive/negative prediction -- Tier B/C/D/E\n")
cat(" candidates were deliberately declared 'uncertain' in advance and cannot contradict.)\n")
contradicting <- all_results %>% filter(Contradicts_Prediction)
if (nrow(contradicting) > 0) {
  print(contradicting %>% select(Position, Stat, Predicted_Direction, Concordance_Point_Estimate_BiasCorrected,
                                  Concordance_CI_Lower_BiasCorrected, Concordance_CI_Upper_BiasCorrected, Boundary_Noise),
        row.names = FALSE)
} else {
  cat("None.\n")
}

cat("\n=== AUC candidates surviving BH-FDR correction (Q < 0.10, per-metric family) ===\n")
auc_survivors <- all_results %>% filter(Breakout_AUC_Q < 0.10 | Bust_AUC_Q < 0.10)
if (nrow(auc_survivors) > 0) {
  print(auc_survivors %>% select(Position, Stat, Breakout_AUC_B, Breakout_AUC_P, Breakout_AUC_Q,
                                  Bust_AUC_B, Bust_AUC_P, Bust_AUC_Q),
        row.names = FALSE)
} else {
  cat("None -- no AUC finding survives BH-FDR correction at Q < 0.10. The raw p<0.05 hits visible\n")
  cat("in the full table above (Breakout_AUC_P / Bust_AUC_P columns) do not survive multiple-\n")
  cat("comparison correction and must not be cited as findings on their own.\n")
}

if (SKIP_RMSE) {
  cat("\n=== RMSE section skipped (SKIP_RMSE = TRUE) ===\n")
  cat("RMSE was not computed at all this run -- it is already quarantined (severely miscalibrated,\n")
  cat("coverage 0.52-0.66 against a nominal 0.90) and computing it anyway wastes roughly half the\n")
  cat("per-candidate runtime for a number nobody is allowed to cite. Set SKIP_RMSE <- FALSE before\n")
  cat("sourcing this script if RMSE numbers are needed for the record.\n")
} else {
  cat("\n=== Candidates whose RMSE interval excludes zero entirely -- QUARANTINED, DO NOT CITE ===\n")
  cat("(RMSE's bootstrap CIs were confirmed SEVERELY MISCALIBRATED -- coverage of 0.52-0.66\n")
  cat(" against a nominal 0.90, both positions -- via validate_bootstrap_coverage.R. Every RMSE\n")
  cat(" number below is shown for the record only, pending a robust reformulation (e.g. log-\n")
  cat(" ratio + BCa intervals) and independent re-validation through the same coverage harness.\n")
  cat(" None of these should be treated as findings, cited, or reported alongside concordance results.)\n")
  notable_rmse <- all_results %>% filter(
    !is.na(RMSE_CI_Lower) & !is.na(RMSE_CI_Upper) & (RMSE_CI_Lower > 0 | RMSE_CI_Upper < 0)
  )
  if (nrow(notable_rmse) > 0) {
    print(notable_rmse %>% select(Position, Stat, RMSE_Point_Estimate, RMSE_CI_Lower, RMSE_CI_Upper),
          row.names = FALSE)
  } else {
    cat("None.\n")
  }
}

dir.create("output", showWarnings = FALSE)
readr::write_csv(all_results, "output/analyze_estimation_battery_results.csv")
cat("\nFull results written to output/analyze_estimation_battery_results.csv\n")
