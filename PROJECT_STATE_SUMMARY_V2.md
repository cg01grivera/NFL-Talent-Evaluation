# NFL Fantasy Talent Evaluation — V2 Package Summary

*This is the updated restart document for the V2 package. Read this alone to know where the project stands. It supersedes the original PROJECT_STATE_SUMMARY.md, which covered the project up to the discovery of the bias-instability incident; this version covers everything since, through the WR/TE build.*

---

## 1. What Changed Since the Last Summary

The last summary left off with a confirmed but not-yet-closed bias-instability incident in the QB bias measurement. Since then:

1. **The incident was fully resolved.** Root cause (via Fable 5's direct code review of the actual files): `estimate_concordance_null_bias()` didn't filter missing values before permuting, so it measured the null on a randomly-varying player pool instead of the same fixed pool the real candidate estimates use. Fixed in the shared library, confirmed via a real re-run (QB bias landed at 0.01228, squarely inside the previously-validated range).
2. **A structural fix, not just a patch**: `build_position_data()` was consolidated into the shared library (`R/beat_adp_battery.R`) after two scripts were found to have silently diverging local copies — the actual cause of the incident. There is now exactly one implementation, used by every position.
3. **The bias estimator was made more robust**: a single 100-replicate bias measurement was shown to occasionally land several standard errors from the true value by chance (confirmed via a dedicated stability check). The standard procedure now averages 5 independent 100-replicate measurements, and results are cached (keyed on both config *and* a data fingerprint, so a changed pool can never silently reuse a stale cache entry).
4. **`run_concordance()`'s pairwise comparison was vectorized** for speed — replacing a slow per-pair loop with matrix operations. Verified via an independently-written reference implementation to produce identical results, not just asserted.
5. **Progress reporting** (percent complete, elapsed time, ETA) was added to every long-running loop in the pipeline.
6. **The RB battery was found to have the same missingness-matching gap** as the original QB incident, for its NGS-derived candidates (`Avg_Time_To_LOS`, `Pct_Stacked_Box`, `NGS_Efficiency`, `RYOE_Per_Att`) — these were sharing a bias measured from a full-history candidate rather than their own, shorter-history-matched one. Fixed the same way.
7. **WR and TE were built out fully**, reusing 100% of the now-validated infrastructure rather than retrofitting it after the fact:
   - Persistence screen expanded from 13 to 22 candidates each (added 4 already-computed-but-untested stats, plus 5 new NGS receiving stats — `Avg_Separation`, `Avg_Cushion`, `Avg_Intended_Air_Yards`, `Pct_Share_Intended_Air_Yards`, `YAC_Above_Expectation`).
   - **A formal pre-registration was written before any WR/TE battery was run** (`WR_TE_PREREGISTRATION.md`) — predicted direction and one-line mechanism for every candidate, declared significance windows, and an explicit "what counts as confirmed" standard. This is the first position in the project where this was done in advance rather than retrofitted.
   - WR/TE were integrated into the **same single battery script** as QB/RB (`analyze_estimation_battery.R`) rather than a separate parallel script — this project explicitly decided against maintaining parallel, similar-but-not-identical battery scripts after the divergence incident, and a second script was built once and then corrected back into the single-script pattern.
8. **`run_estimation_for_position()`** — the function that runs a full position's battery — was moved into the shared library and converted from ~9 implicit global-variable dependencies to explicit parameters, and given its own test coverage for the first time (it had grown real branching logic — which candidate gets which bias, which significance window applies to which stat — with zero prior tests).

---

## 2. Current Validated Infrastructure

### Shared library: `R/beat_adp_battery.R`
The single source of truth for all Beat-ADP testing, now including:
- The original three tests (`run_rmse_cv`, `run_auc_cv`, `run_concordance` — the last now vectorized) and the p-value summary wrapper.
- The bootstrap estimation layer (`run_full_battery_estimation` and its components) — used instead of p-values, since this project's own sample sizes were shown to be badly underpowered for realistic effect sizes.
- The bias-correction layer (`estimate_concordance_null_bias`, `estimate_concordance_null_bias_robust`, `get_or_compute_concordance_null_bias`, `apply_concordance_bias_correction`) — required because concordance carries a real, position-specific, non-zero null bias even after the estimand itself was corrected.
- `build_position_data()` — the single, shared player-level data assembly function used by every position.
- `run_estimation_for_position()` — the single, shared per-position battery-running loop, generalized to support secondary (shorter-history) bias groups and per-candidate significance windows.
- `print_progress()` — shared progress-reporting helper.

### Companion test suite: `test_beat_adp_battery.R`
**Must be run after any change to the shared library, before trusting it for real analysis.** Covers: basic battery mechanics, bootstrap estimation, the concordance estimand fix (regression-tested so it can never silently reappear), the bias-correction layer, the robust/cached bias estimator, the vectorized pairwise construction (reconciled against an independent reference), and — as of this version — `run_estimation_for_position()`'s bias- and window-routing logic specifically.

### Main analysis scripts
- `clean_fantasypros_adp.R` → `apply_team_correction_to_adp_file.R` → `diagnose_uncorrected_team_rows.R`: the ADP data-cleaning pipeline. Run in this order from a raw FantasyPros export to produce a fully team-corrected historic ADP file.
- `analyze_player_talent_decay_rate.R`: the persistence screen, all four positions, 111 total candidate stats.
- `analyze_estimation_battery.R`: **the main battery script — QB, RB, WR, and TE, all in one script**, using the fully validated, bias-corrected estimation layer.
- `analyze_team_context_decay_rate.R` / `analyze_team_context_beat_adp.R`: team-context persistence and battery scripts. **Not yet updated to use the current shared library** — the team-context "confirmed null" conclusion predates the estimand fix and the estimation layer, and should be treated as unconfirmed with valid instruments, not settled, until re-run through the current infrastructure.

---

## 3. Results Status — What's Confirmed vs. Pending

| Position | Status |
|---|---|
| QB | Confirmed, bias-corrected. Core finding: red-zone/general rushing volume, likely one signal across several correlated stats (family-correlation matrix requested repeatedly, still not built). `Sack_Rate` is an unexplained singleton needing a declared mechanism before being trusted. |
| RB | **Needs re-running.** The NGS-candidate bias-matching fix (Section 1, item 6) was applied after the last real RB battery run — `NGS_Efficiency`'s status as a finding is not yet confirmed under the corrected bias. |
| WR | **Never yet run against real data.** Persistence screen complete (see below); the Beat-ADP battery itself has not been executed. |
| TE | **Never yet run against real data.** Same status as WR. |
| Team Context | Existing "confirmed null" conclusion is stale (pre-dates current infrastructure) but not yet re-tested. |

### WR/TE Persistence Screen — Already-Observed Highlights (real data, already run)
- `WOPR` failed to beat its own components (`Target_Share`, `Air_Yards_Share`) at both positions — consistent with the pre-registration's low-confidence prediction for composites.
- `Receiving_aDOT` / `Avg_Intended_Air_Yards` were the strongest, most robust results in the whole screen (0.68–0.74 at both positions) — a genuine surprise, since the pre-registration declared these direction-uncertain in advance.
- `Avg_Separation` (WR: 0.66) broke the pattern established by RB's `RYOE_Per_Att` (~0.30) — the first evidence in this project that an isolated-skill measure can be genuinely sticky.

**None of this has yet been carried through the actual Beat-ADP battery** — persistence is a cheap first filter, not a substitute for the real test.

---

## 4. Immediate Next Steps, in Priority Order

1. **Run `test_beat_adp_battery.R`** — confirm the current library state passes before anything else.
2. **Run `analyze_estimation_battery.R`** — this now covers QB, RB, WR, and TE in one execution. Expect this to be the longest-running script in the project (four positions, several bias measurements each, a WR-specific coverage spot-check, plus the full candidate battery for all four).
3. **Read WR/TE results against `WR_TE_PREREGISTRATION.md` explicitly** — including any result that contradicts its predicted direction, not just the ones that confirm it.
4. **Build the QB family-correlation matrix** — requested multiple times, never yet built. Determines whether QB's "core finding" is one signal or several restated.
5. **Run the QB 2012–2025 de-confounding test** — isolates whether the earlier QB fragility (found when both the ADP source and the season window changed simultaneously) was a real instability or an artifact of the bugs since fixed.
6. **Re-run team context** through the current infrastructure — not relitigating a settled question, since the original conclusion was never tested with valid instruments in the first place.

---

## 5. What Was Deliberately Left Out of This Package

Several one-off diagnostic scripts and historical review documents from earlier in the project are not included here, since they already served their purpose (confirming specific bugs, now fixed and incorporated into the library) and aren't needed to run the pipeline going forward: `check_bias_estimate_stability.R`, `check_null_concordance_centering.R`, `diagnose_null_bias_mechanism.R`, `validate_bootstrap_coverage.R`, `validate_battery_controls.R`, `validate_team_context_pipeline.R`, the superseded original p-value-based QB/RB battery scripts, and the round-by-round Fable 5 correspondence documents. These remain available from earlier in the project's history if needed for reference.

---

## 6. Setup Instructions

1. Open `nfl_talent_eval.Rproj` in RStudio — this sets the working directory correctly for every script's relative paths (`R/`, `data/`, `output/`).
2. Place a raw FantasyPros historic ADP export at `data/FantasyProsHistoricADP.csv` if starting the ADP pipeline from scratch, or place an already-corrected `historic_adp_team_corrected.csv` directly in `data/` if you have one from a prior session.
3. Run `test_beat_adp_battery.R` before anything else, every time.
4. Proceed per Section 4 above.
