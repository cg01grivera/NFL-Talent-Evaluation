# CLAUDE.md — NFL-Talent-Evaluation

Project: preregistered "beat ADP" batteries testing whether player- and
team-level stats improve on `season_ppg ~ RANK + Prior_PPG` for fantasy
drafting. Current effort: the Team-Context Rework — see
`TEAM_CONTEXT_REWORK_PLAN_V2.md` (what to do) and
`TEAM_CONTEXT_PREREGISTRATION_V2.md` (committed predictions and rules).
For execution sequencing and gate details, use the
`team-context-execution` skill.

## Non-negotiable invariants (violating these is worse than failing a task)

1. **Shared library only.** All data assembly goes through
   `build_position_data()` in `R/beat_adp_battery.R`; all null-bias work
   through the `estimate_concordance_null_bias*` family. Extend shared
   functions with backward-compatible parameters — NEVER write a parallel
   local copy in an analysis script. Parallel assembly caused this project's
   worst incident.
2. **Test-first for library changes.** Any change to `R/beat_adp_battery.R`
   ships with tests in `test_beat_adp_battery.R` in the same change; the full
   suite must pass before any real-data run. Tests use small `bias_n_*`
   values and a temp `cache_path` — never the production cache.
3. **Cache hygiene.** `output/concordance_null_bias_cache.csv` rows are never
   deleted or overwritten. New pools/modes get new position labels
   (`RB_teamctx`, `WR_matched`, etc.). Never reuse a bias across pools,
   positions, or permutation modes — measured signs already differ by pool.
4. **Preregistration is append-only.** Never edit a committed prediction,
   direction, window, or mechanism in any `*_PREREGISTRATION*.md`. Machinery
   changes become dated amendment sections. Results contradicting
   predictions are reported as prominently as confirmations.
5. **No goal-seeking.** The objective is an explained result, not a positive
   one. Every machinery change must be justified by a diagnostic that
   predicted it in advance. Do not iterate until a preferred sign appears.
6. **Outputs are immutable history.** Never delete or overwrite files in
   `output/`; new runs write new files. Exploratory outputs carry an
   `EXPLORATORY_` prefix and are never cited as confirmation.
7. **Explicit config blocks.** Every analysis script prints every parameter
   (the `analyze_estimation_battery.R` convention). `N_BOOT = 1000`. No
   implicit globals.
8. **Temporal integrity.** All history joins go through
   `decay_weighted_avg_vec()` (guard: `years_back >= 0`,
   `R/grading_utils.R`). The poisoned-future-row test must stay green; any
   new join path needs its own leakage test before real data.
9. **pbp column rule.** Never assume an nflreadr column exists or means what
   its name suggests: verify with a live `names()` pull and a `table()` of
   values before first use (convention documented in
   `R/fetch_team_context_data.R`).
10. **Stop at gates.** The plan's execution order has explicit gates. Stop
    and report at each; do not run past a gate unattended.

## Reference repos

The legacy parent project (`nfl_team_grades`) is READ-ONLY REFERENCE. Every
function worth having was already ported into `R/beat_adp_battery.R` and
validated there. Do not re-port, copy, or "sync" code from it — re-porting
creates divergent duplicates of validated logic.

## Commands

- Run tests: `Rscript test_beat_adp_battery.R` (must be fully green before
  any real-data run)
- Batteries assume `nflreadr` with network access on first fetch; cached
  loads thereafter.

## Key files

- `R/beat_adp_battery.R` — shared library (assembly, concordance, bias,
  family matrix, direct reads). The only place battery logic lives.
- `R/fetch_team_context_data.R` — team-stat construction from pbp.
- `R/grading_utils.R` — decay weighting + temporal guard.
- `analyze_estimation_battery.R` — config-block and reporting conventions.
- `WR_TE_PREREGISTRATION.md` — committed player-battery prereg (append-only).
- `TEAM_CONTEXT_REWORK_PLAN_V2.md` / `TEAM_CONTEXT_PREREGISTRATION_V2.md` —
  current effort.
