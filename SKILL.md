---
name: team-context-execution
description: Execute the Team-Context Rework (V2) for the NFL-Talent-Evaluation project — the staged, gated workflow in TEAM_CONTEXT_REWORK_PLAN_V2.md. Use this skill whenever the task involves team-context candidates (Team_OL_Composite, Team_RZ_Trip_Rate, OL components, shotgun/goal-to-go stats), extending build_position_data with extra_key_col, team-level permutation nulls, the OL validation ladder, the coach-continuity persistence split, the exploratory 2014-2019 screen, or ANY run of the beat-ADP battery on team-level (broadcast) stats — even if the user doesn't name the skill. Also use it before interpreting any team-context results.
---

# Team-Context Rework — Execution

You are executing a preregistered analysis plan inside the
NFL-Talent-Evaluation repo. The plan is `TEAM_CONTEXT_REWORK_PLAN_V2.md`; the
committed rules and predictions are `TEAM_CONTEXT_PREREGISTRATION_V2.md`.
**Read both from disk before doing anything.** Where this skill and those
documents disagree, the documents win. Where the prereg and convenience
disagree, the prereg wins. Repo-wide invariants live in `CLAUDE.md` and apply
to every step below without restatement — shared library only, test-first,
append-only prereg, immutable outputs, cache hygiene, no goal-seeking.

## Operating mode

Work one gate at a time. At each gate: produce the deliverable, report
pass/fail against the pre-declared criterion, and **stop for user sign-off**
before the next gate. Never run past a gate unattended, never touch real
outcome data before Gate 5 (prereg commit), and never emit the words
"significant" or "confirmed" for anything in the exploratory tier.

## Gate sequence

### Gate 1 — Library extensions + tests (no real data)
Extend `R/beat_adp_battery.R` (backward-compatible parameters, never new
parallel functions):
1. `build_position_data(..., extra_key_col = "norm_name")` — team-context
   callers pass `"Team"`; decay lookup uses `merged$Team`; join back by
   `norm_name`. Default must reproduce existing player-battery output
   byte-for-byte.
2. Bias estimator `permute_within = c("player", "team")` — team mode shuffles
   the team→value mapping within season and re-broadcasts (preserves the
   clustered tie structure of broadcast candidates).
3. Direct-read emission wired into the battery path: fold-coefficient
   summaries + `compute_candidate_signal_read()` per candidate, plus a
   method-disagreement flag (concordance sign vs direct-read sign conflict).

Required new tests in `test_beat_adp_battery.R` (temp cache path, tiny
bias_n_* params): default-regression (byte-identical), team key routing
(same-team players share values; different teams don't), team-switcher
(player gets *target-season* team's trailing history), **poisoned-future-row**
(inject `Season == target_year` team row with an extreme sentinel; output
must be unchanged — if this cannot pass, STOP: the battery does not run and
candidate C5 is dropped, not patched), and permutation-mode routing.
**Gate criterion: full suite green.**

### Gate 2 — Stat construction (fetch layer)
Verify live pbp columns first (`names()` + `table()`), then add to
`fetch_team_context_stats()`: `Team_Sack_Rate_Allowed`,
`Team_QB_Hit_Rate_Allowed`, `Team_Rush_Stuff_Rate` (designed rushes only —
exclude `qb_scramble`), `Team_Shotgun_Rate`, `Team_Goal_To_Go_Pass_Rate`,
`Team_Goal_To_Go_Run_Rate`. Follow the file's existing single-season,
verified-column conventions. Gate criterion: columns verified + construction
spot-checked against a hand-computed team-season.

### Gate 3 — Permutation-unit synthetic
Pre-declared synthetic (reuse the D3 synthetic infrastructure from the
anomaly work): zero-effect *broadcast* candidates; compare calibration of
player-level vs team-level permutation nulls. Resolve `BIAS_PERMUTE_WITHIN`
on the synthetic result, record it in the prereg placeholder (Rule 5), print
it in every config block. Gate criterion: one mode demonstrably calibrated on
broadcast structure; decision written down before real data.

### Gate 4 — Screens and validation ladders (stat-level only, no outcomes)
Run together (shared decay machinery):
- Persistence screen, gate ≥ 0.30, for every new stat → resolves prereg
  Rule 8. Failures reported as screened out, never quietly swapped.
- **HC-continuity split (Stage 0, prereg Rule 14):** persistence of every
  team stat split by HC-continuity vs HC-change seasons (HC from
  `load_schedules()`), scheme family vs roster family. Measurement study:
  report-only, no beat-ADP claims, cannot modify predictions. All follow-on
  coach-keying work is OUT OF SCOPE — do not build playcaller tables or
  coach-keyed candidates even if results look inviting; report and stop.
- OL ladder V2–V4: persistence per component; convergent validity (drop rule:
  correlation < 0.2 vs composite-of-the-others); predictive stability.
  Resolves composite membership (prereg Rule 9). Composite = unweighted mean
  of per-season z-scores, higher = better line. Never fit weights to outcomes.
- V5b (report-only): FTN 2022+ `is_qb_fault_sack` — QB-fault share and
  raw-vs-adjusted sack-rate correlation. Cannot change composite membership;
  low correlation elevates the caveat and requires a sensitivity composite
  later.

Gate criterion: all three prereg placeholders (Rules 5, 8, 9) resolved with
evidence.

### Gate 5 — Prereg commit (hard stop)
Present the finalized `TEAM_CONTEXT_PREREGISTRATION_V2.md` (placeholders
resolved, nothing else changed) to the user for commit. **Do not proceed on
your own authority.** After commit the document is append-only.

### Gate 6 — Null biases
Fresh biases for `QB_teamctx`, `RB_teamctx`, `WR_teamctx`, `TE_teamctx`
(5 × 100 replicates, resolved permutation mode, N_BOOT=1000 in the battery,
BIAS_N_BOOT=500 inside the estimator) + per-pool coverage spot-checks. No
transfer across pools or modes. Gate criterion: four cached entries under
`_teamctx` labels + coverage numbers reported (quote them wherever "excludes
zero" appears later).

### Gate 7 — Confirmatory battery (C1–C5 only)
`run_estimation_for_position()` with explicit parameters; emit family matrix,
boundary-noise margin 0.005, direct reads + disagreement flags, the C5
goal-to-go-run-rate median-split consistency check, and the season-block
bootstrap sensitivity on C1. Read strictly against the prereg: report
contradictions, ordering violations (declared: RB ≥ QB ≥ WR ≥ TE), and
flagged disagreements as prominently as confirmations. Max claim without era
replication: "directionally supported, unconfirmed."

### Gate 8 — Exploratory screen
2014–2019 only. Same machinery. Output = ranked effect sizes by family,
`EXPLORATORY_` prefix, zero confirmatory language. Graduation to the
2020–2025 confirmation era happens only via new user-committed prereg
entries — present candidates and stop.

## Reporting conventions
Every results table names: permutation mode, bias value + cache label,
empirical coverage, family id, boundary-noise status, direct-read signs +
disagreement flag. Rookie-exclusion and clustering caveats (~32 team values
per season wearing ~300 player-row name tags) appear in every team-context
results summary.

## Definition of done (whole effort)
All gates passed in order with written pass/fail reads; prereg committed at
Gate 5 with a clean placeholder-resolution diff; confirmatory results
document reads against every prereg rule including the ones that fired
against us; exploratory report firewalled; a short
`TEAM_CONTEXT_RESULTS_SUMMARY.md` stating, per candidate: prediction, result,
status (confirmed / directionally supported / null / contradicted /
flagged), and consequences.
