# Team-Context Rework — Project Plan V2

*Supersedes `TEAM_CONTEXT_REWORK_PLAN.md` (v1). Revised after (a) the WR/TE
anomaly closeout (`WR_TE_ANOMALY_FINAL_SUMMARY`), (b) reinstatement of WR/TE
as tested positions with declared OL mechanisms, (c) elevation of the OL
component to a fully validated construct, and (d) addition of an explicitly
labeled exploratory tier. Companion: `TEAM_CONTEXT_PREREGISTRATION_V2.md`
(draft — commit before the confirmatory battery runs).*

---

**Revision R1 (2026-07-13).** Four additions from the playcaller-tendencies
review (Heath, Fantasy Points, 2026-07-09), approved by the user: (a)
`Team_Shotgun_Rate` and `Team_Goal_To_Go_Pass_Rate` join the exploratory tier
(Section 5); (b) a pre-declared mechanism-consistency check on C5 (Section 2;
renumbered C8 after the Gate 4 `Team_OL_Composite` retirement — see Section 2);
(c) an FTN-based QB-fault-sack validation subsample in the OL ladder (Section
3.2, V5b); (d) the coach-continuity persistence test (Section 5b) — **Stage 0
only**. The follow-on staging discussed in planning (playcaller-table
curation, franchise-vs-coach forecasting horse race, coach-keyed exploratory
candidates) is deliberately **out of scope for this plan** and held for future
work; nothing in this document depends on it.

## 0. What Changed From V1 and Why

1. **Anomaly gate resolved.** The WR/TE investigation found no reliable
   "receiving volume is mispriced" effect in either direction: the flagship
   stat (`Target_Share`) is a clean null on direct reads; neither the
   statistical-artifact hypothesis (H1) nor the market-overreaction hypothesis
   (H2) was confirmed. Consequences here:
   - The standing "re-examine team-context if shared machinery is implicated"
     caveat is **retired** (machinery was largely vindicated).
   - The "re-measure OL nulls under a matched null if H1 confirms" insurance
     clause is **retired** (H1 failed its own synthetic test).
   - `Team_*_Target_Share` remains excluded from the confirmatory tier — but
     the reason is now "no affirmatively supported mechanism in either
     direction," not "pending resolution." They are eligible for the
     exploratory tier as direction-uncertain.
2. **WR and TE are reinstated as tested positions**, each with a declared OL
   mechanism (Section 2). V1's blanket WR/TE exclusion conflated an
   evidence-based exclusion (target-share stats) with a scope choice
   (positions); V2 separates them.
3. **OL quality is promoted from "one proxy stat" to a validated construct**
   (Section 3) — a four-component panel with a declared validation ladder,
   entering the battery as one composite per position. "Thoroughly
   investigate the OL component" is this section.
4. **Exploratory tier added** (Section 5): the wide net, run honestly —
   discovery on one era, confirmation on the other, no confirmation language
   inside the screen. Pace stats live here (see the factual note below).
5. **Direct signal reads are first-class outputs** (Section 6): the
   fold-coefficient and per-season Spearman reads built during the anomaly
   investigation ride alongside concordance for every candidate, with an
   automatic method-disagreement flag (the `YAC_Share` lesson).
6. **Correction carried forward:** the anomaly closeout doc says "5 of 22"
   candidates contradicted; the committed 2026-07-12 CSV shows 15
   contradiction flags (10 WR + 5 TE). Not material to its conclusions;
   recorded so the smaller number doesn't become the remembered one.

**Factual note on pace stats (asked and answered from the repo record):**
`Team_PROE`, `Team_Raw_Plays_PG`, and `Team_Avg_Plays_Per_Drive` were never
validly rejected. They *passed* the Phase 1 persistence screen (only field
position, the two weekly-variance stats, and turnover margin were cut) and
were tested in the void Phase 1 battery. Status: never validly answered.
They are excluded from the confirmatory tier for having genuinely two-sided
mechanisms, and are included in the exploratory tier.

---

## 1. Two-Tier Structure

- **Confirmatory tier** (Section 2): five preregistered stat × position
  combinations. Committed directions, mechanisms, and window before the run.
  Only this tier can produce "directionally supported" or "confirmed"
  language.
- **Exploratory tier** (Section 5): broad screen, split-sample design,
  explicitly labeled, cannot produce confirmation claims on its own.
  Survivors graduate by earning a preregistration entry for the held-out era
  *before* that era is touched.

The multiplicity logic, stated once: with ~a decade of seasons and ~32 teams,
a 76-combination screen will produce lucky-looking hits by chance. Narrow
preregistration makes a hit meaningful; the exploratory tier gets breadth
without corrupting that, because discovery and proof never come from the same
data.

---

## 2. Confirmatory Candidate List (8 combinations)

**AMENDMENT (Gate 4, 2026-07-13):** `Team_OL_Composite` is retired. Its V4
predictive-stability check (Section 3.2) failed — every V3-surviving
component's own year-over-year persistence beat the composite's ability to
predict that same component's future value, the opposite of the averaging
benefit the composite was meant to provide. Rather than force a new
weighting scheme post hoc, the user chose to test the three V3-surviving
components **separately**, each only at the positions its already-declared
mechanism applies to (not a full 3-component × 4-position cross, which
would need new, un-pre-declared mechanisms). `Team_OL_Penalty_Rate` was
already dropped at V3 (Section 3.2) and is not a candidate in any form.

**Sign convention:** all three components are **lower = better** in raw
units (a higher sack/hit/stuff rate means a WORSE line) — unlike the
retired composite, which was oriented via `-1 * zscore()`. Every direction
below is declared on the RAW, unoriented candidate value.

| # | Candidate | Pos | Direction | One-line mechanism |
|---|---|---|---|---|
| 1 | `Team_Rush_Stuff_Rate` | RB | **Negative** (lead) | A higher stuffed-rush rate directly measures the line failing to create room; run blocking converts touches into yards, and the line is the largest environmental input to RB efficiency. |
| 2 | `Team_Sack_Rate_Allowed` | QB | **Negative** | Pass protection buys time and reduces sacks — a higher sack rate directly measures worse protection, the declared team-level mechanism behind the QB battery's unexplained `Sack_Rate` singleton. |
| 3 | `Team_Sack_Rate_Allowed` | WR | **Negative** | Protection enables longer-developing downfield routes; a higher sack rate signals less time for routes to develop. |
| 4 | `Team_Sack_Rate_Allowed` | TE | **Negative** | Worse protection (higher sack rate) could free the TE from pass-protection duty into routes, OR force hot-read checkdowns that *raise* TE targets. **Counter-mechanism declared in advance:** a robust POSITIVE relationship here (more sacks, more TE production) is pre-interpreted as the checkdown effect dominating, not as a new post-hoc story. |
| 5 | `Team_QB_Hit_Rate_Allowed` | QB | **Negative** | Same protection mechanism as #2, using the hit-rate measure (less outcome-dependent than sacks specifically). |
| 6 | `Team_QB_Hit_Rate_Allowed` | WR | **Negative** | Same mechanism as #3. |
| 7 | `Team_QB_Hit_Rate_Allowed` | TE | **Negative** | Same counter-mechanism as #4. |
| 8 | `Team_RZ_Trip_Rate` | RB | **Positive** | More red-zone trips → more goal-line carries; team-level analogue of the confirmed QB `RZ_Rush_Att_PG` mechanism. Defect #1 lineage flagged; admitted only under the poisoned-future-row test passing. |

**C8 mechanism-consistency check (pre-declared, non-confirmatory):** C8's
mechanism runs through goal-line *carries*, but red-zone trips convert to QB
value on pass-heavy-inside-the-10 teams and RB value on run-heavy ones (the
playcaller review's O'Connell observation). So alongside C8 the battery emits
a decomposition: does C8's effect concentrate among teams with above-median
trailing `Team_Goal_To_Go_Run_Rate` (pbp-derivable, full history)? A confirmed
C8 whose effect lives in the *pass-heavy* half is reported as
mechanism-inconsistent even if the headline sign matches. This check cannot
upgrade C8 — only qualify it.

Expected relative ordering, declared now: `Team_Rush_Stuff_Rate` (#1) is the
single strongest-expected candidate overall (RB-only, no within-family
ordering needed). Within each of the other two component families
(`Team_Sack_Rate_Allowed`: #2–#4; `Team_QB_Hit_Rate_Allowed`: #5–#7), QB ≥
WR ≥ TE, weakest/most contested at TE. Confirmation standard unchanged: (a)
bias-corrected 90% CI excludes zero at window 12 by ≥ 0.005, (b) predicted
direction, (c) era-split replication — plus (d, new) no unresolved sign
disagreement with the direct reads (Section 6).

---

## 3. The OL Construct — Validation Ladder (the "thorough" part)

**The measurement problem, stated plainly:** no public play-by-play feed
measures OL quality directly. Every derivable proxy is contaminated by
someone else's play: sacks are partly the QB holding the ball; stuffed runs
are partly the back and the box count; penalties are cleanly attributable but
a faint signal. So OL quality is treated as a latent construct measured by a
panel of imperfect witnesses, validated before it is allowed anywhere near
the battery.

### 3.1 Component panel (all per team-season, pbp-derived, columns verified live before coding)

| Component | Definition | Orientation | Known contamination (declared) |
|---|---|---|---|
| `Team_Sack_Rate_Allowed` | sacks / dropbacks (dropback = pass play or sack, same denominator as `Team_EPA_Per_Dropback`) | lower = better | QB time-to-throw and scramble tendency |
| `Team_QB_Hit_Rate_Allowed` | `qb_hit` / dropbacks | lower = better | Less outcome-dependent than sacks, still partly QB behavior |
| `Team_Rush_Stuff_Rate` | share of designed rushes (`play_type == "run"`, not `qb_scramble`) with `yards_gained <= 0` | lower = better | RB vision, defensive box counts, down-and-distance mix |
| `Team_OL_Penalty_Rate` | OL penalties / offensive plays (existing, Phase-1-verified) | lower = better | Cleanest attribution, weakest signal |

**Second wave (gated, not wave one):** `Team_OL_Continuity` — share of OL
snaps taken by prior-season starters, from `nflreadr::load_snap_counts()`
(PFR, 2012+). Genuinely distinct information (roster stability vs. play
outcomes) but a heavier build with its own data-quality verification; enters
only after the composite battery runs, with its own prereg entry.
Deliberately out: FTN charting (2022+, history too short), NGS time-to-throw
adjustment of sack rate (optional refinement, noted).

### 3.2 Validation gates, in order — each pass/fail, declared before data

- **V1 Column verification:** live `names(load_pbp())` + `table()` on
  `qb_hit`, `qb_scramble`, `yards_gained`, `play_type` — the fetch-file
  standing rule.
- **V2 Persistence:** each component and the composite must clear the
  decay-weighted year-over-year self-correlation gate **≥ 0.30** (extend
  `analyze_team_context_decay_rate.R` by the new stats). A component failing
  persistence is dropped from the composite and reported as screened out.
- **V3 Convergent validity:** the four components should positively
  intercorrelate if they measure one construct (after orienting all to
  higher = better). Declared rule: a component with correlation **< 0.2**
  against the composite-of-the-others is measuring something else and is
  dropped from the composite (reported, not hidden). Full matrix reported via
  `compute_candidate_collinearity()` / `compute_candidate_family_matrix()`.
- **V4 Predictive stability:** composite in year t should predict its own
  components in year t+1 at least as well as any single component does —
  the aggregation is supposed to average out one-year contamination noise; if
  a single component beats the composite on stability, the weighting is
  reconsidered *before* the battery, never after.
- **V5 Contamination memo:** the table above, committed as-is in the prereg,
  so nobody discovers the QB-contamination caveat only after an inconvenient
  result.
- **V5b QB-fault validation subsample (new, from FTN charting):**
  `nflreadr::load_ftn_charting()` (2022+) includes an `is_qb_fault_sack`
  flag — a direct empirical handle on the sack component's declared
  contamination. Deliverables, report-only: (i) the QB-fault share of team
  sacks and its team-to-team spread; (ii) the correlation between raw
  `Team_Sack_Rate_Allowed` and a QB-fault-excluded version across 2022–2025
  team-seasons. High correlation → the long-history raw proxy is validated;
  low correlation → the contamination caveat is elevated in all reporting and
  a sensitivity composite substituting the adjusted rate (2022+ subsample
  only) becomes a required robustness output. This gate cannot change
  composite membership (that is Rules V2–V3's job); it changes how much the
  sack component's contribution is trusted in interpretation. The 4-season
  FTN history is far too short for the battery itself — validation use only.

**Composite definition (fixed in advance, RETIRED Gate 4 2026-07-13):** per
season, z-score each surviving component across teams, orient to higher =
better, take the unweighted mean. No data-driven weights — weights fitted
to outcomes would smuggle the answer into the question. This construction
was correct as declared, but failed V4 (predictive stability) in practice —
see Section 2's amendment. The three V3-surviving components are tested
separately in the confirmatory tier instead.

### 3.3 Attribution honesty in interpretation

A confirmed `Team_Rush_Stuff_Rate`, `Team_Sack_Rate_Allowed`, or
`Team_QB_Hit_Rate_Allowed` effect (C1–C7, Section 2) is a *team-environment*
effect consistent with the OL mechanism — not proof the offensive line
specifically caused it (each component's contamination correlates with QB
quality and scheme, per the contamination memo in Section 3.1). The prereg
says this now, so the claim can't inflate later; this caveat applies to
each of the three components individually now that they are separate
candidates, not pooled into one composite.

---

## 4. Statistical Adjustments for Broadcast (Team-Level) Candidates

Carried from V1, now firmed by the anomaly work:

1. **Assembly:** shared `build_position_data()` extended with
   `extra_key_col = "Team"` (backward-compatible default `"norm_name"`);
   per-team decay weighting through yr−1, broadcast to that season's roster.
   No parallel assembly. Tests: default-regression, key routing,
   team-switcher, poisoned-future-row (temporal guard made explicit for team
   joins).
2. **Clustering reality:** ~300 player-rows per season carry only ~32
   distinct team values, and teammates share fates (QB, injuries, script).
   Effective sample is far smaller than row count; all uncertainty language
   carries this caveat.
3. **Null permutation unit — resolved by test, not argument:** permute
   team→value mappings within season and re-broadcast, preserving the
   clustered tie structure the real candidate has. Decided via a pre-declared
   synthetic (zero-effect broadcast candidates; the D3 synthetic
   infrastructure from the anomaly work is reused) comparing player-level vs
   team-level permutation calibration — before real data. `BIAS_PERMUTE_WITHIN`
   printed in the config either way.
4. **Fresh null biases** per pool: labels `RB_teamctx`, `QB_teamctx`,
   `WR_teamctx`, `TE_teamctx` (four now, not two). No transfer across pools
   or modes. Coverage spot-check per pool at N_BOOT = 1000; the known ~87%
   empirical coverage at WR (vs 90% nominal) is quoted wherever "excludes
   zero" is claimed.
5. **Pair-resampling caveat:** bootstrap CIs resample pairs; pairs sharing a
   team are correlated for broadcast candidates, so CIs are anti-conservative
   in an amount the coverage spot-checks partially measure. A
   season-block-bootstrap sensitivity run on the lead candidate
   (`Team_Rush_Stuff_Rate` × RB, C1 — `Team_OL_Composite` is retired,
   Section 2) is included as a robustness output.

---

## 5. Exploratory Tier (the wide net, run honestly)

- **Membership:** all 19 Phase-1 team stats (including `Team_PROE`,
  `Team_Raw_Plays_PG`, `Team_Avg_Plays_Per_Drive`, and the three
  `Team_*_Target_Share` stats as direction-uncertain), the OL components
  individually, and two article-derived scheme stats — `Team_Shotgun_Rate`
  and `Team_Goal_To_Go_Pass_Rate` (both plain-pbp-derivable with full
  history, direction-uncertain, columns verified live before construction) —
  × {QB, RB, WR, TE}.
- **Design:** screen era **2014–2019** only. Same machinery (shared assembly,
  team-permuted nulls, family matrix, direct reads, boundary margin). Output
  is ranked effect sizes by family — no "significant," no "confirmed," no
  directional claims beyond the numbers.
- **Graduation rule:** any screen survivor the user wants to pursue gets a
  preregistration entry (direction = the screen's direction, mechanism
  written at graduation time) committed **before** the confirmation era
  **2020–2025** is touched. Confirmation-era results are then read under the
  full confirmatory standard.
- **Power expectation, stated in advance:** halving the data means only large
  effects replicate; a screen hit failing confirmation is the expected
  outcome for small effects and is not evidence of anything mishandled.
- **Firewall:** exploratory outputs live in separate files with an
  `EXPLORATORY_` prefix; the confirmatory report never cites them as support.

---

## 5b. Coach-Continuity Persistence Test (Stage 0 — the only staging item in scope)

**Question:** are team stats franchise properties or coach properties? Our
decay-weighting assumes trailing *team* history carries forward, which is
plausible for roster stats (the OL components) and questionable for scheme
stats (pass rate, PROE, pace, shotgun rate) — scheme plausibly travels with
the playcaller, and roughly a third to half of teams change playcallers in a
given cycle.

**Design (costs nothing beyond data we already pull):** head coach per
team-game is in `nflreadr::load_schedules()` (`home_coach`/`away_coach`) for
the full window. Build an HC-continuity flag per team-season (same primary HC
as prior season, yes/no; HC is an imperfect proxy for playcaller — declared).
Then, for each team stat in this project, compute year-over-year persistence
**separately for continuity vs. change seasons** and report the persistence
drop per stat, scheme family vs. roster family.

**Pre-declared reads:**
- Scheme-stat persistence collapses on coach change while OL/roster stats
  hold → team-keyed scheme histories carry coach-change measurement error;
  this becomes (i) a declared caveat on all exploratory scheme-stat results
  and (ii) the empirical justification for the *future-work* coach-keying
  effort — which remains outside this plan.
- Persistence holds through coach changes → the franchise/roster/QB dominates
  scheme; team-keying is validated as-is and the coach-keying idea is
  deprioritized without further investment.
- Either way this is a **measurement study**: it produces no beat-ADP claims,
  cannot modify the committed confirmatory predictions, and its output is a
  persistence-by-continuity table riding alongside the Section 3.2 / Rule 8
  persistence screens (same decay machinery, one extra split).

**Misclassification note:** HC-change is a noisy stand-in for
playcaller-change (playcalling OCs churn under stable HCs, and some HCs call
plays). That noise attenuates the measured persistence gap toward zero — it
can hide a real coach effect but cannot fabricate one — so a large observed
gap is trustworthy while a null is only suggestive. Stated in the output.

## 6. Direct Signal Reads as First-Class Outputs (YAC_Share lesson)

For every candidate in both tiers, the battery emits alongside concordance:
fold-coefficient summaries (`run_concordance(..., return_fold_coefs = TRUE)` +
`summarize_fold_coefficients()`) and the per-season Spearman read
(`compute_candidate_signal_read()`), all already fixture-tested in
`R/beat_adp_battery.R`. A **method-disagreement flag** fires when the
concordance sign and the direct-read sign conflict with both nominally
non-trivial; a flagged candidate cannot be reported as supported in either
direction until reconciled. The next `YAC_Share` gets caught in the same run,
not two phases later.

---

## 7. Config Block (printed, every parameter)

```
SEASONS_TO_TEST         2014:2025 (confirmatory) | 2014:2019 (exploratory screen)
DECAY_R                 0.5
LOOKBACK_YEARS          4
MIN_GAMES               8
N_BOOT                  1000
CONF_LEVEL              0.90
SIGNIFICANCE_WINDOW     12
BOUNDARY_NOISE_MARGIN   0.005
BIAS_PERMUTE_WITHIN     "team"     # RESOLVED via Section 4.3 synthetic, 2026-07-13 (Gate 3)
BIAS_META_REPLICATES    5
BIAS_N_REPLICATES       100
BIAS_N_BOOT             500
FORCE_RECOMPUTE_BIAS    FALSE
CACHE_PATH              output/concordance_null_bias_cache.csv
POSITION_LABELS         QB_teamctx, RB_teamctx, WR_teamctx, TE_teamctx
OL_COMPONENTS           SackRate, QBHitRate, RushStuffRate, OLPenaltyRate  # V3 result: OLPenaltyRate DROPPED (Gate 4)
OL_COMPOSITE            RETIRED (Gate 4, 2026-07-13) -- V4 predictive-stability failure; the 3
                        V3-survivors are tested as SEPARATE confirmatory candidates (Section 2)
OL_CONVERGENCE_MIN_R    0.2
OL_FTN_VALIDATION       TRUE       # V5b, 2022-2024 subsample (2025 not yet released), report-only -- RESOLVED
PERSISTENCE_GATE        0.30       # RESOLVED (Gate 4): all 6 new stats pass
PERSISTENCE_HC_SPLIT    TRUE       # Section 5b Stage 0, from load_schedules coaches -- RESOLVED (Gate 4)
EXPLORATORY_ADDITIONS   Team_Shotgun_Rate, Team_Goal_To_Go_Pass_Rate
C8_CONSISTENCY_CHECK    Team_Goal_To_Go_Run_Rate median split
EMIT_DIRECT_READS       TRUE
```

---

## 8. Execution Order (gates unchanged in spirit from V1)

1. Library: `extra_key_col`, team-level permutation mode, direct-read
   emission wiring + all tests → full `test_beat_adp_battery.R` pass. **Gate.**
2. Live pbp column verification (V1); add `Team_QB_Hit_Rate_Allowed`,
   `Team_Rush_Stuff_Rate`, `Team_Sack_Rate_Allowed`, `Team_Shotgun_Rate`,
   `Team_Goal_To_Go_Pass_Rate`, `Team_Goal_To_Go_Run_Rate` to
   `fetch_team_context_stats()`.
3. Permutation-unit synthetic (Section 4.3) → resolve `BIAS_PERMUTE_WITHIN`.
   **Gate.**
4. OL validation ladder V2–V4 (fix composite membership) + V5b FTN
   validation subsample + Section 5b coach-continuity persistence split
   (same decay machinery as the persistence screens). **Gate** (V2–V4 only;
   V5b and 5b are report-only).
5. Finalize + commit `TEAM_CONTEXT_PREREGISTRATION_V2.md`. **Gate — nothing
   after this step edits it.**
6. Fresh null biases (4 pools) + coverage spot-checks.
7. Confirmatory battery (8 combinations, post Gate-4 composite retirement)
   with direct reads and season-block-bootstrap sensitivity on the lead
   candidate.
8. Exploratory screen (2014–2019); ranked report; graduation decisions with
   the user.
9. Read strictly against prereg; contradictions and method-disagreement flags
   reported as prominently as confirmations.

## 9. Standing Caveats

Rookie exclusion (`Prior_PPG`), target-season `primary_team` assignment
(documented approximation + sensitivity on confirmed findings), ~87% empirical
coverage qualifier, clustering caveat on all broadcast-candidate uncertainty,
and the two open anomaly residues (`YAC_Share` sign disagreement — now also a
watch-item for any YAC-adjacent team metric; `Avg_Intended_Air_Yards` single-
rung signal) remain open items, low priority, not blockers.
