# Team-Context Beat-ADP Pre-Registration V2

*STATUS: COMMITTED (Gate 5, 2026-07-13). Supersedes the v1 draft (never
committed, so this is a replacement, not an amendment). Rules 5, 8, 9, and
14 were RESOLVED during Gates 3–4 (2026-07-13) — see each rule below.
Rule 9's V4 predictive-stability check FAILED for every surviving
component of the originally-proposed `Team_OL_Composite`; per the plan's
own declared response to that outcome, this was NOT resolved unilaterally
— the user was presented the finding and explicitly chose to retire the
composite and test its three V3-surviving components as separate
confirmatory candidates instead (see Rule 9 and the Confirmatory
Predictions amendment below). All placeholders were resolved and the user
reviewed and approved this document for commit on 2026-07-13. Per Rule 13,
it is now **append-only**: nothing above this line, and no prediction,
direction, window, or mechanism below, may be edited going forward —
machinery changes become new, dated amendment sections. Confirmation
standard: (a) bias-corrected 90% CI excludes zero at the declared window by
≥ 0.005, (b) predicted direction, (c) era-split replication, (d) no
unresolved method-disagreement flag. Contradictions are reported as
prominently as confirmations.*

---

## Restart & Gate-Resolution Declaration

Phase 1 team-context results remain void (pre-estimand-fix machinery,
underpowered tests) and carry zero prior weight. The WR/TE anomaly gate has
resolved as: no reliable receiving-volume mispricing in either direction;
flagship stat a clean null on direct reads; neither artifact (H1) nor market
mechanism (H2) confirmed. Accordingly: no team-level usage-distribution
candidate appears in this confirmatory tier, and no result here will be
interpreted through either unconfirmed mechanism. Factual record: pace stats
(`Team_PROE`, `Team_Raw_Plays_PG`, `Team_Avg_Plays_Per_Drive`) passed Phase
1's persistence screen and were never validly rejected; they are assigned to
the exploratory tier as direction-uncertain.

## Global Rules

1. **Baseline:** `season_ppg ~ RANK + Prior_PPG` vs. same + candidate;
   LOSO; concordance bootstrap point estimate + 90% CI; AUC bust/breakout
   p-values with BH-FDR; RMSE skipped (known-miscalibrated).
2. **Window 12, all candidates.** Team-context values are broadcast — shared
   by teammates — so they are environment quality, not player-tie-breaking
   usage; the window-6 theory does not apply. No window-6 readings reported.
3. **Fresh null biases** per pool: `QB_teamctx`, `RB_teamctx`, `WR_teamctx`,
   `TE_teamctx`. No transfer across pools or modes. Per-pool coverage
   spot-check at N_BOOT = 1000; all "excludes zero" claims carry the measured
   empirical coverage (WR player-pool precedent: ~87% vs 90% nominal).
4. **Assembly:** shared `build_position_data()` with `extra_key_col = "Team"`;
   full test suite green first, including the poisoned-future-row temporal
   test. If that test cannot pass, the battery does not run and candidate C8
   (`Team_RZ_Trip_Rate`, renumbered from C5 after the Gate 4 `Team_OL_
   Composite` retirement) is dropped outright.
5. **Permutation unit — RESOLVED (Gate 3, 2026-07-13): `permute_within =
   "team"`.** Decided by a pre-declared zero-effect broadcast-candidate
   synthetic (test_beat_adp_battery.R, "GATE 3" section) comparing
   player-level vs. team-level permutation-null calibration, before real
   data. The synthetic required its own injected team-level outcome
   component (shared game script / team quality) to be informative at
   all — a first attempt without one showed IDENTICAL calibration for
   both modes, since a linear regression cannot distinguish "these 5
   rows share a value because they're teammates" from "these 5 rows
   share a value by coincidence" unless something else in the model is
   also team-clustered, exactly as real outcomes are. Result: player-mode
   coverage landed BELOW nominal (0.875 vs. 0.90, 35/40) — the
   anti-conservative direction, reproducing this project's known WR
   player-pool ~87%-vs-90% precedent — while team-mode coverage landed
   AT/ABOVE nominal (0.975, 39/40) and was not flagged by the project's
   own one-sided binomial coverage test (p=0.985 vs. p<0.10 threshold).
   Team-mode's null-bias magnitude was also ~20x larger than player-mode's
   (-0.00192 vs. -0.00009), confirming the two modes measure genuinely
   different things on a team-clustered candidate rather than silently
   aliasing each other. `BIAS_PERMUTE_WITHIN = "team"` is printed in the
   config and named in every results table going forward.
6. **Clustering caveat:** ~32 team values per season wear ~300 player-row
   name tags, and teammates share outcomes. All uncertainty statements carry
   this; a season-block-bootstrap sensitivity run on C1 is a required
   robustness output.
7. **Family + boundary reporting from the first run:**
   `compute_candidate_family_matrix()` (|r| ≥ 0.7), `Family_Id`/`Family_Size`
   per row, boundary-noise margin 0.005.
8. **Persistence gate — RESOLVED (Gate 4, 2026-07-13): all six new stats
   pass.** Decay-weighted year-over-year self-correlation at r=0.5 (the
   project's standard decay rate — the gate DECISION is read at this fixed
   r, not a grid-searched "best r", to avoid r-cherry-picking), 2012–2025,
   N=416 team-season pairs per stat: `Team_Shotgun_Rate` 0.607,
   `Team_QB_Hit_Rate_Allowed` 0.516, `Team_Sack_Rate_Allowed` 0.380,
   `Team_Goal_To_Go_Pass_Rate` 0.354, `Team_Goal_To_Go_Run_Rate` 0.354,
   `Team_Rush_Stuff_Rate` 0.349 — all clear the ≥ 0.30 gate. For context
   (not a new stat, but relevant to Rule 9 below): the existing, Phase-1-
   verified `Team_OL_Penalty_Rate` does NOT clear this gate (0.172).
   Full results: `output/analyze_team_context_decay_rate_results.csv`
   (`analyze_team_context_decay_rate.R`).
9. **OL composite membership — RESOLVED (Gate 4, 2026-07-13): composite
   retired, three components tested separately.** Starting components =
   {Sack_Rate_Allowed, QB_Hit_Rate_Allowed, Rush_Stuff_Rate, OL_Penalty_Rate},
   each subject to the pre-declared drops: persistence < 0.30 (Rule 8) or
   convergent correlation < 0.2 with the composite-of-the-others.
   **V3 result:** `Team_OL_Penalty_Rate` DROPPED (r = 0.147 vs. the
   composite-of-the-others, below the 0.2 threshold — consistent with its
   own Rule 8 persistence failure and the plan's own "weakest signal"
   caveat); the other three all clear (Sack_Rate_Allowed 0.489,
   QB_Hit_Rate_Allowed 0.506, Rush_Stuff_Rate 0.240). **Final membership:
   {Sack_Rate_Allowed, QB_Hit_Rate_Allowed, Rush_Stuff_Rate}.** Composite =
   unweighted mean of per-season z-scores of these three, oriented higher =
   better line. No outcome-fitted weights, ever. **V2 result:** this
   3-component composite clears its own persistence gate (r=0.5 correlation
   0.458, ≥ 0.30 PASS). **V4 result (FAILS):** every surviving component's
   own year-over-year persistence (Sack_Rate_Allowed 0.375,
   QB_Hit_Rate_Allowed 0.504, Rush_Stuff_Rate 0.344) beats the composite's
   ability to predict that SAME component's next-year value (0.362, 0.418,
   0.210 respectively) — the opposite of the plan's stated expectation that
   averaging four proxies would net out one-year contamination noise. Per
   the plan's own declared response to this outcome ("the weighting is
   reconsidered before the battery, never after"), this was NOT resolved
   unilaterally: the finding was presented to the user, who chose (2026-07-13)
   to **retire the composite entirely** and test the three surviving
   components as **separate confirmatory candidates**, each restricted to
   the positions its already-declared mechanism applies to, rather than
   force a new weighting scheme post hoc or cross every component with
   every position. See the Confirmatory Predictions section's amendment
   below for the resulting candidate list (C1–C7). Contamination memo (QB
   behavior in sack/hit rates; RB and box counts in stuff rate) is part of
   this document, unchanged, and now applies to each component individually
   per Rule 11.
   V5b (report-only, resolved): the sack component's QB contamination was
   checked against FTN charting's `is_qb_fault_sack` flag on the 2022–2024
   subsample (2025 FTN charting not yet released as of this run — a
   3-season, not 4-season, subsample). QB-fault share of team sacks: mean
   43.5%, SD 17.1%, range [7.7%, 81.2%] — substantial team-to-team spread.
   Raw-vs-QB-fault-excluded `Team_Sack_Rate_Allowed` correlation: r = 0.738
   (n = 96 team-seasons) — moderate (comparable to this project's own 0.7
   "strong correlation" family-matrix convention, but well short of
   near-1.0). Per the plan's qualitative rule (no numeric threshold is
   pre-declared for this specific check), the contamination caveat is
   ELEVATED in all reporting, and a sensitivity composite substituting the
   QB-fault-excluded sack rate (2022+ subsample only) becomes a required
   robustness output for later gates. This does not change composite
   membership (fixed by V2–V3 alone). FTN's 3-season history remains
   validation-only, never battery input.
   Full results: `output/analyze_ol_composite_v3_convergent_validity.csv`,
   `output/analyze_ol_composite_v4_predictive_stability.csv`,
   `output/diagnose_ol_sack_ftn_validation_results.csv`.
10. **Direct signal reads emitted for every candidate** (fold coefficients +
    per-season Spearman). A sign conflict between concordance and direct
    reads raises a method-disagreement flag; flagged candidates cannot be
    reported as supported in either direction until reconciled.
11. **Attribution honesty:** a confirmed `Team_Rush_Stuff_Rate`,
    `Team_Sack_Rate_Allowed`, or `Team_QB_Hit_Rate_Allowed` effect (C1–C7) is
    a team-environment effect *consistent with* the OL mechanism, not proof
    of OL causation — each shares variance with QB quality and scheme (the
    contamination memo in Rule 9 is exactly this: sack/hit rates carry QB
    time-to-throw and scramble tendency; stuff rate carries RB vision and
    box counts). This caveat applies to each of the three components
    individually now that they are separate candidates, not pooled into one
    composite.
12. **Exploratory firewall:** the 2014–2019 exploratory screen produces no
    confirmatory language; survivors graduate only via a new committed prereg
    entry before the 2020–2025 confirmation era is touched. Exploratory
    outputs are prefixed `EXPLORATORY_` and are never cited as support in the
    confirmatory report.
13. **Append-only:** once committed, predictions here are never edited.
    Machinery changes become dated amendments.
14. **Coach-continuity persistence test (Stage 0) is a measurement study —
    COMPLETE (Gate 4, 2026-07-13).** Using head-coach data from
    `load_schedules()` (primary HC per team-season = most-started coach
    that season; 2012–2025, 416 team-season transitions: 316 continuity,
    100 change), year-over-year persistence of every team stat was measured
    split by HC-continuity vs HC-change seasons. **Result: scheme-family
    stats show a much larger persistence drop on coach change (mean 0.293)
    than roster-family stats (mean 0.136)** — e.g. `Team_Shotgun_Rate` drops
    from r=0.720 (continuity) to r=0.236 (change), the single largest drop
    of all 28 stats tested, while `Team_Rush_Stuff_Rate` barely moves
    (0.343 → 0.314). This matches the plan's first pre-declared reading:
    scheme-stat persistence collapses on coach change while OL/roster stats
    largely hold. Per this rule's own declared scope, this produces NO
    beat-ADP claims and modifies no prediction in this document; its use is
    limited to (a) a caveat on exploratory scheme-stat results (`Team_PROE`,
    `Team_Raw_Plays_PG`, `Team_Shotgun_Rate`, `Team_Goal_To_Go_Pass_Rate`)
    — team-keyed histories for these carry real coach-change measurement
    error — and (b) empirical justification for a *future-work*
    coach-keying effort, which remains explicitly OUT OF SCOPE for this
    plan (no playcaller tables or coach-keyed candidates are built here).
    HC-change is a noisy proxy for playcaller-change; that noise attenuates
    the measured gap and cannot fabricate one, so this large observed gap
    is trustworthy. Full results:
    `output/analyze_hc_continuity_persistence_results.csv`
    (`analyze_hc_continuity_persistence.R`).
15. **Exploratory additions (R1):** `Team_Shotgun_Rate` and
    `Team_Goal_To_Go_Pass_Rate` join the exploratory tier as
    direction-uncertain. `Team_Goal_To_Go_Run_Rate` is constructed for the C8
    mechanism-consistency check only.

## Confirmatory Predictions (8 combinations)

### AMENDMENT (Gate 4, 2026-07-13): `Team_OL_Composite` retired, replaced by
its 3 surviving raw components tested separately

The single `Team_OL_Composite` candidate (C1–C4 in the original draft) is
**retired**: its V4 predictive-stability check failed (see Rule 9) — every
surviving component's own year-over-year persistence beat the composite's
ability to predict that same component's future value, the opposite of
the averaging benefit the composite was built to provide. Rather than
resolve this unilaterally, the user chose (2026-07-13) to test the three
V3-surviving components **separately**, each at the positions its
already-declared mechanism actually applies to — not a full 3-component ×
4-position cross (12 combinations), which would have needed new,
un-pre-declared mechanisms for combinations like Rush_Stuff_Rate × WR.

**Sign convention, stated explicitly to prevent a future sign error:** all
three components are **lower = better** in their raw units (a higher sack
rate, hit rate, or stuff rate means a WORSE line). Unlike the retired
composite (which was oriented via `-1 * zscore()` to read higher = better),
these candidates enter the battery as their raw, unoriented values. Every
direction below is therefore declared **Negative** on the raw candidate
value — i.e., the predicted relationship is that a HIGHER raw stat value
predicts LOWER `season_ppg`/worse concordance direction, not the reverse.

### C1: `Team_Rush_Stuff_Rate` (RB only — the run-blocking mechanism)

| # | Candidate | Pos | Direction | One-line mechanism |
|---|---|---|---|---|
| C1 | `Team_Rush_Stuff_Rate` | RB | **Negative** — lead hypothesis, expected strongest | A higher stuffed-rush rate directly measures the line failing to create room; run blocking converts touches into yards, and the line is the largest environmental input to RB efficiency. |

### C2–C4: `Team_Sack_Rate_Allowed` (the protection mechanism, at QB/WR/TE)

| # | Candidate | Pos | Direction | One-line mechanism |
|---|---|---|---|---|
| C2 | `Team_Sack_Rate_Allowed` | QB | **Negative** | Pass protection buys time and reduces sacks — a higher sack rate directly measures worse protection, the declared team-level mechanism behind the QB battery's unexplained `Sack_Rate` singleton; a null here weakens that singleton. |
| C3 | `Team_Sack_Rate_Allowed` | WR | **Negative** | Protection enables longer-developing downfield routes; a higher sack rate signals less time for routes to develop. |
| C4 | `Team_Sack_Rate_Allowed` | TE | **Negative** — declared weakest/most contested | Blocking-snap displacement: worse protection (higher sack rate) frees the TE from pass-protection duty into routes in one reading, but can also force hot-read/checkdown TE targets in another. **Pre-declared counter-mechanism:** a robust POSITIVE relationship at TE (more sacks allowed, MORE TE production) is interpreted as checkdown dominance — that interpretation is committed now, not constructed later. |

### C5–C7: `Team_QB_Hit_Rate_Allowed` (same protection mechanism, less
outcome-dependent than sacks specifically)

| # | Candidate | Pos | Direction | One-line mechanism |
|---|---|---|---|---|
| C5 | `Team_QB_Hit_Rate_Allowed` | QB | **Negative** | Same protection mechanism as C2, using the hit-rate measure (less outcome-dependent than sacks — a hit doesn't require the QB to actually go down). |
| C6 | `Team_QB_Hit_Rate_Allowed` | WR | **Negative** | Same mechanism as C3. |
| C7 | `Team_QB_Hit_Rate_Allowed` | TE | **Negative** — declared weakest/most contested | Same counter-mechanism as C4: a robust POSITIVE relationship here is interpreted as checkdown dominance. |

Declared expected ordering of effect sizes: within each component family
(Sack_Rate_Allowed: C2/C3/C4; QB_Hit_Rate_Allowed: C5/C6/C7), QB ≥ WR ≥ TE.
`Team_Rush_Stuff_Rate` (C1) has no within-family ordering (RB-only) but
remains the single declared "expected strongest" candidate overall. A
confirmed pattern violating either within-family ordering is reported as a
partial contradiction even if individual signs match.

### C8: `Team_RZ_Trip_Rate`

| # | Candidate | Pos | Direction | One-line mechanism |
|---|---|---|---|---|
| C8 | `Team_RZ_Trip_Rate` | RB | **Positive** | More red-zone trips → more goal-line carries, the highest-leverage RB opportunity; team-level analogue of the confirmed QB `RZ_Rush_Att_PG` mechanism. Defect #1 lineage: admitted only under Rule 4's poisoned-row test passing; dropped, not repaired, if it fails. |

**C8 mechanism-consistency check (pre-declared, non-confirmatory):** C8's
effect is decomposed by trailing `Team_Goal_To_Go_Run_Rate` median split.
The mechanism predicts the effect concentrates in the run-heavy half; a
confirmed C8 living in the pass-heavy half is reported as
mechanism-inconsistent even with a matching headline sign. This check can
only qualify C8, never upgrade it.

## Explicit Exclusions From the Confirmatory Tier

1. **`Team_WR/TE/RB_Target_Share`** — no affirmatively supported mechanism in
   either direction after the anomaly investigation (flagship player-level
   stat: clean null; H1 and H2 both unconfirmed). Eligible for the
   exploratory tier as direction-uncertain only.
2. **Pace/script stats** (`Team_PROE`, `Team_Raw_Plays_PG`,
   `Team_Avg_Plays_Per_Drive`, snap-share stats) — genuinely two-sided
   mechanisms; exploratory tier as direction-uncertain. Never validly
   rejected by Phase 1; not carried as "known null."
3. **`Team_OL_Continuity`** (snap-count-based returning-starter share) —
   promising, genuinely distinct information; deferred as a gated second
   wave requiring its own data verification and prereg entry. Not in wave one.
4. **All remaining Phase 1 stats** — exploratory tier or nothing; Phase 1
   results void either way.
5. **YAC-adjacent team metrics** — standing caution from the unresolved
   `YAC_Share` sign disagreement: any future YAC-family team candidate must
   resolve its method-disagreement flag before any directional claim.

Adding any confirmatory candidate after commit requires a new, separately
committed entry before that candidate touches real data.

## What Happens Next (gates in order)

1. Library extension + tests green (assembly key, permutation mode, direct
   reads, poisoned-row).
2. Live pbp column verification; three new OL stats plus
   `Team_Shotgun_Rate`, `Team_Goal_To_Go_Pass_Rate`, and
   `Team_Goal_To_Go_Run_Rate` added to the fetch layer.
3. Permutation-unit synthetic → resolve Rule 5.
4. Persistence screen (with the Rule 14 HC-continuity split riding along) →
   resolve Rule 8; OL validation ladder (convergence, stability) → resolve
   Rule 9; V5b FTN validation subsample (report-only).
5. **DONE (2026-07-13):** Commit this document with placeholders resolved.
   Nothing after this step may modify it.
6. Fresh null biases + coverage spot-checks (4 pools).
7. Confirmatory battery (C1–C8) with direct reads + block-bootstrap
   sensitivity on C1.
8. Exploratory screen (2014–2019); graduation decisions; confirmation era
   only after new committed entries.
