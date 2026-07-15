# WR/TE Opportunity-Direction Anomaly — Resolution Plan

*Status: plan only. Companion skill for the implementing model:
`wr-te-anomaly-diagnosis` (install instructions at the end of this document).
This plan supersedes the "exclude and caveat" treatment in
`TEAM_CONTEXT_REWORK_PLAN.md` Section 1 — the team-context target-share
candidates stay gated on this resolution, but the resolution itself is now a
scheduled work item, not a standing caveat.*

---

## 1. Restating the Anomaly Against the Actual Evidence

The working description — "~15 of 22 opportunity receiving stats point
opposite their preregistered direction, consistent across two runs, at both
positions" — is what the `Contradicts_Prediction` column shows. Reading the
full result table (`output/analyze_estimation_battery_results.csv`) shows the
underlying phenomenon is different, in three verifiable ways:

**1a. The WR effect is position-wide, not opportunity-specific.**
19 of 22 WR raw concordance point estimates are negative — including Tier
C/D/E stats declared *uncertain* in the preregistration (`Receiving_aDOT`
−0.0176, `YAC_Share` −0.0149, `Avg_Intended_Air_Yards` −0.0176,
`Avg_Separation` −0.0079). Only `Catch_Rate` (+0.0102) and
`YAC_Above_Expectation` (+0.0073) are positive. Uncertain-declared stats
cannot trip `Contradicts_Prediction`, so the flag concentrated on the
opportunity tier and made a WR-wide shift look like an opportunity-specific
reversal. Within the shift there is a gradient: family 1 — the target/volume
stats most collinear with `Prior_PPG` — sits deepest negative.

**1b. TE does not share the WR pattern.**
TE opportunity stats are mostly positive after correction (`EZ_Targets_PG`
+0.072, `Rec_Yards` +0.047, `Rec_Yards_PG` +0.034, `RZ_Targets_PG` +0.021,
`Air_Yards_Share` +0.009). TE's five contradictions decompose as: three stats
whose raw estimates are **exactly 0.0000** (`Target_Share`, `Targets_PG`,
`WOPR` — identical to four decimals), rendered negative purely by subtracting
TE's +0.0038 null bias; plus `Targets` (−0.025 raw) and
`Pct_Share_Intended_Air_Yards` (−0.039 raw, the one non-boundary
zero-excluder). TE estimates are also visibly quantized (all values are
multiples of ~0.006, i.e., ~1/164 window-12 pairs from a 175-row pool) —
small-pool granularity, not necessarily a bug, but the exact-zero triple needs
a column-identity check.

**1c. The null-bias machinery is structurally blind to collinearity.**
`estimate_concordance_null_bias()` permutes the candidate within season. That
preserves the candidate's marginal distribution but **destroys its correlation
with `RANK` and `Prior_PPG`**. The null therefore measures the degradation
caused by adding *orthogonal* noise to the model. A real WR receiving stat is
strongly collinear with `Prior_PPG` (receiving volume largely *is* WR fantasy
scoring); collinear regressors inflate coefficient variance and degrade
out-of-fold predictions **more** than orthogonal noise of the same marginal
distribution. The bias correction subtracts the orthogonal-noise degradation
(WR: −0.0033) but not the collinearity surplus — pushing every collinear
candidate's corrected estimate negative. This one mechanism predicts 1a's
position-wide shift, the family-1 gradient, the near-zero behavior of the
least-PPG-collinear stats (`Catch_Rate`, `YAC_Above_Expectation`,
`Avg_Cushion`), and TE's milder pattern (smaller pool, different margin
structure). The `Games_Played` control does not refute it: `Games_Played` has
weak collinearity with `Prior_PPG`, so the orthogonal null is approximately
correct *for the control specifically* (WR corrected +0.0010 ≈ 0, exactly as a
well-behaved control should look under this hypothesis).

---

## 2. Hypothesis Ladder (declared before diagnostics run)

| # | Hypothesis | What it predicts | Status if confirmed |
|---|---|---|---|
| **H1 (lead)** | Collinearity-blind null: permutation destroys candidate↔baseline correlation, under-measuring degradation for collinear candidates | Negative magnitude scales with candidate's multiple correlation with (RANK, Prior_PPG); effect vanishes under a collinearity-matched null | Machinery defect. Fix null, re-run, conclusion likely "opportunity is already priced in" (a null, not a negative) |
| **H2** | Real signal: ADP fully-or-over-prices trailing receiving volume at WR (market overreaction → mean reversion) | Effect **survives** a collinearity-matched null; per-fold fitted coefficients on the candidate are consistently negative; raw per-season Spearman(candidate, residual) is negative | Genuine finding with a declarable mechanism — flips future usage-stat priors to negative/uncertain, including team-context target shares |
| **H3** | Bootstrap miscalibration at WR (the 0.80-vs-0.90 coverage spot-check at N_BOOT=500, flagged in the script header) | Coverage recovers at N_BOOT=1000; CIs widen but point estimates barely move | Widened intervals; does not by itself explain a directional shift — health gate, not the explanation |
| **H4** | Data-level defect: identical/duplicated candidate columns after decay-weighting or join (TE's exact-zero triple; WR pairs sharing identical 4-dp values) | Column-identity audit finds duplicated vectors | Fix join/construction, re-run affected candidates |

H1 and H2 are not mutually exclusive — the matched null can shrink the effect
without zeroing it. The diagnostics below are ordered to separate them
cleanly.

---

## 3. Diagnostic Ladder (each step has a pass/fail read, cheapest first)

**D0 — Machinery health gate.** Re-run the WR coverage spot-check at
N_BOOT=1000 (the discriminating test already named in
`analyze_estimation_battery.R`'s header). Nothing downstream is interpreted
until coverage is known. *Read:* coverage ≈ 0.90 → H3 closed as "N_BOOT=500
artifact," proceed; still ~0.80 → calibration investigation runs first.

**D1 — Column-identity audit (H4, minutes of compute).** After
`build_position_data()` assembly, for WR and TE: check every pair of the 22
candidate columns for exact or near-exact vector identity (post decay-weight,
post join); check per-candidate variance and NA structure. *Read:* TE
`Target_Share`/`Targets_PG`/`WOPR` genuinely distinct vectors → exact-zero
triple attributed to small-pool quantization; identical vectors → construction
bug, fix before anything else.

**D2 — Collinearity gradient test (the H1 signature).** For each of the 22 WR
candidates, compute R² of `candidate ~ RANK + Prior_PPG` on the assembled
data, then regress the 22 raw concordance point estimates against those R²
values. *Read:* strong negative slope (family-1 stats high-R² and most
negative, `Catch_Rate`/`YAC_Above_Expectation` low-R² and non-negative) → H1's
signature confirmed observationally; flat → H1 weakened, H2 promoted.

**D3 — Collinearity-matched null (the discriminating instrument).** Extend the
null generator with a residual-permutation mode: fit `candidate ~ RANK +
Prior_PPG` within season, permute the **residuals** within season, add back
the fitted component. The null candidate then carries the real candidate's
exact collinearity with the baseline but zero incremental information —
measuring precisely the degradation the current null misses. Validate on
synthetic data first (true incremental effect = 0, candidate correlation with
Prior_PPG swept across 0/0.5/0.8): matched null must center corrected
estimates at zero across the sweep **where the current null shows a
correlation-dependent negative gradient**. Then measure matched nulls for WR
family 1's representative (`Target_Share`) and one low-collinearity candidate.
*Read:* family-1 corrected estimates re-center at ~0 under the matched null →
H1 confirmed, effect is artifact; still materially negative with CI excluding
zero → H2 has survived its designed refutation.

**D4 — Direct signal reads (H2 corroboration, machinery-free).** (a)
Distribution of the candidate's fitted coefficient across LOSO folds —
consistently negative coefficients mean the model actively learns
mean-reversion, not that noise degrades it. (b) Per-season
Spearman(decay-weighted candidate, residual from `season_ppg ~ RANK +
Prior_PPG`) — a raw-data read with no bootstrap, no bias correction, no
concordance machinery. *Read:* both negative and stable across seasons → H2
real; coefficients centered at zero → degradation is variance, not signal.

**D5 — Staleness check (secondary, only if H2 survives D3/D4).** Replace the
4-year decay-weighted candidate with the pure year-t−1 value for family 1.
*Read:* negative effect shrinks materially → part of the effect is stale-role
contamination (decay window reaching back past role changes), a construction
choice rather than a market-mispricing finding — report the decomposition.

**D6 — Era split (replication standard).** Whatever direction survives D3/D4
must hold in 2014–2019 and 2020–2025 separately before being called anything
stronger than "directionally supported, unconfirmed."

---

## 4. Decision Tree → What Changes Where

- **H1 confirmed (expected):** the residual-permutation null is promoted into
  the shared library as the standard for collinear candidates (per-family
  representative, cache-keyed with a `_matched` label; per-candidate nulls are
  the fallback if families are heterogeneous). Regression test added so the
  orthogonal-null defect cannot silently return. WR/TE battery re-run under
  matched nulls. The prereg gets an **append-only amendment** recording the
  estimand refinement — committed predicted directions are never edited.
  Likely headline: "trailing opportunity is fully priced by ADP at WR" — a
  clean null with a mechanism, not a reversal.
- **H2 confirmed:** the negative direction is a real market-overreaction
  finding. It is reported as a *contradiction of the prereg* (prominently, per
  the project's own rule), and it **re-writes the priors** for every future
  usage-family candidate — including team-context `Team_*_Target_Share`, whose
  preregistration entries would be drafted with negative/uncertain directions
  and the declared mechanism "ADP overprices trailing volume." Requires D6
  replication before "confirmed."
- **H4 found:** construction fix + targeted re-run precedes everything above.
- **Mixed (matched null shrinks but doesn't zero the effect):** report the
  decomposition explicitly — X of the raw effect is collinearity artifact, Y
  is residual signal — and hold the residual at "directionally supported,
  unconfirmed" pending D6.

**Team-context unblocking:** once this ladder resolves, the three
`Team_*_Target_Share` candidates get their own prereg entries (directions per
the branch above) and join a team-context battery run — through the same
extended `build_position_data()` path and, if H1 confirmed, under matched
nulls from day one. The OL-quality family in `TEAM_CONTEXT_REWORK_PLAN.md` is
unaffected by this work and can proceed in parallel: OL stats are weakly
collinear with `Prior_PPG` for RB, but their prereg should note that if H1 is
confirmed, their nulls are re-measured matched as well, as cheap insurance.

---

## 5. Sequencing With the Team-Context Rework

1. Library work common to both efforts (extra_key_col, tests) — once.
2. D0–D2 (cheap diagnostics) — can run immediately, before any team-context
   battery.
3. D3 synthetic validation + matched-null implementation — the one
   substantial build item; gates both the WR/TE re-run and (if H1 confirms)
   the null-measurement mode for all future batteries.
4. WR/TE re-run under the resolved machinery; prereg amendment.
5. Team-context battery: OL family per existing plan; target-share candidates
   added with post-resolution directions.

---

## 6. Installing the Companion Skill for the Implementing Model

The skill `wr-te-anomaly-diagnosis` packages this ladder — plus the project's
non-negotiable working invariants — so a Claude (Sonnet) session in Claude
Code executes the diagnosis the project's way instead of improvising.

**Option A — project skill (recommended: versioned with the repo).**
Place the folder at the repo root as:

```
NFL-Talent-Evaluation/
└── .claude/
    └── skills/
        └── wr-te-anomaly-diagnosis/
            ├── SKILL.md
            └── references/
                └── anomaly_evidence.md
```

Commit it. Any Claude Code session opened in the repo sees the skill
automatically; it triggers when the task matches the skill description. To
force it, say: *"Use the wr-te-anomaly-diagnosis skill and run diagnostic D0."*

**Option B — personal skill (available across projects).**
Same folder placed at `~/.claude/skills/wr-te-anomaly-diagnosis/`.

**Option C — claude.ai.**
Upload the packaged `wr-te-anomaly-diagnosis.skill` file (provided alongside
this plan) in a conversation, or install it via the file card's **Save skill**
button so it persists in your profile.

Recommended prompts once installed, one diagnostic per session/step:
- "Run D0 (the N_BOOT=1000 WR coverage spot-check) per the
  wr-te-anomaly-diagnosis skill and report against its pass/fail read."
- "Run D1 and D2. Do not proceed to D3 without showing me the gradient plot
  numbers."
- "Implement the D3 matched null with its synthetic validation and library
  tests. Stop before touching real data."

The skill instructs the model to stop at each gate and report, so you retain
the go/no-go at every rung.

---

## AMENDMENT (2026-07-12/13) — D0-D4 execution status

See `.claude/skills/wr-te-anomaly-diagnosis/SKILL.md`'s own amendment for
full detail (D0 inconclusive-but-non-blocking, D1 clean/H4 closed for TE, D2
real-but-imperfect gradient with two named exceptions, D3 built and run but
did NOT confirm H1's synthetic mechanism, D4 built and smoke-tested but not
yet run on real data). Summary for quick orientation:

| Rung | Status |
|---|---|
| D0 | Ambiguous by design's own criterion; pooled across 4 measurements, real-but-modest (~87% vs 90% nominal). Non-blocking for D1/D2. |
| D1 | Clean. H4 (construction bug) closed for TE's raw-0.0000 triple. |
| D2 | Real gradient (p=0.04), imperfect fit -- `Avg_Intended_Air_Yards`/`YAC_Share` are named exceptions. |
| D3 | Built + mechanically verified + synthetically tested. Requirement (a) did NOT reproduce under a properly-powered test. H1 not confirmed. `residual_permute` NOT run on real data as a "fix." |
| D4 | Built + fixture-tested + smoke-tested on synthetic data. NOT yet run on real WR data -- this is the next action. |

`WR_TE_ANOMALY_RESOLUTION.md` (the "definition of done" deliverable from
SKILL.md item 4) has not been written yet -- D4 needs to run on real data
first, and D5/D6 haven't started. Do not write it prematurely.
