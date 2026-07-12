# WR/TE Beat-ADP Pre-Registration

*Written and committed BEFORE the WR/TE battery is run. Per Fable 5's repeated request (rounds 5, 6, 7) and the project's own standing rule (Section 6 of the response plan to Fable 5's original review): a finding only counts as confirmed if it (a) clears its declared significance window's bias-corrected interval, (b) matches the direction predicted here in advance, and (c) survives independent replication (era split or a third ADP source). This document exists specifically so post-hoc mechanism stories can never again be applied only to convenient results.*

---

## Global Rules Declared in Advance

1. **Baseline**: `season_ppg ~ RANK + Prior_PPG` vs. the same plus each candidate, identical to QB/RB.
2. **Significance window default**: window **6** (tightest ADP-pick distance) for every candidate predicted to reflect *opportunity/usage* — per this project's own established theory that a genuine tie-breaking effect concentrates among the closest-drafted players, where ADP itself is most uncertain. Window **12** is used only for candidates whose predicted mechanism is genuinely uncertain or plausibly broader than simple opportunity (marked explicitly below).
3. **Bias correction**: a fresh, position-specific null bias will be measured for both WR and TE before any candidate is evaluated — no assumption that either position's bias resembles QB's (+0.012) or RB's (−0.009), which had opposite signs.
4. **NGS-derived candidates get their own missingness-matched null**, not a borrowed one from a full-history candidate, per the unresolved item from two rounds ago.
5. **A "confirmed finding" requires all three**: bias-corrected CI excludes zero at the declared window, in the predicted direction, AND replicates independently. A result clearing (a) and (b) but not yet tested against (c) is reported as **"directionally supported, unconfirmed."**
6. **Explicit, front-and-center limitation for this round specifically**: this framework's `Prior_PPG` requirement excludes every rookie season. This costs the least at QB and the most at WR, where rookie breakouts are common and fantasy-relevant. Every WR result in the eventual report will carry this caveat prominently, not as a footnote.
7. **Rookie/Games_Played, if tested, are excluded from "confirmed finding" contention regardless of result** — Games_Played has shown near-zero persistence at every position tested so far (QB 0.079-0.136, RB similar), and is included here only as a standing control, not a real candidate.

---

## Predictions by Stat (same list applies to both WR and TE; position-specific notes where the mechanism plausibly differs)

### Tier A — Opportunity/volume stats. Strong prior: POSITIVE, window 6.
*Mechanism, in one sentence each: this project has found opportunity/usage-share stats to be the most consistently persistent and highest-value category at every position tested (QB rushing volume, RB target share, team-context target distribution).*

| Stat | Predicted Direction | One-Line Mechanism |
|---|---|---|
| `Target_Share` | Positive | Direct share of team's passing opportunity; the single most consistent category-winner in every prior persistence screen. |
| `Targets_PG` | Positive | Raw version of the same opportunity signal. |
| `Air_Yards_Share` | Positive | Share of team's downfield opportunity, not just raw volume. |
| `Pct_Share_Intended_Air_Yards` | Positive | NGS-based, cleaner version of `Air_Yards_Share` — same predicted mechanism. |
| `WOPR` | Positive, **low confidence it beats its own components** | Composite of target share + air yards share; RB's own `WOPR` barely outpersisted `Target_Share` alone and then showed a *harmful* effect once tested (later shown to be a design artifact) — flagging in advance that this composite may again add nothing over its parts. |
| `RZ_Targets_PG` | Positive | Red-zone opportunity specifically; opportunity-in-high-value-area has been a real signal at QB (`RZ_Rush_Att_PG`) and a real (if artifact-tainted) signal at RB. |
| `EZ_Targets_PG` | Positive, lower confidence | Subset of red-zone opportunity, narrower and likely noisier. |
| `Rec_Yards_PG` | Positive | Blends opportunity and efficiency but still volume-dominant. |
| `Rec_First_Downs_PG` | Positive | Chain-moving opportunity, a volume-flavored measure. |
| `Rec_Yards` (raw) | Positive, lower confidence than per-game version | Raw totals have shown more noise than per-game/share versions at every position tested so far. |
| `Targets` (raw) | Positive, lower confidence | Same caveat as above. |

### Tier B — Efficiency/rate stats. Weak prior, window 12 (mechanism less clearly tied to tight-window tie-breaking).
*Mechanism: this project has found efficiency stats to underperform opportunity stats consistently at every position (QB completion/EPA metrics, RB per-touch efficiency). No strong reason to expect WR/TE to break this pattern.*

| Stat | Predicted Direction | One-Line Mechanism |
|---|---|---|
| `Yards_Per_Target` | Weak/uncertain | Pure efficiency measure, the category that has underperformed everywhere else. |
| `Catch_Rate` | Weak/uncertain | Efficiency measure, plausibly driven by scheme (short/high-percentage routes) as much as individual skill. |
| `Receiving_EPA` | Weak/uncertain | Direct analogue to QB's `Dropback_EPA`, itself a marginal, boundary-case finding. |
| `Yards_Per_Reception` | Weak/uncertain | Efficiency-flavored, similar reasoning to `Yards_Per_Target`. |

### Tier C — Depth-of-target stats. Genuinely uncertain direction (not just weak magnitude), window 12.
*Mechanism: these could plausibly point either way and this project has no established prior. Deep threats carry a higher ceiling but a lower, less consistent floor; possession receivers carry the reverse. Declared uncertain in advance specifically to avoid inventing a directional story after seeing the result.*

| Stat | Predicted Direction | One-Line Mechanism |
|---|---|---|
| `Receiving_aDOT` | Uncertain | Higher aDOT trades consistency for ceiling — net fantasy-value direction genuinely unclear in advance. |
| `Avg_Intended_Air_Yards` | Uncertain | NGS version of the same tradeoff. |
| `Avg_Cushion` | Uncertain | Could reflect defenses respecting a receiver's speed (more cushion given) or targeting him for extra attention (less cushion) — mechanism cuts both directions, no prior. |

### Tier D — YAC-related stats. Weak-to-uncertain prior, window 12.
*Mechanism: could reflect genuine individual skill (elusiveness after the catch) or could reflect scheme (screens/short designed touches) — same ambiguity flagged for RB's YAC-related measures.*

| Stat | Predicted Direction | One-Line Mechanism |
|---|---|---|
| `YAC_Share` | Weak/uncertain | Same scheme-vs-skill ambiguity noted for RB's YAC measures. |

### Tier E — The flagship "isolated skill" tests. Weak prior specifically BECAUSE of RB's precedent, window 12.
*Mechanism: these are the receiving-side analogues of RB's `RYOE_Per_Att`, which underperformed simple volume once context (blocking, scheme) was stripped away. Declaring a weak prior in advance, specifically as a genuine test of whether that pattern generalizes to receivers — not assuming it will.*

| Stat | Predicted Direction | One-Line Mechanism |
|---|---|---|
| `Avg_Separation` | Weak, low confidence, **but the single most theoretically interesting candidate in this list** | The clearest true skill-isolation measure available (route-running/separation ability, independent of scheme) — if this breaks the established "isolated skill underperforms volume" pattern, it would be a first for this entire project. |
| `YAC_Above_Expectation` | Weak, low confidence | Direct receiving-side RYOE analogue; RB's RYOE showed some signal but weaker than raw volume. |

### Control (not a real candidate)

| Stat | Predicted Direction | One-Line Mechanism |
|---|---|---|
| `Games_Played` | Null (no effect) | Durability has shown near-zero persistence at every position tested; included only as a standing sanity check, not evaluated for "confirmed finding" status regardless of result. |

---

## Position-Specific Note: TE

Tight ends are frequently used as high-percentage safety-valve options rather than deep threats — meaning `Catch_Rate` and `Receiving_aDOT` may show a *different* relationship to fantasy value at TE than at WR even though the same stat is being tested. This is noted here, in advance, as a reason TE's results should be interpreted on their own terms rather than assumed to mirror WR's, not as a post-hoc excuse if the two positions diverge.

---

## What Happens Next

1. Measure WR's and TE's own position-specific null bias, fresh, via the validated `estimate_concordance_null_bias_robust()` procedure.
2. A quick coverage spot-check at WR specifically, given its unusually large player pool (more pairs, more margin compression — the same mechanism that produced RB's distinct bias behavior).
3. Run the persistence screen (already built, 22 candidates per position) if not already completed, followed by the full bias-corrected Beat-ADP battery on whatever survives.
4. Build the family-correlation matrix for WR/TE from the start (not retrofitted), given receiving stats are expected to be more intercorrelated than QB rushing stats were.
5. Report results exactly against the predictions above — including explicitly flagging any result that contradicts its predicted direction, not just the ones that confirm it.
