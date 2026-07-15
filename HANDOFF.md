# Handoff: WR/TE Anomaly Diagnosis — resume here

You are picking up a multi-session investigation into the Beat-ADP fantasy
football project's WR/TE "opportunity direction anomaly": receiving-volume
stats (target share, targets, etc.) came out pointing the wrong direction
against their preregistered predictions. This file is written so you can
orient yourself without re-deriving anything already established. Read it
before touching any file in this repo.

## Files in this package and where they go

Drop these into your existing `NFL-Talent-Evaluation` repo root, overwriting
where paths match:

```
R/beat_adp_battery.R                                  -- OVERWRITE
test_beat_adp_battery.R                                -- OVERWRITE
analyze_estimation_battery.R                           -- OVERWRITE
diagnose_wr_te_anomaly_D0_D1_D2.R                       -- NEW
diagnose_wr_te_anomaly_D4.R                              -- NEW
WR_TE_ANOMALY_RESOLUTION_PLAN.md                        -- OVERWRITE (has a new amendment appended; original content unchanged, nothing edited in place)
.claude/skills/wr-te-anomaly-diagnosis/SKILL.md          -- OVERWRITE (same: amendment appended)
.claude/skills/wr-te-anomaly-diagnosis/references/anomaly_evidence.md  -- OVERWRITE (same)
```

Once these are in place and committed, **Claude Code will pick up the
`wr-te-anomaly-diagnosis` skill automatically** from `.claude/skills/` — you
don't need to be told to use it, but if you want to force it explicitly:
*"Use the wr-te-anomaly-diagnosis skill."*

**Read the skill's amendment section (near the bottom of its `SKILL.md`)
first.** It is the authoritative, dated record of what's actually been done —
more authoritative than the original ladder text above it, which is left
unedited on purpose (this project's convention is append-only amendments,
never silently rewriting a prior conclusion).

## One-paragraph summary of where this stands

D0-D2 (cheap diagnostics) ran on real data: coverage checks were noisy but
non-blocking, no construction bugs were found, and a real (if imperfect)
statistical pattern was confirmed in real WR data — collinearity with the
model's baseline predicts how negative a candidate's raw effect looks. D3 (a
proposed statistical fix, "the null-bias measurement itself is blind to this
collinearity") was built, mechanically verified, and synthetically tested —
and did **not** survive its own designed test, once tested with proper
statistical power. That fix was therefore **not** applied to real data. D4
(two simple, no-bootstrap, no-permutation direct reads of the real data) was
built, fixture-tested, and smoke-tested on fake data — **but has not yet
been run on the real WR dataset.** That is the next action.

## Immediate next action

Run this (requires network access to `nflreadr`'s data source — check that
before assuming this environment can do it standalone):

```r
rm(list = intersect(ls(), c("SEASONS_TO_TEST","DECAY_R","LOOKBACK_YEARS","MIN_GAMES",
                             "N_BOOT","SKIP_RMSE","CONF_LEVEL","SIGNIFICANCE_WINDOW",
                             "FORCE_RECOMPUTE_BIAS")))
source("test_beat_adp_battery.R")   # expect 73 passing, 1 failing -- the 1 IS the D3
                                     # result, reported deliberately. Do not "fix" it.
source("diagnose_wr_te_anomaly_D4.R")
```

Then **stop and report** the output to a human before drawing conclusions or
proceeding to D5/D6. This project's own working rules (below) require a gate
here, same as every rung before it.

## Non-negotiable working rules for anyone continuing this (condensed)

These come from this project's own review history — every one exists because
violating it already cost a retraction or a multi-round investigation. Full
detail may exist elsewhere in this repo (`Project_Context.txt`,
`Task_Protocol.md` if present); this is the condensed version relevant to
this specific task:

1. **Read the artifact, not the summary.** Never accept a claim about what
   code does, or what a result was, without checking the actual file/output.
   A cached value is not a reproduction.
2. **Single source of truth.** All battery/bootstrap/bias/data-assembly logic
   lives in `R/beat_adp_battery.R`. Never write a parallel local copy in an
   analysis or diagnostic script — extend the shared function with a
   backward-compatible parameter instead. This has already been the cause of
   this project's worst incident once.
3. **Test-first, with real fixtures.** Any change to `R/beat_adp_battery.R`
   ships with tests in `test_beat_adp_battery.R` in the same change, using
   small parameters and an isolated temp `cache_path` — never the production
   cache (`output/concordance_null_bias_cache.csv`). The full suite must pass
   (or fail *only* on an already-understood, deliberately-reported result)
   before any real-data run.
4. **Synthetic validation before real data**, for any new statistical
   mechanism. Construct a case where you know the right answer, and check the
   code actually gets it — not once, but with enough replication (multiple
   independent draws, not one) that a "pass" isn't just noise. This exact
   thing happened during D3: a single-draw synthetic test gave a false
   positive; a properly-powered version reversed it. Suspect your own first
   "clean" result, especially if it confirms what you expected.
5. **No goal-seeking.** The objective is an explained result, not a
   preferred-sign result. Do not iterate on machinery until a hypothesis you
   like gets confirmed. Every machinery change must be justified by a
   diagnostic that predicted it *in advance*, not one that happened to
   produce a convenient answer after the fact.
6. **Stop at gates.** After each diagnostic rung, stop and report the
   pass/fail read before proceeding to the next one. Do not run the whole
   remaining ladder unattended unless a human explicitly says to.
7. **Report distributions, not adjectives.** Replicate counts, SEs,
   percentiles, binomial CIs on rates — not "meaningfully different" or
   "clearly resolved" without the numbers that back it up.
8. **Own mistakes and correct the record plainly** rather than defending a
   prior conclusion. This project's history includes several logged
   retractions and is stronger for it — this handoff itself documents one
   from this exact investigation (D3's initial false-positive synthetic
   result).

## What NOT to do

- Do not treat D3's failure as "H2 confirmed" — it only rules out one
  proposed explanation (H1) for a real pattern (D2), it does not confirm the
  alternative. H2 needs its own evidence (that's what D4 is for).
- Do not write `WR_TE_ANOMALY_RESOLUTION.md` (the final summary deliverable)
  until D4 has actually run on real data and a human has reviewed it. D5/D6
  haven't started.
- Do not promote `residual_permute` into the standard battery pipeline
  (`run_estimation_for_position()` does not currently expose `null_mode` —
  that wiring was deliberately deferred, and per D3's result, should stay
  deferred unless new evidence supersedes it).
- Do not re-run the D0 coverage check again expecting a decisive answer at
  n=50 or even n=200 — four measurements across two sample sizes already
  exist and are logged in the skill's amendment; a fifth quick spot-check
  adds little. If coverage precision actually matters going forward, use the
  project's real `validate_bootstrap_coverage.R` harness, not another spot-check.
