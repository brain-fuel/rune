# Task 2 Report: Re-open PerceusBalanceable to accel programs

## Status

DONE. Commit: 37925db on branch feat/perceus-accel-operand.

## Step 1: Rewrite TestPerceusAccelFrontier

File: codegen/perceus_test.go

- Replaced `TestPerceusAccelFrontier` (asserted exclusion + leak) with:
  - `TestPerceusAccelInFlatFragment` -- asserts `PerceusBalanceable` returns true AND
    `assertSteadyFlatInts` passes AND output is "18".
  - `TestPerceusAccelCorpusFlat` -- subtests for sum/prod/diff/diffZero; each asserts
    balanceable + flat + correct output (8000/10000/5/0).
- Updated doc comment on `perceusAccelArmSrc` constant to reflect closure (no longer a
  frontier; references the two new receivers).
- Updated `TestPerceusCorpusSteady` doc comment to reflect 10 balanced, 0 skipped.
- Kept `perceusAccelArmSrc` constant and its content unchanged (still referenced).

## Step 2: Grep result -- corpus count assertions

Command: `grep -n "Nat.Ops|accel.*excl|excl.*accel|skipped|balanced 3|3->6|6 balanced|nBalanced|nSkipped" codegen/perceus_test.go`

Result: `TestPerceusCorpusSteady` tracks counts via `nBalanced`/`nSkipped` but asserts
only `nBalanced > 0` (no hard-coded count). No corpus count receiver hard-codes
accel-as-skipped with a specific number -- only the log message at the end. No count
update was needed; the comment was updated to reflect the new expected values.

## Step 3: Receivers fail at PerceusBalanceable (predicate still excludes accel)

```
--- FAIL: TestPerceusAccelInFlatFragment (0.02s)
    perceus_test.go:1012: accel-operand leak closed: an accel program with flat bodies must be IN the flat fragment
--- FAIL: TestPerceusAccelCorpusFlat (0.02s)
    --- FAIL: TestPerceusAccelCorpusFlat/sum (0.01s)
    perceus_test.go:1032: accel listing sum must be balanceable
    ... (prod/diff/diffZero same)
FAIL    goforge.dev/rune/v3/codegen  0.039s
```

Both fail at the `PerceusBalanceable` assertion as expected.

## Step 4: Remove wholesale accel exclusion in PerceusBalanceable

File: codegen/perceus.go

Removed:
```go
// Accel-op programs are conservatively excluded (see comment above; Task 2 widens).
if p.Nat != nil && len(p.Nat.Ops) > 0 {
    return false
}
```

Replaced with comment:
```go
// Accel-op programs are now admitted: accelDispatch frees both owned operands
// (the accel-operand leak is closed), so a flat accel use reaches steady-flat. Any
// remaining unbalanceable construct (CBounce, over-applied NatElim) is still caught
// per-spine by cirUnbalanceable below.
```

Also updated the function godoc comment to say "ONE REMAINING exclusion category"
(CBounce only) instead of "two".

No corpus count receiver required a count update (none had hard-coded counts).

## Step 5: Receivers pass

```
--- PASS: TestPerceusAccelInFlatFragment (0.05s)
--- PASS: TestPerceusAccelCorpusFlat (0.17s)
    --- PASS: TestPerceusAccelCorpusFlat/sum (0.04s)
    --- PASS: TestPerceusAccelCorpusFlat/prod (0.04s)
    --- PASS: TestPerceusAccelCorpusFlat/diff (0.04s)
    --- PASS: TestPerceusAccelCorpusFlat/diffZero (0.04s)
PASS    goforge.dev/rune/v3/codegen  0.231s
```

Existing frontier tests confirmed still correct:
- `TestPerceusOverAppliedFrontier` -- PASS (over-applied NatElim still excluded)
- `TestPerceusFrontierBoundary` -- PASS (CBounce still excluded)
- `TestPerceusCorpusSteady` -- PASS, logs "10 balanced (steady flat), 0 skipped"

## Step 6: R-PERCEUS.md edits

File: ref_docs/wootz/R-PERCEUS.md

Changes (matching `--` dash convention, no em/en-dashes):

1. **Corpus table** (steady-state gate section): changed "6 balanced, 4 skipped" to
   "10 balanced, 0 skipped"; flipped sum/prod/diff/diffZero row from NO to YES.

2. **"two REMAINING excluded classes" section**: replaced with "ACCEL-operand residual:
   CLOSED" section documenting the mechanism (accelDispatch frees both owned operands;
   every operand is a private owned ref via consumeOwning + shared/multi-use dups) and
   "ONE REMAINING excluded class" (CBounce/partials only).

3. **Plan 6b-2 outcome table**: added row for "accel-CVar-operand leak" closed by
   accel-operand plan Task 1 + Task 2.

4. **"resulting flat fragment" list**: expanded the accel arithmetic item to note that
   accel ops on owned locals (addN n n, accel-in-fold-step) are now included.

5. **satElimDispatch section footer**: updated "grew from 3 to 6" to note it
   subsequently grew to 10 after the accel-operand closure.

6. **PerceusBalanceable description**: updated to state only ONE exclusion remains
   (CBounce) and that accel programs are now admitted.

7. **steady-flat count in Plan 6b-2 outcome paragraph**: updated to "10 balanced, 0
   skipped (was 3 balanced, 7 skipped after 6b-2; 0 balanced, 10 skipped before)".

## Step 7: Full codegen gate

Command: `go test ./codegen/ -count=1 -timeout 40m`

Result:
```
ok      goforge.dev/rune/v3/codegen     65.422s
```

ALL PASS. 8-backend conformance byte-identical; every Perceus/ARC/steady receiver green,
including Task 1's receivers and the two new Task 2 receivers.

## Concerns

None. The per-spine `cirUnbalanceable` scan correctly excludes CBounce and over-applied
NatElim as confirmed by `TestPerceusFrontierBoundary` and `TestPerceusOverAppliedFrontier`
staying green. The corpus steady gate grew from 6 to 10 balanced with zero failures.
