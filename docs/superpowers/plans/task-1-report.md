# Task 1 Report: accelDispatch frees both operands unconditionally

## Status

COMPLETE. Commit `df131a9` on branch `feat/perceus-accel-operand`.

## Changes Made

### `codegen/wasm.go`

**accelDispatch** -- deleted `freshOwned` predicate; now releases both operands unconditionally:

```go
fmt.Fprintf(b, "    (call $rt_release %s)\n", ea)
fmt.Fprintf(b, "    (call $rt_release %s)\n", eb)
```

`freshOwned` only released `AppClosure`/`CLit`/`MkClosure`/`CPair`/`CFst`/`CSnd`/`CField`/`CCase`
results -- not bare `CVar` or CDup-wrapped operands -- leaving owned operands alive after the
accel op consumed them.

### `codegen/perceus.go`

**annotateBareSpine CDup hoist** -- the root cause of the `TestPerceusAccelArmStepFlat`
failure (delta = 7/run after the accelDispatch fix alone).

CDups inserted for shared-owned-locals (e.g. `n` used in both the base case `addN n n` and
the step closure `fn(k)(ih) addN ih (succ n) end`) were placed INSIDE the AppClosure backbone
at intermediate spine levels. `NatElimSpine` peels only `AppClosure` nodes; a `CDup` in the
`Clo` position breaks the loop, causing the match to fail. `satElimDispatch` fell through to
the generic 4-way `rt_apply` curry chain, leaking b1/b2/b3 intermediate closures (+7/run for
`armUse 3`).

The fix: in `annotateBareSpine`, after obtaining `clo` from the recursive call, peel any CDup
wrappers from it and re-wrap them ABOVE the final `AppClosure{clo, arg}`. This keeps the
AppClosure backbone clean so `NatElimSpine` / `accelMatchC` / `satCtorDispatch` recognize it
correctly, while still emitting all the retains before any arg is consumed.

### `codegen/perceus_test.go`

Five new adversarial receiver tests added at the end of the file:

| Test | Program | Expected |
|---|---|---|
| `TestPerceusAccelSharedOperandFlat` | `addN n n` | steady-flat, output 6000 |
| `TestPerceusAccelDistinctOperandsFlat` | `mulN a b` | steady-flat, output 10000 |
| `TestPerceusAccelLiveAfterNoUAF` | `addN (addN n n) n` | steady-flat, output 3000 |
| `TestPerceusAccelArmStepFlat` | `addN ih (succ n)` in fold step | steady-flat, output 18 |
| `TestPerceusAccelFreshOperandsFlat` | `mulN (addN 10 10) (addN 20 20)` | steady-flat, output 800 |

## Test Summary

```
--- PASS: TestPerceusAccelSharedOperandFlat
--- PASS: TestPerceusAccelDistinctOperandsFlat
--- PASS: TestPerceusAccelLiveAfterNoUAF
--- PASS: TestPerceusAccelArmStepFlat
--- PASS: TestPerceusAccelFreshOperandsFlat
--- FAIL: TestPerceusAccelFrontier   <-- expected signal for Task 2 (see below)
all other TestPerceus* and TestWasm* PASS
```

Full codegen suite: `go test ./codegen/ -count=1 -timeout 600s` -- one failure only.

## TestPerceusAccelFrontier: Expected Signal

Per the plan Step 6 NOTE: `TestPerceusAccelFrontier` now FAILS with:

```
expected accel-in-step to leak (frontier not vacuous), got flat [3 3 3 3 3]
```

`perceusAccelArmSrc` uses `mainArm = armUse 3` where `armUse` is a lambda with an outer `n`
argument. After both fixes, `satElimDispatch` now fires correctly for the NatElim spine inside
`armUse`'s body, and `accelDispatch` releases `ih` per fold iteration. The result is
steady-flat (delta = 0/run), which is what Task 1 was supposed to achieve.

The test was written to confirm the leak EXISTS before the fix. It now correctly shows the
leak is CLOSED. Task 2 must update `TestPerceusAccelFrontier` and `PerceusBalanceable` to
reflect that accel programs are now in the flat fragment.

## Root Cause Analysis

### Why delta was 9/run before Task 1

For `armUse 3` (n=3, 3 fold iterations):

- `addN n n` (base case): accelDispatch did not release `n` (CVar, not freshOwned). CDup#3
  added 1 retain. Net: +2 leaked references to `n` per call but 0 extra blocks (same block).
- `addN ih (succ n)` (step body): accelDispatch released `succ n` result (AppClosure,
  freshOwned) but NOT `ih` (CVar). Each iteration left the old accumulator alive:
  +K_BIG(10), +K_BIG(14) leaked. +2 blocks.
- NatElim curry: satElimDispatch failed (CDup inside backbone). Generic path: +b1, +motive,
  +b2, +K_BIG(6), +step_clo, +b3 = +6 blocks.
- n (K_BIG(3)): CDup#1 and CDup#2 retain, neither released. +1 block.
- Total: 9 blocks/run.

### Why delta was 7/run after accelDispatch fix only

- accelDispatch releases `n` x2 in `addN n n`. CDup#3 counteracts 1 retain, -2 releases:
  n.rc: 3(CDup#3) + 1(original) - 2(releases) = net 2. Still 2 leaked refs, same block.
  Oh, wait -- The block count was 9 before because K_BIG(10) and K_BIG(14) were leaked (ih
  not freed). After fix: ih IS freed per iteration. K_BIG(10) and K_BIG(14) are gone. -2.
- NatElim curry still broken: 7 blocks/run from b1/motive/b2/K_BIG(6)/step_clo/b3/n.

### Why CDup hoist was needed

`annotateBareSpine` at Level 3 (of the 4-level NatElim spine) detected that `n` is shared
between the Level 2 Clo sub-spine (which contains `addN n n`) and the Level 3 Arg (step
closure). It inserted `CDup{n, AppClosure{inner3, step}}`. At Level 4, this CDup-wrapped
result was placed as the Clo of `AppClosure{Clo: CDup{n,...}, Arg: n}`.

`NatElimSpine` peels `AppClosure` nodes from the spine. At Level 4, the first peel gives
`Arg = n` and `t = CDup{n,...}`. `CDup` is not `AppClosure`, so the loop breaks. `t` is not
`CGlobal`, so match fails. satElimDispatch returns false.

With CDup hoisting: the CDups from Level 3 and Level 4 are both collected and placed OUTSIDE
the final `AppClosure{4-arg-spine}`. `NatElimSpine` sees a clean `AppClosure` chain all the
way to `CGlobal{NatElim}`. satElimDispatch fires, builds b3_clo directly, runs the fold, and
releases b3_clo (freeing base K_BIG(6), step_clo, and transitively n).

Final n.rc balance with CDup hoist:
```
+1 (CDup L3 hoist)
+1 (CDup L4 hoist)
+1 (CDup addN inside z -- inside z arg, not backbone)
-2 (accelDispatch releases addN's two n's)
 0 (step MOVE, no rc change)
-1 (satElimDispatch releases b3 -> step -> n)
-1 (satElimDispatch releases rn = fold bound)
= 0
```

## Concerns

1. **TestPerceusAccelFrontier** must be updated in Task 2 to assert flat rather than leaking,
   and `PerceusBalanceable` must be extended to include accel programs.

2. **CDup hoist is a broader fix** than strictly required for accel spines -- it also fixes
   arity-3+ constructor spines where shared-owned-locals across levels would have produced
   CDups inside the backbone. No existing tests cover that shape, so the bonus fix is
   invisible to the test suite but is correct.

3. **Output is byte-identical across all backends** -- the fix changes only `codegen/wasm.go`
   and `codegen/perceus.go` (the WASM Perceus pass). The C, LLVM, JVM, Erlang, Go, JS, and
   WASM backends all produce the same numeric output. `TestPerceusCorpusOutputInvariance`
   confirms this.

---

## Code-review Fixes (Quality pass on df131a9)

### Finding 1 -- New regression guards

Two receivers added at the bottom of `codegen/perceus_test.go`:

**(a) `TestPerceusCtorSharedOwnedLocalNoDoubleFree`**

Source: `perceusCtorSharedOwnedLocalSrc` -- a 2-field Box2 constructor applied to the
same owned Nat local `q` in BOTH field positions (`mk2 (succ q) (succ q)`). Box2Elim
sums both fields via an inline NatElim to prove neither was corrupted.

Output assertion: `"8"` (succ 3 + succ 3 = 4 + 4). PASS confirmed.
`assertSteadyFlatInts(t, p, 5)`: PerceusBalanceable = true (no accel ops, no CBounce,
4-arg NatElim inside arm is not over-applied). PASS confirmed.

**(b) `TestPerceusAccelBorrowedCEnvFlat`**

Source: `perceusAccelBorrowedCEnvSrc` -- `addK 500 500` where the inner closure
captures `n=500` as CEnv and uses it as the first operand to `addN n m`. consumeOwning
dup's the CEnv before accelDispatch sees it; accelDispatch frees the dup'd ref; the
closure's original ref survives.

Output assertion: `"1000"`. PASS confirmed.
`assertSteadyFlatInts(t, p, 5)`: PerceusBalanceable = false (accel ops registered, Task 2
will un-exclude). assertSteadyFlatInts runs independently and measures true flat. PASS confirmed.

### Finding 2 -- Stale freshOwned/old-accel-leak comments corrected

All `freshOwned` references eliminated. Final grep:

```
grep -rn "freshOwned" codegen/
(no output)
```

Files and specific changes:

- `codegen/perceus.go:367-369` (annotate AppClosure): rewritten to say accelDispatch frees
  BOTH owned operands unconditionally; consumeOwning ensures every operand is privately owned.
- `codegen/perceus.go:373-376` (isRecognizedSpine comment): same -- removed freshOwned phrasing.
- `codegen/perceus.go:564-573` (annotateBareSpine docstring): the "bare CVar does NOT free /
  +1/run LEAK" block replaced with the correct description: accelDispatch frees both,
  CDup prevents double-free for shared locals, addN n n is now steady-flat.
- `codegen/perceus.go:735-749` (PerceusBalanceable exclusion comment): replaced the stale
  "accel ops BORROW-read and leak owned-CVar operands / freshOwned" explanation with the
  current reality: Task 1 closed the leak; the predicate still conservatively excludes
  accel programs because Task 2 must rewrite TestPerceusAccelFrontier first.
- `codegen/perceus_test.go:914` (perceusSharedConsumeSrc): replaced "freshOwned (accelDispatch
  releases its result)" with "owned (accelDispatch frees both operands unconditionally)".
- `codegen/perceus_test.go:991-996` (perceusAccelArmSrc): replaced the "borrow-reads /
  freshOwned / does not free a bare-CVar" description with the current behavior.
- `codegen/perceus_test.go:1739` (accelFreshSrc): replaced "after freshOwned is removed"
  with "now that accelDispatch frees both operands unconditionally".

### Finding 3 -- Panic on invariant violation in CDup peel loop

`codegen/perceus.go` CDup peel loop in `annotateBareSpine`: replaced the silent `break`
("non-CVar-keyed CDup: leave in place (shouldn't occur here)") with:

```go
panic(fmt.Sprintf("perceus: non-CVar CDup %T on bare-spine backbone", dup.V))
```

`fmt` was already imported. `go vet` confirms clean.

### Test run after quality fixes

```
go test ./codegen/ -run 'TestPerceus' -count=1 -timeout 40m

--- FAIL: TestPerceusAccelFrontier  <-- intentional, fixed in Task 2
all other TestPerceus* PASS
```
