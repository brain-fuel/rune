# Perceus 6b-2: Close the Leak Residuals (real WASM programs run steady-flat) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the four named leak residuals from Plan 6b so a realistic inline WASM program (pattern-match + construct + nat-literal + nat-fold) reaches steady-flat `$live` (zero per-run leak), then re-open `PerceusBalanceable` so the corpus steady-state gate actually covers real listings.

**Architecture:** Four residuals across three layers, closed in a coupling-respecting order. (1) `emitNatFold` (the hand-rolled builtin-nat eliminator loop) leaks its per-iteration counter / accumulator / step-closure temps and borrow-passes the counter to the step - rewrite it to OWN and release those temps and to retain the counter before the step (so the step becomes a normal owned-arg consumer). (2) With the nat-fold step no longer borrow-passing, REMOVE the Perceus curry-through carve-out so every dead curry argument - including a datatype eliminator's captured-dead MOTIVE - drops normally. (3) Emit a SATURATED constructor application directly to `rt_mkcon` (no intermediate partial-application `K_CLO`), eliminating the arity>=2 container leak. (4) Release the per-digit temporaries inside `rt_big_parse` so a nat literal does not leak ~4 `K_BIG` blocks. Finally (5) re-open `PerceusBalanceable` and prove a realistic program reaches true flat.

**Tech Stack:** Go (`codegen/`); WAT in `codegen/wasm.go` (`emitNatFold`, `emitCtorBlock`, `accelDispatch`/the saturated-ctor emit path) and `codegen/wasm_runtime.go` (`rt_big_parse`); the Perceus pass `codegen/perceus.go`; the steady-state harness in `codegen/wasm_steady_test.go` + receivers in `codegen/perceus_test.go`; wasmtime v42 (present). Programs reach the backend via `internal/session`'s `EmitProgram`.

## Global Constraints

- **Kernel frozen.** No edits under `core/` or `store/`. No new core constructor; hash 0x06 unchanged. This plan changes only `codegen/` + `ref_docs/wootz/`.
- **The 6a ARC RUNTIME is UN-frozen for this plan, additively only.** Plan 6b froze `wasm_runtime.go`; 6b-2 edits it (Task 4: `rt_big_parse` temp release). Edits are ADDITIVE ownership (release calls on the runtime's own temporaries) and must keep the 6a ARC tests (`TestARCRecursiveRelease`/`TestARCLeafRetainRelease`/`TestARCFreeListReuse`/`TestARCPairRelease`/`TestARCClosureRelease`/`TestARCHeaderLiveCounter`) byte-for-byte green. The retain/release primitives, the header layout, `$live`, and the free list are NOT changed. store=MOVE is preserved (no retain-on-store).
- **Output-invariance is non-negotiable.** Every task keeps every WASM program's printed output byte-identical (the `TestWasmConformsToJS` corpus + the 8-backend conformance gate stay green). A leak fix that changes a result is a bug.
- **Opt-in stays.** Only `Wasm.Emit` runs Perceus; the C/LLVM and six source backends consume un-annotated CIr and stay byte-identical. The emitter changes (Tasks 1, 3) are WASM-only (`codegen/wasm.go`); do not touch the shared matchers' behavior for other backends.
- **CBounce / partials are OUT OF SCOPE (reclassified).** The WASM emitter has NO `CBounce` case (`emitIn` would panic), so partials are an UNSUPPORTED FEATURE on WASM, not a leak. 6b-2 does not touch CBounce; `PerceusBalanceable` keeps excluding it as "unsupported on WASM", and R-PERCEUS records "WASM partial/trampoline support" as a separate future plan. Do not attempt trampoline ownership here.
- **The measurement discipline.** The residuals COMPOUND (a real program leaks via several at once), so a per-task receiver cannot reach flat alone. Each task asserts its residual's per-run leak DELTA is removed (the steady count's per-run increment drops by the residual's contribution), with output unchanged. Only Task 5's realistic receiver asserts TRUE flat (zero per-run increment) once all four are closed.
- **Process standards.** NO em or en dashes (ASCII hyphen-minus only). Conventional Commits. Run `go test ./codegen/` + `go test ./harness/ -run 'Conformance|Backend'` before tagging; `go build ./...` + `go vet ./codegen/` stay clean.

---

### Task 1: `emitNatFold` owns its loop temporaries (counter / accumulator / step)

The builtin-nat eliminator loop (`codegen/wasm.go` `emitNatFold`) allocates a fresh `K_BIG` counter every iteration (`rt_big_succ`) and a fresh step-closure + accumulator every iteration, leaking the previous ones; and it borrow-passes the counter `$k` to the step (which is the reason the Perceus carve-out exists). Rewrite the loop to release the superseded counter / step / accumulator each iteration, and to RETAIN the counter before applying the step (so the step owns and drops its argument like any function).

**Files:**
- Modify: `codegen/wasm.go` (`emitNatFold`)
- Test: `codegen/perceus_test.go`

**Interfaces:**
- Consumes: the 6a runtime `$rt_retain`/`$rt_release`/`$rt_big_succ`/`$rt_big_cmp`/`$rt_big_from_long`/`$rt_apply`/`$rt_env`; the fold env layout (slot 1 = `acc`/`c0`, slot 2 = `c1`/step).
- Produces: an `emitNatFold` whose per-iteration `$live` footprint is constant (no per-iteration leak of counter/step/acc), and whose step is applied to a RETAINED counter (so the step block becomes a normal owned-arg consumer - this is what Task 2 relies on to remove the carve-out).

- [ ] **Step 1: Write the failing delta test**

A `NatElim` recursion over a literal whose per-run `$live` currently GROWS with the literal's magnitude (each fold iteration leaks). Use a SMALL literal so the parse-leak (Task 4, still open) is a constant offset and the FOLD leak is the per-iteration growth we measure.

```go
// codegen/perceus_test.go
// TestPerceusNatFoldOwnership: a NatElim fold over a small nat. BEFORE: each of the
// N iterations leaks the prior counter + step closure + acc, so steady grows with N.
// AFTER Task 1: the fold's per-iteration temps are released, so the steady per-run
// increment drops by the fold contribution. We assert the per-run DELTA shrinks to the
// non-fold residual (parse temps, Task 4 - still present here), i.e. the fold no longer
// contributes per-iteration growth. Output unchanged.
func TestPerceusNatFoldOwnership(t *testing.T) {
	// `double n = natElim (k acc. succ (succ acc)) zero n` style: a fold that runs n
	// steps. With n=3 the fold body runs 3 times; the leak delta between consecutive
	// runs must NOT include 3x (counter+step+acc) after the fix.
	src := perceusNatFoldSrc // defined below: a NatElim-driven function applied to 3
	got := runWasm(t, emitWith(t, cg.Wasm{}, src, "mainFold"))
	if got != natFoldWant { // the correct numeric result, unchanged
		t.Fatalf("output changed under Task 1: got %q want %q", got, natFoldWant)
	}
	// Measure the per-run steady increment; after Task 1 it must be the constant
	// non-fold residual, NOT growing with the fold's iteration count.
	p := mustProgram(t, src, "mainFold")
	counts := wasmSteadyLivePInts(t, p, 4) // helper returning []int (add if absent)
	d := counts[3] - counts[2]
	if d > natFoldMaxResidual { // a small constant bound (parse temps only); the fold
		t.Fatalf("nat-fold still leaks per-iteration: per-run delta=%d > %d", d, natFoldMaxResidual)
	}
}
```

The implementer DEFINES `perceusNatFoldSrc` (a concrete `data Nat` + `builtin nat` + a `NatElim`-driven function like a doubling or counting fold applied to `3`), `natFoldWant` (its correct output), and `natFoldMaxResidual` (the measured constant parse-residual upper bound, e.g. a handful of blocks - set it after observing the post-fix counts; it must be INDEPENDENT of the fold iteration count). Add `wasmSteadyLivePInts` if a numeric variant of `wasmSteadyLiveP` is not present (parse the printed counts to ints).

- [ ] **Step 2: Run it to confirm the per-iteration leak**

Run: `go test -run TestPerceusNatFoldOwnership ./codegen/`
Expected: FAIL - the per-run delta grows with the fold's iteration count (the prior counter/step/acc leak each iteration).

- [ ] **Step 3: Rewrite `emitNatFold` to release temps + retain the counter**

Replace the loop body in `codegen/wasm.go` `emitNatFold`. The current loop:

```wat
    (local.set $k (call $rt_big_from_long (i32.const 0)))
    (block $done (loop $l
      (br_if $done (i32.ge_s (call $rt_big_cmp (local.get $k) (local.get $arg)) (i32.const 0)))
      (local.set $step (call $rt_apply (local.get $c1) (local.get $k)))
      (local.set $acc (call $rt_apply (local.get $step) (local.get $acc)))
      (local.set $k (call $rt_big_succ (local.get $k)))
      (br $l)))
    (local.get $acc))
```

Rewrite so each iteration: RETAINs `$k` before the step consumes it (the step owns+drops its arg), RELEASEs the intermediate `$step` closure after the second apply, RELEASEs the OLD `$acc` is NOT needed (it is consumed by the step apply - moved in), and RELEASEs the old `$k` after computing its successor. Concretely:

```wat
    (local.set $k (call $rt_big_from_long (i32.const 0)))
    (block $done (loop $l
      (br_if $done (i32.ge_s (call $rt_big_cmp (local.get $k) (local.get $arg)) (i32.const 0)))
      ;; the step owns its counter arg: retain $k so our loop keeps its own reference
      (call $rt_retain (local.get $k))
      (local.set $step (call $rt_apply (local.get $c1) (local.get $k)))   ;; step owns the retained $k
      (local.set $acc (call $rt_apply (local.get $step) (local.get $acc))) ;; step consumes $acc (move), returns new acc
      (call $rt_release (local.get $step))                                 ;; the partial step closure is dead
      (local.set $knext (call $rt_big_succ (local.get $k)))
      (call $rt_release (local.get $k))                                    ;; old counter dead
      (local.set $k (local.get $knext))
      (br $l)))
    (call $rt_release (local.get $k))   ;; the final counter (== arg's successor bound) is dead
    (local.get $acc))
```

Declare the new `$knext` local. IMPLEMENTER NOTE (gate-arbitrated): the EXACT retain/release placement depends on whether `rt_apply` / the step block consume their arguments under Perceus. The invariant to reach: after each iteration `$live` returns to the same value it had at the top of the iteration (the loop is steady), and `$acc` carries exactly one owned reference out. If the gate shows a leak, a temp is missing a release; if it shows a use-after-free (wrong output / trap), a release is too early (e.g. releasing `$k` before `rt_big_succ` reads it, or releasing `$acc`/`$arg` which are still needed). `$arg` (the bound) is BORROWED by the loop (read by `rt_big_cmp`) and owned by the caller - do NOT release it here. The step closure `$c1` (env slot 2) is borrowed from the env - do NOT release it. Tune against `TestPerceusNatFoldOwnership` until the per-run delta is the constant parse-residual.

- [ ] **Step 4: Run the delta test + confirm output-invariance**

Run: `go test -run TestPerceusNatFoldOwnership ./codegen/`
Expected: PASS (per-run delta is the constant parse residual, independent of iteration count; output unchanged).

Run: `go test -run 'Wasm|WASM|ARC|Perceus' ./codegen/`
Expected: PASS (all 6a ARC tests + every prior receiver + conformance; the nat-fold output is unchanged for every program that uses it).

- [ ] **Step 5: Commit**

```bash
git add codegen/wasm.go codegen/perceus_test.go
git commit -m "feat(wasm): emitNatFold owns its loop temporaries (counter/step/acc) + retains the counter for the step"
```

---

### Task 2: Remove the curry-through carve-out (drop the dead motive + any dead curry arg)

With Task 1 making the nat-fold step a normal owned-arg consumer, the Perceus curry-through carve-out (a block whose body is a bare `MkClosure` keeps its argument borrowed) is no longer needed - and it is what leaks a datatype eliminator's captured-DEAD motive (+1/run). Remove the carve-out so every dead curry argument drops uniformly.

**Files:**
- Modify: `codegen/perceus.go` (remove the carve-out branch in `ownScope`/the curry-through handling)
- Modify: `ref_docs/wootz/R-PERCEUS.md` (the carve-out section: now removed; explain why Task 1 made it safe)
- Test: `codegen/perceus_test.go`

**Interfaces:**
- Consumes: Task 1's owned-counter nat-fold (the only former borrow-passer); `ownScope` / the curry-through detection.
- Produces: `ownScope` drops dead owned arguments in ALL blocks, including curry-through (bare-`MkClosure`-body) blocks; no special carve-out remains.

- [ ] **Step 1: Write the failing delta test**

```go
// TestPerceusDeadMotiveDropped: an INLINE general eliminator (optElim m c0 c1 (some 7)).
// The motive m is captured-dead; BEFORE, the carve-out leaves it borrowed -> +1/run.
// AFTER removing the carve-out, the dead motive drops -> the eliminator's motive
// contribution (+1/run) is gone. We assert the per-run delta drops by 1 vs the prior
// (Task-1) baseline. Output unchanged. (Other residuals - parse temps, ctor container -
// may still contribute; we measure the MOTIVE delta specifically by comparing to a
// recorded baseline, or assert the absolute residual matches the post-Task-2 expectation.)
func TestPerceusDeadMotiveDropped(t *testing.T) {
	src := perceusInlineElimSrc // optElim ... (some 7), inline (NOT cached)
	got := runWasm(t, emitWith(t, cg.Wasm{}, src, "mainElim"))
	if got != "7" {
		t.Fatalf("output changed: got %q want 7", got)
	}
	p := mustProgram(t, src, "mainElim")
	counts := wasmSteadyLivePInts(t, p, 4)
	d := counts[3] - counts[2]
	if d > inlineElimResidualAfterMotive { // the residual WITHOUT the motive leak
		t.Fatalf("dead motive still leaks: per-run delta=%d > %d", d, inlineElimResidualAfterMotive)
	}
}
```

Reuse the Task 6-fix `perceusInlineElimSrc` (the inline `optElim ... (some 7)`) if present; else define it. `inlineElimResidualAfterMotive` is the measured residual after the motive is dropped (set after observing post-fix counts; it should be exactly 1 less than the Task-1 baseline for this program).

- [ ] **Step 2: Run it to confirm the motive leak**

Run: `go test -run TestPerceusDeadMotiveDropped ./codegen/`
Expected: FAIL (the dead motive leaks +1/run under the carve-out).

- [ ] **Step 3: Remove the carve-out**

In `codegen/perceus.go`, find the curry-through carve-out (the branch in `ownScope` that, for a block whose body is a bare `MkClosure`, SKIPS dropping the block's dead argument - search the comments for "curry-through" / "carve-out" / "bare MkClosure"). DELETE that special case so the dead-argument drop applies uniformly. Confirm by reading: after removal, `ownScope` drops a block's argument iff it is owned and dead (`!cirUsesArg(body, 0)`), with no exception for bare-`MkClosure` bodies.

(Because Task 1 made the nat-fold step retain-then-own its counter, dropping a curry-through block's dead argument is now safe: no frozen emitter path borrow-passes an argument that the block would then drop. If the gate shows a use-after-free in a nat-arithmetic program, Task 1's retain placement is incomplete - fix Task 1, not re-add the carve-out.)

- [ ] **Step 4: Run the delta test + the full gate**

Run: `go test -run TestPerceusDeadMotiveDropped ./codegen/`
Expected: PASS (motive dropped; per-run delta down by 1; output 7).

Run: `go test -run 'Wasm|WASM|ARC|Perceus' ./codegen/` and `go test ./harness/ -run 'Conformance|Backend'`
Expected: PASS (no carve-out regression; nat-arithmetic programs - the former borrow-passers - still correct AND now leak-free on the fold path; conformance byte-identical).

- [ ] **Step 5: Update R-PERCEUS.md + commit**

Edit the carve-out section of `ref_docs/wootz/R-PERCEUS.md` to record that the carve-out is REMOVED, made safe by Task 1's owned-counter nat-fold. Verify no dashes (`grep -nP "[\x{2013}\x{2014}]" ref_docs/wootz/R-PERCEUS.md`).

```bash
git add codegen/perceus.go codegen/perceus_test.go ref_docs/wootz/R-PERCEUS.md
git commit -m "feat(wasm): remove the curry-through carve-out (drop the dead eliminator motive)"
```

---

### Task 3: Emit saturated constructor applications directly (no intermediate K_CLO)

`emitCtorBlock` builds a partial-application `K_CLO` for every curry step of an arity>=2 constructor; a saturated application `mk a b` therefore allocates an intermediate `K_CLO` (`mk a`) that the bare-spine carve-out leaves un-released (releasing it would double-free the moved field `a`). Recognize a SATURATED constructor application in the WASM emitter and build the `K_CON` directly from the argument values, so the intermediate `K_CLO` is never allocated.

**Files:**
- Modify: `codegen/wasm.go` (`emitIn`'s `AppClosure` path: a saturated-constructor recognizer + a direct `rt_mkcon` emit, mirroring `accelDispatch`)
- Modify: `codegen/perceus.go` (the bare-spine recognizer may now treat a saturated ctor as NOT-bare, since no intermediate exists to protect - adjust if needed so the saturated ctor's args still get `consumeOwning` exactly once)
- Test: `codegen/perceus_test.go`

**Interfaces:**
- Consumes: the constructor metadata (`CtorSpec` name/tag/arity; `p.Datas`), the saturated-spine recognition (an `AppClosure` spine whose head `CGlobal` is a constructor with exactly `arity` args), `rt_mkcon`/`rt_con_set`.
- Produces: a saturated constructor application emits `rt_mkcon(tag,name,arity)` + `rt_con_set` per arg (each arg value evaluated once, moved in), with NO intermediate `K_CLO`. A PARTIALLY applied constructor still uses the curried `emitCtorBlock` path (its `K_CLO` is a legitimate value the pass owns + drops normally).

- [ ] **Step 1: Write the failing delta test**

```go
// TestPerceusSaturatedCtorNoContainer: `mk a b` (arity-2 ctor, saturated, inline).
// BEFORE: the partial `mk a` K_CLO is allocated and leaked (+1/run, the K_CLO_mk1
// container). AFTER: the saturated application emits rt_mkcon directly, no intermediate
// K_CLO, so the +1/run container leak is gone. Assert the per-run delta drops by 1 and
// output unchanged.
func TestPerceusSaturatedCtorNoContainer(t *testing.T) {
	src := perceusSatCtorSrc // a `data Pr is mk : Nat -> Nat -> Pr end` + `mk a b` inline
	got := runWasm(t, emitWith(t, cg.Wasm{}, src, "mainCtor"))
	if got != satCtorWant {
		t.Fatalf("output changed: got %q want %q", got, satCtorWant)
	}
	p := mustProgram(t, src, "mainCtor")
	counts := wasmSteadyLivePInts(t, p, 4)
	d := counts[3] - counts[2]
	if d > satCtorResidualAfter { // residual without the K_CLO_mk1 container
		t.Fatalf("saturated ctor still leaks the container: delta=%d > %d", d, satCtorResidualAfter)
	}
}
```

Define `perceusSatCtorSrc` (a 2-field datatype + a `mk a b` inline application, the args being owned values), `satCtorWant`, `satCtorResidualAfter` (measured post-fix).

- [ ] **Step 2: Run it to confirm the container leak**

Run: `go test -run TestPerceusSaturatedCtorNoContainer ./codegen/`
Expected: FAIL (the partial `mk a` K_CLO leaks +1/run).

- [ ] **Step 3: Implement the saturated-constructor direct emit**

In `codegen/wasm.go`, in `emitIn`'s `AppClosure` handling, BEFORE the generic `rt_apply` path, add a saturated-constructor recognizer (mirror `accelDispatch`'s structure):

```go
// satCtorDispatch: if `app` is a saturated application of a constructor (an AppClosure
// spine headed by a CGlobal naming a constructor, with exactly arity args), emit the
// K_CON directly (rt_mkcon + rt_con_set per arg), no intermediate K_CLO. Returns the
// result expression + true on a hit.
func (f *wasmFunc) satCtorDispatch(b *strings.Builder, app AppClosure, locals []string) (string, bool) {
	args, head, ok := unwindSpine(app)        // collect args (in order) + the CGlobal head
	if !ok { return "", false }
	c, isCtor := f.em.ctorByName[head.Name]   // a map CtorName -> CtorSpec (build it in newWasmEmitter)
	if !isCtor || len(args) != c.Arity { return "", false }
	// evaluate each arg (each moved into the con), then build the con
	vals := make([]string, len(args))
	for i, a := range args { vals[i] = f.emitIn(b, a, locals) /* bind to a local for stability */ }
	o := f.fresh()
	fmt.Fprintf(b, "    (local.set %s (call $rt_mkcon (i32.const %d) (i32.const %d) (i32.const %d)))\n",
		o, c.Tag, f.em.intern... /* name offset, as emitCtorBlock uses */, c.Arity)
	for i, v := range vals {
		fmt.Fprintf(b, "    (call $rt_con_set (local.get %s) (i32.const %d) %s)\n", o, i, v)
	}
	return "(local.get " + o + ")", true
}
```

Wire it in `emitIn`'s `AppClosure` case before the accel check or alongside it:

```go
	case AppClosure:
		if out, ok := f.satCtorDispatch(b, x, locals); ok { return out }
		if out, ok := f.accelDispatch(b, x, locals); ok { return out }
		// ... existing generic rt_apply path
```

IMPLEMENTER NOTES:
- `ctorByName` (a `map[string]CtorSpec`) must be built in `newWasmEmitter` from `cp.Datas`. The constructor name offset (`nameOff`) for `rt_mkcon` is the interned cstr the existing `emitCtorBlock` uses - reuse the same interning so output is byte-identical.
- A PARTIALLY applied constructor (fewer than arity args) does NOT match (`len(args) != c.Arity`) and falls through to the curried `emitCtorBlock` path - unchanged.
- The Perceus pass already `consumeOwning`s each arg of a recognized bare ctor spine exactly once (Task 6-pre-fix's `isCtor` dup). The saturated args here are those same arg terms; confirm each arg is moved in exactly once (no double consume, no missing dup for `mk x x` - the shared-var dup from 6-pre-fix must still fire). If the direct emit changes how args are annotated, keep the `mk x x` guard (`TestPerceusCtorMkXX` must stay green).
- OUTPUT-INVARIANCE: a saturated ctor's printed form must be byte-identical to the curried path (same tag, same field order, same name). Verify against `TestWasmConformsToJS`.

- [ ] **Step 4: Run the delta test + the full gate**

Run: `go test -run TestPerceusSaturatedCtorNoContainer ./codegen/`
Expected: PASS (no intermediate K_CLO; container leak gone; output unchanged).

Run: `go test -run 'Wasm|WASM|ARC|Perceus' ./codegen/` and `go test ./harness/ -run 'Conformance|Backend'`
Expected: PASS (TestPerceusCtorMkXX green, conformance byte-identical, all constructor programs print identically).

- [ ] **Step 5: Commit**

```bash
git add codegen/wasm.go codegen/perceus.go codegen/perceus_test.go
git commit -m "feat(wasm): emit saturated constructor applications directly (no intermediate K_CLO)"
```

---

### Task 4: `rt_big_parse` releases its per-digit temporaries

A nat literal parses through `rt_big_parse` (`codegen/wasm_runtime.go`), which allocates a fresh `K_BIG` per digit, a `$ten` constant, and intermediate `rt_nat_mul`/`big_add` results - all leaked. Release them so a nat literal does not leak ~4 blocks. This edits the 6a runtime (additive ownership only; the ARC primitives + layout are untouched).

**Files:**
- Modify: `codegen/wasm_runtime.go` (`rt_big_parse`)
- Test: `codegen/perceus_test.go`

**Interfaces:**
- Consumes: `$rt_release`/`$big_add`/`$rt_nat_mul`/`$rt_big_from_long`/`$big_alloc`.
- Produces: an `rt_big_parse` whose only surviving allocation is the returned accumulator; every intermediate (`$ten`, per-digit `K_BIG`, superseded `$acc`, `rt_nat_mul` result) is released.

- [ ] **Step 1: Write the failing delta test**

```go
// TestPerceusBignumParseTemps: a program whose main is a bare multi-digit nat literal
// (e.g. 137), so the only per-run allocation is the parse. BEFORE: each run leaks the
// parse temps (~ per-digit). AFTER: the literal parses with only the result surviving,
// which the harness releases -> steady FLAT (zero per-run increment). This is the FIRST
// residual whose receiver can reach true flat in isolation (a bare literal has no
// eliminator/ctor/fold).
func TestPerceusBignumParseTemps(t *testing.T) {
	src := perceusBignumLitSrc // `... main : Nat is 137 end`
	got := runWasm(t, emitWith(t, cg.Wasm{}, src, "mainLit"))
	if got != "137" {
		t.Fatalf("output changed: got %q want 137", got)
	}
	p := mustProgram(t, src, "mainLit")
	assertSteadyFlatInts(t, p, 4) // zero per-run increment
}
```

Define `perceusBignumLitSrc` (a `data Nat`/`builtin nat` preamble + `main : Nat is 137 end`). `assertSteadyFlatInts` asserts the numeric per-run deltas are 0 (add it if the existing `assertSteadyFlat` is string-output-based; reuse `wasmSteadyLivePInts`).

- [ ] **Step 2: Run it to confirm the parse leak**

Run: `go test -run TestPerceusBignumParseTemps ./codegen/`
Expected: FAIL (the literal leaks parse temps each run).

- [ ] **Step 3: Release the temporaries in `rt_big_parse`**

Rewrite the loop in `codegen/wasm_runtime.go` `rt_big_parse` to release each intermediate. Current:

```wat
    (local.set $acc (call $big_alloc (i32.const 0)))
    (local.set $ten (call $rt_big_from_long (i32.const 10)))
    (block $b (loop $l
      (local.set $c (i32.load8_u (local.get $p)))
      (br_if $b (i32.eqz (local.get $c)))
      (local.set $acc (call $big_add
        (call $rt_nat_mul (local.get $acc) (local.get $ten))
        (call $rt_big_from_long (i32.sub (local.get $c) (i32.const 48)))))
      (local.set $p (i32.add (local.get $p) (i32.const 1)))
      (br $l)))
    (local.get $acc))
```

Rewrite to bind and release each intermediate:

```wat
    (local.set $acc (call $big_alloc (i32.const 0)))
    (local.set $ten (call $rt_big_from_long (i32.const 10)))
    (block $b (loop $l
      (local.set $c (i32.load8_u (local.get $p)))
      (br_if $b (i32.eqz (local.get $c)))
      (local.set $mul (call $rt_nat_mul (local.get $acc) (local.get $ten)))  ;; new
      (call $rt_release (local.get $acc))                                    ;; old acc dead
      (local.set $dig (call $rt_big_from_long (i32.sub (local.get $c) (i32.const 48)))) ;; new
      (local.set $acc (call $big_add (local.get $mul) (local.get $dig)))     ;; new acc
      (call $rt_release (local.get $mul))                                    ;; mul intermediate dead
      (call $rt_release (local.get $dig))                                    ;; digit dead
      (local.set $p (i32.add (local.get $p) (i32.const 1)))
      (br $l)))
    (call $rt_release (local.get $ten))   ;; the constant ten is dead
    (local.get $acc))
```

Declare the new `$mul` and `$dig` locals. IMPLEMENTER NOTE (gate-arbitrated): `rt_nat_mul` and `big_add` must not retain their inputs (store=MOVE / pure compute returning a fresh `K_BIG`) for these releases to be balanced - confirm by reading them; if either retains an input, adjust. The FIRST iteration releases the initial `big_alloc(0)` acc (rc 1 -> 0, freed) - confirm `big_alloc(0)` returns an rc=1 block the release frees cleanly. The 6a ARC tests must stay green (they do not call `rt_big_parse`, but run them to confirm no collateral). If the gate shows a use-after-free (wrong literal value / trap), a release is too early; if it shows a leak, one is missing.

- [ ] **Step 4: Run the flat test + ARC + conformance**

Run: `go test -run TestPerceusBignumParseTemps ./codegen/`
Expected: PASS (a bare literal reaches steady FLAT - the first isolated true-flat receiver).

Run: `go test -run 'Wasm|WASM|ARC|Perceus' ./codegen/` and `go test ./harness/ -run 'Conformance|Backend'`
Expected: PASS (ARC green, every literal prints identically, conformance byte-identical).

- [ ] **Step 5: Commit**

```bash
git add codegen/wasm_runtime.go codegen/perceus_test.go
git commit -m "feat(wasm): rt_big_parse releases its per-digit temporaries (nat literals no longer leak)"
```

---

### Task 5: Re-open `PerceusBalanceable` + prove a realistic program reaches true flat (the payoff)

With the four residuals closed, a realistic inline program (pattern-match + saturated construct + nat literal + nat fold) reaches true steady-flat. Re-open `PerceusBalanceable` (remove the four now-closed exclusions; keep CBounce excluded as UNSUPPORTED), assert the corpus steady gate now covers real listings, and add the realistic payoff receiver.

**Files:**
- Modify: `codegen/perceus.go` (`PerceusBalanceable`: remove the dead-motive / saturated-ctor / nat-fold / bignum-parse exclusions; keep only the CBounce/partial exclusion, relabeled "unsupported on WASM")
- Modify: `ref_docs/wootz/R-PERCEUS.md` ("The corpus gate" + "Held for 6b-2" -> now closed; CBounce reclassified as a separate WASM-partial-support feature)
- Test: `codegen/perceus_test.go`

**Interfaces:**
- Consumes: Tasks 1-4 (all four residuals closed); the existing `PerceusBalanceable` + `TestPerceusCorpusSteady` + the corpus `cases`.
- Produces: a `PerceusBalanceable` that returns true for inline pattern-matching / construction / nat-literal / nat-fold programs (excluding only CBounce/partials); a corpus steady gate that asserts flat on the now-non-empty balanceable subset; a realistic payoff receiver at true flat.

- [ ] **Step 1: Write the realistic payoff receiver (failing until all prior tasks land)**

```go
// TestPerceusRealisticFlat: a realistic inline program exercising ALL four formerly-
// leaking constructs at once - build a 2-field constructor, match it with an inline
// eliminator, use a multi-digit nat literal, and a NatElim fold - must reach TRUE
// steady-flat (zero per-run increment) now that 6b-2 closed every residual.
func TestPerceusRealisticFlat(t *testing.T) {
	src := perceusRealisticSrc // defined below
	got := runWasm(t, emitWith(t, cg.Wasm{}, src, "mainReal"))
	if got != realisticWant {
		t.Fatalf("output changed: got %q want %q", got, realisticWant)
	}
	p := mustProgram(t, src, "mainReal")
	assertSteadyFlatInts(t, p, 5) // ZERO per-run increment across runs 2..5
}
```

Define `perceusRealisticSrc` as a program that, in its per-run main path, (a) parses a multi-digit nat literal, (b) builds a 2-field constructor, (c) inline-matches it via the generated eliminator, and (d) runs a `NatElim` fold - and `realisticWant` (its correct output). This is the program that was impossible to make flat before 6b-2.

- [ ] **Step 2: Run it - confirm it now reaches flat (post Tasks 1-4)**

Run: `go test -run TestPerceusRealisticFlat ./codegen/`
Expected: PASS if Tasks 1-4 are complete (zero per-run increment). If it still leaks, the residual is in whichever task's path the delta points to - fix that task, do not weaken this assertion.

- [ ] **Step 3: Re-open `PerceusBalanceable`**

In `codegen/perceus.go`, edit `PerceusBalanceable` (and its `cirUnbalanceable` scan): REMOVE the exclusion arms for (a) inline general eliminators (dead-motive - closed by Task 2), (b) saturated arity>=2 constructors (container - closed by Task 3), (c) inline nat-fold (closed by Task 1), (d) bignum-literal temps (closed by Task 4). KEEP the CBounce exclusion, relabeled: `// CBounce: partials are UNSUPPORTED on WASM (no emitIn case), not a leak. Excluded as unsupported, see R-PERCEUS.` Update the doc comment so the predicate now means "in the steady-flat fragment" = everything except WASM-unsupported partials.

- [ ] **Step 4: Strengthen `TestPerceusCorpusSteady` to require real coverage**

The Task 6-gate `TestPerceusCorpusSteady` logged "0 balanced". Now it must cover real listings. Update it to ASSERT a non-zero balanced count and flat on each:

```go
	// After 6b-2, the balanceable subset is non-empty: real pattern-matching /
	// construction / nat programs reach steady-flat. Assert coverage is real.
	if nBalanced == 0 {
		t.Fatalf("6b-2 should make real corpus listings balanceable; got 0")
	}
```

(Confirm which corpus `cases` are now balanceable - those without `partial`/CBounce. If the existing `cases` are all nat-arithmetic / constructor programs, they should now balance; if any still leaks, the residual points at the responsible task.)

- [ ] **Step 5: Run the full gate**

Run: `go test -run 'Wasm|WASM|ARC|Perceus' ./codegen/`
Expected: PASS (the realistic receiver flat; the corpus steady gate covers >0 real listings at flat; every prior receiver green).

Run: `go test ./codegen/` and `go test ./harness/ -run 'Conformance|Backend'` and `go build ./...` and `go vet ./codegen/`
Expected: all green, conformance byte-identical.

- [ ] **Step 6: Finalize R-PERCEUS.md + commit**

In `ref_docs/wootz/R-PERCEUS.md`: change "Held for Plan 6b-2" to record the four residuals as CLOSED (with their tasks), and RECLASSIFY CBounce as "WASM partial/trampoline support - a separate unsupported-feature plan, not a leak." Add a "6b-2 outcome" line: realistic inline programs reach steady-flat; the only excluded class is WASM-unsupported partials. Verify no dashes.

```bash
git add codegen/perceus.go codegen/perceus_test.go ref_docs/wootz/R-PERCEUS.md
git commit -m "feat(wasm): re-open PerceusBalanceable - real programs reach steady-flat (6b-2 residuals closed)"
```

---

## Self-Review

**Spec coverage (against the 5 named 6b residuals):**
- dead-motive: Task 2 (remove the carve-out), enabled by Task 1. Covered.
- arity>=2 ctor container: Task 3 (saturated-ctor direct emit). Covered.
- nat-fold temps: Task 1 (emitNatFold ownership). Covered.
- bignum-parse (rt_big_parse) temps: Task 4. Covered.
- CBounce/nat-fold "trampoline": RECLASSIFIED - CBounce is an unsupported WASM feature (no emitIn case), not a leak; explicitly out of scope, kept excluded, documented as a separate future plan. Covered (as a reclassification + exclusion).
- The payoff: Task 5 (re-open PerceusBalanceable + a realistic program at true flat + corpus coverage > 0). Covered.

**Placeholder scan:** The WAT rewrites (Tasks 1, 4) and the saturated-ctor emit (Task 3) are concrete against the real functions (`emitNatFold`, `rt_big_parse`, `emitCtorBlock`, `accelDispatch`). The exact retain/release PLACEMENT is flagged gate-arbitrated (the steady-delta test is the oracle; missing release = leak, early release = UAF/wrong-output) - this is a measure-and-tune instruction with the invariant stated (the loop/parse returns `$live` to steady, exactly one owned result survives), not a TBD. The per-task numeric bounds (`natFoldMaxResidual`, `inlineElimResidualAfterMotive`, `satCtorResidualAfter`) are "measure after the fix, assert independence from the leaking dimension" - the implementer records the observed constant; the test's TEETH are that the delta no longer GROWS with the residual's dimension.

**Type consistency:** `wasmSteadyLivePInts` / `assertSteadyFlatInts` (numeric variants of the existing string helpers) are introduced in Task 1 and reused in Tasks 2-5. `ctorByName` (map built in `newWasmEmitter`) + `satCtorDispatch` + `unwindSpine` are introduced in Task 3. `PerceusBalanceable` is edited in Task 5 consistently with its 6b definition. The measurement discipline (per-run delta in Tasks 1-3, true flat in Tasks 4-5) is uniform.

**Two honest notes for the author:**
1. The residuals COMPOUND, so Tasks 1-3 measure a DELTA (the residual's contribution removed), not absolute flat; only Task 4's bare-literal receiver and Task 5's realistic receiver assert true zero-per-run flat. This is the honest structure given the compounding; the per-task delta tests still have teeth (the delta must stop growing with the residual's dimension).
2. Task 1 and Task 2 are COUPLED: Task 1 makes the nat-fold step own its counter, which is the precondition that makes removing the carve-out (Task 2) safe. They must land in order; if Task 2 shows a UAF in a nat-arithmetic program, the fix is in Task 1's retain placement, not re-adding the carve-out. Task 3 (ctor) and Task 4 (parse) are independent of each other and of 1/2.

## Execution Handoff

Plan complete and saved to `docs/superpowers/plans/2026-06-27-perceus-6b2-leak-residuals.md`. Two execution options:

1. **Subagent-Driven (recommended)** - I dispatch a fresh subagent per task, review between tasks, fast iteration. Given the gate-arbitrated WAT ownership (Tasks 1, 4) and the saturated-ctor emit (Task 3), per-task review + a final whole-branch review (which caught the 6b use-after-free) is the safety net.
2. **Inline Execution** - Execute tasks in this session using executing-plans, batch execution with checkpoints.

Which approach?
