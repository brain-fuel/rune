# Perceus Accel-Operand Residual Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the WASM Perceus accel-operand leak so an accel op (`addN`/`mulN`/`monusN`) applied to an owned-local operand (`addN n n`, `addN ih (succ n)`) frees that operand, making accel-registering programs reach steady-flat and join the `PerceusBalanceable` flat fragment.

**Architecture:** WASM-only Perceus ARC. `annotateBareSpine` (codegen/perceus.go) already routes every recognized accel-spine leaf operand through `consumeOwning`, so each operand reaching `accelDispatch` (codegen/wasm.go) is a PRIVATE OWNED reference the borrow-reading accel op must free. The current `accelDispatch` frees only `freshOwned` operand forms (fresh allocations), leaking bare `CVar` (owned local), dup'd `CEnv`, and let-bound borrowed roots. Fix: release BOTH operands unconditionally — privacy is guaranteed by the shared-owned-local dup in `annotateBareSpine` plus the enclosing scope's multi-use dups (the exact dual of `succ_code` freeing its arg). Then drop the wholesale accel exclusion in `PerceusBalanceable`.

**Tech Stack:** Go; WAT emitted text; `wasmtime` test runner; existing steady-flat + 8-backend conformance harness.

## Global Constraints

- NO em-dashes or en-dashes anywhere (code, comments, commits, docs). Use `--` in prose where a dash is unavoidable, matching existing file style.
- Kernel FROZEN: no change to core/store, no hash change (current 0x06). This is internal WASM codegen only.
- 6a WASM runtime is FROZEN (`store = MOVE`); do NOT edit codegen/wasm_runtime.go in this plan. The fix is emitter + annotation + predicate only.
- Output must stay BYTE-IDENTICAL across all backends: `rt_release` adjusts refcounts only, never the returned result expression.
- Conventional Commits. End every commit message with the trailer:
  `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`
- This is the C1/C-1 memory-safety class (a double-free here previously survived per-task review and was caught only by the final whole-branch review). Every receiver must assert OUTPUT CORRECTNESS in addition to the leak/flat property, so a use-after-free or double-free surfaces as wrong output, not just a refcount delta.

**Memory model facts the implementer must rely on (do NOT re-derive):**
- After Perceus, the accel spine is bare `AppClosure(AppClosure(CGlobal, a'), b')` where `a'`, `b'` are the ANNOTATED operands; any outer shared-owned-local `CDup` is peeled by `emitIn`'s `CDup` case before the `AppClosure` dispatch runs, so `accelMatchC` still matches and yields the annotated operands.
- `consumeOwning` makes every operand owned: a bare owned `CVar` stays bare (single private ref); a `CEnv` becomes `CDup{CEnv,CEnv}` (fresh owned ref); a `CGlobal`/`CField`/`CFst`/`CSnd` becomes `CLet{$own, leaf, CDup{CVar0,CVar0}}` (fresh owned ref). So `emitIn` of any operand yields a value whose ownership this op holds.
- A CVar operand used ALSO outside the spine is dup'd by the enclosing scope (`annotate` AppClosure/CLet multi-use dup); a CVar used in BOTH operand sub-spines is dup'd once by `annotateBareSpine`. So `n` consumes are matched by exactly `n` live references before the op. `ownScope` does NOT also drop an owned local that the spine uses (`cirUsesArg` is true), so the accel release is that local's single drop -- no double-drop.

---

### Task 1: accelDispatch frees both operands unconditionally

**Files:**
- Modify: `codegen/wasm.go` (function `accelDispatch` ~lines 815-850; helper `freshOwned` ~lines 859-866)
- Test: `codegen/perceus_test.go` (new adversarial receivers)

**Interfaces:**
- Consumes: `accelMatchC(app, f.em.accel) (op core.NatOp, a, bb CIr, ok bool)`; `f.emitIn(b, t, locals) string`; `f.fresh() string`.
- Produces: no signature change to `accelDispatch`. Deletes the package-level `func freshOwned(t CIr) bool` (its only caller is `accelDispatch`).

- [ ] **Step 1: Write the failing adversarial receivers**

Add to `codegen/perceus_test.go`. These cover every operand-ownership shape: shared owned local, two distinct owned locals, borrowed-root operands, accel-in-fold-step, and accel-on-fresh (regression that the existing fresh release still holds). Each asserts BOTH steady-flat AND correct output (a double-free / UAF shows as wrong output).

```go
// --- Frontier: accel-operand ownership (Task 1) ---

// accelSharedSrc: an accel op on a SHARED owned local (addN n n). annotateBareSpine
// inserts one shared-owned-local dup (rc 1->2); accelDispatch must free BOTH operands
// (2->0). A double-free here corrupts output; a missed free leaks.
const accelSharedSrc = accelNatSrc + `
shared : Nat -> Nat is fn (n : Nat) is addN n n end end
mainShared : Nat is shared 3000 end
`

func TestPerceusAccelSharedOperandFlat(t *testing.T) {
	p := mustProgram(t, accelSharedSrc, "mainShared")
	assertSteadyFlatInts(t, p, 5)
	if got := runWasm(t, emitWith(t, cg.Wasm{}, accelSharedSrc, "mainShared")); got != "6000" {
		t.Fatalf("addN n n with n=3000: got %q, want 6000", got)
	}
}

// accelDistinctSrc: two DISTINCT owned locals, each used once (mulN a b). Each operand
// is a private owned ref; accelDispatch frees each once.
const accelDistinctSrc = accelNatSrc + `
distinct : Nat -> Nat -> Nat is fn (a : Nat) (b : Nat) is mulN a b end end
mainDistinct : Nat is distinct 100 100 end
`

func TestPerceusAccelDistinctOperandsFlat(t *testing.T) {
	p := mustProgram(t, accelDistinctSrc, "mainDistinct")
	assertSteadyFlatInts(t, p, 5)
	if got := runWasm(t, emitWith(t, cg.Wasm{}, accelDistinctSrc, "mainDistinct")); got != "10000" {
		t.Fatalf("mulN a b with 100,100: got %q, want 10000", got)
	}
}

// accelLiveAfterSrc: an owned local used in the accel AND again after it (addN n n then
// add the result to n). The enclosing scope dups n for the later use, so the accel
// release frees only the spine's private ref -- the later use must still see a live n.
// A premature free here is a use-after-free (wrong output or trap).
const accelLiveAfterSrc = accelNatSrc + `
liveAfter : Nat -> Nat is fn (n : Nat) is addN (addN n n) n end end
mainLiveAfter : Nat is liveAfter 1000 end
`

func TestPerceusAccelLiveAfterNoUAF(t *testing.T) {
	p := mustProgram(t, accelLiveAfterSrc, "mainLiveAfter")
	assertSteadyFlatInts(t, p, 5)
	// addN (addN 1000 1000) 1000 = 3000.
	if got := runWasm(t, emitWith(t, cg.Wasm{}, accelLiveAfterSrc, "mainLiveAfter")); got != "3000" {
		t.Fatalf("addN (addN n n) n with n=1000: got %q, want 3000", got)
	}
}

// accelArmFlatSrc: the canonical frontier shape -- an accel op consumed INSIDE a
// NatElim fold step (addN ih (succ n)). ih is an owned step local (released, killing
// the per-iteration leak); (succ n) is fresh (released). Previously leaked per
// iteration; must now be steady-flat.
const accelArmFlatSrc = accelNatSrc + `
armUse : Nat -> Nat is
  fn (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) (addN n n) (fn (k : Nat) (ih : Nat) is addN ih (succ n) end) n
  end
end
mainArm2 : Nat is armUse 3 end
`

func TestPerceusAccelArmStepFlat(t *testing.T) {
	p := mustProgram(t, accelArmFlatSrc, "mainArm2")
	assertSteadyFlatInts(t, p, 5)
	// Same shape/value as the old frontier program: result 18.
	if got := runWasm(t, emitWith(t, cg.Wasm{}, accelArmFlatSrc, "mainArm2")); got != "18" {
		t.Fatalf("accel-in-step output: got %q, want 18", got)
	}
}

// accelFreshSrc: a regression guard that accel-on-fresh operands (mulN of two fresh
// accel results) still reach flat after freshOwned is removed.
const accelFreshSrc = accelNatSrc + `
freshOp : Nat is mulN (addN 10 10) (addN 20 20) end
`

func TestPerceusAccelFreshOperandsFlat(t *testing.T) {
	p := mustProgram(t, accelFreshSrc, "freshOp")
	assertSteadyFlatInts(t, p, 5)
	// (10+10) * (20+20) = 20*40 = 800.
	if got := runWasm(t, emitWith(t, cg.Wasm{}, accelFreshSrc, "freshOp")); got != "800" {
		t.Fatalf("mulN (addN 10 10) (addN 20 20): got %q, want 800", got)
	}
}
```

- [ ] **Step 2: Run the new receivers to verify they fail**

Run: `go test ./codegen/ -run 'TestPerceusAccel(SharedOperand|DistinctOperands|LiveAfter|ArmStep|FreshOperands)' -count=1 -timeout 20m -v`
Expected: `TestPerceusAccelSharedOperandFlat`, `...LiveAfterNoUAF`, `...ArmStepFlat` FAIL (steady not flat -- per-run leak). `...DistinctOperandsFlat` and `...FreshOperandsFlat` MAY already pass (operands are fresh or independent) -- that is fine; the leak cases are the teeth.

- [ ] **Step 3: Make accelDispatch release both operands unconditionally; delete freshOwned**

In `codegen/wasm.go`, replace the trailing `freshOwned`-gated releases in `accelDispatch` (the block from the comment `// The accel op BORROW-reads its operands` through the two `if freshOwned(...)` blocks) with an unconditional release of both operands:

```go
	fmt.Fprintf(b, "    (local.set %s (call $%s %s %s))\n", r, opName, ea, eb)
	// The accel op BORROW-reads its two operands (rt_nat_* read both inputs and
	// return a fresh K_BIG) and has now read them, so both operands are dead. Every
	// operand reaching here is a PRIVATE OWNED reference: annotateBareSpine routes each
	// accel leaf through consumeOwning (a bare owned CVar stays a single private ref; a
	// borrowed CEnv/CGlobal/projection is dup'd to a fresh owned ref), and a local
	// consumed in both operand sub-spines is dup'd once by annotateBareSpine's
	// shared-owned-local dup, while a local also used outside the spine is dup'd by the
	// enclosing scope. So exactly one live reference reaches the op per operand, and the
	// op must free both -- the accel dual of succ_code freeing its $arg under PATH B.
	// Releasing unconditionally (not just fresh-producing forms) is what closes the
	// owned-CVar / dup'd-borrow operand leak; a borrowed root is never reached bare here
	// because consumeOwning already replaced it with an owned dup.
	fmt.Fprintf(b, "    (call $rt_release %s)\n", ea)
	fmt.Fprintf(b, "    (call $rt_release %s)\n", eb)
	return "(local.get " + r + ")", true
}
```

Then DELETE the now-unused `func freshOwned(t CIr) bool` and its doc comment (its only caller was `accelDispatch`). Confirm no other reference remains.

- [ ] **Step 4: Run the new receivers to verify they pass**

Run: `go test ./codegen/ -run 'TestPerceusAccel(SharedOperand|DistinctOperands|LiveAfter|ArmStep|FreshOperands)' -count=1 -timeout 20m -v`
Expected: all five PASS (steady-flat AND correct output).

- [ ] **Step 5: Confirm freshOwned removal compiles and no stale references**

Run: `go vet ./codegen/ && grep -rn "freshOwned" codegen/`
Expected: vet clean; `grep` returns NOTHING (helper fully removed).

- [ ] **Step 6: Run the existing accel + ARC + steady receivers (no regression)**

Run: `go test ./codegen/ -run 'TestPerceus|TestWasm' -count=1 -timeout 40m`
Expected: PASS. NOTE: `TestPerceusAccelFrontier` and `TestPerceusCorpus*` (the accel-exclusion assertions) are updated in Task 2 -- if `TestPerceusAccelFrontier` now FAILS here because the program no longer leaks, that is the EXPECTED signal the leak is closed; record it and fix it in Task 2. Do not change it in Task 1.

- [ ] **Step 7: Commit**

```bash
git add codegen/wasm.go codegen/perceus_test.go
git commit -m "$(cat <<'EOF'
fix(wasm): accelDispatch frees both owned operands (close accel-operand leak)

Every accel-spine operand reaching accelDispatch is a private owned reference
(annotateBareSpine routes each leaf through consumeOwning; shared/multi-use
locals are dup'd), so the borrow-reading accel op must free both -- the dual of
succ_code freeing its arg. Release unconditionally instead of only fresh forms;
delete the now-dead freshOwned predicate. Closes the owned-CVar / dup'd-borrow
operand leak (addN n n, accel-in-step). Output byte-identical.

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>
EOF
)"
```

---

### Task 2: Re-open PerceusBalanceable to accel programs

**Files:**
- Modify: `codegen/perceus.go` (function `PerceusBalanceable` ~lines 710-715: the wholesale accel exclusion)
- Modify: `codegen/perceus_test.go` (rewrite `TestPerceusAccelFrontier` into a flat/balanceable receiver; add an accel-corpus steady receiver)
- Modify: `ref_docs/wootz/R-PERCEUS.md` (mark the accel-operand residual CLOSED; update the corpus balanced/skipped table)

**Interfaces:**
- Consumes: `PerceusBalanceable(p Program) bool`; `cirUnbalanceable(t CIr, natElim string) bool` (already excludes `CBounce` and over-applied `NatElimSpine`); `mustProgram`, `assertSteadyFlatInts`, `wasmSteadyLivePInts`, `runWasm`, `emitWith`, `accelNatSrc`.
- Produces: no signature changes. `PerceusBalanceable` returns `true` for accel-registering programs whose bodies are otherwise flat.

- [ ] **Step 1: Rewrite the frontier receiver to assert balanceable + flat**

In `codegen/perceus_test.go`, replace `TestPerceusAccelFrontier` (the test asserting accel programs are EXCLUDED and that `perceusAccelArmSrc` leaks) with a receiver asserting the accel program is now IN the flat fragment and flat. Reuse the existing `perceusAccelArmSrc` source (the `armUse`/`mainArm` program already defined above it):

```go
// TestPerceusAccelInFlatFragment: with the accel-operand leak closed (accelDispatch
// frees both owned operands), an accel-registering program whose bodies are otherwise
// flat is now INSIDE the balanceable fragment and reaches steady-flat. This is the
// flipped former-frontier receiver (was TestPerceusAccelFrontier).
func TestPerceusAccelInFlatFragment(t *testing.T) {
	p := mustProgram(t, perceusAccelArmSrc, "mainArm")
	if !cg.PerceusBalanceable(p) {
		t.Fatalf("accel-operand leak closed: an accel program with flat bodies must be IN the flat fragment")
	}
	assertSteadyFlatInts(t, p, 5)
	if got := runWasm(t, emitWith(t, cg.Wasm{}, perceusAccelArmSrc, "mainArm")); got != "18" {
		t.Fatalf("accel-in-step output: got %q, want 18", got)
	}
}

// TestPerceusAccelCorpusFlat: the accel conformance listings (sum/prod/diff/diffZero)
// now reach steady-flat (their addN/mulN/monusN fold-def bodies free their accel
// operands, including mulN's accel-in-step `addN n ih`).
func TestPerceusAccelCorpusFlat(t *testing.T) {
	cases := []struct{ main, want string }{
		{"sum", "8000"}, {"prod", "10000"}, {"diff", "5"}, {"diffZero", "0"},
	}
	for _, c := range cases {
		c := c
		t.Run(c.main, func(t *testing.T) {
			p := mustProgram(t, accelNatSrc, c.main)
			if !cg.PerceusBalanceable(p) {
				t.Fatalf("accel listing %s must be balanceable", c.main)
			}
			assertSteadyFlatInts(t, p, 5)
			if got := runWasm(t, emitWith(t, cg.Wasm{}, accelNatSrc, c.main)); got != c.want {
				t.Fatalf("accel listing %s: got %q, want %s", c.main, got, c.want)
			}
		})
	}
}
```

NOTE: the constants `perceusAccelArmSrc` and its doc comment remain in the file (still referenced). Only the OLD `TestPerceusAccelFrontier` function body is replaced. If any other test (e.g. a corpus balanced/skipped count assertion) hard-codes that accel programs are skipped, update that expectation here too -- search first (Step 2).

- [ ] **Step 2: Find any test that asserts accel programs are skipped/excluded**

Run: `grep -n "Nat.Ops\|accel.*excl\|excl.*accel\|skipped\|balanced 3\|3->6\|6 balanced" codegen/perceus_test.go`
Expected: identify any corpus balanced/skipped COUNT receiver (e.g. a test asserting "6 balanced, 4 skipped"). If one exists and counts accel programs as skipped, note its name -- it must be updated in Step 4 after the predicate change so its counts reflect accel programs now balanceable.

- [ ] **Step 3: Run the rewritten receivers to verify they fail (predicate still excludes accel)**

Run: `go test ./codegen/ -run 'TestPerceusAccelInFlatFragment|TestPerceusAccelCorpusFlat' -count=1 -timeout 20m -v`
Expected: FAIL at the `PerceusBalanceable` assertion (`must be IN the flat fragment`) -- the wholesale accel exclusion is still present.

- [ ] **Step 4: Remove the wholesale accel exclusion**

In `codegen/perceus.go`, delete the early-return accel exclusion at the top of `PerceusBalanceable`:

```go
	// Accel-op programs are excluded wholesale (the accel-operand leak above is not
	// statically separable from flat accel uses).
	if p.Nat != nil && len(p.Nat.Ops) > 0 {
		return false
	}
```

Replace it with a brief comment recording that accel programs are now admitted and rely on the per-spine `cirUnbalanceable` scan (which still excludes `CBounce` and over-applied `NatElimSpine`):

```go
	// Accel-op programs are now admitted: accelDispatch frees both owned operands
	// (the accel-operand leak is closed), so a flat accel use reaches steady-flat. Any
	// remaining unbalanceable construct (CBounce, over-applied NatElim) is still caught
	// per-spine by cirUnbalanceable below.
```

If Step 2 found a corpus count receiver that excludes accel programs, update its expected counts now to reflect accel listings becoming balanceable.

- [ ] **Step 5: Run the rewritten receivers to verify they pass**

Run: `go test ./codegen/ -run 'TestPerceusAccelInFlatFragment|TestPerceusAccelCorpusFlat' -count=1 -timeout 20m -v`
Expected: PASS (balanceable AND steady-flat AND correct output).

- [ ] **Step 6: Update R-PERCEUS.md**

In `ref_docs/wootz/R-PERCEUS.md`: change the documented "accel-operand residual" / wholesale-accel-exclusion section to state it is CLOSED (accelDispatch frees both owned operands; mechanism: every operand is a private owned ref via consumeOwning + shared/multi-use dups). Update the corpus balanced/skipped table so the accel listings (sum/prod/diff/diffZero) move from skipped to balanced. Leave `CBounce`/partials listed as the remaining frontier. Match the file's existing dash convention (`--`, never em-dash).

- [ ] **Step 7: Full codegen + conformance gate (byte-identical, no leak)**

Run: `go test ./codegen/ -count=1 -timeout 40m`
Expected: PASS. Confirms the accel fix is byte-identical across the 8-backend conformance corpus and every Perceus/ARC/steady receiver is green.

- [ ] **Step 8: Commit**

```bash
git add codegen/perceus.go codegen/perceus_test.go ref_docs/wootz/R-PERCEUS.md
git commit -m "$(cat <<'EOF'
feat(wasm): re-open PerceusBalanceable to accel programs (operand leak closed)

Drop the wholesale accel-program exclusion: with accelDispatch freeing both
owned operands, a flat accel use reaches steady-flat, and any remaining
unbalanceable construct (CBounce, over-applied NatElim) is still caught
per-spine by cirUnbalanceable. Flip the accel frontier receiver to assert
balanceable+flat; add an accel-corpus steady receiver (sum/prod/diff/diffZero).
Document the accel-operand residual CLOSED in R-PERCEUS.

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>
EOF
)"
```

---

## Self-Review

- **Spec coverage:** Task 1 closes the emitter-side leak (the residual's mechanism); Task 2 admits accel programs to the flat fragment + documents closure. Together they discharge the "accel-operand residual" frontier in full.
- **Type consistency:** `accelDispatch`, `accelMatchC`, `PerceusBalanceable`, `cirUnbalanceable`, `consumeOwning`, `assertSteadyFlatInts`, `wasmSteadyLivePInts`, `mustProgram`, `emitWith`, `runWasm` all used with their existing signatures. `freshOwned` deleted (sole caller removed).
- **Adversarial coverage (memory-safety class):** shared owned local (double-free risk), distinct owned, live-after (UAF risk), accel-in-step (the per-iteration leak), accel-on-fresh (regression). Each asserts correct OUTPUT so a double-free/UAF fails the test, not just a refcount delta.
- **Frozen-surface check:** no edit to wasm_runtime.go, no kernel/hash change; releases adjust refcounts only so output stays byte-identical.

## Execution Handoff

Subagent-Driven: fresh implementer per task, task review (spec + quality) after each, broad whole-branch review (opus, adversarial UAF hunt) at the end, then finishing-a-development-branch. The final whole-branch review is load-bearing for this class -- it caught the C1 double-free and C-1 over-applied leak that per-task reviews missed.
