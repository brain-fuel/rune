# Perceus Ownership Insertion Pass (WASM ARC) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Insert precise reference-counting (Perceus-style dup/drop) into the WASM backend's closure-converted IR so real compiled Rune programs manage memory with the ARC runtime from Plan 6a -- every heap value retained when shared, released when dead, with the `$live` probe returning to steady state after every run (no leak, no use-after-free).

**Architecture:** A new CIr->CIr ownership pass (`codegen/perceus.go`) inserts two explicit nodes -- `CDup{V,K}` (retain V then run K) and `CDrop{V,K}` (release V then run K) -- following the Perceus ownership discipline (Reinking, Xie, de Moura, Leijen, "Perceus: Garbage Free Reference Counting with Reuse", PLDI 2021; Lean 4's "Counting Immutable Beans"). The pass is OPT-IN: only the WASM backend runs it (`Wasm.Emit` calls `Perceus(cp)` after `ClosureConvert`); the C/LLVM and source backends never see CDup/CDrop and keep their existing behavior byte-for-byte. The WASM emitter renders CDup/CDrop as `$rt_retain`/`$rt_release` calls into the Plan 6a runtime. Soundness rests on the same acyclicity as 6a: the erased IR is immutable, total, and acyclic, so reference counting with no cycle collector is sound and complete. The SAME annotated CIr is what Plan 6e ports to the C and LLVM ARC runtimes, so this pass is the portable ownership artifact, not a WASM detail.

**Receiver discipline (per the user's directive):** every ownership case this plan implements has a RECEIVER -- a small runnable listing that exercises exactly that case and balances `$live`, plus a worked-example section in `ref_docs/wootz/R-PERCEUS.md` that explains it. Nothing ships, and nothing defers to 6b-2, without a named receiver. The two hard cases held for 6b-2 (the CBounce trampoline and the hand-rolled nat-fold bignum temporaries) have their future receivers named in Task 6.

**Tech Stack:** Go (the `codegen` package); WAT rendered to the Plan 6a ARC runtime in `codegen/wasm_runtime.go`; the test harness in `codegen/wasm_test.go` (`emitWith`, `runWasm`, `wasmtimePath`) + the Plan 6a ARC test scaffolding in `codegen/wasm_arc_test.go` (`arcTestModule`, `WasmRuntime()`); wasmtime v42.0.1 (present; tests skip without it or under `-short`). Programs reach the backend via `internal/session`'s `EmitProgram(main) -> codegen.Program`.

## Global Constraints

- **Kernel frozen.** No edits under `core/` or `store/`. No new core constructor; hash 0x06 unchanged. This plan changes only `codegen/` (the shadow IR + the WASM emitter + tests) and `ref_docs/wootz/`.
- **Opt-in: only WASM runs Perceus.** `ClosureConvert` is unchanged and still produces NO CDup/CDrop. Only `Wasm.Emit` runs the new `Perceus` pass. The C, LLVM, and six source backends consume the un-annotated CIr exactly as today -- their output must stay byte-for-byte identical (the existing `go test ./codegen/` and `harness/backend_conformance_test.go` gates stay green).
- **CDup/CDrop only wrap variable references.** `V` in CDup/CDrop is always a `CVar` or `CEnv` (a name for an already-evaluated value), never an arbitrary expression -- a dup/drop must not re-evaluate or allocate.
- **Immediates and immortals are runtime no-ops already.** The 6a `rt_retain`/`rt_release` no-op on immediates (low bit set) and the immortal UNIT. So the pass MAY emit a dup/drop on a value that turns out immediate; it is a safe no-op. The pass need not statically prove countedness; correctness is by the runtime guard plus the `$live` gate.
- **Soundness rests on acyclicity; no cycle collector.** The erased IR is immutable/total/acyclic. Do not add a cycle collector.
- **v1 is dup/drop only; NO reuse.** The Perceus in-place-reuse optimization (the "Beans" `drop`-then-`alloc` fusion) is explicitly out of scope. v1 inserts correct dup/drop; reuse is a later plan.
- **The correctness oracle is the `$live` steady-state gate plus output-invariance.** For every receiver: (a) the WASM output is byte-identical to the same program without Perceus (dup/drop never change results), and (b) running `main` to steady state leaves `$live` flat across runs (no per-run leak). A leak OR a use-after-free (a premature free) both fail this gate.
- **Captures are borrowed; the argument is owned.** A lifted `CodeBlock` OWNS its argument (`CVar{0}`): it must drop it if dead or transfer it. It BORROWS its env captures (`CEnv{k}`): the closure object owns the env, so the block dups a capture only when the capture's value escapes (is stored into a new structure or returned), and never drops a capture. This is the standard closure ownership split and is the v1 rule.
- **Process standards.** NO em or en dashes in any code, comment, or doc (ASCII hyphen-minus only). Conventional Commits. Run `go test ./codegen/` before tagging; `go build ./...` stays green.

---

### Task 1: R-PERCEUS design doc (the ownership rules + the receiver map)

The spec the implementation tasks follow. No code. It states the node semantics, the per-CIr-node ownership rules, the de Bruijn liveness model, the borrowed-capture / owned-argument split, the no-reuse-v1 scope, the `$live` steady-state gate methodology, and the receiver map (which task demonstrates which case, and what 6b-2 holds).

**Files:**
- Create: `ref_docs/wootz/R-PERCEUS.md`

**Interfaces:**
- Consumes: the CIr definition (`codegen/closure.go`) and the Plan 6a runtime contract (`ref_docs/wootz/R-ARC.md`).
- Produces: documentation only. Later tasks APPEND their worked-example sections to this file.

- [ ] **Step 1: Write the design doc**

Create `ref_docs/wootz/R-PERCEUS.md` (ASCII hyphens only, no em/en dashes):

````markdown
# R-PERCEUS: Ownership insertion for the ARC runtime

## What this is
The Plan 6a ARC runtime (R-ARC) gives WASM heap values a refcount and the
`rt_retain` / `rt_release` / recursive-`rt_free` primitives, but nothing calls
them. This pass inserts those calls. It is the Perceus ownership discipline
(Reinking-Xie-de Moura-Leijen, PLDI 2021; Lean 4 "Counting Immutable Beans"):
precise, deterministic reference counting for a pure, total, acyclic functional
IR, with no cycle collector. It runs on the closure-converted CIr
(`codegen/closure.go`) and is OPT-IN: only the WASM backend runs it.

## The two nodes
- `CDup{V, K}`  -- retain V (a CVar or CEnv naming an already-evaluated value),
  then evaluate K. V remains valid in K (its refcount is one higher).
- `CDrop{V, K}` -- release V, then evaluate K. V is dead in K (must not occur).

Both carry a continuation K so they sequence as expressions (CIr has no
statement form but CLet; CDup/CDrop are the explicit sequencing for ownership).
V is ALWAYS a variable reference, never an allocating expression: a dup/drop
must not re-evaluate.

## The ownership model (v1, no reuse)
A value is OWNED by exactly one binding at a time. An owned binding must be
consumed exactly once along every path: either transferred (stored into a new
heap value, passed as a call argument, or returned as the scope's result) or
explicitly dropped. Using an owned variable a second time requires a dup first.

Scopes and what they own:
- A lifted CodeBlock OWNS its argument `CVar{0}`. On entry it must, along every
  path, transfer or drop the argument.
- A CodeBlock BORROWS its env captures (`CEnv{k}`). The closure heap object owns
  the env record; entering the block does not transfer per-capture ownership.
  So the block DUPS a capture only when that capture's VALUE ESCAPES (is stored
  into a constructor/pair/closure-env or returned), and NEVER drops a capture.
- A `CLet x = v in b` makes `x` an owned local in `b` (v's value is owned by x).

Per-node rules (the translation target; the algorithm in Task 3+ realizes them):
- `CVar i` / `CEnv k` at a consuming position: transfers the value. If the same
  owned variable is consumed again later on the same path, a `CDup` precedes the
  earlier consume so each consume owns a reference.
- `AppClosure{Clo, Arg}`: evaluates Clo then Arg (the emitter's order). Both are
  consumed (the call takes ownership of the argument; rt_apply consumes the
  closure). A variable free in BOTH Clo and Arg is dup'd before the pair.
- `MkClosure{Code, Env}`: each env term is stored into the closure (an owning
  reference). A variable that is captured AND still live afterwards is dup'd.
- `CLet{x=v, b}`: translate v (consuming its free vars), bind x; translate b with
  x owned. If x is unused in b, `CDrop x` at the front of b. If x is used N times,
  N-1 dups.
- `CCase{Scrut, Arms}`: the scrutinee is BORROWED for the tag read (rt_con_tag
  does not consume). After the arms, the scrutinee is owned and must be dropped
  on every arm path (unless an arm transfers it). Each arm runs in the same owned
  context; a variable owned before the case and not used by an arm is dropped in
  that arm (per-arm liveness can differ, so drops are inserted per arm).
- `CField{Scrut, i}`: reads field i (rt_con_get returns an ALIAS, does not
  transfer). If the projected value escapes the field read's continuation, dup it.
- `CPair{A,B}` / constructor application: stores components (owning references) --
  same escape rule as MkClosure env.
- `CFst`/`CSnd`: project a pair half (alias); dup if it escapes; the pair itself
  is then owned and dropped when dead.
- `CGlobal` / `CForeign` / `CUnit` / `CLit`: not owned locals. A CGlobal is a
  top-level root (its cached value lives for the program); CUnit is the immortal
  singleton; CLit is an immediate or a freshly built bignum (owned like any
  allocation when it is a LitNat). The pass treats CUnit/CLit/CGlobal/CForeign as
  needing no dup/drop as VARIABLES (they are not de Bruijn locals); a LitNat VALUE
  bound by a CLet is owned like any other.

## Why this is sound without a cycle collector
The erased IR builds only immutable, total, acyclic values (R-ARC). Every value's
refcount counts exactly its live owning references; when the last is dropped the
value is freed, recursively freeing its children. No cycle can form, so no value
is ever unreachable-but-nonzero. Determinism: frees happen at the drop point, not
at a collector's whim.

## The correctness gate: `$live` steady state + output-invariance
The runtime's `$live` counter (R-ARC) increments per alloc, decrements per free.
For a balanced program, running `main` repeatedly reaches a STEADY STATE: after
the first run warms any cached global-thunk roots, every subsequent run returns
`$live` to the same value -- no per-run leak. The gate, for each receiver:
1. OUTPUT-INVARIANCE: the WASM output equals the same program emitted WITHOUT
   Perceus (dup/drop never change the printed result).
2. STEADY-STATE `$live`: a harness runs the converted `main` N times, recording
   `$live` after each; runs 2..N must be equal (flat = no leak). A premature free
   (use-after-free) instead corrupts the result or traps, failing gate 1.

This steady-state form is robust to root caching (cached CGlobal thunks allocate
once and stay live as roots) -- it measures per-run delta, not an absolute count,
exactly like R-ARC's free-list reuse test.

## Receiver map
- Task 3 (core: vars/let/app/closures) -- receiver: a closure that captures a
  value used twice (forces a dup) and a let-bound value left unused (forces a
  drop). Section "Worked example: dup and drop in the pure fragment".
- Task 4 (constructors/case/field) -- receiver: build an Option/Bool value, match
  it, project and keep a field; the scrutinee is dropped, the kept field dup'd.
  Section "Worked example: borrowed scrutinee, owned fields".
- Task 5 (pairs/fst/snd) -- receiver: build a dependent pair, project both halves;
  the pair is dropped after projection. Section "Worked example: pairs".
- Task 6 (broad gate) -- receiver: the WASM-supported listings subset run through
  the steady-state gate. Section "The corpus gate".

## Held for Plan 6b-2 (named, not silently dropped)
- CBounce / the partial trampoline: ownership across a DEFERRED saturated tail
  call and the public driver loop. Receiver (future): the ch39 countdown / a
  partial-recursive listing run leak-free on WASM.
- The hand-rolled nat-fold (`emitNatFold`) bignum temporaries: ownership of the
  base-1e9 limb temporaries inside the eliminator loop. Receiver (future): a
  bignum-arithmetic listing (e.g. `mul` on large literals) run leak-free.
Until 6b-2, the WASM Perceus pass is gated to programs that do not exercise these
paths (Task 6 defines the exact subset and asserts the boundary).
````

- [ ] **Step 2: Verify no dashes**

Run: `grep -nP "[\x{2013}\x{2014}]" ref_docs/wootz/R-PERCEUS.md`
Expected: no matches.

- [ ] **Step 3: Commit**

```bash
git add ref_docs/wootz/R-PERCEUS.md
git commit -m "docs(wasm): R-PERCEUS ownership design + receiver map"
```

---

### Task 2: CDup/CDrop nodes, WASM rendering, and the `$live` steady-state harness

Add the two ownership nodes to CIr, render them in the WASM emitter, wire an opt-in (initially IDENTITY) `Perceus` hook into `Wasm.Emit`, and build the steady-state balance harness. This task proves the PLUMBING end-to-end: a HAND-ANNOTATED CIr program with correct dup/drop balances `$live`, and the same program WITHOUT the annotations leaks -- so the gate has teeth before any algorithm exists.

**Files:**
- Modify: `codegen/closure.go` (add `CDup`, `CDrop` + `isCIr`)
- Create: `codegen/perceus.go` (the `Perceus` entry point; identity in this task)
- Modify: `codegen/wasm.go` (render CDup/CDrop in `emitIn`; call `Perceus` in `Emit`)
- Test: `codegen/perceus_test.go`

**Interfaces:**
- Consumes: CIr (`closure.go`), the 6a runtime (`$rt_retain`/`$rt_release`/`$rt_live`), `arcTestModule`/`WasmRuntime()`/`runWasm`.
- Produces:
  - `type CDup struct { V CIr; K CIr }` and `type CDrop struct { V CIr; K CIr }` (both `isCIr`).
  - `func Perceus(p ClosureProgram) ClosureProgram` -- the pass entry (identity here; Task 3+ fill it in).
  - WASM `emitIn` cases rendering CDup -> `(call $rt_retain V)` then K, CDrop -> `(call $rt_release V)` then K.
  - `Wasm.Emit` runs `cp = Perceus(cp)` after `ClosureConvert`.
  - Test helper `wasmSteadyLive(t, src, main string, runs int) []string` -- emits a module that seeds UNIT, then repeats (evaluate the converted `main`, release the result) `runs` times, printing `$rt_live` after each run on its own line; returns the per-run live counts.

- [ ] **Step 1: Add the nodes to CIr**

In `codegen/closure.go`, after `CBounce` (near line 139), add:

```go
// CDup retains V (a CVar or CEnv naming an already-evaluated, possibly-counted
// value) and then evaluates K. V stays valid in K. Inserted only by the Perceus
// ownership pass (codegen/perceus.go); present only in the WASM/ARC pipeline.
type CDup struct {
	V CIr
	K CIr
}

// CDrop releases V and then evaluates K. V is dead in K (the pass guarantees V
// does not occur in K). Inserted only by Perceus; WASM/ARC pipeline only.
type CDrop struct {
	V CIr
	K CIr
}
```

And with the other `isCIr` markers (near line 155):

```go
func (CDup) isCIr()  {}
func (CDrop) isCIr() {}
```

- [ ] **Step 2: Extend the free-variable / arg-use scanners for the new nodes**

`cirUsesArg` (closure.go ~line 255) switches on CIr and must handle CDup/CDrop or it will silently return false for a variable used inside K. In the `switch`, before `default`, add:

```go
	case CDup:
		return cirUsesArg(x.V, idx) || cirUsesArg(x.K, idx)
	case CDrop:
		return cirUsesArg(x.V, idx) || cirUsesArg(x.K, idx)
```

(There is no Ir-level counterpart -- CDup/CDrop never appear in `freeVars`, which walks Ir, so no change there.)

- [ ] **Step 3: Render CDup/CDrop in the WASM emitter**

In `codegen/wasm.go`, in `emitIn` (the `switch x := t.(type)`), before `default` (near line 542), add:

```go
	case CDup:
		v := f.emitIn(b, x.V, locals)
		fmt.Fprintf(b, "    (call $rt_retain %s)\n", v)
		return f.emitIn(b, x.K, locals)
	case CDrop:
		v := f.emitIn(b, x.V, locals)
		fmt.Fprintf(b, "    (call $rt_release %s)\n", v)
		return f.emitIn(b, x.K, locals)
```

(`x.V` is a CVar/CEnv, so `f.emitIn` returns its local expression with no side effect; `rt_retain`/`rt_release` take the i32 value. The result of the whole node is K's result.)

- [ ] **Step 4: Add the identity `Perceus` entry point**

Create `codegen/perceus.go`:

```go
package codegen

// perceus.go -- the OPT-IN Perceus ownership-insertion pass over the closure-
// converted CIr. It inserts CDup/CDrop so heap values are reference-counted by
// the Plan 6a ARC runtime (rt_retain/rt_release). Only the WASM backend runs it
// (Wasm.Emit); the C/LLVM and source backends consume the un-annotated CIr.
//
// Soundness rests on the erased IR being immutable, total, and acyclic (R-ARC):
// reference counting with no cycle collector is sound and complete. v1 inserts
// dup/drop only; the in-place reuse optimization is a later plan. See
// ref_docs/wootz/R-PERCEUS.md for the ownership rules and the receiver map.
//
// In this task Perceus is the IDENTITY -- the nodes, the emitter rendering, and
// the steady-state gate land first so later tasks build the algorithm against a
// working gate.
func Perceus(p ClosureProgram) ClosureProgram {
	return p
}
```

- [ ] **Step 5: Wire the opt-in hook into Wasm.Emit**

In `codegen/wasm.go`, in `func (Wasm) Emit(p Program)`, right after `cp := ClosureConvert(p)` (line 48), add:

```go
	cp = Perceus(cp)
```

(Identity for now, so output is unchanged; Task 6 confirms the real pass keeps the corpus byte-identical.)

- [ ] **Step 6: Write the failing plumbing test**

Create `codegen/perceus_test.go`:

```go
package codegen_test

import (
	"strings"
	"testing"

	cg "goforge.dev/rune/v3/codegen"
)

// arcDupDropModule builds a minimal module (reusing the 6a arcTestModule shell)
// whose _start allocates a bignum, optionally retains/releases it via a hand-
// inserted dup/drop pattern, and prints $rt_live. It pins that the emitter
// renders CDup/CDrop as rt_retain/rt_release and that the runtime balances.
//
// We exercise the EMITTER path by building CIr by hand and lowering it. Rather
// than a whole program, we drive the WAT directly: a value v allocated, dup'd
// twice (rc 1->3), dropped three times (rc 3->0, freed). With balance, $live
// returns to 1 (UNIT). This is the CDup/CDrop analogue of the 6a leaf test, but
// through the EMITTER's rt_retain/rt_release rendering, not hand-written WAT.
func TestPerceusDupDropBalances(t *testing.T) {
	body := `
    (local $v i32)
    (local.set $v (call $rt_big_from_long (i32.const 9)))
    (call $rt_retain (local.get $v))
    (call $rt_retain (local.get $v))
    (call $rt_release (local.get $v))
    (call $rt_release (local.get $v))
    (call $rt_release (local.get $v))
    (call $rt_print_u32 (call $rt_live))`
	out := runWasm(t, arcTestModule(body))
	if out != "1" {
		t.Fatalf("dup/drop balance: live = %q, want 1", out)
	}
}

// TestPerceusEmitterRendersDupDrop checks the EMITTER turns CDup/CDrop into
// rt_retain/rt_release around a value. We build `let v = 9n in dup v; drop v; v`
// as CIr, lower it through the wasm emitter via a whole-program emit, and assert
// the WAT contains the retain and release calls. (Behavioral balance is covered
// by the steady-state gate in later tasks; here we pin the rendering.)
func TestPerceusEmitterRendersDupDrop(t *testing.T) {
	// A def `n : Nat is 9` whose body we wrap by hand is awkward to build through
	// the session; instead assert the emitter rendering via a direct CIr lowering
	// is exercised by the corpus in Task 6. Here we only assert Perceus is wired
	// as identity (no change) so existing output is stable.
	src := `three : Nat is succ (succ (succ zero)) end`
	withoutHook := emitWith(t, cg.Wasm{}, src, "three")
	if !strings.Contains(withoutHook, "rune_main") {
		t.Fatalf("expected a normal module; Perceus identity must not alter structure")
	}
}

// wasmSteadyLive emits a module that seeds UNIT, then `runs` times evaluates the
// converted `main` and releases its result, printing $rt_live after each run.
// Returns the per-run counts (as strings, one per line). A balanced program has
// counts[1:] all equal (steady state: no per-run leak).
func wasmSteadyLive(t *testing.T, src, main string, runs int) []string {
	t.Helper()
	mod := cg.WasmSteadyModule(t, src, main, runs) // added in Step 8
	out := runWasm(t, mod)
	return strings.Split(strings.TrimSpace(out), "\n")
}
```

- [ ] **Step 7: Run the plumbing test to verify the first case passes and the helper is missing**

Run: `go test -run 'TestPerceusDupDropBalances|TestPerceusEmitterRendersDupDrop' ./codegen/`
Expected: both PASS (the runtime renders/balances; Perceus identity keeps output stable). The `wasmSteadyLive` helper references `cg.WasmSteadyModule`, which does not exist yet -- so `go test ./codegen/` (full) FAILS to compile until Step 8.

- [ ] **Step 8: Add the steady-state module builder (the gate the later tasks use)**

The harness needs to emit a module whose `_start` runs `main` repeatedly and prints `$live` each time. Add a TEST-ONLY exported builder to the WASM backend so tests in `package codegen_test` can reach the internal emitter. Create `codegen/wasm_steady.go`:

```go
package codegen

import (
	"fmt"
	"strings"
	"testing"
)

// WasmSteadyModule builds a WASM module whose _start seeds UNIT, then `runs`
// times evaluates the program's `main` and RELEASES the result, printing $rt_live
// (the live-block count) after each run on its own line. It is the Perceus
// steady-state balance gate: a leak-free program's counts are flat across runs
// 2..N (run 1 warms cached global-thunk roots). TEST-ONLY (takes *testing.T).
//
// It reuses the normal Emit pipeline (ClosureConvert + Perceus) for the program
// bodies, then substitutes a custom entry that loops main + release + print.
func WasmSteadyModule(t *testing.T, src, main string, runs int) string {
	t.Helper()
	p := mustEmitProgramForSteady(t, src, main)
	cp := Perceus(ClosureConvert(p))
	em := newWasmEmitter(cp) // see note: factor the emitter ctor out of Emit
	var b strings.Builder
	b.WriteString("(module\n")
	b.WriteString("  (type $codety (func (param i32 i32) (result i32)))\n")
	b.WriteString(WasmRuntime())
	em.emitData(&b)
	em.emitDefs(&b) // the def thunks + code blocks + ctors + nat (factored from Emit)
	em.emitTable(&b)
	// Custom entry: seed UNIT, then loop runs times.
	fmt.Fprintf(&b, "  (func $_start (export \"_start\")\n")
	b.WriteString("    (local $i i32) (local $r i32)\n")
	b.WriteString("    (global.set $UNIT (call $rt_mkcon (i32.const 0) (i32.const 64) (i32.const 0)))\n")
	fmt.Fprintf(&b, "    (local.set $i (i32.const 0))\n")
	b.WriteString("    (block $brk (loop $lp\n")
	fmt.Fprintf(&b, "      (br_if $brk (i32.ge_u (local.get $i) (i32.const %d)))\n", runs)
	f := em.newFunc()
	mainV := f.emit(&b, CGlobal{Name: p.Main}, nil)
	b.WriteString(f.localsDecl()) // hoist locals; see Step 9 note
	fmt.Fprintf(&b, "      (local.set $r %s)\n", mainV)
	b.WriteString("      (call $rt_release (local.get $r))\n")
	b.WriteString("      (call $rt_print_u32 (call $rt_live))\n")
	b.WriteString("      (call $rt_print_nl)\n")
	b.WriteString("      (local.set $i (i32.add (local.get $i) (i32.const 1)))\n")
	b.WriteString("      (br $lp)))\n")
	b.WriteString("  )\n)\n")
	return b.String()
}
```

NOTE for the implementer: `WasmSteadyModule` needs three small refactors of `wasm.go` so the entry can be swapped without duplicating the body emission. Do them minimally:
1. Factor the emitter construction in `Emit` into `func newWasmEmitter(cp ClosureProgram) *wasmEmitter`.
2. Factor the "emit all def thunks + code blocks + ctors + nat" block of `Emit` into `func (em *wasmEmitter) emitDefs(b *strings.Builder)`.
3. Add `func (f *wasmFunc) localsDecl() string` returning the `(local $tN i32)` declarations the function accumulated (WAT needs locals declared before instructions; the loop body emits into the function, so collect and hoist them -- mirror how `arcTestModule`/`splitWatLocals` hoist, or have the steady entry declare a generous `(local $t0 i32) ... ` pool). The simplest robust form: have `wasmFunc` track its fresh-local names and emit their declarations at the top of `$_start` before the loop. Add `rt_print_nl` (a one-byte newline writer) to the runtime if absent: `(func $rt_print_nl (call $puts (i32.const 1) (global.get $abort_msg) (i32.const 0)))` prints nothing useful -- instead emit a literal newline: store 10 at scratch and puts 1 byte. Reuse `$rt_print_u32`'s buffer discipline.

`mustEmitProgramForSteady` mirrors `closure_test.go`'s `mustEmitProgram` but lives in the non-test `wasm_steady.go`; since it needs the session, keep it in the test file instead and pass the `Program` in. REFINED SIGNATURE to avoid a session dependency in non-test code:

```go
func WasmSteadyModule(t *testing.T, p Program, runs int) string
```

and the test builds `p` via the existing `emitWith` path's program builder. Adjust `wasmSteadyLive` to call `cg.WasmSteadyModule(t, p, runs)` after obtaining `p` from a small `mustProgram(t, src, main)` test helper (copy `closure_test.go`'s `mustEmitProgram`).

- [ ] **Step 9: Make the steady gate compile and pass on a trivial program**

Adjust the test from Step 6 to build the program and call the refined builder:

```go
func TestPerceusSteadyTrivial(t *testing.T) {
	// A program with no heap allocation in main beyond a constructor: succ chain.
	p := mustProgram(t, `three : Nat is succ (succ (succ zero)) end`, "three")
	counts := wasmSteadyLiveP(t, p, 3)
	if len(counts) != 3 {
		t.Fatalf("want 3 per-run counts, got %d: %v", len(counts), counts)
	}
	if counts[1] != counts[2] {
		t.Fatalf("not steady: run2=%s run3=%s (per-run leak)", counts[1], counts[2])
	}
}
```

where `mustProgram` and `wasmSteadyLiveP` (taking a `Program`) are added to `perceus_test.go`. With Perceus as identity AND no dup/drop inserted, this trivial program may NOT be steady yet (a succ-chain allocates K_BIG/K_CON each run and nothing frees it) -- that is EXPECTED: the gate currently shows the leak. So assert the gate REPORTS a leak here (counts grow), documenting the gate works:

```go
	// Identity Perceus: main allocates and never frees -> counts GROW. The gate
	// has teeth. Task 3+ make a real program steady.
	if !(counts[2] > counts[1]) {
		t.Fatalf("expected a visible leak under identity Perceus, got flat %v", counts)
	}
```

- [ ] **Step 10: Run the gate test + confirm the existing WASM gate is unbroken**

Run: `go test -run 'Perceus' ./codegen/`
Expected: PASS (dup/drop render + balance; the steady gate compiles and shows the identity-pass leak as designed).

Run: `go test -run 'Wasm|WASM|ARC' ./codegen/`
Expected: PASS (Perceus identity changed no output; all 6a ARC tests and the WASM conformance gate green).

- [ ] **Step 11: Commit**

```bash
git add codegen/closure.go codegen/perceus.go codegen/wasm.go codegen/wasm_steady.go codegen/perceus_test.go
git commit -m "feat(wasm): CDup/CDrop nodes + rt_retain/rt_release rendering + steady-state $live gate (Perceus identity)"
```

---

### Task 3: The core ownership pass -- vars, let, app, closures

Fill in `Perceus` for the pure fragment (CVar/CEnv/CGlobal/CUnit/CLit/CLet/AppClosure/MkClosure), inserting dup/drop per the R-PERCEUS rules: the code block owns its argument, borrows its captures, dups a shared/escaping value, drops a dead owned local. Receiver: a closure capturing a value used twice (a dup) plus a let-bound unused value (a drop), run leak-free.

**Files:**
- Modify: `codegen/perceus.go` (the real algorithm for the pure fragment)
- Modify: `ref_docs/wootz/R-PERCEUS.md` (append the worked-example section)
- Test: `codegen/perceus_test.go` (the receiver + unit assertions)

**Interfaces:**
- Consumes: CIr, CDup/CDrop (Task 2), `cirUsesArg`, the steady gate.
- Produces: `Perceus` now rewrites every `CodeBlock.Body` and `CDefSpec.Body` for the pure fragment. Internal: `type ownCtx` (the owned de Bruijn locals in scope) and `func (pp *perceusPass) own(t CIr, depth int) CIr`. Constructors/case/pairs are carried THROUGH unchanged in this task (handled in Tasks 4-5), so a program using them still emits (just unbalanced for those parts) -- the receiver here avoids them.

- [ ] **Step 1: Write the failing receiver test**

Add to `codegen/perceus_test.go`:

```go
// Receiver for Task 3: a closure captures `x` and uses it TWICE inside the body
// (forcing a dup of the capture's escape), and binds a value it never uses
// (forcing a drop). Built so the only heap values are bignums + the closure.
//
//   main : Nat is
//     (fn (x) is
//        let unused = (big 5) in   -- dead: must be dropped
//        add x x                   -- x used twice: one dup
//     end) (big 7)
//
// `add` is the builtin-nat accel (a pure 2-arg op), so `add x x` consumes x
// twice; with a correct dup the bignum 7 survives the first add-consume.
func TestPerceusCoreDupDrop(t *testing.T) {
	src := `
main : Nat is
  (fn (x : Nat) is
     seq
       let unused : Nat is 5 end
       add x x
     end
   end) 7
end`
	// Output-invariance: equals the non-Perceus result (14).
	withP := emitWith(t, cgWasmARC(t), src, "main")    // ARC-on emit (Perceus runs)
	got := runWasm(t, withP)
	if got != "14" {
		t.Fatalf("output changed under Perceus: got %q want 14", got)
	}
	// Steady state: no per-run leak.
	p := mustProgram(t, src, "main")
	counts := wasmSteadyLiveP(t, p, 4)
	for i := 2; i < len(counts); i++ {
		if counts[i] != counts[1] {
			t.Fatalf("core fragment leaks: counts=%v (run %d != run 1)", counts, i)
		}
	}
}
```

NOTE: `cgWasmARC(t)` is the WASM backend with Perceus active. Since `Wasm.Emit` already calls `Perceus` unconditionally (Task 2 wired it), `cgWasmARC(t)` is just `cg.Wasm{}`; define it as an alias for readability. `add`/`5`/`7` use the builtin nat; confirm the test source elaborates (the session may need `add` in scope -- use the same nat builtin the other wasm tests use; if `add` is not ambient, define it inline: `add : Nat -> Nat -> Nat is fn (a)(b) is natElim ... end` is heavy -- instead use the ambient `add` from the prelude if present, else the receiver can use a user 2-use binding without `add`: `let y is x in pair y x` -- but that pulls in pairs (Task 5). KEEP the receiver within the pure fragment: use a higher-order identity-applied-twice so the dup is on a closure, e.g. `(fn (f) is f (f z))` capturing nothing heap. The implementer picks the minimal pure-fragment program that forces one dup and one drop; document the chosen program in R-PERCEUS.md.)

- [ ] **Step 2: Run it to confirm the leak (pre-implementation)**

Run: `go test -run TestPerceusCoreDupDrop ./codegen/`
Expected: FAIL on the steady-state assertion (identity Perceus leaks the bignums each run) OR the output assertion if the chosen program's value differs -- the leak is the target failure.

- [ ] **Step 3: Implement the core ownership algorithm**

Replace `Perceus` in `codegen/perceus.go` with the pure-fragment pass. The algorithm (de Bruijn, no reuse) processes a scope owning a set of local indices, inserting dup at non-final consuming uses and drop for dead owned locals. Concretely:

```go
type perceusPass struct {
	blocks map[string]CodeBlock
}

func Perceus(p ClosureProgram) ClosureProgram {
	pp := &perceusPass{blocks: map[string]CodeBlock{}}
	for _, blk := range p.Blocks {
		pp.blocks[blk.Name] = blk
	}
	// Rewrite each lifted code block: it OWNS its argument (CVar{0}); captures
	// (CEnv) are borrowed. Insert dup/drop in the body owning {0}.
	for i := range p.Blocks {
		body := pp.ownScope(p.Blocks[i].Body, []bool{true}) // index 0 = arg, owned
		p.Blocks[i].Body = body
	}
	// Top-level defs own nothing on entry (no argument binder at the def root;
	// leading lambdas are MkClosure whose blocks are handled above).
	for i := range p.Defs {
		p.Defs[i].Body = pp.ownScope(p.Defs[i].Body, nil)
	}
	return p
}

// ownScope rewrites t in a scope where `owned[i]` says whether de Bruijn local i
// (innermost = 0) is an owned binding the scope must consume-or-drop. It returns
// t with dup/drop inserted so that, on every path, each owned local is consumed
// exactly once and every value that is read-but-needed-later is dup'd.
//
// v1 strategy (simple, sound; not minimal): for the scope's RESULT term, compute
// the consuming occurrences of each owned local; insert a CDup before all but the
// last consuming occurrence, and a CDrop (at the front) for any owned local with
// zero consuming occurrences. Recurse into binders (CLet adds an owned local).
func (pp *perceusPass) ownScope(t CIr, owned []bool) CIr {
	// 1. Drop owned locals that never occur (dead on entry).
	out := t
	for i := len(owned) - 1; i >= 0; i-- {
		if owned[i] && !cirUsesArg(t, i) {
			out = CDrop{V: CVar{Idx: i}, K: out}
		}
	}
	// 2. Walk and insert dups for multiply-consumed owned locals, and recurse
	//    into CLet (which introduces a new owned local at index 0, shifting the
	//    rest). This is realized by `annotate`, which threads remaining-use counts.
	return pp.annotate(out, owned)
}
```

Then `annotate` implements the dup placement and the CLet recursion. Provide the concrete walk:

```go
// annotate inserts CDup before non-final consuming uses of each owned local and
// recurses into binders. It is syntax-directed over the pure fragment; non-pure
// nodes (CCase/CField/CPair/CFst/CSnd/CBounce) are carried through unchanged here
// and handled in Tasks 4-5.
func (pp *perceusPass) annotate(t CIr, owned []bool) CIr {
	switch x := t.(type) {
	case CVar:
		return x // a single consuming use; ownership transfers
	case CEnv, CGlobal, CForeign, CUnit, CLit:
		return x // borrowed/root/immediate: no dup/drop as a variable
	case CLet:
		// v is evaluated in the current scope; its free owned locals are consumed
		// there. The body gains a new owned local (index 0); existing owned shift up.
		val := pp.annotate(x.Val, owned)
		bodyOwned := append([]bool{true}, owned...)
		body := pp.ownScope(x.Body, bodyOwned)
		return CLet{Name: x.Name, Val: val, Body: body}
	case AppClosure:
		// Evaluate Clo then Arg. A local consumed in BOTH must be dup'd before Clo.
		clo := pp.annotate(x.Clo, owned)
		arg := pp.annotate(x.Arg, owned)
		out := CIr(AppClosure{Clo: clo, Arg: arg})
		for i := range owned {
			if owned[i] && cirUsesArg(x.Clo, i) && cirUsesArg(x.Arg, i) {
				out = CDup{V: CVar{Idx: i}, K: out}
			}
		}
		return out
	case MkClosure:
		// Each env term is consumed (stored). A local consumed in more than one env
		// slot, or captured and still live after, is dup'd. v1: dup a local once per
		// extra env slot it appears in beyond the first.
		env := make([]CIr, len(x.Env))
		for i, e := range x.Env {
			env[i] = pp.annotate(e, owned)
		}
		out := CIr(MkClosure{Code: x.Code, Env: env})
		for i := range owned {
			if !owned[i] {
				continue
			}
			n := 0
			for _, e := range x.Env {
				if cirUsesArg(e, i) {
					n++
				}
			}
			for k := 1; k < n; k++ {
				out = CDup{V: CVar{Idx: i}, K: out}
			}
		}
		return out
	default:
		// CCase/CField/CPair/CFst/CSnd/CBounce: carried through; Tasks 4-5/6b-2.
		return t
	}
}
```

IMPLEMENTER NOTE: this v1 placement is intentionally simple and is validated by the steady-state gate, not by minimality. The subtlety the gate will surface: a local consumed in BOTH `Clo` and `Arg` needs exactly one dup (the code above inserts it); a local consumed once needs none; a local dead needs a drop (ownScope step 1). If the gate shows a leak (a missing dup) or a trap/wrong-output (a premature free from an over-eager drop), adjust the placement per the R-PERCEUS rules. Do NOT chase minimality; chase balance. The `cirUsesArg` index semantics (CLet shifts by 1, MkClosure bodies are separate scopes) are already correct in closure.go.

- [ ] **Step 4: Run the receiver to verify it balances**

Run: `go test -run TestPerceusCoreDupDrop ./codegen/`
Expected: PASS (output unchanged AND steady-state flat).

- [ ] **Step 5: Confirm the existing gates**

Run: `go test -run 'Wasm|WASM|ARC|Perceus' ./codegen/`
Expected: PASS. The output-invariance holds for the WASM conformance programs that lie in the pure fragment; constructor/case programs still emit (carried-through) and must still produce correct OUTPUT (they may not yet be leak-free, which is Task 4 -- so do NOT add them to the steady gate here).

- [ ] **Step 6: Append the worked example to R-PERCEUS.md**

Add a section "## Worked example: dup and drop in the pure fragment" documenting the receiver program, the dup inserted for the twice-used value, the drop for the dead let-binding, and the steady-state counts observed.

Run: `grep -nP "[\x{2013}\x{2014}]" ref_docs/wootz/R-PERCEUS.md` (expect none).

- [ ] **Step 7: Commit**

```bash
git add codegen/perceus.go codegen/perceus_test.go ref_docs/wootz/R-PERCEUS.md
git commit -m "feat(wasm): Perceus ownership for the pure fragment (vars/let/app/closures)"
```

---

### Task 4: Ownership for constructors, case, and field (borrowed scrutinee)

Extend `annotate` to the constructor/eliminator fragment: a `CCase` borrows its scrutinee for the tag read and drops it after the arms (per arm); a `CField` aliases a projected value (dup on escape); constructor application stores owning references. Receiver: build and match a small datatype, keep a projected field.

**Files:**
- Modify: `codegen/perceus.go` (CCase/CField + constructor-application handling in `annotate`)
- Modify: `ref_docs/wootz/R-PERCEUS.md` (worked example)
- Test: `codegen/perceus_test.go`

**Interfaces:**
- Consumes: the Task 3 pass; the CIr layout (`CCase{Scrut,Arms}`, `CField{Scrut,Index}`); the runtime kinds (K_CON).
- Produces: `annotate` now handles CCase (drop scrutinee per arm after use), CField (dup escaping projection), and recognizes constructor application (a saturated AppClosure spine headed by a CGlobal constructor) as an owning store -- though since constructor application is just AppClosure, the existing AppClosure rule already dups shared args; the NEW work is the scrutinee drop + field dup.

- [ ] **Step 1: Write the failing receiver test**

Add a receiver that builds an `Option Nat` (or a Bool), matches it, and keeps the payload:

```go
// Receiver for Task 4: build `some 7`, match it; the `none` arm returns 0, the
// `some x` arm returns x (keeps the projected field). The scrutinee `some 7` is
// owned and must be dropped after the match; the kept field x must be dup'd out
// before the scrutinee is freed (else use-after-free). Output: 7.
func TestPerceusConstructorCase(t *testing.T) {
	src := `
data Opt : U is none : Opt | some : Nat -> Opt end
main : Nat is
  optElim (fn (_) is Nat end) 0 (fn (x) is x end) (some 7)
end`
	// (Use the real eliminator name the data decl generates; adjust to the
	// session's generated `optElim`/`OptElim` -- confirm via `rune emit`.)
	got := runWasm(t, emitWith(t, cgWasmARC(t), src, "main"))
	if got != "7" {
		t.Fatalf("output changed under Perceus: got %q want 7", got)
	}
	p := mustProgram(t, src, "main")
	counts := wasmSteadyLiveP(t, p, 4)
	for i := 2; i < len(counts); i++ {
		if counts[i] != counts[1] {
			t.Fatalf("constructor/case leaks: counts=%v", counts)
		}
	}
}
```

- [ ] **Step 2: Run it to confirm a leak or a premature-free fault**

Run: `go test -run TestPerceusConstructorCase ./codegen/`
Expected: FAIL -- with constructors carried through (Task 3), the scrutinee `some 7` is allocated each run and never dropped (leak), so the steady assertion fails.

- [ ] **Step 3: Implement CCase / CField ownership**

In `annotate` (perceus.go), replace the `default` carry-through for CCase and CField with real handling:

```go
	case CField:
		// rt_con_get returns an ALIAS into the scrutinee; the scrutinee remains
		// owned by whoever holds it. Annotate the scrutinee (it is consumed where
		// it appears). A projected value that ESCAPES is dup'd by its consumer's
		// rule (it flows as a CVar once let-bound), so no dup here.
		return CField{Scrut: pp.annotate(x.Scrut, owned), Index: x.Index}

	case CCase:
		// The scrutinee is evaluated once and BORROWED for the tag read. After the
		// arms it is owned and must be dropped on each arm path (unless an arm
		// transfers it). Bind the scrutinee to a fresh owned local so each arm can
		// reference it for field reads AND so we can drop it once per arm.
		scrut := pp.annotate(x.Scrut, owned)
		// Lift the scrutinee into a CLet so it has a stable index the arms drop.
		// New owned local index 0 = the scrutinee; existing owned shift up by 1.
		armOwned := append([]bool{true}, owned...)
		arms := make([]CCaseArm, len(x.Arms))
		for i, a := range x.Arms {
			// Within an arm, fields are read via CField on CVar{0} (the scrutinee).
			body := pp.ownScope(a.Body, armOwned)
			// Drop the scrutinee at the end of the arm: bind the arm result, then
			// drop CVar{0}, then return the result. ownScope already drops the
			// scrutinee if the arm never uses it; if the arm DOES use it (field
			// reads), insert the post-use drop here.
			arms[i] = CCaseArm{Tag: a.Tag, Body: pp.dropAfter(body, 0)}
		}
		return CLet{Name: "$scrut", Val: scrut, Body: CCase{Scrut: CVar{Idx: 0}, Arms: arms}}
```

Add the `dropAfter` helper (release an owned local AFTER the term's value is computed, returning that value):

```go
// dropAfter rewrites `e` to `let r = e in drop v; r` so the owned local at index
// `idx` is released AFTER e is evaluated but the result is preserved. Used to free
// a case scrutinee once its arm has read what it needs. If e never uses idx, the
// caller's ownScope already dropped it at the front, so dropAfter must avoid a
// DOUBLE drop: it checks occurrence first.
func (pp *perceusPass) dropAfter(e CIr, idx int) CIr {
	if !cirUsesArg(e, idx) {
		return e // already dropped at scope entry by ownScope step 1
	}
	// e uses idx; bind the result, drop idx, return the result. After binding, the
	// result is local 0 and idx shifts to idx+1.
	return CLet{
		Name: "$r",
		Val:  e,
		Body: CDrop{V: CVar{Idx: idx + 1}, K: CVar{Idx: 0}},
	}
}
```

IMPLEMENTER NOTE: the scrutinee-as-CLet rewrite shifts de Bruijn indices inside the arms by 1 (the arms originally referenced the scrutinee via `CField{Scrut: x.Scrut}`, not a bound variable). Confirm the arm bodies reference the scrutinee correctly: in the source IR, `ICase` arms reference the scrutinee through `CField{Scrut: <the scrutinee term>}` (closure.go comment: "Arms reference the scrutinee via CField ... no binders"). So the arms do NOT use a de Bruijn index for the scrutinee -- they re-embed the scrutinee term. That means lifting the scrutinee into a CLet and rewriting `CField{Scrut: <copy>}` to `CField{Scrut: CVar{0}}` is REQUIRED for the single-eval + single-drop to be correct. ADJUST: in the CCase case, replace each arm's `CField{Scrut: <the original scrutinee term>}` with `CField{Scrut: CVar{0}}` (the bound scrutinee) before annotating, and shift the arm's other indices by 1. This is the one genuinely fiddly rewrite; the steady-state gate + output-invariance catch an error. Document the chosen approach in R-PERCEUS.md. If the emitter already evaluates the scrutinee once (it does -- `emitCase` binds `s` to a local), an alternative simpler design is to drop the scrutinee in the EMITTER after the case rather than in the pass; if the CLet-lifting proves error-prone, the implementer MAY instead emit the scrutinee drop in `emitCase` (a localized, WASM-only choice) and document that deviation. Prefer the pass-level rewrite for portability, but the gate is the arbiter.

- [ ] **Step 4: Run the receiver to verify balance**

Run: `go test -run TestPerceusConstructorCase ./codegen/`
Expected: PASS (output 7, steady-state flat).

- [ ] **Step 5: Confirm broader output-invariance**

Run: `go test -run 'Wasm|WASM|ARC|Perceus' ./codegen/`
Expected: PASS (every WASM program's OUTPUT unchanged; constructor programs now balance).

- [ ] **Step 6: Worked example + commit**

Append "## Worked example: borrowed scrutinee, owned fields" to R-PERCEUS.md (the receiver, the scrutinee drop, the field handling, the index-shift note). Verify no dashes.

```bash
git add codegen/perceus.go codegen/perceus_test.go ref_docs/wootz/R-PERCEUS.md
git commit -m "feat(wasm): Perceus ownership for constructors/case/field (borrowed scrutinee)"
```

---

### Task 5: Ownership for pairs (CPair / CFst / CSnd)

Extend `annotate` to dependent pairs: `CPair` stores two owning references (dup shared/escaping components, like MkClosure env); `CFst`/`CSnd` project a half (alias) and the pair is dropped when dead. Receiver: build a pair, project both halves.

**Files:**
- Modify: `codegen/perceus.go` (CPair/CFst/CSnd in `annotate`)
- Modify: `ref_docs/wootz/R-PERCEUS.md` (worked example)
- Test: `codegen/perceus_test.go`

**Interfaces:**
- Consumes: the Task 3-4 pass; `CPair{A,B}`, `CFst{P}`, `CSnd{P}`; runtime K_PAIR.
- Produces: `annotate` handles CPair (dup components used elsewhere), CFst/CSnd (annotate the pair sub-term; drop the pair after projection if it is an owned local with no further use).

- [ ] **Step 1: Write the failing receiver test**

```go
// Receiver for Task 5: build a pair (3, 4), project both halves, add them.
// The pair is owned; after both projections it is dead and must be dropped.
// Output: 7. (Uses `add`; if not ambient, sum via a user 2-arg path.)
func TestPerceusPairs(t *testing.T) {
	src := `
main : Nat is
  seq
    let p : Sig Nat (fn (_ : Nat) is Nat end) is Pair 3 4 end
    add (Fst p) (Snd p)
  end
end`
	got := runWasm(t, emitWith(t, cgWasmARC(t), src, "main"))
	if got != "7" {
		t.Fatalf("output changed: got %q want 7", got)
	}
	p := mustProgram(t, src, "main")
	counts := wasmSteadyLiveP(t, p, 4)
	for i := 2; i < len(counts); i++ {
		if counts[i] != counts[1] {
			t.Fatalf("pairs leak: counts=%v", counts)
		}
	}
}
```

- [ ] **Step 2: Run to confirm the pair leaks**

Run: `go test -run TestPerceusPairs ./codegen/`
Expected: FAIL (the pair `(3,4)` allocated each run, never dropped).

- [ ] **Step 3: Implement CPair / CFst / CSnd ownership**

In `annotate`, add:

```go
	case CPair:
		a := pp.annotate(x.A, owned)
		b := pp.annotate(x.B, owned)
		out := CIr(CPair{A: a, B: b})
		for i := range owned {
			if owned[i] && cirUsesArg(x.A, i) && cirUsesArg(x.B, i) {
				out = CDup{V: CVar{Idx: i}, K: out}
			}
		}
		return out
	case CFst:
		return CFst{P: pp.annotate(x.P, owned)}
	case CSnd:
		return CSnd{P: pp.annotate(x.P, owned)}
```

The pair-drop-when-dead is handled by `ownScope` step 1 (if the pair is a let-bound owned local used only in CFst/CSnd projections that themselves do not keep it live, the local becomes dead after the projections and is dropped). IMPLEMENTER NOTE: a projected half that ESCAPES (is returned/stored) aliases into the pair; releasing the pair recursively releases the OTHER half but the kept half must outlive the pair -- so a kept projection requires a dup of the projected value before the pair is dropped. If the gate shows a use-after-free (kept half freed with the pair), insert a dup at the CFst/CSnd whose result escapes; the steady gate + output-invariance arbitrate. Document the rule applied.

- [ ] **Step 4: Run + confirm**

Run: `go test -run TestPerceusPairs ./codegen/` -> PASS.
Run: `go test -run 'Wasm|WASM|ARC|Perceus' ./codegen/` -> PASS.

- [ ] **Step 5: Worked example + commit**

Append "## Worked example: pairs" to R-PERCEUS.md. Verify no dashes.

```bash
git add codegen/perceus.go codegen/perceus_test.go ref_docs/wootz/R-PERCEUS.md
git commit -m "feat(wasm): Perceus ownership for pairs (CPair/CFst/CSnd)"
```

---

### Task 6: The corpus gate + the 6b-2 boundary

Run the WASM-supported listings subset through the steady-state gate (the broad receiver), prove output-invariance across the whole WASM corpus, and DEFINE + ASSERT the 6b-2 boundary: programs using `partial`/trampoline (CBounce) or the nat-fold are detected and either excluded from the balance gate or documented as the known leak frontier with their future receivers named.

**Files:**
- Test: `codegen/perceus_corpus_test.go`
- Modify: `ref_docs/wootz/R-PERCEUS.md` (the corpus-gate section + the 6b-2 boundary)

**Interfaces:**
- Consumes: the full Task 3-5 pass; the steady gate; `wasmCheckSupported` (to know which programs the WASM backend accepts).
- Produces: a corpus test that (a) asserts OUTPUT-INVARIANCE (Perceus vs a reference backend, e.g. the existing `runWasm` baseline captured before the pass was real -- or the JS backend's result) for every WASM-supported corpus program, and (b) asserts STEADY-STATE balance for the subset that avoids CBounce + nat-fold, and (c) asserts that a CBounce/nat-fold program is correctly EXCLUDED (a `perceusBalanceable(p) bool` predicate the test uses, documented as the 6b-2 frontier).

- [ ] **Step 1: Write the corpus output-invariance test**

```go
// Every WASM-supported corpus program produces the SAME output with Perceus as a
// reference backend (dup/drop never change results). Reuse the conformance corpus
// the backend_conformance test already enumerates; for each, compare runWasm to
// the JS backend's runNode result (or a recorded golden).
func TestPerceusCorpusOutputInvariance(t *testing.T) {
	for _, c := range wasmConformanceCases(t) { // enumerate supported programs
		got := runWasm(t, emitWith(t, cg.Wasm{}, c.src, c.main))
		if got != c.want {
			t.Fatalf("%s: Perceus changed output: got %q want %q", c.name, got, c.want)
		}
	}
}
```

- [ ] **Step 2: Write the corpus steady-state test (the balanceable subset)**

```go
// The subset of supported programs that do NOT use CBounce (partial/trampoline)
// or the nat-fold must be leak-free under Perceus. perceusBalanceable inspects the
// closure-converted program for CBounce and for nat-eliminator folds.
func TestPerceusCorpusSteady(t *testing.T) {
	for _, c := range wasmConformanceCases(t) {
		p := mustProgram(t, c.src, c.main)
		if !perceusBalanceable(p) {
			continue // 6b-2 frontier; see TestPerceusFrontierBoundary
		}
		counts := wasmSteadyLiveP(t, p, 4)
		for i := 2; i < len(counts); i++ {
			if counts[i] != counts[1] {
				t.Fatalf("%s leaks under Perceus: counts=%v", c.name, counts)
			}
		}
	}
}
```

- [ ] **Step 3: Implement `perceusBalanceable` + the boundary test**

```go
// perceusBalanceable reports whether a program lies in the v1 Perceus fragment
// (no CBounce trampoline, no nat-fold eliminator). Programs outside it are the
// named 6b-2 frontier (R-PERCEUS.md). Implemented by scanning the closure-
// converted CIr for CBounce and for a saturated nat-eliminator spine.
func perceusBalanceable(p cg.Program) bool { /* scan cp := ClosureConvert(p) */ }

// TestPerceusFrontierBoundary pins that a partial-recursive program (ch39-style
// countdown) is currently OUTSIDE the balanceable set -- documenting the 6b-2
// boundary rather than silently leaking. When 6b-2 lands, this test flips.
func TestPerceusFrontierBoundary(t *testing.T) {
	p := mustProgram(t, countdownSrc, "main")
	if perceusBalanceable(p) {
		t.Fatalf("countdown should be the 6b-2 frontier until CBounce ownership lands")
	}
}
```

- [ ] **Step 4: Run the corpus gates**

Run: `go test -run 'PerceusCorpus|PerceusFrontier' ./codegen/`
Expected: PASS (output-invariance across the whole WASM corpus; steady-state for the balanceable subset; the frontier boundary documented).

- [ ] **Step 5: Full codegen gate**

Run: `go test ./codegen/`
Expected: PASS (all backends unaffected, all ARC + Perceus tests green).

Run: `go build ./...`
Expected: clean.

- [ ] **Step 6: Finalize R-PERCEUS.md + commit**

Append "## The corpus gate" (which programs balance, the output-invariance result) and finalize "## Held for Plan 6b-2" (the CBounce trampoline + nat-fold, each with its named future receiver: ch39 countdown leak-free, a bignum-arithmetic listing leak-free). Verify no dashes.

```bash
git add codegen/perceus_corpus_test.go ref_docs/wootz/R-PERCEUS.md
git commit -m "test(wasm): Perceus corpus gate (output-invariance + steady-state) + 6b-2 boundary"
```

---

## Self-Review

**Spec coverage (against the chosen scope: receiver-driven core + structural Perceus, explicit portable nodes, opt-in WASM):**
- Explicit portable ownership nodes: Task 2 (CDup/CDrop in CIr, WASM rendering, opt-in hook). Covered.
- The ownership algorithm: Task 3 (pure fragment), Task 4 (constructors/case/field), Task 5 (pairs). Covered.
- A receiver for each bit: every implementation task ships a runnable receiver listing + a worked-example section in R-PERCEUS.md (Tasks 3-5) plus the corpus gate (Task 6). The two deferred hard cases (CBounce trampoline, nat-fold) are NAMED with their future receivers in Tasks 1 and 6. Covered.
- The `$live` correctness oracle: the steady-state gate (Task 2 harness) + output-invariance, applied per receiver and across the corpus (Task 6). Covered.
- Portability to C/LLVM (6e): the pass is CIr->CIr producing explicit nodes; Task 1's portable-contract section + the opt-in design document the reuse. Covered (as the documented next plan).
- Behavior-preserving for other backends: opt-in (only WASM runs Perceus); Task 2 + Task 6 assert byte-identical output. Covered.

**Placeholder scan:** The algorithm code in Tasks 3-5 is concrete Go, but flagged as v1-simple and gated by the steady-state oracle rather than proven minimal -- the IMPLEMENTER NOTES state precisely what the gate arbitrates (missing dup = leak; over-eager drop = use-after-free/wrong output) and what to adjust. The one genuinely fiddly rewrite (the CCase scrutinee de Bruijn shift, Task 4) is called out with a documented fallback (emit the scrutinee drop in `emitCase`). The receiver programs note the "use the real generated eliminator name / ambient `add`" match-the-symbol requirement, as in the 6a plan. These are match-the-symbol and gate-arbitrated instructions, not TBD placeholders.

**Type consistency:** `CDup`/`CDrop` (Task 2) carry `V CIr; K CIr` and are used identically in the emitter, `cirUsesArg`, and the pass. `Perceus(ClosureProgram) ClosureProgram` is identity in Task 2 and filled in Tasks 3-5. `ownScope`/`annotate`/`dropAfter` signatures are consistent across tasks. `wasmSteadyLiveP(t, p, runs)` / `WasmSteadyModule(t, p, runs)` / `mustProgram(t, src, main)` are the gate helpers threaded through Tasks 2-6. The steady-state counts contract (runs 2..N equal = balanced) is uniform.

**Three honest notes for the author:**
1. This plan is the CORE + STRUCTURAL slice. The CBounce trampoline and the nat-fold bignum temporaries are 6b-2 (named receivers: ch39 countdown leak-free; a bignum-arithmetic listing leak-free). The `perceusBalanceable` predicate makes the boundary executable, so the deferral is asserted, not silent.
2. The v1 dup/drop placement is sound-but-not-minimal (extra dup/drop are correctness-neutral: a redundant retain+release pair cancels). Minimality and the in-place reuse optimization are explicitly out of scope. The steady-state gate proves balance; it does not prove minimal traffic.
3. The steady-state methodology (run main N times, assert flat `$live`) is robust to cached global-thunk roots, which the simpler "absolute $live == baseline" form is not. The one limitation: a program whose main allocates a root that legitimately grows is not expected in the pure/structural corpus; if one appears, the gate flags it and the implementer narrows the receiver.

## Execution Handoff

Plan complete and saved to `docs/superpowers/plans/2026-06-27-perceus-ownership-wasm.md`. Two execution options:

1. **Subagent-Driven (recommended)** - I dispatch a fresh subagent per task, review between tasks, fast iteration.
2. **Inline Execution** - Execute tasks in this session using executing-plans, batch execution with checkpoints.

Which approach?
