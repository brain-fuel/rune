# Native ARC, Plan A: the C backend Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the C backend's mark-sweep GC with Perceus ARC (the WASM discipline), byte-identical on every existing gate, with native ARC balance gates.

**Architecture:** The shared Perceus pass (codegen/perceus.go) annotates the ClosureProgram the C emitter already consumes; the emitter lowers CDup/CDrop to rt_retain/rt_release via GNU statement expressions; the cRuntime block swaps its GC header/collector for an rc header + malloc/free + per-kind free walker. Prim bodies get a PATH B ownership sweep. LLVM is Plan B (do not touch ll.go/ll_runtime.go/ll_test.go).

**Tech Stack:** Go (codegen package), C (the emitted runtime string), cc/gcc/clang for gates, ASAN where available.

## Global Constraints

- Spec: `docs/superpowers/specs/2026-07-05-native-arc-design.md`. Read its Decision 4 ownership rules before any task; they bind every task here.
- **Byte-identity is absolute**: no existing test's expected OUTPUT may change. ARC is memory-only.
- Kernel untouchable: no changes to core/, store/, elaborate/, surface/, hash format.
- WASM emission must stay byte-identical (the Perceus pass may be extended, never altered in a way that changes wasm.go's output; `go test ./codegen/ -run 'Wasm|ARC|Perceus'` guards it).
- Plan B owns LLVM: zero edits to codegen/ll.go, codegen/ll_runtime.go, codegen/ll_test.go in this plan.
- Ownership rules (spec Decision 4, verbatim): apply() consumes both operands; builders (mkclo env slots, mkcon fields, mkpair halves, bounce args) take ownership of slot arguments; CGlobal thunk reads and CForeign accessor reads are BORROWED; foreign prim bodies receive BORROWED args and return OWNED results; dup-on-consume-borrowed.
- Commit with explicit pathspecs; Conventional Commits; `go test -timeout 30m` for suite runs (default 10m too short).
- Multiple agents may share the repo; work only in the designated worktree.

## Reference map (all tasks)

- C emitter: codegen/c.go — `Emit` (:62), `emitDefThunk` (:257), `emitBlock` (:274), `exprIn` (:397, expression-oriented via GNU statement exprs `({ ... })`), `mkClosure` (:470), `caseExpr` (:487), `natDispatch` (:375), `accelDispatch` (grep), `bounceExpr` (grep), prim blocks (:717-1268), `cRuntime` (:1275-~1825), `cQuotRuntime` (:1832), `cIORuntime` (:1866). Object model: fat `Obj` struct (kind + gc_next/gc_mark + union-ish fields + `Value slots[1]` flexible tail); immediates are odd-tagged ints (`IS_INT`); `UNIT` is a global Value singleton made in main (c.go:194) and rooted.
- Perceus pass: codegen/perceus.go — `Perceus(cp ClosureProgram) ClosureProgram` (:283), inserts `CDup{V,K}`/`CDrop{V,K}` (closure.go:141-152; V is always a CVar/CEnv, pure). WASM lowering reference: wasm.go:808-817 (retain/release then evaluate K), emitCase (:898), emitBounce (:829), Perceus call site wasm.go:52.
- Existing gates: codegen/codegen_test.go (C tests; tiny-heap GC test uses `-DRUNE_GC_THRESHOLD` at :215), harness/backend_conformance_test.go (corpus rows incl c), harness/bible_conformance_test.go (builders byte-identity; scale gate still EXCLUDES c/ll until Plan B), WASM ARC family codegen/wasm_arc_test.go + wasm_steady_test.go (the patterns Task 4 twins).
- Interim soundness ladder: rc runtime WITHOUT annotations = nothing ever released = exactly the old allocation behavior minus collection (pure leak) = all outputs unchanged. Each task ends green.

---

### Task 1: rc runtime core (retain/release/walker; mark-sweep deleted)

**Files:**
- Modify: `codegen/c.go` — the `cRuntime` string (:1275-1520 region) + `emitDefThunk` (:257) + the main()-emission (grep `gc_stack_bottom` and `gc_add_root` call sites in emitted main, c.go:190-200 region)
- Test: existing `go test ./codegen/ -run 'TestC'` + full C-consuming gates

**Interfaces:**
- Produces (C runtime symbols later tasks rely on): `void rt_retain(Value v)`, `void rt_release(Value v)`, `static long rt_live` counter, `RUNE_ARC_REPORT` compile-time flag (atexit prints `rt_live=<n>` to stderr). `arc_alloc(size_t)` replaces `gc_alloc` (same call sites).

- [ ] **Step 1: swap the Obj header**

In `cRuntime`: replace the two GC header fields with an rc field:

```c
typedef struct Obj {
  int kind;
  long rc;              /* ARC reference count; 1 at birth */
  Value (*code)(Value, Value*);
  int nenv;
  int tag;
  const char* name;
  int nfield;
  const char* str;
  long handle;
  double dval;
  Value slots[1];
} Obj;
```

(`gc_next`/`gc_mark` deleted; everything else byte-for-byte as today.)

- [ ] **Step 2: replace gc_alloc with arc_alloc; delete the collector**

DELETE from `cRuntime`: the whole GC comment block and machinery — `gc_objs`, `gc_live_bytes`, `gc_threshold`, `RUNE_GC_THRESHOLD`, `gc_static_roots`/`gc_n_static_roots`/`gc_add_root`, `gc_stack_bottom`, `#include <setjmp.h>`, `gc_collect` (decl + def), `gc_obj_size`, `gc_mark_obj`/`gc_mark_value`, `gc_find_obj`, and the conservative-scan code. Add in their place:

```c
/* ===========================================================================
   ARC — Perceus-style reference counting (the WASM backend's discipline,
   spec docs/superpowers/specs/2026-07-05-native-arc-design.md Decision 3/4).
   Ownership rules (PATH B): apply() consumes both operands; the mk* builders
   take ownership of their slot arguments; thunk-cache reads (CGlobal) and
   foreign accessors are BORROWED (callers dup on consumption); foreign prim
   bodies receive borrowed args and return owned results. rune heap values are
   immutable and acyclic, so counting is complete — no cycle collector.
   =========================================================================== */
static long rt_live = 0;

#ifdef RUNE_ARC_REPORT
static void arc_report(void) { fprintf(stderr, "rt_live=%ld\n", rt_live); }
#endif

static void* arc_alloc(size_t n) {
  size_t a = sizeof(void*);
  n = (n + a - 1) & ~(a - 1);
  Obj* o = (Obj*)calloc(1, n);   /* zeroed: half-built objects have null slots */
  if (!o) { fprintf(stderr, "rune-c: out of memory\n"); exit(1); }
  o->rc = 1;
  rt_live++;
  return o;
}
```

Rename every `gc_alloc(` call site in the runtime strings to `arc_alloc(` (mkclo/mkcon/mkpair/mkstr/mkptr/mkfloat/mkunit/mkbig/bytes/bounce — grep `gc_alloc` in c.go, expect ~10-14 sites across cRuntime + prim blocks).

- [ ] **Step 3: rt_retain / rt_release / the free walker**

Add after `arc_alloc` (note: `obj()` and `UNIT` are declared later in the runtime today — forward-declare as needed, or place these after `obj()`; keep C compiling):

```c
static Obj* obj(Value v);           /* fwd decl (defined below as today) */
extern Value UNIT;                  /* the boxed singleton made in main */

static int rt_counted(Value v) {
  if (v == 0 || IS_INT(v) || v == UNIT) return 0;
  return 1;
}

static void rt_retain(Value v) {
  if (!rt_counted(v)) return;
  obj(v)->rc++;
}

static void rt_release(Value v) {
  if (!rt_counted(v)) return;
  Obj* o = obj(v);
  if (--o->rc > 0) return;
  /* rc hit zero: release children per kind, then free the block. */
  switch (o->kind) {
    case K_CLO:
      for (int i = 0; i < o->nenv; i++) rt_release(o->slots[i]);
      break;
    case K_CON:
      for (int i = 0; i < o->nfield; i++) rt_release(o->slots[i]);
      break;
    case K_PAIR:
      rt_release(o->slots[0]); rt_release(o->slots[1]);
      break;
    case K_BOUNCE:
      /* slots[0] = the head partial's step thunk result (counted), then the
         collected args — mirror gc_mark_obj's old K_BOUNCE tracing exactly. */
      for (int i = 0; i < o->nfield; i++) rt_release(o->slots[i]);
      break;
    /* K_STR: ->str points at a C string literal or an emitter-owned buffer we
       never free (audit in this step: every mkstr producer passes a literal —
       if any passes a heap buffer, strdup there and free here). K_PTR, K_UNIT,
       K_FLOAT: leaves. K_BIG / K_BYTES: limbs/bytes live INLINE in slots[]
       (see old gc_obj_size), freed with the block. */
    default: break;
  }
  rt_live--;
  free(o);
}
```

Check the old `gc_mark_obj` before writing the walker: the per-kind traced-slot sets there are AUTHORITATIVE — the walker must release exactly the slots the marker traced (K_BOUNCE especially). Audit all `mkstr(` producers for the K_STR note.

- [ ] **Step 4: thunk caches and main lose their GC hooks**

- `emitDefThunk` (c.go:257): delete the `gc_add_root(&cache);` line and its comment (the cache now simply holds one never-released reference — that IS the root).
- The emitted `main` (c.go:190-200 region): delete the `gc_stack_bottom` capture and the `gc_add_root(&UNIT)` root registration; keep `UNIT = mkunit();`. Add, guarded:

```go
b.WriteString("#ifdef RUNE_ARC_REPORT\n  atexit(arc_report);\n#endif\n")
```

- `cQuotRuntime`/`cIORuntime`/prim blocks: any other `gc_add_root` call sites (grep) are deleted the same way — a static closure cache holding a reference IS its root.

- [ ] **Step 5: run the C gates (interim: leak-only, outputs unchanged)**

Run: `go test -timeout 20m ./codegen/ -run 'TestC|TestCodegen'` then `go test -timeout 30m ./harness/ -run 'BackendConformance'`
Expected: PASS, byte-identical outputs (nothing is ever released yet; that is the ladder). The old tiny-heap C test in codegen_test.go (:215, `-DRUNE_GC_THRESHOLD`) now FAILS TO COMPILE the flag away — mark it skipped with `t.Skip("mark-sweep deleted; ARC pressure gate replaces this in Task 4")` in THIS task so the suite stays green (Task 4 replaces it for real).

- [ ] **Step 6: Commit**

```bash
git add codegen/c.go codegen/codegen_test.go
git commit -m "feat(codegen): C runtime swaps mark-sweep for ARC core (rc header, retain/release, walker)" -- codegen/c.go codegen/codegen_test.go
```

---

### Task 2: Perceus annotations lowered in the C emitter

**Files:**
- Modify: `codegen/c.go` — `Emit` (:62, add the pass), `exprIn` (:397, two new cases), plus the ownership alignment of `caseExpr`/`natDispatch`/`accelDispatch`/`bounceExpr` with their WASM twins
- Possibly modify: `codegen/perceus.go` — ONLY if a spine-recognition gap for the C accel paths surfaces (the pass already references accelMatchC; extend recognition there, never hack the emitter around a wrong annotation)
- Test: `codegen/c_arc_test.go` (new, first balance smoke)

**Interfaces:**
- Consumes: Task 1's `rt_retain`/`rt_release`/`RUNE_ARC_REPORT`.
- Produces: the C path emits Perceus-annotated code; test helper `buildAndRunCWithReport(t, src, mainName) (stdout string, rtLive int64)` in c_arc_test.go that compiles with `-DRUNE_ARC_REPORT`, runs, parses the stderr `rt_live=N` line (Task 4 reuses it).

- [ ] **Step 1: write the failing balance smoke**

Create `codegen/c_arc_test.go`. Model the compile-and-run scaffolding on the existing C tests in codegen_test.go (cc lookup + skip, temp dir, build). The test: a program that allocates in a loop and discards, run at two sizes; ARC balance means final `rt_live` is INVARIANT in the iteration count.

```go
package codegen

// (imports and the buildAndRunCWithReport helper modeled on codegen_test.go's
// C compile-run helper, adding -DRUNE_ARC_REPORT to the cc command line and
// parsing the final "rt_live=" line from stderr.)

// TestCARCSteadyLoop: builds double-chains of two very different depths and
// checks the retained heap does not scale with the work done — releases fire.
func TestCARCSteadyLoop(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
double : Nat -> Nat is
  fn (n : Nat) is case n of | zero -> zero | succ k with ih -> succ (succ ih) end end
end
main : Nat is double %d end`
	_, liveSmall := buildAndRunCWithReport(t, fmt.Sprintf(src, 50), "main")
	_, liveBig := buildAndRunCWithReport(t, fmt.Sprintf(src, 5000), "main")
	if liveBig-liveSmall > 64 {
		t.Fatalf("retained heap scales with work: rt_live %d (n=50) vs %d (n=5000) - releases not firing", liveSmall, liveBig)
	}
}
```

(If builtin-nat accel folds `double` away, pick a shape the accel does not fold — e.g. build and case-consume a cons list; the point is loop-scaled allocation. Note what you chose in the report.)

- [ ] **Step 2: run it, verify it fails**

Run: `go test ./codegen/ -run TestCARCSteadyLoop -v`
Expected: FAIL — no releases are emitted yet, `liveBig` scales with n.

- [ ] **Step 3: wire the pass and the two cases**

In `Emit` (c.go:62 path), immediately after the `ClosureConvert(p)` call (c.go:63): `cp = Perceus(cp)` — mirroring wasm.go:51-52. In `exprIn`, add before `default:`:

```go
case CDup:
	// Retain V (a CVar/CEnv -- pure) then evaluate K. Statement expression
	// keeps the emitter expression-oriented (same pattern as CLet).
	return fmt.Sprintf("({ rt_retain(%s); %s; })", em.exprIn(x.V, locals), em.exprIn(x.K, locals))
case CDrop:
	return fmt.Sprintf("({ rt_release(%s); %s; })", em.exprIn(x.V, locals), em.exprIn(x.K, locals))
```

- [ ] **Step 4: align the special-form lowerings with WASM's ownership**

Read the WASM twins first; each C lowering must implement the SAME ownership the pass assumed:
- `caseExpr` (c.go:487) vs wasm emitCase (wasm.go:898): where the WASM lowering releases the scrutinee / retains extracted fields, the C lowering does the identical operation (statement-expression form). If WASM's emitCase leaves everything to pass-inserted CDrops, C does too — do not invent extra ops.
- `bounceExpr` vs wasm emitBounce (wasm.go:829): the bounce owns its collected args; the trampoline driver (`tramp` in cRuntime) must release each intermediate apply-closure exactly as `$rt_tramp` does (wasm_runtime.go:728-786) — port that protocol into the C `tramp`.
- `natDispatch`/`accelDispatch`: compare with the pass's spine recognition (perceus.go, grep isRecognizedSpine/accelMatch). The invariant: whatever ownership the pass assumes for recognized-spine operands (borrowed vs consumed), the C accel helpers must match. If C's accel spines are NOT recognized by the pass while WASM's are, extend the recognition in perceus.go (shared, backend-tagged if needed) — and then re-run the WASM guard suite to prove WASM bytes unchanged.
- `mkClosure`/`CPair`/ctor fills: builders take ownership — with calloc-zeroed slots and pass-inserted dups this is already the semantics; verify no double-retain is emitted.

- [ ] **Step 5: run the smoke + the byte-identity gates + the WASM guard**

Run: `go test ./codegen/ -run 'TestCARCSteadyLoop' -v` — PASS.
Run: `go test -timeout 20m ./codegen/ -run 'TestC|TestCodegen|Wasm|ARC|Perceus'` — PASS (WASM untouched).
Run: `go test -timeout 30m ./harness/ -run 'BackendConformance'` — PASS byte-identical.
Any output difference or crash here is an ownership bug: fix per the rules, never by weakening a gate.

- [ ] **Step 6: Commit**

```bash
git add codegen/c.go codegen/perceus.go codegen/c_arc_test.go
git commit -m "feat(codegen): C emitter lowers Perceus CDup/CDrop; ownership aligned with WASM" -- codegen/c.go codegen/perceus.go codegen/c_arc_test.go
```

---

### Task 3: prim-body ownership sweep (PATH B at the foreign edge)

**Files:**
- Modify: `codegen/c.go` prim blocks (:717-1268: d6 fs/stream/sort/db, float/BLAS, bin, crypto, TLS, proc, CPython embed) + `cIORuntime`/`cQuotRuntime` (:1832-)
- Test: existing FFI/D3/D4/D6 native gates + one new targeted test

**Interfaces:**
- Consumes: Task 1's rules and Task 2's annotated call sites.
- Produces: every emitted C prim body obeys: args BORROWED (never released by the body), results OWNED (fresh rc=1 allocations, or explicitly retained when returning a borrowed input), and every internal `apply(f, x)` respects apply-consumes (retain `f` before each reuse, retain any borrowed value passed as `x`).

- [ ] **Step 1: the mechanical audit rule**

For every `static Value <prim>_c*(...)` body in the prim blocks:
1. `env[i]` reads and the direct argument are BORROWED — the body must not release them, and must `rt_retain` them if it stores them into a constructor/pair/closure it returns (builders take ownership) or passes them to `apply` (apply consumes).
2. Loops that reuse a function value across `apply` calls retain it per call: `foldLines_c5` (c.go:788) does `s = apply(apply(apply(step, s), line), UNIT)` per line — `step` needs `rt_retain(step)` before each outer apply, `s` is consumed and replaced (correct as-is), `line` is fresh/owned (correct), `UNIT` is uncounted (correct). `d6_foldwalk` (:805) identical pattern.
3. Values the body creates and does NOT return are released before returning (e.g. intermediate cons cells replaced in a fold are consumed by the next apply — fine; a discarded `none` after building `some` must be released).
4. `d6_s2h`-style helpers that read a packed value's bytes into a malloc buffer do not touch rc (borrowed read) — unchanged.
Sweep order: d6 block, float/BLAS, bin, crypto/TLS, proc, CPython embed, then cIORuntime's IO chain runner and cQuotRuntime.

- [ ] **Step 2: targeted failing test first**

Add to `codegen/c_arc_test.go` a fold-under-report test — ch548-family foldLines is the highest-risk shape (apply-in-loop):

```go
// TestCARCFoldPrimBalance: a foldLines over a temp file at two sizes; retained
// heap must not scale with line count (the prim loop retains `step` per call
// and releases nothing it borrowed).
```

Write it against the existing host-op listing shape (see harness/bible_ops_test.go for the foldLines listing source to crib; run via the same buildAndRunCWithReport). Run: expect FAIL (or crash) before the sweep, PASS after. If it unexpectedly PASSES before the sweep, verify the annotations already cover it and say so in the report - then the sweep is a verification pass, still required.

- [ ] **Step 3: sweep, then run every foreign-edge gate**

Run: `go test -timeout 30m ./codegen/ -run 'TestC'` and `go test -timeout 30m ./harness/ -run 'D3|D4|D6|FFI|Bible|Furnace'`
Expected: PASS byte-identical (these suites execute the prim bodies on the C backend; crashes = double release, ASAN in Task 4 will double-check).

- [ ] **Step 4: Commit**

```bash
git add codegen/c.go codegen/c_arc_test.go
git commit -m "feat(codegen): C prim bodies follow PATH B ownership (borrowed args, owned results)" -- codegen/c.go codegen/c_arc_test.go
```

---

### Task 4: the native ARC gate family (+ tiny-heap tests replaced, + ASAN)

**Files:**
- Modify: `codegen/c_arc_test.go` (grow the family), `codegen/codegen_test.go` (delete the skipped tiny-heap C test)
- Test: itself

**Interfaces:**
- Consumes: `buildAndRunCWithReport` from Task 2.
- Produces: the C ARC gate family (steady, recursive-release, pair/closure/bignum/bytes balance, pressure, ASAN).

- [ ] **Step 1: twin the WASM ARC family**

Read codegen/wasm_arc_test.go; for each behavior it pins (recursive release through CON chains, pair release, closure env release, leaf kinds, bignum, bytes) write the C twin as a steady-style assertion: run the shape at a small and a large size under `-DRUNE_ARC_REPORT`, assert rt_live invariance (the caches make absolute baselines meaningless; INVARIANCE is the pin). One test per shape, names `TestCARCRecursiveRelease`, `TestCARCPairBalance`, `TestCARCClosureEnvBalance`, `TestCARCBignumBalance`, `TestCARCBytesBalance`.

- [ ] **Step 2: the pressure gate (replaces tiny-heap)**

```go
// TestCARCPressure: allocates and discards ~200MB of cons cells in a loop.
// Under mark-sweep this needed a tiny -DRUNE_GC_THRESHOLD to force collections;
// under ARC the release walker IS the collector. Gate: the process completes
// AND rt_live stays flat - if releases never fired the loop retains ~200MB of
// cells and rt_live betrays it even where the machine has RAM to hide it.
```

Implement with a list-build-and-sum program sized so leak-vs-release differs by >1e6 objects; assert rt_live invariance across two sizes AND successful completion. Then DELETE the skipped tiny-heap C test and its `-DRUNE_GC_THRESHOLD` helper from codegen_test.go (:215 region) — Plan B handles the ll twin. Grep c.go for any dead `RUNE_GC_THRESHOLD` residue.

- [ ] **Step 3: ASAN variant**

Add `TestCARCUnderASAN`: reuse the pressure + fold shapes compiled with `-fsanitize=address` (probe support by compiling a 3-line program first; `t.Skip` if the toolchain lacks ASAN). A double-free or use-after-free anywhere in the walker/ownership fails loudly here.

- [ ] **Step 4: run the family + commit**

Run: `go test -timeout 30m ./codegen/ -run 'TestCARC'`
Expected: all PASS.

```bash
git add codegen/c_arc_test.go codegen/codegen_test.go
git commit -m "test(codegen): native C ARC gate family (balance, pressure, ASAN); tiny-heap GC test retired" -- codegen/c_arc_test.go codegen/codegen_test.go
```

---

### Task 5: docs + full-suite proof

**Files:**
- Modify: `codegen/c.go` package doc (:11-54 — the GC rationale paragraphs), `ref_docs/wootz/R-PERCEUS.md` (a "native C port landed" note at the portability aside, :54-57), `PARKING-LOT.md` (:476 O(N_live) entry: note the C side is superseded by ARC, LL + scale gate land in Plan B), `docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md` (item 13: C half done, LLVM half next)
- Test: the whole suite

**Interfaces:**
- Consumes: everything above.

- [ ] **Step 1: rewrite the c.go package doc GC section**

Replace the mark-sweep rationale (c.go:42-54) with the ARC story: Perceus pass shared with WASM, rc header, PATH B rules (point at the spec), why counting is complete (immutable acyclic values), and that LLVM mirrors in Plan B. No em/en dashes.

- [ ] **Step 2: doc notes**

R-PERCEUS.md: at the :54-57 portability aside add one sentence - the C port landed (this plan), the pass now has C call sites. PARKING-LOT.md :476: annotate the entry (C superseded by ARC; entry fully closes with Plan B). Beta checklist item 13: mark the C half done with the plan/commit reference.

- [ ] **Step 3: full suite**

Run: `go test -timeout 30m ./...`
Expected: PASS everywhere (WASM divergence locks, bible builders, listings, infra - everything). Any failure traces to an ownership bug or an unintended byte change; fix, never waive.

- [ ] **Step 4: Commit**

```bash
git add codegen/c.go ref_docs/wootz/R-PERCEUS.md PARKING-LOT.md docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md
git commit -m "docs: native C ARC landed (package doc, R-PERCEUS, parking-lot, beta checklist)" -- codegen/c.go ref_docs/wootz/R-PERCEUS.md PARKING-LOT.md docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md
```
