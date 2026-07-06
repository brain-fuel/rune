# Native ARC, Plan B: the LLVM mirror + scale-gate re-admission Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Mirror the landed C-backend ARC conversion (v3.370.0) into the LLVM backend's external-linkage twin runtime, then remove the c/ll exclusion from the bible real-data scale gate.

**Architecture:** The C backend's ARC conversion IS the reference: every task here ports a landed, reviewed c.go decision into ll_runtime.go (the external-linkage twin of cRuntime) and ll.go (the .ll emitter), keeping the "== c.go" sync discipline. The .ll emitter is statement-oriented, so CDup/CDrop lower to plain `call void @rt_retain/@rt_release` instructions - simpler than C's statement expressions. Finish by deleting the scale-gate exclusion, the prize of the whole 6e conversion.

**Tech Stack:** Go (codegen), C (llRuntimeC twin), LLVM IR text, clang/llc toolchain (ll gates skip without it), ASAN.

## Global Constraints

- Spec: `docs/superpowers/specs/2026-07-05-native-arc-design.md` (Decisions 3-5; acceptance items 1-4). Plan A landed at v3.370.0 - the C implementation in codegen/c.go is the AUTHORITATIVE ownership reference for every mirror decision; where c.go and older prose disagree, c.go governs.
- **Byte-identity is absolute**: no existing test's expected OUTPUT changes. TestLLConformsToC is the cross-backend behavioral lock.
- Kernel untouchable (core/, store/, elaborate/, surface/); WASM emission untouched (wasm*.go frozen); C backend files (c.go, c_arc_test.go) frozen in this plan EXCEPT the shared-comment sync markers if a mirror requires none - default zero c.go edits.
- codegen/perceus.go and codegen/closure.go: NO changes expected (the pass and StepIgnoresIH peel are already backend-portable from Plan A). If a genuine LL-only pass gap appears, STOP and report - do not edit shared files without controller sign-off.
- The external-linkage twin discipline: ll_runtime.go's C functions are non-static (extern) twins of c.go's static ones; keep the `== c.go` sync-marker comments updated. The two never share a translation unit.
- Ownership contract (from landed c.go, verbatim): apply() borrows its closure operand and CONSUMES its argument; builders own their slot args; CGlobal/CForeign reads are BORROWED (accessors memoized as cached roots); foreign prim bodies receive BORROWED args and return OWNED results; TERMINAL bodies release their owned direct arg when not stored/returned.
- Known shared residuals stay parked (pair-projection leak, natDispatch extra-args constant residue) - LL inherits them exactly as C and WASM carry them; do not attempt the pass-level fix here.
- Commit with explicit pathspecs; Conventional Commits; suites with `-timeout 30m`.

## Reference map

- LLVM backend: codegen/ll.go (1375 lines) - `Emit` (:49, `cp := ClosureConvert(p)` at :50), `EmitRuntime` (:201) / `EmitRuntimeFor` (:215, prim-gated runtime assembly), `emitDefThunk` (:783), `emitCtorLL` (:823), `emitCachedThunk` (:866), `emitNatLL` (:892), llFunc emitIn cases (:1047-1111: CVar..CBounce; add CDup/CDrop here), `emitPartialLL` (:1221), StepIgnoresIH consumer (:995). codegen/ll_runtime.go (619 lines) - `llRuntimeC` (:74, the mark-sweep twin: gc_objs/:106, roots :113, stack_bottom :115, RUNE_GC_THRESHOLD :109).
- The landed C reference: codegen/c.go - arc_alloc/rt_retain/rt_release/walker (grep `rt_release` for the walker), `RUNE_ARC_REPORT`/arc_report, satCtorDispatch/satElimDispatch, the CForeign per-call-site static cache lowering, terminal-prim arg releases, foldLines/tramp protocols. codegen/c_arc_test.go - the 12-test gate family + buildAndRunCWithReport (twin its shapes, not its C-specific plumbing).
- LL gates: codegen/ll_test.go - TestLLEmitAndRunNat (:119), TestLLConformsToC (:151), TestLLGCConformanceUnderTinyHeap (:199, uses -DRUNE_GC_THRESHOLD via :66 and succChain from codegen_test.go); clang-gated skips.
- The prize: harness/bible_conformance_test.go:388-404 - `if bk.name == "c" || bk.name == "ll" { ...continue }` with the "native GC too slow at N=1500" comment; PARKING-LOT.md O(N_live) entry (annotated C-superseded in Plan A; closes fully here).
- Interim ladder (same as Plan A): rc runtime without annotations = leak-only = outputs unchanged; every task ends green.

---

### Task 1: rc runtime core in the LLVM twin

**Files:**
- Modify: `codegen/ll_runtime.go` (the llRuntimeC string: header swap, arc_alloc, rt_retain/rt_release/walker, delete mark-sweep incl RUNE_GC_THRESHOLD/roots/stack scan), `codegen/ll.go` (emitDefThunk/emitCachedThunk/main-emission: drop gc_add_root + stack-bottom hooks; add the RUNE_ARC_REPORT atexit; the `.ll` extern declarations block ~ll_runtime.go:15-20 gains `rt_retain`/`rt_release` declarations if the emitter will call them - declare now, call in Task 2)
- Test: `go test ./codegen/ -run 'TestLL' -count=1`

**Interfaces:**
- Consumes: the landed C runtime in codegen/c.go as the source text to mirror (external-linkage variants: drop `static` where c.go's twin function is extern in the current llRuntimeC).
- Produces: extern `void rt_retain(Value)`, `void rt_release(Value)`, `long rt_live`, `RUNE_ARC_REPORT` atexit in llRuntimeC; arc_alloc replacing gc_alloc at every llRuntimeC call site.

- [ ] **Step 1: mirror the header + allocator + retain/release/walker from c.go**

Open c.go's ARC section (grep `ARC - Perceus-style` in codegen/c.go) and transcribe into llRuntimeC with the twin's linkage rules: the Obj header change (gc_next/gc_mark -> `long rc;`), `arc_alloc` (calloc, rc=1, rt_live++), `rt_counted` (null/IS_INT/UNIT guard - note llRuntimeC's UNIT is an extern global, same comparison), `rt_retain`, `rt_release` with the per-kind walker EXACTLY as c.go has it (K_CLO env, K_CON fields, K_PAIR both, K_BOUNCE slots[0..nfield), leaves default). Preserve the `== c.go` sync markers, updating their text. Delete: gc_objs/gc_live_bytes/gc_threshold/RUNE_GC_THRESHOLD, roots array + gc_add_root, gc_stack_bottom + setjmp scan, gc_collect/gc_mark_*/gc_find_obj/gc_obj_size. `rt_retain`/`rt_release`/`rt_live` must be EXTERN (the .ll module calls them); `arc_report` static behind RUNE_ARC_REPORT.

- [ ] **Step 2: emitter-side hook removal (ll.go)**

Mirror Plan A Task 1 Step 4: `emitDefThunk` (:783) and `emitCachedThunk` (:866) drop their gc_add_root emission (grep ll.go for gc_add_root - every site); the emitted main drops stack-bottom capture and UNIT root registration, keeps UNIT init, gains the guarded `atexit(arc_report)` twin (whatever form c.go's emitted main uses - check how ll.go emits main and mirror). Add `declare void @rt_retain(i64)` / `declare void @rt_release(i64)` to the .ll extern declarations block (ll_runtime.go:15-20 region or wherever foreignNames/extern decls are assembled in ll.go).

- [ ] **Step 3: gates (interim leak-only)**

Run: `go test -timeout 20m ./codegen/ -run 'TestLL' -count=1 -v 2>&1 | tail -20`
Expected: TestLLEmitAndRunNat + TestLLConformsToC PASS (byte-identical; nothing released yet). TestLLGCConformanceUnderTinyHeap now references a deleted flag: mark it `t.Skip("mark-sweep deleted; ARC pressure gate replaces this in Task 3")` in this task (Task 3 deletes it for real). Then `go test -timeout 30m ./harness/ -run 'BackendConformance' -count=1` PASS.

- [ ] **Step 4: Commit**

```bash
git add codegen/ll_runtime.go codegen/ll.go codegen/ll_test.go
git commit -m "feat(codegen): LLVM twin runtime swaps mark-sweep for ARC core (mirrors c.go)" -- codegen/ll_runtime.go codegen/ll.go codegen/ll_test.go
```

---

### Task 2: Perceus into LL.Emit + dispatch/prim mirror

**Files:**
- Modify: `codegen/ll.go` (Perceus call site at :50; CDup/CDrop cases in the emitIn switch :1047 region; ownership alignment of emitNatLL/satCtor-satElim twins/accel/bounce/tramp/emitPartialLL with c.go's landed lowerings; the CForeign accessor memoization twin - ll.go already has emitCachedThunk, verify accessor reads route through a cached root; prim bodies in EmitRuntimeFor's blocks: float/BLAS/CPython/d6 twins get the SAME PATH B sweep c.go's got, including terminal-body owned-arg releases)
- Create: `codegen/ll_arc_test.go` (buildAndRunLLWithReport helper + first balance tests)
- Test: itself + LL gates + conformance

**Interfaces:**
- Consumes: Task 1's extern rt_retain/rt_release + RUNE_ARC_REPORT.
- Produces: annotated LL emission; `buildAndRunLLWithReport(t, src, mainName) (string, int64)` helper (compile .ll + runtime.c with clang, -DRUNE_ARC_REPORT on the runtime TU, parse rt_live from stderr) that Task 3 reuses.

- [ ] **Step 1: failing steady test first**

Create `codegen/ll_arc_test.go` with the helper (model the clang compile-link plumbing on ll_test.go's existing runner; add -DRUNE_ARC_REPORT when compiling the runtime C) and twin c_arc_test.go's TestCARCSteadyLoop as `TestLLARCSteadyLoop` (same rune source, same two-sizes rt_live-invariance assertion, 64-object tolerance). Run: expect FAIL (scaling) - nothing annotated yet.

- [ ] **Step 2: wire the pass + the two cases**

ll.go:50: `cp := ClosureConvert(p)` gains `cp = Perceus(cp)`. In the llFunc emitIn switch add, mirroring the WASM statement lowering (wasm.go:808-817) in .ll form:

```go
case CDup:
	v := f.emitIn(b, x.V, locals)
	fmt.Fprintf(b, "  call void @rt_retain(i64 %s)\n", v)
	return f.emitIn(b, x.K, locals)
case CDrop:
	v := f.emitIn(b, x.V, locals)
	fmt.Fprintf(b, "  call void @rt_release(i64 %s)\n", v)
	return f.emitIn(b, x.K, locals)
```

(Adjust the value-rendering/register plumbing to emitIn's actual conventions - read the CLet case first; V is a CVar/CEnv, pure.)

- [ ] **Step 3: mirror the landed C ownership lowerings**

For each, read c.go's LANDED version and port to .ll emission: satCtorDispatch + satElimDispatch (c.go has them since Plan A Task 4; ll.go's existing sat handling at emitNatLL/:892 region aligns to the same release protocol - motive released, c0/c1 moved into the fold env, x released once), accelDispatch operand releases, the trampoline protocol in emitPartialLL/tramp (bounce owns args, intermediate apply-closures released - c.go's tramp is the reference), CForeign accessors as cached borrowed roots (ll.go's emitCachedThunk pattern - verify every foreign accessor read routes through it; if ll.go allocates fresh per read like C did, apply the same per-call-site cache fix), and StepIgnoresIH already peels (shared closure.go - no change, but verify ll.go:995's call still behaves since LL NOW runs Perceus and WILL see annotated IR: the peel exists exactly for this).

- [ ] **Step 4: the prim-body sweep (LL twins)**

Every prim body emitted for LL (EmitRuntimeFor blocks + any emitFloatPrimsLL/emitCPython/d6 twins - grep ll.go and ll_runtime.go for `static Value` and rt_-prefixed helpers): apply the SAME sweep c.go's prim bodies got, using each C body as the line-for-line reference (borrowed args/env, owned results, retain-before-consuming-borrowed in apply loops, terminal bodies release the owned direct arg, discarded intermediates released). Where llRuntimeC shares helpers (d6 codec, big_divmod), Task 1's mirror already carries c.go's fixed versions - verify big_divmod's non-aliasing rewrite came across.

- [ ] **Step 5: gates**

Run in order, all PASS byte-identical:
1. `go test ./codegen/ -run 'TestLLARCSteadyLoop' -v` - now PASS.
2. `go test -timeout 20m ./codegen/ -count=1` - whole package incl WASM guards + TestLLConformsToC.
3. `go test -timeout 30m ./harness/ -run 'BackendConformance|Bible|D3|D4|D6|Foreign' -count=1` - the ll rows execute the prim bodies.
Any ll-row crash or output drift = ownership bug; fix against the c.go reference, never weaken.

- [ ] **Step 6: Commit**

```bash
git add codegen/ll.go codegen/ll_runtime.go codegen/ll_arc_test.go
git commit -m "feat(codegen): LLVM backend runs Perceus; ownership mirrors the landed C ARC" -- codegen/ll.go codegen/ll_runtime.go codegen/ll_arc_test.go
```

---

### Task 3: LL ARC gate family + tiny-heap retirement

**Files:**
- Modify: `codegen/ll_arc_test.go` (grow to the family), `codegen/ll_test.go` (DELETE TestLLGCConformanceUnderTinyHeap + its threshold helper :66; keep succChain import intact or inline it if it was codegen_test.go-hosted)
- Test: itself

**Interfaces:**
- Consumes: buildAndRunLLWithReport from Task 2.
- Produces: TestLLARC family: RecursiveRelease, PairBalance, ClosureEnvBalance, BignumBalance, BytesBalance, Pressure, UnderASAN.

- [ ] **Step 1: twin the C family**

Port each c_arc_test.go shape (same rune sources - lift them into shared consts or duplicate verbatim per file convention; check whether a shared test-source helper already exists between codegen tests, else duplicate with a comment naming the twin) as TestLLARC* with the same two-size invariance assertions, incl the pressure shape (the loop-churn one, NOT a deep list - c_arc_test.go documents why) and the terminal-prim/fold shapes IF the ll prim plumbing supports them in-process (foldLines needs a temp file - same as the C test).

- [ ] **Step 2: ASAN + tiny-heap deletion**

TestLLARCUnderASAN: compile runtime.c + program with -fsanitize=address (probe first, t.Skip if unsupported; clang required anyway for ll). Delete TestLLGCConformanceUnderTinyHeap and the `-DRUNE_GC_THRESHOLD` helper from ll_test.go; grep ll_runtime.go for residue (expect none after Task 1).

- [ ] **Step 3: run + commit**

Run: `go test -timeout 30m ./codegen/ -run 'TestLLARC|TestLL' -count=1 -v 2>&1 | tail -25` - all PASS/SKIP-clean.

```bash
git add codegen/ll_arc_test.go codegen/ll_test.go
git commit -m "test(codegen): LLVM ARC gate family; tiny-heap GC test retired" -- codegen/ll_arc_test.go codegen/ll_test.go
```

---

### Task 4: the prize - scale-gate re-admission

**Files:**
- Modify: `harness/bible_conformance_test.go:388-404` (delete the c/ll exclusion + its comment; the WASM keep-in comment at :402-404 may simplify), `PARKING-LOT.md` (the O(N_live) entry: CLOSED, superseded by 6e ARC both backends), `docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md` (item 13 fully DONE), `codegen/ll_runtime.go`/`codegen/c.go` doc headers if they reference the exclusion
- Test: the N=1500 gate itself

**Interfaces:**
- Consumes: everything above.

- [ ] **Step 1: remove the exclusion**

Delete the `if bk.name == "c" || bk.name == "ll" { ... continue }` block and its "native GC too slow" comment. Read the surrounding test to confirm c/ll rows now flow through the same emit+cc+run path the gate uses for the other backends (runNativeListing exists at :638 - the real-data gate may need to route native rows through it; wire minimally, changing NO assertions).

- [ ] **Step 2: run the real-data gate**

Run: `go test -timeout 30m ./harness/ -run 'BibleConformanceRealData|BibleConformance' -count=1 -v 2>&1 | tail -20` (find the exact gate name at :380 region first).
Expected: PASS with c and ll rows included, byte-identical to the other backends, in sane wall-clock (record the timing in your report; if a native row exceeds the 30m suite budget materially, STOP and report the numbers - do not re-exclude silently).

- [ ] **Step 3: close the parking-lot entry + docs**

PARKING-LOT O(N_live) entry: mark CLOSED (superseded by 6e ARC, both native backends in the scale gate as of this commit). Beta checklist item 13: fully DONE. No em/en dashes in new text.

- [ ] **Step 4: full suite + commit**

Run: `go test -timeout 30m ./...` - ALL PASS.

```bash
git add harness/bible_conformance_test.go PARKING-LOT.md docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md
git commit -m "feat(harness): native c/ll rejoin the bible real-data scale gate (6e complete)" -- harness/bible_conformance_test.go PARKING-LOT.md docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md
```
