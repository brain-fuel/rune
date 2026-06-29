# WASM Partial Support Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the WASM backend lower and run `partial`/general-recursion programs (the `CBounce` trampoline node) ARC-correctly, retiring the last exclusion from the WASM Perceus flat fragment.

**Architecture:** Mirror the C backend's trampoline split (a `partial` becomes a `_step` thunk + curried driver blocks + a public driver that `tramp`s), but add the ARC ownership the GC'd native backends never needed. A new `K_BOUNCE` heap kind holds the cached step closure (borrowed) plus the eagerly-evaluated owned args; a `$rt_tramp` loop re-applies the step to the args one iteration at a time, shell-freeing each spent bounce (args moved into the step call), so deep tail recursion runs in O(1) WASM stack at steady-flat heap.

**Tech Stack:** Go (the emitter + Perceus pass); WAT (the emitted runtime); `wasmtime` (the conformance runner); the existing 8-backend conformance harness + the `$rt_live` steady-flat probe.

## Global Constraints

- Kernel FROZEN: no core/store change, no hash change (current 0x06). WASM codegen + runtime only.
- Output BYTE-IDENTICAL across all backends: ARC adjusts refcounts only, never the returned result expression.
- NO em-dashes or en-dashes anywhere (code, comments, commits, docs). Use `--` in prose.
- Conventional Commits. End every commit message with the trailer:
  `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`
- C1/C-1 memory-safety class: every behavioral receiver asserts OUTPUT CORRECTNESS in addition to the leak/flat property, so a use-after-free or double-free surfaces as wrong output, not just a refcount delta. (A double-free in this class previously survived per-task review and was caught only by the final whole-branch review.)

**Memory-model facts the implementer must rely on (do NOT re-derive):**
- `CBounce{Call CIr}` (`codegen/closure.go:139`) is the closure-converted saturated tail call to a partial-group member. `Call` is the `AppClosure` spine whose head is a `CGlobal` naming the member.
- `ClosureProgram.Partials map[string]bool` flags partial def names; `CDefSpec.Arity` is the curried leading-lambda count.
- C reference (`codegen/c.go`): `mkbounce(step, nargs)` stores `slot[0]=step, slots[1..n]=args, tag=nargs`; `tramp(v)` while `v` is `K_BOUNCE`: `f=slot[0]; for i in 1..nargs: f=apply(f, slot[i]); v=f`. `emitPartialC` (c.go:521) splits a partial into a memoized `_step` thunk + `arity` curried driver blocks + a public thunk; the last driver saturates `_step` and `tramp`s. The native backends use mark-sweep GC, so there is NO ARC across the bounce chain -- this plan adds it.
- The step closure stored in a bounce is the cached `_step()` result (a memoized singleton shared by every bounce of that partial), so it is BORROWED -- the bounce does not own it and `$rt_tramp` does not release it. Only the args are owned (freshly evaluated per bounce).
- WASM runtime (`codegen/wasm_runtime.go`): kind-tagged bump heap (`K_CLO=0`, `K_CON=1`, `K_PAIR=2`, `K_UNIT=5`, `K_BIG=6`), 8-byte `[size][rc]` header. `$alloc(n)` returns a payload pointer with rc=1; `$sw`/`$w` store/load word i; `$rt_mkclo`/`$rt_apply` build/apply closures; `$rt_release` decrements rc and calls `$rt_free` at zero; `$rt_free` releases child pointers by kind (a `$done` block sets `$n` child count + `$base` first-child word, then a loop releases `$n` slots from `$base`, then `$live--` and pushes to the size-classed free list).
- WASM emitter (`codegen/wasm.go`): `emitDefs` (wasm.go:208) routes every def through `emitDefThunk`; `emitCachedThunk` (wasm.go:357) wraps a memoized body; `emitCtor`/`emitCtorBlock` (wasm.go:394/418) are the EXACT structural template for curried driver blocks (collect args into env via `$rt_clo_set`/`$rt_env`, last block does the saturating work); `emitIn` (wasm.go:558) is the `CIr` dispatch whose `default` panics.
- Perceus (`codegen/perceus.go`): `annotate` (perceus.go:259) is the ownership pass; its `default` arm (perceus.go:494) currently carries `CBounce` through UNCHANGED (no ownership). `consumeOwning(t)` (perceus.go:120) returns a CIr whose result value is OWNED (dup-on-consume-borrowed). `cirUnbalanceable` (perceus.go:768) flags `CBounce` as unbalanceable (`case CBounce: return true`, perceus.go:770); `PerceusBalanceable` (perceus.go:741) scans every def/block body with it.
- Test helpers (`codegen/*_test.go`): `mustProgram(t, src, main) cg.Program`; `emitWith(t, cg.Wasm{}, src, main) string` (the WAT); `runWasm(t, wat) string` (runs under wasmtime, returns stdout); `assertSteadyFlatInts(t, p, runs)` (asserts per-run `$rt_live` delta is 0 across runs 2..runs); `cg.WasmRuntime() string` (read-only runtime access).

---

### Task 1: WASM runtime bounce machinery (K_BOUNCE + mkbounce + tramp + shell-free)

Add the heap kind and the three runtime functions the emitter will target, plus the full-free branch for the dead-without-forcing path. Reviewer gate: runtime memory-safety in isolation, before any emitter drives it.

**Files:**
- Modify: `codegen/wasm_runtime.go` (the `wasmRuntime` const: the `$rt_free` `$done` dispatch ~lines 507-530; append the three new funcs before the closing backtick ~line 553)
- Test: `codegen/wasm_test.go` (a synthetic bounce round-trip)

**Interfaces:**
- Produces (WAT functions, called by Task 2's emitter and by `$rt_release`):
  - `$rt_mkbounce (param $step i32) (param $nargs i32) (result i32)` -- allocate a `K_BOUNCE` object `[kind=7][step][nargs][arg0..argN-1]` (step at word 1, nargs at word 2, args at words 3..). Returns the payload pointer (rc=1); arg slots are filled by the emitter via `$rt_bounce_set`.
  - `$rt_bounce_set (param $b i32) (param $i i32) (param $x i32)` -- store arg `$x` into slot `$i` (0-based; word `3+$i`).
  - `$rt_tramp (param $v i32) (result i32)` -- the driver loop. While `$v` is a `K_BOUNCE`: read `$step` (word 1) + `$nargs` (word 2), apply each arg slot to the step via `$rt_apply`, `$rt_bounce_free_shell` the spent bounce, set `$v` to the result. Return the first non-`K_BOUNCE` value.
  - `$rt_bounce_free_shell (param $b i32)` -- free the bounce's memory WITHOUT releasing the arg slots (moved into the step call) or the step (borrowed): `$live--` then push the block onto its size-class free list. (Mirrors the tail of `$rt_free`.)
- Consumes: `$alloc`, `$sw`, `$w`, `$rt_apply`, `$rt_bucket_addr`, the `$live`/`$freelist` globals.

- [ ] **Step 1: Write the failing synthetic round-trip test**

Add to `codegen/wasm_test.go`. `cg.WasmRuntime()` returns the BARE runtime body (functions + memory + the `$hp`/`$live`/`$sbuf`/`$freelist`/`$UNIT` globals), NOT a `(module ...)` wrapper, and it READS three program-emitted globals (`$abort_msg`, `$abort_len`, `$fn_msg`, normally emitted by `emitData`). So a standalone test module must add the `fd_write` import, the `$codety` apply type, a 1-slot table, and STUB those three globals. Use an immediate-int step (`$rt_mkint`) so the test does not depend on the unset `$UNIT` singleton.

A `K_BOUNCE` with 0 args: `$rt_tramp` reads slot[0]=step (an immediate int), applies nothing, shell-frees, and returns the int (a non-bounce). The post-tramp `$rt_live` must equal the baseline (the shell was reclaimed; the immediate int is not heap-counted).

```go
// wasmTestModule wraps the bare runtime body (cg.WasmRuntime()) into a standalone
// module for a synthetic runtime test: it adds the fd_write import, the apply type, a
// 1-slot table, and STUBS the three program-emitted globals the runtime references
// ($abort_msg/$abort_len/$fn_msg, normally emitted by emitData). startBody is spliced
// as the body of a $main exported as _start (wasmtime invokes the WASI _start export).
func wasmTestModule(runtimeBody, startBody string) string {
	return `(module
  (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (type $codety (func (param i32 i32) (result i32)))
  (table 1 funcref)
  (global $abort_msg i32 (i32.const 32))
  (global $abort_len i32 (i32.const 5))
  (global $fn_msg i32 (i32.const 64))
` + runtimeBody + `
  (func $main (export "_start")
` + startBody + `))`
}

// TestWasmBounceRuntimeRoundTrip pins the Task-1 runtime: a 0-arg K_BOUNCE of an
// immediate-int step tramps to a non-bounce and shell-frees, leaving $rt_live at the
// pre-bounce baseline (the shell was reclaimed, the borrowed step is an immediate).
func TestWasmBounceRuntimeRoundTrip(t *testing.T) {
	rt := cg.WasmRuntime()
	for _, fn := range []string{"$rt_mkbounce", "$rt_tramp", "$rt_bounce_free_shell", "$rt_bounce_set"} {
		if !strings.Contains(rt, fn) {
			t.Fatalf("runtime missing %s", fn)
		}
	}
	start := `    (local $b i32) (local $r i32) (local $base i32)
    (local.set $base (global.get $live))
    (local.set $b (call $rt_mkbounce (call $rt_mkint (i32.const 7)) (i32.const 0)))
    (local.set $r (call $rt_tramp (local.get $b)))
    (if (i32.eqz (call $is_int (local.get $r))) (then unreachable))
    (call $rt_print_u32 (i32.sub (global.get $live) (local.get $base)))`
	if got := runWasm(t, wasmTestModule(rt, start)); strings.TrimSpace(got) != "0" {
		t.Fatalf("bounce round-trip live delta = %q, want 0", got)
	}
}
```

- [ ] **Step 2: Run it to verify it fails**

Run: `go test ./codegen/ -run TestWasmBounceRuntimeRoundTrip -count=1`
Expected: FAIL -- `runtime missing $rt_mkbounce` (the funcs do not exist yet).

- [ ] **Step 3: Add the K_BOUNCE full-free branch to `$rt_free`**

In `codegen/wasm_runtime.go`, inside `$rt_free`'s `$done` block, add a `K_BOUNCE=7` branch BEFORE the leaf fallthrough (after the `K_PAIR` branch ~line 527). It mirrors `K_CLO`: child count `$n` at word 2 (`nargs`), children start at `$base=3` (the args). The borrowed step at word 1 is NOT a child here -- a bounce that reaches rc=0 without forcing owns only its args:

```wat
      ;; K_BOUNCE=7: arg slots start at word 3, count (nargs) at word 2. The step at
      ;; word 1 is the borrowed cached _step closure -- not owned, not released here.
      (if (i32.eq (local.get $kind) (i32.const 7))
        (then
          (local.set $n (call $w (local.get $v) (i32.const 2)))
          (local.set $base (i32.const 3))
          (br $done)))
```

- [ ] **Step 4: Append the three bounce functions to the runtime**

In `codegen/wasm_runtime.go`, just before the closing backtick of `wasmRuntime` (after `$rt_print_u32`, ~line 553), add:

```wat
  ;; ---- T2 trampoline: [K_BOUNCE=7][step][nargs][arg0..] ----
  ;; A partial's saturated tail call lowers to a K_BOUNCE: the cached _step closure
  ;; (slot at word 1, BORROWED) plus nargs eagerly-evaluated OWNED args (words 3..).
  (func $rt_mkbounce (param $step i32) (param $nargs i32) (result i32)
    (local $o i32)
    (local.set $o (call $alloc (i32.add (i32.const 12) (i32.shl (local.get $nargs) (i32.const 2)))))
    (call $sw (local.get $o) (i32.const 0) (i32.const 7))      ;; K_BOUNCE
    (call $sw (local.get $o) (i32.const 1) (local.get $step))
    (call $sw (local.get $o) (i32.const 2) (local.get $nargs))
    (local.get $o))
  (func $rt_bounce_set (param $b i32) (param $i i32) (param $x i32)
    (call $sw (local.get $b) (i32.add (i32.const 3) (local.get $i)) (local.get $x)))

  ;; shell-free: reclaim the bounce object WITHOUT releasing the args (moved into the
  ;; step call) or the step (borrowed). Mirrors the live--/freelist tail of $rt_free.
  (func $rt_bounce_free_shell (param $b i32)
    (local $size i32) (local $bkt i32)
    (global.set $live (i32.sub (global.get $live) (i32.const 1)))
    (local.set $size (i32.load (i32.sub (local.get $b) (i32.const 8))))
    (local.set $bkt (call $rt_bucket_addr (local.get $size)))
    (if (local.get $bkt)
      (then
        (i32.store (local.get $b) (i32.load (local.get $bkt)))
        (i32.store (local.get $bkt) (local.get $b)))))

  ;; tramp: force the bounce chain. While v is a K_BOUNCE, re-apply its step to its
  ;; args (one partial-body iteration -> the next bounce or a value), shell-free the
  ;; spent bounce (args MOVED into the applies), and continue. O(1) WASM stack.
  (func $rt_tramp (param $v i32) (result i32)
    (local $step i32) (local $nargs i32) (local $i i32) (local $f i32)
    (block $done (loop $lp
      ;; immediates are never bounces; a heap value is a bounce iff word0 == 7.
      (br_if $done (call $is_int (local.get $v)))
      (br_if $done (i32.ne (call $w (local.get $v) (i32.const 0)) (i32.const 7)))
      (local.set $step (call $w (local.get $v) (i32.const 1)))
      (local.set $nargs (call $w (local.get $v) (i32.const 2)))
      (local.set $f (local.get $step))
      (local.set $i (i32.const 0))
      (block $ab (loop $al
        (br_if $ab (i32.ge_u (local.get $i) (local.get $nargs)))
        (local.set $f (call $rt_apply (local.get $f)
          (call $w (local.get $v) (i32.add (i32.const 3) (local.get $i)))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $al)))
      (call $rt_bounce_free_shell (local.get $v))
      (local.set $v (local.get $f))
      (br $lp)))
    (local.get $v))
```

- [ ] **Step 5: Run the test to verify it passes**

Run: `go test ./codegen/ -run TestWasmBounceRuntimeRoundTrip -count=1`
Expected: PASS (live delta 0; the unit result has kind word 5).

Note on `$rt_apply` ownership: a 0-arg bounce never applies, so Task 1 does not exercise the apply path. Whether the first `$rt_apply` of a borrowed step needs a dup is settled empirically by Task 2's deep-iteration steady-flat receiver, where any per-iteration rc imbalance shows as a nonzero `$rt_live` delta. Do NOT add a speculative dup here.

- [ ] **Step 6: Confirm the whole runtime still assembles**

Run: `go test ./codegen/ -run 'TestWasm' -count=1`
Expected: PASS (the appended funcs do not break existing WASM emission; `wasmtime` validates the module).

- [ ] **Step 7: Commit**

```bash
git add codegen/wasm_runtime.go codegen/wasm_test.go
git commit -m "$(printf 'feat(wasm): K_BOUNCE runtime -- mkbounce/tramp/shell-free + rt_free branch\n\nThe heap machinery for partial-trampoline lowering: a K_BOUNCE holds the\nborrowed cached step closure + owned args; rt_tramp forces the chain in\nO(1) stack, shell-freeing each spent bounce (args moved into the step\napplies). rt_free gains the full-free branch for the unforced path.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 2: Emitter lowering + Perceus ownership (parity + ARC)

The C1 task. Emit a `partial` def as a `_step` thunk + driver blocks + public tramp thunk; lower `CBounce` to a bounce object; annotate the bounce spine args as owned so the trampoline is leak-free and double-free-free. Deliverable: ch39 runs on WASM byte-identical to the other backends AND reaches steady-flat across every operand-ownership shape.

**Files:**
- Modify: `codegen/wasm.go` (`emitDefs` ~line 224 to route partials; new `emitPartialWasm`; new `case CBounce:` in `emitIn` ~line 627 before the `default` panic)
- Modify: `codegen/perceus.go` (`annotate` ~line 494: replace the `CBounce` default carry-through with an explicit owned-spine case)
- Test: `codegen/perceus_test.go` (parity + the ARC shape suite + the dangerous-line receiver)

**Interfaces:**
- Consumes: Task 1's `$rt_mkbounce`/`$rt_bounce_set`/`$rt_tramp`; `consumeOwning` (perceus.go:120); `emitCachedThunk`, `emitCtorBlock`'s currying pattern, `codeRef`, `wasmThunkName`, `wasmName`, `f.fresh`, `f.emitIn`, `$rt_apply`, `$rt_env`, `$rt_clo_set`, `$rt_mkclo`.
- Produces: `emitPartialWasm(b *strings.Builder, name string, arity int, body CIr)`; a `wasmStepName(name) string` helper (= `wasmThunkName(name)+"_step"`); the `CBounce` annotate + emit behavior. No exported signature changes.

- [ ] **Step 1: Write the failing parity receiver**

Add to `codegen/perceus_test.go`. `countdownSrc` already exists in this file (the ch39-style partial). Assert it RUNS on WASM to `zero`:

```go
// TestPerceusPartialCountdownRunsWasm: the ch39-style partial countdown lowers and
// runs on the WASM backend (today: panics "unknown CIr node codegen.CBounce"). The
// headline parity gate -- WASM joins the other 8 backends on general recursion.
func TestPerceusPartialCountdownRunsWasm(t *testing.T) {
	if got := runWasm(t, emitWith(t, cg.Wasm{}, countdownSrc, "countdownMain")); got != "zero" {
		t.Fatalf("countdown 3 on wasm: got %q, want zero", got)
	}
}
```

- [ ] **Step 2: Run it to verify it fails**

Run: `go test ./codegen/ -run TestPerceusPartialCountdownRunsWasm -count=1`
Expected: FAIL -- a panic `codegen(wasm): unknown CIr node codegen.CBounce` (emitIn has no case).

- [ ] **Step 3: Add the Perceus ownership case for CBounce**

In `codegen/perceus.go`, replace the `default` carry-through (perceus.go:494-496) with an explicit `CBounce` case BEFORE `default`. The bounce's args must each be an owned reference (the bounce owns them; the trampoline moves them into the step applies). Route the spine through `consumeOwning` so each arg leaf is dup'd into an owned reference exactly as the accel/ctor spines do. The head `CGlobal` (the step) stays borrowed:

```go
	case CBounce:
		// The bounce object owns its eagerly-evaluated args (the trampoline moves them
		// into the step applies, then shell-frees the bounce). Make each arg an OWNED
		// reference via consumeOwning down the spine; the head CGlobal (the cached _step
		// closure) stays borrowed. Mirrors annotateBareSpine's per-operand consumeOwning.
		return CBounce{Call: pp.annotateBounceSpine(x.Call, owned)}

	default:
		// No remaining CIr node is unhandled; keep the panic so a new node is caught.
		panic(fmt.Sprintf("codegen(perceus): annotate unknown CIr node %T", t))
	}
```

Then add the helper near `annotateBareSpine` (perceus.go ~576). It walks the `AppClosure` spine, annotating + `consumeOwning`-ing each `Arg` (so a bare borrowed leaf becomes an owned `CDup`/`CLet`, an owned local passes through) and leaving the head `CGlobal` bare:

```go
// annotateBounceSpine annotates a CBounce's application spine so every ARGUMENT is an
// owned reference (the bounce object owns its arg slots; $rt_tramp moves them into the
// step applies). The head CGlobal stays borrowed (the cached _step closure singleton).
// It is the bounce analogue of annotateBareSpine, but every operand is consumeOwning'd
// (a bounce always materializes its args into heap slots -- there is no emitter shortcut
// that borrows them, unlike the accel/nat spines).
func (pp *perceusPass) annotateBounceSpine(t CIr, owned []bool) CIr {
	app, ok := t.(AppClosure)
	if !ok {
		// Bare head (CGlobal) or an unrecognized shape: annotate as usual.
		return pp.annotate(t, owned)
	}
	return AppClosure{
		Clo: pp.annotateBounceSpine(app.Clo, owned),
		Arg: consumeOwning(pp.annotate(app.Arg, owned)),
	}
}
```

(Verify the exact name of the existing unknown-node panic in `annotate` before replacing `default`; if `annotate` previously had NO panic and relied on the `default` carry-through for other nodes, keep a carry-through `default` and place the `CBounce` case above it instead. Read perceus.go:259-497 first.)

- [ ] **Step 4: Add `emitPartialWasm` and route partials in `emitDefs`**

In `codegen/wasm.go`, change `emitDefs` (wasm.go:224-226) to route partial defs:

```go
	// Definition thunks (eliminators converted as CDefSpec, then user defs). A `partial`
	// def is split into a _step body + curried drivers + a public trampoline thunk.
	for _, def := range em.cp.Defs {
		if em.cp.Partials[def.Name] {
			em.emitPartialWasm(b, def.Name, def.Arity, def.Body)
			continue
		}
		em.emitDefThunk(b, def.Name, def.Body)
	}
```

Add `wasmStepName` near `wasmThunkName` and `emitPartialWasm` (mirror `emitPartialC` + `emitCtor`/`emitCtorBlock`). The `_step` thunk is the memoized body; the `arity` driver blocks collect args into the closure env exactly like constructor curry blocks; the last driver saturates `_step()` with the collected args via `$rt_apply` and wraps the result in `$rt_tramp`; the public thunk caches a closure entering driver 0:

```go
func wasmStepName(name string) string { return wasmThunkName(name) + "_step" }

// emitPartialWasm lowers a `partial` def to the T2 trampoline split (the WASM dual of
// emitPartialC): a memoized _step thunk (the body, where a CBounce is a K_BOUNCE), then
// `arity` curried driver code blocks that collect args into the closure env, the last of
// which saturates _step and drives the bounce chain via $rt_tramp; and a public thunk
// returning a closure entering driver 0.
func (em *wasmEmitter) emitPartialWasm(b *strings.Builder, name string, arity int, body CIr) {
	// _step: the memoized body (a curried lambda of `arity` args; a tail call inside is
	// a K_BOUNCE). Emitted under a distinct cached-thunk name.
	em.emitCachedThunk(b, name+"\x00step", func(f *wasmFunc, bb *strings.Builder) string {
		return f.emit(bb, body, nil)
	})
	step := wasmStepName(name)

	if arity == 0 {
		// No args to collect: the public thunk drives the (bounce-free) step once.
		em.emitCachedThunk(b, name, func(f *wasmFunc, bb *strings.Builder) string {
			r := f.fresh()
			fmt.Fprintf(bb, "    (local.set %s (call $rt_tramp (call $%s)))\n", r, step)
			return "(local.get " + r + ")"
		})
		return
	}

	base := "drvfn_" + wasmName(name)
	for i := 0; i < arity; i++ {
		em.emitPartialDriverBlock(b, fmt.Sprintf("%s_%d", base, i), name, arity, i)
	}
	em.emitCachedThunk(b, name, func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, em.codeRef(base+"_0"))
		return "(local.get " + r + ")"
	})
}

// emitPartialDriverBlock emits the i-th curry block of a partial's public driver. For
// i<arity-1 it builds the next driver closure capturing env[0..i-1] + arg (identical to
// emitCtorBlock's collect arm). For i==arity-1 it saturates _step() with env[0..i-1] +
// arg via $rt_apply, then $rt_tramp's the result.
func (em *wasmEmitter) emitPartialDriverBlock(b *strings.Builder, fname, name string, arity, i int) {
	em.codeRef(fname)
	base := "drvfn_" + wasmName(name)
	step := wasmStepName(name)
	f := &wasmFunc{em: em}
	var inner strings.Builder
	var ret string
	if i < arity-1 {
		cl := f.fresh()
		fmt.Fprintf(&inner, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const %d)))\n", cl, em.codeRef(fmt.Sprintf("%s_%d", base, i+1)), i+1)
		for k := 0; k < i; k++ {
			fmt.Fprintf(&inner, "    (call $rt_clo_set (local.get %s) (i32.const %d) (call $rt_env (local.get $env) (i32.const %d)))\n", cl, k, k)
		}
		fmt.Fprintf(&inner, "    (call $rt_clo_set (local.get %s) (i32.const %d) (local.get $arg))\n", cl, i)
		ret = "(local.get " + cl + ")"
	} else {
		fv := f.fresh()
		fmt.Fprintf(&inner, "    (local.set %s (call $%s))\n", fv, step)
		for k := 0; k < i; k++ {
			fmt.Fprintf(&inner, "    (local.set %s (call $rt_apply (local.get %s) (call $rt_env (local.get $env) (i32.const %d))))\n", fv, fv, k)
		}
		fmt.Fprintf(&inner, "    (local.set %s (call $rt_apply (local.get %s) (local.get $arg)))\n", fv, fv)
		fmt.Fprintf(&inner, "    (local.set %s (call $rt_tramp (local.get %s)))\n", fv, fv)
		ret = "(local.get " + fv + ")"
	}
	fmt.Fprintf(b, "  (func $%s (param $arg i32) (param $env i32) (result i32)\n", fname)
	b.WriteString(f.localDecls())
	b.WriteString(inner.String())
	fmt.Fprintf(b, "    %s)\n", ret)
}
```

Note on the `_step` cache name: `emitCachedThunk` derives its globals + thunk name from `wasmName(name)`. The `_step` thunk must NOT collide with the public thunk's name. Confirm `wasmThunkName`/`wasmName` mangling: if `wasmName("countdown\x00step")` yields a valid distinct WAT identifier matching `wasmStepName("countdown")` (= `wasmThunkName("countdown")+"_step"`), keep the `\x00step` suffix trick; otherwise add a dedicated `emitCachedThunkNamed(b, thunkName, globalsKey, body)` variant and pass `wasmStepName(name)` explicitly. Read `wasmThunkName`/`wasmName`/`emitCachedThunk` (wasm.go:357 + the name helpers) and pick whichever yields `$<step>` exactly as `emitPartialDriverBlock` calls it.

- [ ] **Step 5: Add the `CBounce` case to `emitIn`**

In `codegen/wasm.go`, add `case CBounce:` before the `default` panic (wasm.go:627). Mirror `bounceExpr` (c.go:488): walk the spine to the head `CGlobal`, build `$rt_mkbounce((call $<step>), nargs)`, store each evaluated arg via `$rt_bounce_set`. A non-`CGlobal` head falls back to a plain emit of the call:

```go
	case CBounce:
		return f.emitBounce(b, x.Call, locals)
```

And the method (near `emitCase`):

```go
// emitBounce lowers a CBounce (a saturated tail call to a partial member) to a K_BOUNCE:
// rt_mkbounce((call $<head>_step), nargs) then store each evaluated arg. The driver loop
// ($rt_tramp) forces the chain. A non-CGlobal head is not a recognizable partial spine,
// so emit the call directly (it runs, just not as a bounce).
func (f *wasmFunc) emitBounce(b *strings.Builder, call CIr, locals []string) string {
	var args []CIr
	t := call
	for {
		app, ok := t.(AppClosure)
		if !ok {
			break
		}
		args = append([]CIr{app.Arg}, args...)
		t = app.Clo
	}
	g, ok := t.(CGlobal)
	if !ok {
		return f.emitIn(b, call, locals)
	}
	o := f.fresh()
	fmt.Fprintf(b, "    (local.set %s (call $rt_mkbounce (call $%s) (i32.const %d)))\n", o, wasmStepName(g.Name), len(args))
	for i, a := range args {
		fmt.Fprintf(b, "    (call $rt_bounce_set (local.get %s) (i32.const %d) %s)\n", o, i, f.emitIn(b, a, locals))
	}
	return "(local.get " + o + ")"
}
```

- [ ] **Step 6: Run the parity receiver to verify it passes**

Run: `go test ./codegen/ -run TestPerceusPartialCountdownRunsWasm -count=1`
Expected: PASS (`countdown 3` -> `zero`). If it panics on an unknown step-thunk name, fix the `_step` naming per Step 4's note before proceeding.

- [ ] **Step 7: Write the ARC ownership-shape receivers**

Add to `codegen/perceus_test.go`. Each builds a partial whose tail call exercises one operand-ownership shape, and asserts BOTH steady-flat AND correct output. They share the `Nat`/`builtin nat` preamble from `countdownSrc` (factor a `partialNatPreamble` const = the `data Nat ... builtin nat Nat zero succ` lines if not already shared, or inline it):

```go
const partialNatPreamble = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
`

// A partial whose step DROPS an unused arg: the second param `tag` is owned on entry to
// each bounce and never used in the recursive tail, so it must be freed each iteration
// or the trampoline leaks. Output: counts `a` down to zero.
const partialDropSrc = partialNatPreamble + `
partial dropper : Nat -> Nat -> Nat is
  fn (a : Nat) (tag : Nat) is
    NatElim (fn (x : Nat) is Nat end) zero
      (fn (k : Nat) (ih : Nat) is dropper k tag end) a
  end
end
dropMain : Nat is dropper 2000 7 end
`

func TestPerceusPartialDropFlat(t *testing.T) {
	p := mustProgram(t, partialDropSrc, "dropMain")
	assertSteadyFlatInts(t, p, 5)
	if got := runWasm(t, emitWith(t, cg.Wasm{}, partialDropSrc, "dropMain")); got != "zero" {
		t.Fatalf("dropper: got %q, want zero", got)
	}
}

// A partial whose step DUPS an arg (uses `tag` twice: once carried, once added in). The
// bounce-owned ref plus the in-body use need one dup each iteration. Output: zero.
const partialDupSrc = partialNatPreamble + `
addN : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat end) b (fn (k : Nat) (ih : Nat) is succ ih end) a
  end
end
partial duper : Nat -> Nat -> Nat is
  fn (a : Nat) (tag : Nat) is
    NatElim (fn (x : Nat) is Nat end) (addN tag tag)
      (fn (k : Nat) (ih : Nat) is duper k tag end) a
  end
end
dupMain : Nat is duper 2000 3 end
`

func TestPerceusPartialDupFlat(t *testing.T) {
	p := mustProgram(t, partialDupSrc, "dupMain")
	assertSteadyFlatInts(t, p, 5)
	if got := runWasm(t, emitWith(t, cg.Wasm{}, partialDupSrc, "dupMain")); got != "zero" {
		t.Fatalf("duper: got %q, want zero", got)
	}
}

// A SHARED-owned-local arg: the tail call passes `n n` (one local, two bounce slots) so
// annotateBounceSpine + the enclosing dup must balance both consumes. Output: zero.
const partialSharedSrc = partialNatPreamble + `
partial shared : Nat -> Nat -> Nat is
  fn (a : Nat) (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) zero
      (fn (k : Nat) (ih : Nat) is shared2 k n n end) a
  end
end
partial shared2 : Nat -> Nat -> Nat -> Nat is
  fn (a : Nat) (p : Nat) (q : Nat) is
    NatElim (fn (x : Nat) is Nat end) zero
      (fn (k : Nat) (ih : Nat) is shared2 k p q end) a
  end
end
sharedMain : Nat is shared 1500 4 end
`

func TestPerceusPartialSharedFlat(t *testing.T) {
	p := mustProgram(t, partialSharedSrc, "sharedMain")
	assertSteadyFlatInts(t, p, 5)
	if got := runWasm(t, emitWith(t, cg.Wasm{}, partialSharedSrc, "sharedMain")); got != "zero" {
		t.Fatalf("shared: got %q, want zero", got)
	}
}
```

- [ ] **Step 8: Run the ARC receivers; fix rc placement until flat**

Run: `go test ./codegen/ -run 'TestPerceusPartial(Drop|Dup|Shared)Flat' -count=1`
Expected: PASS. If a `steady-state not flat` delta appears, the imbalance is in the apply chain or the spine annotation -- inspect whether the borrowed step needs a dup before the first `$rt_apply` (the cached `_step` singleton must not be consumed by apply), or whether `consumeOwning` is missing on a shared operand. Adjust `$rt_tramp`/`annotateBounceSpine` and re-run. Do NOT proceed until all three are flat AND output `zero`. (This is the C1 crux; the flat delta is the leak/double-free detector.)

- [ ] **Step 9: Write the dangerous-line receiver (full-free vs shell-free never double-free)**

A bounce built but never forced must free its args via `$rt_free`'s K_BOUNCE branch (full-free); a forced bounce frees only the shell. Construct a program whose partial conditionally returns a bounce value WITHOUT trampolining it in one arm -- hard to do organically, so assert it at the emit/runtime layer with a direct unit test that mkbounces an arg, releases it via `$rt_release` (full path), and checks `$rt_live` returns to baseline (the arg was freed exactly once):

```go
// TestWasmBounceUnforcedRelease: a K_BOUNCE built then released WITHOUT tramp (the
// totality path) frees its owned args exactly once via rt_free's K_BOUNCE branch --
// live returns to baseline, no double-free of the arg, the immediate-int step untouched.
// (Reuses wasmTestModule from Task 1.)
func TestWasmBounceUnforcedRelease(t *testing.T) {
	start := `    (local $arg i32) (local $b i32) (local $base i32)
    (local.set $base (global.get $live))
    (local.set $arg (call $rt_mkcon (i32.const 0) (i32.const 0) (i32.const 0))) ;; one owned K_CON, 0 fields
    (local.set $b (call $rt_mkbounce (call $rt_mkint (i32.const 1)) (i32.const 1)))
    (call $rt_bounce_set (local.get $b) (i32.const 0) (local.get $arg))
    ;; never tramp: release the whole bounce. rt_free's K_BOUNCE branch releases the arg.
    (call $rt_release (local.get $b))
    (call $rt_print_u32 (i32.sub (global.get $live) (local.get $base)))`
	if got := runWasm(t, wasmTestModule(cg.WasmRuntime(), start)); strings.TrimSpace(got) != "0" {
		t.Fatalf("unforced-bounce release live delta = %q, want 0 (arg freed once via full-free)", got)
	}
}
```

- [ ] **Step 10: Run the dangerous-line receiver**

Run: `go test ./codegen/ -run TestWasmBounceUnforcedRelease -count=1`
Expected: PASS (live delta 0). If it traps, the K_BOUNCE branch in `$rt_free` (Task 1 Step 3) has the wrong `$base`/`$n`; fix and re-run Task 1's test too.

- [ ] **Step 11: Run the full codegen suite (no regression)**

Run: `go test ./codegen/ -count=1`
Expected: PASS. `TestPerceusFrontierBoundary` is updated in Task 3 -- if it now FAILS here because `countdownSrc` no longer panics but `PerceusBalanceable` still returns false (the predicate is unchanged), that is fine: the partial RUNS but is still excluded from the asserted flat fragment until Task 3. Confirm the failure (if any) is ONLY `TestPerceusFrontierBoundary` and is the predicate-not-yet-flipped signal; do not change it here.

- [ ] **Step 12: Commit**

```bash
git add codegen/wasm.go codegen/perceus.go codegen/perceus_test.go
git commit -m "$(printf 'feat(wasm): lower partial/CBounce on WASM, ARC-correct trampoline\n\nemitPartialWasm splits a partial into a _step thunk + curried drivers +\na public rt_tramp thunk; emitIn lowers CBounce to a K_BOUNCE; the Perceus\npass makes each bounce arg an owned reference (consumeOwning down the\nspine) while the cached _step head stays borrowed. ch39 countdown runs on\nWASM to zero, steady-flat across drop/dup/shared operand shapes; the\nunforced-bounce path full-frees its args exactly once.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 3: Re-open PerceusBalanceable to partial programs

With the leak closed, drop the `CBounce` exclusion so partial programs join the asserted flat fragment, and flip the frontier receiver.

**Files:**
- Modify: `codegen/perceus.go` (`cirUnbalanceable` `case CBounce`, perceus.go:770-773; the doc comments at perceus.go:732-740 and the `cirUnbalanceable` header at 761-767)
- Modify: `codegen/perceus_test.go` (`TestPerceusFrontierBoundary` -> assert balanceable + flat)

**Interfaces:**
- Consumes: `PerceusBalanceable`, `assertSteadyFlatInts`, `countdownSrc`.
- Produces: `PerceusBalanceable` returns true for a flat partial program.

- [ ] **Step 1: Rewrite the frontier receiver to assert balanceable + flat**

Replace `TestPerceusFrontierBoundary` (perceus_test.go) with the flipped assertion (the move the accel listings made -- frontier test becomes a flat assertion):

```go
// TestPerceusPartialInFlatFragment: with CBounce lowered and ARC-correct (the bounce
// owns its args; $rt_tramp shell-frees each iteration), a partial-recursive countdown
// is now INSIDE the balanceable fragment and reaches steady-flat. This was
// TestPerceusFrontierBoundary (the last WASM Perceus exclusion); the flip records the
// closure.
func TestPerceusPartialInFlatFragment(t *testing.T) {
	p := mustProgram(t, countdownSrc, "countdownMain")
	if !cg.PerceusBalanceable(p) {
		t.Fatalf("CBounce lowered + ARC-correct: a flat partial program must be IN the flat fragment")
	}
	assertSteadyFlatInts(t, p, 5)
}
```

- [ ] **Step 2: Find any other test asserting CBounce/partial is skipped**

Run: `grep -rn "CBounce\|FrontierBoundary\|partial.*frontier\|OUTSIDE the v1" codegen/*_test.go`
Expected: only the (now-renamed) frontier test + the steady-corpus list. Note any corpus test that enumerates excluded programs so Step 4 can move `countdownSrc` from excluded to balanced.

- [ ] **Step 3: Run the rewritten receiver to verify it fails**

Run: `go test ./codegen/ -run TestPerceusPartialInFlatFragment -count=1`
Expected: FAIL -- `PerceusBalanceable` still returns false (the predicate excludes `CBounce`).

- [ ] **Step 4: Drop the CBounce exclusion in `cirUnbalanceable`**

In `codegen/perceus.go`, change the `case CBounce` (perceus.go:770-773) from flagging to recursing into the spine, so a CBounce no longer marks the program unbalanceable but its inner spine is still scanned (for over-applied NatElim etc.):

```go
	case CBounce:
		// CBounce (partial/trampoline tail call) is now LOWERED + ARC-correct on WASM
		// (emitPartialWasm + the K_BOUNCE runtime; the bounce owns its args, $rt_tramp
		// shell-frees each iteration). No longer an exclusion; scan the spine for any
		// remaining unbalanceable construct (e.g. an over-applied NatElim).
		return cirUnbalanceable(x.Call, natElim)
```

Update the `PerceusBalanceable` doc block (perceus.go:732-740) and the `cirUnbalanceable` header (761-767): the "ONE REMAINING excluded class (CBounce)" text becomes "no missing-feature exclusion remains; only the over-applied NatElim spine is flagged." Keep the dash convention (`--`).

- [ ] **Step 5: Run the rewritten receiver to verify it passes**

Run: `go test ./codegen/ -run TestPerceusPartialInFlatFragment -count=1`
Expected: PASS (balanceable + steady-flat).

- [ ] **Step 6: Run the full codegen suite**

Run: `go test ./codegen/ -count=1`
Expected: PASS (the over-applied NatElim frontier tests stay green -- `cirUnbalanceable` still flags that spine; only the CBounce exclusion is gone).

- [ ] **Step 7: Commit**

```bash
git add codegen/perceus.go codegen/perceus_test.go
git commit -m "$(printf 'feat(wasm): re-open PerceusBalanceable to partial programs\n\nCBounce is now lowered + ARC-correct, so cirUnbalanceable scans a bounce\nspine instead of flagging it. The last WASM Perceus exclusion is gone;\nthe ch39 countdown joins the asserted flat fragment.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 4: Documentation

Record the closure in the design ledger + the phase map.

**Files:**
- Modify: `ref_docs/wootz/R-PERCEUS.md` (the "ONE REMAINING excluded class" section + the corpus flat table)
- Modify: `CLAUDE.md` (the WASM-backend / frontier note)

**Interfaces:** none (docs only).

- [ ] **Step 1: Update R-PERCEUS.md**

In `ref_docs/wootz/R-PERCEUS.md`, change the "### The ONE REMAINING excluded class" section (the CBounce/partials item) to state it is CLOSED: CBounce is lowered on WASM via `emitPartialWasm` + the `K_BOUNCE` runtime; the bounce owns its eagerly-evaluated args while the cached `_step` closure is borrowed; `$rt_tramp` shell-frees each spent bounce (args moved into the step applies) and `$rt_free` full-frees an unforced bounce; the trampoline is steady-flat. Move the partial/countdown listing from "excluded" to "balanced" in the corpus table. State that NO missing-feature exclusion remains (only the over-applied NatElim spine is still flagged by `cirUnbalanceable`). Match the file's `--` dash convention (no em-dashes).

- [ ] **Step 2: Update the CLAUDE.md frontier note**

In `CLAUDE.md`, find the WASM-backend frontier line (the `wasm-backend-ninth-and-frontier` summary that lists C-REG / R-GLUE / general-adequacy as open). Add one concise line: WASM `partial`/general-recursion support landed (CBounce lowered + ARC trampoline + flat-fragment re-opened), bringing the 9th backend to partial parity with the other 8. No core change, no hash bump. Reference `ref_docs/wootz/R-PERCEUS.md`. No em-dashes.

- [ ] **Step 3: Commit**

```bash
git add ref_docs/wootz/R-PERCEUS.md CLAUDE.md
git commit -m "$(printf 'docs(wasm): record WASM partial support closure (last Perceus exclusion)\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

## Final verification (after all tasks)

- [ ] `go test ./codegen/ -count=1` -- all green, including the new parity + ARC + flat receivers and the flipped frontier test.
- [ ] `go test ./harness/ -run 'TestListings' -count=1 -timeout 40m` -- the ch39 partial listing runs on WASM in the cross-backend conformance, byte-identical 9/9. (Skip the unrelated long SHA/TLS suites; this change is WASM-codegen only.)
- [ ] `go build ./...` clean.

## Self-review notes (coverage map)

- Spec "lower partial def + CBounce" -> Task 1 (runtime) + Task 2 (emitPartialWasm + emitIn CBounce).
- Spec "ARC-correct, steady-flat" -> Task 2 Steps 7-10 (drop/dup/shared/unforced receivers, all assert flat + correct output).
- Spec "the dangerous line (shell-free vs full-free never double-free)" -> Task 1 Step 3 (full-free branch) + Task 2 Step 9 (unforced-release receiver).
- Spec "re-open PerceusBalanceable" -> Task 3.
- Spec "docs (R-PERCEUS closed-section + CLAUDE)" -> Task 4.
- Spec constraints (no em-dashes, byte-identical, kernel frozen, C1 output-correctness) -> Global Constraints + every behavioral receiver asserts output.
