# WASM Partial Support (CBounce lowering + ARC trampoline) -- Design

**Date:** 2026-06-28
**Track:** B (portable codegen, telos 2) / R-PERCEUS (WASM ARC), the last excluded class.
**Status:** design approved, ready for an implementation plan.

## Goal

Bring the WASM backend (the 9th backend) to `partial`/general-recursion parity with
the other 8, and retire the LAST exclusion in the WASM Perceus flat fragment. Today a
`partial` program lowered through closure conversion produces a `CBounce` node that
WASM's `emitIn` has no case for, so the program does not lower at all (it panics
`codegen(wasm): unknown CIr node codegen.CBounce`). After this work: a `partial` lowers,
runs ARC-correct (steady-flat, no leak / no double-free), and joins the asserted
`PerceusBalanceable` flat fragment.

Scope is FULL closure (user-approved): lower + ARC-correct + drop the `CBounce`
exclusion from `PerceusBalanceable`.

## Context (verified facts the implementer relies on, do NOT re-derive)

- `CBounce{Call CIr}` (`codegen/closure.go:139`) is the closure-converted `IBounce` (T2
  native trampoline): a SATURATED tail call to a partial-group member, deferred. `Call`
  is the converted `AppClosure` spine whose head is a `CGlobal` naming the group member.
- `ClosureProgram.Partials map[string]bool` (`closure.go:201`) flags which def names are
  `partial`; `CDefSpec.Arity` (`closure.go:207`) is the curried leading-lambda count the
  driver collects.
- The C reference (`codegen/c.go`): `emitPartialC` (c.go:521) splits a partial into a
  memoized `_step` thunk (the body, where `CBounce` is a `K_BOUNCE`), `arity` curried
  driver code blocks, and a public thunk; the last driver saturates `_step` and `tramp`s.
  `bounceExpr` (c.go:488) walks the spine to the head `CGlobal` and builds
  `mkbounce(<head>_step(), nargs)` + `bounce_set` per arg; a non-recognizable spine falls
  back to emitting the call directly.
- **The native backends (C/LLVM) use mark-sweep GC, not ARC.** Their trampoline lets the
  GC collect spent bounce objects -- there is NO ownership management across the bounce
  chain. WASM is the ONLY ARC (Perceus) backend, so the ARC trampoline below is NOVEL
  work with no reference implementation. This is the C1/C-1 memory-safety risk class.
- WASM runtime (`codegen/wasm_runtime.go`) is a kind-tagged bump heap (`K_CLO=0`,
  `K_CON=1`, `K_PAIR=2`, `K_UNIT=5`, `K_BIG=6`) with an 8-byte `[size][rc]` header,
  `$alloc`, `$rt_release`, `$rt_dup`, `$rt_mkclo`, `$rt_apply`. The 6a "runtime frozen"
  constraint was scoped to the accel-operand plan; THIS plan legitimately extends the
  runtime (a new kind + the tramp loop).
- WASM emitter (`codegen/wasm.go`): `emitDefs` (wasm.go:208) routes every def through
  `emitDefThunk`; `emitCurryBlock` (wasm.go:534) + `emitCachedThunk` (wasm.go:357) are
  the reusable currying/thunk machinery; `emitIn` (wasm.go:558) is the `CIr` dispatch
  whose `default` panics. `perceus.go:91` already shifts through `CBounce` but assigns it
  no ownership; `annotateBareSpine` routes recognized spine operands through
  `consumeOwning` (the accel-operand template).

## Architecture (Approach A: heap bounce + runtime `$rt_tramp` loop)

Approach B (a WASM structured-control-flow `(loop)` with zero per-iteration heap) is
PARKED as a future `CBounce`-to-loop peephole: it only covers DIRECT self-tail-calls and
bypasses the shared `CBounce` IR node, breaking 9-backend uniformity. Land correctness
via the shared node first.

Four code sites:

### Runtime (`codegen/wasm_runtime.go`)
New `K_BOUNCE` kind (next free kind word, `7`): layout
`[kind=7][step_code_idx][nargs][arg0..argN-1]`. Three functions:
- `$rt_mkbounce(step_code, nargs) -> ptr` -- allocate + set kind/step/nargs (parallels
  `$rt_mkclo`); arg slots filled by the emitter.
- `$rt_tramp(v) -> v` -- the driver loop. While `v` is `K_BOUNCE`: read `step_code` +
  args, call the step with the args TRANSFERRED in, `$rt_bounce_free_shell` the spent
  bounce, set `v = next`. Return the first non-bounce value.
- `$rt_bounce_free_shell(ptr)` -- free the object's memory WITHOUT recursing into arg
  slots (they were moved into the step call).
- `$rt_release` gains a `K_BOUNCE` case = the DUAL: release each live arg slot, then free
  the header (for a bounce built but never forced -- the totality path).

### Emitter def-side (`codegen/wasm.go`)
`emitPartialWasm(name, arity, body)`, routed from `emitDefs` when `cp.Partials[name]`.
Emits: a cached `_step` thunk (the body; `CBounce` inside lowers to a bounce), `arity`
curried driver blocks via the existing `emitCurryBlock`, and a public thunk whose final
driver saturates `_step` and wraps the result in `$rt_tramp`. Mirror `emitPartialC`'s
arity=0 special case (no args to collect -> public entry drives the bounce-free step).

### Emitter expr-side (`codegen/wasm.go`)
New `case CBounce:` in `emitIn`, mirroring `bounceExpr`: walk the `AppClosure` spine to
the head `CGlobal`, emit `$rt_mkbounce(<head>_step, nargs)` + store each evaluated arg
into its slot. A non-`CGlobal` head falls back to a direct emit of the call (the same
guard C uses).

### Predicate (`codegen/perceus.go`)
Drop condition 1 (the `CBounce`/partial exclusion) from `PerceusBalanceable`'s
`cirUnbalanceable` scan, ONCE ARC correctness lands. The remaining unbalanceable
constructs (over-applied `NatElim`, etc.) stay caught per-spine.

## ARC ownership model (the crux)

The invariant chain that makes the trampoline leak-free and double-free-free:

1. **Bounce owns its args.** `CBounce` spine args reach `$rt_mkbounce` as PRIVATE OWNED
   refs -- route each through `consumeOwning` in `annotateBareSpine` (the accel-operand
   template). A `K_BOUNCE` holds `nargs` owned references, one per slot. Multi-use /
   shared-local args get their dups from the enclosing scope exactly as today; the bounce
   slot is one consume.
2. **`_step` consumes its args.** The step body is ordinary Perceus-annotated code: each
   parameter owned on entry, dup'd if used twice, dropped if unused. When `$rt_tramp`
   calls the step with the bounce's arg values, ownership TRANSFERS from the bounce slots
   into the step frame; the step's normal ARC discipline balances them.
3. **`$rt_tramp` shell-frees, never double-frees.** After the step returns the next
   value, the spent bounce's arg slots are ALREADY moved out, so `$rt_tramp` calls
   `$rt_bounce_free_shell` (header only), NOT `$rt_release` (which would recurse and
   double-free the moved args). This is the single most dangerous line; dedicated
   adversarial receiver.
4. **Result is owned.** Each step returns an owned value (another owned bounce, or the
   final owned result); the loop carries that ownership forward; the final non-bounce is
   returned owned to the caller. Net per iteration: one bounce allocated, one shell freed,
   args conserved -> STEADY-FLAT, zero growth.
5. **Dead-without-forcing path.** A bounce built but never trampolined must still be
   total: `$rt_release` on `K_BOUNCE` releases the live arg slots then frees the header.
   `$rt_tramp` uses shell-free (args moved); `$rt_release` uses full-free (args live); the
   two NEVER overlap on the same object.

Per C1 discipline: every receiver asserts OUTPUT CORRECTNESS alongside the flat/leak
property, so a use-after-free / double-free surfaces as wrong output, not just a refcount
delta.

## Testing

New receivers in `codegen/perceus_test.go` + the WASM conformance path. Every receiver
asserts steady-flat AND correct output.

**Lowering / parity:**
- `TestPerceusPartialCountdownRunsWasm` -- a ch39-style `partial` countdown lowers + runs
  on WASM to `zero` (today panics on `CBounce`). The headline gate.
- WASM joins the existing ch39 cross-backend conformance -- byte-identical 9/9.

**ARC ownership shapes (C1 receivers):**
- step DROPS an unused arg (owned, never used -> must free, else leak).
- step DUPS an arg (used twice -> dup balances the bounce-owned ref).
- MULTI-ARG bounce (>=2 distinct owned slots -> both transfer + conserve).
- SHARED-owned-local arg (`f n n` in the tail call -> one shared dup, both slots free).
- DEEP iteration (n ~3000) asserting steady-flat: heap after warmup == heap at end.

**The dangerous-line receiver:**
- bounce BUILT-BUT-NOT-FORCED (`$rt_release` full-free) vs FORCED (`$rt_tramp`
  shell-free), pinning the two free routes never double-free. If hard to construct
  organically, a direct `emit`/`cirUnbalanceable` unit test on a hand-built `CBounce`.

**Predicate flip:**
- `TestPerceusFrontierBoundary` currently asserts the partial program is EXCLUDED; after
  the exclusion drops it flips to asserting BALANCEABLE + FLAT (the move the accel
  listings made). Grep for any other test asserting CBounce/partial is skipped; update in
  the predicate task, not before.

**Gate:** `go test ./codegen/` + the WASM conformance suite (`wasmtime`); ch39
byte-identical 9/9.

## Staging (for the plan)

1. **Lower + ARC** -- runtime `K_BOUNCE`/`$rt_mkbounce`/`$rt_tramp`/`$rt_bounce_free_shell`
   + `$rt_release` case; `emitPartialWasm` + `emitIn` `CBounce` case; `annotateBareSpine`
   ownership for bounce operands. Receivers: parity + all ARC shapes + the dangerous-line.
   Inseparable -- a leaking lowering is not soundly shippable on the ARC pipeline.
2. **Re-open the predicate** -- drop condition 1 from `cirUnbalanceable`; flip
   `TestPerceusFrontierBoundary` to balanceable+flat; update any skip-assertions.
3. **Docs** -- R-PERCEUS "ONE REMAINING excluded class" section: CBounce CLOSED; the
   corpus flat table; CLAUDE.md phase-map note. (No em-dashes / en-dashes in prose.)

## Constraints

- Kernel FROZEN: no core/store change, no hash change (0x06). WASM codegen + runtime only.
- Output BYTE-IDENTICAL across all backends: ARC adjusts refcounts only, never the
  returned result expression.
- NO em-dashes / en-dashes anywhere (code, comments, commits, docs); use `--`.
- Conventional Commits; trailer `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- C1/C-1 memory-safety class: every receiver asserts output correctness, not just a
  refcount delta (a double-free here previously survived per-task review and was caught
  only by the final whole-branch review).
