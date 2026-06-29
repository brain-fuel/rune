# R-PERCEUS: Ownership insertion for the ARC runtime

## What this is
The Plan 6a ARC runtime (R-ARC) gives WASM heap values a refcount and the
`$rt_retain` / `$rt_release` / recursive-`$rt_free` primitives, but nothing calls
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
  CONSUMED by the application. The ARGUMENT is consumed by the callee: the code
  block owns its parameter `CVar{0}` and drops-or-transfers it, so the caller's
  arg occurrence transfers ownership into the call and the caller does NOT drop
  it. The CLOSURE is consumed by the application itself: the code block only
  borrows the env during the call, so the closure object (and its env) must be
  RELEASED after `$rt_apply` returns. `$rt_apply` is NOT modified to do this (that
  would free closures at refcount 1 in the no-Perceus runtime and break the
  existing WASM gate); instead the PASS releases the closure -- a `CDrop` of the
  closure value after the call (bind the closure to a local, apply, drop it, then
  yield the result). A closure applied N times therefore has N consuming
  occurrences: dup it N-1 times (the owned-variable rule), and each application
  releases one reference, so it frees exactly once. A variable free in BOTH Clo
  and Arg is also dup'd before the pair. (Implementation freedom, gate-arbitrated:
  the post-call closure release MAY instead be emitted in the WASM `emitCase`-style
  lowering of `AppClosure`, WASM-only; the pass-level `CDrop` is preferred for
  portability to the C/LLVM ARC port. Either way `$rt_apply` stays unchanged.)
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

## Worked example: dup and drop in the pure fragment

Receiver program (`TestPerceusCoreDupDrop`):

```rune
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ

applyTwice : ((Nat -> Nat) -> (Nat -> Nat)) -> (Nat -> Nat) is
  fn (f : (Nat -> Nat) -> (Nat -> Nat)) is
    let unused : Nat -> Nat = fn (y : Nat) is y end in
    f (f (fn (z : Nat) is z end))
  end
end
idFun : (Nat -> Nat) -> (Nat -> Nat) is fn (g : Nat -> Nat) is g end end
main : Nat -> Nat is applyTwice idFun end
```

The receiver forces three distinct ownership events:

**1. CDup on f** (`fn (f : ...) is f (f (...)) end`). The body is
`AppClosure(f, AppClosure(f, Mk_z))` where f = CVar{0}. Both the outer Clo
position and the inner Clo/Arg search see f, so the algorithm inserts
`CDup{f, <annotated-AppClosure>}`. f.rc goes 1->2 before the two applications.

**2. CLet+CDrop pair for each AppClosure(f, ...)** (scope limitation: Clo is a
CVar so the closure is released after `rt_apply`). Each application wraps the
call in:
```
CLet("$clo", f,
  CLet("$res", AppClosure(CVar{0}, arg),
    CDrop{CVar{1}, CVar{0]}))
```
The outer application's CDrop brings f.rc from 2 to 1; the inner application's
CDrop brings it from 1 to 0, freeing the K_CLO_f heap object.

**3. CDrop on unused** (dead CLet binder). Inside the CLet("unused", MkClosure,
body) binder, `ownScope` sees unused (CVar{0}) is dead in body and wraps:
`CDrop{CVar{0}, body}`. This frees the K_CLO_unused immediately after binding.

**ARC trace (one hot run; argCount=1 so code-block args are not dead-dropped):**

| Event | Object | rc delta | rc after |
|-------|--------|----------|----------|
| rt_mkclo idFun arg | K_CLO_g | alloc | 1 |
| CDup f in applyTwice block | K_CLO_g | +1 | 2 |
| rt_mkclo unused | K_CLO_unused | alloc | 1 |
| CDrop unused | K_CLO_unused | -1, freed | 0 |
| rt_mkclo fn(z)isz | K_CLO_z | alloc | 1 |
| inner AppClosure: CDrop $clo_i | K_CLO_g | -1 | 1 |
| outer AppClosure: CDrop $clo_o | K_CLO_g | -1, freed | 0 |
| harness rt_release result | K_CLO_z | -1, freed | 0 |

Net per run after warm-up: 0. $rt_live is flat after run 1. Output: `<function>`.

**SUPERSEDED by PATH B (Task 3.5).** Task 3 used an `argCount` parameter to
`ownScope` that SKIPPED dead-dropping the code-block argument entirely. That was a
sound-but-non-generalizable workaround (Task 4's owned-scrutinee receivers must DROP
owned args, which the skip suppresses). It is removed: code-block arguments are now
owned and dropped when dead, the caller dups borrowed values before passing them, and
the one remaining borrow case (a curry-through step argument the frozen fold/curry
machinery reuses) is handled by the carve-out documented under "PATH B:
borrowed-vs-owned and dup-on-consume" below. The trace above still holds (no code-block
arg in `applyTwice` is dead, so none is dropped); read the per-event ownership through
the PATH B rules.

## Held for Plan 6b-2 (named, not silently dropped)

NOTE (current): ALL FIVE residuals below are now CLOSED. Residuals 2-5 closed under
Plan 6b-2; residual 1 (CBounce) closed under WASM-partial-support (the partial
trampoline lowers + is ARC-correct -- see "CBounce / partials: CLOSED" near the end of
this document). The snapshot below is the historical post-Task-6 state; no per-spine
exclusion remains except an over-applied NatElim spine.

Five leak-only residuals remain after Task 6. All are characterized: no UAF, no
double-free, no wrong output. Only $rt_live grows per run. Each has a named future
receiver that will become the 6b-2 steady gate when the boundary is closed.

1. **CBounce / the partial trampoline** -- ownership across a DEFERRED saturated
   tail call and the public driver loop. The driver reuses args across bounces in a
   way the current drop rules do not model. Future receiver: the ch39 countdown /
   a partial-recursive listing run leak-free on WASM.

2. **Inline builtin-nat-fold bignum temporaries** -- the frozen `emitNatFold`
   borrow-passes its loop counter via non-retaining `rt_apply` and creates bignum
   temporaries (base-1e9 limbs via `rt_big_parse`). Ownership of the loop-internal
   temporaries is not modeled. Future receiver: a bignum-arithmetic listing
   (e.g. `mulN 1000 1000`) run leak-free.

3. **Dead-motive carve-out (+1/run inline eliminator)** -- when a FULLY-SATURATED
   eliminator application is inlined (all motive + cases + scrutinee at one site),
   the dup'd motive arg that the curry-through block leaves borrowed is not released.
   Future receiver: `perceusInlineElimSrc` steady-flat (currently output-invariance-
   only).

4. **K_CLO_mk1 container (+1/run per inline multi-arg constructor call-site)** --
   `emitCtorBlock` keeps the first intermediate K_CLO BARE (releasing it mid-spine
   would UAF a field the K_CON just moved in). Future receiver: `perceusCtorMkXXSrc`
   steady-flat (currently output-invariance-only, not measured in Task 5).

5. **rt_big_parse literal temporaries** -- bignum literal values (`CLit{Kind:LitNat}`)
   create `rt_big_parse` K_BIG nodes per run even when the value is subsequently
   used only for its mathematical content (e.g. as a pair component). Future receiver:
   `perceusWasmBignumPairSrc` steady-flat (currently output-invariance-only in Task 6).

These residuals are all closed. `TestPerceusPartialInFlatFragment` (the former
`TestPerceusFrontierBoundary`) now asserts the ch39 countdown is INSIDE the fragment and
steady-flat; `PerceusBalanceable` admits partial programs.

## PATH B: borrowed-vs-owned and dup-on-consume

Task 3.5 replaces Task 3's `argCount` dead-drop SKIP (a sound-but-non-generalizable
workaround) with the principled PATH B convention. The 6a runtime stays FROZEN: store
is MOVE (rt_mkcon / rt_clo_set / rt_mkpair do NOT retain; rt_apply does NOT retain its
argument; rt_con_get / rt_pair_fst / rt_pair_snd return ALIASES). The pass adapts to
that runtime; it does not change it.

### Owned vs borrowed (the refined rule)
- OWNED: a freshly-allocated value (alloc gives rc=1) or a value received as an owned
  argument. Consuming an owned value MOVES it (no dup). A value consumed N times needs
  N-1 dups.
- BORROWED: a `CGlobal` (a thunk-cached root the cache owns for the life of the
  program), a `CEnv` capture (the closure heap object owns the env record), and a
  `CField` / `CFst` / `CSnd` projection result (an alias into a parent the parent owns).
  Reading a borrowed value does not transfer ownership.

### Dup-on-consume-borrowed
When a BORROWED value is CONSUMED in an OWNING position (passed as a function argument,
stored into a constructor / pair / closure-env slot, or returned as the scope result),
the pass inserts a `CDup` of it FIRST so the owner keeps its reference and the consumer
takes a fresh one. The helper `consumeOwning(t)` realizes this: an already-owned `t`
(an owned-local `CVar`, or a fresh `MkClosure` / `AppClosure` / `CPair`) passes through;
a bare `CEnv` becomes `CDup{V: CEnv, K: CEnv}`; a `CGlobal` / `CField` / `CFst` / `CSnd`
(not a bare variable, and `CDup.V` must name a `CVar` / `CEnv`) is let-bound first then
the binding is dup'd (`CLet $own = t in CDup{CVar 0, CVar 0}`). It reaches the tail
value through `CLet` / `CDup` / `CDrop` continuations and is idempotent. Without this, a
cached root flowing into a consuming position would be dropped under the cache:
use-after-free (the exact crash Task 3's skip dodged).

### Args are owned by the callee
A code block OWNS `CVar{0}` and DROPS it when dead, uniformly with dead `CLet` locals
(the `argCount` skip is removed). Because the caller dups any borrowed value before
passing it, a cached root arrives at the callee as a FRESH owned reference, so the
callee dropping it leaves the cache's reference intact. The no-Perceus output is
unchanged (refcounts only inflate, are never freed; the printed value is identical).

### The curry-through borrow carve-out (REMOVED by Plan 6b-2 Task 2)

HISTORICAL NOTE: The carve-out was present from the original Perceus implementation
through Task 1. It marked a code block whose entire body is a single `MkClosure`
(a currying step) as BORROWED for its argument -- the `isCurryThrough` check in
`Perceus()` routed such blocks through `consumeOwning(annotate(...))` rather than
`ownScope(...)`. The motivation was the FROZEN 6a fold/curry machinery (`emitNatFold`,
`emitCurryBlock`) which borrow-passed the loop counter to step blocks without retaining
it first. Dropping a curry-through argument in that context would free a value the
frozen caller still reused -- producing the `mulN 100 100 = "101"` use-after-free.

WHY IT WAS UNSAFE: `emitNatFold` passed `$k` to the step block via `rt_apply` WITHOUT
retaining it, then incremented `$k` afterward. A dead-drop in the step block would
free `$k` before the increment.

WHY TASK 1 MADE IT SAFE: Plan 6b-2 Task 1 added `(call $rt_retain (local.get $k))`
in `emitNatFold` BEFORE the `rt_apply` step call. The step block now receives an
OWNED reference to `$k`. A dead-drop of a curry-through argument is safe: the step
block's owned copy is dropped independently of the fold's retained loop counter.

WHY THE CARVE-OUT WAS REMOVED: With the carve-out in place, the dead motive argument
of an INLINE general eliminator application leaked +1/run. When OptElim (or any
general eliminator) is applied with an inline lambda motive, closure conversion
produces a first curry block (b0) whose body is `MkClosure{Code: b1, Env: []}`. The
motive argument (CVar{0}) does NOT appear in b0's env (it is DEAD). With the
carve-out, b0 is a bare-MkClosure block so its dead argument is left borrowed. The
caller's `consumeOwning` dups the motive K_CLO before the call; b0 borrows the arg
(does not drop). The dup'd K_CLO is never freed, leaking +1/run ($rt_live grows).

WHAT REPLACED IT: The carve-out branch and the `isCurryThrough` helper were DELETED.
The `Perceus()` loop now applies `ownScope(body, []bool{true})` to EVERY code block
uniformly. `ownScope` dead-drops CVar{0} iff `!cirUsesArg(body, 0)` -- which is
true for b0 (empty env). The dead motive K_CLO is freed each run. $rt_live is flat.
Receiver: `TestPerceusDeadMotiveDropped` (Plan 6b-2 Task 2 gate).

NOTE on `shiftCIr`: it is now the structural dual of `cirUsesArg`, recursing through
EVERY node (`CCase` scrut + arms, `CPair`, `CField`, `CFst`, `CSnd`, `CBounce`), so
feeding a constructor or case as a closure-application argument keeps de Bruijn indices
correct. A `MkClosure`'s code-block body is a separate closed scope and is never
shifted; only its `Env` terms, evaluated in the enclosing frame, shift.

### Worked receivers (Task 3.5)
- `TestPerceusDeadFreshArg` (`mainFresh = dropArg (fn w is w end)`): `dropArg`'s body
  is a `let r = ... in r` (a real consumer, not a curry-through), so its dead
  freshly-allocated closure argument is DROPPED each run. Steady-state goes flat
  `[2 2 2 2]`; under the old `argCount` skip it LEAKED `[3 4 5 6]` (+1/run). Output
  `<function>`. Proves the skip removal.
- `TestPerceusCachedRootArg` (`mainRoot = dropArg root`): `root` is a `CGlobal`
  thunk-cached root passed as an argument. dup-on-consume-borrowed dups it before the
  call, so the callee drops a FRESH reference and the cache survives. Steady-state flat
  `[2 1 1 1]` (run 1 warms the cache) with correct output `<function>`; without the dup,
  removing the skip would cascade-free the cached root and corrupt the cache. Proves
  dup-on-consume-borrowed.
- `TestPerceusCaptureUseLater` (`mainCap = capUse (fn m is m end)`): an owned local `g`
  is captured into a closure env AND applied again later, so it is consumed twice and
  needs one cross-`CLet` dup (Task 3's `MkClosure`-only escape-dup missed this); the
  returned capture (`cap`'s body is a `CEnv`) is dup'd at the return position.
  Steady-state flat `[2 2 2 2]`, output `<function>`.
- `TestPerceusCoreDupDrop` (Task 3's receiver) stays GREEN: a closure used twice (one
  real dup) and a dead let-binding (one real drop) balance flat, output `<function>`.

## Worked example: borrowed scrutinee, owned fields

Task 4 wires `CCase` and `CField` into the pass. A datatype's eliminator is lowered to
ordinary IR (`LowerElim`, codegen/lower.go): a curried lambda over the motive, the
constructor cases, and the scrutinee, whose body is a `CCase` whose arms project the
scrutinee's fields with `CField` and (for recursive arguments) call the eliminator
recursively. After closure conversion the eliminator's FINAL lambda-x block is a `CCase`
whose `Scrut` is the block's own argument `CVar{0}` and whose arms reference that same
`CVar{0}` through `CField` (arms share the enclosing context, no binder, no index shift).
The scrutinee arrives OWNED: a saturated eliminator call passes it through an AppClosure
argument, where `consumeOwning` already dup'd it if it was borrowed.

### The two node rules
- `CField{Scrut, i}` reads field `i` with `rt_con_get`, an ALIAS into the scrutinee (a
  borrowed read; it transfers nothing). The pass annotates the scrutinee and otherwise
  carries the projection through. A projected value that ESCAPES into an owning position
  (the kept field a `some x` arm returns, or the field a recursive-IH arm feeds to the
  recursive call) is dup'd THERE by `consumeOwning`'s `CField` arm, not at the read.
- `CCase{Scrut, Arms}` borrows the scrutinee for the tag read (`rt_con_tag` does not
  consume) and for the arms' field reads, then must DROP the owned scrutinee on EVERY arm
  path AFTER those reads. `annotateCase` handles the common shape an eliminator produces,
  an OWNED-local scrutinee `CVar{k}`:
  - Each arm is annotated with index `k` marked NOT-owned, so the multi-use dup logic and
    the per-arm dead-drop never touch the scrutinee (its field reads are borrows, not
    consumes; it is consumed exactly once, by the drop below). A multi-field arm that
    reads `CVar{k}` through several `CField`s thus does not spuriously dup the scrutinee.
  - Each arm RESULT is run through `consumeOwning`, so a borrowed return (a returned case
    capture `CEnv`, or the kept field) is dup'd. The kept field thus carries its own
    reference PAST the scrutinee's free, so dropping the scrutinee (which recursively
    releases its children) does not dangle the returned value.
  - PER-ARM LIVENESS: an owned local live across the case but dead in THIS arm is dropped
    at the arm front (a local dead in EVERY arm is dropped once before the case by
    `ownScope`). The scrutinee is excluded -- it has the drop-after.
  - DROP-AFTER: the scrutinee is dropped after the arm body computes, as `let $scrut =
    <arm> in drop scrutinee; $scrut`. Binding the result shifts the scrutinee index by 1
    in the `CDrop` continuation; the result (`CVar{0}`) does not mention the scrutinee, so
    the `CDrop` is clean.

  A scrutinee that is NOT an owned local (a borrowed `CGlobal`/`CEnv`, or -- never emitted
  by a lowered eliminator -- a non-variable term) is borrowed by, not owned by, this
  scope: there is nothing to drop, and `consumeOwning` stops at a `CCase` (it owns inside
  via the per-arm handling), so an enclosing consuming position sees the `CCase` as
  already owned.

### Receiver `TestPerceusConstructorCase`
```rune
data OptF : U is noneF : OptF | someF : (Nat -> Nat) -> OptF end
optFMot : OptF -> U is fn (o : OptF) is Nat -> Nat end end
matchF : OptF -> (Nat -> Nat) is
  OptFElim optFMot (fn (z : Nat) is z end) (fn (x : Nat -> Nat) is x end)
end
mainOptF : Nat -> Nat is matchF (someF (fn (w : Nat) is w end)) end
```
The lowered `OptFElim`'s final block is
`Case(CVar{0}){ tag0 -> CEnv{c0} ; tag1 -> App(CEnv{c1}, Field(CVar{0}, 0)) }`. The
`someF` arm KEEPS the projected field (the payload closure). The pass produces, for that
arm, `let $scrut = App(CEnv{c1}, (let $own = Field(CVar{0},0) in dup $own; $own)) in drop
CVar{1}; $scrut`: the field is dup'd before it is handed to the identity case `c1`, then
the scrutinee `someF ...` is released (freeing the K_CON and decrementing the payload back
to the single dup'd reference), and the payload is returned and freed by the harness.

The eliminator is PARTIAL-APPLIED at top level (`matchF`), so the saturated curry spine --
which allocates one intermediate K_CLO per applied argument -- is a CACHED thunk built
ONCE, not per run. The pass cannot release curry-spine intermediates in-band: the WASM
emitter recognizes the frozen nat-fold / accel spine by pattern-matching the raw
`AppClosure` chain (`NatElimSpine` / `accelMatchC`), and wrapping an `AppClosure`-headed
`Clo` in a `CDrop` would break that recognition (the emitter is out of edit scope).
Caching the spine sidesteps it; per run only the scrutinee allocates. The motive
`optFMot` is likewise a cached def: the curry-through carve-out leaves the eliminator's
ignored motive argument borrowed, and a cached root's refcount inflates while `$live`
does not.

Gate: OUTPUT `<function>` (the kept closure) AND `$live` flat. Steady-state goes from
`[10 16 22 28 34]` (+6 per run under the pre-Task-4 carry-through: the K_CON scrutinee,
its payload, and the per-run curry intermediates all leaked) to `[4 4 4 4 4]` (flat).

The payload is a CLOSURE, not a bignum literal, for the same reason the Task 3 / 3.5
receivers avoid bignums: `rt_big_parse`'s intermediate K_BIG temporaries are a deferred
6b-2 leak that would mask the balance. The kept-field-is-a-bignum path (a RECURSIVELY
freed field) is exercised for OUTPUT-invariance by the companion `some 7` program
(`OptElim optMot zero (fn x is x end) (some 7)` -> `7`): if the `some x` arm did not dup
the field, dropping the scrutinee `some 7` would free the K_BIG(7) before it is returned,
a wrong value or a trap. Output `7` confirms the dup-on-escape holds for a recursively
freed field too -- this is the first runtime proof of `consumeOwning`'s `CField` arm
(the Task 3.5 Minor).

## Worked example: pairs

### Rules (Task 5)

**CPair{A, B}** -- both components are OWNING positions (`rt_mkpair` stores them with
store=MOVE, no retain). Each component passes through `consumeOwning` before the pair is
built: a borrowed value (`CEnv`, `CGlobal`, a projection `CFst`/`CSnd`/`CField`) is
dup'd so the pair becomes the sole owner; an owned local (`CVar`) moves in unchanged.
If the same owned local appears in BOTH A and B (e.g. `Pair v v`), a CDup is inserted
once before the CPair.

When the Val of a CLet is CPair and its components are owned CVar locals, those locals
are marked not-owned in the body scope (bodyOwned[k+1] = false), preventing ownScope
from dead-dropping them: `rt_free` on the pair will release each component when the pair
itself is dropped.

**CFst{P} / CSnd{P}** -- `rt_pair_fst` / `rt_pair_snd` return ALIASES (raw pointers, no
retain). The annotation simply recurses into P. The result is treated as BORROWED at its
use site:

- When CFst/CSnd escapes to an OWNING position (AppClosure arg, MkClosure env slot,
  return via `consumeOwning` at a scope exit), `consumeOwning`'s `CFst`/`CSnd` arm
  let-binds the projection and inserts a CDup (already present from Task 3.5). This
  gives the kept projection its own reference so it survives the pair's recursive free.

- When CFst/CSnd is the Val of a CLet (`let a = Fst p`), the CLet case detects the
  pattern via `pairProjSrc` and applies `consumeOwning` to the projected value (CDup),
  binding `a` with an owned reference. Then if the pair `p` is DEAD in the body
  (cirUsesArg false for p in x.Body), a DROP-AFTER is inserted:

  ```
  let $pres = annotated_body in CDrop{p_shifted, $pres}
  ```

  The sequence at runtime: project Fst (alias), retain the component (CDup), evaluate
  body, bind result as $pres, release the pair. Releasing the pair calls `rt_free`
  which recursively releases both components: the Fst component goes from rc=2 to
  rc=1 (still alive, $pres holds it), the Snd component goes to rc=0 (freed). $pres
  (rc=1) is returned and eventually freed by the caller.

The DROP-AFTER ordering guarantees: the kept projection is always dup'd BEFORE the pair
is released. Any borrowing of the same pair in an intermediate let body (e.g. a second
`let b = Snd p` before p is dead) pushes the drop-after to the innermost let where p
first becomes dead, naturally chaining drops in the correct order.

The guard in the shared-var dup loop skips pair projections (`srcIdx >= 0 && i ==
srcIdx`): a projection is a BORROW, not a MOVE, so a dup of the pair source would be
wrong (the pair is not consumed by the projection).

### Receiver `TestPerceusWasmPairs`

```rune
mainPairs : Nat -> Nat is
  let f : Nat -> Nat = fn (x : Nat) is x end in
  let g : Nat -> Nat = fn (y : Nat) is y end in
  let p : Sig (Nat -> Nat) (fn (z : Nat -> Nat) is Nat -> Nat end)
        = Pair (Nat -> Nat) (fn (z : Nat -> Nat) is Nat -> Nat end) f g in
  let a : Nat -> Nat = Fst p in
  a
end
```

Per-run allocation/free ledger (with Perceus):

| step | event | f.rc | g.rc | p.rc | $live |
|------|-------|------|------|------|-------|
| alloc f | rt_mkclo | 1 | - | - | +1 |
| alloc g | rt_mkclo | 1 | 1 | - | +1 |
| alloc p | rt_mkpair(f, g) | 1 | 1 | 1 | +1 |
| Fst p + CDup | rt_pair_fst; rt_retain(f) | 2 | 1 | 1 | 0 |
| CDrop{p} (drop-after) | rt_free(p): rt_release(f), rt_release(g) | 1 | 0 | 0 | -2 |
| harness rt_release(result=f) | rt_free(f) | 0 | 0 | 0 | -1 |

Net per run: +3 allocations, -3 frees = 0. $live flat from run 2 onward.

Gate: OUTPUT `<function>` (the identity `Nat -> Nat` closure f) AND $live flat. Under
the pre-Task-5 carry-through, ownScope dead-drops f and g BEFORE `rt_pair_fst` (because
they are dead in `let a = Fst p in a`), causing use-after-free and the wrong output
`()`. After Task 5, the CPair bodyOwned update suppresses those dead-drops and the
drop-after releases the pair in the correct order.

Closures are used as pair components (not bignums) for the same reason as earlier
receivers: `rt_big_parse` leaks K_BIG temporaries that would mask the balance signal.

## Curry-intermediate release (the recognize-then-skip rule)

Task 3 releases an applied closure only when it is CVar- or MkClosure-headed (an owned
local or a freshly allocated inline lambda). An AppClosure-HEADED closure -- a CURRY
INTERMEDIATE, the `(f a)` in `(f a) b` -- was left bare, so it LEAKED every run.
Inline saturated multi-argument application is pervasive, so the steady-state corpus
gate cannot reach flat without releasing these. Phase 6-pre releases them, WITHOUT
breaking the WASM emitter's spine matchers.

The emitter pattern-matches the RAW (un-annotated) AppClosure backbone in two places:
accelMatchC (codegen/c.go) recognizes a 2-argument accel-op application and lowers it
DIRECTLY to rt_nat_add/mul/monus(a, b) -- the intermediate closure `(add a)` is NEVER
BUILT -- and NatElimSpine (codegen/closure.go) recognizes the builtin-nat eliminator
spine the frozen emitNatFold drives (it borrow-passes its loop counter via non-retaining
rt_apply and reuses it). Inserting a CLet/CDup/CDrop wrapper mid-backbone makes the
`t.(AppClosure)` walk fail, so the matcher no longer fires (mulN 100 100 mis-renders to
`<function>`), and the emitter is out of edit scope for the runtime semantics.

The rule is RECOGNIZE-THEN-SKIP. Before releasing an AppClosure-headed Clo, the pass
asks isRecognizedSpine on the ORIGINAL un-annotated AppClosure:

  - accelMatchC matches (a saturated accel op), OR
  - NatElimSpine matches (the builtin-nat eliminator, >= 4 args), OR
  - the spine's BASE head (spineBaseGlobal) is a datatype CONSTRUCTOR (bareSpineHead).

NOTE (6-pre-fix FIX A): general DATATYPE ELIMINATORS are NOT in the recognized set.
The original 6-pre implementation added d.ElimName to bareSpineHead on a false analogy
to constructors. This was wrong: a general eliminator's curry blocks consumeOwning their
CEnv prefix captures (the motive/branch closures are dup'd before being stored), so
releasing an eliminator intermediate is BALANCED, not a UAF. Leaving eliminators in the
skip-set caused every inline pattern-match to leak its N-1 curry intermediates per run
(hollow 6-gate). FIX A removes them; the builtin-nat eliminator remains handled by the
separate NatElimSpine path (which correctly borrows the loop counter).

The constructor clause is necessary because a constructor's hand-emitted emitCtorBlock
WAT moves each accumulated field into the K_CON by store=MOVE: releasing an intermediate
before the K_CON is built would free a field the K_CON now owns (use-after-free). These
spines are recognized at ANY arity (a partially applied constructor used as a value must
also keep its intermediates).

If recognized, annotateBareSpine annotates only the LEAF args (consumeOwning each) and
keeps the entire AppClosure backbone BARE -- no CLet/CDup/CDrop anywhere on the Clo chain
-- so the matcher still fires and the emitter shortcuts the spine. Constructors also apply
the FIX B shared-owned-local dup (see below). This is behaviour-preserving for every
accel / nat / constructor program (conformance byte-identical across all 8 backends; the
C backend, which never runs Perceus, is unaffected -- it sees no ownership nodes).

If NOT recognized, the Clo is an ORDINARY curry intermediate (an ordinary user function,
inline-lambda spine, or -- after FIX A -- a general eliminator curry step). Its final
block dup-on-escapes its result (consumeOwning at the return position), so its captures
carry an independent reference. The pass extends the Task 3 closure-release to this case:
bind the intermediate, apply, drop it, yield the result -- the CLet+CDrop pattern.

Receiver for ordinary curry intermediate release: an INLINE saturated curried application
of an ordinary two-argument function (TestPerceusInlineCurry).

```
const2 : (Nat -> Nat) -> (Nat -> Nat) -> (Nat -> Nat) is
  fn (a : Nat -> Nat) is fn (b : Nat -> Nat) is a end end
end
mainCurry : Nat -> Nat is const2 (fn (x : Nat) is x end) (fn (y : Nat) is y end) end
```

`const2 A B` closure-converts to AppClosure(AppClosure(CGlobal const2, A), B). const2 is
an ordinary def (not a constructor/eliminator/accel/nat), so the inner AppClosure
`(const2 A)` -- a fresh closure capturing A, allocated every run -- is released after the
call. Per-run ledger:

| step | event | A.rc | intermediate.rc | B.rc | $live |
|------|-------|------|-----------------|------|-------|
| alloc A | rt_mkclo | 1 | - | - | +1 |
| alloc B | rt_mkclo | 1 | - | 1 | +1 |
| `const2 A` | rt_apply: A moves into env | 1 | 1 | 1 | +1 |
| `(const2 A) B` | inner block returns CEnv A (CDup); drops dead b | 2 | 1 | 0 | -1 |
| CDrop{intermediate} | rt_free: rt_release(env A) | 1 | 0 | 0 | -1 |
| harness rt_release(result=A) | rt_free(A) | 0 | 0 | 0 | -1 |

Net per run: +3 allocations, -3 frees = 0. Before Phase 6-pre the intermediate (and the
A it holds) leaked: $live counts [4, 6, 8, 10] (+2/run). After: flat from run 2. Output
`<function>` (const2 A B = A). Bignum-free (closures), so no rt_big_parse temporaries
mask the balance. mulN 100 100 -> 10000 and the nat/list eliminator + multi-field
constructor conformance all stay correct (the matchers are intact).

## 6-pre-fix: FIX B -- constructor shared-owned-local dup

annotateBareSpine was correct for accel/nat spines (their emitters borrow-read each arg,
never storing it by move) but UNSOUND for constructor spines (store = MOVE into the K_CON
slots). The missing case: an owned local appearing in two or more argument slots of the
SAME constructor spine is consumed twice with no dup. The K_CON owns the same heap object
in two slots at refcount 1. When the K_CON is released, both field releases fire:
rc 1 -> 0 (freed), then rc 0 -> -1 (use-after-free / WASM trap or corrupted output).

FIX B adds the shared-owned-local dup to annotateBareSpine when isCtor=true. For each
owned local at de Bruijn index i where cirUsesArg(x.Clo, i) AND cirUsesArg(x.Arg, i),
a CDup{CVar{i}, <spine>} is inserted (mirroring the general AppClosure and CPair shared-
var dup logic). The check fires at each level of the spine recursion, so it covers every
(Clo, Arg) pair in a multi-field constructor application. For accel/nat spines (isCtor
false), the dup is NOT inserted: those spines borrow-read their args, so inserting a
CDup would overcount.

The isCtor flag is passed from the call site in annotate: after FIX A, bareSpineHead
contains ONLY constructors (not eliminators), so isCtor = spineBaseGlobal(x) in
bareSpineHead is exactly the constructor case.

### Known residuals (6b-2 boundary)

Two steady-state residuals remain for the recognise-then-skip path:

1. ARITY>=2 CONSTRUCTOR INTERMEDIATE K_CLO (+1/run per call-site): for an n-ary
   constructor (n >= 2), emitCtorBlock builds n-1 intermediate K_CLOs (one per currying
   step before the final K_CON). These intermediates are kept BARE by annotateBareSpine
   (releasing them would UAF a field the K_CON just moved in). So each inline application
   of a 2-field constructor leaks one K_CLO per run. Cached top-level partial applications
   avoid the per-run leak (the intermediate is built once). A fix requires either (a)
   emitCtorBlock to emit a release of the Clo after the final K_CON is built (within the
   FROZEN WAT emitter, out of edit scope for this task), or (b) a new multi-field ctor
   emitter that builds the K_CON in a single step without any intermediate. Bucketed 6b-2.

2. DEAD-MOTIVE CARVE-OUT LEAK: FIXED by Plan 6b-2 Task 2. The carve-out that left
   a curry-through block's dead argument borrowed was removed. `ownScope` now dead-drops
   the motive in b0 (the first curry block, whose env is empty for non-recursive
   datatypes). Receiver: `TestPerceusDeadMotiveDropped`. See the "curry-through borrow
   carve-out (REMOVED)" section above for the full mechanistic explanation.

### Receiver TestPerceusCtorMkXX

```rune
data Pr : U is mk : (Nat -> Nat) -> (Nat -> Nat) -> Pr end
prElim : Pr -> (Nat -> Nat) is PrElim prMot prCase end
mainMkXX : Nat -> Nat is
  let x : Nat -> Nat = fn (y : Nat) is y end in
  prElim (mk x x)
end
```

Without FIX B: mk x x double-owns x at rc=1 -> annotateCase drops K_CON -> rt_release
field[0] frees x (rc 1->0) -> rt_release field[1] is a use-after-free -> corrupted output
or WASM trap. After FIX B: x is dup'd to rc=2 before the constructor spine; both field
releases balance correctly (rc 2->1->0). Output "<function>" (prElim returns field[0]).

Steady: [7 8 9 10] = +1/run from K_CLO_mk1 (arity-2 ctor intermediate, 6b-2 residual).
The delta is exactly 1 -- proving x itself is balanced and only K_CLO_mk1 leaks.

### Receiver TestPerceusInlineElim (FIX A gate)

```rune
mainInlineElim : Nat is OptElim optMot zero (fn (x : Nat) is x end) (some 7) end
```

Before FIX A: OptElim in bareSpineHead -> 3 intermediate K_CLOs leaked per run. After
FIX A: intermediates released as ordinary curry closures. Output "7" confirms no UAF
from releasing them (the curry blocks dup-on-escape their captured args, so each
intermediate's env carries an independent reference; releasing the K_CLO frees only the
env copy, not the captures themselves).

Steady: [9 13 17 21] = +4/run from dead-motive carve-out + rt_big_parse bignum temps
(the 7 literal, 6b-2 boundary). Inline-eliminator programs are excluded from
`PerceusBalanceable` until 6b-2 closes the carve-out.

## The corpus gate (Task 6)

Task 6 closes the Perceus plan with three assertions layered over the full WASM
conformance corpus (the 10 programs from `TestWasmConformsToJS`):

### Output-invariance gate (TestPerceusCorpusOutputInvariance)

`dup`/`drop` operations adjust only refcounts -- they never change the computed value.
Therefore, the WASM Perceus output must be byte-identical to the JS reference for every
corpus program. `TestPerceusCorpusOutputInvariance` asserts this for all 10 listings
by running both backends and comparing. A regression in the pass that changes output
(e.g., a missing dup that causes a use-after-free) will be caught immediately.

### Steady-state gate (TestPerceusCorpusSteady)

Programs in the v1 Perceus balanceable fragment -- those passing `PerceusBalanceable` --
must reach a true steady state: $rt_live is flat from run 2 onward (no per-run leak).
`TestPerceusCorpusSteady` iterates the corpus, skips programs with
`PerceusBalanceable = false` (the 6b-2 frontier), and asserts flat for the rest.

Result with the current corpus (after the accel-operand closure): 10 balanced, 0
skipped.

| Corpus listing | In the flat fragment? |
|---|---|
| three (natSrc add) | YES -- accel-free inline NatElim fold, flat |
| two (listSrc length) | YES -- accel-free inline ListElim fold, flat |
| p (pairSrc mk) | YES -- saturated arity-4 ctor (satCtorDispatch), flat |
| big, bigger, product (bigNatSrc) | YES -- accel-free NatElim multiplies (satElimDispatch), flat |
| sum, prod, diff, diffZero (accelNatSrc) | YES -- accel ops admitted; accelDispatch frees both owned operands |

Every balanced listing is asserted flat by `TestPerceusCorpusSteady`. The receiver programs
cover the steady-flat assertion for every major ownership pattern (closures, constructors,
case, pairs, curry intermediates, inline + nested + recursive eliminators, NatElim folds,
and now accel arithmetic).

### Frontier boundary (TestPerceusFrontierBoundary)

`TestPerceusFrontierBoundary` asserts that the ch39-style countdown program (which uses
CBounce under closure conversion) has `PerceusBalanceable = false`. This is an
executable boundary marker: if a future change accidentally widens the predicate to
include trampoline programs without 6b-2 CBounce ownership, the test fails visibly. When
6b-2 lands and the pass handles CBounce, the test flips to a steady-flat assertion.

### PerceusBalanceable

`PerceusBalanceable(p Program) bool` in `codegen/perceus.go` scans the closure-converted
CIr of p for exclusion patterns. It is conservative: when unsure, it returns false
(exclude from the steady gate). After the accel-operand closure, only ONE exclusion
remains: CBounce (the per-spine `cirUnbalanceable` scan). Accel-op programs are now
ADMITTED -- `p.Nat.Ops` non-empty no longer triggers a wholesale exclusion. The former
condition-3 (inline general eliminator) and condition-4 (inline arity>=2 constructor)
were already removed; the accel-program wholesale exclusion is now also removed.

### CSnd minor (TestPerceusWasmSnd)

Task 6 also closes the CSnd receiver (Task 5 minor). `perceusWasmSndSrc` projects the
second half of a closure-pair (`mainSnd = Snd (Pair f g)`), drops the pair (which frees
`f`), and returns `g`. The steady gate confirms the CSnd arm in `pairProjSrc` is
correctly wired: $rt_live is flat from run 2 onward (output `<function>`).

### Bignum-pair output-invariance (TestPerceusWasmBignumPair)

`perceusWasmBignumPairSrc` builds a pair of Nat bignum literals (3, 4) and projects
`Fst`. This is the first runtime proof that `CPair`'s dup-on-escape holds for bignum
components: if the CDup were missing, the pair drop would free `K_BIG(3)` before the
return, producing wrong output or a WASM trap. Output "3" confirms correctness. Steady
is NOT asserted (rt_big_parse temps from literals leak per run, 6b-2 residual 5).

### Bare-variable-rebind safety (TestPerceusBareRebind, TestPerceusBareRebindUsesBoth)

Task 6-fix closes the bare-variable-rebind case. `let h = g in body` where `g` is a bare
OWNED local (CLet Val = CVar{k}, owned[k] = true) is a MOVE: `h` and `g` name the same
heap pointer. The bug was that the CLet case marked moved-from owned locals NOT-owned in
the body scope ONLY for CPair components and pair-projections, with no arm for a bare
CVar. So `g` stayed owned, ownScope dead-dropped it (released the pointer), and `h` (the
same pointer) lived on -- use-after-free.

The fix mirrors the CPair arm: when Val is CVar{k} with owned[k] true, set
`bodyOwned[k+1] = false`. This suppresses the dead-drop of `g`; `h` (at body index 0)
owns the value and drops it when dead. `g`'s remaining uses in the body are BORROWS (an
alias); any owning-position use of `g` in the body gets CDup'd by consumeOwning.

The shared-var dup loop already handles the "uses both h and g" case: if both Val and
Body mention index k (Val IS CVar{k}; Body uses CVar{k+1}), a CDup is inserted before
the CLet, giving rc=2. `h` owns one reference; `g` borrows the same pointer. Owning
uses of `g` in the body CDup again (rc+1 per use), which the callee's drop balances.

`TestPerceusBareRebind` covers the basic case (`fn (g) is let h = g in h end`):
steady flat, output `<function>`. `TestPerceusBareRebindUsesBoth` covers the aliased use
(`let h = g in const2 h g`): steady flat, output `<function>`. Both pass after FIX 1.

## Plan 6b-2 outcome: the flat fragment is re-opened

Plan 6b-2 closed the leak-only residuals that the original four-condition
`PerceusBalanceable` was guarding against, then RE-OPENED the predicate to the now-flat
fragment. The closures, in order:

| Residual (formerly held) | Closed by | Receiver |
|---|---|---|
| dead datatype-eliminator MOTIVE (+1/run) | Task 2 (removed the curry-through carve-out; `ownScope` dead-drops the dead motive in b0) | `TestPerceusDeadMotiveDropped` |
| arity>=2 constructor container K_CLO_mk1 (+1/run) | Task 3 (satCtorDispatch emits `rt_mkcon` directly, no intermediate) | `TestPerceusSaturatedCtorNoContainer` |
| rt_big_parse per-digit temporaries (nat literals) | Task 4 (rt_big_parse releases its per-digit temps + big_alloc zeroing) | `TestPerceusBignumParseTemps` |
| rt_big_succ internal K_BIG(1) temp | Task 4b | `TestPerceusBigSuccTempReleased` |
| accel/succ owned-operand frees (succ-chains + add/mul/monus arithmetic) | Task 4d (rt_nat_add/mul/monus + succ_code free their owned operands) | `TestPerceusSuccChainFlat` |
| accel-CVar-operand leak (addN n n, addN ih (succ n) in fold steps) | accel-operand plan Task 1 + Task 2 (accelDispatch frees both owned operands; wholesale exclusion dropped) | `TestPerceusAccelInFlatFragment`, `TestPerceusAccelCorpusFlat` |

### The resulting flat fragment

After 6b-2, the following all reach TRUE steady-flat (zero per-run `$rt_live` delta,
runs 2..N, measured by `wasmSteadyLivePInts` / `assertSteadyFlatInts`):

- **succ-chains** (`succ (succ (succ zero))`, `bigger = succ (succ 5000)`),
- **accel arithmetic** (`addN`/`mulN`/`monusN` bound via `builtin natAdd`/`natMul`/
  `natMonus`, lowered directly to `rt_nat_add`/`mul`/`monus`), including accel ops on
  owned locals (`addN n n`, accel-in-fold-step `addN ih (succ n)`) -- the accel-operand
  residual is CLOSED (both operands freed unconditionally by accelDispatch),
- **saturated constructors** (the arity-4 `mk Nat Nat (succ zero) (succ (succ zero))`),
- **inline NON-RECURSIVE user-eliminators** (`OptElim`/`BoxElim` over a non-recursive
  record), AND empirically the **recursive datatype eliminators** `three` (a NatElim
  fold via the user `add`, no `builtin nat`) and `two` (a ListElim fold via `length`) --
  both measure `[4 4 4 4]`,
- **nat literals** (`big = 5000`, `137`).

The capstone receiver `TestPerceusRealisticFlat` combines a multi-digit literal, a
saturated arity-2 constructor, an inline non-recursive eliminator, and succ arithmetic
in one per-run main path (`BoxElim mot (fn a rest is succ (succ a) end) (mk 42 (succ 7))`
-> `44`) and reaches `[3 3 3 3 3]`.

`PerceusBalanceable` was re-opened in stages: DELETING the former conditions 3 (inline
general eliminator) and 4 (inline arity>=2 constructor), then removing the wholesale accel
exclusion. `TestPerceusCorpusSteady` now reports **10 balanced, 0 skipped** (was 3
balanced, 7 skipped after 6b-2; 0 balanced, 10 skipped before). The HARD INVARIANT stays
one-sided: `PerceusBalanceable` is CONSERVATIVE and must NEVER return true for a non-flat
program; a flat program it excludes is only a missed opportunity.

NOTE on recursive eliminators: the corpus's recursive folds (`three`/`two`) recurse via
the induction hypothesis `ih`, not by calling the eliminator head by NAME in the source
step. Both are empirically flat, so condition 3 is removed rather than narrowed. A
pathological eliminator whose SOURCE step re-invokes the eliminator head is not in the
corpus and is not separately verified flat; if such a program were both balanceable and
leaky, `TestPerceusCorpusSteady` (which asserts flat for every balanceable listing) would
fail loudly rather than silently mis-asserting.

### Builtin-nat-elim FOLD-SETUP residual: CLOSED by satElimDispatch

The fold-SETUP residual is now CLOSED. A saturated builtin-nat fold (`NatElim mot z step
n`) previously lowered via the generic `rt_apply` curry chain (the cached eliminator
thunk's b0->b1->b2->b3), leaking the three intermediate partial-application K_CLOs plus
the erased motive closure -- a constant per-run cost (~6/run; the corpus `product`
measured far more pre-Task-4d). `satElimDispatch` (codegen/wasm.go, the eliminator dual of
Task 3's `satCtorDispatch`) recognizes the saturated 4-arg `NatElimSpine` in `emitIn` and
runs the fold DIRECTLY: it builds ONLY the b3 fold closure (env `{unit, base, step}`),
applies it to `n`, then RELEASES it (freeing base + step) and releases `n`. The erased
motive is evaluated for its annotated side effects and freed immediately (the fold reads
only env slots 1, 2, so the motive is not stored). `emitNatFold` retains its `$acc` so the
base survives the b3 release (the 0-iteration return-the-base case and the step's
per-iteration consume both alias env slot 1). All four spine args arrive OWNED
(`annotateBareSpine` consumeOwning'd each), so the releases are balanced.

No separate Perceus-level nat-elim ownership annotation was needed: the C1 fix already
makes `annotateBareSpine` consumeOwning each nat-elim operand and dup a shared owned local,
so the args reach `satElimDispatch` owned. The result: an ACCEL-FREE SATURATED (4-arg) NatElim fold reaches
TRUE steady-flat, iteration-independent (verified for succ-step, ctor-step, and nested
folds, `TestPerceusNatFoldFlat`, and for the corpus `big`/`bigger`/`product` -- plain
NatElim multiplies of 100 iterations -- all `[1..5]/[2..5]` flat). 8-backend conformance
byte-identical. `PerceusBalanceable` no longer excludes NatElim folds; the corpus
steady-flat set grew from 3 to 6 (`three`/`two`/`p`/`big`/`bigger`/`product`), and
subsequently grew to 10 after the accel-operand closure admitted `sum`/`prod`/`diff`/
`diffZero`.

### ACCEL-operand residual: CLOSED

The accel-operand residual is now CLOSED. accelDispatch (codegen/wasm.go) previously
released only freshly-PRODUCED operand forms (AppClosure/CLit/..., the `freshOwned`
predicate), leaving bare-CVar operands (owned locals) and dup'd-borrow operands un-freed.
The fix (Task 1 of the accel-operand plan): release BOTH operands unconditionally. Privacy
is guaranteed: `annotateBareSpine` routes every accel-spine leaf through `consumeOwning`
(a bare owned CVar stays a single private ref; a borrowed CEnv/CGlobal/projection is dup'd
to a fresh owned ref), and a local consumed in both operand sub-spines is dup'd once by
`annotateBareSpine`'s shared-owned-local dup, while a local also used outside the spine is
dup'd by the enclosing scope. So exactly one live reference reaches the op per operand, and
releasing both is the accel dual of succ_code freeing its arg.

With the leak closed, Task 2 drops the wholesale accel-program exclusion from
`PerceusBalanceable`: any remaining unbalanceable construct (CBounce, over-applied NatElim)
is still caught per-spine by `cirUnbalanceable`. The accel conformance listings
(`sum`/`prod`/`diff`/`diffZero`) join the flat fragment. Receivers:
`TestPerceusAccelInFlatFragment` (was the frontier, now asserts balanceable + flat) and
`TestPerceusAccelCorpusFlat` (sum/prod/diff/diffZero: balanceable + flat + correct output).

### CBounce / partials: CLOSED (WASM-partial-support)

The last excluded class is CLOSED. A `partial`/trampoline program lowered through closure
conversion produces a `CBounce`, which the WASM backend now lowers:

- **Lowering** (`codegen/wasm.go`). `emitPartialWasm` splits a `partial` into a memoized
  `_step` thunk + `arity` curried driver blocks + a public thunk; the saturating driver
  block builds the starting bounce and `$rt_tramp`s it. `emitIn`'s `CBounce` case lowers a
  tail call to a `K_BOUNCE`. The runtime (`codegen/wasm_runtime.go`) gains the `K_BOUNCE=7`
  kind `[kind][step][nargs][arg0..]`, `$rt_mkbounce`/`$rt_bounce_set`/`$rt_tramp`/
  `$rt_bounce_free_shell`, plus a `K_BOUNCE` branch in `$rt_free` (the full-free path).

- **ARC ownership** (novel -- the GC native backends have no analogue). A bounce OWNS its
  args (the cached `_step` head is borrowed); each spine arg is made owned via
  `consumeOwning` in the Perceus pass (`annotateBounceSpine`). `$rt_tramp` applies the step
  to the args, RELEASING each intermediate apply-closure (the borrowed step at i==0 is not
  released), then `$rt_bounce_free_shell`s the spent bounce (args moved into the applies, so
  the shell-free does not touch the slots). An unforced bounce frees its args via `$rt_free`
  (full-free). The driver collect+saturate blocks RETAIN each forwarded env capture, because
  a partial driver intermediate IS released by the generic apply path (unlike constructor
  spines, which are recognized and never built/released) -- without the retain, releasing an
  intermediate driver frees an arg the next driver still aliases (the arity>=3 use-after-free
  found in review).

Receivers (`codegen/perceus_test.go`, `codegen/wasm_test.go`): `TestPerceusPartialCountdownRunsWasm`
(parity), `TestPerceusPartial{Drop,Dup,Shared}Flat` (steady-flat across operand shapes +
correct output), `TestWasmBounceRuntimeRoundTrip` / `TestWasmBounceUnforcedRelease` (the
runtime + the full-free path), and `TestPerceusPartialInFlatFragment` (the former
`TestPerceusFrontierBoundary`, now asserting balanceable + flat). The ch39 countdown is
byte-identical to the other 8 backends.

No missing-feature exclusion remains. The sole per-spine exclusion in `cirUnbalanceable` is
now an OVER-APPLIED NatElim spine (its surplus-application K_CLO leaks).
