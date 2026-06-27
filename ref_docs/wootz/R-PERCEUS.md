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
- CBounce / the partial trampoline: ownership across a DEFERRED saturated tail
  call and the public driver loop. Receiver (future): the ch39 countdown / a
  partial-recursive listing run leak-free on WASM.
- The hand-rolled nat-fold (`emitNatFold`) bignum temporaries: ownership of the
  base-1e9 limb temporaries inside the eliminator loop. Receiver (future): a
  bignum-arithmetic listing (e.g. `mul` on large literals) run leak-free.
Until 6b-2, the WASM Perceus pass is gated to programs that do not exercise these
paths (Task 6 defines the exact subset and asserts the boundary).

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

### The curry-through borrow carve-out (the frozen-runtime boundary)
ONE class of argument stays BORROWED by the callee: a code block whose entire body is a
single `MkClosure` is a CURRYING step that returns a closure. Its argument may be
applied by the FROZEN 6a fold/curry machinery (`emitNatFold`, the builtin-nat
eliminator loop, and `emitCurryBlock`), which BORROWS the value (rt_apply does not
retain) and REUSES it afterward -- `emitNatFold` succ's its loop counter `$k` AFTER
passing it to the step. Dropping that argument would free a value the frozen caller
still uses (observed as the `mulN 100 100 = "101"` and `length = "succ <function>"`
corruptions). So the pass conservatively does NOT drop a curry-through argument
(`isCurryThrough` / the carve-out in `Perceus`): it annotates the body and dups any
escaping capture but leaves the argument borrowed. A REAL consumer (a non-`MkClosure`
body -- an eliminator leaf such as `fn ih is succ ih end`, or a Task 4 match arm that
drops an owned scrutinee) drops its dead argument normally. This is the principled
boundary between the pass's move-discipline and the frozen runtime's borrow-discipline
at the eliminator/fold interface. The residue is a bounded leak of an ignored curried
argument (never a corruption, never a wrong value), refined once Task 4+ brings
eliminator/fold ownership into the pass.

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
