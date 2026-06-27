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

## Held for Plan 6b-2 (named, not silently dropped)
- CBounce / the partial trampoline: ownership across a DEFERRED saturated tail
  call and the public driver loop. Receiver (future): the ch39 countdown / a
  partial-recursive listing run leak-free on WASM.
- The hand-rolled nat-fold (`emitNatFold`) bignum temporaries: ownership of the
  base-1e9 limb temporaries inside the eliminator loop. Receiver (future): a
  bignum-arithmetic listing (e.g. `mul` on large literals) run leak-free.
Until 6b-2, the WASM Perceus pass is gated to programs that do not exercise these
paths (Task 6 defines the exact subset and asserts the boundary).
