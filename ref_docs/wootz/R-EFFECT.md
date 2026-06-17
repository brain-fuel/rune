# R-EFFECT — Effects & IO over a pure total core

> Roadmap node C3 (`C3 [I] effects/IO layer ⇐ R-EFFECT`). Telos enabler for D5
> (OTP-class concurrency), D6 (IO/OS/net/time/fs), E-track (actors), and the M0
> vertical slice (a program that *runs and deploys* on BEAM).

## Problem (what's stuck/absent today)

The Rune outer core is **pure and total by construction** and has no notion of
effects or IO at all. Concretely:

- The core term language (`core/term.go`) is MLTT + OTT + QTT + universes. There
  is no `IO`, no `World`, no primitive that touches the outside. Every value
  normalizes (`core/eval.go` `NormalizeUnfold`); there is no way to *not*
  evaluate something, and no place a side effect could live.
- Totality is structural: Phase 4 made the eliminator "the only recursion
  principle, so no termination checker exists or is needed" (CLAUDE.md, Phase 4).
  An effect that blocks, loops, or fails has no home in a language where every
  closed term has a normal form.
- The only path to a running artifact is erasure → JS shadow:
  `internal/session/session.go:495` `emitDefs` → `codegen.Erase`
  (`codegen/ir.go:115`) → `codegen/js.go`. The erased IR (`codegen/ir.go`) is an
  untyped lambda calculus: `IVar/IGlobal/IUnit/ILam/IApp/ILet`. There is **no IR
  node for a primitive call**. The JS backend (`codegen/js.go:17` `Emit`) emits
  `console.log($show(main))` — it can *print one pure value* and nothing else.
  There is no `readFile`, no socket, no clock.
- `builtin nat`/`builtin bin` (`session.go:230`, `:256`) are the *only* existing
  "reach into the runtime" mechanism, and they only swap a pure datatype's
  representation (Peano → BigInt, `codegen/js.go:70` `emitNat`). They are not an
  effect channel — they compile a *pure* type faster.

So: a Wootz program today cannot read a file, open a socket, read the clock, or
print more than its single final value. Lambert's bar (reach real OS/net/time/fs)
is unmet, and the M0 slice (deploy a replicated counter on BEAM) is impossible
without this node. R-EFFECT is the missing edge between the verified core and the
real machine.

The constraint that makes this hard *and* makes it a clean fit for this
substrate: **Thompson says the outer core must not grow.** No `IO` type
constructor, no effect rows, no new `core.Tm` case, no hash-format bump. Whatever
ships must ship the way quotients (v2) and the fibrant layer (v3) did — a
**contained builtin group** plus a **minimal runtime primitive** behind the
codegen interface.

## Prior art (what the literature/other systems do)

Three families, with the dependently-typed instances called out:

1. **Monadic IO over an abstract world token (Haskell / Idris / Lean).**
   `IO a` is morally `World -> (a, World)` — a state monad whose state is "the
   real world", represented at runtime by an *opaque token* the runtime supplies.
   - **Idris 2**: `PrimIO a = (1 w : %World) -> IORes a`; `%foreign "C:..."`
     declares a primitive; the `%World` token is erased and the backend turns
     `bind` into ordinary sequencing. IO needs **no core/type-system extension** —
     it is library + one runtime primitive (the world) + FFI pragma.
     ([Idris2 FFI docs](https://idris2.readthedocs.io/en/latest/ffi/ffi.html),
     [Idris2 pragmas](https://idris2.readthedocs.io/en/latest/reference/pragmas.html),
     [Idris-dev IO.idr](https://github.com/idris-lang/Idris-dev/blob/master/libs/prelude/IO.idr))
   - **Lean 4**: `IO a = EIO IO.Error a`, ultimately a state monad over an
     abstract `IO.RealWorld`; primitives are `@[extern]` and the world token is a
     compile-time fiction the runtime threads. "pure returns the world unchanged,
     bind passes the modified world along."
     ([Lean IO logical model](https://lean-lang.org/doc/reference/latest/IO/Logical-Model/),
     [lean4 Init/System/IO.lean](https://github.com/leanprover/lean4/blob/master/src/Init/System/IO.lean))
   - Totality interaction: the world token is **linear/erased**, never
     pattern-matched, never duplicated — so purity and totality of the *core* are
     untouched; the monad's `bind` is a perfectly total tree-builder. The
     *effect* happens only when the backend runtime runs the built tree.

2. **Algebraic effects + handlers (Eff / Koka / Frank / OCaml 5 / Idris
   `Effects`).** Effects are *operations* (`get`, `put`, `print`); a *handler*
   gives them meaning, possibly capturing the continuation. Plotkin–Power
   (effects, 2003) and Plotkin–Pretnar (handlers, 2009).
   - **Koka** puts row-typed effects in the type: `f : int -> <console,exn> int`.
     Compiles via type-directed CPS / evidence passing.
     ([Koka paper](https://arxiv.org/pdf/1406.2061),
     [Leijen, row-typed effect compilation](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/12/algeff.pdf))
   - **Idris `Effects`** (Brady, *Programming and Reasoning with Algebraic
     Effects and Dependent Types*, ICFP 2013) is the load-bearing precedent for
     *this* substrate: a full algebraic-effects-and-handlers system **embedded as
     a library in a dependent type theory, with no language/type-system
     extension** — effects are a first-class `Effect` datatype, handlers are
     records, the whole thing is just dependent data + functions.
     ([Brady, effects.pdf](https://www.type-driven.org.uk/edwinb/papers/effects.pdf),
     [resource-dependent effects](https://link.springer.com/chapter/10.1007/978-3-319-14675-1_2))
   - Cost: to *run* real effects you still need a base monad that actually
     touches the world. Handlers compose pure effects; the leaves are IO. So
     algebraic effects do **not** remove the need for a primitive-IO base — they
     sit on top of it.

3. **Effect systems as type-level annotations (Koka rows, F\* `STExn`,
   capability/region systems).** Track effects in types to prove e.g.
   exception-freedom or determinism. Powerful for *reasoning* but they push
   complexity into the type system (rows, subtyping, unification) — directly
   against Thompson, and they still need a runtime base.

Cross-cutting lesson, three smiths:
- Every dependently-typed system that actually reaches the OS does it with **one
  runtime primitive (the world / an extern call) + a library**, never by growing
  the core (Idris, Lean). That is exactly the Wootz containment doctrine.
- Algebraic-effects-as-library (Brady) proves the *handler* story is *also*
  expressible as pure dependent data on top of that base — so we are not forced
  to choose "monad XOR effects" at the core; the core only owes the base.

## Chosen approach for THIS substrate

**Recommendation: a monadic-IO base as a contained builtin group + exactly one
runtime primitive (the world token) behind the codegen interface; algebraic
effects ship later as a pure *library* on top, not as core machinery.**

Rationale against the three smiths:

- **Thompson (kernel never grows).** This is the quotient/fibrant move again. No
  new `core.Tm`, no hash-format bump (the rule from CLAUDE.md: "No hash-format
  bump unless new core constructor"). We add a sixth-style builtin group
  `store/io.go` of *bodiless, content-addressed* definitions whose heads are
  permanently neutral, exactly like `store/quot.go` / `store/fib.go`. The only
  genuinely new thing is in the **codegen stratum** (a contained interface
  extension), where reality is *supposed* to live — codegen is the menu, per the
  strata thesis.
- **Savage (teachable).** Monadic IO with `do`/`seq` notation is the most-taught
  effect story on earth (Haskell/Idris/Lean curricula). The furnace on-ramp is
  natural: a learner writes `IO`, runs it, *then* learns that `IO a` is "a recipe
  the runtime performs," *then* (much later) meets handlers as a library. One
  concept at the start, depth available on demand. Row-typed effect systems
  (Koka) are powerful but fail the "hold it in your head on day one" test.
- **Lambert (real systems).** The world-token monad is precisely how Idris/Lean
  reach real fs/net/time. It does not "shatter on a partition": failure is a
  *value* (`IOResult`/`Either Error a`), not an exception in the pure core. It
  scales to BEAM (M0): BEAM processes and message sends become IO primitives;
  OTP (D5) and actors (E-track) are libraries whose leaves are these primitives.

Why monad-base *over* algebraic-effects-base as the kernel commitment: an
effects-and-handlers core would either (a) demand a delimited-continuation
primitive in the runtime ABI on day one (more reality surface than M0 needs), or
(b) be implemented as a *free-monad* library — which still bottoms out at the
monadic-IO base anyway. So the base is monadic IO regardless; effects-as-library
is a clean later node (parks under C-ABS / D-track), and Brady shows it composes.

### The shape

The core observation that makes this *zero* outer-core growth: **`IO` is just a
type, `pure`/`bind` are just functions, and the World is just an erased argument.**
QTT already gives us the erasure boundary the world token needs.

```
World  : U                              -- opaque outer type; no constructors, no eliminator
IO     : U -> U                         -- IO A = (0 _ : World) -> IORes A      (definitional)
IORes  : U -> U                         -- a pair-ish carrier: result + next world (see note)
pureIO : (A : U) -> A -> IO A
bindIO : (A B : U) -> IO A -> (A -> IO B) -> IO B
```

Two honest implementation choices for the body of `IO`/`bindIO`:

- **(Preferred) `World` is a 0-quantity (erased) argument.** `IO A` desugars to
  `(0 w : World) -> A` — the world is *proof-only state-threading*: present for
  ordering at type level, erased to a unit at runtime (`core.QZero`,
  `quantity/zeroone.go`, the erasure boundary). `bindIO` is then total and pure:
  it builds a tree. This keeps `IO` definable as an ordinary bodied definition
  whose `bind` *computes* (it's just function composition), and **erasure already
  drops the world** because 0-quantity positions erase to `IUnit`
  (`codegen/ir.go` `IUnit`, the TypedEraser honors `QZero`). No core change.
  Note: with an erased world you lose the runtime guarantee that ordering is
  *forced* by data flow; ordering is instead enforced by the **runtime
  primitive**'s strictness (below) and by `bind` being the only sequencer. This
  matches Lean/Idris, where the token is erased and the backend serializes.

- **(Alternative) `World` linear (`(1 w : World)`).** Threads a single-use token
  so the *type* forbids duplicating/dropping the world — closer to Idris
  `PrimIO`'s `(1 w : %World)`. Pro: ordering is enforced in the pure type, not
  just the backend. Con: requires `IORes` to *return* a fresh world, which means
  a pair carrying `World`, which means **R-SUM (Sigma) must land first** (C1),
  and linear plumbing every primitive. **Decision: ship the erased-world version
  for M0 (no R-SUM dependency); revisit linear-world once C1 lands** if a
  property test shows backends mis-order. Recorded as an open sub-question.

The **primitives** are the contained reality. Each OS/net/time/fs operation is a
bodiless builtin whose *type* lives in Rune and whose *meaning* is a
backend-provided extern:

```
primPutStr   : String -> IO Unit
primGetLine  : IO String
primReadFile : String -> IO (Result IOError String)
primTimeNanos: IO Nat
primOpenTCP  : String -> Nat -> IO (Result IOError Socket)   -- D6 / E-track
... (the D6 surface; added on demand per the parking-lot discipline)
```

These are **permanently neutral heads in the core** (they never reduce; there is
no ι-rule for them — they are the one thing the pure evaluator *cannot* compute,
which is correct: a file read has no normal form). They are the IO analog of a
postulate, but unlike `ua` they are *not* inner-tainted — they have a genuine
**erased runtime meaning** supplied by the backend. This is the key difference
from the §F inner layer.

### How it stays contained: the codegen extension

`codegen.Backend` (`codegen/codegen.go:19`) gains a contained extension — the
*only* new outer-visible surface — a registry of foreign/primitive
implementations:

```go
// codegen/ir.go: ONE new IR node — the foreign call. (No core.Tm change.)
type IForeign struct {
    Op   string   // e.g. "putStr", "readFile", "timeNanos"
    Args []Ir
}
func (IForeign) isIr() {}
```

`store/io.go` registers the builtin group and a reverse lookup
`IOPrimOf(core.Hash) (op string, arity int, ok bool)` (mirroring
`FibHash`/`KanHash`). The eraser (`internal/session/session.go` `emitDefs`,
`elaborate.TypedEraser`) lowers a saturated `primPutStr s` application to
`IForeign{Op:"putStr", Args:[erase s]}` instead of an `IGlobal`. Each backend
maps `IForeign.Op` → real target code (`codegen/js.go`: `putStr` →
`process.stdout.write`; the BEAM backend, B3: `putStr` → `io:put_chars`). An
unknown op for a target is a backend error — the conformance corpus (M4) checks
parity.

`IO` and `World` and `IORes` *formers* erase to the unit token like every type
(the `typeRefs` set in `emitDefs`, `session.go:505`). `pureIO`/`bindIO`, being
ordinary bodied defs, erase to ordinary lambdas — `bindIO` becomes "run the first
action, feed its result to the second," i.e. plain sequencing, because the world
arg is `IUnit`. The `Main` of a program is now allowed to be an `IO Unit`: the
backend's runtime entry *runs* it instead of `$show`-ing it (`js.go:44`).

This is contained: outer core 0 changes, hash format 0 changes, one new IR node,
one backend-interface registry, one new builtin group. Reality lives in codegen,
where the strata thesis says it belongs.

### Interaction with QTT erasure

- The world token is `QZero` → erases to `IUnit` automatically (the existing
  erasure boundary, `quantity/zeroone.go` `Mul`/`Compatible`, `codegen/ir.go`).
  No new erasure logic for the world.
- `IO` primitive *value* arguments (the `String` to `putStr`) are `QMany` → kept.
- Because `bindIO`/`pureIO` are pure total functions, **the proof cache works
  unchanged**: an `IO`-typed definition is checked and certified exactly like any
  other (`session.go:148` `AddDef`), and its dependency set is logged through the
  same `Unfold` gateway. Effects never enter type checking, so the Frame Lemma
  (R-FRAME) is untouched by this node.

### Interaction with backends

- JS backend (B2 reference): `IForeign` → node API calls; `Main : IO Unit` →
  run the thunk tree, world = `null`.
- BEAM backend (B3, M0's target): `IForeign` → Erlang BIFs / OTP calls; this is
  where `primSpawn`/`primSend`/`primReceive` become the D5/E-track substrate. The
  monad serializes message-passing; OTP supervision is a *library* over these
  primitives. This is the edge that makes M0 ("deploys and runs on BEAM")
  reachable.
- Portable IR (B1/R-IR): `IForeign.Op` is the backend-neutral name; the per-
  backend shim is exactly the "portable core + per-backend shims" the IR node
  promises. R-EFFECT defines the *op vocabulary*; R-IR carries it.

### Relationship to the inner-taint mechanism

Crucial distinction from §F: the inner-taint ban (`session.go:621` `innerTaint`,
`EmitProgram` refusal at `:439`) exists because ua-transport *has no erased
meaning*. IO primitives are the opposite: they have a *defined* erased meaning
(the foreign op). So IO defs are **deployable**, not tainted. We do *not* add IO
primitives to the `innerTaint` set. (If anything, a future check ensures a
*pure-claimed* def doesn't secretly call a primitive — but that is what the `IO`
type in the signature already announces.)

## Interfaces & signatures to add

Go (store + core + codegen — all contained, no outer-core term change):

```go
// core/eval.go — mirror of FibInfo/KanInfo. Machine.Io field, nil = no IO group.
type IORole byte
const (
    IORoleNone IORole = iota
    IORoleWorld   // World : U          (opaque, no ctor/elim)
    IORoleIO      // IO    : U -> U
    IORoleIORes   // IORes : U -> U
    IORolePure    // pureIO
    IORoleBind    // bindIO
    IORolePrim    // any primNNN : ... -> IO T
)
type IOInfo interface {
    IORoleOf(Hash) IORole
    IOPrimOf(Hash) (op string, arity int, ok bool) // reverse lookup for the eraser
}
// add to Machine:  Io IOInfo
// pureIO/bindIO are BODIED defs that compute by normal β; primitives are rigid
// heads (rigidHead returns true for IORolePrim, like a constructor).

// store/io.go — the builtin group (pattern of store/fib.go AddFib)
func (s *Store) AddIO() []core.Hash
func IONames() []string
func (s *Store) IORoleOf(h core.Hash) core.IORole
func (s *Store) IOPrimOf(h core.Hash) (string, int, bool)
func (s *Store) IOHashes() ([]core.Hash, bool)

// codegen/ir.go — one new IR node
type IForeign struct { Op string; Args []Ir }

// codegen/codegen.go — Backend gains foreign-op lowering (contained extension)
type Backend interface {
    Target() string
    Emit(p Program) (TargetSource, error)
    // EmitForeign lowers one primitive op to target source given erased args;
    // an unknown op is an error (conformance corpus checks parity).
}
// js.go provides a map[string]func([]string) string for the supported ops.
```

Rune surface (a *library* module, `lib/io.rune` — NOT core):

```
-- IO as an erased-world state thread; World has no constructors.
World : U is ...               -- bodiless builtin (store/io.go)
IO    : U -> U is fn (A : U) is (0 _ : World) -> A end     -- bodied; bind computes
pureIO : {A : U} -> A -> IO A
bindIO : {A B : U} -> IO A -> (A -> IO B) -> IO B
primPutStr  : String -> IO Unit
primGetLine : IO String
-- `seq … end` (the existing GRAMMAR.md block) desugars do-notation onto bindIO.
```

(Surface sugar: the existing `seq … end` desugar — CLAUDE.md, GRAMMAR.md — is
already a sequencing block over `let`; an *opt-in* `do`/`seq`-for-IO desugar onto
`bindIO` is the teachable notation. That sugar is a `surface/` change, contained,
no core impact — park the exact syntax under C-INFRA ergonomics.)

## Worked micro-example (the teachable artifact)

`echo.rune` — the furnace's first *running, deployed* effectful program, and M0's
"hello":

```
greet : IO Unit is
  seq
    name <- primGetLine
    primPutStr (strConcat "hello, " name)
  end
main : IO Unit is greet
```

What the learner sees and can verify:
1. `greet` type-checks as `IO Unit` — the kernel certifies it (`AddDef`), and the
   certificate lands in the proof cache exactly like a pure def. No effect leaked
   into checking.
2. `rune emit echo.rune main` produces JS where `bindIO` has erased to plain
   sequencing and `primGetLine`/`primPutStr` are `IForeign` nodes →
   `await readLine()` / `process.stdout.write`. The world argument is gone
   (erased `QZero`).
3. `rune run echo.rune main` actually reads stdin and writes stdout — the first
   time a Wootz program touches the OS.
4. The teaching beat: "`IO A` is a *recipe* the runtime cooks; `bind` is *and
   then*; the world is the kitchen, which you never see." Later chapter: re-derive
   `bindIO` and show it's pure. Much later: handlers as a library.

Contrast artifact (shows containment): `pure.rune` with `n : Nat is bindIO ...`
is a *type error* — you cannot get a `Nat` out of `IO Nat` without running it,
and only `main` runs. The purity firewall is visible.

## Risks / open sub-questions

- **Erased vs linear world (open).** Erased world (preferred, no R-SUM dep) means
  ordering is enforced by the backend's strict evaluation of the foreign-op tree,
  *not* by the type. Risk: a backend that reorders or shares could mis-sequence.
  *Mitigation:* backend contract — `IForeign` ops are evaluated in `bind` order,
  strictly; the M4 conformance corpus includes an ordering test (interleave
  prints). If it fails on a backend, promote to linear world (needs C1/R-SUM).
  **Status: ready-to-build for the erased version; linear version is research,
  parked behind R-SUM.**
- **`IORes`/pairing.** The erased-world `IO A = (0 w:World) -> A` form sidesteps
  needing a result-world pair (no Sigma). If we ever need the world *returned*
  (linear version, or affine resource tracking for fs handles), R-SUM is a hard
  prerequisite. **Status: research, parked.**
- **Totality vs blocking/divergent IO.** A `primGetLine` that blocks forever does
  not threaten *core* totality (the core only builds the recipe; the recipe is a
  finite total term). But a *recursive* IO loop (`forever : IO a`) needs general
  recursion, which the core forbids (Phase 4, no fix-point). **This is the C4 /
  R-PART boundary**: effectful servers (an echo *loop*) need partiality. M0 can
  ship straight-line / eliminator-bounded IO; long-running servers wait on R-PART.
  **Status: documented dependency on R-PART for unbounded IO loops.**
- **Exceptions / failure model.** Recommend failure-as-value (`Result IOError a`,
  Lean's `EIO`), never a runtime panic that bypasses the type. Lambert: a network
  error is *data*. **Status: ready-to-build (it's a stdlib datatype, D1).**
- **Foreign-op vocabulary explosion.** Resist; add ops on demand (parking-lot
  discipline, CLAUDE.md rule 1). The op set is the R-FFI boundary at its crudest;
  R-FFI later generalizes `IForeign` to *contract-carrying* foreign calls. **The
  primitive set here is the un-typed-contract precursor R-FFI hardens.**
- **Concurrency primitives' semantics.** `primSpawn/Send/Receive` for BEAM raise
  real failure semantics (Lambert: crashes, partitions). Their *typed* meaning is
  D5/R-OTP/E-track work; R-EFFECT only commits to "they are IO primitives with a
  backend extern." **Status: R-EFFECT defines the seam; semantics are R-OTP.**

## Test/gate plan

- **Containment gates (Thompson, must pass):** no change to `core/term.go`,
  `core/hash.go`, or `defFormatVersion`/`hashFormatVersion`; a test asserts the
  hash of an existing pure def is byte-identical before/after the IO group is
  registered (the fib/interval groups already prove this is achievable —
  "fib/interval/path hashes untouched"). `store/io_test.go` mirrors
  `store/fib_test.go`.
- **Purity firewall (property):** any def NOT typed `IO _` erases to an IR with no
  `IForeign` node; conversely a primitive can only appear under `IO`. Property-
  tested in `harness/` (mutation target: a def that smuggles a primitive into a
  pure type must fail to check).
- **Erasure correctness:** the world `QZero` argument erases to `IUnit`; `bindIO`
  erases to sequencing. Golden-IR test in `codegen/`.
- **Run gate (Lambert):** `echo.rune` runs under node and round-trips stdin→
  stdout; `time.rune` reads a monotonic clock; `readfile.rune` reads a fixture
  and returns `Result`. Listing chapter (next furnace listing, e.g. ch24) is the
  acceptance gate, paralleling the `listings/` discipline.
- **Cross-backend conformance (M4):** the same `echo`/`readfile` program produces
  equal observable behavior on JS and BEAM; an ordering test (two interleaved
  prints) pins `bind` sequencing on every backend.
- **Proof-cache regression:** an `IO`-typed def is certified and cache-hits on
  reload exactly like a pure def (extend `store/cert_test.go`).

## Unblocks (which implement nodes, and what they still need)

- **C3 (this node, the implement edge):** ready-to-build for the *erased-world
  monadic-IO base + foreign-op IR node + JS backend impl*. Still needs: the
  agreed initial op vocabulary (small — putStr/getLine/readFile/timeNanos) and
  the `seq`-for-IO surface desugar (contained `surface/` work).
- **M0 vertical slice:** unblocked for the *IO/print/clock* half. The BEAM half
  needs B1+B2+B3 (the IR + BEAM backend) to map `IForeign` ops to Erlang; this
  node defines the op seam those backends implement.
- **D6 (IO/OS/net/time/fs typed+tested tier):** directly unblocked — D6 *is* the
  growth of the primitive vocabulary + `Result`-typed wrappers over it. Needs D1
  (`Result`/`IOError` datatypes) and B4 (FFI) for the richer fs/net surface.
- **D5 (OTP-class concurrency):** unblocked at the seam — `primSpawn/Send/Receive`
  are R-EFFECT primitives; the verified actor/mailbox/supervision *library* is
  D5/R-OTP work over them. Needs B3 (BEAM) and R-OTP semantics.
- **E-track (process calculus → runnable actors):** the projection target
  ("project to runnable actors") bottoms out at these IO concurrency primitives.
  Needs E1/E2 (the calculus) and C5/R-COIND (bisimulation).
- **R-FFI (B4):** `IForeign` is the contract-free precursor; R-FFI generalizes it
  to attach/discharge Rune contracts at the foreign boundary. Needs this node's
  IR node as its substrate.
- **Algebraic effects + handlers (future, NOT this node):** ship as a *pure
  library* (free monad / Brady-style `Effect` data) over `bindIO`. Needs C1
  (R-SUM/Sigma for the effect signatures) and likely C5 (for resumable
  continuations done well). Parked as a D-track library node; the core commitment
  here (monadic base) is what makes it a library and not a core change.

---

**Status: ready-to-build** for the erased-world monadic-IO base, the single
`IForeign` IR node, the `store/io.go` builtin group, and the JS backend impl —
this is a faithful repeat of the v2-quotient / v3-fibrant containment pattern with
the one honest novelty (a foreign-call IR node) placed exactly where the strata
thesis says reality belongs: the codegen menu. **Research/parked:** linear-world
IO (needs R-SUM), unbounded effectful loops (needs R-PART), and algebraic-effect
handlers as a library (needs R-SUM, R-COIND).
