# R-FRAME — Two-layer proof-cache Frame Lemma for cubical

## Problem (what's stuck/absent today, with file:line)

The proof cache is sound by the **Frame Lemma** (`ref_docs/rune-proof-cache-semantics.md`
§2): `check(d)`'s result is invariant under any store difference that preserves the
bodies in the *consulted set* `U`. The certificate is keyed `CertHash = hash(h, ‖U‖)`
(`store/cert.go:27`, `certKey`), with `U` accumulated as the Machine's write-only
dependency log (`core/eval.go:27` `Machine.Deps`, `core/eval.go:465` `logDep`). The
load-bearing invariant is: **the only way to read a definition's body is `unfold`,
and every `unfold` logs** (`core/eval.go:405` `refVal` — forcing the glued thunk
calls `m.G.Unfold(h)` then `m.logDep(h)`; the body barrier, CLAUDE.md rule 3).

The cache-semantics doc itself flags the gap (`rune-proof-cache-semantics.md:119`):

> The **honest exception** is the cubical-inner frontier (§F)… Confirming the frame
> property survives `Glue` and composition is a *research obligation* of §F, not a
> settled fact.

The obligation is now sharper than "audit that nobody peeks a body," because §F
phase 3 added a **new kind of consultation that is not a body unfold** and is
therefore invisible to `U`:

1. **Constancy probing via the freshness sentinel.** `tryTransp`
   (`core/eval.go:1122`) decides `transp A a0 ~> a0` by applying `A` to
   `kanFreshSentinel` (`core/eval.go:1069`) and scanning the result with
   `mentionsRefVal` (`core/eval.go:1079`). The *outcome of this scan* — constant vs
   varying — is a fact about the **normal form of `A`'s body line**, not about its
   syntax. `mentionsRefVal` is documented as "without forcing glued unfoldings… a
   purely structural occurrence check" (`core/eval.go:1075`). So the reduction's
   result can depend on whether a referenced definition `g` *unfolds to* something
   mentioning `i`, while the scan deliberately does **not** force `g` and therefore
   does **not** log it. `tryComp`'s ⊥ branch has the identical probe
   (`core/eval.go:1267`).

2. **Former-directed structural recursion.** `fibFormer` (`core/eval.go:1177`)
   forces a code value and dispatches on its head former (`FRolePiF`, …). The
   structural `transp`/`hcomp`/`comp` rules (`core/eval.go:1141`,
   `core/eval.go:1223`, `core/eval.go:1282`) fire *only when the head former is
   `piF`*. Whether a neutral code `C` exposes a `piF` head can require unfolding a
   global definition `C = myType …`. `fibFormer` calls `m.Force` (which **does** log
   — `core/eval.go:524`), so this path is currently safe; but the *constancy of the
   domain* `cargs[0]` is again decided by `mentionsRefVal` (`core/eval.go:1149`,
   `core/eval.go:1282`), the non-logging probe.

The concrete soundness hazard: a definition `d` checks, and during conversion a
`transp` fires (or fails to fire) because `mentionsRefVal` found (or didn't find)
the sentinel in the *unforced* spine of some global `g`. The reduction result that
made `d` check thus **depended on `g`**, but `g ∉ U`. In a later store a different
`g′` with the same hash cannot exist (content addressing protects *that* `g`) — but
the danger is subtler: **the result depended on `g`'s body being NOT unfolded**, i.e.
on a *negative* fact ("the sentinel does not occur in the un-forced spine"). The
current `U` records positive unfolds only. When Glue/`ua` land (A6/A7), transport
*through* `ua` (`tryFibIota` `FRoleCastU`, `core/eval.go:810`) and Glue's
transp/hcomp equations will multiply these former-directed, probe-driven decisions,
and each is a place where the consulted set can silently under-approximate.

Today this is masked because every cubical value member is **inner-tainted** and
refused at deploy (`internal/session/session.go:621` `innerTaint`,
`session.go:439` `EmitProgram`), and because the shipped Kan rules are
deliberately shallow (regularity, total/empty system, constant-domain `piF`). But
the cache certifies *checking*, and checking already runs these rules. X1 in the
roadmap ("two-layer closure key (cache captures inner)", DAG Track X) gates **every**
A-milestone; it must land with A1 and stay ahead of A6/A7.

## Prior art (what the literature/other systems do; cite)

- **CCHM / cubicaltt** (Cohen–Coquand–Huber–Mörtberg, *Cubical Type Theory: a
  constructive interpretation of the univalence axiom*). Conversion is decided by
  evaluating to a semantic domain with explicit **dimension environments**; the
  `transp`/`comp` rules are defined *by recursion on the type's head former*
  exactly as Rune's `fibFormer` dispatch does. Crucially, the "is this line
  constant in `i`?" question in cubicaltt is answered by **substituting a fresh
  dimension and checking syntactic independence** — Rune's `kanFreshSentinel` is the
  content-addressed analogue. cubicaltt has no proof cache, so it never had to ask
  *which definitions the constancy decision consulted*.
- **cooltt / Sterling–Angiuli, Normalization for Cubical Type Theory**
  (arXiv:2101.11479). Normalization is a **gluing model**: semantic computability
  is "type shape-directed reflection and reification," always working with typed
  terms *up to judgmental equality*. The lesson for R-FRAME: the *type shape* (head
  former) drives reduction, so the consulted set must include **every definition
  unfolded to expose a head former or to settle a dimension/constancy side-condition**,
  not merely those unfolded to compare two spines. cooltt's NbE forces types to
  whnf to read their shape; the analogue here is "every `fibFormer`/`mentionsRefVal`
  probe must run on the *logged* force path."
- **Cubical Agda** (`Glue : (A) {φ} → Partial φ (Σ T (T ≃ A)) → Set`). Glue's
  transport equations are **stability-under-substitution** identities: `transp`
  over `Glue` reduces by cases on whether `φ` is `⊤`, and the result mentions the
  equivalence's underlying functions. Each such case is a former-directed decision
  whose inputs (the partial family, the equivalence) come from possibly-global
  definitions — the multiplier R-FRAME must be ready for.
- **Separation-logic frame rule** (Reynolds; O'Hearn–Reynolds–Yang) — the
  structural analogy the doc already instantiates (`rune-proof-cache-semantics.md`
  refs). The cubical twist: the "footprint" now includes *facts the reduction
  branched on*, not only heap cells it read.
- **Unison** (content-addressed, append-only) — the immutability that makes "no
  invalidation" true; unaffected by the cubical layer, which is the point: R-FRAME
  must preserve "no invalidation," not reintroduce it.

## Chosen approach for THIS substrate (concrete; respects containment)

**Thesis: keep the *one* key shape `hash(h, ‖U‖)` and the *one* barrier; make every
cubical side-condition decision route through a logging gateway, so `U` regains the
"exact, not over-approximation" property the Frame Lemma needs — with a contained,
conservative *full-closure fallback* for the cases we cannot yet instrument
precisely.** No new core constructor, no hash-format bump (CLAUDE.md), no new
certificate table. This is Track X1, built behind the existing `core.Machine`
interface.

The design has three parts.

### (1) Make the constancy/former probes log — the "probe gateway"

The hole is that `mentionsRefVal` walks an *un-forced* value and that the sentinel
test's outcome depends on what would have been revealed by forcing. Two sound
fixes, combined:

- **Force-before-probe (precision path).** Replace the bare `mentionsRefVal(m,
  line, kanFreshSentinel)` constancy tests with a helper `probeConstant(m, line)`
  that **fully forces** the line value (a deep, logging `Force`/`forceDeep` that
  unfolds every glued thunk reachable, logging each via the existing
  `m.G.Unfold`+`logDep` path) *before* the structural occurrence scan. Forcing is
  the gateway; every definition whose body had to be revealed to settle "does `i`
  occur?" thereby enters `U`. The occurrence scan then runs on a δ-normal value, so
  its answer is a function of `h` and the now-logged `U` only.

  Concretely, the constancy decision becomes a *two-step* logged operation:
  `forceDeepLog(line)` then `mentionsRefVal(forced, sentinel)`. The forced value is
  exactly the input the Frame Lemma frames over. This is sound because: after deep
  forcing, the occurrence of the sentinel is determined structurally, and every
  body consulted to reach that structure was logged — the Frame Lemma's hypothesis
  ("agree on the bodies in `U`") now entails "agree on the forced line" hence
  "agree on the constancy verdict." (Caveat: deep forcing under binders requires
  applying closures to fresh `VVar`s, the same trick `mentionsRefVal` already uses
  at `core/eval.go:1088`; the sentinel is a *reserved hash*, distinct from any
  `NVar`, so it survives.)

- **Former probes already log** (`fibFormer` → `m.Force`), so they need only an
  audit assertion (part 3), not a code change. But the *domain-constancy* sub-probe
  inside the `piF` rules (`core/eval.go:1149`, `:1282`) is a `mentionsRefVal` on
  `cargs[0]` and MUST be routed through `probeConstant` too.

### (2) The two-layer closure key is the SAME key — with a layer tag the audit enforces

The roadmap calls X1 a "two-layer closure key." The honest finding from this
substrate is: **the key does not need two slots.** Both the strict and the fibrant
layer route every body access through one `Machine`, one `Deps` set, one `Unfold`
gateway (`core/eval.go:16` `Globals`; the inner ι-rules force scrutinees exactly as
the outer eliminator does — see the comments at `core/eval.go:768`,
`core/eval.go:872`). So `U` *already* mixes outer-δ and inner-δ dependencies in one
set, and `certKey` already canonicalizes them uniformly. The "two layers" are a
property of *what gets logged*, not of the *key structure*.

What R-FRAME adds is the guarantee that the inner layer's *non-δ* consultations
(constancy/former side-conditions) also funnel into that one set, via (1). The
"two-layer closure key" is therefore realized as: **the same `hash(h, sort(U))`,
where `U` is now closed under inner reduction's consulted set as well as outer
δ.** The teachable statement: *one barrier, indifferent to layer count*
(`rune-proof-cache-semantics.md:113`) — we make that literally true for cubical by
making the cubical probes obey the barrier.

### (3) The safe full-closure fallback (contained, opt-in, retiring)

Some cubical decisions are not yet expressible as "force then structurally decide" —
specifically anything that branches on a **semantic** equivalence that the engine
cannot yet compute (e.g. once Glue/`ua` land, a transport rule that fires based on
an `Eq`-proof's *content* would, by proof irrelevance, be invisible to `U`). For
these, the design ships a **conservative over-approximation** that is sound by
construction and *retires itself* as precision lands:

- A per-run flag `Machine.CubicalImprecise bool`. When any Kan/Glue rule takes a
  branch whose soundness-relevant input it cannot guarantee was logged, it calls
  `m.markImprecise(reachableClosure(h))` — adding the **full syntactic dependency
  closure** of the definition under check to `U`. This is the "coarse full
  transitive closure fallback" the semantics doc retired for the δ-only case
  (`rune-proof-cache-semantics.md:53`), reinstated *locally and explicitly* only for
  the not-yet-instrumented cubical branches.
- Effect: the certificate for such a `d` is keyed on `d`'s entire reachable closure,
  so it is invalidated (missed) by *any* edit to *any* transitive dependency —
  sound but imprecise. As each cubical rule graduates to a logged probe (1), it
  drops its `markImprecise` call and the precision returns. The flag is the
  "research vs ready-to-build" dial made executable.
- The closure is computed from the **store's syntactic reachability** (the store is
  dependency-closed by construction, `rune-proof-cache-semantics.md:96`; `store/scc.go`
  already walks refs for SCC hashing — reuse that walk), never from conversion, so
  it logs nothing extra and cannot diverge.

**Containment check (Thompson).** Parts (1)–(3) live entirely in `core/` (the
`Machine`) and `store/` (the closure walk). No outer-core constructor, no surface
syntax, no new stratum interface method beyond the additive `Globals.Reachable`
(below). The equality interface is untouched. The cache key format is unchanged
(deps still sorted-then-hashed; an imprecise run simply produces a *larger* `U`).

## Interfaces & signatures to add (Go + Rune surface as relevant)

Go, in `core/`:

```go
// forceDeepLog fully δ-unfolds v (under binders, applying closures to fresh
// VVars), logging every body it forces — the gateway-respecting deep force the
// cubical constancy probes run before a structural decision.
func (m *Machine) forceDeepLog(v Val) Val

// probeConstant decides whether a type-LINE (a function I -> UF, or its body at
// the sentinel) is constant in its interval argument, SOUNDLY: it deep-forces
// (logging) then runs the sentinel occurrence scan. Replaces the bare
// mentionsRefVal(...) constancy tests in tryTransp/tryComp and the piF domain
// sub-probe. Returns (isConstant bool).
func (m *Machine) probeConstant(line Val) bool

// markImprecise widens U to the full syntactic reachable closure of the
// definition currently under check — the contained full-closure fallback for a
// cubical branch not yet provably captured by probe logging. No-op once every
// branch is instrumented.
func (m *Machine) markImprecise(root Hash)
```

Additive to the store-view interface (`core/eval.go:16`):

```go
type Globals interface {
    TypeOf(Hash) (Tm, bool)
    Unfold(Hash) (Tm, bool)
    Reachable(Hash) []Hash   // NEW: syntactic dependency closure (pure; logs nothing).
}
```

`store.Store.Reachable` reuses the SCC ref-walk (`store/scc.go`); it is *pure*
(reads sealed bodies inside `store/`, returns hashes only — never a `Tm` body
across the barrier), so it does not breach rule 3.

Machine field (additive; nil/false-default = today's behavior):

```go
type Machine struct {
    // ...existing...
    UnderCheck       Hash   // NEW: the def hash whose check this run certifies (for markImprecise).
    CubicalImprecise bool   // NEW: set true while a not-yet-instrumented cubical branch may fire.
}
```

No Rune surface change. No certificate-table change (`store/cert.go` stays; `U` is
just sometimes larger). No hash-format bump.

## Worked micro-example (the teachable artifact)

The furnace corpus needs a *minimal* program that exercises the seam. Two
definitions that differ only in a body the constancy probe consults:

```
-- a fibrant code that is CONSTANT in i (a degenerate line)
constLine : I -> UF is fn (i : I) is fib Bool end end

-- transp over it must reduce to the identity by regularity
useConst : El (fib Bool) -> El (fib Bool) is
  fn (b) is transp constLine b end
end
```

Checking `useConst` fires `tryTransp`. The constancy verdict — "`constLine i` does
not mention `i`" — required forcing `constLine` (a global). **Therefore `constLine`
must appear in `U(useConst)`.** The teachable assertion the test makes concrete:

1. `runConv(check useConst)` returns `(ok, U)` with `constLine ∈ U` and
   `Bool, fib ∈ U` (the bodies the line unfolded to). Before the fix, `constLine`
   was *absent* — the bug.
2. Mint a sibling store `S′` identical except an unrelated `decoy : I -> UF is fn
   (i) is fib Nat end end` is added. The Frame Lemma demands `(ok, U)` unchanged.
   It is — `decoy ∉ U`. (Precision preserved: the fallback did not fire.)
3. Mutate `constLine`'s body to `fn (i) is somethingThatVariesIn i end`. It mints a
   new hash → new `U` → **cache miss**, re-check. Correct: `transp` no longer
   reduces by regularity, so `useConst` may not even type-check; the cache must not
   serve the stale "ok."

The pedagogy (Savage): *the cache key is the receipt for "which bodies I had to look
inside." Cubical reduction looks inside bodies to decide if a line is degenerate —
so those bodies are on the receipt.* One sentence; demonstrable in ten lines.

## Risks / open sub-questions

- **Deep forcing cost (Lambert).** `forceDeepLog` is strictly more work than the
  current syntactic `mentionsRefVal`. Mitigation: it runs *only* on the type-line
  argument of a Kan op, only when the cheap syntactic scan is *inconclusive*
  (syntactic-no-mention is already sound-constant without forcing; deep force is
  needed only to *rule in* constancy that hides behind a δ-redex). Layer it:
  syntactic scan first (free), deep-force-and-rescan only on a maybe. **Open:** is
  there a closed-form "the line is constant iff its whnf-at-sentinel omits the
  sentinel" that avoids *full* deep forcing? (Research; likely yes — whnf suffices
  if the former is exposed, matching cooltt's shape-directed reflection.)
- **Proof-irrelevant branches under Glue (the hard open one).** When A6/A7 land, a
  Kan-over-Glue rule may branch on an equivalence proof whose *content* is
  irrelevant to conversion (`core/conv.go:36` VRefl irrelevance,
  `core/conv.go:87` cast-proof skip). If a *reduction* ever branches on such a
  proof's shape, `U` cannot capture it by forcing (the proof is never compared).
  This is exactly where `markImprecise` must fire. **Open sub-question:** can every
  Glue transport rule be written to branch only on the *face* `φ` (a cofibration,
  fully forceable and logged) and never on equivalence-proof content? CCHM's
  formulation says yes — the equivalences are *carried into the result*, not
  *cased on*. If that holds here, the fallback is never needed for Glue and X1 is
  fully precise. **This is the core research claim to verify before A6.**
- **Sentinel collision.** `kanFreshSentinel` is a fixed non-hash pattern
  (`core/eval.go:1069`). Deep forcing must never let it reach `Unfold` (it has no
  body); `forceDeepLog` must treat the sentinel as a rigid head (it already is —
  no store entry). Low risk; assert it.
- **`Reachable` and recursion.** The acyclic-only resolver (PARKING-LOT.md) means
  `Reachable` terminates today; when recursive groups land (with Phase-4 totality),
  `Reachable` must walk SCCs as units — `store/scc.go` already models that.

## Test/gate plan

Property tests (rapid; `harness/`, extending `cubical_props_test.go` and the Frame
test `harness/phase1_stubs_test.go:137`):

1. **Cubical Frame Lemma (the X1 gate).** Generalize `TestProofCacheFrameLemma`:
   for a generated well-typed cubical `d`, `(ok, U) = runConv(check d)` in `S`; for
   every `S′` agreeing with `S` on the bodies in `U` (modeled as "add arbitrary
   unrelated defs"), `runConv(check d)` returns `(ok, U)`. Generators must produce
   `transp`/`hcomp`/`comp` over `piF` lines whose constancy hides behind a δ-redex
   (a global alias) — the exact shape part (1) fixes.
2. **Consulted-set exactness.** For the `useConst` family: assert the probed global
   is *in* `U` and an unrelated decoy is *not* — precision, not just soundness.
3. **Mutation tests (MUT, per `rune-proof-cache-semantics.md:127`).** Two mutants
   the cubical layer adds:
   - *Drop the probe-log:* revert `probeConstant` to the non-logging
     `mentionsRefVal`. Property (1) must FAIL (some `S′` run disagrees). A surviving
     mutant ⇒ the seam has a hole.
   - *Decouple imprecise widening:* make `markImprecise` a no-op while a branch
     still needs it. Property (1) must FAIL on the Glue/`ua` corpus.
4. **No-regression / no-invalidation.** On the pure-MLTT and quotient listings
   (ch06–ch10), `U` is byte-identical to today's (the cubical paths never fire), and
   `Certified` still hits — the fallback never triggers, "no invalidation" intact.
5. **Canonicity bridge (Track X2 hand-off).** Closed cubical terms that normalize
   to a constructor form must do so with a `U` that frames correctly — wire X1's
   `runConv`-returns-`U` into X2's canonicity harness so a canonicity failure that
   is *also* a frame failure is attributed to X1.

Static gate (the barrier assertion, `rune-proof-cache-semantics.md:132`): an
Iron-Sieve check that **no constancy/former decision in `core/eval.go` calls
`mentionsRefVal` outside `probeConstant`** — i.e. every cubical side-condition
probe goes through the logging gateway. This turns "did we instrument every cubical
branch?" from review into a compile-/test-time fact, the same move the body barrier
makes for `unfold`.

## Unblocks (which implement nodes, and what they still need)

- **X1 (two-layer closure key)** — this *is* the design for X1. Ready-to-build for
  the **phase-3 shipped rules** (regularity, total/empty system, constant-domain
  `piF`): parts (1) `probeConstant`/`forceDeepLog`, the additive `Reachable`, and
  the generalized Frame property are concrete and gated. Build alongside A1.
- **A1–A4 (cubical interior).** X1 gates them; with part (1) in place, every
  `transp`/`comp`/`hcomp`/`pathJ-via-comp` rule they add must (a) route any
  constancy/former probe through `probeConstant`, and (b) pass the generalized
  Frame property. They still need: the `transpFill`/varying-domain machinery
  (R-FILL) — orthogonal to R-FRAME, but each new rule is a new Frame test case.
- **A6 (Glue) / A7 (ua-as-Glue).** **Needs the open research resolved:** confirm
  (or refute) that every Kan-over-Glue and transport-through-`ua` rule branches
  only on forceable cofibrations `φ`, never on proof-irrelevant equivalence content.
  If confirmed, X1 stays fully precise (no `markImprecise`). If refuted, A6/A7 ship
  *behind* `markImprecise` (sound, imprecise) until a precise capture is designed —
  that is the labelled research remainder. This decision must be made **before** A6
  lands, because it determines whether the cache stays exact across univalence.
- **X2 (cubical property/mutation tests).** Consumes X1's `runConv`-with-`U` to
  attribute frame-vs-canonicity failures; the mutation targets in the Test plan are
  X2's adversarial reads.

**Status of this node:** the phase-3 slice (probe-logging + additive `Reachable` +
contained fallback + generalized Frame/MUT gates) is **ready-to-build**. The
Glue/`ua` precision question (does any reduction branch on irrelevant proof content?)
is **research**, and is the single gate that must clear before A6/A7 to keep the
cache exact through univalence.
