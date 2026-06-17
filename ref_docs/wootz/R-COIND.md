# R-COIND — Coinduction / corecursion

> Roadmap node C5 (`C5 [I] coinductive types / guarded recursion ⇐ R-COIND`).
> Hard prerequisite for E2 (`E2 [I] bisimulation/equivalence proof library ⇐
> R-CALC, C5, E1`) and named by R-CALC.md as "the hard prerequisite, and the
> reason E2 lists C5 as a dep" (ref_docs/wootz/R-CALC.md:38). The distributed
> track's central notion — *P ~ Q* (bisimilarity) — is coinductive, and the
> outer core is inductive-only. This node makes coinduction *statable* and
> *checkable* without growing the outer core.

## Problem (what's stuck/absent today, with file:line)

The Rune core is **inductive-only and total by construction**. Concretely:

- Datatypes are **initial algebras only**. `data D … is C : … end` declares a
  former, constructors, and a *generated eliminator* (CLAUDE.md, Phase 4). The
  eliminator is "the only recursion principle, so no termination checker exists
  or is needed" (CLAUDE.md, Phase 4). There is **no `codata`, no final
  coalgebra, no observation/projection, no `force`, no guarded fixpoint, no
  productivity checker** — confirmed by R-CALC.md:36-37.
- Strict positivity is checked at declaration and recursion flows *into*
  constructors; `core.CtorSig.Rec []bool` (core/eval.go:72) records which
  constructor arguments are recursive so `tryIota` (core/eval.go:1317) can hand
  each an induction hypothesis. The whole machine is built to *consume* finite
  trees, never to *produce* infinite ones.
- Evaluation is total normalization: `NormalizeUnfold` reduces every closed term
  to a normal form (internal/session/session.go:365). There is no value that
  means "not yet observed" — every neutral is *stuck*, not *delayed*. The only
  laziness is the glued-NbE unfolding thunk `VNeu.Unfold` (core/val.go:28), which
  exists for speed and the proof-cache seam, not to model productivity.
- **You literally cannot state "R is a bisimulation, therefore P ~ Q"** in
  today's core (R-CALC.md:37). A bisimulation is the greatest fixed point of a
  monotone operator — `νX. F(X)` — and the core has only `μX. F(X)`. An infinite
  process, a stream of observable actions, a non-well-founded proof: none have a
  home.

The constraint that makes this hard *and* clean: **Thompson — the outer core
must not grow.** No `codata` term constructor, no new `core.Tm` case, no
hash-format bump (the standing rule: "No hash-format bump unless new core
constructor"). Coinduction must ship the way quotients (v2), the fibrant layer
(v3), the interval, paths, faces, systems, and the Kan operations all shipped: a
**contained builtin group** of bodiless, content-addressed definitions with
permanently-neutral heads and ι-rules in the evaluator, on its own hash space
(CLAUDE.md, "Builtin-group pattern"). The productivity guarantee must come from
the *types* (so no separate termination/productivity checker is added), exactly
as Phase 4 got totality "by construction" from the eliminator discipline.

## Prior art (what the literature/other systems do; cite)

Four families. The dependently-typed instances are called out, and I evaluate
each against this substrate's two non-negotiables: *don't grow the outer core*
and *give productivity by typing, not by a new checker*.

1. **Copatterns / coinductive records (Agda, Abel–Pientka–Thibodeau–Setzer).**
   A coinductive type is a *record of observations*: a black box with a finite
   set of projections (`head`, `tail`), defined by *copattern matching*. The
   terminal coalgebra `force : νF → F(νF)` is the eliminator, dual to a
   constructor; "infinite objects only unfold in an elimination (projection)
   context."
   ([Copatterns: Programming Infinite Structures by Observations](https://cs.ioc.ee/~tarmo/tsem12/abel-slides.pdf);
   [Elaborating dependent (co)pattern matching](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/F13CECDAB2B6200135D45452CA44A8B3/S0956796819000182a.pdf/elaborating-dependent-copattern-matching-no-pattern-left-behind.pdf))
   - *Productivity*: by a **syntactic guardedness check** on the corecursive
     definition (every corecursive call sits under a constructor / inside a
     projection clause). This is the part that "often makes programming with
     coinductive types convoluted"
     ([Veltri–van der Weide, *Guarded Recursion in Agda via Sized Types*](https://cs.ru.nl/~nweide/AgdaSizedTypes.pdf))
     — and it is **a new checker**, exactly what Phase 4 was proud of *not*
     needing.
   - *Fit*: the *observation/record* presentation is the cleanest surface and the
     dual of what we already have. But naïve copatterns import a syntactic
     guardedness checker, and (the killer for the E-track) the built-in identity
     type does **not** coincide with bisimilarity.

2. **Sized types (Agda `Size`, Abel).** Track the number of permitted future
   observations in the type: `Stream A i` is a stream observable `i` times.
   Productivity (and termination) reduce to *type checking* — no separate
   checker. ([Abel, *Coinduction in Agda Using Copatterns and Sized
   Types*](https://www.irif.fr/~letouzey/types2014/abstract-40.pdf)).
   - *Fit*: attractive ("productivity by typing"), **but** Agda's sized types
     were found **inconsistent** in their original form (the `Size< ∞`
     interaction), and sizes leak into every signature. They also add an ordinal
     pretype and subtyping over it — a substantial new sort. Too much surface for
     a contained group, and the metatheory risk is real.

3. **Guarded recursion + the later modality `▹` (Nakano; Atkey–McBride;
   Birkedal et al.).** Add a modality `▹ A` ("`A` one step later"), a unit
   `next : A → ▹ A`, applicative application `⊛ : ▹(A→B) → ▹A → ▹B`, and a
   *guarded fixpoint* `dfix : (▹ A → A) → A` with the unfolding
   `dfix f ≡ f (next (dfix f))`. **Productivity is free**: the guard `▹` in the
   fixpoint's domain *is* the proof that every recursive call is one step delayed
   — no syntactic checker, no sizes. To recover genuine (clock-free) coinductive
   types you universally quantify over a **clock variable** `∀κ. …` and use
   `▹^κ`. ([Atkey–McBride, productive coprogramming;
   [Veltri's Agda formalization](https://cs.ru.nl/~nweide/AgdaSizedTypes.pdf)).
   - **Ticked Cubical Type Theory (TCTT)** replaces delayed substitutions with
     *ticks* `(@tick x : A) → B` and — decisively for us — has "confluent,
     strongly normalising reduction semantics satisfying canonicity"
     ([*Towards a cubical type theory without an interval* / TCTT notes];
     Guarded Cubical Agda docs:
     https://agda.readthedocs.io/en/latest/language/guarded.html).
   - **The E-track payoff** — *Bisimulation as a path type for guarded recursive
     types* (Møgelberg–Veltri, POPL 2019): for any functor, the category-theoretic
     bisimilarity of the final guarded coalgebra **is equivalent to path
     equality**, so "guarded recursion can be used to give simple equational
     reasoning proofs of bisimilarity."
     ([arXiv:1810.13261](https://arxiv.org/abs/1810.13261)). And it has been
     *applied*: *Formalizing CCS and π-calculus in Guarded Cubical Agda*
     ([ScienceDirect](https://www.sciencedirect.com/science/article/abs/pii/S2352220822000992))
     does exactly the E2 job — process calculus, bisimilarity as a path — on a
     guarded-cubical base. **This is the literature's answer to our distributed
     track, on a substrate (cubical inner stratum) we are already building.**

4. **Mixed inductive/coinductive by eliminators only (the "no new sort" minimal
   route).** Encode `νF` via its universal property as a builtin group:
   `Nu F : UF`, a destructor `out : Nu F → F (Nu F)`, and the *corecursor*
   (anamorphism) `unfold : (S → F S) → S → Nu F` with the single computation rule
   `out (unfold c s) ~> F (unfold c) (c s)`. Productivity comes from the fact that
   `unfold`'s seed coalgebra `c : S → F S` produces *exactly one layer* per
   `out` — there is no general fixpoint, so nothing to check. This is the dual of
   "datatypes by eliminators" and needs **no new sort, no checker, no modality**.
   It is weaker (no nested guarded fixpoints, corecursion only via `unfold`), but
   it is the smallest thing that lets you *state and prove* a bisimulation.

## Chosen approach for THIS substrate (concrete; respects containment)

**Recommendation: a two-stratum plan, both contained builtin groups, sequenced
by need.**

- **C5a (ready-to-build, ships first): the coalgebra group `Nu` / `out` /
  `unfold`** — family 4. This is the minimal, no-new-sort, no-checker core. It
  rides on the *fibrant* universe `UF` (so coinductive types are fibrant and can
  later carry cubical bisimilarity paths), exactly mirroring how datatypes-by-
  eliminators gave initial algebras. It is enough to *state* `νX.F(X)`, define a
  bisimulation relation, and prove `P ~ Q` propositionally.

- **C5b (research → ready once M2 lands): the guarded modality group `Later` /
  `next` / `lap` (⊛) / `dfix` plus clock quantification `Clock`/`∀κ`** — family 3
  (TCTT-style, *tick-free at the surface*, see below). This is what makes
  bisimilarity *compute as a path* (Møgelberg–Veltri) and gives the E-track its
  "equational reasoning proofs of bisimilarity." It depends on the cubical inner
  stratum being far enough along that paths carry computational content (M2:
  A5–A8), so it is gated behind that.

Both are contained builtin groups on their own hash spaces, registered ambiently
in `session.Reset()` (session.go:59) like every other group, with permanently-
neutral canonical heads and ι-rules in `core/eval.go`. **No `core.Tm` case is
added; no hash-format bump** (standing rule). The outer core stays inductive; the
*power* lives in the group + the evaluator's ι-rules, behind `UF`.

### Why this split honours the three smiths

- **Thompson (radical simplicity).** C5a is the dual of an existing mechanism
  (Phase 4 eliminators), so it adds *one idea*, not a sort. It reuses the exact
  builtin-group machinery (placeholder-rewritten group digest, `FibRoleOf`-style
  reverse lookup, `tryIota`-style ι-rule). C5b is deferred until a real consumer
  (E2) and a ready substrate (M2) both exist — no speculative sort.
- **Savage (teachable).** The teachable on-ramp is *observations*: a `Stream` is
  "a thing you can ask `head` and `tail` of, forever." `Nu`/`out`/`unfold` reads
  exactly like the dual of `data`/`C`/`DElim` a learner already met. C5b's
  `dfix`-unfolds-once and "bisimilar means there's a path" is the advanced
  chapter, introduced only after cubical paths are taught.
- **Lambert (real systems).** Bisimulation is the *correctness currency* of the
  distributed track (E2/E3): "this optimized protocol is observationally equal to
  the spec." C5b's path-equals-bisimilarity is what lets that equality be
  *rewritten with* (transport), not just asserted. The choice is driven by the
  real consumer, not elegance.

### The productivity guarantee (the load-bearing claim)

- **C5a**: productivity is *structural and trivial*. `unfold c` produces one `F`
  layer per `out`; there is no general recursion, so there is no productivity
  obligation at all — exactly as Phase 4 got totality "by construction" because
  the eliminator was the only recursion principle. `unfold` is the only
  corecursion principle. **No productivity checker is added.**
- **C5b**: productivity is *by typing*, via the guard. `dfix : (▹A → A) → A` can
  only be fed a function whose recursive argument is under `▹`; the modality *is*
  the proof that the call is one step later. The reduction `dfix f ~> f (next
  (dfix f))` peels exactly one `▹` and **cannot fire again until an observation
  forces the `next`** — so normalization of a closed `out`-spine always
  terminates after finitely many observations. This is the TCTT canonicity result
  ([Guarded Cubical Agda](https://agda.readthedocs.io/en/latest/language/guarded.html));
  again **no separate productivity checker** — the guard does the work the type
  system already enforces.

### Operational realization on the glued NbE (the substrate fit)

The glued machine already has every primitive C5b needs:

- `▹A` ("later") is realized by the **existing lazy-neutral thunk**. A value
  `next a : ▹A` is a delayed `a` — precisely `VNeu{Unfold: …}` with a canonical
  `Later`-headed spine. `out`/observation forces it. The substrate built laziness
  for the proof-cache seam (core/val.go:28); coinduction reuses it.
- The **`dfix f ~> f (next (dfix f))` one-step unfold** is the same "fire on a
  saturated spine, build a glued neutral whose `Unfold` re-enters the rule" shape
  that `tryIota` already uses for induction hypotheses (core/eval.go:1366: an IH
  is a `VNeu` whose `Unfold` runs the recursive elimination). The cofixpoint's
  delayed self-reference is *the dual of an IH* and uses the identical memoized-
  thunk trick — a case that ignores its delayed value never forces it, so a
  productive corecursion that is only finitely observed normalizes finitely.
- **Guard against runaway unfolding** (the dual of the regularity/constancy
  check): `dfix` must *not* eagerly expand. It fires only when an `out`/projection
  sits on top (an *elimination context*), exactly Abel's "infinite objects only
  unfold in elimination context." The evaluator already distinguishes "fire on a
  just-extended spine" in `tryRules` (core/eval.go:645); `dfix` is registered to
  fire **only** when its result is immediately observed, never standalone. (See
  Risks for the precise trigger discipline.)
- **Bisimilarity-as-path** falls out of C5b living in `UF`: `Nu F` is a fibrant
  code, so `pathF (Nu F) p q` is the bisimilarity type, and the Møgelberg–Veltri
  equivalence becomes a *derived library lemma* (`bisim ≃ pathF`), not new core.

## Interfaces & signatures to add (Go + Rune surface as relevant)

### Go — C5a coalgebra group (store/coind.go, new file; mirrors store/quot.go)

```go
// The coinductive (final-coalgebra) builtin group. Three members, on their own
// hash space, registered against the fibrant group (Nu lands in UF).
//
//   Nu     : (F : UF -> UF) -> UF                          final coalgebra (code)
//   out    : (F : UF -> UF) -> El (Nu F) -> El (F (Nu F))  the one observation
//   unfold : (F : UF -> UF) -> (S : UF) -> (El S -> El (F S))
//            -> El S -> El (Nu F)                           anamorphism / corecursor
//
// Canonical/neutral heads: Nu (a code) and unfold (the corecursion intro) are
// permanently neutral. out COMPUTES by the single coinductive ι-rule:
//
//   out F (unfold F S c s)  ~>  fmapF (unfold F S c) (c s)
//
// where fmapF is F's functorial action. (See Risks: F's functoriality.)
var coindNames = [3]string{"Nu", "out", "unfold"}

type coindEntry struct{ hashes [3]core.Hash }

func (s *Store) AddCoind(fib [11]core.Hash) [3]core.Hash // mirrors AddKan signature

// core.CoindInfo, wired into Machine.Cn (like Machine.Kn), with reverse lookup:
func (s *Store) CoindRoleOf(h core.Hash) core.CoindRole          // CRoleNu/Out/Unfold/None
func (s *Store) CoindHash(role core.CoindRole) (core.Hash, bool) // construct sub-terms
```

```go
// core/coind.go — the role enum + interface (mirrors core.KanInfo)
type CoindRole int
const (CRoleNoneC CoindRole = iota; CRoleNu; CRoleOut; CRoleUnfold)
type CoindInfo interface {
    CoindRoleOf(Hash) CoindRole
    CoindHash(CoindRole) (Hash, bool)
}
// Machine gains: Cn CoindInfo  (core/eval.go:27 struct, set in session.elaborator)
```

```go
// core/eval.go — the ι-rule, dispatched from tryRules (the FRoleEl/out case),
// dual to tryIota. Fires when out's argument forces to an unfold-headed spine.
func (m *Machine) tryCoindIota(role CoindRole, args []Val) (Val, bool)
// out F (unfold F S c s) ~> apply F's functorial action (unfold F S c) over (c s)
```

### Go — C5b guarded group (store/guarded.go; gated behind M2)

```go
//   Later : UF -> UF                                  ▹A  ("A one step later")
//   next  : (A : UF) -> El A -> El (Later A)           the unit
//   lap   : (A B : UF) -> El (Later (piF A (\_.B)))    applicative ⊛
//             -> El (Later A) -> El (Later B)
//   dfix  : (A : UF) -> (El (Later A) -> El A) -> El A guarded fixpoint
//   Clock : U                                          clock pretype (no transport)
//   forallK : (Clock -> UF) -> UF                      ∀κ. — strips ▹ for true codata
//
// dfix COMPUTES by the one-step unfold IN AN ELIMINATION CONTEXT only:
//   out … (dfix A f)  fires  f (next A (dfix A f))   then the outer out reduces.
// next/lap stay canonical; lap computes on next (lap (next g) (next a) ~> next (g a)).
```

### Rune surface (sugar over the groups; the named layer, no core change)

```
# C5a: codata as sugar that desugars to Nu/out/unfold (like `data` -> former+ctors+elim)
codata Stream (A : UF) is
  head : A
  tail : Stream A
end
# desugars to: Stream A := Nu (\X. sigmaF A (\_. X))  -- needs R-SIGMA (A5) for the record
# head s := fst (out _ s);  tail s := snd (out _ s)
# corecursion via copattern sugar elaborates to a single `unfold`:
ones : Stream Nat is unfold (\_. (1, unit)) unit  # head=1, tail=ones
```

The `codata`/copattern surface is **pure elaboration sugar** to `Nu`/`out`/
`unfold`, exactly as `data` elaborates to former + constructors + `DElim`
(session.go:381 `AddData`). The core never sees `codata`.

## Worked micro-example (the teachable artifact)

The furnace/listing artifact: **two definitions of the constant stream, proven
bisimilar.** It teaches the dual of induction and lands the E-track's payoff.

```
# ones, the direct way:
ones  : Stream Nat is unfold Nat (\n. (n, n))         1 1 1 …   (seed = 1)
# ones', via map (+0) applied to ones — a different program, same observations:
ones' : Stream Nat is mapS (\x. x + 0) ones

# bisimulation: a relation R with R ones ones', closed under head/tail.
# C5a (propositional): exhibit R and the closure proof; conclude (Eq-on-observations).
# C5b (cubical, after M2): the relation IS a pathF, so:
onesEq : El (pathF (Stream Nat) ones ones')
       is bisimToPath ones ones' (refl-up-to-+0)     -- Møgelberg–Veltri, as a lib lemma
```

What it teaches, in forgeable order (Savage): (1) a `codata` is defined *by what
you can observe* (`head`/`tail`), the mirror of `data` being defined by how you
*build* it; (2) `unfold` is corecursion — "give a seed and a step, get an
infinite object," dual to the eliminator's "give cases, consume a finite one";
(3) two infinite objects are equal *exactly when no observation can tell them
apart* — bisimilarity — and (C5b) that equality is a path you can transport
along. This is the on-ramp from "I observed they match" to "I proved they're
equal."

## Risks / open sub-questions

- **F's functoriality (C5a, the real sub-question).** `out (unfold c s) ~>
  fmapF (unfold c) (c s)` needs `F`'s functorial action `fmapF`. Options, in
  order of containment:
  (a) **restrict `F` to strictly-positive *codes* built from `sigmaF`/`piF`/`fib`
  and the bound `X`**, and *derive* `fmapF` by recursion on the code (the same
  way strict-positivity is already checked for `data`); ι computes `fmapF`
  structurally. This is the recommended route — **but it needs R-SIGMA (A5)** for
  product/record functors (streams are `X ↦ A × X`). *Honest label: C5a is
  ready-to-build for `piF`/`fib` functors now; the `sigmaF` (record/stream) case
  is gated on A5.*
  (b) take `fmapF` as an explicit argument to `unfold` (a `Functor` dictionary).
  Uglier surface, zero substrate dependency — a viable fallback if A5 slips.
- **The `dfix` trigger discipline (C5b).** Firing `dfix f ~> f (next (dfix f))`
  *eagerly* loops forever in a strict evaluator. It must fire **only** under an
  observation (`out`/projection), and the re-entrant `dfix` inside `next` must be
  a *memoized glued thunk* (the `tryIota` IH trick, core/eval.go:1366) so an
  unobserved tail is never forced. Getting this exactly right (and confluent with
  the cubical rules) is the **research half** — but TCTT proves it *can* be made
  confluent + strongly normalizing + canonical
  ([Guarded Cubical Agda](https://agda.readthedocs.io/en/latest/language/guarded.html)),
  so the target is known-reachable, not speculative.
- **Clocks vs. a single implicit clock.** Full `∀κ` clock quantification is heavy
  surface. For the E-track's needs (one notion of "step"), a **single ambient
  clock** (no clock variables in the surface, `▹` monomorphic) may suffice —
  Atkey–McBride's clocks matter when you mix coinductive types at different rates,
  which the process calculus may not need. *Open: confirm with E1/E2 whether one
  clock is enough; if so, the C5b surface shrinks dramatically.* Recommend
  starting single-clock and adding `Clock`/`forallK` only if E2 demands it
  (Thompson: no feature without a consumer).
- **Coincidence of `Eq` and bisimilarity.** The strict outer `Eq` (Prop, UIP)
  will *not* equate two `unfold`s with the same observations — that is the whole
  point of needing C5b's *path* (the Møgelberg–Veltri result lives in the
  fibrant/cubical layer, not the strict one). C5a alone gives only *propositional*
  bisimilarity (a user-stated relation + closure proof); it does **not** make
  bisimilar processes definitionally or even `Eq`-equal. Be honest in the
  listing: C5a states/proves bisimulation as a relation; C5b makes it a usable
  equality. E2 needs C5b for the "rewrite with bisimilarity" ergonomics.
- **Codegen / deploy.** Like every inner-stratum member, C5a/C5b values are
  **inner-tainted** (check, don't deploy) until an erased runtime meaning exists.
  `next`/`dfix`/`out`/`unfold` join the `innerTaint` set
  (session.go:621). An erased meaning (delay = a thunk/closure, `out` = force,
  `unfold` = a corecursive closure on the target — natural on BEAM, which *is*
  lazy-process-shaped) is a B-track item (cf. R-ERASE2), not this node.
- **Positivity for `Nu`.** Strict positivity must be re-examined for *codata*:
  `F` must be positive in `X` for `Nu F` to be a sound final coalgebra, the dual
  of the `data` positivity check. The existing checker's machinery applies but the
  polarity is the mirror — *label: needs a positivity-checker variant, small but
  not free.*

## Test/gate plan

- **Property (harness/, rapid):** for C5a, `out (unfold c s)` always reduces to
  `fmapF (unfold c) (c s)` on closed seeds (the ι-rule fires); `out` on a neutral
  stays stuck; *finite-observation normalization* — any closed `headⁿ`/`tailⁿ`
  spine of a `codata` value normalizes to a constructor form (the canonicity
  analogue, mirroring harness/cubical_props_test.go's "every closed interval term
  normalizes to an endpoint").
- **C5b productivity pin (internal/session/):** `dfix f` does **not** loop
  standalone (stays a value), but `out (dfix f)` reduces one layer; an unobserved
  tail is never forced (assert the memo thunk fires at most once). A non-guarded
  candidate (recursive call *not* under `▹`) **fails to type-check** — the guard,
  not a checker, rejects it.
- **Confluence/preservation (Track X):** the C5a/C5b ι-rules are confluent with
  the existing data/quot/fib/interval/path/face/Kan rules (each head belongs to
  exactly one family — `tryRules` dispatch invariant, core/eval.go:643). Type
  preservation on the new ι-rules.
- **Listing gate (Savage):** a new listing (the bisimilar-streams artifact above)
  elaborates and checks; the C5b half is gated behind M2 (when paths compute) and
  added to the listings corpus then.
- **Hash stability:** the coind/guarded groups are content-addressed; every
  session agrees on their hashes; **no hash-format bump** (assert
  `hashFormatVersion` unchanged — no new `core.Tm`). Existing
  fib/interval/path/Kan/ch hashes are untouched (own hash space, as every prior
  group preserved).

## Unblocks (which implement nodes, and what they still need)

- **E2 (bisimulation/equivalence proof library)** — the direct consumer. C5a
  unblocks *stating* bisimulation and proving `P ~ Q` as a relation. **E2 still
  needs C5b** for bisimilarity-as-path (the Møgelberg–Veltri equivalence) to get
  usable equational reasoning — so E2 is fully unblocked only after M2 (paths
  compute) + C5b. Until then E2 can be prototyped against C5a's propositional
  bisimulation.
- **E3 (verified protocols + projection)** — needs E2, transitively this node.
- **R-CALC / E1 (the calculus)** — names R-COIND as its hard prerequisite
  (R-CALC.md:38); E1 can define process *terms* without it, but its behavioral-
  equivalence layer cannot exist until C5a (state the relation) lands.
- **R-EFFECT / C3** — R-EFFECT.md:437 notes effect *equivalences* and handler
  reasoning "need E1/E2 and C5/R-COIND"; this node is the upstream edge.
- **D-track / OTP (D5):** infinite process behaviors (servers that "loop
  forever") are coinductive; C5a gives them a type. Not blocking D5's first cut
  (which can be inductive request/response), but the *specification* of a
  long-running supervised process is C5a-shaped.

**Status summary.** C5a (`Nu`/`out`/`unfold`, single ι-rule, no new sort, no
checker) is **ready-to-build now for `piF`/`fib` functors** and ready for stream/
record functors once **R-SIGMA (A5)** lands. C5b (`Later`/`next`/`dfix` +
optional clocks) is **research → ready** and gated behind **M2** (cubical paths
computing); its productivity-by-guard and confluence are *known reachable* (TCTT)
but the strict-evaluator trigger discipline is the genuine implementation
research. Recommend: build C5a immediately (it makes bisimulation *statable* —
the thing R-CALC.md says is impossible today), defer C5b to ride M2 with E2 as
its forcing consumer.
