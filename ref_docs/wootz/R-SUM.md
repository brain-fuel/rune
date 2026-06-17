# R-SUM — Sigma + dependent records in the outer core

> Scope note. This node is about the **OUTER** core (the strict OTT kernel:
> `core/term.go`, `core/val.go`, `core/conv.go`, `core/hash.go`). It is a
> *deliberate, last-resort* kernel change — the roadmap names only C-OVERLAP and
> R-SUM as qualifying (`humble-humming-elephant.md:84-86`). It is **not** R-SIGMA,
> which is the *inner, fibrant* `sigmaF` builtin group routed around the outer core
> (`ref_docs/wootz/R-SIGMA.md`). R-SUM is C1; it also resolves **C-ABS** (the
> ad-hoc-polymorphism mechanism) by making typeclasses ride on these records.

## Problem (what's stuck/absent today, with file:line)

1. **There is no Σ in the outer core at all.** `core/term.go:176-189` enumerates
   the entire sealed `Tm` set — `Var, Meta, Prop, Eq, Refl, Cast, Subst, Ref,
   Univ, Pi, Lam, App, Let, Ann` — and there is no `Sig`/`Pair`/`Fst`/`Snd`.
   `core/val.go:81-87` mirrors it: `VNeu, VU, VProp, VEq, VRefl, VPi, VLam`, no
   pair value, no projection neutral. The v1 design *reserved* `Sig`/`Pair`/`Fst`/
   `Snd` (`rune-v1-design.md:81`) but they were parked for want of a consumer
   (`PARKING-LOT.md:71-73`).

2. **Eq-U decomposition is stuck on it.** `equality/observational.go:17-19`:
   *"Deeper type-equality DECOMPOSITION (`Eq U (Pi …) (Pi …)` unfolding to a
   telescope of equalities) needs Sigma types and is parked."* `PARKING-LOT.md:9-11`
   records the same. Today `EvalEq` decomposes a function type pointwise
   (funext, `observational.go:31-38`) but cannot decompose `Eq U A B` for two
   *type* endpoints — the result is a telescope `Σ (p : Eq U dom₁ dom₂), Eq (…)
   cod₁ cod₂`, and the conjunction-of-equalities target type *is itself a Σ*. With
   no Σ, the rule has nowhere to land, so `Eq U (Pi …) (Pi …)` stays a stuck
   `VEq`.

3. **No records ⇒ no module values, no first-class theories, no typeclasses.**
   The DAG hangs C2 (ad-hoc polymorphism), C6 (modules/namespaces), D1 (algebra:
   `Monoid…Field` are *records of operations + laws*), and E1 (typed channels
   carry record-shaped session state) all off C1
   (`humble-humming-elephant.md:193-231`). A `Monoid A` is `Σ (op : A→A→A) Σ (e :
   A) (laws…)`; with no Σ it cannot be a *value* you pass, store, or resolve.

4. **The encoding workarounds are insufficient for these consumers.** Two exist
   and both fail the same way:
   - *Impredicative Church-Σ* (`Σ A B ≅ Π (C:U), (Π a, B a → C) → C`), already
     used for propositional truncation (`CLAUDE.md` v2.0.0 line). Projections are
     only *propositionally* equal to their inputs; **η fails**; `fst (pair a b)`
     is not definitional. The Eq-U telescope and typeclass instance conversion
     both need definitional projection + η.
   - *A single-constructor `data` declaration* (Phase 4 eliminator machinery,
     `store/data.go`). This gives a constructor `mk` and an eliminator that
     ι-reduces `elim … (mk a b) ~> case a b` (so projections *as `elim`* compute
     on a literal `mk`). But the eliminator pattern provides **no definitional η**:
     a *neutral* record `r` does not convert to `mk (r.1) (r.2)`. Lean's whole
     point (below) is that records need *exactly that* η to make instance
     resolution and structure equality go through. The data machinery also can't
     express the Eq-U target without first *having* Σ.

The honest conclusion: the three real consumers (Eq-U decomposition, typeclass
records, module values) need **definitional projection AND definitional η**, and
η is precisely what the bodiless-builtin / eliminator pattern cannot deliver.
That is the minimal Thompson justification for touching the kernel.

## Prior art (what the literature/other systems do; cite)

- **Lean 4 — structures are single-constructor inductives WITH kernel η.** A
  structure is a non-recursive one-constructor inductive; Lean auto-generates
  projections (`Expr.proj`, *primitive projections*), and crucially the **kernel
  has definitional η for structures**: `s ≡ mk s.1 … s.n`. This was added
  deliberately (`leanprover/lean4#777`) because instance resolution and structure
  equality fail without it. Lean's lesson is exactly R-SUM's: records *are* Σ, and
  the *one* thing the kernel must grow for them to be usable is η.
  ([Lean structures](https://lean-lang.org/theorem_proving_in_lean4/Structures-and-Records/),
  [Lean.Meta.Structure](https://lean-lang.org/doc/api/Lean/Meta/Structure.html),
  [#777 definitional eta](https://github.com/leanprover/lean4/issues/777),
  [Lean4Lean kernel](https://arxiv.org/pdf/2403.14064))

- **Agda — record types η by default.** Non-recursive records enjoy
  η-equality (`eta-equality`/`no-eta-equality` to toggle). Σ is a record. η is the
  default *because* it is what makes records behave as products definitionally.
  ([Agda records](https://agda.readthedocs.io/en/latest/language/record-types.html))

- **Coq — Primitive Projections give structural η** (off by default;
  `Set Primitive Projections`). Without it, records are sugar over a
  one-constructor inductive and η is absent — and the Mathematical Components
  community documents the pain that causes for canonical structures
  ([Coq Typeclasses-and-Canonical-Structures wiki](https://github.com/coq/coq/wiki/Typeclasses-and-Canonical-Structures)).

- **C-ABS prior art — typeclasses vs canonical structures vs ML functors.**
  - *Typeclasses* (Haskell/Coq/Lean): instances are records; a *dictionary* is
    passed implicitly, resolved by search. Coq classes allow overlapping
    instances and use powerful-but-unpredictable unification.
  - *Canonical structures* (Coq/MathComp): the same bundled records, but
    resolution is driven by *unification hints* (canonical instances), which is
    simpler to reason about but forces bundled representations and "requires
    significant ingenuity" at scale (Garillot's thesis).
  - *ML functors* (SML/OCaml): explicit module-to-module functions; no implicit
    search, fully predictable, but verbose — the caller wires every instance.
  - The "Type Classes for Mathematics in Type Theory" (Spitters–van der Weegen)
    line and the MathComp "multiple-inheritance hazards" paper both conclude that
    **the substrate is records either way**; the only axis is *how an instance is
    found* (search vs hint vs explicit).
  ([okmij typeclasses](https://okmij.org/ftp/Computation/typeclass.html),
  [Type Classes for Mathematics](https://arxiv.org/pdf/1102.1323),
  [Modular Type Classes, Dreyer](https://www.cs.cmu.edu/~rwh/papers/mtc/short.pdf),
  [multiple-inheritance hazards](https://arxiv.org/pdf/2306.00617),
  [HN: type classes vs functors](https://news.ycombinator.com/item?id=9050956))

- **OTT/Σ.** Pujet–Tabareau OTT (the outer stratum, `rune-v3-design.md:49`)
  decomposes `Eq` on every type former, including Σ: `Eq (Σ A B) p q ~> Σ (e :
  Eq A p.1 q.1), Eq (B[…]) (cast … p.2) q.2`. So Σ is needed both as the
  *target* of the Eq-U telescope **and** to give Σ-types their own observational
  Eq rule — it closes a hole in the OTT stratum, not just adds a feature.

## Chosen approach for THIS substrate (concrete; respects containment)

### Decision 0 — grow the kernel, minimally, with negative-pair Σ (not n-ary records).

The three consumers need definitional projection + η, which the contained
patterns cannot give (Problem 4). So R-SUM *does* add core constructors. The
Thompson discipline is satisfied by adding the **smallest** thing that has η:
a **binary negative (η-style) Σ** with two projections. Records of arbitrary
arity are *right-nested binary Σ* built in the surface/elaborator — **no n-ary
record constructor enters the core**. Four new `Tm` constructors only:

```
Sig  (A : Tm) (B : Scope)   -- Σ (x : A), B          (B binds x)
Pair (A) (B : Scope) (a) (b)-- the pair (a, b) at Σ A B   (A,B carried for typing/quote)
Fst  (p : Tm)               -- p.1
Snd  (p : Tm)               -- p.2
```

This is exactly the v1 design's reserved set (`rune-v1-design.md:81`,
`Sig Tm (Scope Tm) | Pair Tm Tm | Fst Tm | Snd Tm`) — R-SUM *un-parks* it. Pair
carries `A,B` (like `pabs`/`pairF` carry their codes) so quote is total and the
type is recoverable; conversion ignores them (they are inferable).

**Why negative (η) and not positive (eliminator) Σ?** A positive Σ would reuse
the Phase-4 eliminator pattern and need *no* new core constructor — but it has no
definitional η (Problem 1.4), which is the whole reason we are here. A negative Σ
makes `Fst`/`Snd` the primitives and adds the η rule `p ≡ Pair (Fst p) (Snd p)`
to conversion. This is Lean's primitive-projection choice and Agda's default.

### Decision 1 — conversion gets β, η, and projection rules; that is the only `conv.go` change.

In `core/eval.go`:
- `Fst (Pair _ _ a _) ~> a`, `Snd (Pair _ _ _ b) ~> b` (β, fired in `apply`'s
  sibling — a new `proj` reducer, eagerly, like ι).
- `Fst`/`Snd` of a neutral grow a new neutral head `NFst`/`NSnd`.

In `core/conv.go` (`Conv`, `core/conv.go:23-54`):
- `VSig` vs `VSig`: `Conv Dom` and `Conv (Cod v)` under a fresh var (exactly the
  `VPi` case, `conv.go:42-49`).
- **η for pairs:** if either side forces to a `VPair` *or* is at a Σ type, compare
  `Fst`/`Snd` componentwise — mirroring the existing function-η entry
  (`conv.go:16-21`, `convApplied`). Concretely: a new `convProj` that compares
  `m.Fst(a)` to `m.Fst(b)` and `m.Snd(a)` to `m.Snd(b)`. This fires whenever one
  side is a `VPair`, so a neutral `r` converts to `Pair (Fst r) (Snd r)` — the η
  Lean's `#777` adds. **This is the single Thompson-sensitive edit**; it is local
  (one new case + one helper), structurally identical to function-η already
  present, and property-tested as an equivalence + congruence (the existing Frame
  / conversion harness).

### Decision 2 — hash format bumps once: 0x04 → 0x05.

New core constructors ⇒ new tags ⇒ a hash-preimage change (`core/hash.go:33`,
`hashFormatVersion`). Add four append-only tags after `tagSubst`
(`hash.go:37-51`): `tagSig, tagPair, tagFst, tagSnd`. Bump
`hashFormatVersion` to `0x05` (the comment line documents it, as 0x04 did for
Phase 6). This is the one-time cache nuke priced in for any genuine core
constructor — same event as Phase 6's universe-levels bump (`CLAUDE.md` Phase 6).
The builtin groups (fib/interval/path/…) use `defFormatVersion`, a *separate*
tag, so their hashes are untouched **except** that any member type that itself
*contains* a `Sig` would change — and none does today, so ch09–ch23 stay
byte-identical. (Verify with the hash-stability regression below.)

### Decision 3 — records, modules, and "structures" are surface sugar over nested Σ.

No record machinery in the core. The surface gains a `record`/`struct` form that
the elaborator desugars to right-nested `Sig`, with named fields recorded as
`Scope.Name` hints (display-only, not hashed — `term.go:165-174`). Field access
`r.field` desugars to the right `Fst`/`Snd` chain. A literal `{f1 = a; f2 = b}`
desugars to nested `Pair`. This keeps modules (C6) and theories (D1) as *plain
values of a Σ type* — exactly the roadmap's "module values" (`R-SUM` blurb).
Manifest (non-dependent) record fields and dependent ones are the same Σ; the
sugar just chooses the binder names.

### Decision 4 (resolves C-ABS) — typeclasses-as-records with implicit-instance resolution; recommend it over canonical structures and functors.

All three C-ABS candidates bundle into records (Prior art); the only axis is how
an instance is *found*. The recommendation, **riding on R-SUM**:

> **A typeclass is a Σ-record type; an instance is a value of it; resolution is
> implicit-argument synthesis that searches the registered instance set, with the
> dictionary passed as an ordinary implicit value.**

Concretely:
- A class `class Monoid (A : U) is op : … ; e : … ; laws : … end` desugars to a
  record type `Monoid : U → U` (a Σ). No new core anything — it is a definition.
- An `instance` is a `Pair`-nested value bound in the store like any def, plus an
  entry in a session-level *instance table* keyed by the class head + the
  argument's head symbol (a syntactic key, like canonical-structure hints — cheap,
  predictable).
- A class-constrained signature `(M : Monoid A) => …` elaborates `M` as an
  **implicit** binder (`Icit = Impl`, already in the core since Phase 2,
  `term.go:64-71`). At a use site the elaborator inserts the dictionary by
  *instance search over the table*, not Miller unification — so it reuses the
  Phase-2 implicit-insertion seam (`elaborate/`) but with a class-aware solver.

Why this over the alternatives:
- *vs canonical structures:* CS need the kernel's unification to consult a hint
  database during conversion — that would entangle `core/conv.go` with instance
  resolution (Thompson-hostile; conversion must stay a pure βδη decision).
  Implicit-search keeps resolution in the *elaborator*, leaving conversion pure.
- *vs ML functors:* functors are the explicit fallback and **we keep them for
  free** — an `instance` value can always be passed *explicitly* (`{M}`), so a
  user who wants functor-style predictability just supplies the dictionary. The
  implicit search is the ergonomic default layered on top, not a replacement.
- *Coherence pain:* by keying the table on the argument's head symbol and forbidding
  two instances with the same key (a checkable, first-instance-wins or
  ambiguity-error rule in the *session*, not the kernel), we sidestep the
  overlapping-instance coherence problems Coq classes have, while still allowing
  the explicit-dictionary escape hatch for the rare genuine overlap.

This is the C-ABS recommendation the roadmap already leaned toward
(`humble-humming-elephant.md:153-156`, "canonical-structures/records since
Sigma+records land anyway") — refined to: **records (Σ) + implicit-search
resolution**, with explicit dictionaries (functor-style) as the always-available
floor. It rides entirely on R-SUM's Σ; C2 adds only the elaborator's instance
table and search, **zero core**.

### Decision 5 — Σ's own observational Eq + the Eq-U telescope land together.

With Σ in the core, `equality/observational.go` gains two rules (in the stratum,
*not* the kernel — `EvalEq` is a stratum hook, `eval.go:553-559`):
- `Eq (Σ A B) p q ~> Σ (e : Eq A (Fst p) (Fst q)), Eq (B[cast … (Fst p)]) (…p.2)
  q.2` (Σ-η-aware OTT rule).
- `Eq U (Σ A₁ B₁) (Σ A₂ B₂) ~> Σ (e : Eq U A₁ A₂), (x:A₁) → Eq U (B₁ x) (B₂ …)`,
  and the **parked `Eq U (Pi…) (Pi…)` telescope now lands** because its target Σ
  exists (Problem 2). This retires `PARKING-LOT.md:9-11` and
  `observational.go:17-19`.

These are stratum reductions, so they are *contained behind the equality
interface* — the kernel only supplies the `Sig`/`Pair`/`Fst`/`Snd` data and η.

## Interfaces & signatures to add (Go + Rune surface as relevant)

### core/term.go (the four new constructors)
```go
type Sig  struct { Dom Tm; Cod Scope }            // Σ (x : Dom), Cod
type Pair struct { Dom Tm; Cod Scope; A Tm; B Tm }// (A, B) : Σ Dom Cod
type Fst  struct { P Tm }
type Snd  struct { P Tm }
func (Sig) isTm(){}; func (Pair) isTm(){}; func (Fst) isTm(){}; func (Snd) isTm(){}
```
`usesVar` (`eval.go:608-636`) gains the four cases; so do `pretty`, the resolver,
and `surface.Exp` for the record sugar.

### core/val.go (values + neutrals)
```go
type VSig  struct { Name string; Dom Val; Cod func(Val) Val }   // closure like VPi
type VPair struct { Dom Val; Cod func(Val) Val; A Val; B Val }
func (VSig) isVal(){}; func (VPair) isVal(){}
type NFst struct { P Neutral }   // p.1 stuck on a neutral p
type NSnd struct { P Neutral }
func (NFst) isNeutral(){}; func (NSnd) isNeutral(){}
```

### core/eval.go
```go
case Sig:  return VSig{Name: tm.Cod.Name, Dom: m.Eval(env, tm.Dom),
                       Cod: func(v Val) Val { return m.Eval(env.Extend(v), tm.Cod.Body) }}
case Pair: return VPair{ Dom:…, Cod:…, A: m.Eval(env, tm.A), B: m.Eval(env, tm.B) }
case Fst:  return m.proj1(m.Eval(env, tm.P))
case Snd:  return m.proj2(m.Eval(env, tm.P))
// proj1: VPair -> .A ; VNeu -> VNeu{NFst{spine}} gluing through Unfold ; else panic
// proj2: symmetric. Both fire eagerly, like ι in apply (eval.go:497-499).
```
`Fst`/`Snd` over a glued neutral thread `Unfold` exactly as `apply` does
(`eval.go:485-494`) so forcing a projection forces the head and logs the dep —
the proof-cache seam is reused unchanged.

### core/conv.go (the one Thompson-sensitive edit)
```go
// at the top of Conv, alongside the VLam η entries (conv.go:16-21):
if _, ok := a.(VPair); ok { return m.convProj(lvl, a, b) }
if _, ok := b.(VPair); ok { return m.convProj(lvl, a, b) }
// new case in the switch:
case VSig:
    if y, ok := b.(VSig); ok {
        v := VVar(lvl)
        return m.Conv(lvl, x.Dom, y.Dom) && m.Conv(lvl+1, x.Cod(v), y.Cod(v))
    }
// convProj: m.Conv(lvl, m.proj1(a), m.proj1(b)) && m.Conv(lvl, m.proj2(a), m.proj2(b))
// convSpine: NFst/NSnd cases (compare inner spines), mirroring NApp (conv.go:97-99).
```
`Sub` (`conv.go:110-132`) gains Σ covariance only if a listing needs Σ
subtyping — park it (no consumer yet).

### core/quote.go, core/hash.go
Quote: `VSig→Sig`, `VPair→Pair` (quoting `A,B`), `NFst→Fst`, `NSnd→Snd`. Hash:
four append-only tags + bump to `0x05` (Decision 2).

### Surface / elaborator (the sugar + C-ABS)
- `record Name (params) is field : T ; … end` → nested `Sig` def.
- `{ f1 = a ; f2 = b }`, `r.field` → nested `Pair` / `Fst`/`Snd` chains.
- `class` / `instance` → record def + instance-table entry; `=> ` constraints →
  implicit binders; a class-aware instance search in the implicit-insertion path.
  (C2; **zero core** — all in `elaborate/` + `internal/session`.)

## Worked micro-example (the teachable artifact)

Two artifacts, in the kernel's `refl`-pinning style.

**(a) η + the Eq-U decomposition that was stuck.** This is the parked
`PARKING-LOT.md:9-11` finally going through:
```
-- A neutral pair is definitionally its own projections (η):
etaPair : (A : U) -> (B : A -> U) -> (p : Sig A B) ->
    Eq (Sig A B) p (pair (fst p) (snd p)) is
  fn (A : U) (B : A -> U) (p : Sig A B) is refl p end   -- checks ONLY with η

-- Eq of two FUNCTION TYPES now decomposes (its target is a Sig):
eqPiDecomp : (A : U) -> (B C : A -> U) ->
    Eq U ((x : A) -> B x) ((x : A) -> C x) is
  -- elaborates to a Sig (telescope of domain/codomain equalities);
  -- before R-SUM this Eq U was an irreducible stuck VEq.
  ...
```
The lesson (Savage): *"two function types are equal exactly when their domains
are equal and their codomains are pointwise equal — and 'and' between proofs is a
Σ. No Σ, no decomposition."*

**(b) A typeclass riding on the record (C-ABS).** The teachable C2 artifact:
```
class Monoid (A : U) is
  op  : A -> A -> A
  e   : A
  lid : (x : A) -> Eq A (op e x) x
  rid : (x : A) -> Eq A (op x e) x
end                              -- desugars to  Monoid : U -> U  (a Sig)

instance natAdd : Monoid Nat is
  op = add ; e = zero ; lid = addLeftId ; rid = addRightId
end                              -- a Pair-nested value + a table entry

-- a class-constrained function: the dictionary is implicit, found by search
double : (A : U) => (M : Monoid A) => A -> A is
  fn (x) is op x x end           -- `op` resolves to M.op via the instance table

twice : Nat is double zero       -- M = natAdd inserted by instance search
```
The lesson (Savage): *"a typeclass is just a record of operations and laws; an
instance is just a value; finding the instance is the only new idea, and it lives
in the elaborator, never in the kernel."*

## Risks / open sub-questions

1. **η + neutrals + decidability.** Definitional η for Σ is standard and
   decidable (Lean, Agda ship it), but the η entry must fire on *exactly* the
   right trigger or conversion loops. Mitigation: copy the function-η discipline
   already in `conv.go:16-21` verbatim (trigger on a concrete `VPair`, project
   the other side), and lean on the existing "conversion is an
   equivalence + congruence" harness property to catch a bad trigger. **Ready to
   build**, but η is the part to property-test hardest (it is the metatheory-
   review event the roadmap warns about, `humble-humming-elephant.md:84-86`).

2. **Hash bump blast radius.** 0x05 nukes the cache and re-hashes every def
   (priced in, Decision 2). Open: confirm *no shipped builtin member type
   contains a Σ* so the `defFormatVersion` groups stay byte-stable; a `grep` of
   `store/*.go` `*Types()` for any future Σ is the gate. (None today.)

3. **Eq-U telescope termination.** The OTT decomposition of `Eq U (Σ…) (Σ…)`
   recurses through the codomain family; under a non-terminating open family it
   could loop in `EvalEq`. Pujet–Tabareau's rules are normalizing on well-typed
   input; since `EvalEq` runs only on checked terms this is safe, but add a
   property test that `EvalEq` on closed type-equalities always reaches a normal
   form. *Research-adjacent* until exercised by a real listing; the Σ-Eq rule for
   *values* (5a) is ready, the *type* telescope (5b) lands with the first listing
   that needs `Eq U` of two type formers.

4. **C-ABS coherence / overlap.** The first-instance-wins + ambiguity-error rule
   is a *policy* in the session, not a theorem. Genuine overlapping instances
   (e.g. `Ord (List A)` for any `Ord A`) need a priority/specificity order;
   parametric instances need the search to recurse (resolve `Ord A` to build
   `Ord (List A)`). **The basic non-overlapping table is ready-to-build; recursive
   parametric instance search is a labelled increment** (C2b), and full coherence
   (Haskell-style global uniqueness) is research — but the explicit-dictionary
   floor means no program is *blocked* by it, only made more verbose.

5. **Interaction with QTT.** A Σ binder has a quantity like a Pi (`term.go:127-132`).
   Open: does `Pair` split the quantity across components, and does `Fst`/`Snd`
   count a use? Recommend: Σ binders default ω (like Let, `CLAUDE.md` Phase 5);
   linear Σ is parked until a listing needs it (mirror `PARKING-LOT.md:39-46`).

6. **Interaction with erasure/codegen.** A Σ erases to a 2-tuple, `Fst`/`Snd` to
   tuple projections, `Pair` to a 2-array — straightforward in the JS backend
   (`codegen/js.go`), and *not* inner-tainted (Σ is outer, runs). One new IR shape
   (a pair) or reuse of the existing constructor-record shape. Ready-to-build.

## Test/gate plan

- **Conversion harness (the metatheory-review event):** the existing "conversion
  is an equivalence + congruence" and Frame-Lemma properties (`harness/`,
  `CLAUDE.md` engineering conventions) extended to generate `Sig`/`Pair`/`Fst`/
  `Snd` terms; **η specifically** gets a property: for any closed `p : Sig A B`,
  `Conv p (Pair (Fst p) (Snd p))` holds, and congruence under Σ. Mutation-test
  the η case (a surviving mutant in the η trigger is a soundness hole).
- **β/proj unit tests** (`core/nbe_test.go`): `Fst (Pair…a…) ~> a`,
  `Snd (Pair…b) ~> b`, projection of a neutral stays `NFst`/`NSnd`, glued
  projection forces+logs the head.
- **Eq-U decomposition tests** (`equality/` test): `Eq U (Pi…) (Pi…)` and
  `Eq (Sig…) p q` reduce to the telescopes; the parked `PARKING-LOT.md:9-11`
  case now passes (negative-pin removed).
- **Hash regression:** assert `hashFormatVersion == 0x05`; assert the four new
  tags are append-only (existing tags unmoved); assert every shipped builtin
  group's hashes are byte-identical before/after (no member type gained a Σ).
- **Listing gate:** add `listings/ch_records.rune` (record + projection + η) and
  `listings/ch_typeclass.rune` (Monoid class + instance + class-constrained
  function) to `harness/listings_test.go`; both elaborate, check, **and run**
  (Σ is outer, not inner-tainted — distinguishes R-SUM from R-SIGMA).
- **C-ABS resolution tests** (`internal/session`): instance found by search;
  explicit dictionary overrides; duplicate-key instance errors; missing-instance
  errors with an instructive message (Savage).
- **Round-trip:** `parse ∘ pretty = id` over the record/class sugar; pretty-print
  of a core `Sig`/`Pair` back to named record surface.

## Unblocks (which implement nodes, and what they still need)

- **C1 (Sigma + dependent records)** — *this node*. **Ready-to-build:** the four
  core constructors, β/η/proj conversion, the hash bump, the OTT Σ-Eq value rule,
  and the record sugar. The `Eq U` *type-former telescope* (5b) is ready in shape
  but lands with the first listing that needs it.
- **C2 (ad-hoc polymorphism)** — **unblocked, zero core.** Basic
  (non-overlapping) typeclass records + implicit-search + explicit-dictionary
  floor are ready-to-build on R-SUM. *Still needs:* recursive parametric instance
  search (C2b increment) and an overlap/priority policy for genuine overlap
  (labelled; not blocking — explicit dictionaries are the floor).
- **C6 (modules/namespaces)** — unblocked to represent a module as a Σ-value; the
  *namespace/qualified-name surface* and packaging are additional surface work on
  top, no core.
- **D1 (proven foundations: algebra)** — unblocked: `Monoid…Field` become record
  types and the numeric tower carries its laws as record fields. *Still needs:*
  C2 for the ad-hoc dispatch and the actual proofs.
- **E1 (the calculus: typed channels)** — unblocked to give channels record-shaped
  typed state. *Still needs:* R-CALC (the calculus itself) and C5 (coinduction).
- **Retires:** `PARKING-LOT.md:9-11` (Eq-U decomposition), `PARKING-LOT.md:71-73`
  (Sigma/pairs), `observational.go:17-19` (the parked decomposition comment).
- **Does NOT depend on / is independent of R-SIGMA.** R-SIGMA's inner `sigmaF`
  El-decodes today to an impredicative weak-Σ *because* outer Σ was absent
  (`R-SIGMA.md` Decision 2, Risk 1). Once R-SUM lands, R-SIGMA's `El (sigmaF A B)`
  decoding *may* be upgraded to a native outer `Sig` with **no sigmaF
  member-type/hash change** (only the ι-rule body changes) — a clean,
  scheduled-later improvement, not a dependency in either direction.

**Status of this node: ready-to-build** for the core Σ (constructors, β/η/proj
conversion, hash bump 0x05, the value-level Σ-Eq rule, record sugar) and for the
basic non-overlapping C-ABS layer. **Labelled remainders (not blocking):** the
`Eq U` type-former telescope lands with its first consumer; recursive/overlapping
typeclass instance resolution is a C2 increment; Σ-subtyping in `Sub` and linear
Σ in QTT are parked until a listing needs them. The one genuine
metatheory-review event is **definitional η in `core/conv.go`** — property- and
mutation-tested as the soundness gate, exactly as Thompson/the roadmap demand for
an outer-core change.

Sources:
[Lean structures](https://lean-lang.org/theorem_proving_in_lean4/Structures-and-Records/),
[Lean.Meta.Structure](https://lean-lang.org/doc/api/Lean/Meta/Structure.html),
[Lean #777 definitional eta for structures](https://github.com/leanprover/lean4/issues/777),
[Lean4Lean: Verifying a Typechecker for Lean](https://arxiv.org/pdf/2403.14064),
[Agda record types](https://agda.readthedocs.io/en/latest/language/record-types.html),
[Coq Typeclasses and Canonical Structures](https://github.com/coq/coq/wiki/Typeclasses-and-Canonical-Structures),
[Implementing Type Classes (okmij)](https://okmij.org/ftp/Computation/typeclass.html),
[Type Classes for Mathematics in Type Theory](https://arxiv.org/pdf/1102.1323),
[Modular Type Classes (Dreyer)](https://www.cs.cmu.edu/~rwh/papers/mtc/short.pdf),
[Multiple-inheritance hazards in algebraic hierarchies](https://arxiv.org/pdf/2306.00617).
