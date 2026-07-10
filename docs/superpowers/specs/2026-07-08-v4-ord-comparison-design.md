# v4 Ord + comparison: DecEq / Ord classes, ordered-algebra bridge, host-native lowering: Design

Date: 2026-07-08
Status: PLAN A + PLAN B COMPLETE (2026-07-10). Full scope: tower instances +
ordered-algebra bridge + all-backend lowering. Plan A (the classes + tower DecEq/Ord instances,
Decisions 1-4 + 7) landed on feat/ord-classes across five commits: d369e9b (Task 1:
Ordering type + DecEq/Ord ops classes + laws records + comparison defaults),
35039f5 (Task 2: Whole DecEq/Ord instances from the leb corpus), 6950fee (Task 3:
Int ileb/ieqb + sign-case DecEq/Ord instances), 618c5af (Task 4: Frac comparison
over the quotient + DecEq/Ord instances, the positive-scaling respect proof), and
Task 5 (ch574 doctrine chapter + the DecEq/Ord/Ordering hash-collision audit +
Plan A close). Cold `rune repl` prelude load steady-state ~0.5s vs the 3s budget
(cold-cache first exec 3.5s is page-cache warmup).

Plan A OUTCOME. The three coherent comparison views (le / compare / Le) landed as
one two-field Ord ops record plus the erased Le proof-view, with DecEq a
principled standalone class (Bool-marker Sig, distinct hash confirmed by the
audit). The campaign's real content is that the Frac quotient order is proven to
respect the relation by POSITIVE-SCALING MONOTONICITY (ilebMulPosR / lebMulPosR),
never by multiplicative cancellation and never by any gcd / lowest-terms fact -
the hard line of the spec held.

Plan B (the ordered-algebra bridge, Decision 5) landed on feat/ord-bridge across
five commits: d23455c (Task 1: the two order-compat statement formers AddMonoT/
MulNonnegT + the three law records OrderedSemiringLaws/OrderedRingLaws/
OrderedFieldLaws, the OrderedRing-through-OrderedSemiring ladder-bridge reuse over
r.1), 23c62d0 (Task 2: Whole ordered-semiring instance - addMono from lebAddMono
via addWComm, mulNonneg definitional), 443d13a (Task 3: Int ordered-ring instance -
sign-case addMonoInt + wsub monotonicity helpers, nonneg-product mulNonnegInt),
f73ab02 (Task 4: Frac ordered-field instance - addMonoF/mulNonnegF lifted over the
quotient by qind, closed by addMonoInt/mulNonnegInt + ilebMulPosR positive scaling),
and Task 5 (ch575 doctrine chapter + the OrderedSemiring/Ring/Field hash-collision
audit + Plan B close). Cold `rune repl` prelude load 0.4-1.1s vs the 3s budget
(Plan A was ~0.5s; the ordered-algebra section is proofs-only, negligible load cost).

Plan B OUTCOME. `orderedFieldLawsFrac` is THE DELIVERABLE and the tower's FIRST
ordered field - an ordered comm ring over the Frac DivRing, composing with
fieldLawsFrac for the full ordered-field story. The bridge is laws-only (no new ops
shape): the two compat facts addMono/mulNonneg are stated once at the semiring rung
and a ring reuses them over its embedded semiring, exactly the ladder-bridge doctrine
the algebra hierarchy already runs. The Frac quotient lifts held to the campaign's
hard line - positive-scaling monotonicity (ilebMulPosR / addMonoInt), never
multiplicative cancellation, never gcd or lowest terms (the STOP contingency never
triggered). The three new record formers hash distinctly against the whole tower
(TestTowerClassHashesDistinct extended). Plan C (all-backend native lowering,
Decision 6) is next.
Parent: docs/superpowers/plans/2026-07-06-three-majors-roadmap.md (v4 numerics;
Ord is the named prerequisite for Real/IEEE754/Decimal).
Consumes: the v4 algebra hierarchy (Semiring/Ring/DivRing ops + laws split,
v3.373-v3.377) and the junk-free Int + quotient Frac campaigns (ringLawsInt,
fieldLawsFrac). Seed: main @ v3.377.0.

## Premise

The numeric tower has arithmetic (the algebra hierarchy) but no ordering as a
class. The prelude has Whole `leb`/`leW` with a proven lemma corpus
(lebRefl/lebTrans/lebComplete + antisymmetry at prelude.rune:5066), but no Int
or Frac comparison, no `Ordering` type, and no comparison CLASS. The bespoke
per-type order chapters (ch65/80/178/179/334/335) are pedagogy over their own
local Nat, not a tower-wide abstraction.

This campaign builds the ordering layer as CLASSES, following the exact
ops/laws-split doctrine the algebra hierarchy established: ops classes carry
operations, separate laws records carry proofs, the guarded tier is the absence
of the laws record (IEEE754 later will have Ord ops with no OrdLaws), positional
Sigma-record discipline with a hash-collision audit.

Two classes ship, both principled and independently usable:

- `DecEq A` (decidable equality) - a matter of principle beyond ordering; the
  eventual Rule-5 home for the tower's ad-hoc equalities (eqW/eqNat/isZeroInt),
  though migrating those is a deferred sweep, not this campaign.
- `Ord A` (total order) - the comparison layer.

Backwards compatibility is a non-goal (v4). The doctrine flip holds: each
feature ships with its book chapter.

## Decision 1: the three coherent comparison views (author directive)

The author's directive: `le : A -> A -> Bool` is the required primitive;
`compare -> Ordering` and a Prop-valued `Le` also exist and are used where
computationally prudent so generated code is not needlessly slow; the compiler
lowers all of them to target-native comparisons (Plan C).

- `Ordering : U is lt : Ordering | eq : Ordering | gt : Ordering end` with its
  generated `OrderingElim`. In the prelude. `flipOrd` (lt<->gt, eq fixed) as a
  plain function.
- `Ord A` ops class carries BOTH computational entry points:
  `Ord A = { le : A -> A -> Bool, compare : A -> A -> Ordering }`.
  Rationale: a guard site wants one Bool (`le`); a sort/merge site wants one
  three-way pass (`compare`) and must not pay for two `le` calls. Carrying both
  lets each call site pick the prudent form; an instance supplies whichever is
  target-native and fills the other with a default.
- Defaults (plain functions, used when an instance provides only one form):
  `compareFromLe : (A -> A -> Bool) -> A -> A -> Ordering` (le a b then le b a;
  both true -> eq, only first -> lt, only second -> gt; the eager-case
  discipline: precompute both Bools with let, then a single nested decision) and
  `leFromCompare : (A -> A -> Ordering) -> A -> A -> Bool` (compare = gt -> false,
  else true).
- `Le : A -> A -> Prop` derived proof-side view: `Le o a b = Eq Bool (le a b) true`
  (Eq at the canonical level is proof-irrelevant, and every existing lebRefl-shape
  lemma already produces exactly `Eq Bool (leb a b) true`). `Le` is erased
  (0-quantity in proofs), costs nothing at runtime, and is what the laws are
  stated over. Derived strict/reversed forms `lt`/`ge`/`gt` as functions where a
  proof or a guard wants them.

## Decision 2: DecEq class + laws

- `DecEq A = { eqb : A -> A -> Bool }` (single ops field; the decidable equality
  test).
- `DecEqLaws A (d : DecEq A) = { eqbSound : (a b) -> Eq Bool (eqb a b) true ->
  Eq A a b, eqbRefl : (a) -> Eq Bool (eqb a a) true }`. (Soundness + reflexivity;
  completeness `Eq A a b -> eqb a b = true` follows from eqbRefl by subst, but is
  provided as a named `eqbComplete` derived lemma for consumers.)
- DecEq is structurally INDEPENDENT of Ord (author: a matter of principle
  outside ordering). Where an Ord exists, an instance MAY define `eqb a b =
  andB (le a b) (le b a)` and prove the laws from OrdLaws; where not, `eqb` is
  native. The class does not force either.

## Decision 3: Ord laws (total order)

```
OrdLaws A (o : Ord A) = {
  leRefl   : (a : A) -> Le o a a
  leTrans  : (a b c : A) -> Le o a b -> Le o b c -> Le o a c
  leAntisym: (a b : A) -> Le o a b -> Le o b a -> Eq A a b
  leTotal  : (a b : A) -> Eq Bool (or (le a b) (le b a)) true
  cmpEq    : (a b : A) -> Eq Ordering (compare a b) eq -> Eq A a b   -- compare-coherence
  cmpLe    : (a b : A) -> Eq Ordering (compare a b) lt -> Le o a b } -- to the le view
```

- Built from the And-chain discipline (a Sig with a constant family), each law
  one named proposition, exactly as SemiringLaws/RingLaws.
- The compare-coherence pair ties the `compare` view to `le`/propositional `Eq`
  so the two computational forms cannot diverge in a lawful instance; kept
  minimal (a full `compare`-trichotomy record is available if a consumer needs
  it, deferred by YAGNI).
- `cmpEq` uses propositional `Eq A`, NOT DecEq, so OrdLaws has no structural
  DecEq dependency.

## Decision 4: proven tower instances (DecEq + Ord)

- **Whole:** `le = leb`; `compare = compareFromLe leb` (or a native three-way
  `compareW`, whichever profiles cleaner - default is fine for Plan A). OrdLaws
  from the existing corpus: leRefl=lebRefl, leTrans=lebTrans, leAntisym= the
  prelude.rune:5066 lemma (name it `lebAntisym` if unnamed), leTotal from
  `lebComplete` (leb a b = false -> leb b a = true) cased into the `or`.
  DecEq: `eqb = eqW` (exists), laws from eqW's soundness/reflexivity (Task 1 of
  the Frac campaign proved eqW's shape; reuse).
- **Int:** new `ileb : Int -> Int -> Bool` by cases on both constructors:
  nonneg a / nonneg b -> leb a b; negsucc k / negsucc j -> leb j k (order
  reverses on negatives); negsucc _ / nonneg _ -> true; nonneg _ / negsucc _ ->
  false. `ieqb` similarly (or via andB of ileb both ways). Laws by case analysis
  reducing to Whole lemmas. `icompare` = native or default.
- **Frac:** new `leF : Frac -> Frac -> Bool` via double qlift over the
  cross-comparison `ileb (imul (qnum p) (qden q)) (imul (qnum q) (qden p))`
  (denominators positive by construction, so no sign flip). `eqF` = the quotient
  equality (andB leF both ways, or via QRel-decision). The RESPECT PROOF is the
  campaign's real content: p ~ p' and q ~ q' imply the cross-comparison is
  invariant. This needs an Int order lemma `ilebMulPosR` / `ilebMulPosCancel`:
  `ileb` is invariant under multiplying BOTH sides by a positive Int (positive =
  `nonneg (succ _)`). This is order-MONOTONICITY under positive scaling, NOT gcd
  and NOT cancellation of the forbidden `imul k a = imul k b -> a = b` shape -
  it is the ordered-semiring fact that positive scaling preserves and reflects
  `<=`. If the respect proof turns out to demand the forbidden multiplicative
  cancellation, STOP and report (same contingency the field-laws campaign
  carried; it did not fire there).

## Decision 5: the ordered-algebra bridge (laws records over existing ops)

Mirrors the algebra hierarchy's laws-split: no new ops shapes, only new law
records combining an existing Ring/DivRing with an Ord.

```
OrderedRingLaws A (r : Ring A) (o : Ord A) = {
  ringLaws  : RingLaws A r
  ordLaws   : OrdLaws A o
  addMono   : (a b c : A) -> Le o a b -> Le o (add (a+c)) (b+c)   -- add is monotone
  mulNonneg : (a b : A) -> Le o 0 a -> Le o 0 b -> Le o 0 (mul a b) } -- product of nonnegs
OrderedFieldLaws A (d : DivRing A) (o : Ord A) (nz : A -> U) = {
  orderedRingLaws : OrderedRingLaws A (d.1) o
  commLaws        : CommLaws A (d.1.1) }
```

- `addMono`/`mulNonneg` stated over `Le` (the erased view), operands via the
  Ring/Ord projections (exact projection spelling resolved at plan time against
  the shipped records, as the Frac assemblies did).
- Proven instances: `orderedRingLawsWhole` (Whole is a Ring? Whole has no
  negation - it is a Semiring, not a Ring; so Whole gets an
  `OrderedSemiringLaws` variant OR is stated over its CommMonoid+Semiring. RESOLVE
  at plan time: either add `OrderedSemiringLaws A (s : Semiring A) (o : Ord A)`
  for Whole, or restrict the bridge instances to Int/Frac which are Rings. The
  spec's position: add the `OrderedSemiring` rung so Whole participates; it is
  one more small law record and the ladder-completeness pattern favors it).
- `orderedRingLawsInt`, `orderedFieldLawsFrac` (the deliverable of Plan B - the
  first ordered field). The Int/Frac `addMono`/`mulNonneg` lemmas are the work;
  they reuse ringLawsInt/fieldLawsFrac + the new order lemmas from Plan A.

## Decision 6: host-native lowering, all 9 backends (Plan C)

The author wants comparison lowered to target-native operations now, not left as
proven-but-interpreted definitions. This touches the codegen shadow + the accel
machinery (internal/session), NOT the frozen kernel (core/store/elaborate).

- The accel machinery today has only ARITHMETIC accel kinds (natAdd/natMul/
  natMonus/natDiv/natMod) returning Nat; there is NO Bool- or Ordering-returning
  accel kind (this is why the Frac campaign's eqW had to ride subW). Plan C ADDS
  a comparison accel kind:
  - `natLeb` (Bool-returning): fires on closed NatLit pairs, gated by the same
    differential-soundness check (the def's unfolded peeling must agree with the
    Go `big.Int` comparison). `leb`/`leW` register against it; `ileb`/`leF`
    reach it through their Whole-comparison cores.
  - Ordering-returning `natCompare` is OPTIONAL: if the Bool `natLeb` accel plus
    a two-call `compareFromLe` already lowers to fast target code, a dedicated
    Ordering accel buys little (YAGNI). Decision deferred to the Plan C
    measurement: add `natCompare` only if a one-pass three-way profiles
    meaningfully faster than two `natLeb` calls on the target.
- Per-backend host bodies: each of the 9 backends (js/py/go/rust/beam/jvm/c/ll/
  wasm) gets a baked native comparison for the registered ops, gated by
  `usesForeign`/the comparison-prim set, mirroring how the arithmetic accels and
  the D6 host ops are baked (codegen/*.go). The native path is the target's own
  `<=` on its bignum/integer representation.
- CONFORMANCE GATE: a comparison corpus (le/compare/eqb on Whole/Int/Frac at
  boundary and interior values, incl. large bignums and negative Ints) observes
  byte-identical results on all 9 backends, mirroring the arithmetic divergence-
  lock gates. This is Plan C's acceptance.
- REPL acceptance (mandatory memory rule): `le`, `compare`, `eqb`, and the tower
  instances behave in `rune repl`; large-bignum comparisons are host-speed (no
  succ-chain materialization), exactly as the C7 compressed-numeral arithmetic.

## Decision 7: chapters (doctrine flip)

- ch574: the Ord + DecEq classes, Ordering type, the three views + defaults, the
  laws records, the proven Whole/Int/Frac instances. Byte-identical mirror of the
  prelude's Ord section with book narration; the Frac respect proof narrated in
  full (the campaign's novelty).
- ch575: the ordered-algebra bridge (OrderedSemiring/OrderedRing/OrderedField
  laws), the Whole/Int/Frac instances, the ordered-field deliverable.
- Bespoke local-Nat order chapters (ch65/80/178/179/334/335) UNTOUCHED (pedagogy
  over own types; nothing superseded - Rule 5 does not fire, they share no
  definitions with the prelude classes).

## Testing summary

- Presence/audit tests per plan (accumulating pattern; class-hash collision
  audit extended for DecEq/Ord/Ordering/OrderedRing/OrderedField).
- REPL comparison pins (le/compare/eqb on the tower, negative Ints, large
  bignums, quotient Frac non-canonical representatives).
- Plan C: the 9-backend comparison conformance gate (the real acceptance) +
  differential-soundness gate on the natLeb accel registration.
- Load-budget watch (the Frac campaigns added negligible load; Ord adds a
  comparable proof corpus - measure single-prelude-load wall time in each plan's
  final task, 3s budget per Decision 5 of the Frac spec).
- Full `go test -timeout 30m ./...` before each tag.

## Sequencing: three plans

- Plan A (classes): Ordering + DecEq + Ord + laws + Whole/Int/Frac instances +
  ch574. Tag on green.
- Plan B (bridge): OrderedSemiring/OrderedRing/OrderedField laws + instances +
  ch575. Tag on green.
- Plan C (lowering): natLeb accel kind + per-backend native comparison bodies +
  9-backend conformance gate + REPL host-speed pins. Tag on green.

## Non-goals

- No new numeric types (Word/Decimal/IEEE754/Real/Complex/Quaternion - later v4
  sub-specs; Ord is their prerequisite, IEEE754 will be the first guarded-tier
  Ord: ops, no OrdLaws).
- No core kernel change / no hash-format bump (Plan C touches codegen + session
  accel machinery only, the mutable shadow per Rule 4).
- No migration of the existing ad-hoc equalities (eqW/eqNat/isZeroInt) to DecEq
  - a deferred Rule-5 sweep once DecEq has settled and a consumer motivates it.
- No sorting / Set / Map / balanced-tree consumers (Ord is the prerequisite;
  the consumers arrive with the v6 stdlib).
