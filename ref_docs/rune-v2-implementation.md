# Rune v2.0.0 — Implementation Notes

*What shipped, where it lives, and the two deliberate deltas from
[rune-v2-design.md](./rune-v2-design.md).*

## What shipped

The quotient former and its kit, as **builtin definitions, not core syntax**:

```
Quot   : (A : U) -> (A -> A -> Prop) -> U
qin    : (A : U) -> (R : A -> A -> Prop) -> A -> Quot A R
qsound : (A : U) -> (R : A -> A -> Prop) -> (a : A) -> (b : A)
         -> R a b -> Eq (Quot A R) (qin A R a) (qin A R b)
qlift  : (A : U) -> (R : A -> A -> Prop) -> (B : U) -> (f : A -> B)
         -> ((a : A) -> (b : A) -> R a b -> Eq B (f a) (f b))
         -> Quot A R -> B
qind   : (A : U) -> (R : A -> A -> Prop) -> (P : Quot A R -> Prop)
         -> ((a : A) -> P (qin A R a)) -> (q : Quot A R) -> P q
```

Computation: `qlift … (qin … a) ~> f a` and `qind … (qin … a) ~> h a`, by
quotient ι-rules in the evaluator, firing exactly like datatype eliminator
ι (saturated spine, scrutinee forced — and therefore dependency-logged —
to a saturated `qin`).

| Concern | Where |
|---|---|
| The group, its hashes, roles, types | `store/quot.go` (`AddQuot`, `QuotRoleOf`) |
| ι-rules | `core/eval.go` (`tryQuotIota`, `core.QuotInfo`, `Machine.Quot`) |
| Ambient registration + wiring | `internal/session` (`Reset`, `elaborator`, `NormalizeExpr`, `EmitProgram`) |
| Erasure | `codegen/js.go` (`quotRuntime`: qin = identity, qlift = apply, proofs = unit) plus the unit-headed-application collapse in `codegen/ir.go` |
| The gate | `listings/ch06_quotients.rune`, `ch07_integers.rune`, `ch08_truncation.rune`; `harness/listings_test.go` |

The group is content-addressed exactly like a `data` declaration: a group
digest over the five member types with intra-group references as positional
placeholders, member hashes derived from `(group, index)`. Every session
computes the same hashes, so certificates and emitted references agree across
sessions with no coordination.

## Delta 1 — no new core constructors (and therefore no hash-format bump)

The design doc's engineering list ("new conversion cases, new hash cases,
new syntax") priced quotients as new term formers. The implementation instead
reuses the Phase-4 mechanism whole: bodiless content-addressed heads +
evaluator ι-rules keyed by stored role. Consequences:

- **Hashing, conversion, quotation, unification, the printer, the grammar,
  and the elaborator are untouched** (quotient applications are ordinary
  `App` spines over `Ref` heads). `hashFormatVersion` stays 0x04; v1
  certificates survive.
- The proof-cache seam the design worried about (§Risks: "unfolding-tracking
  has to see quotient-eliminator reductions") is satisfied for free: the only
  reduction the rules perform forces the scrutinee through the same glued
  `Force` every ι-reduction uses, so the dependency log sees exactly what it
  saw before.
- Quantities thread as ordinary application (the design's prediction held).

`GRAMMAR.md` is therefore unchanged in substance: v2 adds **names**, not
syntax.

## Delta 2 — Eq stays stuck at quotient types (the Lean-style presentation)

The design sketched the full observational move: extend `EvalEq` so
`Eq (Quot A R) (qin a) (qin b)` *computes to* `R a b`. That reduction is
unsound unless `R` is an equivalence relation — with it, `subst` manufactures
symmetry and transitivity of `R` from the general theory of `Eq`, proving
false things about asymmetric relations. The sound versions of that design
either (a) demand equivalence proofs as arguments to the former, or (b)
reduce to the truncated zigzag closure of `R`. Both are real machinery with
no v2 chapter that needs them — and (a) also breaks `refl : Eq Q x x` at
points, since the reduced goal `R a a` is no longer an equality type.

So v2 ships the boring-er version of the boring version, deliberately:

- `Eq` at a quotient type is **stuck**; `refl`, `sym`, `trans`, `subst`, and
  `cast` behave uniformly (and `cast` is still the identity at convertible
  quotient types).
- Identification is **introduced** by `qsound` and **consumed** by the
  irrelevance machinery v1 already has. UIP holds untouched.
- The cost is quotient **effectiveness** (`Eq Q (qin a) (qin b) -> R a b`),
  which is not provable — recorded in PARKING-LOT.md with the upgrade path:
  it is an `EvalEq`/`EvalCast` extension behind the existing
  `core.EqStratum` interface, exactly where the design doc put it.

This is the same judgment call the design doc itself makes one level up
("you don't need homotopy, you need a quotient"): most of "I wish Eq computed
on quotients" is "I wish lifts computed on points" — and they do.

## Truncation

`‖A‖` ships as a listing, not a former: `Squash A := (P : Prop) -> (A -> P) -> P`
in impredicative Prop, with `squash` and `squashElim` as plain definitions
(ch08). The freeze rule decided this: the core may gain nothing the chapters
don't use, and the chapters don't use a primitive `Trunc` — the encoding
already gives introduction, elimination into Prop, and functoriality, and
proof irrelevance makes any two squashed proofs equal. If a future chapter
needs `Eq (Squash A) x y` to compute, the parking-lot entry names the former.

## What the chapters demonstrate

- **ch06** — the kit itself: a parity quotient on Nat; `qsound` closing a
  computation-true identification; a one-line respect proof; `qind` with
  proof irrelevance paying the respect bill.
- **ch07** — ℤ as pairs-mod-equal-differences: respect proofs paid once per
  operation (`zsucc` costs one `cong`); `zadd` by lifting twice, where the
  outer respect obligation is an equality of FUNCTIONS and funext-as-reduction
  turns it into a pointwise `qind` — chapter 3's equality and the quotient kit
  are one mechanism; `znegInvol` and `zaddComm` (nested `qind` through Prop's
  impredicativity) as theorems; the shadow runs `2 + (−1)` under node with
  every proof erased.
- **ch08** — truncation and the propositional toolkit from impredicativity
  alone.
