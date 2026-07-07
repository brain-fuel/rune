# v4 sub-project 1b: the full Magma-to-Field ladder: Design

Date: 2026-07-06
Status: Author-directed ("I want full magma to field ladder. Start with
first proximal bits.") - this supersedes the minimal-3-rung choice of
Decision 2 in 2026-07-06-v4-tower-hierarchy-design.md, whose ops/laws
split (Decision 1) it keeps and extends.
Seed: main @ v3.373.0.

## The structural fact that shapes the ladder

Ops classes are identified by STRUCTURE (hash of the Σ-record shape), and
the split doctrine puts every law in a separate record. So ladder rungs
that differ ONLY by laws share one ops shape and cannot be distinct ops
classes:

- Magma and Semigroup have the same operations {op} (associativity is a law).
- Monoid and CommMonoid share {op, e} (commutativity is a law).
- Group and AbGroup share {op, e, inv}.
- Ring and CommRing share {semiring, neg}; DivRing and Field share
  {ring, recip}.

The full Magma-to-Field ladder is therefore SIX ops shapes plus TEN law
records, with the classical names distributed honestly between them:

```
ops shapes (dispatchable classes):
  Magma   A = A -> A -> A                      (rename of today's Semigroup)
  Monoid  A = Sig (A -> A -> A) A              (unchanged, {op, e})
  Group   A = Sig (Monoid A) (A -> A)          (NEW, {monoid, inv})
  Semiring A / Ring A / DivRing A              (shipped in 1a, unchanged)

law records (over an ops instance; proven tier only):
  SemigroupLaws  A (m : Magma A)   = AssocT A m
  MonoidLaws     A (d : Monoid A)  = And assoc (And idL idR)
  CommMonoidLaws A (d : Monoid A)  = And MonoidLaws (CommT op)
  GroupLaws      A (g : Group A)   = And MonoidLaws (And invL invR)
  AbGroupLaws    A (g : Group A)   = And GroupLaws (CommT op)
  SemiringLaws / RingLaws / DivRingLaws        (shipped in 1a, unchanged)
  CommRingLaws   A (r : Ring A)            = And (RingLaws A r) (CommLaws A r.1)
  FieldLaws      A (d : DivRing A) (nz)    = And (DivRingLaws A d nz) (CommLaws A d.1.1)
```

InvL/InvR reuse the shipped NegInvLT/NegInvRT formers verbatim - their
statements are already generic in (op, inv, e).

## Decision 1: rename Semigroup to Magma

Today's `Semigroup : U -> U = A -> A -> A` carries NO associativity - it
is honestly a magma. Rename it (definition, the `{d : Semigroup A}`
constraint on `++`, `instance semigroupBytes` to `magmaBytes`, comments,
and the audit-test name list; one comment in repl_test.go). No behavior
change: `++` dispatch is untouched in substance, Bytes/String append
still works, and content hashes do not depend on surface names. The
freed name Semigroup then denotes the LAWFUL level via SemigroupLaws.
No backwards compat owed (v4 policy).

`++` and `mempty` stay dispatched over the OPS records (Magma, Monoid):
the String Monoid instance has no associativity proof and must keep
working - which is precisely the split doctrine's guarded tier.

## Decision 2: Group ops shape + its laws

`Group A = Sig (Monoid A) (fn (_m : Monoid A) is A -> A end)` - nests
Monoid the way Ring nests Semiring, projection g.1 monoid / g.2 inv.
Structurally distinct from Ring (first component Monoid A vs Semiring A).
mkGroup builder in the established style. GroupLaws' inverse laws:
NegInvLT A (g.1.1) (g.2) (g.1.2) and the RT twin (op = g.1.1, e = g.1.2).

No Group instances enter the instance table (nothing dispatches on Group
yet); Group values are plain defs, same YAGNI rule as ringInt.

## Decision 3: the ladder's edges (bridges + law transports)

Named projection/construction functions connecting the rungs, each with
its law transport proven by REASSEMBLY (extract fields from the source
And-chain, rebuild the target chain - no new induction anywhere):

- magmaOfMonoid : Monoid A -> Magma A (= d.1); monoidOfGroup (= g.1).
- addMonoidOfSemiring : Semiring A -> Monoid A ({s.1, s.2.1});
  mulMonoidOfSemiring ({s.2.2.1, s.2.2.2}).
- groupOfRing : Ring A -> Group A ({addMonoidOfSemiring r.1, r.2}).
- Law transports: addCommMonoidLawsOfSemiring (SemiringLaws gives assoc,
  comm, idL, idR of add - a CommMonoidLaws for the add monoid);
  mulMonoidLawsOfSemiring (assoc, idL, idR of mul); groupLawsOfRing
  (RingLaws gives the add MonoidLaws + NegInv pair); abGroupLawsOfRing
  (adds addComm from the semiring laws).

These edges are the mathematical content of "full ladder": a ring IS an
abelian group under addition and a monoid under multiplication, proven,
not narrated.

## Decision 4: Whole's position on the lower rungs (the free instances)

(Whole, addW, zero) and (Whole, mulW, one) are commutative monoids, and
every law already exists from sub-project 1a. Ship plain defs (NOT
instance-table entries - `++` on Whole would make append mean addition,
the Raku lesson):

- monoidWholeAdd / monoidWholeMul : Monoid Whole
- commMonoidLawsWholeAdd / commMonoidLawsWholeMul : CommMonoidLaws over
  them, assembled from addWAssoc/addWComm/addWZeroL/R and
  mulWAssoc/mulWComm/mulWOneL/R.

Whole has no Group (no additive inverses - that is Int's rung, deferred
with the Int law campaign).

## Decision 5: chapter + tests (doctrine flip applied)

- Chapter ch571 (next free): the full ladder - six ops shapes, ten law
  records, the bridge edges with transports, Whole's lower-rung
  positions; self-contained, mirrors the prelude byte-identically for
  shared definitions, runnable witness.
- Collision audit gains Magma and Group (and keeps the renamed list
  green).
- Presence tests for the new law records, bridges, transports, and the
  Whole monoid values.
- REPL pins unchanged: `"a" ++ "b"`, mempty behavior, all 1a pins.

## Non-goals

- No Int/Frac law instances (still the deferred campaigns; when Int's
  RingLaws lands, groupLawsOfRing instantly yields Int's AbGroup - the
  bridge is the point).
- No Quasigroup/Loop/left-right-unital variants (no consumer; the six
  shapes cover the tower's needs through Field).
- No instance-table entries for Magma/Monoid/Group beyond the existing
  Bytes/String append instances.
- No kernel/codegen changes; prelude + listings + session/repl tests only.

## Testing summary

Failing-first presence tests per task; collision audit extended; full
`go test -timeout 30m ./...` before tag; REPL append + arithmetic pins.
