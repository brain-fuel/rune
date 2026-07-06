# v4 sub-project 1: tower graph + lawful class hierarchy: Design

Date: 2026-07-06
Status: Draft for author review (laws-split decision author-approved in
session; ladder depth and remaining decisions controller-recommended while
author AFK, flagged below).
Parent: docs/superpowers/plans/2026-07-06-three-majors-roadmap.md (v4).
Seed: main @ 8021e37.

## Premise

v4 rebuilds the numeric tower as a graph of types under a lawful algebraic
class hierarchy. Today the prelude has one Σ-record class `Num` bundling
`+`/`*` per closed level, result-typed promotion classes for `-`/`/`, and no
law-carrying instances. IEEE754 is coming (later sub-spec) and forces the
central design question: floats can HAVE ring operations but can never PROVE
associativity, so proven and guarded instances must differ somewhere
structural.

Backwards compatibility is explicitly not a goal (author directive,
three-majors roadmap). The doctrine flip is live: every feature here ships
with its book chapter.

## Decision 1: split ops classes from laws classes (author-approved)

- Ops classes carry operations only. EVERY tower type gets ops instances,
  including IEEE754 later.
- Laws classes are separate proof records parameterized by an ops instance.
  Only exact types get laws instances.
- The guarded tier IS the absence of a laws record, plus Go-level property
  gates (differential/property tests) recorded where the instance is
  defined. An instance page states its tier.
- Rejected: evidence-sum fields in one record (proven consumers must unwrap
  everywhere); laws bundled with floats excluded (floats fall out of generic
  code, fails the tower/graph goal).

## Decision 2: minimal 3-rung ops ladder (controller-recommended, author AFK)

```
Semiring A = { add : A -> A -> A, zero : A, mul : A -> A -> A, one : A }
Ring     A = { semiring : Semiring A, neg : A -> A }
DivRing  A = { ring : Ring A, recip : A -> A }
```

- Nesting gives superclass access by projection (a Ring instance yields its
  Semiring by field access); no separate coercion machinery.
- `recip` is TOTAL with the junk value `recip zero = zero` (the
  Lean/Mathlib convention). The true inverse law lives laws-side, guarded
  by a nonzero hypothesis. This keeps ops records proof-free, which the
  split design requires.
- Commutativity is a LAW, so it never appears ops-side: field vs division
  ring (Quaternion later is noncommutative) differ only in which laws
  records exist. One DivRing ops shape serves both.
- Existing `Semigroup`/`Monoid` (`++`, `mempty`) stay as they are; they
  serve the append family, not arithmetic.
- Hash-collision discipline unchanged from `Num`: a Σ-record is
  distinguished by field POSITION, and any other class with an identical
  field-type sequence would collide. The spec accepts this with the same
  documented discipline (comment at each class stating its shape), plus a
  new collision audit test (Decision 7).
- Rejected: full Magma-to-Field ladder (many near-identical record shapes,
  maximal collision surface, heavy per-type plumbing); flat per-type
  bundles (no generic code over Ring, fails the graph goal).

## Decision 3: laws records

```
SemiringLaws A (s : Semiring A) = {
  addAssoc, addComm, addZeroL, addZeroR,
  mulAssoc, mulOneL, mulOneR,
  distribL, distribR, mulZeroL, mulZeroR }
RingLaws A (r : Ring A) = { semiringLaws : SemiringLaws A (projection), negAddInv }
DivRingLaws A (d : DivRing A) = { ringLaws : RingLaws A (projection),
  recipInvL/R : (x : A) -> Neq x zero -> Eq A (mul x (recip x)) one,
  oneNeqZero }
CommLaws A (s : Semiring A) = { mulComm }
```

- Exact statements (argument orders, Eq shapes) are fixed at plan time to
  match the existing listing lemmas so proofs port rather than re-prove.
- `CommLaws` stands alone so "commutative ring" = Ring + RingLaws +
  CommLaws and "field" = DivRing + DivRingLaws + CommLaws by composition,
  with no extra ops shapes.
- addComm stays INSIDE SemiringLaws (standard semiring: additive
  commutative monoid); only mulComm is split out.

## Decision 4: instances in this sub-project (existing types only)

- Whole: Semiring + SemiringLaws (port/reference the existing arithmetic
  lemmas from the book listings).
- Int: Ring + RingLaws + CommLaws (ch107 int_ring_complete already proves
  the ring axioms; port).
- Frac: DivRing + DivRingLaws + CommLaws (a field).
- Nat (counting, no zero): NO semiring instance; it remains a graph node
  injected into Whole by wholeOf. Its arithmetic stays as today.
- Later sub-specs slot in without hierarchy changes: IEEE754 = DivRing ops
  with no laws records (guarded tier); Word types = Ring (mod 2^n) with
  proven-or-guarded laws per representation choice; Decimal = Ring + partial
  div; Complex A / Quaternion A = parameterized DivRing over a
  CommLaws-carrying base, Quaternion omitting CommLaws.

## Decision 5: operator rewiring + Num deletion

- `+` and `*` become Semiring-constrained functions (dispatch identical in
  kind to today's Num dispatch; the elaborator machinery is unchanged).
- `Num` is DELETED, not deprecated (Rule 5: subsumed by Semiring). All
  listings and prelude code using Num migrate in place, no compat shim.
- Promotion classes (`Sub`/`Div`/`NegR` result-typed) are UNTOUCHED: they
  solve cross-type promotion, not within-type closure. `-` still promotes
  Whole to Int; `/` still promotes to Frac.
- Whole keeps `subW`-based monus and `// %` as plain functions, as today.
- REPL acceptance (mandatory, memory rule): `1 + 1`, `1/3 + 2/3`, `2 - 5`,
  `4000 * 4000` behave exactly as before, accels still fire, repl tests
  extended to pin dispatch-through-Semiring.

## Decision 6: the tower graph (conversions with homomorphism proofs)

- Named injection functions per edge (no Into typeclass; one-field classes
  are a hash-collision trap and nothing needs the abstraction yet):
  wholeOf (Nat to Whole, exists), intOfWhole, ratOfInt, ratOfWhole.
- Per edge, homomorphism lemmas: preserves add, preserves mul, preserves
  one (and zero where the source has it).
- Coherence: ratOfWhole agrees with ratOfInt composed with intOfWhole
  (propositional lemma if not definitional).
- The graph is documentation-real: a diagram in the chapter naming every
  node, edge, and lemma.

## Decision 7: tests and gates

- Collision audit test: register every class in a fresh session, assert
  pairwise-distinct hashes (turns the positional discipline from a comment
  into a gate).
- Session/typeclass tests for each new instance and the operator dispatch.
- REPL tests per Decision 5.
- Listings gates: the two new chapters enter listings_test.
- Full `go test -timeout 30m ./...` before any tag, as always.

## Decision 8: chapters (doctrine flip applied)

Two new listings, numbers chosen at plan time (next free):
- Hierarchy chapter: Semiring/Ring/DivRing + laws records, instances for
  Whole/Int/Frac, generic code demo (e.g. sum over any Semiring).
- Tower-graph chapter: the injections, homomorphism lemmas, coherence
  triangle, and the graph diagram.
The prelude remains the REPL's source of truth; chapters mirror it (same
definitions, book narration), consistent with the existing ch11 pattern.

## Non-goals (this sub-project)

- Ord/comparison classes (arrive with the Real/IEEE754 sub-spec, where
  ordered-field compatibility is first needed).
- Any new types (Word, Decimal, IEEE754, Real, Complex, Quaternion).
- Literal syntax extensions, FloatLit, the hash-format bump.
- REPL compiled-eval mode, backend-parity REPL work, perf conformance gate.
- Kernel/core changes of any kind: this sub-project is prelude + listings +
  session tests only. The kernel stays frozen.

## Testing summary

New: collision audit, instance/dispatch session tests, REPL dispatch pins,
two listings gates. Re-run: full suite before tag. Acceptance: REPL
arithmetic unchanged on the pinned expressions; ch107-family proofs port
without weakening statements.
