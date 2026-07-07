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

SUPERSEDED 2026-07-06 by author directive: the full Magma-to-Field ladder
ships via 2026-07-06-v4-full-ladder-design.md (the 3 rungs remain as its
upper ops shapes).

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
RingLaws A (r : Ring A) = { semiringLaws : SemiringLaws A (projection), negAddInvL/R }
DivRingLaws A (d : DivRing A) (nz : A -> U) = { ringLaws : RingLaws A (projection),
  recipInvL/R : (x : A) -> nz x -> Eq A (mul x (recip x)) one }
CommLaws A (s : Semiring A) = { mulComm }
```

- The laws bodies are built from REUSABLE statement formers (AssocT, CommT,
  IdLT/IdRT, DistribLT/DistribRT, AnnihLT/AnnihRT) chained with a
  non-dependent `And` (a Sig with a constant family), so each law is one
  named proposition and the records stay readable.
- DivRingLaws is parameterized by a nonzero PREDICATE `nz : A -> U` rather
  than a global Neq: the prelude has no Empty/negation encoding, each
  carrier states nonzeroness its own way (Frac: the numerator's isZero is
  false), and the guard is exactly what the law needs. oneNeqZero is
  DROPPED for now (needs a negation encoding, no consumer yet; returns
  with the Real/IEEE754 sub-spec).
- Exact statements (argument orders, Eq shapes) follow the actual prelude
  kernels (addW/mulW recurse on the FIRST argument via `case`, mulW steps
  by `addW ih n`), with ch107's inductions as the shape guide. ch107
  proves over its OWN data Nat, so the Whole proofs are re-derived with
  the same induction shapes, not literally ported.
- STAGING (realism): the law-record TYPES all land in this sub-project,
  and Whole's SemiringLaws instance is PROVEN here. The Int RingLaws and
  Frac DivRingLaws INSTANCES are their own follow-up proof campaigns
  (sign-magnitude case theory for iadd; gcd/reduce theory for Frac) and
  ship as separate sub-specs. Ops instances for all three types land now.
- `CommLaws` stands alone so "commutative ring" = Ring + RingLaws +
  CommLaws and "field" = DivRing + DivRingLaws + CommLaws by composition,
  with no extra ops shapes.
- addComm stays INSIDE SemiringLaws (standard semiring: additive
  commutative monoid); only mulComm is split out.

## Decision 4: instances in this sub-project (existing types only)

- Whole: Semiring + SemiringLaws PROVEN (re-derived over addW/mulW by
  WholeElim, ch107-shaped inductions).
- Int: Ring ops instance now; RingLaws + CommLaws instances deferred
  (follow-up sub-spec; ch107 proves the ring axioms over a quotient
  representation, not the prelude's sign-magnitude Int, so the proofs do
  not port directly).
- Frac: DivRing ops instance now (recip total, `recip 0 = 0/1` junk via
  numerator case); DivRingLaws + CommLaws instances deferred (gcd/reduce
  theory campaign).
- Nat (counting, no zero): NO semiring instance; it remains a graph node
  injected into Whole by wholeOf. Its arithmetic stays as today.
- Later sub-specs slot in without hierarchy changes: IEEE754 = DivRing ops
  with no laws records (guarded tier); Word types = Ring (mod 2^n) with
  proven-or-guarded laws per representation choice; Decimal = Ring + partial
  div; Complex A / Quaternion A = parameterized DivRing over a
  CommLaws-carrying base, Quaternion omitting CommLaws.

## Decision 5: operator rewiring + Num deletion

- `+` and `*` become Semiring-constrained functions (dispatch identical in
  kind to today's Num dispatch; the elaborator machinery is unchanged; no
  hardcoded Num references exist in elaborate/ or internal/session, verified).
- ONLY Semiring instances enter the instance table (semiringWhole/Int/Frac,
  the Int and Frac ones by projection from named ringInt/divRingFrac
  defs). Ring/DivRing values stay plain defs until a class-constrained
  consumer exists (YAGNI; registering them buys nothing today).
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
  wholeOf (Nat to Whole, exists), intOf (Whole to Int, exists), fracOf
  (Whole to Frac, exists), ratOfInt (Int to Frac, NEW: keep sign and
  magnitude over denominator 1).
- intOf edge homomorphism lemmas PROVEN now: preserves add, mul, one
  (each reduces to the `mkInt false m = int false m` normalization lemma,
  a two-branch case).
- fracOf/ratOfInt edge homomorphism lemmas DEFERRED with the Frac laws
  campaign: addF/mulF reduce through gcd, so the lemmas need the same
  gcd/reduce theory.
- Coherence triangle PROVEN now: ratOfInt (intOf n) = fracOf n is
  definitional (both sides are `frac false n 1`), pinned by refl.
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
