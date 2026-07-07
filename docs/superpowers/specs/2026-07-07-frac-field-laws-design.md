# v4 Frac law campaign: FieldLaws on a quotient Frac: Design

Date: 2026-07-07
Status: Author-directed ("Frac campaign"); quotient route per the recorded
recommendation (Int campaign final report), proceeding.
Parent: 2026-07-06-v4-tower-hierarchy-design.md (Decision 4 deferred Frac
laws); consumes 2026-07-06-int-ring-laws-design.md (ringLawsInt is the
algebra engine) and the 1b ladder (FieldLaws/DivRingLaws bundles).
Seed: main @ v3.375.0.

## Premise and the route decision

Frac's laws were deferred because its ops reduce through gcd, and gcd is
fuel-based Euclid over the accelerated `%` - direct canonical-form theory
needs gcd divisibility PLUS lowest-terms uniqueness (coprimality), real
number theory over fueled recursion.

The quotient route dissolves this: re-found Frac as a QUOTIENT of raw
pairs (the kernel's v2 Quot/qin/qsound/qlift machinery, ι-computing,
codegen-supported since v2.0.0), with ops on representatives UNREDUCED.
Then:

- Respect proofs are pure Int ring algebra - and ringLawsInt (v3.375.0)
  is exactly the required engine. No gcd fact is ever proven.
- Canonical DISPLAY is computational, not propositional: the REPL fold
  reduces in Go; deployed Show reduces via the rune `gcd` as a plain
  function. reduce is never the subject of a proof.
- The field laws port from ch113/ch116/ch203, which prove the complete
  commutative field over exactly this quotient shape. Our position is
  STRICTLY better than those chapters': their integer layer was itself a
  quotient (forcing the zIsZero ZRel-respect machinery, ch116's hard
  part); our Int is canonical data, so its zero-test is a direct case
  with NO respect obligation. That whole layer evaporates.

The cost is integration churn (representation switch), the same trade the
Int campaign just made profitably. Backwards compat is a non-goal (v4).

## Decision 1: the representation

```
data QPair : U is qpair : Int -> Whole -> QPair end   -- num, den = succ d (POSITIVE by construction)
QRel : QPair -> QPair -> U   -- cross-multiplication over Int:
--   QRel (qpair i1 d1) (qpair i2 d2) = Eq Int (imul i1 (qden (qpair i2 d2))) (imul i2 (qden (qpair i1 d1)))
--   where qden (qpair _ d) = nonneg (succ d)  (the denominator as a positive Int)
Frac : U is Quot QPair QRel end
```

- Sign lives in the Int numerator (junk-free); the denominator is
  positive BY CONSTRUCTION (succ d) - no zero-denominator state exists.
- The old `data Frac is frac : Bool -> Whole -> Whole` is DELETED
  (Rule 5) along with fneg/fnum/fden/reduce-as-law-bearer; `fracOf n =
  qin QPair QRel (qpair (nonneg n) zero)` keeps `builtin rat Frac fracOf`
  valid (the registration validates by TYPE, Nat -> Frac, verified).

## Decision 2: ops on representatives, UNREDUCED, via qlift

- Raw pair ops (num/den arithmetic over imul/iadd, cross-multiplication
  addition, componentwise multiplication, numerator negation, reciprocal
  by swapping with sign transfer and the ZERO-NUMERATOR junk case giving
  0/1 - recip total, the ladder convention).
- Lifted to Frac by the ch113 double-qlift pattern; each lift carries its
  respect proof, whose content is ringLawsInt algebra (assoc/comm/
  distrib/cancellation via the Int lemmas; addWCancelL-style cancellation
  at the Int layer comes from the transport toolkit where needed).
- Division: divF x y = mulF x (recipF y); divWF/divIF re-derived on top.
  NO reduce anywhere in the ops.
- Int-level cancellation (imulCancelL : imul k a = imul k b -> a = b for
  k with nonzero... NOT needed: the ch113 respect proofs avoid
  cancellation by cross-multiplication rearrangement only. If a proof
  turns out to demand cancellation, STOP and re-scope - report first.)

## Decision 3: canonical display and codec, computationally

- REPL: the Go DecConfig Frac fold is REWRITTEN to match the quotient
  normal form `qin QPair QRel (qpair i d)` (qin is a builtin head with a
  known hash; qpair's hash joins DecConfig), reduce num/den by Go gcd,
  and print the canonical sign + n/d (or whole-valued n, or 0) - the
  same output strings as today. Same DecConfig discipline as the Int
  campaign's licensed display change.
- Deployed/Show: encodeFrac applies a rune-side computational reduceQ
  (gcd as the existing fuel-based function, used as a FUNCTION, never in
  a proof) to the representative, then prints; parseFrac builds a qin.
  Round-trip gate stays.
- to_radix: ported from ch203's toRadixQ (qlift + toRadixRespects); the
  long-division core stays representative-level. Output type RDec and
  all display strings unchanged.

## Decision 4: the law values (the campaign's point)

Ported from ch113/ch116/ch203 shapes onto our quotient (proof by qind to
representatives + qsound of cross-multiplication ring identities, all
closed by ringLawsInt lemmas):

- semiringLawsFrac : SemiringLaws Frac semiringFrac (11 slots)
- ringLawsFrac : RingLaws Frac ringFrac (+ neg-inv pair)
- commLawsFrac : CommLaws Frac semiringFrac
- divRingLawsFrac : DivRingLaws Frac divRingFrac nzFrac, where
  nzFrac x = the numerator's zero-test is false (stated via the direct
  Int zero-test - no respect machinery, Int is canonical; the predicate
  is the nz : A -> U parameter the 1b bundle anticipated)
- fieldLawsFrac : FieldLaws Frac divRingFrac nzFrac (the 1b bundle's
  first inhabitant - THE deliverable)
- abGroupLawsFrac via the bridge (one line, as for Int)
- The recip inverse laws are guarded by nzFrac (recip is total with junk
  at zero; the laws only speak where nz holds), exactly the DivRingLaws
  shape shipped in 1a.

## Decision 5: load-budget contingency (the PARKING-LOT question)

The Int campaign added ~26s to the repl suite via per-load elaboration.
This campaign adds a comparable-or-larger proof corpus. HARD GATE in the
first plan: measure single-prelude-load wall time before and after the
representation switch, and again after the laws land. Budget: single
`rune repl` cold start must stay under 3s. If the laws blow it, the
CONTINGENCY is pre-approved: the law VALUES move to a separate
`internal/prelude/laws.rune` loaded by tests and importable on demand,
with the prelude keeping ops + instances + light lemmas. Record whichever
outcome in the spec status and PARKING-LOT.

## Decision 6: chapters + tests (doctrine)

- NEW ch573: the quotient Frac - the route argument (why quotient beats
  gcd theory; why canonical Int made it cheap), the representation, one
  lift + its respect proof in full, the law-port story, honest scope
  notes. Runnable witness.
- ch569/ch571 are UNAFFECTED (they never touch Frac). Any listing
  mirroring the OLD Frac must be realigned - sweep and report (ch570
  mirrors fracOf/ratOfInt: update its Frac block).
- REPL pins unchanged as strings: `1/3`, `-1/3`, `1/3 + 2/3` -> 1,
  `(1/2) * (2/3)` -> 1/3, `1.3` -> 13/10, `2 - 5` composes, to_radix
  pins (`1/3 |> to_radix` -> 0.{3}), roundFrac round-trip.
- Presence tests per task; full suite before each tag.

## Non-goals

- No gcd/coprimality proofs of any kind. No Ord. No kernel changes (Quot
  is shipped v2 machinery; no new core constructors, no hash-format
  bump). No new instance-table classes.
- Quaternion/Complex/IEEE754 still later sub-specs (Frac's field laws
  are their parameterized base's first proven instance).

## Sequencing: two plans

- Plan A (re-foundation): QPair/QRel/Frac + unreduced qlift ops + all
  respect proofs + display/codec/to_radix + instance rewiring + pins
  green + load measurement. Tag on green.
- Plan B (laws): the six law values + ch573 + ch570 Frac realignment +
  load-budget decision + bookkeeping. Tag on green.

## Testing summary

Failing-first presence tests; REPL pin battery (arithmetic, decimal
literals, to_radix, negation); roundFrac; listings gates; two full
`go test -timeout 30m ./...` runs (one per plan) before tags.
