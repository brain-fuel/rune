# v4 Int law campaign: RingLaws for a junk-free Int: Design

Date: 2026-07-06
Status: Author-directed ("Int/Frac law campaigns"); Int first, Frac follows
as its own spec (see the fore-note at the end).
Parent: 2026-07-06-v4-tower-hierarchy-design.md (Decision 4 deferred this)
and 2026-07-06-v4-full-ladder-design.md (whose bridges this campaign cashes).
Seed: main @ v3.374.0.

## Premise

Int's Ring OPS instance shipped in sub-project 1a; its laws were deferred
because sign-magnitude proofs are a case-theory campaign. Investigating the
transport route exposed a representation defect worth fixing first: the
current `data Int is int : Bool -> Whole -> Int` admits the junk value
`int true zero` (negative zero), which every op avoids PRODUCING (mkInt
normalizes) but which inhabits the type. Laws quantify over ALL Int, and
junk is outside the normalized image, so every transported law would need a
per-variable junk-peeling frame (~3 extra lemmas per law, ~39 total).

Backwards compatibility is not a goal (v4 policy). The right fix is the
representation.

## Decision 1: re-found Int junk-free

```
data Int : U is
  nonneg  : Whole -> Int      -- nonneg n  denotes  n
| negsucc : Whole -> Int      -- negsucc k denotes  -(k+1)
end
```

Every integer has EXACTLY one representative; there is no normalization
function and no junk. mkInt is DELETED (Rule 5: nothing to normalize).
Consequences swept in the same task:

- Ops rewritten over the two constructors, public names unchanged: intOf
  (= nonneg), ineg, iadd, isub, imul, and the promotion wsub. wsub becomes
  total without normalization: `case leb b a of true -> nonneg (subW a b)
  | false -> negsucc (subW b (succ a))` (b - a - 1 when a < b).
- isign/imag survive as DERIVED accessors (isign (nonneg _) = false,
  isign (negsucc _) = true; imag (nonneg n) = n, imag (negsucc k) =
  succ k) - divIF, encodeInt, and the Frac interplay keep working through
  them with one definition site each.
- Binary codec (encodeInt/parseInt) adapted; roundInt roundtrip is the
  gate. Show via encodeInt unchanged in OUTPUT ("-3" still prints "-3").
- Instances keep their names and types: semiringInt/ringInt (zero =
  nonneg zero, one = nonneg (succ zero)), negInt/subInt/divInt.
- `builtin int Int intOf` re-registers against the new intOf.
- Tower graph restated: mkIntFalse is deleted with mkInt; intOfAdd/
  intOfMul/intOfOne/ratOfIntOf are restated and EXPECTED TO BECOME refl
  (iadd (nonneg a) (nonneg b) computes to nonneg (addW a b) directly).
- REPL pins unchanged as STRINGS: `2 - 5` still `-3 : Int`, `-3 + 1`
  still `-2 : Int`, `-1/3` still Frac.

## Decision 2: the difference-pair transport (ZW model)

Laws are proven on the model where they are trivial, then transported:

- ZW = pairs of Wholes, (a, b) denoting a - b (ch107's construction over
  the prelude's builtin Whole; no quotient machinery needed because we
  transport along a RESPECT lemma instead of forming the quotient).
- The keystone RESPECT lemma:
  `wsubEq : (a b c d : Whole) -> Eq Whole (addW a d) (addW c b) ->
   Eq Int (wsub a b) (wsub c d)` - wsub identifies cross-equal
  differences. Its proof is the concentrated case theory (leb cases +
  the monus library below), done ONCE.
- Op SPEC lemmas rewrite each Int op as a wsub of Whole arithmetic:
  `iaddSpec : (x y : Int) -> Eq Int (iadd x y)
     (wsub (addW (posP x) (posP y)) (addW (negP x) (negP y)))`
  `imulSpec`, `inegSpec` likewise, where posP/negP are the evident
  ZW components (posP (nonneg n) = n, negP (nonneg n) = zero;
  posP (negsucc k) = zero, negP (negsucc k) = succ k).
  Each is a small constructor-case proof (4 cases for binary ops)
  discharging through wsubEq plus Whole-semiring shuffling.
- The retraction is EXACT (the payoff of Decision 1):
  `wsubRep : (x : Int) -> Eq Int (wsub (posP x) (negP x)) x` - both
  constructor cases compute (no junk exception).
- Each ring law then follows the same pipeline: rewrite both sides by the
  spec lemmas, land in wsub-of-Whole-expressions, close with wsubEq and
  the SHIPPED Whole semiring lemmas (addWAssoc/addWComm/distribWL/R/...)
  plus a small set of new 4-term shuffle helpers.

## Decision 3: the monus/order library (new Whole lemmas)

The wsubEq proof needs a bounded, reusable library over leb/subW/addW
(statements fixed at plan time; all by WholeElim induction, guided by the
shipped subWZeroL/subWSucc/lebEquiv proofs):

- lebComplete : leb a b = false implies leb b a = true (totality)
- subWAddCancel : leb b a = true implies addW b (subW a b) = a
- addWCancelL : addW k a = addW k b implies a = b
- plus the addW-rearrangement helpers the spec proofs demand (in the
  addWSwapR/addWShiftSucc family already shipped).

These are permanent tower assets (the Frac campaign and Ord will reuse
them), not scaffolding.

## Decision 4: the deliverable law values

- ringLawsInt : RingLaws Int ringInt (the 13 slots: 11 semiring + 2 neg-inv)
- commLawsInt : CommLaws Int semiringInt (imul commutes)
- commRingLawsInt : CommRingLaws Int ringInt (assembled by mkAnd - the 1b
  bundle's first non-Whole inhabitant)
- abGroupLawsInt : AbGroupLaws Int (groupOfRing Int ringInt), obtained by
  `abGroupLawsOfRing Int ringInt ringLawsInt` - the 1b bridge cashed, zero
  new proof. (Int, +, 0, neg) is a machine-checked abelian group.

All plain defs; no new instance-table entries.

## Decision 5: chapters + tests

- ch570 (tower graph) UPDATED to mirror the new Int (mirror doctrine; its
  lemmas simplify with the representation).
- NEW ch572: the Int campaign chapter - the junk-free representation
  argument (why negative zero poisons transport), the ZW model, wsubEq,
  the spec-lemma pipeline, the law values, and the bridge payoff
  (abGroupLawsInt). Self-contained, runnable witness.
- Presence tests for the law values and the library keystones; the
  collision audit is unaffected (Int's shape change does not touch class
  shapes). REPL pins (strings unchanged) + roundInt roundtrip + full suite.

## Non-goals

- Frac laws (own spec, below). Ord. New types. Kernel changes (data
  declarations are ordinary surface; no core constructor is added, no
  hash-format bump).
- No quotient-type machinery: the respect-lemma transport avoids Quot
  entirely.

## Fore-note: the Frac campaign (next spec, design open)

Frac's ops reduce through gcd, and the prelude's gcd is FUEL-based Euclid
over the accelerated `//`/`%` (gcdF, fuel = succ b), so direct gcd theory
means reasoning over fueled recursion. Two candidate routes for the next
spec, to be decided after Int ships:
(a) canonical-form theory: raw-pair laws (easy semiring algebra) + reduce
    respects cross-equality (gcd divides both + division cancellation) +
    lowest-terms uniqueness (coprimality; the hard number theory), or
(b) representation switch to the kernel's quotient machinery (Quot/qin/
    qlift; ch116/ch203 prove the full field there and port), trading proof
    difficulty for integration churn (display via a canonical-form
    function, codec, instance dispatch).
The Int campaign's monus/order library is prerequisite to both.

## Testing summary

Failing-first presence tests per task; roundInt roundtrip; REPL pins;
listings gates for ch570 (updated) + ch572 (new); full
`go test -timeout 30m ./...` before tag.
