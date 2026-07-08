# Frac Field Laws (Plan B) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Prove the six law values over the quotient Frac (semiring, ring, comm, divRing, FIELD, abGroup) - FieldLaws' first inhabitant, the campaign's deliverable - plus the deferred frac-edge homomorphisms, ch573, and the load-budget decision.

**Architecture:** Every law is proven representative-level (a QRel cross-multiplication identity over Int, closed by ringLawsInt's named lemmas) then lifted to Frac by qind + qsound. No gcd fact is ever proven (spec hard line). The one genuinely new proof is the GENERAL recip inverse law: ch113/ch116 only closed the positive subtype (their Z was itself a quotient, sign normalization open); our canonical Int closes the general case by numerator-constructor cases.

**Tech Stack:** rune prelude proofs (internal/prelude/prelude.rune); Go tests (internal/session, internal/repl); port sources listings/ch113_rational_field.rune + ch116_rational_field_complete.rune.

## Global Constraints

- Spec: docs/superpowers/specs/2026-07-07-frac-field-laws-design.md (Decisions 4, 5, 6).
- Kernel FROZEN: no changes under core/, store/, elaborate/. Prelude + listings + Go tests only.
- NO gcd/coprimality/lowest-terms proofs of any kind. If a proof turns out to demand one, STOP and report; do not invent number theory.
- Int-level multiplicative CANCELLATION (imul k a = imul k b -> a = b) is expected NOT to be needed for the ring laws (cross-multiplication rearrangement suffices, per spec Decision 2). Task 1's zero-product lemma is the narrow exception and is scoped there explicitly. If any OTHER proof demands cancellation, STOP and report.
- Statements must NOT be weakened, permuted, or specialized to make a proof go through. The statement types below are the contract.
- NO em-dashes or en-dashes anywhere (comments, docs, commit messages). ASCII hyphens only.
- rune gotchas: every `fn ... is` needs its own `end`; case arms evaluate eagerly (recursive ih in exactly one arm; precompute predicates with let); seq breaks on multi-line RHS (use nested let ... in); prelude elaboration IS the proof check - if the prelude loads, the proofs hold.
- Positional Sigma-record discipline: law records are And-chains; assembly values follow the exact mkAnd nesting shape of semiringLawsInt (prelude.rune:2267) verbatim.
- Conventional Commits; every commit message ends with the line: Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>
- Work in the dedicated worktree .worktrees/feat-frac-laws (branch feat/frac-laws off main @ v3.376.0). Commit with explicit pathspecs only.
- Full `go test -timeout 30m ./...` before the tag (Task 7), plus per-task `go test ./internal/session/ ./internal/repl/ -count=1` gates.

## Existing material the tasks consume (all in internal/prelude/prelude.rune, main @ v3.376.0)

- Quotient Frac: `Frac = Quot QPair QRel` (~2377); `QPair`/`qpair : Int -> Whole -> QPair`; accessors `qnum`/`qdpred` (2381-2382), `qden p = nonneg (succ (qdpred p))` (2385); `QRel p q = Eq Int (imul (qnum p) (qden q)) (imul (qnum q) (qden p))` (2390, Prop-valued).
- Raw ops: `rawMul` (2417, componentwise), `rawAdd` (2422, cross terms), `rawNeg` (2427), `rawRecipOn`/`rawRecip` (2432/2444: numerator nonneg zero -> junk 0/1; nonneg (succ k) -> (d+1)/(k+1); negsucc dual).
- Lifted ops: `mulF` (2571), `addF` (2693), `fnegate` (2763), `recipF` (2937), `divF` (2948), each via double qlift with respect proofs `mulRespL/R` (2527/2496), `addRespL/R` (2640/2587).
- Ops instances: `divRingFrac : DivRing Frac` (3026); `semiringFrac = divRingFrac.1.1` (instance, 3032); the Ring value is `divRingFrac.1` (no named ringFrac; use the projection inline).
- Law records: `SemiringLaws` (592, 11-slot And-chain), `RingLaws` (946), `DivRingLaws` (957, nz-predicate param), `CommLaws` (968), `AbGroupLaws` (1035), `CommRingLaws` (1042), `FieldLaws` (1045). Statement formers AssocT/CommT/IdLT/IdRT/DistribLT/DistribRT/AnnihLT/AnnihRT/NegInvLT/NegInvRT/RecipInvLT/RecipInvRT.
- Bridge: `abGroupLawsOfRing : (A : U) -> (r : Ring A) -> RingLaws A r -> AbGroupLaws A (groupOfRing A r)` (the 1b payoff; grep `abGroupLawsOfRing`).
- Int engine: `semiringLawsInt` (2267) and its ingredient lemmas by name - iaddAssoc, iaddComm, iaddZeroL/R, imulAssoc, imulOneL/R, idistribL/R, imulZeroL/R, imulComm, inegInvL/R - plus the transport toolkit (iaddSpec/imulSpec/inegSpec/wsubEq etc.) and the monus/order library. These close every cross-multiplication identity.
- Quotient kernel machinery: `qin`, `qsound A R a b (h : R a b) : Eq (Quot A R) (qin A R a) (qin A R b)`, `qind` (Prop-valued motive; `nz x -> Eq ...` is a Prop by impredicativity), `qlift`.
- Eq combinators: both the explicit-arg congE/symEq/transEq family and the implicit-arg cong/sym/trans/cong2 combinators added in Plan A Task 2 (grep for their defs before use; do NOT add a third family).
- Port sources: listings/ch113_rational_field.rune (qmulCommP 1058, qaddCommP 1268, qmulOneRP 1392, qaddZeroP 1420, qmulAssocP 1454, qaddAssocNum/qaddAssocP 1513/1546, qmulDistRP 1683, qmulInvPosP/R 1806/1826) and ch116 (same names, later lines). CRITICAL port note: their integer layer Z is itself a quotient of Nat-pairs, so every Z-step there is a ZRel-respect dance; our Int is canonical data, so those steps collapse to direct ringLawsInt lemma applications. Port the SHAPE of the cross-multiplication algebra, not the Z plumbing.

## File Structure

- Modify: `internal/prelude/prelude.rune` - all new defs go in a clearly-bannered FRAC LAWS section placed AFTER divRingFrac/semiringFrac (3032) and BEFORE the tower-graph section, except Task 1's Int support lemmas which join the Int lemma area (before the Frac section) so the Frac code can see them (top-down name resolution).
- Modify: `internal/session/tower_hash_test.go` - presence tests (accumulating pattern; see TestIntLawsPresent for the style).
- Modify: `listings/ch570_tower_graph.rune` - Frac-block realignment + the new edge homomorphisms mirrored (Task 6).
- Create: `listings/ch573_frac_field_laws.rune` - the chapter (Task 6).
- Modify (Task 7): spec status, PARKING-LOT.md, roadmap, this plan's checkboxes.
- Contingency file (Task 7, only if the 3s budget blows; pre-approved by spec Decision 5): `internal/prelude/laws.rune` split.

---

### Task 1: nzFrac - the lifted zero-test and its Int support lemmas

**Files:**
- Modify: `internal/prelude/prelude.rune`
- Test: `internal/session/tower_hash_test.go`

**Interfaces:**
- Consumes: junk-free Int (nonneg/negsucc), imul, Whole kernels (mulW/addW), the quotient Frac layer above.
- Produces (later tasks rely on these exact names and types):
  - `isZeroInt : Int -> Bool` (nonneg zero -> true; nonneg (succ _) -> false; negsucc _ -> false)
  - `nonnegInj : (a : Whole) -> (b : Whole) -> Eq Int (nonneg a) (nonneg b) -> Eq Whole a b`
  - `intTagDisc : (k : Whole) -> (n : Whole) -> Eq Int (negsucc k) (nonneg n) -> Eq Bool true false` (constructor discrimination, consumed via the existing boolDiscTF ex-falso idiom)
  - `addWPosR : (x : Whole) -> (d : Whole) -> Eq Bool (isZero (addW x (succ d))) false` (or the equivalent succ-headedness statement; check the monus/order library first - an equivalent may already exist, in which case consume it and do NOT duplicate)
  - `mulWPosZero : (n : Whole) -> (d : Whole) -> Eq Whole (mulW n (succ d)) zero -> Eq Whole n zero`
  - `imulPosZero : (a : Int) -> (d : Whole) -> Eq Int (imul a (nonneg (succ d))) (nonneg zero) -> Eq Int a (nonneg zero)` (the zero-product lemma; the ONE sanctioned cancellation-adjacent fact, scoped to a positive right factor)
  - `isZeroResp : (p : QPair) -> (q : QPair) -> QRel p q -> Eq Bool (isZeroInt (qnum p)) (isZeroInt (qnum q))`
  - `isZeroF : Frac -> Bool is qlift QPair QRel Bool (fn (p : QPair) is isZeroInt (qnum p) end) isZeroResp end` (adjust the exact qlift argument spelling to match addF's usage)
  - `nzFrac : Frac -> U is fn (x : Frac) is Eq Bool (isZeroF x) false end end`

Proof sketches (the implementer derives the bodies; statements are the contract):
- `imulPosZero` by case on a. nonneg n: imul computes to nonneg (mulW n (succ d)); nonnegInj then mulWPosZero, close with cong nonneg. negsucc k: imul computes through ineg to negsucc-headed; the hypothesis is then intTagDisc's shape; ex falso via boolDiscTF (subst along Eq Bool true false with a Bool-case motive picking the goal in the true arm) - mirror the existing uses, grep boolDiscTF.
- `isZeroResp`: both directions from imulPosZero. If isZeroInt (qnum p) = true then qnum p = nonneg zero (case-inversion helper: isZeroInt a = true -> Eq Int a (nonneg zero), by case on a; name it `isZeroIntSound`), so imul (qnum q) (qden p) = imul (qnum p) (qden q) = nonneg zero (imulZeroL after rewriting), qden p = nonneg (succ (qdpred p)) so imulPosZero gives qnum q = nonneg zero. Symmetric for the other direction. Assemble the Bool equality by case on both isZeroInt values (four cases, two closed by the transports, two refl).

- [ ] **Step 1:** Write failing presence test `TestFracNzPresent` in internal/session/tower_hash_test.go asserting the session resolves: isZeroInt, nonnegInj, imulPosZero, isZeroResp, isZeroF, nzFrac (copy the TestIntLawsPresent structure: fresh session, LoadSource prelude, lookup each name). Run: `go test ./internal/session/ -run TestFracNzPresent -count=1` - expected FAIL (names missing).
- [ ] **Step 2:** Add the Int support lemmas (isZeroInt, isZeroIntSound, nonnegInj, intTagDisc, addWPosR if not already present, mulWPosZero, imulPosZero) in the Int lemma area, each with a one-line comment stating what it is. Verify the prelude still loads: `go test ./internal/session/ -run TestPrelude -count=1` (or the cheapest prelude-loading test; find one with grep).
- [ ] **Step 3:** Add isZeroResp, isZeroF, nzFrac in the new FRAC LAWS banner section. Prelude loads again.
- [ ] **Step 4:** Run: `go test ./internal/session/ -run TestFracNzPresent -count=1` - expected PASS. Then `go test ./internal/session/ ./internal/repl/ -count=1 -timeout 15m` - all green.
- [ ] **Step 5:** Commit: `git add internal/prelude/prelude.rune internal/session/tower_hash_test.go && git commit -m "feat(prelude): nzFrac - lifted Frac zero-test over canonical-Int zero-product"`

---

### Task 2: the additive laws (assoc, comm, zeroL, zeroR)

**Files:**
- Modify: `internal/prelude/prelude.rune`
- Test: `internal/session/tower_hash_test.go`

**Interfaces:**
- Consumes: rawAdd, addF, qind, qsound, ringLawsInt ingredient lemmas (iaddAssoc/iaddComm/iaddZeroL/R, imulAssoc/imulComm, idistribL/R, imulOneL/R, imulZeroL/R), the cong/sym/trans combinators, `fracZeroP : QPair is qpair (nonneg zero) zero end` style local abbreviations as needed.
- Produces (exact statements):
  - Representative level (each a QRel fact): `addAssocP : (p : QPair) -> (q : QPair) -> (r : QPair) -> QRel (rawAdd (rawAdd p q) r) (rawAdd p (rawAdd q r))`; `addCommP : (p : QPair) -> (q : QPair) -> QRel (rawAdd p q) (rawAdd q p)`; `addZeroLP : (p : QPair) -> QRel (rawAdd (qpair (nonneg zero) zero) p) p`; `addZeroRP : (p : QPair) -> QRel (rawAdd p (qpair (nonneg zero) zero)) p`
  - Lifted: `addFAssoc : (x : Frac) -> (y : Frac) -> (z : Frac) -> Eq Frac (addF (addF x y) z) (addF x (addF y z))`; `addFComm : (x : Frac) -> (y : Frac) -> Eq Frac (addF x y) (addF y x)`; `addFZeroL : (x : Frac) -> Eq Frac (addF (fracOf zero) x) x`; `addFZeroR : (x : Frac) -> Eq Frac (addF x (fracOf zero)) x`

Port guide: ch113 qaddCommP (1268), qaddZeroP (1420, right-zero; derive left from comm or prove directly), qaddAssocNum + qaddAssocP (1513/1546 - the assoc numerator identity is the long one; their qaddAssocNum factors the polynomial rearrangement, mirror that factoring with Int lemmas). The lift pattern: triple/double/single qind with motive `fn (x : Frac) is Eq Frac ... end` (Prop), each leaf closed by `qsound QPair QRel _ _ (addAssocP p q r)` style. Mirror how ch113's qaddAssoc (1586) stacks the qinds, but our qind/qlift argument spelling is the prelude's (copy from mulF/addF's own definitions).

CAUTION: addF is built via addFR (double lift, 2684/2693). When x, y, z are qin-values, addF (qin p) (qin q) must COMPUTE to qin (rawAdd p q) for qsound to close the leaf - verify with a quick REPL probe before writing the long proof; if an extra step is stuck, the leaf needs a transEq through the qlift iota, look at how Plan A's toRadixRespects handled leaves.

- [ ] **Step 1:** Extend the presence test (new `TestFracAddLawsPresent`): addAssocP, addCommP, addZeroLP, addZeroRP, addFAssoc, addFComm, addFZeroL, addFZeroR. Run - FAIL.
- [ ] **Step 2:** Prove the four representative-level lemmas (comm and the two zeros first - short; assoc last - the long polynomial). Prelude loads after each.
- [ ] **Step 3:** Lift all four. Prelude loads.
- [ ] **Step 4:** `go test ./internal/session/ -run 'TestFracAddLawsPresent' -count=1` PASS; then `go test ./internal/session/ ./internal/repl/ -count=1 -timeout 15m` green.
- [ ] **Step 5:** Commit: `git commit -m "feat(prelude): Frac additive laws (assoc/comm/zero) by qind+qsound over ringLawsInt"` (explicit pathspecs).

---

### Task 3: the multiplicative, distributive, and annihilation laws

**Files:**
- Modify: `internal/prelude/prelude.rune`
- Test: `internal/session/tower_hash_test.go`

**Interfaces:**
- Consumes: rawMul, mulF, everything Task 2 consumed.
- Produces (exact statements):
  - Representative: `mulAssocP : (p q r : QPair) -> QRel (rawMul (rawMul p q) r) (rawMul p (rawMul q r))`; `mulCommP : (p q : QPair) -> QRel (rawMul p q) (rawMul q p)`; `mulOneLP : (p : QPair) -> QRel (rawMul (qpair (nonneg (succ zero)) zero) p) p`; `mulOneRP : (p : QPair) -> QRel (rawMul p (qpair (nonneg (succ zero)) zero)) p`; `distribLP : (p q r : QPair) -> QRel (rawMul p (rawAdd q r)) (rawAdd (rawMul p q) (rawMul p r))`; `distribRP : (p q r : QPair) -> QRel (rawMul (rawAdd p q) r) (rawAdd (rawMul p r) (rawMul q r))`; `mulZeroLP : (p : QPair) -> QRel (rawMul (qpair (nonneg zero) zero) p) (qpair (nonneg zero) zero)`; `mulZeroRP : (p : QPair) -> QRel (rawMul p (qpair (nonneg zero) zero)) (qpair (nonneg zero) zero)`
  - Lifted (all (x y z : Frac) as appropriate): `mulFAssoc`, `mulFComm`, `mulFOneL`, `mulFOneR`, `distribFL : Eq Frac (mulF x (addF y z)) (addF (mulF x y) (mulF x z))`, `distribFR : Eq Frac (mulF (addF x y) z) (addF (mulF x z) (mulF y z))`, `mulFZeroL : Eq Frac (mulF (fracOf zero) x) (fracOf zero)`, `mulFZeroR : Eq Frac (mulF x (fracOf zero)) (fracOf zero)`

Port guide: ch113 qmulCommP (1058), qmulOneRP (1392), qmulAssocP (1454), qmulDistRP (1683). mulAssocP over componentwise rawMul is nearly definitional modulo imulAssoc on num and the den succ-arithmetic (qden (rawMul p q) = imul (qden p) (qden q) is already proven: qdenMul at 2485 - USE IT). distribL derive from distribR + mulCommP + addCommP transports if that is shorter (ch113 derived L from R at the Nat layer, 248); either route is fine as long as the statement is exact. The annihilation laws should be near-refl (rawMul with zero numerator has numerator imul (nonneg zero) _ = nonneg zero by imulZeroL; the QRel to 0/1 then closes by imulZeroL both sides).

- [ ] **Step 1:** Presence test `TestFracMulLawsPresent` (all 16 names). FAIL.
- [ ] **Step 2:** Prove mulCommP, mulOneLP/RP, mulZeroLP/RP (short ones). Prelude loads.
- [ ] **Step 3:** Prove mulAssocP, distribRP, distribLP (the long ones). Prelude loads.
- [ ] **Step 4:** Lift all eight. Prelude loads. Presence PASS.
- [ ] **Step 5:** `go test ./internal/session/ ./internal/repl/ -count=1 -timeout 15m` green.
- [ ] **Step 6:** Commit: `git commit -m "feat(prelude): Frac multiplicative/distributive/annihilation laws"`

---

### Task 4: negation laws, ring-level assembly, and the frac-edge homomorphisms

**Files:**
- Modify: `internal/prelude/prelude.rune`
- Test: `internal/session/tower_hash_test.go`

**Interfaces:**
- Consumes: Tasks 2-3's lifted laws; fnegate/rawNeg; inegInvL/R; abGroupLawsOfRing; groupOfRing; fracOf; ratOfInt; intOf.
- Produces (exact statements):
  - `negInvLP : (p : QPair) -> QRel (rawAdd (rawNeg p) p) (qpair (nonneg zero) zero)`; `negInvRP : (p : QPair) -> QRel (rawAdd p (rawNeg p)) (qpair (nonneg zero) zero)`
  - `fnegInvL : (x : Frac) -> Eq Frac (addF (fnegate x) x) (fracOf zero)`; `fnegInvR : (x : Frac) -> Eq Frac (addF x (fnegate x)) (fracOf zero)`
  - Assemblies (mirror the Int assembly shapes at 2267-2363 verbatim, substituting Frac ops addF/mulF/fnegate, identities fracOf zero / fracOf (succ zero), and the Frac lemma names):
    - `semiringLawsFrac : SemiringLaws Frac (divRingFrac.1.1)`
    - `ringLawsFrac : RingLaws Frac (divRingFrac.1)`
    - `commLawsFrac : CommLaws Frac semiringFrac is mulFComm end`
    - `commRingLawsFrac : CommRingLaws Frac (divRingFrac.1)`
    - `abGroupLawsFrac : AbGroupLaws Frac (groupOfRing Frac (divRingFrac.1)) is abGroupLawsOfRing Frac (divRingFrac.1) ringLawsFrac end`
  - Tower-graph edges (deferred here by the parent spec 2026-07-06-v4-tower-hierarchy-design.md Decision 6; now cheap because ops are unreduced):
    - `fracOfAddHom : (a : Whole) -> (b : Whole) -> Eq Frac (fracOf (addW a b)) (addF (fracOf a) (fracOf b))`
    - `fracOfMulHom : (a : Whole) -> (b : Whole) -> Eq Frac (fracOf (mulW a b)) (mulF (fracOf a) (fracOf b))`
    - `ratOfIntAddHom : (a : Int) -> (b : Int) -> Eq Frac (ratOfInt (iadd a b)) (addF (ratOfInt a) (ratOfInt b))`
    - `ratOfIntMulHom : (a : Int) -> (b : Int) -> Eq Frac (ratOfInt (imul a b)) (mulF (ratOfInt a) (ratOfInt b))`
    Each is a single qsound over denominator-1 representatives (rawAdd (qpair i zero) (qpair j zero) has numerator iadd' cross terms with qden = 1: imul i 1 + imul j 1; the QRel to qpair (iadd i j) zero closes by imulOneR + den arithmetic). fracOf n = ratOfInt (nonneg n) definitionally (the coherence triangle), so the fracOf homs may be derived from the ratOfInt ones by refl-transport if that is shorter.
- IMPORTANT check before assembling: confirm the ops-instance ARGUMENT of each law record projects to the intended function. SemiringLaws A s states its slots over s.1 (add), s.2.1 (zero), s.2.2.1 (mul), s.2.2.2 (one). divRingFrac.1.1 projections must compute definitionally to addF / fracOf zero / mulF / fracOf (succ zero) (they do: mkSemiring Frac addF (fracOf zero) mulF (fracOf (succ zero)) at 3028). If any slot statement fails to typecheck against a lemma, the mismatch is a projection-order bug in the assembly, not in the lemma.

- [ ] **Step 1:** Presence test `TestFracRingLawsPresent` (fnegInvL/R, semiringLawsFrac, ringLawsFrac, commLawsFrac, commRingLawsFrac, abGroupLawsFrac, the four homs). FAIL.
- [ ] **Step 2:** negInvLP/RP + lifts (numerator of rawAdd (rawNeg p) p is iadd (imul (ineg i) d) (imul i d) with matching dens; close by idistrib-free route: cong through inegSpec or direct inegInvL after factoring - port shape from how ringLawsInt's inegInvL was consumed). Prelude loads.
- [ ] **Step 3:** The five assemblies, byte-mirroring the Int nesting. Prelude loads.
- [ ] **Step 4:** The four homomorphisms. Prelude loads.
- [ ] **Step 5:** Presence PASS; `go test ./internal/session/ ./internal/repl/ -count=1 -timeout 15m` green.
- [ ] **Step 6:** Commit: `git commit -m "feat(prelude): Frac ring-level law records + tower-graph frac-edge homomorphisms"`

---

### Task 5: the recip inverse laws and FieldLaws - THE deliverable

**Files:**
- Modify: `internal/prelude/prelude.rune`
- Test: `internal/session/tower_hash_test.go`

**Interfaces:**
- Consumes: nzFrac/isZeroF/isZeroIntSound (Task 1), rawRecip/recipF, mulF, Tasks 2-4.
- Produces (exact statements; RecipInvLT/RT shapes from prelude.rune ~940):
  - `recipInvLP : (p : QPair) -> Eq Bool (isZeroInt (qnum p)) false -> QRel (rawMul p (rawRecip p)) (qpair (nonneg (succ zero)) zero)` (and the RT dual `recipInvRP` with rawMul (rawRecip p) p)
  - `recipFInvL : (x : Frac) -> nzFrac x -> Eq Frac (mulF x (recipF x)) (fracOf (succ zero))`; `recipFInvR : (x : Frac) -> nzFrac x -> Eq Frac (mulF (recipF x) x) (fracOf (succ zero))` (match the exact RecipInvLT/RT argument order in the record - read the formers first and mirror them)
  - `divRingLawsFrac : DivRingLaws Frac divRingFrac nzFrac`
  - `fieldLawsFrac : FieldLaws Frac divRingFrac nzFrac` (mkAnd of divRingLawsFrac and commLawsFrac - the 1b FieldLaws bundle's FIRST INHABITANT)

Proof route (the genuinely new proof; ch113/ch116 left the general case open, our canonical Int closes it):
- Representative level, by case on qnum p:
  - nonneg zero: the hypothesis is Eq Bool true false after isZeroInt computes; ex falso via boolDiscTF (the guard discharges the junk case - no equation about junk is ever proven).
  - nonneg (succ k), den pred d: rawRecip p = qpair (nonneg (succ d)) k. rawMul numerator = imul (nonneg (succ k)) (nonneg (succ d)) = nonneg (mulW (succ k) (succ d)); den pred = mulW-shape of (k,d) via the rawMul den. The QRel to 1/1: imul num 1 = imul 1 den, i.e. nonneg (mulW (succ k) (succ d)) vs qden (rawMul ...) - both sides equal the same Whole product modulo mulWComm; close with imulOneL/R + qdenMul + cong nonneg + mulWComm. Port shape: ch113 qmulInvPosP (1806) - theirs is exactly this positive case; ours re-derives with Int lemmas.
  - negsucc k: rawRecip p = the negsucc dual (read rawRecipOn 2432 for the exact shape). The product's numerator is imul (negsucc k) (negsucc-or-neg...) - two negatives; imul computes through double ineg to nonneg (mulW (succ k) (succ d)). Then IDENTICAL closing algebra to the positive case (factor the sign first with inegSpec/imulSpec lemmas if the computation does not go through on the nose - probe in the REPL what `imul (negsucc k') (negsucc d')` normalizes to on closed values first).
- Lift: qind with motive `fn (x : Frac) is nzFrac x -> Eq Frac (mulF x (recipF x)) (fracOf (succ zero)) end` (a Prop). The hypothesis at a qin leaf: nzFrac (qin ... p) must COMPUTE to Eq Bool (isZeroInt (qnum p)) false (isZeroF's qlift iota fires on qin) - verify with a REPL probe; if stuck, thread a transEq through the iota equation.
- STOP contingency: if the negsucc case demands a fact about imul's sign structure not derivable from the existing spec lemmas (inegSpec/imulSpec/imulWsub family), STOP and report the exact obligation; do not invent new sign theory inline.

- [ ] **Step 1:** Presence test `TestFracFieldLawsPresent` (recipInvLP, recipInvRP, recipFInvL, recipFInvR, divRingLawsFrac, fieldLawsFrac). FAIL.
- [ ] **Step 2:** REPL probes: normalize `rawMul (qpair (nonneg (succ zero)) zero) (rawRecip (qpair (nonneg (succ zero)) zero))` and the negsucc analogue on closed values; record what computes. Then prove recipInvLP (three cases) and recipInvRP (derive from recipInvLP + mulCommP transport if the statement allows; otherwise prove directly). Prelude loads.
- [ ] **Step 3:** Lift both; assemble divRingLawsFrac (mkAnd ringLawsFrac (mkAnd ... recipFInvL recipFInvR) following the DivRingLaws record shape) and fieldLawsFrac. Prelude loads.
- [ ] **Step 4:** Presence PASS; `go test ./internal/session/ ./internal/repl/ -count=1 -timeout 15m` green.
- [ ] **Step 5:** Commit: `git commit -m "feat(prelude): FieldLaws Frac - the ladder's first field inhabitant (general recip inverse over canonical Int)"`

---

### Task 6: ch573 chapter + ch570 realignment

**Files:**
- Create: `listings/ch573_frac_field_laws.rune`
- Modify: `listings/ch570_tower_graph.rune`
- Test: the listings gate (harness; find the listing test by grepping listings_test for ch572 and mirror its registration; ch572's entry shows whether new chapters need explicit listing or are glob-discovered)

**Interfaces:**
- Consumes: everything landed in Tasks 1-5.
- Produces: ch573, a byte-identical MIRROR of the prelude's Frac-laws section with book narration (the ch572 pattern): the route argument (quotient beats gcd theory; canonical Int made the general inverse provable where ch113 could not), nzFrac and the zero-product lemma, ONE representative-level law in full narration (recipInvLP - the campaign's novelty), the six law values, honest scope notes (Show/Binary removal + Plan-B-of-Show open question). Every definition in ch573 must be byte-identical to its prelude twin (the reviewer will diff); narration lives in comments only.
- ch570 realignment: ch570 currently mirrors the OLD Frac (own-shape data Frac). Realign its Frac block to the quotient prelude (fracOf/ratOfInt over qin/qpair) and ADD the four edge homomorphisms with their proofs mirrored from Task 4. ch570 is self-contained (defines its own types) - keep it elaborating standalone; the mirror discipline is shape-fidelity to the prelude, not literal hash equality (its Int/Whole are local).

- [ ] **Step 1:** Read ch572_int_ring_laws.rune's header + structure as the template. Write ch573 (mirror + narration). Ensure no em-dashes.
- [ ] **Step 2:** Realign ch570's Frac block + add the homs.
- [ ] **Step 3:** Run the listings gate for both chapters (the harness test that elaborates listings; run the narrow -run filter first, then the full listings suite): expected PASS.
- [ ] **Step 4:** Commit: `git commit -m "docs(listings): ch573 Frac field laws chapter + ch570 quotient realignment"`

---

### Task 7: load budget, full suite, bookkeeping, close

**Files:**
- Modify: `docs/superpowers/specs/2026-07-07-frac-field-laws-design.md` (status), `PARKING-LOT.md`, `docs/superpowers/plans/2026-07-06-three-majors-roadmap.md`, this plan (checkboxes).
- Contingency: create `internal/prelude/laws.rune` ONLY if the gate fails.

- [ ] **Step 1:** Measure: build ./cmd/rune once, three cold `echo "1/3 + 2/3" | rune repl` runs, record wall times. GATE: under 3s (spec Decision 5). Plan A baseline was 0.34-0.37s. If the gate FAILS, execute the pre-approved contingency (law VALUES move to internal/prelude/laws.rune loaded by tests and importable on demand; prelude keeps ops + instances + light lemmas) and re-run Tasks' presence tests against the new load path - this is a large re-plumb, so if triggered, STOP after moving the values and report before rewiring tests.
- [ ] **Step 2:** Full `go test -timeout 30m ./...` - ALL PASS (run detached, poll the log file; do not wait on pgrep of your own command string).
- [ ] **Step 3:** Bookkeeping: spec Status -> Plan B COMPLETE (commits + measured load time + budget verdict); PARKING-LOT proof-load entry updated with the Plan B number and the proof-caching question's disposition; roadmap Frac line -> campaign COMPLETE; note the STILL-OPEN items (Show/Binary for Frac design question; duplicate cong/sym/trans combinator consolidation) wherever they are already recorded.
- [ ] **Step 4:** Em-dash grep over all changed files (added lines): zero.
- [ ] **Step 5:** Commit: `git commit -m "docs: close Frac Plan B (load measurement, bookkeeping)"`

---

## Self-review notes

- Spec coverage: Decision 4's six values -> Tasks 4-5; Decision 5 gate -> Task 7; Decision 6 chapter/realignment/pins -> Task 6 (REPL pins unchanged - no pin edits are licensed in this plan); nzFrac (Decision 4's nz parameter) -> Task 1; parent-spec deferred frac-edge homs -> Task 4.
- The `recipFInvL/R` statements must match RecipInvLT/RT's argument order exactly - flagged in Task 5.
- Type consistency: isZeroF/nzFrac (Task 1) consumed by Task 5's guards; divRingFrac.1 / divRingFrac.1.1 projections used consistently as the Ring/Semiring arguments; commLawsFrac over semiringFrac (the instance name) matching commLawsInt's pattern over semiringInt.
- Statement-correctness review per task is mandatory (wrong-but-well-typed slots are the known failure mode; reviewers hand-trace).
