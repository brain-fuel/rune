# v4 Ord Plan B (ordered-algebra bridge) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Wire the Ord class (Plan A) to the algebra hierarchy: `OrderedSemiringLaws` / `OrderedRingLaws` / `OrderedFieldLaws` law records (addition monotone, product of nonnegatives nonnegative) with PROVEN instances for Whole/Int/Frac - `orderedFieldLawsFrac` the deliverable (the first ordered field).

**Architecture:** Laws-only, mirroring the algebra hierarchy's laws-split: no new ops shapes, only new law records combining an existing Semiring/Ring/DivRing with an Ord. OrderedRingLaws reuses OrderedSemiringLaws over the ring's embedded semiring (the ladder-bridge doctrine, like abGroupLawsOfRing). The two order-compat facts (addMono, mulNonneg) are stated over the erased `Le` view and proven per type: Whole from the existing `leb` monotonicity, Int by sign-case, Frac by qind to representatives closed with the Plan A order lemmas.

**Tech Stack:** rune prelude proofs (internal/prelude/prelude.rune); Go tests (internal/session, internal/repl); no kernel/codegen changes.

## Global Constraints

- Spec: docs/superpowers/specs/2026-07-08-v4-ord-comparison-design.md (Decision 5).
- Kernel FROZEN: no changes under core/, store/, elaborate/, codegen/. Prelude + listings + Go tests only. No hash-format bump.
- Statements must NOT be weakened, permuted, or specialized to make a proof go through. The statement types below are the contract.
- NO gcd/coprimality/lowest-terms proofs. NO general multiplicative cancellation. The Frac order-compat proofs reuse Plan A's positive-scaling monotonicity (ilebMulPosR/lebMulPosR) and the Frac order laws (ordLawsFrac); if a proof demands cancellation, STOP and report.
- NO em-dashes or en-dashes anywhere. ASCII hyphens only.
- rune gotchas: every `fn ... is` needs its own `end`; case arms eager (ih in one arm; precompute with let); seq breaks on multi-line RHS (nested let ... in); qind motives must be Prop (write the unfolded `Eq Bool (leOf ..) true`, keep `Le` at top-level types); prelude elaboration IS the proof check.
- Eq combinators: explicit-arg family (congE/symEq/transEq + congImul*/congIadd*).
- Positional Sigma-record discipline: new law records get a shape comment; the class-hash audit (TestTowerClassHashesDistinct) is extended in Task 5 with the new record formers.
- Conventional Commits; every commit message ends with: Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>
- Work in the dedicated worktree .worktrees/feat-ord-bridge (branch feat/ord-bridge off main @ v3.378.0). Commit with explicit pathspecs only.
- Full `go test -timeout 30m ./...` before the tag (Task 5), plus per-task `go test ./internal/session/ ./internal/repl/ -count=1` gates.

## Existing material the tasks consume (all internal/prelude/prelude.rune, main @ v3.378.0)

- Ops instances/formers: `Semiring`/`mkSemiring` (94/103), `Ring`/`mkRing` (119/122), `DivRing` (grep), `semiringWhole` (146, Whole is a Semiring, NOT a Ring - no negation), `ringInt` (498), `divRingFrac` (3141). Projections: a Ring `r.1` is its Semiring, `r.1.1` its add, `r.1.2.1` its zero, `r.1.2.2.1` its mul (confirm exact projection depth against mkSemiring/mkRing before use).
- Algebra laws: `RingLaws`/`ringLawsInt` (2346), `CommLaws` (968)/`commLawsInt`, `commRingLawsInt` (2356), `FieldLaws`/`fieldLawsFrac` (4411), `SemiringLaws`/`semiringLawsWhole`(~841)/`semiringLawsInt`(2267)/`semiringLawsFrac`. `groupOfRing` (1071).
- Ord layer (Plan A): `Ord`/`mkOrd`/`leOf` (5030)/`compareOf`/`Le` (5040); `ordWhole`/`ordInt`/`ordFrac` instances; `ordLawsWhole` (6559)/`ordLawsInt` (6816)/`ordLawsFrac` (9136); `leb`, `ileb`, `leF`; the And/mkAnd machinery (556/557).
- Whole order monotonicity: `lebAddMono : (c x y) -> Eq Bool (leb x y) true -> Eq Bool (leb (addW c x) (addW c y)) true` (5508, add-on-LEFT); `addWComm` (grep) to reach add-on-right; `leb zero n` reduces to `true` (so Whole nonnegativity is definitional).
- Plan A scaling lemmas: `lebAddSameL` (8739), `lebMulPosR` (8754), `ilebMulPosR` (8835), the Frac respect proofs `leRespL`/`leRespR`.
- Int/Frac ops: iadd/imul/ineg; addF/mulF/fnegate; qin/qsound/qlift/qind for the Frac lifts; `nonneg`/`negsucc` constructors; `imul (nonneg a)(nonneg b)` computes to `nonneg (mulW a b)` on the nose.

## File Structure

- Modify: `internal/prelude/prelude.rune` - a new bannered ORDERED ALGEBRA section placed AFTER the Ord instances/laws (after ordLawsFrac, ~9136+) and after fieldLawsFrac, so it sees everything.
- Modify: `internal/session/tower_hash_test.go` - presence + hash-audit tests (accumulating).
- Create (Task 5): `listings/ch575_ordered_algebra.rune`.
- Modify (Task 5): spec status, roadmap, this plan's checkboxes.

---

### Task 1: the order-compat statement formers + the three ordered-algebra law records

**Files:**
- Modify: `internal/prelude/prelude.rune`
- Test: `internal/session/tower_hash_test.go`

**Interfaces:**
- Consumes: Ord/Le/leOf, Semiring/Ring/DivRing formers + projections, SemiringLaws/RingLaws/CommLaws/OrdLaws records, And/mkAnd.
- Produces (exact names/types):
  - `AddMonoT : (A : U) -> Ord A -> (A -> A -> A) -> U is fn (A : U) (o : Ord A) (add : A -> A -> A) is (a : A) -> (b : A) -> (c : A) -> Le A o a b -> Le A o (add a c) (add b c) end end` (addition monotone on the right; a<=b implies a+c<=b+c).
  - `MulNonnegT : (A : U) -> Ord A -> (A -> A -> A) -> A -> U is fn (A : U) (o : Ord A) (mul : A -> A -> A) (z : A) is (a : A) -> (b : A) -> Le A o z a -> Le A o z b -> Le A o z (mul a b) end end` (product of nonnegatives is nonnegative).
  - `OrderedSemiringLaws : (A : U) -> Semiring A -> Ord A -> U` = the And-chain `And (SemiringLaws A s) (And (OrdLaws A o) (And (AddMonoT A o (s.1)) (MulNonnegT A o (s.2.2.1) (s.2.1))))` (the semiring's add is `s.1`, its mul `s.2.2.1`, its zero `s.2.1` - CONFIRM the projection depths against mkSemiring at 103 before committing; adjust if the accessors differ).
  - `OrderedRingLaws : (A : U) -> Ring A -> Ord A -> U` = `And (RingLaws A r) (OrderedSemiringLaws A (r.1) o)` (a ring's order-compat IS its embedded semiring's, per the bridge doctrine; RingLaws carried alongside).
  - `OrderedFieldLaws : (A : U) -> DivRing A -> Ord A -> (A -> U) -> U` = `And (OrderedRingLaws A (d.1) o) (CommLaws A (d.1.1))` (ordered comm ring over the DivRing's ring; the field/recip laws live in FieldLaws separately, composed by the consumer - matching how the algebra hierarchy kept CommLaws split). CONFIRM d.1 is the DivRing's Ring and d.1.1 its Semiring against divRingFrac's mkDivRing shape.
- IMPORTANT: verify every projection (s.1/s.2.1/s.2.2.1, r.1, d.1/d.1.1) computes to the intended op/identity by a REPL probe or by reading mkSemiring/mkRing/mkDivRing, BEFORE writing the records; a wrong projection depth makes the whole task's assemblies fail cryptically.

- [ ] **Step 1:** Presence test `TestOrderedAlgebraRecordsPresent` (AddMonoT, MulNonnegT, OrderedSemiringLaws, OrderedRingLaws, OrderedFieldLaws). Run `go test ./internal/session/ -run TestOrderedAlgebraRecordsPresent -count=1` - FAIL.
- [ ] **Step 2:** Add the two formers + three records in the ORDERED ALGEBRA banner section. Prelude loads (`go test ./internal/session/ -run TestTowerClassHashesDistinct -count=1` as a load check).
- [ ] **Step 3:** Presence PASS; `go test ./internal/session/ ./internal/repl/ -count=1 -timeout 15m` green.
- [ ] **Step 4:** Commit: `git commit -m "feat(prelude): ordered-algebra law records (OrderedSemiring/Ring/Field + addMono/mulNonneg formers)"` (explicit pathspecs).

---

### Task 2: Whole ordered-semiring instance

**Files:**
- Modify: `internal/prelude/prelude.rune`
- Test: `internal/session/tower_hash_test.go`

**Interfaces:**
- Consumes: Task 1 records; semiringWhole, semiringLawsWhole, ordWhole, ordLawsWhole, lebAddMono, addWComm, `leb zero n` reduction.
- Produces:
  - `addMonoWhole : AddMonoT Whole ordWhole addW` - `a<=b -> a+c<=b+c`. `Le Whole ordWhole a b` = `Eq Bool (leb a b) true`; goal `Eq Bool (leb (addW a c)(addW b c)) true`. lebAddMono gives add-on-LEFT (`leb (addW c a)(addW c b)`); rewrite `addW a c = addW c a` and `addW b c = addW c b` via addWComm (subst/cong), then apply lebAddMono.
  - `mulNonnegWhole : MulNonnegT Whole ordWhole mulW zero` - `0<=a -> 0<=b -> 0<=a*b`. `Le Whole ordWhole zero (mulW a b)` = `Eq Bool (leb zero (mulW a b)) true`, and `leb zero _` reduces to `true`, so this is `refl true` (the hypotheses are unused; that is fine - Whole nonnegativity is universal).
  - `orderedSemiringLawsWhole : OrderedSemiringLaws Whole semiringWhole ordWhole` = the And-chain mkAnd of semiringLawsWhole, ordLawsWhole, addMonoWhole, mulNonnegWhole (following the record's nesting; transcribe the And-type annotations carefully).

- [ ] **Step 1:** Presence test `TestWholeOrderedSemiringPresent` (addMonoWhole, mulNonnegWhole, orderedSemiringLawsWhole). FAIL.
- [ ] **Step 2:** Prove addMonoWhole + mulNonnegWhole. Prelude loads. REPL probe: type-check that they inhabit AddMonoT/MulNonnegT (a `defined addMonoWhole` shows the type).
- [ ] **Step 3:** Assemble orderedSemiringLawsWhole. Prelude loads. Presence PASS.
- [ ] **Step 4:** `go test ./internal/session/ ./internal/repl/ -count=1 -timeout 15m` green.
- [ ] **Step 5:** Commit: `git commit -m "feat(prelude): Whole ordered-semiring instance (addMono from lebAddMono, mulNonneg definitional)"`

---

### Task 3: Int ordered-ring instance

**Files:**
- Modify: `internal/prelude/prelude.rune`
- Test: `internal/session/tower_hash_test.go`

**Interfaces:**
- Consumes: Task 1 records; ringInt, ringLawsInt, ordInt, ordLawsInt, ileb, iadd/imul, the Int order laws, addMonoWhole (reusable for the nonneg cases), lebAddMono.
- Produces:
  - `addMonoInt : AddMonoT Int ordInt iadd` - `a<=b -> a+c<=b+c` over Int. Prove by case analysis on the constructors of a, b, c (iadd's cases + ileb's cases). This is the substantial slot: the sign combinations each reduce to a Whole monotonicity fact (addMonoWhole / lebAddMono) or to Int order reasoning. Structure as a clean case tree; if a case needs a Whole lemma not present, add a small one in the Whole area. STOP contingency: if a case demands cancellation, report.
  - `mulNonnegInt : MulNonnegT Int ordInt imul (nonneg zero)` - `0<=a -> 0<=b -> 0<=a*b`. `Le Int ordInt (nonneg zero) a` with a = nonneg n means `ileb (nonneg zero)(nonneg n)` = `leb zero n` = true (always), and a = negsucc _ makes the hypothesis `ileb (nonneg zero)(negsucc _)` = false, so that branch is ex-falso (boolDiscTF). So under the hypotheses both a,b are nonneg; `imul (nonneg a)(nonneg b) = nonneg (mulW a b)`, and `ileb (nonneg zero)(nonneg (mulW a b))` = `leb zero _` = true. Clean.
  - `orderedRingLawsInt : OrderedRingLaws Int ringInt ordInt` = `mkAnd (RingLaws Int ringInt) (OrderedSemiringLaws Int (ringInt.1) ordInt) ringLawsInt orderedSemiringPartInt` where the OrderedSemiring part is a local `mkAnd (SemiringLaws Int (ringInt.1)) ... semiringLawsInt ordLawsInt addMonoInt mulNonnegInt`. (Build the OrderedSemiringLaws value for Int's embedded semiring inline or as a named `orderedSemiringLawsInt`; naming it is cleaner and lets ch575 mirror it.)

- [ ] **Step 1:** Presence test `TestIntOrderedRingPresent` (addMonoInt, mulNonnegInt, orderedSemiringLawsInt, orderedRingLawsInt). FAIL.
- [ ] **Step 2:** Prove addMonoInt (case tree) + mulNonnegInt. Prelude loads. REPL probe a few: e.g. that `addMonoInt` applied to a witness `Le Int ordInt (negsucc 0)(nonneg 0)` at c=nonneg 1 yields `Le Int ordInt (iadd (negsucc 0)(nonneg 1))(iadd (nonneg 0)(nonneg 1))` = `Le .. (nonneg 0)(nonneg 1)` (0<=1, true).
- [ ] **Step 3:** Assemble orderedSemiringLawsInt + orderedRingLawsInt. Prelude loads. Presence PASS.
- [ ] **Step 4:** `go test ./internal/session/ ./internal/repl/ -count=1 -timeout 15m` green.
- [ ] **Step 5:** Commit: `git commit -m "feat(prelude): Int ordered-ring instance (sign-case addMono, nonneg-product mulNonneg)"`

---

### Task 4: Frac ordered-field instance - THE deliverable

**Files:**
- Modify: `internal/prelude/prelude.rune`
- Test: `internal/session/tower_hash_test.go`

**Interfaces:**
- Consumes: Task 1 records; divRingFrac, fieldLawsFrac, commLawsFrac (grep - the Frac CommLaws value from the field campaign), ordFrac, ordLawsFrac, addF/mulF, qin/qsound/qlift/qind, leF/leRespL/R, ilebMulPosR/lebMulPosR, addMonoInt/mulNonnegInt (the representative-level Int facts), the Frac field-laws respect-proof style.
- Produces:
  - `addMonoF : AddMonoT Frac ordFrac addF` - `a<=b -> a+c<=b+c` over Frac. Lift by qind to representatives (motive a Prop: the unfolded `Eq Bool (leF ...) true` shape). At representatives, `leF` is the cross-comparison; addF adds cross terms; the monotonicity reduces to Int addMono (addMonoInt) on the cross-multiplied numerators plus ilebMulPosR to align the positive denominators. Mirror the Frac field-laws respect choreography (scale, rewrite, apply the Int fact). STOP contingency: cancellation -> report.
  - `mulNonnegF : MulNonnegT Frac ordFrac mulF (fracOf zero)` - `0<=a -> 0<=b -> 0<=a*b` over Frac. At representatives: `Le Frac ordFrac (fracOf zero) x` means the numerator of x is >= 0 (cross-compared with 0); the product's numerator is the product of two nonneg numerators (mulNonnegInt) and the denominator positive, so the cross-comparison with 0 holds. Lift by qind.
  - `orderedSemiringLawsFrac : OrderedSemiringLaws Frac (divRingFrac.1.1) ordFrac` (Frac's embedded semiring) = mkAnd semiringLawsFrac ordLawsFrac addMonoF mulNonnegF.
  - `orderedRingLawsFrac : OrderedRingLaws Frac (divRingFrac.1) ordFrac` = mkAnd ringLawsFrac orderedSemiringLawsFrac.
  - `orderedFieldLawsFrac : OrderedFieldLaws Frac divRingFrac ordFrac nzFrac` = mkAnd orderedRingLawsFrac commLawsFrac - THE DELIVERABLE (the first ordered field: an ordered comm ring over the DivRing, composing with fieldLawsFrac for the full ordered-field story).
- CONFIRM the projection targets: OrderedFieldLaws A (d) (o) (nz) uses d.1 (Ring) and d.1.1 (Semiring); with d = divRingFrac these must be the Frac Ring/Semiring. Probe before assembling.

- [ ] **Step 1:** Presence test `TestFracOrderedFieldPresent` (addMonoF, mulNonnegF, orderedSemiringLawsFrac, orderedRingLawsFrac, orderedFieldLawsFrac). FAIL.
- [ ] **Step 2:** REPL-probe the representative cross computations first. Prove addMonoF (qind lift + Int addMono + scaling). Prelude loads.
- [ ] **Step 3:** Prove mulNonnegF (qind lift + Int mulNonneg). Prelude loads.
- [ ] **Step 4:** Assemble the three ordered-algebra values. Prelude loads. Presence PASS. REPL sanity: addMonoF/mulNonnegF inhabit their types; a concrete witness like `0 <= 1/3 -> 0 <= (1/3)*(1/2)` via mulNonnegF checks.
- [ ] **Step 5:** `go test ./internal/session/ ./internal/repl/ -count=1 -timeout 15m` green.
- [ ] **Step 6:** Commit: `git commit -m "feat(prelude): Frac ordered-field instance - the ladder's first ordered field (addMono/mulNonneg over the quotient)"`

---

### Task 5: ch575 chapter + hash audit + full suite + bookkeeping

**Files:**
- Create: `listings/ch575_ordered_algebra.rune`
- Modify: `internal/session/tower_hash_test.go` (extend TestTowerClassHashesDistinct with OrderedSemiringLaws/OrderedRingLaws/OrderedFieldLaws)
- Modify: spec status, roadmap, this plan's checkboxes.

**Interfaces:**
- Consumes: everything in Tasks 1-4.
- Produces: ch575, a self-contained byte-identical MIRROR of the prelude's ORDERED ALGEBRA section with book narration (the ch574 pattern; chapters load with NO prelude, so ch575 imports the transitive closure verbatim - it will pull the full Ord layer + algebra hierarchy + Int-ring + quotient-Frac, the deepest chapter yet; follow ch574's faithful-verbatim-extraction convention, no divergent copies). Narration covers: the ordered-algebra bridge (ordered ring = ring + order + compat), addMono/mulNonneg as the compat laws, the ladder-bridge reuse (OrderedRing through OrderedSemiring), and the ordered-field deliverable. Every definition byte-identical to its prelude twin; narration in comments only.
- Hash audit: add the three new record formers to the TestTowerClassHashesDistinct names slice; confirm no collision.

- [ ] **Step 1:** Extend TestTowerClassHashesDistinct with OrderedSemiringLaws/OrderedRingLaws/OrderedFieldLaws. Run `go test ./internal/session/ -run TestTowerClassHashesDistinct -count=1` - PASS (if a collision, resolve and re-run).
- [ ] **Step 2:** Read ch574's header/structure as the template. Write ch575 (self-contained mirror + narration; no em-dashes). Run the listings gate for ch575 (`go test ./harness -run 'TestListingsElaborateAndCheck/ch575' -count=1 -timeout 20m`, or the discovered gate name) - PASS.
- [ ] **Step 3:** Full `go test -timeout 30m ./...` - ALL PASS (run detached, poll the log file; do not foreground-wait or pgrep your own command string). Measure single cold `rune repl` prelude load (build ./cmd/rune once, three `echo "1/3 + 2/3" | rune repl` runs; 3s budget).
- [ ] **Step 4:** Bookkeeping: spec Status -> Plan B COMPLETE (commits + load vs budget; note the ordered-field deliverable); roadmap Ord line -> Plan B done, Plan C (native lowering) next; check this plan's boxes. Em-dash grep on all changed files (added lines): zero.
- [ ] **Step 5:** Commit: `git commit -m "docs(listings): ch575 ordered-algebra chapter + hash audit + Plan B close"` (explicit pathspecs).

---

## Self-review notes

- Spec coverage: Decision 5 (OrderedSemiring/Ring/Field + addMono + mulNonneg + Whole/Int/Frac instances + the ordered-field deliverable) -> Tasks 1-4; ch575 -> Task 5. Decision 6 (native lowering) is Plan C, out of scope.
- The OrderedRingLaws-through-OrderedSemiringLaws reuse (Task 1) avoids duplicating addMono/mulNonneg statements between the ring and semiring records (Rule-5 clean).
- Type consistency: `Le A o z (mul a b)` etc. computes to `Eq Bool (leOf A o z (mul a b)) true`; Whole's mulNonneg is `refl true` (leb zero _ = true); Int/Frac reuse addMonoInt/mulNonnegInt at the representative level; orderedFieldLawsFrac's projections (divRingFrac.1 Ring, divRingFrac.1.1 Semiring) must match OrderedFieldLaws' parameter uses - flagged in Tasks 1 and 4 to confirm before assembling.
- The Frac addMonoF/mulNonnegF (Task 4) are the campaign's real work; the STOP contingency (forbidden cancellation) is stated; they reuse Plan A's positive-scaling monotonicity, not new number theory.
- Statement-correctness review per task is mandatory (a reversed addMono - `a+c <= b+c -> a <= b` instead of the forward direction - or a mulNonneg with a swapped zero-side typechecks but is wrong; reviewers hand-trace).
