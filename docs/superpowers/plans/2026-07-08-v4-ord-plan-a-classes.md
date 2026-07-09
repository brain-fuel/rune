# v4 Ord Plan A (classes + tower instances) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship the `Ordering` type, the `DecEq` and `Ord` classes (three coherent comparison views: `le`/`compare`/`Le`), their laws records, and PROVEN DecEq + Ord instances for Whole/Int/Frac - the ordering layer of the numeric tower, prerequisite for the ordered-algebra bridge (Plan B) and native lowering (Plan C).

**Architecture:** Mirror the algebra hierarchy's ops/laws split exactly (ops classes = Sigma-records, laws = separate And-chain records, guarded tier = absence of laws). Comparison primitives are decidable Bool/Ordering functions that compute. Order laws over Whole reuse the existing `leb` corpus verbatim; Int laws are constructor-case analysis over Whole lemmas; the Frac instance lifts a cross-comparison out of the quotient by qlift, its respect proof an Int positive-scaling monotonicity fact (no gcd, no cancellation).

**Tech Stack:** rune prelude proofs (internal/prelude/prelude.rune); Go tests (internal/session, internal/repl); no kernel/codegen changes.

## Global Constraints

- Spec: docs/superpowers/specs/2026-07-08-v4-ord-comparison-design.md (Decisions 1-4, 7).
- Kernel FROZEN: no changes under core/, store/, elaborate/, codegen/. Prelude + listings + Go tests only. No hash-format bump. (Native lowering is Plan C.)
- Statements must NOT be weakened, permuted, or specialized to make a proof go through. The statement types below are the contract.
- NO gcd/coprimality/lowest-terms proofs. NO general multiplicative cancellation (`imul k a = imul k b -> a = b`). The Frac respect proof uses positive-scaling order MONOTONICITY (`ileb (imul a c) (imul b c)` reflects/preserves `ileb a b` for positive c), which is an ordered-semiring fact, NOT cancellation. If a proof demands the forbidden cancellation, STOP and report.
- NO em-dashes or en-dashes anywhere (comments, docs, commit messages). ASCII hyphens only.
- rune gotchas: every `fn ... is` needs its own `end`; case arms evaluate eagerly (recursive ih in exactly one arm; precompute predicates with let, then a single nested decision); seq breaks on multi-line RHS (use nested let ... in); prelude elaboration IS the proof check - if the prelude loads, the proofs hold.
- Positional Sigma-record discipline: ops classes are Sigma-records distinguished by field POSITION; a new class with an identical field-type sequence COLLIDES. Every new class gets a comment stating its shape AND an entry in TestTowerClassHashesDistinct (Task 5).
- Eq combinators: use the EXPLICIT-arg family (congE/symEq/transEq and the congImul*/congIadd* helpers the Frac campaign added); the implicit-arg cong/sym/trans family resolves only BELOW its definition point, confirm scope before use.
- Conventional Commits; every commit message ends with: Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>
- Work in the dedicated worktree .worktrees/feat-ord-classes (branch feat/ord-classes off main @ v3.377.0). Commit with explicit pathspecs only.
- Full `go test -timeout 30m ./...` before the tag (Task 5), plus per-task `go test ./internal/session/ ./internal/repl/ -count=1` gates.

## Existing material the tasks consume (all internal/prelude/prelude.rune, main @ v3.377.0)

- Whole comparison: `leb : Whole -> Whole -> Bool` (193), `leW` (260), `lebEquiv` (265). Lemmas: `lebRefl` (5425), `lebTrans` (5399), `lebComplete : (a b) -> Eq Bool (leb a b) false -> Eq Bool (leb b a) true` (1203, the totality ingredient), `lteAntisym : (a b) -> Eq Bool (leb a b) true -> Eq Bool (leb b a) true -> Eq Whole a b` (5063, antisymmetry - despite the `lte` name it is over `leb`).
- Whole equality: `eqW : Whole -> Whole -> Bool` (4556, subW-based), `eqWRefl` (6191), `eqWComplete : (x y) -> Eq Whole x y -> Eq Bool (eqW x y) true` (6203), `eqWSound : (x y) -> Eq Bool (eqW x y) true -> Eq Whole x y` (6253).
- Bool ops: `or : Bool -> Bool -> Bool` (4958), `andB : Bool -> Bool -> Bool` (7797).
- Junk-free Int: `data Int is nonneg : Whole -> Int | negsucc : Whole -> Int` (negsucc k = -(k+1)); `imul`/`iadd`/`ineg`; `ringInt : Ring Int` (~499); `ringLawsInt` and its lemmas. Int equality helper `isZeroInt` (Frac campaign, Task 1) if useful.
- Quotient Frac: `Frac = Quot QPair QRel`; `qnum`/`qdpred`/`qden p = nonneg (succ (qdpred p))`; `qin`/`qsound`/`qlift`/`qind`; the double-qlift lift pattern from `addF`/`mulF`; the field-laws respect-proof style (mulRespL/R etc.).
- Class scaffolding to mirror: `And : U -> U -> U` (556), `mkAnd` (557); statement formers `AssocT`/`CommT`/`IdLT`/`IdRT` (561-580); `Semiring`/`mkSemiring` (94/103); the SemiringLaws And-chain assembly shape (semiringLawsWhole ~841, semiringLawsInt ~2267).
- Instance-registration pattern: `instance name : Class Type is value end` (e.g. semiringWhole 146). DecEq/Ord instances register the same way.
- Ordering reference (local, in a listing): listings/ch335_compare_sym.rune has `data Ordering is lt | eq | gt end` + `flipOrd` + `OrderingElim` usage - the shape to lift into the prelude.
- Presence-test pattern: internal/session/tower_hash_test.go (TestTowerClassHashesDistinct at line 9; TestWholeSemiringLawsPresent etc.). New tests accumulate here.

## File Structure

- Modify: `internal/prelude/prelude.rune` - a new bannered ORDERING + DECEQ + ORD section. Placement: AFTER the Whole `leb` corpus and `eqW` (so Whole instances see them) and AFTER the Int ops and quotient Frac ops (so Int/Frac instances see imul/leF ingredients). Concretely: place the CLASS definitions (Ordering/DecEq/Ord/laws/defaults) high (they depend only on Bool/Eq), the Whole instances after the leb corpus, the Int instances after ringInt, the Frac instances after the Frac ops. Task-specific placement notes below.
- Modify: `internal/session/tower_hash_test.go` - presence + hash-audit tests (accumulating).
- Create (Task 5): `listings/ch574_ord_classes.rune`.
- Modify (Task 5): spec status, roadmap, this plan's checkboxes.

---

### Task 1: Ordering type + DecEq / Ord classes + laws records + defaults

**Files:**
- Modify: `internal/prelude/prelude.rune`
- Test: `internal/session/tower_hash_test.go`

**Interfaces:**
- Consumes: Bool, `Eq`, `or`, `andB`, `And`/`mkAnd`, the Sigma-record machinery (Sig/Pair/.1/.2).
- Produces (exact names/types later tasks rely on):
  - `data Ordering : U is lt : Ordering | eq : Ordering | gt : Ordering end` (+ auto `OrderingElim`).
  - `flipOrd : Ordering -> Ordering` (lt->gt, eq->eq, gt->lt) via OrderingElim.
  - `DecEq : U -> U is fn (A : U) is A -> A -> Bool end end` (single-field ops class; the eqb operation IS the class value, following the one-op convention - OR a Sig wrapper `Sig (A -> A -> Bool) (fn _ is Unit end)` if a bare function type would collide with another one-arg class; DECIDE by checking the hash-audit: a bare `A -> A -> Bool` type is structurally identical to any other binary-Bool op class, so WRAP it in a named single-field record `mkDecEq`/`DecEq A = Sig (A -> A -> Bool) (fn (_e : A -> A -> Bool) is TypeUnit end)` only if a collision appears; start with the record form for audit-safety, matching how Semiring is a record not a bare tuple). Provide `mkDecEq : (A : U) -> (A -> A -> Bool) -> DecEq A` and accessor `eqbOf : (A : U) -> DecEq A -> A -> A -> Bool`.
  - `Ord : U -> U is fn (A : U) is Sig (A -> A -> Bool) (fn (_le : A -> A -> Bool) is A -> A -> Ordering end) end end` (two-field ops record: le then compare). `mkOrd : (A : U) -> (A -> A -> Bool) -> (A -> A -> Ordering) -> Ord A`. Accessors `leOf : (A) -> Ord A -> A -> A -> Bool` (= o.1), `compareOf : (A) -> Ord A -> A -> A -> Ordering` (= o.2).
  - `Le : (A : U) -> Ord A -> A -> A -> U is fn (A : U) (o : Ord A) (a b : A) is Eq Bool (leOf A o a b) true end end` (the erased proof-side view).
  - Defaults: `compareFromLe : (A : U) -> (A -> A -> Bool) -> A -> A -> Ordering` (let lab = le a b, lba = le b a in: case lab of true -> (case lba of true -> eq | false -> lt) | false -> gt); `leFromCompare : (A : U) -> (A -> A -> Ordering) -> A -> A -> Bool` (case compare a b of gt -> false | lt -> true | eq -> true).
  - Derived: `ge`/`lt`/`gt` as functions over an Ord (ge A o a b = leOf A o b a; ltB A o a b = andB (leOf A o a b) (notB... )) - provide `ltB : (A) -> Ord A -> A -> A -> Bool` = `andB (leOf A o a b) (notOf (leOf A o b a))` using the existing `not`/`notB` (grep for the Bool negation; if none, `case ... of true -> false | false -> true` inline). Only `ge`/`ltB` are required; add others only if a later task needs them.
  - `DecEqLaws : (A : U) -> DecEq A -> U` = And of `(a b : A) -> Eq Bool (eqbOf A d a b) true -> Eq A a b` (soundness) and `(a : A) -> Eq Bool (eqbOf A d a a) true` (reflexivity). Provide the two statement formers inline in the And.
  - `OrdLaws : (A : U) -> Ord A -> U` = the six-slot And-chain from spec Decision 3: leRefl `(a) -> Le A o a a`, leTrans `(a b c) -> Le A o a b -> Le A o b c -> Le A o a c`, leAntisym `(a b) -> Le A o a b -> Le A o b a -> Eq A a b`, leTotal `(a b) -> Eq Bool (or (leOf A o a b) (leOf A o b a)) true`, cmpEq `(a b) -> Eq Ordering (compareOf A o a b) eq -> Eq A a b`, cmpLe `(a b) -> Eq Ordering (compareOf A o a b) lt -> Le A o a b`. Build as reusable statement formers (LeReflT/LeTransT/... or inline And-chain like SemiringLaws; match the SemiringLaws style).

- IMPORTANT collision note: `DecEq A` as a bare `A -> A -> Bool` and `Ord`'s `le` field are the same type. This is why DecEq is a RECORD (mkDecEq wraps the function) - so its hash is a Sig, distinct from a bare arrow and from Ord's two-field Sig. Confirm in Task 5's audit; if the record form still collides with another one-field record, add a phantom marker field (documented).

- [ ] **Step 1:** Write failing presence test `TestOrdScaffoldPresent` in tower_hash_test.go asserting the session resolves: Ordering, flipOrd, DecEq, mkDecEq, eqbOf, Ord, mkOrd, leOf, compareOf, Le, compareFromLe, leFromCompare, DecEqLaws, OrdLaws. Run: `go test ./internal/session/ -run TestOrdScaffoldPresent -count=1` - FAIL.
- [ ] **Step 2:** Add Ordering + flipOrd + the two classes + accessors + Le + defaults + derived + the two laws records, in the high ORDERING+DECEQ+ORD banner section (depends only on Bool/Eq/Sig). Prelude loads (`go test ./internal/session/ -run TestTowerClassHashesDistinct -count=1` as a cheap load check).
- [ ] **Step 3:** Run: `go test ./internal/session/ -run TestOrdScaffoldPresent -count=1` - PASS. Then `go test ./internal/session/ ./internal/repl/ -count=1 -timeout 15m` green.
- [ ] **Step 4:** Commit: `git commit -m "feat(prelude): Ordering type + DecEq/Ord classes + laws records + comparison defaults"` (explicit pathspecs).

---

### Task 2: Whole DecEq + Ord instances (reuse the leb/eqW corpus)

**Files:**
- Modify: `internal/prelude/prelude.rune`
- Test: `internal/session/tower_hash_test.go`

**Interfaces:**
- Consumes: Task 1's classes; leb, eqW, lebRefl, lebTrans, lteAntisym, lebComplete, eqWSound, eqWRefl, or.
- Produces:
  - `instance decEqWhole : DecEq Whole is mkDecEq Whole eqW end`
  - `instance ordWhole : Ord Whole is mkOrd Whole leb (compareFromLe Whole leb) end`
  - `decEqLawsWhole : DecEqLaws Whole decEqWhole` = mkAnd of eqWSound and eqWRefl (adapt argument shapes to the DecEqLaws formers; eqbOf Whole decEqWhole computes to eqW so the lemmas apply directly).
  - `ordLawsWhole : OrdLaws Whole ordWhole` = the six-slot mkAnd chain: leRefl=lebRefl, leTrans=lebTrans, leAntisym=lteAntisym, leTotal from `lebComplete` (case on leb a b: true -> or _ _ = true by refl-ish; false -> lebComplete gives leb b a = true, so or false true = true - build the Bool identity), cmpEq and cmpLe from the `compareFromLe` definition unfolding (compare a b = eq means both leb directions true, so lteAntisym closes cmpEq; compare a b = lt means leb a b true, closing cmpLe - these unfold the default `compareFromLe`; probe the reduction in the REPL first).
- Le Whole ordWhole a b computes to `Eq Bool (leb a b) true`, so the existing lemmas' outputs match the slot types on the nose.

- [ ] **Step 1:** Presence test `TestWholeOrdInstancesPresent` (decEqWhole, ordWhole, decEqLawsWhole, ordLawsWhole). FAIL.
- [ ] **Step 2:** Add the two instances (after the leb/eqW corpus). Prelude loads. Probe in REPL: `compareOf Whole ordWhole 2 3` -> lt-shaped, `leOf Whole ordWhole 3 3` -> true, `eqbOf Whole decEqWhole 4 4` -> true.
- [ ] **Step 3:** Add decEqLawsWhole + ordLawsWhole. The cmpEq/cmpLe slots require unfolding compareFromLe: if a slot sticks, add a small local lemma `compareFromLeEq`/`compareFromLeLt` characterizing the default (these are reusable by Int/Frac later, so name them generally, not Whole-specific). Prelude loads.
- [ ] **Step 4:** Presence PASS; `go test ./internal/session/ ./internal/repl/ -count=1 -timeout 15m` green.
- [ ] **Step 5:** Commit: `git commit -m "feat(prelude): Whole DecEq + Ord instances (total order from the leb corpus)"`

---

### Task 3: Int comparison + DecEq + Ord instances

**Files:**
- Modify: `internal/prelude/prelude.rune`
- Test: `internal/session/tower_hash_test.go`

**Interfaces:**
- Consumes: junk-free Int (nonneg/negsucc), leb + its lemmas, Task 1 classes, Task 2's compareFromLe characterization lemmas.
- Produces:
  - `ileb : Int -> Int -> Bool` by cases on both args: `nonneg a`/`nonneg b` -> `leb a b`; `nonneg _`/`negsucc _` -> `false`; `negsucc _`/`nonneg _` -> `true`; `negsucc k`/`negsucc j` -> `leb j k` (order reverses: negsucc k = -(k+1), so -(k+1) <= -(j+1) iff j <= k). Use nested case (outer on first arg, inner on second); eager-arm discipline (no recursion here, just cases).
  - `ieqb : Int -> Int -> Bool` = `andB (ileb a b) (ileb b a)` OR direct constructor-case (nonneg a/nonneg b -> eqW a b; negsucc k/negsucc j -> eqW k j; mixed -> false). Direct is cleaner for the soundness proof; choose it.
  - `instance decEqInt : DecEq Int is mkDecEq Int ieqb end`
  - `instance ordInt : Ord Int is mkOrd Int ileb (compareFromLe Int ileb) end`
  - `decEqLawsInt : DecEqLaws Int decEqInt` (soundness by case analysis reducing to eqWSound + constructor injectivity `nonnegInj`/negsucc-inj; reflexivity by case + eqWRefl).
  - `ordLawsInt : OrdLaws Int ordInt` - leRefl/leTrans/leAntisym/leTotal by case analysis on the Int constructors reducing each to the corresponding Whole lemma (the negsucc/negsucc case reverses, so leTrans on two negsuccs uses lebTrans with swapped operands; leAntisym on negsuccs uses lteAntisym then cong negsucc). cmpEq/cmpLe via the compareFromLe characterization lemmas from Task 2.
- Constructor injectivity: `nonnegInj` exists (Frac Task 1). Need `negsuccInj : (k j) -> Eq Int (negsucc k) (negsucc j) -> Eq Whole k j` (mirror nonnegInj via the isign/imag-style projection; grep for a negsucc projection, else add it). Mixed-constructor discrimination via `intTagDisc` (Frac Task 1) / its dual.

- [ ] **Step 1:** Presence test `TestIntOrdInstancesPresent` (ileb, ieqb, decEqInt, ordInt, decEqLawsInt, ordLawsInt). FAIL.
- [ ] **Step 2:** Add ileb + ieqb + any needed injectivity helper. Prelude loads. REPL probe: `ileb (nonneg 2) (nonneg 3)` -> true; `ileb (negsucc 0) (nonneg 0)` -> true (-1 <= 0); `ileb (negsucc 2) (negsucc 0)` -> true (-3 <= -1); `ileb (nonneg 0) (negsucc 0)` -> false; `ieqb (negsucc 1) (negsucc 1)` -> true.
- [ ] **Step 3:** Add the instances + laws. Prelude loads.
- [ ] **Step 4:** Presence PASS; `go test ./internal/session/ ./internal/repl/ -count=1 -timeout 15m` green.
- [ ] **Step 5:** Commit: `git commit -m "feat(prelude): Int comparison (ileb/ieqb) + DecEq + Ord instances (sign-case order)"`

---

### Task 4: Frac comparison over the quotient + DecEq + Ord instances (the respect proof)

**Files:**
- Modify: `internal/prelude/prelude.rune`
- Test: `internal/session/tower_hash_test.go`

**Interfaces:**
- Consumes: quotient Frac (qnum/qden/qin/qsound/qlift/qind), ileb (Task 3), imul, the field-laws respect-proof style, ringLawsInt.
- Produces:
  - `ilebMulPosR : (a : Int) -> (b : Int) -> (d : Whole) -> Eq Bool (ileb (imul a (nonneg (succ d))) (imul b (nonneg (succ d)))) (ileb a b)` - THE campaign's key lemma: multiplying both sides of an Int `<=` by a POSITIVE Int (nonneg (succ d)) leaves the comparison unchanged (both preserves and reflects, stated as Bool equality of the two comparisons). This is ordered-semiring monotonicity, NOT cancellation. Prove by case on a, b (four sign cases) reducing to a Whole monotonicity lemma `lebMulPosR : (x y : Whole) -> (d : Whole) -> Eq Bool (leb (mulW x (succ d)) (mulW y (succ d))) (leb x y)` (positive-scaling on Whole; prove by induction, likely already have ingredients in the lebAddMono/monus library - grep lebAddMono at 5275, lebMulPos, reuse or extend). If lebMulPosR needs a genuinely new Whole induction, add it in the Whole lemma area.
  - `leCrossResp : (p p' q q' : QPair) -> QRel p p' -> QRel q q' -> Eq Bool (ileb (imul (qnum p) (qden q)) (imul (qnum q) (qden p))) (ileb (imul (qnum p') (qden q')) (imul (qnum q') (qden p')))` - the double-argument respect fact (both representatives change). Derive from ilebMulPosR applied to the cross-multiplication rearrangement (the QRel equalities let you scale both comparison sides by the other pair's positive denominator). This mirrors the field-laws mulRespL/R structure. If it splits into leRespL + leRespR (one arg at a time), do that (cleaner, matches addRespL/R).
  - `leF : Frac -> Frac -> Bool` via double qlift over `fn (p q : QPair) is ileb (imul (qnum p) (qden q)) (imul (qnum q) (qden p)) end` with leCrossResp (mirror the addF double-qlift spelling exactly).
  - `eqF : Frac -> Frac -> Bool` = `andB (leF x y) (leF y x)` (quotient equality via antisymmetry) OR a direct qlift over the QRel-decision `ieqb (imul (qnum p) (qden q)) (imul (qnum q) (qden p))`; the andB form reuses leF's respect proof (no new respect obligation) - PREFER it.
  - `instance decEqFrac : DecEq Frac is mkDecEq Frac eqF end`
  - `instance ordFrac : Ord Frac is mkOrd Frac leF (compareFromLe Frac leF) end`
  - `decEqLawsFrac : DecEqLaws Frac decEqFrac` - soundness: eqF x y = true means leF both ways, antisymmetry (ordLawsFrac.leAntisym) gives Eq Frac x y; reflexivity from leF reflexivity. (So decEqLawsFrac may consume ordLawsFrac - order the defs accordingly.)
  - `ordLawsFrac : OrdLaws Frac ordFrac` - the six slots lifted by qind to representatives, each closed by ileb's Int order laws (ordLawsInt) plus ilebMulPosR for the cross-terms. leAntisym is the substantial one (cross `<=` both ways at the representative level gives QRel, then qsound). leTotal from ileb totality (ordLawsInt.leTotal) on the cross terms. cmpEq/cmpLe via the compareFromLe characterization.
- STOP contingency: if leCrossResp or ordLawsFrac.leAntisym demands the forbidden multiplicative cancellation (not just positive-scaling monotonicity), STOP and report the exact obligation.

- [ ] **Step 1:** Presence test `TestFracOrdInstancesPresent` (ilebMulPosR, leCrossResp or leRespL/R, leF, eqF, decEqFrac, ordFrac, decEqLawsFrac, ordLawsFrac). FAIL.
- [ ] **Step 2:** REPL-probe the cross computation on closed values first (`ileb (imul (nonneg 1)(nonneg 3)) (imul (nonneg 1)(nonneg 2))` for 1/2 vs 1/3). Prove lebMulPosR (Whole) then ilebMulPosR (Int). Prelude loads.
- [ ] **Step 3:** Prove leCrossResp (or leRespL/R), define leF/eqF via qlift, add instances. Prelude loads. REPL probe: `leF (1/3) (1/2)` -> true; `leF (1/2) (1/3)` -> false; `leF (2/4) (1/2)` and `leF (1/2) (2/4)` both true (non-canonical equal); `eqF (2/4) (1/2)` -> true; a negative: `leF (-1/2) (1/3)` -> true.
- [ ] **Step 4:** Prove ordLawsFrac + decEqLawsFrac. Prelude loads. Presence PASS.
- [ ] **Step 5:** `go test ./internal/session/ ./internal/repl/ -count=1 -timeout 15m` green.
- [ ] **Step 6:** Commit: `git commit -m "feat(prelude): Frac comparison over the quotient (leF/eqF) + DecEq + Ord instances (positive-scaling respect proof)"`

---

### Task 5: ch574 chapter + hash audit + full suite + bookkeeping

**Files:**
- Create: `listings/ch574_ord_classes.rune`
- Modify: `internal/session/tower_hash_test.go` (extend TestTowerClassHashesDistinct)
- Modify: spec status, roadmap, this plan's checkboxes.

**Interfaces:**
- Consumes: everything in Tasks 1-4.
- Produces: ch574, a byte-identical MIRROR of the prelude's ORDERING+DECEQ+ORD section with book narration (the ch572/ch573 pattern; chapters load with NO prelude, self-contained - restate the Bool/Whole/Int/Frac supporting types the section needs, following how ch573 scoped itself). Narration covers: the three coherent views and why both le and compare live in the ops class, the ops/laws split for ordering, DecEq as a principled standalone class, and the Frac respect proof in full (positive-scaling monotonicity, the campaign's novelty). Every definition byte-identical to its prelude twin; narration in comments only.
- Hash audit: add "DecEq", "Ord", "Ordering" (and confirm no collision) to the `names` slice in TestTowerClassHashesDistinct.

- [ ] **Step 1:** Extend TestTowerClassHashesDistinct with DecEq/Ord/Ordering. Run `go test ./internal/session/ -run TestTowerClassHashesDistinct -count=1` - expected PASS (if a collision appears, resolve per Task 1's phantom-marker note, then re-run). Commit is folded into Step 4.
- [ ] **Step 2:** Read ch572/ch573 headers as the template. Write ch574 (self-contained mirror + narration; no em-dashes). Run the listings gate: `go test ./harness -run 'Listings.*ch574|ListingsElaborateAndCheck' -count=1 -timeout 20m` (find the exact gate name by grepping harness/ for how ch573 is run) - expected PASS.
- [ ] **Step 3:** Full `go test -timeout 30m ./...` - ALL PASS (run detached, poll the log file; do not foreground-wait or pgrep your own command string). Measure single cold `rune repl` prelude load (build ./cmd/rune once, three `echo "1/3 + 2/3" | rune repl` runs, record wall time; 3s budget).
- [ ] **Step 4:** Bookkeeping: spec Status -> Plan A COMPLETE (commits + load time vs budget); roadmap Ord line -> Plan A done, Plan B (bridge) next; check this plan's Task 1-5 boxes. Em-dash grep on all changed files (added lines): zero.
- [ ] **Step 5:** Commit: `git commit -m "docs(listings): ch574 Ord classes chapter + hash audit + Plan A close"` (chapter + test + docs, explicit pathspecs).

---

## Self-review notes

- Spec coverage: Decision 1 (three views + defaults) -> Task 1; Decision 2 (DecEq) -> Tasks 1-4; Decision 3 (OrdLaws) -> Task 1 record, Tasks 2-4 instances; Decision 4 (tower instances) -> Tasks 2-4; Decision 7 (ch574) -> Task 5. Decisions 5-6 (bridge, lowering) are Plans B/C, out of scope.
- The Frac respect proof (Task 4) is the campaign's real content; its STOP contingency (forbidden cancellation) is stated. The positive-scaling lemma ilebMulPosR/lebMulPosR is monotonicity, explicitly distinguished from cancellation.
- Type consistency: `Le A o a b` computes to `Eq Bool (leOf A o a b) true` so every leb-corpus lemma's output type matches the OrdLaws slots on the nose; ordWhole/ordInt/ordFrac all use `compareFromLe` so the cmpEq/cmpLe characterization lemmas (introduced Task 2) serve all three; decEqLawsFrac consuming ordLawsFrac requires ordFrac/ordLawsFrac defined before decEqLawsFrac.
- Collision risk: DecEq is a record (mkDecEq wraps the function) so its hash differs from a bare arrow and from Ord's two-field Sig; Task 5's audit is the gate, Task 1 carries the phantom-marker fallback.
- Statement-correctness review per task is mandatory (wrong-but-well-typed slots and polarity swaps - e.g. a reversed leTotal or a compareFromLe that maps eq to lt - are the known failure mode; reviewers hand-trace).
