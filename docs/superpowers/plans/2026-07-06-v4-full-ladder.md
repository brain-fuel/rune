# v4 full Magma-to-Field ladder Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the shipped 3-rung hierarchy to the full Magma-to-Field ladder: rename Semigroup to Magma, add the lower law records (Semigroup/Monoid/CommMonoid), the Group ops shape with Group/AbGroup laws, the CommRing/Field law bundles, the bridge functions + law transports connecting the rungs, Whole's lower-rung positions, and chapter ch571.

**Architecture:** Prelude-only (kernel frozen). The structural fact drives everything: laws-only rungs share ops shapes, so the ladder is six ops shapes + ten law records + proven bridges. All new proofs are REASSEMBLY of the shipped Whole lemmas and law records (no new induction).

**Tech Stack:** rune surface (internal/prelude/prelude.rune, listings/), Go tests (internal/session, internal/repl).

## Global Constraints

- Spec: `docs/superpowers/specs/2026-07-06-v4-full-ladder-design.md` (binding). It extends `2026-07-06-v4-tower-hierarchy-design.md`, whose ops/laws split and shipped artifacts (Semiring/Ring/DivRing, formers, SemiringLaws, Whole lemmas, graph) are UNTOUCHED except where this spec names them.
- KERNEL FROZEN: only internal/prelude/prelude.rune, listings/, internal/session/tower_hash_test.go, internal/repl/repl_test.go may change.
- Projection layouts (established): Monoid A = Sig (A->A->A) A, d.1 op, d.2 e. Semiring s.1 add, s.2.1 zero, s.2.2.1 mul, s.2.2.2 one. Ring r.1 semiring, r.2 neg. DivRing d.1 ring, d.2 recip. SemiringLaws 11-slot And-chain order: addAssoc, addComm, addZeroL, addZeroR, mulAssoc, mulOneL, mulOneR, distribL, distribR, annihL, annihR (And = Sig with constant family, so slot k is .2^(k-1).1, last slot .2^10). RingLaws = And (SemiringLaws) (And negInvL negInvR).
- NO instance-table entries for Magma/Monoid/Group beyond the existing append instances (magmaBytes after rename, monoidBytes). Whole monoid values are plain defs.
- REPL pins stay green: `"a" ++ "b"` and string/mempty behavior (TestREPLStringConcat), all arithmetic pins (TestREPLTowerArithmetic, TestREPLIntTower, TestREPLNegationPromotes, TestREPLDecimalLiterals).
- No em or en dashes anywhere (ASCII hyphen only) - hard rule, previously tripped on comments.
- Conventional Commits, explicit pathspecs, trailer `Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>`.
- Work on branch `feat/v4-full-ladder` in a worktree under `.worktrees/`.
- Full `go test -timeout 30m ./...` before finishing.

---

### Task 1: rename Semigroup to Magma

**Files:**
- Modify: `internal/prelude/prelude.rune` (the Semigroup/Monoid section ~lines 155-175, `instance semigroupBytes` ~line 1449, comments ~1447 and ~1747)
- Modify: `internal/session/tower_hash_test.go` (audit name list)
- Modify: `internal/repl/repl_test.go` (one comment, line ~884)

**Interfaces:**
- Produces: `Magma : U -> U` (the bare `A -> A -> A`, formerly Semigroup), `instance magmaBytes : Magma Bytes`, `++` constrained `{d : Magma A}`. Consumed by every later task.

- [ ] **Step 1: adjust the audit test first (failing)**

In `internal/session/tower_hash_test.go`, replace `"Semigroup"` with `"Magma"` in the TestTowerClassHashesDistinct name list.

Run: `go test ./internal/session/ -run TestTowerClassHashesDistinct -count=1 -v`
Expected: FAIL with `class Magma not found in prelude`.

- [ ] **Step 2: the rename**

In `internal/prelude/prelude.rune`:
- Line ~163: `Semigroup : U -> U is ...` becomes `Magma : U -> U is fn (A : U) is A -> A -> A end end`.
- The `++` definition: both occurrences of `Semigroup A` become `Magma A`.
- Line ~1449: `instance semigroupBytes : Semigroup Bytes is strApp end` becomes `instance magmaBytes : Magma Bytes is strApp end`.
- Banner comments (the SEMIGROUP/MONOID banner ~155-162, the String comment ~1447, the pattern reference ~1747): reword so the OPS class is called Magma and the word "semigroup" is reserved for the lawful level, e.g. "`++` dispatches over `Magma` (a bare binary operation; a semigroup is a magma whose op is PROVEN associative - `SemigroupLaws`, below)". Keep `Monoid` as-is (the {op, e} ops record; `mempty` unchanged).
- Search the whole file for any remaining `Semigroup` occurrences and update them (`grep -n Semigroup internal/prelude/prelude.rune` must return zero after; SemigroupLaws does not exist yet - Task 2 adds it).

In `internal/repl/repl_test.go` line ~884: reword the comment "(Semigroup dispatch)" to "(Magma dispatch)".

- [ ] **Step 3: gates**

Run: `go test ./internal/session/ -run TestTowerClassHashesDistinct -count=1 -v` -> PASS.
Run: `go test ./internal/repl/ -count=1` -> PASS (TestREPLStringConcat proves `++` survived).

- [ ] **Step 4: commit**

```bash
git add internal/prelude/prelude.rune internal/session/tower_hash_test.go internal/repl/repl_test.go
git commit -m "refactor(prelude)!: rename Semigroup ops class to Magma (honest name; laws are separate)" -- internal/prelude/prelude.rune internal/session/tower_hash_test.go internal/repl/repl_test.go
```

---

### Task 2: lower law records + Whole's commutative monoids

**Files:**
- Modify: `internal/prelude/prelude.rune` (append to the laws section, after commLawsWhole)
- Modify: `internal/session/tower_hash_test.go` (presence test)

**Interfaces:**
- Consumes: Magma (Task 1), Monoid, And/mkAnd, AssocT/CommT/IdLT/IdRT, mkMonoid, addW/mulW lemmas (addWAssoc, addWComm, addWZeroL, addWZeroR, mulWAssoc, mulWComm, mulWOneL, mulWOneR).
- Produces: `SemigroupLaws : (A : U) -> Magma A -> U`, `MonoidLaws : (A : U) -> Monoid A -> U`, `CommMonoidLaws : (A : U) -> Monoid A -> U`, `monoidWholeAdd : Monoid Whole`, `monoidWholeMul : Monoid Whole`, `commMonoidLawsWholeAdd`, `commMonoidLawsWholeMul`.

- [ ] **Step 1: failing presence test**

Add to tower_hash_test.go a sibling function (same body shape as TestWholeSemiringLawsPresent) named `TestLowerLadderPresent` checking: `"SemigroupLaws", "MonoidLaws", "CommMonoidLaws", "monoidWholeAdd", "monoidWholeMul", "commMonoidLawsWholeAdd", "commMonoidLawsWholeMul"`.

Run: `go test ./internal/session/ -run TestLowerLadderPresent -count=1 -v` -> FAIL.

- [ ] **Step 2: the records and values**

Append to the laws section (complete code; adjust only if elaboration rejects a definitional identification, and report):

```
-- ====================================================================
-- THE LOWER LADDER (v4 full ladder). Laws for the append-family ops
-- shapes: a semigroup is a magma with associativity, a monoid adds the
-- identity laws, a commutative monoid adds commutativity. Whole sits on
-- the CommMonoid rung twice - under addition and under multiplication -
-- with every law already proven above. These are plain defs, not
-- instance-table entries: registering Magma Whole would make `++` mean
-- addition (the Raku lesson: keep `+` arithmetic, `++` append).
-- ====================================================================
SemigroupLaws : (A : U) -> Magma A -> U is
  fn (A : U) (m : Magma A) is AssocT A m end
end
MonoidLaws : (A : U) -> Monoid A -> U is
  fn (A : U) (d : Monoid A) is
    And (AssocT A (d.1)) (And (IdLT A (d.1) (d.2)) (IdRT A (d.1) (d.2)))
  end
end
CommMonoidLaws : (A : U) -> Monoid A -> U is
  fn (A : U) (d : Monoid A) is And (MonoidLaws A d) (CommT A (d.1)) end
end

monoidWholeAdd : Monoid Whole is mkMonoid Whole addW zero end
monoidWholeMul : Monoid Whole is mkMonoid Whole mulW (succ zero) end

commMonoidLawsWholeAdd : CommMonoidLaws Whole monoidWholeAdd is
  mkAnd (MonoidLaws Whole monoidWholeAdd) (CommT Whole addW)
    (mkAnd (AssocT Whole addW)
      (And (IdLT Whole addW zero) (IdRT Whole addW zero))
      addWAssoc
      (mkAnd (IdLT Whole addW zero) (IdRT Whole addW zero) addWZeroL addWZeroR))
    addWComm
end
commMonoidLawsWholeMul : CommMonoidLaws Whole monoidWholeMul is
  mkAnd (MonoidLaws Whole monoidWholeMul) (CommT Whole mulW)
    (mkAnd (AssocT Whole mulW)
      (And (IdLT Whole mulW (succ zero)) (IdRT Whole mulW (succ zero)))
      mulWAssoc
      (mkAnd (IdLT Whole mulW (succ zero)) (IdRT Whole mulW (succ zero)) mulWOneL mulWOneR))
    mulWComm
end
```

(The MonoidLaws statements over `monoidWholeAdd` project to addW/zero definitionally, so the raw-kernel lemmas check on the nose - same mechanism semiringLawsWhole relies on.)

- [ ] **Step 3: gates**

Run: `go test ./internal/session/ -run 'TestLowerLadderPresent|TestTowerClassHashesDistinct' -count=1 -v` -> PASS.
Run: `go test ./internal/repl/ -count=1` -> PASS.

- [ ] **Step 4: commit**

```bash
git add internal/prelude/prelude.rune internal/session/tower_hash_test.go
git commit -m "feat(prelude): Semigroup/Monoid/CommMonoid law records + Whole's two commutative monoids" -- internal/prelude/prelude.rune internal/session/tower_hash_test.go
```

---

### Task 3: Group ops shape + Group/AbGroup/CommRing/Field law records

**Files:**
- Modify: `internal/prelude/prelude.rune` (ops-ladder section gains Group after DivRing; laws section gains the four records)
- Modify: `internal/session/tower_hash_test.go` (audit + presence)

**Interfaces:**
- Consumes: Monoid, NegInvLT/NegInvRT (generic in op/inv/e), MonoidLaws (Task 2), RingLaws, DivRingLaws, CommLaws, CommT, And.
- Produces: `Group : U -> U` (Sig (Monoid A) (A -> A); g.1 monoid, g.2 inv), `mkGroup : (A : U) -> Monoid A -> (A -> A) -> Group A`, `GroupLaws : (A : U) -> Group A -> U`, `AbGroupLaws : (A : U) -> Group A -> U`, `CommRingLaws : (A : U) -> Ring A -> U`, `FieldLaws : (A : U) -> DivRing A -> (A -> U) -> U`.

- [ ] **Step 1: failing tests**

Add `"Group"` to the TestTowerClassHashesDistinct list. Add a sibling presence function `TestUpperLadderPresent` checking `"Group", "GroupLaws", "AbGroupLaws", "CommRingLaws", "FieldLaws"`.

Run: `go test ./internal/session/ -run 'TestUpperLadderPresent|TestTowerClassHashesDistinct' -count=1 -v` -> FAIL (Magma etc. found, Group not).

- [ ] **Step 2: the ops shape (into the ops-ladder section, after mkDivRing)**

```
-- Group: a monoid with an inverse operation. Nests Monoid the way Ring
-- nests Semiring (projection g.1 monoid, g.2 inv); structurally distinct
-- from Ring because the first component differs (Monoid A vs Semiring A).
-- Abelian-ness is a LAW (AbGroupLaws below), never ops-side.
Group : U -> U is
  fn (A : U) is Sig (Monoid A) (fn (_m : Monoid A) is A -> A end) end
end
mkGroup : (A : U) -> Monoid A -> (A -> A) -> Group A is
  fn (A : U) (m : Monoid A) (i : A -> A) is
    Pair (Monoid A) (fn (_m : Monoid A) is A -> A end) m i
  end
end
```

- [ ] **Step 3: the law records (into the laws section, after Task 2's block)**

```
-- Group laws = monoid laws + the two inverse laws. NegInvLT/RT are
-- already generic in (op, inv, e), so they serve groups verbatim.
GroupLaws : (A : U) -> Group A -> U is
  fn (A : U) (g : Group A) is
    And (MonoidLaws A (g.1))
    (And (NegInvLT A (g.1.1) (g.2) (g.1.2))
         (NegInvRT A (g.1.1) (g.2) (g.1.2)))
  end
end
AbGroupLaws : (A : U) -> Group A -> U is
  fn (A : U) (g : Group A) is And (GroupLaws A g) (CommT A (g.1.1)) end
end

-- The classical top rungs as law bundles over the shipped ops shapes:
-- a commutative ring is a Ring whose multiplication commutes; a field is
-- a DivRing whose multiplication commutes.
CommRingLaws : (A : U) -> Ring A -> U is
  fn (A : U) (r : Ring A) is And (RingLaws A r) (CommLaws A (r.1)) end
end
FieldLaws : (A : U) -> DivRing A -> (A -> U) -> U is
  fn (A : U) (d : DivRing A) (nz : A -> U) is
    And (DivRingLaws A d nz) (CommLaws A (d.1.1))
  end
end
```

- [ ] **Step 4: gates + commit**

Run: `go test ./internal/session/ ./internal/repl/ -count=1` -> PASS.

```bash
git add internal/prelude/prelude.rune internal/session/tower_hash_test.go
git commit -m "feat(prelude): Group ops shape + Group/AbGroup/CommRing/Field law records" -- internal/prelude/prelude.rune internal/session/tower_hash_test.go
```

---

### Task 4: bridges + law transports (the ladder's edges)

**Files:**
- Modify: `internal/prelude/prelude.rune` (new section after the Task 3 laws)
- Modify: `internal/session/tower_hash_test.go` (presence)

**Interfaces:**
- Consumes: everything above; SemiringLaws slot layout (Global Constraints); RingLaws layout (rl.1 semiring laws, rl.2.1 negInvL, rl.2.2 negInvR).
- Produces (exact names later tasks + the chapter use): `magmaOfMonoid`, `monoidOfGroup`, `addMonoidOfSemiring`, `mulMonoidOfSemiring`, `groupOfRing`, the slot extractors `srlAddAssoc/srlAddComm/srlAddZeroL/srlAddZeroR/srlMulAssoc/srlMulOneL/srlMulOneR`, and the transports `addCommMonoidLawsOfSemiring`, `mulMonoidLawsOfSemiring`, `groupLawsOfRing`, `abGroupLawsOfRing`.

- [ ] **Step 1: failing presence test**

Sibling function `TestLadderBridgesPresent` checking `"groupOfRing", "addMonoidOfSemiring", "addCommMonoidLawsOfSemiring", "mulMonoidLawsOfSemiring", "groupLawsOfRing", "abGroupLawsOfRing"`. Run -> FAIL.

- [ ] **Step 2: bridges + extractors + transports**

```
-- ====================================================================
-- THE LADDER'S EDGES. Every classical containment is a FUNCTION with a
-- proven law transport: a ring IS an abelian group under addition and a
-- monoid under multiplication. Transports are pure reassembly - project
-- the law out of the source And-chain, rebuild the target chain. When
-- Int's RingLaws land (deferred campaign), groupLawsOfRing instantly
-- yields Int's AbGroup; the bridges are written once, here.
-- ====================================================================
magmaOfMonoid : (A : U) -> Monoid A -> Magma A is
  fn (A : U) (d : Monoid A) is d.1 end
end
monoidOfGroup : (A : U) -> Group A -> Monoid A is
  fn (A : U) (g : Group A) is g.1 end
end
addMonoidOfSemiring : (A : U) -> Semiring A -> Monoid A is
  fn (A : U) (s : Semiring A) is mkMonoid A (s.1) (s.2.1) end
end
mulMonoidOfSemiring : (A : U) -> Semiring A -> Monoid A is
  fn (A : U) (s : Semiring A) is mkMonoid A (s.2.2.1) (s.2.2.2) end
end
groupOfRing : (A : U) -> Ring A -> Group A is
  fn (A : U) (r : Ring A) is mkGroup A (addMonoidOfSemiring A (r.1)) (r.2) end
end

-- Named slot extractors for the SemiringLaws And-chain (nicer than raw
-- .2.2... trails at every use site; the chain order is documented at the
-- record). Only the slots the transports need are named.
srlAddAssoc : (A : U) -> (s : Semiring A) -> SemiringLaws A s -> AssocT A (s.1) is
  fn (A : U) (s : Semiring A) (l : SemiringLaws A s) is l.1 end
end
srlAddComm : (A : U) -> (s : Semiring A) -> SemiringLaws A s -> CommT A (s.1) is
  fn (A : U) (s : Semiring A) (l : SemiringLaws A s) is l.2.1 end
end
srlAddZeroL : (A : U) -> (s : Semiring A) -> SemiringLaws A s -> IdLT A (s.1) (s.2.1) is
  fn (A : U) (s : Semiring A) (l : SemiringLaws A s) is l.2.2.1 end
end
srlAddZeroR : (A : U) -> (s : Semiring A) -> SemiringLaws A s -> IdRT A (s.1) (s.2.1) is
  fn (A : U) (s : Semiring A) (l : SemiringLaws A s) is l.2.2.2.1 end
end
srlMulAssoc : (A : U) -> (s : Semiring A) -> SemiringLaws A s -> AssocT A (s.2.2.1) is
  fn (A : U) (s : Semiring A) (l : SemiringLaws A s) is l.2.2.2.2.1 end
end
srlMulOneL : (A : U) -> (s : Semiring A) -> SemiringLaws A s -> IdLT A (s.2.2.1) (s.2.2.2) is
  fn (A : U) (s : Semiring A) (l : SemiringLaws A s) is l.2.2.2.2.2.1 end
end
srlMulOneR : (A : U) -> (s : Semiring A) -> SemiringLaws A s -> IdRT A (s.2.2.1) (s.2.2.2) is
  fn (A : U) (s : Semiring A) (l : SemiringLaws A s) is l.2.2.2.2.2.2.1 end
end

addCommMonoidLawsOfSemiring : (A : U) -> (s : Semiring A) -> SemiringLaws A s -> CommMonoidLaws A (addMonoidOfSemiring A s) is
  fn (A : U) (s : Semiring A) (l : SemiringLaws A s) is
    mkAnd (MonoidLaws A (addMonoidOfSemiring A s)) (CommT A (s.1))
      (mkAnd (AssocT A (s.1))
        (And (IdLT A (s.1) (s.2.1)) (IdRT A (s.1) (s.2.1)))
        (srlAddAssoc A s l)
        (mkAnd (IdLT A (s.1) (s.2.1)) (IdRT A (s.1) (s.2.1)) (srlAddZeroL A s l) (srlAddZeroR A s l)))
      (srlAddComm A s l)
  end
end
mulMonoidLawsOfSemiring : (A : U) -> (s : Semiring A) -> SemiringLaws A s -> MonoidLaws A (mulMonoidOfSemiring A s) is
  fn (A : U) (s : Semiring A) (l : SemiringLaws A s) is
    mkAnd (AssocT A (s.2.2.1))
      (And (IdLT A (s.2.2.1) (s.2.2.2)) (IdRT A (s.2.2.1) (s.2.2.2)))
      (srlMulAssoc A s l)
      (mkAnd (IdLT A (s.2.2.1) (s.2.2.2)) (IdRT A (s.2.2.1) (s.2.2.2)) (srlMulOneL A s l) (srlMulOneR A s l))
  end
end
groupLawsOfRing : (A : U) -> (r : Ring A) -> RingLaws A r -> GroupLaws A (groupOfRing A r) is
  fn (A : U) (r : Ring A) (l : RingLaws A r) is
    mkAnd (MonoidLaws A (addMonoidOfSemiring A (r.1)))
      (And (NegInvLT A (r.1.1) (r.2) (r.1.2.1)) (NegInvRT A (r.1.1) (r.2) (r.1.2.1)))
      (mkAnd (AssocT A (r.1.1))
        (And (IdLT A (r.1.1) (r.1.2.1)) (IdRT A (r.1.1) (r.1.2.1)))
        (srlAddAssoc A (r.1) (l.1))
        (mkAnd (IdLT A (r.1.1) (r.1.2.1)) (IdRT A (r.1.1) (r.1.2.1)) (srlAddZeroL A (r.1) (l.1)) (srlAddZeroR A (r.1) (l.1))))
      (mkAnd (NegInvLT A (r.1.1) (r.2) (r.1.2.1)) (NegInvRT A (r.1.1) (r.2) (r.1.2.1)) (l.2.1) (l.2.2))
  end
end
abGroupLawsOfRing : (A : U) -> (r : Ring A) -> RingLaws A r -> AbGroupLaws A (groupOfRing A r) is
  fn (A : U) (r : Ring A) (l : RingLaws A r) is
    mkAnd (GroupLaws A (groupOfRing A r)) (CommT A (r.1.1))
      (groupLawsOfRing A r l)
      (srlAddComm A (r.1) (l.1))
  end
end
```

The target statements over projections (e.g. `(addMonoidOfSemiring A s).1`) must compute to the source shapes (`s.1`) definitionally - the mkMonoid Pair projections do so via Σ β. If the elaborator disagrees at any site, the fallback is stating the mkAnd type arguments over the PROJECTED forms instead; the extractor bodies stay `.1`/`.2` chains regardless. Report any fallback used.

- [ ] **Step 3: gates + commit**

Run: `go test ./internal/session/ ./internal/repl/ -count=1` -> PASS.

```bash
git add internal/prelude/prelude.rune internal/session/tower_hash_test.go
git commit -m "feat(prelude): ladder bridges + law transports (ring is an abelian group, semiring carries two monoids)" -- internal/prelude/prelude.rune internal/session/tower_hash_test.go
```

---

### Task 5: chapter ch571

**Files:**
- Create: `listings/ch571_full_ladder.rune`

**Interfaces:**
- Consumes: all prelude sections above as the SOURCE to mirror (byte-identical for shared definitions, the ch569/ch570 discipline). Listings are self-contained (no prelude; `builtin nat Whole zero succ` + kernels restated, NO accel registration lines).

- [ ] **Step 1: write the chapter**

`listings/ch571_full_ladder.rune`, ch107-style narration. Banner: the structural fact (laws-only rungs share ops shapes: Magma/Semigroup, Monoid/CommMonoid, Group/AbGroup, Ring/CommRing, DivRing/Field), the six-shapes + ten-law-records ladder diagram:

```
-- ops shapes:   Magma -> Monoid -> Group        Semiring -> Ring -> DivRing
-- law rungs:    Semigroup   CommMonoid  AbGroup    CommRing      Field
--               (each = the shape at its left + the named law record)
-- edges:        addMonoidOfSemiring / mulMonoidOfSemiring / groupOfRing
--               with proven law transports (a ring IS an abelian group).
```

Contents, mirrored verbatim from the prelude where shared: kernels (addW/mulW), Magma + `++`-style op story (no instance table in a listing - just the types), Monoid/mkMonoid, Group/mkGroup, Semiring/Ring/DivRing + builders, And/mkAnd + the formers (AssocT/CommT/IdLT/IdRT/NegInvLT/NegInvRT - only the ones this chapter uses), SemigroupLaws/MonoidLaws/CommMonoidLaws/GroupLaws/AbGroupLaws, the Whole add/mul lemma subset needed (addWAssoc/addWComm/addWZeroL/addWZeroR + helpers it depends on: addWSuccR, congW - copy the dependency-closed set from the prelude, verbatim), monoidWholeAdd + commMonoidLawsWholeAdd, the bridges + one transport spelled out (addCommMonoidLawsOfSemiring, over a locally defined semiringWhole + the SemiringLaws subset it needs - if pulling the FULL eleven-lemma set makes the chapter unwieldy, scope the chapter's transport demo to the CommMonoid rung and NARRATE groupOfRing/groupLawsOfRing with their types stated as comments; report the choice). End with a runnable witness, e.g. `main : Whole is (monoidWholeAdd.1) 2 2 end` (runs to 4).

- [ ] **Step 2: gates**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck' -count=1 -timeout 20m` -> PASS with ch571 swept (confirm via -v subtest output). Verify the witness runs: `go run ./cmd/rune run listings/ch571_full_ladder.rune main --target js` -> expected value.

- [ ] **Step 3: commit**

```bash
git add listings/ch571_full_ladder.rune
git commit -m "docs(listings): ch571 the full Magma-to-Field ladder" -- listings/ch571_full_ladder.rune
```

---

### Task 6: full suite + bookkeeping

**Files:**
- Modify: `docs/superpowers/plans/2026-07-06-three-majors-roadmap.md` (status), `docs/superpowers/specs/2026-07-06-v4-tower-hierarchy-design.md` (Decision 2 cross-reference note)

- [ ] **Step 1: full suite**

Run: `go test -timeout 30m ./...` (background + poll). ALL PASS.

- [ ] **Step 2: bookkeeping**

- In the 1a spec's Decision 2, append one line: "SUPERSEDED 2026-07-06 by author directive: the full Magma-to-Field ladder ships via 2026-07-06-v4-full-ladder-design.md (the 3 rungs remain as its upper shapes)."
- In the roadmap Status: after the sub-project 1 sentence, add "Sub-project 1b (full Magma-to-Field ladder: Magma rename, lower law records, Group/AbGroup, CommRing/Field bundles, bridges + transports, Whole's commutative monoids, ch571) implemented."

- [ ] **Step 3: commit**

```bash
git add docs/superpowers/plans/2026-07-06-three-majors-roadmap.md docs/superpowers/specs/2026-07-06-v4-tower-hierarchy-design.md
git commit -m "docs: full-ladder status + spec supersession note" -- docs/superpowers/plans/2026-07-06-three-majors-roadmap.md docs/superpowers/specs/2026-07-06-v4-tower-hierarchy-design.md
```
