# v4 tower graph + lawful class hierarchy Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the prelude's flat `Num` class with a lawful 3-rung ops ladder (Semiring/Ring/DivRing) + separate law records, prove Whole's semiring laws, land the tower conversion graph with intOf homomorphisms, and co-create the two book chapters.

**Architecture:** Prelude-only language changes (kernel frozen): ops classes are Σ-records in the established mkNum style, laws are And-chained named propositions over an ops instance, `+`/`*` re-dispatch through Semiring, Num is deleted. Chapters ch569/ch570 mirror the prelude self-contained. Session/repl tests pin dispatch, hashes, and REPL behavior.

**Tech Stack:** rune surface (internal/prelude/prelude.rune, listings/), Go tests (internal/session, internal/repl, harness).

## Global Constraints

- Spec: `docs/superpowers/specs/2026-07-06-v4-tower-hierarchy-design.md` (read it; it binds).
- KERNEL FROZEN: no edits under core/, store/, elaborate/, surface/, codegen/, quantity/, equality/. This plan touches ONLY internal/prelude/prelude.rune, listings/, internal/session tests, internal/repl tests.
- No hash-FORMAT bump; prelude definition hashes will change (expected, sessions rehash).
- REPL acceptance pins unchanged behavior: `1 + 1` = 2, `4000 * 4000` instant (accel fires), `1/3 + 2/3` = 1, `2 - 5` = -3 : Int, `-1/3` : Frac. Existing TestREPLTowerArithmetic / TestREPLIntTower / TestREPLNegationPromotes / TestREPLDecimalLiterals must stay green.
- `Num`, `mkNum`, `numWhole`, `numInt`, `numFrac` are DELETED (Rule 5), not kept alongside. No consumers exist outside the prelude (verified: listings are self-contained; no Go references).
- Laws staging per spec: SemiringLaws PROVEN for Whole only; RingLaws/DivRingLaws/CommLaws land as TYPES (+ builders), Int/Frac law instances deferred.
- Work on branch `feat/v4-tower-hierarchy` in a git worktree under `.worktrees/` (concurrent agents share this repo).
- Conventional Commits with explicit pathspecs; every commit message ends with the Co-Authored-By line: `Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>`.
- No em or en dashes anywhere (ASCII hyphen only), including comments and docs.
- Full `go test -timeout 30m ./...` before finishing the branch.

---

### Task 1: ops ladder classes + collision audit

**Files:**
- Modify: `internal/prelude/prelude.rune` (insert a new section AFTER the `Num` block, around line 109; Num itself is untouched in this task)
- Create: `internal/session/tower_hash_test.go`

**Interfaces:**
- Produces: `Semiring : U -> U` (Σ-record {add, zero, mul, one}), `mkSemiring : (A : U) -> (A -> A -> A) -> A -> (A -> A -> A) -> A -> Semiring A`, `Ring : U -> U` (Σ {semiring, neg}), `mkRing : (A : U) -> Semiring A -> (A -> A) -> Ring A`, `DivRing : U -> U` (Σ {ring, recip}), `mkDivRing : (A : U) -> Ring A -> (A -> A) -> DivRing A`. Projection layout consumed by ALL later tasks: for `s : Semiring A`, add = `s.1`, zero = `s.2.1`, mul = `s.2.2.1`, one = `s.2.2.2`; for `r : Ring A`, semiring = `r.1`, neg = `r.2`; for `d : DivRing A`, ring = `d.1`, recip = `d.2`.

- [ ] **Step 1: write the failing collision-audit test**

Create `internal/session/tower_hash_test.go`. Follow the package's existing session-test conventions (see typeclass_test.go for how a session is built and the prelude loaded; reuse its helper if one exists):

```go
package session

import (
	"testing"

	"goforge.dev/rune/v3/internal/prelude"
)

// TestTowerClassHashesDistinct is the collision audit the positional
// Sigma-record discipline demands: every class former in the tower must
// hash distinctly, or instance registration silently merges two classes
// onto one key (the Add/Mul trap the old Num comment documents).
func TestTowerClassHashesDistinct(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	names := []string{"Semiring", "Ring", "DivRing", "Monoid", "Div", "NegR", "SubR", "Binary", "Show"}
	seen := map[string]string{}
	for _, n := range names {
		h, ok := s.Lookup(n)
		if !ok {
			t.Fatalf("class %s not found in prelude", n)
		}
		key := h.String()
		if prev, dup := seen[key]; dup {
			t.Fatalf("hash collision: %s and %s share %s", prev, n, key)
		}
		seen[key] = n
	}
}
```

Adjust `New()` / `s.Lookup(n)` / `h.String()` to the session package's actual constructor and lookup signatures (read internal/session/session.go first; Lookup exists, it was used by cmd/rune/five_outputs_test.go as `session.Lookup("GC")`). Semantics must stay: load prelude, resolve each name to its definition hash, fail on any pairwise equality.

- [ ] **Step 2: run it to verify it fails**

Run: `go test ./internal/session/ -run TestTowerClassHashesDistinct -count=1 -v`
Expected: FAIL with `class Semiring not found in prelude`.

- [ ] **Step 3: add the ops ladder to the prelude**

Insert after the `*` operator definition (line 108) and before the SEMIGROUP/MONOID banner, a new section. Complete text to insert:

```
-- ====================================================================
-- THE ALGEBRAIC OPS LADDER (v4). Three Σ-record classes carrying OPERATIONS
-- ONLY: Semiring {add, zero, mul, one} -> Ring {+neg} -> DivRing {+recip}.
-- Laws live in SEPARATE records (SemiringLaws and friends, below), so a type
-- that can compute but not prove (IEEE754, later) still gets ops instances.
-- Superclass access is projection: a Ring contains its Semiring as .1.
-- POSITIONAL DISCIPLINE (the Num lesson): a class is identified by its
-- STRUCTURE, so two classes with identical field-type sequences collide.
-- Shapes here: Semiring A = Sig (A->A->A) (Sig A (Sig (A->A->A) A));
-- Ring A = Sig (Semiring A) (A->A); DivRing A = Sig (Ring A) (A->A) — all
-- pairwise distinct, gated by TestTowerClassHashesDistinct.
-- `recip` is TOTAL with the junk value recip zero = zero (Mathlib's move);
-- the true inverse law is guarded by a nonzero hypothesis laws-side.
-- ====================================================================
Semiring : U -> U is
  fn (A : U) is
    Sig (A -> A -> A) (fn (_add : A -> A -> A) is
      Sig A (fn (_zero : A) is
        Sig (A -> A -> A) (fn (_mul : A -> A -> A) is A end)
      end)
    end)
  end
end
mkSemiring : (A : U) -> (A -> A -> A) -> A -> (A -> A -> A) -> A -> Semiring A is
  fn (A : U) (a : A -> A -> A) (z : A) (m : A -> A -> A) (o : A) is
    Pair (A -> A -> A) (fn (_add : A -> A -> A) is
        Sig A (fn (_zero : A) is
          Sig (A -> A -> A) (fn (_mul : A -> A -> A) is A end)
        end)
      end)
      a
      (Pair A (fn (_zero : A) is
          Sig (A -> A -> A) (fn (_mul : A -> A -> A) is A end)
        end)
        z
        (Pair (A -> A -> A) (fn (_mul : A -> A -> A) is A end) m o))
  end
end

Ring : U -> U is
  fn (A : U) is Sig (Semiring A) (fn (_s : Semiring A) is A -> A end) end
end
mkRing : (A : U) -> Semiring A -> (A -> A) -> Ring A is
  fn (A : U) (s : Semiring A) (n : A -> A) is
    Pair (Semiring A) (fn (_s : Semiring A) is A -> A end) s n
  end
end

DivRing : U -> U is
  fn (A : U) is Sig (Ring A) (fn (_r : Ring A) is A -> A end) end
end
mkDivRing : (A : U) -> Ring A -> (A -> A) -> DivRing A is
  fn (A : U) (r : Ring A) (rc : A -> A) is
    Pair (Ring A) (fn (_r : Ring A) is A -> A end) r rc
  end
end
```

- [ ] **Step 4: run the audit + the prelude gates**

Run: `go test ./internal/session/ -run TestTowerClassHashesDistinct -count=1 -v` -> PASS.
Run: `go test ./internal/repl/ -count=1` -> PASS (prelude still loads, nothing rewired yet).

- [ ] **Step 5: commit**

```bash
git add internal/prelude/prelude.rune internal/session/tower_hash_test.go
git commit -m "feat(prelude): Semiring/Ring/DivRing ops ladder + class hash collision audit" -- internal/prelude/prelude.rune internal/session/tower_hash_test.go
```

---

### Task 2: rewire + and * through Semiring, delete Num

**Files:**
- Modify: `internal/prelude/prelude.rune` (three sites: the Num block lines ~96-108, the numFrac line ~454, the numInt line ~498, plus new instance/def lines at those rungs)
- Modify: `internal/repl/repl_test.go` (extend one existing tower test with dispatch pins)

**Interfaces:**
- Consumes: Task 1's classes and projection layout (add = `d.1`, mul = `d.2.2.1`).
- Produces: `instance semiringWhole : Semiring Whole`, `recipF : Frac -> Frac`, `ringInt : Ring Int`, `divRingFrac : DivRing Frac`, `instance semiringInt : Semiring Int`, `instance semiringFrac : Semiring Frac`. `+`/`*` now `{d : Semiring A}`-constrained. `Num`/`mkNum`/`numWhole`/`numInt`/`numFrac` GONE.

- [ ] **Step 1: extend the REPL pin test first**

In `internal/repl/repl_test.go`, find `TestREPLTowerArithmetic` (line ~230) and read how it feeds inputs and asserts outputs. Add to it (same style as its existing cases) pins for: `1 + 1` -> 2, `4000 * 4000` -> 16000000, `1/3 + 2/3` -> 1, `(1/2) * (2/3)` -> 1/3. If all four are already pinned there, add only what is missing; if nothing is missing, add one new case `7 * 6` -> 42 so this task still carries a test delta proving dispatch survives the rewire.

- [ ] **Step 2: run it to confirm the baseline is green**

Run: `go test ./internal/repl/ -run TestREPLTowerArithmetic -count=1 -v`
Expected: PASS (pre-rewire baseline; the point is it must STILL pass after).

- [ ] **Step 3: the rewire**

In `internal/prelude/prelude.rune`:

(a) Replace the whole Num block (the banner comment lines 80-95, `Num`, `mkNum`, `instance numWhole`, and the `+`/`*` definitions, lines 96-108) with:

```
-- ====================================================================
-- ARITHMETIC DISPATCH. `+` and `*` are Semiring-constrained: the elaborator
-- resolves the dictionary from the argument TYPE (class = type family,
-- instance = registered value; see the ops-ladder banner above). Whole,
-- Int, and Frac each register a Semiring instance below at their rungs.
-- Subtraction is NOT here: ℕ is not closed under it (2 − 5 has no whole
-- answer), so `-` PROMOTES (Whole → Int) via the result-typed `SubR` class
-- below; `/` likewise promotes to Frac; `// %` are integer ops at Whole.
-- ====================================================================
instance semiringWhole : Semiring Whole is mkSemiring Whole addW zero mulW (succ zero) end

+ : {A : U} -> {d : Semiring A} -> A -> A -> A is
  fn {A : U} {d : Semiring A} (x : A) (y : A) is (d.1) x y end
end
* : {A : U} -> {d : Semiring A} -> A -> A -> A is
  fn {A : U} {d : Semiring A} (x : A) (y : A) is (d.2.2.1) x y end
end
```

NOTE the ordering constraint: the ops-ladder section from Task 1 must sit ABOVE this block (it does, if inserted at the old line-109 position - move the Task 1 section up to replace where the Num banner began if needed; final order must be: kernels, ops ladder, dispatch block, Semigroup/Monoid, ...).

(b) Replace line 454 `instance numFrac : Num Frac is mkNum Frac addF mulF end` with:

```
-- Frac's full rung is a DivRing: reciprocal swaps numerator and denominator
-- (keeping the sign); recip of zero is the junk value zero itself (0/1),
-- per the ops-ladder convention. A reduced fraction swaps to a reduced
-- fraction, so no re-reduce is needed.
recipF : Frac -> Frac is
  fn (f : Frac) is
    case fnum f of
    | zero -> f
    | succ k -> frac (fneg f) (fden f) (fnum f)
    end
  end
end
divRingFrac : DivRing Frac is
  mkDivRing Frac
    (mkRing Frac (mkSemiring Frac addF (frac false zero (succ zero)) mulF (frac false (succ zero) (succ zero))) fnegate)
    recipF
end
instance semiringFrac : Semiring Frac is divRingFrac.1.1 end
```

(c) Replace line 498 `instance numInt : Num Int is mkNum Int iadd imul end` with:

```
-- Int's full rung is a Ring (ℤ is not closed under reciprocal; division
-- promotes to Frac via divIF below).
ringInt : Ring Int is
  mkRing Int (mkSemiring Int iadd (int false zero) imul (int false (succ zero))) ineg
end
instance semiringInt : Semiring Int is ringInt.1 end
```

Ordering note for (b)/(c): `recipF`/`divRingFrac`/`semiringFrac` need `fnegate` (line 424), which sits ABOVE the old numFrac site (line 454), so replacing in place works. `ringInt`/`semiringInt` need `ineg` (line 470), above the old numInt site (line 498) - in place works.

- [ ] **Step 4: full REPL + session gates**

Run: `go test ./internal/repl/ ./internal/session/ -count=1`
Expected: ALL PASS, including TestREPLTowerArithmetic (with the Step-1 additions), TestREPLIntTower, TestREPLNegationPromotes, TestREPLDecimalLiterals, TestREPLPrelude, and the Task 1 audit. If a decimal or negation test fails, the dispatch rewiring broke promotion interplay - fix the prelude, not the test.

- [ ] **Step 5: sweep for stragglers, then wider gates**

Run: `grep -rn "mkNum\|numWhole\|numInt\|numFrac" --include="*.rune" --include="*.go" . | grep -v .worktrees` - expect ZERO hits (pre-verified; if any appear, migrate them to the Semiring names).
Run: `go test ./cmd/... ./harness/ -run 'TestExamples|TestFiveOutputs|TestREPL|Explain' -count=1 -timeout 20m`
Expected: PASS. If `rune explain` goldens (cmd/rune/testdata/explain) drift because `+`'s body shape changed, regenerate/hand-fix the goldens to the new honest rendering and include them in the commit.

- [ ] **Step 6: commit**

```bash
git add internal/prelude/prelude.rune internal/repl/repl_test.go
git commit -m "feat(prelude)!: + and * dispatch through Semiring; Num deleted (Rule 5)" -- internal/prelude/prelude.rune internal/repl/repl_test.go
# add cmd/rune/testdata/explain to the pathspec iff goldens changed
```

---

### Task 3: law statement formers + SemiringLaws + the Whole proofs

**Files:**
- Modify: `internal/prelude/prelude.rune` (new section AFTER the SubR block, ~line 540, so Int/Frac types are in scope for later tasks; only Whole is used here)
- Modify: `internal/session/tower_hash_test.go` (add a laws-presence test)

**Interfaces:**
- Consumes: Task 1 layout (`s.1` add, `s.2.1` zero, `s.2.2.1` mul, `s.2.2.2` one), Task 2's `semiringWhole`.
- Produces: `And : U -> U -> U`, `mkAnd : (P : U) -> (Q : U) -> P -> Q -> And P Q`, statement formers `AssocT`, `CommT`, `IdLT`, `IdRT`, `DistribLT`, `DistribRT`, `AnnihLT`, `AnnihRT` (exact shapes in Step 3), `SemiringLaws : (A : U) -> Semiring A -> U`, and the PROVEN `semiringLawsWhole : SemiringLaws Whole semiringWhole`. Also the eleven named Whole lemmas (Step 4 list) which Task 5's chapter mirrors.

- [ ] **Step 1: failing presence test**

Add to `internal/session/tower_hash_test.go`:

```go
// TestWholeSemiringLawsPresent pins that the prelude PROVES Whole's
// semiring laws (the proven tier of the v4 hierarchy): the laws value
// must exist and elaborate against the laws record over semiringWhole.
func TestWholeSemiringLawsPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{"SemiringLaws", "semiringLawsWhole", "addWAssoc", "addWComm", "mulWAssoc", "distribWL"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}
```

(Same signature adjustments as Task 1. The elaboration guarantee is implicit: LoadSource type checks everything it loads.)

- [ ] **Step 2: run it to verify it fails**

Run: `go test ./internal/session/ -run TestWholeSemiringLawsPresent -count=1 -v`
Expected: FAIL with `SemiringLaws not found`.

- [ ] **Step 3: the statement formers + the record**

Insert into the prelude (new banner section after SubR, ~line 540):

```
-- ====================================================================
-- LAW STATEMENTS (v4). Reusable propositions over raw operations; the law
-- RECORDS chain them with a non-dependent And. Laws are SEPARATE from the
-- ops classes so proof-free carriers (IEEE754, later) still get ops.
-- ====================================================================
And : U -> U -> U is fn (P : U) (Q : U) is Sig P (fn (_p : P) is Q end) end end
mkAnd : (P : U) -> (Q : U) -> P -> Q -> And P Q is
  fn (P : U) (Q : U) (p : P) (q : Q) is Pair P (fn (_p : P) is Q end) p q end
end

AssocT : (A : U) -> (A -> A -> A) -> U is
  fn (A : U) (f : A -> A -> A) is (a : A) -> (b : A) -> (c : A) -> Eq A (f (f a b) c) (f a (f b c)) end
end
CommT : (A : U) -> (A -> A -> A) -> U is
  fn (A : U) (f : A -> A -> A) is (a : A) -> (b : A) -> Eq A (f a b) (f b a) end
end
IdLT : (A : U) -> (A -> A -> A) -> A -> U is
  fn (A : U) (f : A -> A -> A) (e : A) is (a : A) -> Eq A (f e a) a end
end
IdRT : (A : U) -> (A -> A -> A) -> A -> U is
  fn (A : U) (f : A -> A -> A) (e : A) is (a : A) -> Eq A (f a e) a end
end
DistribLT : (A : U) -> (A -> A -> A) -> (A -> A -> A) -> U is
  fn (A : U) (add : A -> A -> A) (mul : A -> A -> A) is
    (a : A) -> (b : A) -> (c : A) -> Eq A (mul a (add b c)) (add (mul a b) (mul a c))
  end
end
DistribRT : (A : U) -> (A -> A -> A) -> (A -> A -> A) -> U is
  fn (A : U) (add : A -> A -> A) (mul : A -> A -> A) is
    (a : A) -> (b : A) -> (c : A) -> Eq A (mul (add a b) c) (add (mul a c) (mul b c))
  end
end
AnnihLT : (A : U) -> (A -> A -> A) -> A -> U is
  fn (A : U) (mul : A -> A -> A) (z : A) is (a : A) -> Eq A (mul z a) z end
end
AnnihRT : (A : U) -> (A -> A -> A) -> A -> U is
  fn (A : U) (mul : A -> A -> A) (z : A) is (a : A) -> Eq A (mul a z) z end
end

-- The semiring laws over an ops instance: additive commutative monoid,
-- multiplicative monoid, distributivity both sides, zero annihilates.
SemiringLaws : (A : U) -> Semiring A -> U is
  fn (A : U) (s : Semiring A) is
    And (AssocT A (s.1))
    (And (CommT A (s.1))
    (And (IdLT A (s.1) (s.2.1))
    (And (IdRT A (s.1) (s.2.1))
    (And (AssocT A (s.2.2.1))
    (And (IdLT A (s.2.2.1) (s.2.2.2))
    (And (IdRT A (s.2.2.1) (s.2.2.2))
    (And (DistribLT A (s.1) (s.2.2.1))
    (And (DistribRT A (s.1) (s.2.2.1))
    (And (AnnihLT A (s.2.2.1) (s.2.1))
         (AnnihRT A (s.2.2.1) (s.2.1)))))))))))
  end
end
```

- [ ] **Step 4: the eleven Whole lemmas + assembly**

Prove, in the same section, these lemmas over the prelude kernels (addW recurses on its FIRST argument by `case ... with ih`; mulW steps by `addW ih n`). Use `WholeElim` with an explicit motive plus `subst`/`symEq`/`transEq` (already in the prelude, lines 150-159) exactly as the existing subWZeroL/subWSucc proofs do (lines 169-193). Shape guide: listings/ch107_int_ring_complete.rune lines 60-240 proves the same statements over its own data Nat whose `add` has the same first-argument recursion; its `mul` steps by `add n ih` (flipped operand order vs mulW's `addW ih n`), so adapt the mul proofs to mulW's actual step rather than transcribing.

Names and exact statements (each `(a b c : Whole)` spelled as separate Pis, matching the formers):

1. `addWAssoc : AssocT Whole addW`
2. `addWComm : CommT Whole addW` (needs helpers `addWZeroR` first and an `addWSuccR : (m n : Whole) -> Eq Whole (addW m (succ n)) (succ (addW m n))`, mirroring ch107's addZeroR/addSuccR; addWZeroL is definitional - `addW zero n` computes to n - so `IdLT` will be `fn (a : Whole) is refl a end`)
3. `addWZeroL : IdLT Whole addW zero` (refl, see above)
4. `addWZeroR : IdRT Whole addW zero` (induction)
5. `mulWAssoc : AssocT Whole mulW`
6. `mulWOneL : IdLT Whole mulW (succ zero)` (mulW (succ zero) n computes to addW (mulW zero n) n = addW zero n = n... verify by writing it as refl first; if the case tree does not reduce it definitionally, prove by the zero case + addWZeroL)
7. `mulWOneR : IdRT Whole mulW (succ zero)` (induction)
8. `distribWL : DistribLT Whole addW mulW` (typically via distribWR + mulWComm, or directly; ch107 does right-distributivity first - follow whichever order makes the inductions go through, but BOTH lemmas must exist with the stated types)
9. `distribWR : DistribRT Whole addW mulW`
10. `mulWZeroL : AnnihLT Whole mulW zero` (definitional: mulW zero a computes to 0 - refl)
11. `mulWZeroR : AnnihRT Whole mulW zero` (induction)

A `mulWComm : CommT Whole mulW` helper is expected to be needed for the distributivity pair (ch107 needs it); define it as a named lemma too (it is chapter material even though SemiringLaws does not include it - it feeds CommLaws for Whole, which DOES land here as the one free CommLaws instance: add `commLawsWhole` after Task 4 defines CommLaws... NO - keep it a plain named lemma in THIS task; Task 4 assembles `commLawsWhole`).

Then assemble:

```
semiringLawsWhole : SemiringLaws Whole semiringWhole is
  mkAnd (AssocT Whole addW) (And (CommT Whole addW) (And (IdLT Whole addW zero) (And (IdRT Whole addW zero) (And (AssocT Whole mulW) (And (IdLT Whole mulW (succ zero)) (And (IdRT Whole mulW (succ zero)) (And (DistribLT Whole addW mulW) (And (DistribRT Whole addW mulW) (And (AnnihLT Whole mulW zero) (AnnihRT Whole mulW zero))))))))))
    addWAssoc
    (mkAnd ... )
```

The nested mkAnd chain is mechanical but verbose; write it fully (10 mkAnd applications, innermost pairing mulWZeroL with mulWZeroR). IMPORTANT: the record is stated over `semiringWhole`, whose projections compute to addW/mulW/zero/succ zero definitionally, so lemmas stated over the raw kernels check against the projected statements on the nose - if the elaborator disagrees, state the lemmas over the projections instead (`semiringWhole.1` etc.) and keep the raw-kernel names as the bodies.

CAUTION (rune strict case arms, a known trap): `case` arms evaluate eagerly - in inductive steps put the `ih` use in exactly one arm and precompute any shared subterms with `let` outside the case, or elaboration cost explodes.

- [ ] **Step 5: run the gates**

Run: `go test ./internal/session/ -run 'TestWholeSemiringLawsPresent|TestTowerClassHashesDistinct' -count=1 -v` -> PASS.
Run: `go test ./internal/repl/ -count=1` -> PASS (prelude load time will grow; if TestREPLPrelude or session startup becomes pathologically slow, >30s, STOP and report - the proof bodies may be hitting an elaboration cliff and the controller must decide between optimizing proofs and moving laws to a listing).

- [ ] **Step 6: commit**

```bash
git add internal/prelude/prelude.rune internal/session/tower_hash_test.go
git commit -m "feat(prelude): law statement formers + SemiringLaws + proven Whole semiring laws" -- internal/prelude/prelude.rune internal/session/tower_hash_test.go
```

---

### Task 4: RingLaws / DivRingLaws / CommLaws types + Whole CommLaws

**Files:**
- Modify: `internal/prelude/prelude.rune` (extend the laws section)
- Modify: `internal/session/tower_hash_test.go` (extend the presence test)

**Interfaces:**
- Consumes: Task 3's formers/And/SemiringLaws and `mulWComm`; Task 1's projection layout.
- Produces: `NegInvLT`, `NegInvRT`, `RecipInvLT`, `RecipInvRT`, `RingLaws : (A : U) -> Ring A -> U`, `DivRingLaws : (A : U) -> DivRing A -> (A -> U) -> U`, `CommLaws : (A : U) -> Semiring A -> U`, `commLawsWhole : CommLaws Whole semiringWhole`. NO Int/Frac law instances (spec staging).

- [ ] **Step 1: extend the presence test (failing)**

Add names `"RingLaws", "DivRingLaws", "CommLaws", "commLawsWhole"` to the loop in TestWholeSemiringLawsPresent (or a sibling test function TestLawRecordTypesPresent with the same body shape).

Run: `go test ./internal/session/ -run TestWholeSemiringLawsPresent -count=1 -v` (or the sibling) -> FAIL with `RingLaws not found`.

- [ ] **Step 2: the records**

Append to the laws section:

```
NegInvLT : (A : U) -> (A -> A -> A) -> (A -> A) -> A -> U is
  fn (A : U) (add : A -> A -> A) (neg : A -> A) (z : A) is (a : A) -> Eq A (add (neg a) a) z end
end
NegInvRT : (A : U) -> (A -> A -> A) -> (A -> A) -> A -> U is
  fn (A : U) (add : A -> A -> A) (neg : A -> A) (z : A) is (a : A) -> Eq A (add a (neg a)) z end
end
RecipInvLT : (A : U) -> (A -> A -> A) -> (A -> A) -> A -> (A -> U) -> U is
  fn (A : U) (mul : A -> A -> A) (rc : A -> A) (o : A) (nz : A -> U) is
    (x : A) -> nz x -> Eq A (mul (rc x) x) o
  end
end
RecipInvRT : (A : U) -> (A -> A -> A) -> (A -> A) -> A -> (A -> U) -> U is
  fn (A : U) (mul : A -> A -> A) (rc : A -> A) (o : A) (nz : A -> U) is
    (x : A) -> nz x -> Eq A (mul x (rc x)) o
  end
end

-- Ring laws = semiring laws over the embedded semiring + additive inverses.
RingLaws : (A : U) -> Ring A -> U is
  fn (A : U) (r : Ring A) is
    And (SemiringLaws A (r.1))
    (And (NegInvLT A (r.1.1) (r.2) (r.1.2.1))
         (NegInvRT A (r.1.1) (r.2) (r.1.2.1)))
  end
end

-- DivRing laws take a nonzero PREDICATE nz (each carrier states nonzeroness
-- its own way; the prelude has no ambient negation type). recip zero is junk
-- by convention, so the inverse laws are guarded by nz.
DivRingLaws : (A : U) -> DivRing A -> (A -> U) -> U is
  fn (A : U) (d : DivRing A) (nz : A -> U) is
    And (RingLaws A (d.1))
    (And (RecipInvLT A (d.1.1.2.2.1) (d.2) (d.1.1.2.2.2) nz)
         (RecipInvRT A (d.1.1.2.2.1) (d.2) (d.1.1.2.2.2) nz))
  end
end

-- Commutativity is its own record: a commutative ring is Ring + RingLaws +
-- CommLaws; a field is DivRing + DivRingLaws + CommLaws; Quaternion (later)
-- is a DivRing withOUT CommLaws. Whole's instance is free from mulWComm.
CommLaws : (A : U) -> Semiring A -> U is
  fn (A : U) (s : Semiring A) is CommT A (s.2.2.1) end
end
commLawsWhole : CommLaws Whole semiringWhole is mulWComm end
```

(Projection spellings: for `r : Ring A`, add = `r.1.1`, zero = `r.1.2.1`; for `d : DivRing A`, mul = `d.1.1.2.2.1`, one = `d.1.1.2.2.2`. Double-check against the Task 1 layout before committing; a wrong projection is a type error, so elaboration is the gate.)

- [ ] **Step 3: run the gates**

Run: `go test ./internal/session/ ./internal/repl/ -count=1` -> PASS.

- [ ] **Step 4: commit**

```bash
git add internal/prelude/prelude.rune internal/session/tower_hash_test.go
git commit -m "feat(prelude): RingLaws/DivRingLaws/CommLaws records + Whole CommLaws" -- internal/prelude/prelude.rune internal/session/tower_hash_test.go
```

---

### Task 5: the tower graph (ratOfInt + intOf homomorphisms + coherence)

**Files:**
- Modify: `internal/prelude/prelude.rune` (new section after the laws section)
- Modify: `internal/session/tower_hash_test.go` (presence pins)

**Interfaces:**
- Consumes: `intOf`, `fracOf`, `mkInt`, `iadd`, `imul`, `addW`, `mulW`, `subst`/`symEq` from the prelude; Task 3 formers not needed here.
- Produces: `ratOfInt : Int -> Frac`, `mkIntFalse : (m : Whole) -> Eq Int (mkInt false m) (int false m)`, `intOfAdd : (a : Whole) -> (b : Whole) -> Eq Int (iadd (intOf a) (intOf b)) (intOf (addW a b))`, `intOfMul` (same shape over imul/mulW), `intOfOne : Eq Int (intOf (succ zero)) (int false (succ zero))`, `ratOfIntOf : (n : Whole) -> Eq Frac (ratOfInt (intOf n)) (fracOf n)`.

- [ ] **Step 1: failing presence pins**

Add `"ratOfInt", "mkIntFalse", "intOfAdd", "intOfMul", "ratOfIntOf"` to the presence test (new sibling function `TestTowerGraphLemmasPresent`, same body shape as Task 3 Step 1).

Run: `go test ./internal/session/ -run TestTowerGraphLemmasPresent -count=1 -v` -> FAIL.

- [ ] **Step 2: the graph section**

```
-- ====================================================================
-- THE TOWER GRAPH (v4). Named injections up the tower with homomorphism
-- certificates. Edges: wholeOf (Nat -> Whole, the projection, above),
-- intOf (Whole -> Int), fracOf (Whole -> Frac), ratOfInt (Int -> Frac).
-- The intOf edge preserves + * 1 (proven below via the mkInt normalization
-- lemma); the Frac edges' homomorphisms need gcd/reduce theory and ship
-- with the Frac laws campaign. The triangle ratOfInt . intOf = fracOf is
-- DEFINITIONAL (both sides are frac false n 1).
-- ====================================================================
ratOfInt : Int -> Frac is
  fn (i : Int) is frac (isign i) (imag i) 1 end
end

-- mkInt on an explicit false sign is the bare constructor, whatever the
-- magnitude (mkInt only normalizes -0; +0 is already normal).
mkIntFalse : (m : Whole) -> Eq Int (mkInt false m) (int false m) is
  fn (m : Whole) is
    case m of
    | zero -> refl (int false zero)
    | succ k -> refl (int false (succ k))
    end
  end
end

-- iadd on two non-negative ints computes to mkInt false (addW a b): the
-- sign xor is false, so the like-signs arm fires; mkIntFalse finishes.
intOfAdd : (a : Whole) -> (b : Whole) -> Eq Int (iadd (intOf a) (intOf b)) (intOf (addW a b)) is
  fn (a : Whole) (b : Whole) is mkIntFalse (addW a b) end
end
intOfMul : (a : Whole) -> (b : Whole) -> Eq Int (imul (intOf a) (intOf b)) (intOf (mulW a b)) is
  fn (a : Whole) (b : Whole) is mkIntFalse (mulW a b) end
end
intOfOne : Eq Int (intOf (succ zero)) (int false (succ zero)) is
  refl (int false (succ zero))
end

-- The commuting triangle, by refl: ratOfInt (intOf n) = frac false n 1 = fracOf n.
ratOfIntOf : (n : Whole) -> Eq Frac (ratOfInt (intOf n)) (fracOf n) is
  fn (n : Whole) is refl (frac false n 1) end
end
```

If `intOfAdd`'s body does not check as the bare `mkIntFalse (addW a b)` (i.e. the case tree does not reduce definitionally against open a, b), the fallback is a case split on `xorB false false` being definitional - it IS (`xorB false false` computes through `case false of ...` to `false`), so the like-signs arm `mkInt false (addW (imag (int false a)) (imag (int false b)))` reduces with `imag (int false a) ~> a`; if any single reduction sticks, insert a `transEq` step through the stuck point. Report which form checked in the task report. Similarly `mkIntFalse` case-with-Eq-motive: if the elaborator needs the motive spelled, use `WholeElim (fn (x : Whole) is Eq Int (mkInt false x) (int false x) end) (refl (int false zero)) (fn (k : Whole) (_ih : ...) is refl (int false (succ k)) end) m` (the step ignores its IH; both branches are refl).

- [ ] **Step 3: run the gates**

Run: `go test ./internal/session/ ./internal/repl/ -count=1` -> PASS.

- [ ] **Step 4: commit**

```bash
git add internal/prelude/prelude.rune internal/session/tower_hash_test.go
git commit -m "feat(prelude): tower graph - ratOfInt, intOf homomorphisms, coherence triangle" -- internal/prelude/prelude.rune internal/session/tower_hash_test.go
```

---

### Task 6: the two chapters (doctrine flip applied)

**Files:**
- Create: `listings/ch569_algebra_hierarchy.rune`
- Create: `listings/ch570_tower_graph.rune`
- Test: the existing harness sweep (harness/listings_test.go reads the listings dir automatically; no test-file edit expected)

**Interfaces:**
- Consumes: the prelude sections from Tasks 1-5 as the SOURCE to mirror. Listings are SELF-CONTAINED (they do not load the prelude): every needed definition is restated verbatim in the chapter, the ch107 pattern.

- [ ] **Step 1: ch569 - the algebra hierarchy chapter**

Write `listings/ch569_algebra_hierarchy.rune`, self-contained, with the book-narration comment style of ch107 (banner explaining WHY ops split from laws - the IEEE754 argument; the positional-hash discipline; recip-total junk convention). Contents, restated from the prelude with `builtin nat Whole zero succ` + addW/mulW kernels at the top (copy the prelude's kernel text verbatim, WITHOUT the `builtin natAdd`/`builtin natMul` accel lines - listings that predate the prelude do not register accels; check a recent prelude-era listing like ch559 for whether accel registration is used in listings and match that convention):
- Semiring/mkSemiring, Ring/mkRing, DivRing/mkDivRing
- instance semiringWhole + `+`/`*` dispatch + a use: `four : Whole is 2 + 2 end` and a GENERIC demo `square : (A : U) -> Semiring A -> A -> A is fn (A : U) (s : Semiring A) (x : A) is (s.2.2.1) x x end end` applied at Whole
- And/mkAnd + the eight statement formers + SemiringLaws
- the eleven Whole lemmas + semiringLawsWhole (copy the proven bodies from the prelude)
- RingLaws/DivRingLaws/CommLaws + commLawsWhole
The final definition should be a runnable witness, e.g. `main : Whole is square Whole semiringWhole 6 end` (runs to 36) so the chapter enters the run gate, matching how recent chapters end with a computable value.

- [ ] **Step 2: ch570 - the tower graph chapter**

Write `listings/ch570_tower_graph.rune`, self-contained: kernels, Nat-as-subset + wholeOf, Int + intOf/mkInt/iadd/imul, Frac + fracOf, ratOfInt, then mkIntFalse/intOfAdd/intOfMul/intOfOne/ratOfIntOf with the graph diagram in a banner comment:

```
--        Nat --wholeOf--> Whole --intOf--> Int
--                           \               |
--                          fracOf        ratOfInt
--                             \             |
--                              v            v
--                               Frac  (triangle commutes: ratOfIntOf)
```

End with a runnable witness, e.g. `main : Frac is ratOfInt (intOf 3) end`.

- [ ] **Step 3: run the listings gates**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck|TestListingsRun' -count=1 -timeout 20m`
Expected: PASS with the two new chapters swept in. If TestListingsRun requires a specific entry name or skip-list registration, read the test's conventions (lines 240-470) and conform.

- [ ] **Step 4: commit**

```bash
git add listings/ch569_algebra_hierarchy.rune listings/ch570_tower_graph.rune
git commit -m "docs(listings): ch569 algebra hierarchy + ch570 tower graph chapters" -- listings/ch569_algebra_hierarchy.rune listings/ch570_tower_graph.rune
```

---

### Task 7: full suite + bookkeeping

**Files:**
- Modify: `docs/superpowers/plans/2026-07-06-three-majors-roadmap.md` (status line)
- Test: everything

- [ ] **Step 1: full suite**

Run: `go test -timeout 30m ./...` (background the run and poll). ALL PASS. Fix any straggler this branch broke (explain goldens, examples) before proceeding; anything failing that is PRE-EXISTING on main gets reported, not fixed here.

- [ ] **Step 2: roadmap status**

In `docs/superpowers/plans/2026-07-06-three-majors-roadmap.md`, replace the Status paragraph's v4 sentence with: "v4 sub-project 1 (tower graph + lawful hierarchy) implemented on feat/v4-tower-hierarchy: Semiring/Ring/DivRing + law records, Whole laws proven, Num deleted, intOf homomorphisms + coherence triangle, chapters ch569/ch570. Next v4 sub-specs: Int/Frac law campaigns, new types, literals, REPL compiled-eval, perf gate."

- [ ] **Step 3: commit**

```bash
git add docs/superpowers/plans/2026-07-06-three-majors-roadmap.md
git commit -m "docs(plans): three-majors roadmap - v4 sub-project 1 status" -- docs/superpowers/plans/2026-07-06-three-majors-roadmap.md
```
