# v4 Float Track A: Guarded-Tier Instances + Semantic Parity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make IEEE754 `Float` the first guarded-tier numeric type - `+ - * le compare eqb` all work on Float via the tower's existing ops classes with NO laws records - and prove all four numeric types (Whole/Int/Frac/Float) agree on arithmetic and comparison byte-identically across all 9 backends.

**Architecture:** Pure prelude additions atop the existing `Std.Float` module and the shipped Semiring/SubR/Ord/DecEq ops classes. One `foreign fleqN` declaration whose backend bodies already exist; everything else (`fle`/`feq`/`isNaN`/`totalCompare` and the four instances) derives from `fleqN` + existing arithmetic. No new core, no new codegen, no new classes, kernel frozen. Acceptance is a 9-backend semantic parity conformance gate emitting only discrete results (Float verified by comparison, never by printing a float - display parity is Track B).

**Tech Stack:** rune prelude (internal/prelude/prelude.rune), the listings corpus, the harness multi-backend conformance framework (harness/comparison_conformance_test.go is the template), `rune repl`.

## Global Constraints

- KERNEL FROZEN: no edits to core/, store/, elaborate/. No new codegen. No hash bump. Track A is prelude + tests + one listing only.
- Float has NO laws records: do NOT add `SemiringLaws Float`, `OrdLaws Float`, `OrderedSemiring Float`, or any DecEq/Ord law for Float. The point is that the laws do not hold (NaN breaks order; float `+` is non-associative). Each instance carries a comment naming the violated law.
- No `FloatLit`, no numeric-literal change: Float values are built from `fromNat` + arithmetic. `3.14` still means exact `Frac`.
- No new float primitives: only the existing `fadd`/`fsub`/`fmul`/`fdiv`/`fromNat`/`fleqN` (the last already baked 9-way; this plan only DECLARES it in the prelude).
- Float ARITHMETIC correctness is verified by COMPARISON (feq), never by printing a float. The parity corpus emits only discrete results (Bool tag, Ordering tag, printNat count). Display parity is Track B.
- NO em-dashes or en-dashes anywhere (code, comments, docs, commit messages). Hyphens only. Dash-scan changed files before committing.
- Conventional Commits; every commit message ends with `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- Commit with explicit pathspecs only; work in a dedicated git worktree off `main`.
- A rune feature is not done until it works in `rune repl` (REPL acceptance is mandatory, Task 3).
- Full `go test -timeout 30m ./...` green before the tag (controller runs it at finishing).

## Verified facts (grounding, 2026-07-10)

- Class constructors (exact arg order): `mkSemiring : (A) -> (A->A->A add) -> (A zero) -> (A->A->A mul) -> (A one) -> Semiring A` (prelude:103; e.g. `mkSemiring Whole addW zero mulW (succ zero)`). `mkOrd : (A) -> (A->A->Bool) -> (A->A->Ordering) -> Ord A` (5031). `mkDecEq : (A) -> (A->A->Bool) -> DecEq A` (5014). `compareFromLe : (A) -> (A->A->Bool) -> A -> A -> Ordering` (5053). `SubR : U -> U -> U` with `instance subWhole : SubR Int Whole is wsub` (551).
- Helpers: `isZero : Whole -> Bool` (223), `notB : Bool -> Bool` (7386), `and : Bool -> Bool -> Bool` (6859), `isPos : Whole -> Bool` (379). `data Ordering is lt | eq | gt end`. Accessors `leOf`/`compareOf` (5036/5039), `eqbOf` (5019).
- `Std.Float` module (8720): `Float : U`, `fromNat : Whole -> Float`, `fadd`/`fsub`/`fmul`/`fdiv : Float -> Float -> Float`, `parseFloat`, `getFloat`, `printFloat`. NO comparison declared.
- `fleqN : Float -> Float -> Whole` (1 if a<=b else 0; NaN yields 0) is baked on all 9 backends (codegen/ioprims.go:177 + per-backend bodies) but NOT declared in the prelude.
- Conformance template: harness/comparison_conformance_test.go loads `prelude.Source()` + a corpus string, emits per backend via `bibleBackends()`, runs, asserts a fixed expected literal byte-identical, with env-gated skips.

---

### Task 1: Prelude Float guarded-tier section

**Files:**
- Modify: `internal/prelude/prelude.rune` (add `foreign fleqN` inside the `Std.Float` module; add a FLOAT GUARDED TIER section inside the module after the foreigns)
- Test: `internal/session/float_guarded_test.go` (create)

**Interfaces:**
- Produces (all inside module `Std.Float`, so qualified `Std.Float.<name>`; instances register by type-head regardless of name):
  - `fleqN : Float -> Float -> Whole` (foreign)
  - `fle : Float -> Float -> Bool`, `feq : Float -> Float -> Bool`, `isNaN : Float -> Bool`, `totalCompare : Float -> Float -> Ordering`
  - `instance semiringFloat : Semiring Float`, `instance subRFloat : SubR Float Float`, `instance ordFloat : Ord Float`, `instance decEqFloat : DecEq Float`
- Consumed by Task 2 (parity corpus) and Task 3 (ch577 + REPL).

- [ ] **Step 1: Write the failing test**

Create `internal/session/float_guarded_test.go`. It loads the prelude and checks that Float comparison/equality/NaN compute to the right discrete values, and that the operators resolve the Float instances. Model the load + normalize helpers on the existing session tests (grep `internal/session` for how a test loads `prelude.Source()` and normalizes an expression to a string, e.g. the tower/REPL tests; reuse that helper).

```go
package session

import "testing"

// Float guarded-tier: derived comparison/equality/NaN compute correctly, and the
// operator instances resolve on Float. Values are built from fromNat + arithmetic
// (no FloatLit). NaN is produced by fdiv 0 0.
func TestFloatGuardedTier(t *testing.T) {
	cases := []struct{ expr, want string }{
		// fle 1.0 2.0 = true ; fle 2.0 1.0 = false
		{"Std.Float.fle (Std.Float.fromNat (succ zero)) (Std.Float.fromNat (succ (succ zero)))", "true"},
		{"Std.Float.fle (Std.Float.fromNat (succ (succ zero))) (Std.Float.fromNat (succ zero))", "false"},
		// feq 1.0 1.0 = true
		{"Std.Float.feq (Std.Float.fromNat (succ zero)) (Std.Float.fromNat (succ zero))", "true"},
		// NaN: feq nan nan = false, isNaN nan = true, where nan = fdiv 0 0
		{"Std.Float.feq (Std.Float.fdiv (Std.Float.fromNat zero) (Std.Float.fromNat zero)) (Std.Float.fdiv (Std.Float.fromNat zero) (Std.Float.fromNat zero))", "false"},
		{"Std.Float.isNaN (Std.Float.fdiv (Std.Float.fromNat zero) (Std.Float.fromNat zero))", "true"},
		// totalCompare puts NaN last: totalCompare 1.0 nan = lt ; totalCompare nan 1.0 = gt ; totalCompare nan nan = eq
		{"Std.Float.totalCompare (Std.Float.fromNat (succ zero)) (Std.Float.fdiv (Std.Float.fromNat zero) (Std.Float.fromNat zero))", "lt"},
		{"Std.Float.totalCompare (Std.Float.fdiv (Std.Float.fromNat zero) (Std.Float.fromNat zero)) (Std.Float.fromNat (succ zero))", "gt"},
		// operator dispatch: eqbOf / compareOf / leOf resolve the Float instances.
		{"leOf Std.Float.Float ordFloatInst (Std.Float.fromNat (succ zero)) (Std.Float.fromNat (succ (succ zero)))", "true"},
	}
	_ = cases
	// Implement the loop with the package's normalize-to-string helper:
	//   s := New(); s.LoadSource(prelude.Source())
	//   for each case: parse+elaborate+normalize expr, compare printed form to want.
	// The last case references the Float Ord instance by its qualified name
	// Std.Float.ordFloat (adjust `ordFloatInst` to the real qualified name).
}
```

Replace the `_ = cases` stub with the real driver using the package's existing normalize helper (find it: grep for a test that asserts an expression normalizes to a literal string, e.g. in `tower_hash_test.go` neighbors or a REPL/normalize test). If no in-package string-normalize helper exists, drive it through `s.ElabExpr` + `s.NormalizeExpr` + `s.Pretty` (all exist on Session per session.go). For the operator-dispatch case, reference the instance by its actual qualified name `Std.Float.ordFloat`.

- [ ] **Step 2: Run to verify it fails**

Run: `go test ./internal/session/ -run TestFloatGuardedTier -v`
Expected: FAIL (fle/feq/isNaN/totalCompare/instances undefined).

- [ ] **Step 3: Add `fleqN` to the Std.Float module**

In `internal/prelude/prelude.rune`, inside `module Std.Float is ... end` (starts line 8720), add after the `printFloat` foreign and before the module's closing `end`:

```
  -- Comparison primitive (baked 9-way in codegen/ioprims.go): 1 if a <= b else 0.
  -- NaN yields 0 (the IEEE partial order: NaN is <= nothing), so it is NOT a total
  -- order - the reason Float has Ord OPS but no OrdLaws.
  foreign fleqN : Float -> Float -> Whole end
```

- [ ] **Step 4: Add the FLOAT GUARDED TIER section inside the module**

Still inside `module Std.Float is`, after the `fleqN` foreign (all names here are unqualified because we are inside the module; the outer classes Semiring/SubR/Ord/DecEq, their mk* constructors, compareFromLe, and, notB, isZero, and Ordering/lt/eq/gt are visible from the enclosing top-level scope):

```
  -- ============================================================
  -- FLOAT GUARDED TIER (v4). IEEE754 f64 has OPS but no LAWS on BOTH axes:
  -- order (NaN <= NaN is false, so fle is not reflexive, not total) and
  -- algebra (float + is not associative). So Float instantiates the ops
  -- classes WITHOUT any laws record. All comparison/equality/NaN below
  -- derive from the single fleqN - no new primitives.
  -- ============================================================

  -- Bool le from fleqN (1/0). fleqN a b = 1 -> isZero 1 = false -> notB -> true.
  fle : Float -> Float -> Bool is fn (a : Float) (b : Float) is notB (isZero (fleqN a b)) end end

  -- IEEE == : a == b iff a <= b and b <= a. Gives -0 == +0 (both hold) and
  -- NaN != NaN (fle NaN NaN is false). This is the guarded DecEq (no refl law).
  feq : Float -> Float -> Bool is fn (a : Float) (b : Float) is and (fle a b) (fle b a) end end

  -- A NaN is the only f64 not <= itself.
  isNaN : Float -> Bool is fn (x : Float) is notB (fle x x) end end

  -- A NaN-last TOTAL order for sorting: both-NaN -> eq, one-NaN -> NaN is
  -- greatest, else the numeric compare. Reflexive/antisymmetric/total. Pragmatic:
  -- does NOT distinguish -0 from +0; not the full IEEE754 bit-level totalOrder.
  totalCompare : Float -> Float -> Ordering is
    fn (a : Float) (b : Float) is
      case fle a a of
      | false -> case fle b b of | false -> eq | true -> gt end
      | true  -> case fle b b of | false -> lt | true -> compareFromLe Float fle a b end
      end
    end
  end

  -- Guarded-tier instances: ops present, NO laws records.
  -- + and * (float + is non-ASSOCIATIVE, so NO SemiringLaws Float):
  instance semiringFloat : Semiring Float is mkSemiring Float fadd (fromNat zero) fmul (fromNat (succ zero)) end
  -- - (subtraction promotes A - A to A; NO laws):
  instance subRFloat : SubR Float Float is fsub end
  -- le / compare (NaN breaks reflexivity + totality, so NO OrdLaws Float):
  instance ordFloat : Ord Float is mkOrd Float fle (compareFromLe Float fle) end
  -- eqb (NaN != NaN, -0 = +0, so NO DecEq law for Float):
  instance decEqFloat : DecEq Float is mkDecEq Float feq end
```

- [ ] **Step 5: Run to verify it passes**

Run: `go test ./internal/session/ -run TestFloatGuardedTier -v`
Expected: PASS. If the operator-dispatch case fails to resolve the instance, confirm the instance's qualified name (`Std.Float.ordFloat`) and that instance search finds it by (class, type-head); if `case ... of | false ... | true ...` on a Bool needs the ctor order confirmed, check `data Bool` in the prelude (false/true order) and match.

- [ ] **Step 6: Prelude loads + no regression**

Run: `go test ./internal/session/... ./internal/prelude/... -count=1`
Expected: PASS (the prelude still loads; the four instances register without colliding).

- [ ] **Step 7: Dash-scan + commit**

```bash
grep -nP '[\x{2013}\x{2014}]' internal/prelude/prelude.rune internal/session/float_guarded_test.go && echo DASH || echo clean
git add internal/prelude/prelude.rune internal/session/float_guarded_test.go
git commit -m "feat(prelude): Float guarded-tier ops instances + derived fle/feq/isNaN/totalCompare"
```
(Only proceed if the dash-scan prints `clean`.)

---

### Task 2: 9-backend semantic numeric-tower parity gate

**Files:**
- Create: `harness/numeric_parity_test.go`
- Test: the file is the gate (a conformance test)

**Interfaces:**
- Consumes: the Task 1 Float instances + the shipped Whole/Int/Frac instances, all via `prelude.Source()`.
- Produces: `TestNumericTowerParity`, byte-identical across all 9 backends.

- [ ] **Step 1: Write the gate (it fails until Task 1 is present, then passes)**

Create `harness/numeric_parity_test.go`, modeled EXACTLY on `harness/comparison_conformance_test.go` (read it first: reuse its `runComparisonBackend`-style helper, the `bibleBackends()` list, the emit/compile/run scaffolding, and the byte-identity assertion helper). Give it its own corpus string exercising ALL FOUR types with arithmetic AND comparison, folding every check into one `and`-chain that prints `true`:

```
-- Whole: + * and le/compare/eqb (arithmetic via the Semiring/DecEq instances).
wAdd : Bool is eqbOf Whole decEqWhole (addW (succ (succ zero)) (succ (succ (succ zero)))) (succ (succ (succ (succ (succ zero))))) end
wLe  : Bool is leOf Whole ordWhole (succ (succ zero)) (succ (succ (succ zero))) end
-- Int: + * - and le/eqb over the shipped Int instances (use the real Int ctors nonneg/negsucc and Int add/mul names - grep prelude).
iNeg : Bool is leOf Int ordInt (negsucc (succ zero)) (nonneg (succ zero)) end
-- Frac: le/eqb over non-canonical representatives (qin QPair QRel (qpair ...)).
fLe  : Bool is leOf Frac ordFrac (qin QPair QRel (qpair (nonneg (succ zero)) (succ zero))) (qin QPair QRel (qpair (nonneg (succ (succ zero))) (succ (succ (succ zero))))) end
-- Float: arithmetic checked BY COMPARISON (never printed). fadd 1 2 == 3 ; fmul 2 3 == 6 ; fsub 5 2 == 3 ; fle 1 2 ; feq 2 2 ; NaN is not <= itself.
flAdd : Bool is Std.Float.feq (Std.Float.fadd (Std.Float.fromNat (succ zero)) (Std.Float.fromNat (succ (succ zero)))) (Std.Float.fromNat (succ (succ (succ zero)))) end
flMul : Bool is Std.Float.feq (Std.Float.fmul (Std.Float.fromNat (succ (succ zero))) (Std.Float.fromNat (succ (succ (succ zero))))) (Std.Float.fromNat (succ (succ (succ (succ (succ (succ zero))))))) end
flLe  : Bool is Std.Float.fle (Std.Float.fromNat (succ zero)) (Std.Float.fromNat (succ (succ zero))) end
flNaN : Bool is Std.Float.isNaN (Std.Float.fdiv (Std.Float.fromNat zero) (Std.Float.fromNat zero)) end

main : Bool is
  and wAdd (and wLe (and iNeg (and fLe (and flAdd (and flMul (and flLe flNaN))))))
end
```

VERIFY every name against the prelude before finalizing: the Int constructors and Int add/mul/order accessor names (`nonneg`/`negsucc`, `ordInt`, `decEqInt` if used), the Frac `qin QPair QRel (qpair ...)` construction (mirror the just-merged comparison_conformance_test.go's `fracA`/`fracB`), and `decEqWhole`/`ordWhole`/`ordFrac`. Keep the corpus small; every conjunct must be independently correct so a single divergence flips `main` to `false`.

The Whole/Int/Float checks run the FULL 9-way `bibleBackends()`. If the Frac check trips the WASM qlift gap (as in comparison_conformance_test.go), split a `mainNoFrac` that runs 9-way and a `mainFrac` that excludes wasm with the SAME documented justification (wasm has no qlift/Quot runtime; pre-existing, orthogonal to this feature). Assert each backend's trimmed stdout equals the fixed literal `true`.

- [ ] **Step 2: Run**

Run: `go test ./harness/ -run TestNumericTowerParity -v`
Expected: PASS byte-identical on all locally-available backends (report which ran vs env-skipped). If ANY backend diverges on a discrete result, that is a real bug - capture the differing bytes and STOP (do not exclude a backend to go green).

- [ ] **Step 3: Dash-scan + commit**

```bash
grep -nP '[\x{2013}\x{2014}]' harness/numeric_parity_test.go && echo DASH || echo clean
git add harness/numeric_parity_test.go
git commit -m "test: 9-backend semantic parity across all four numeric types (Float via feq)"
```

---

### Task 3: ch577 chapter + REPL acceptance

**Files:**
- Create: `listings/ch577_float_guarded.rune`
- Modify: `harness/listings_test.go` (add a `TestListingsRun/ch577` run-subtest, mirroring the ch576 entry pattern)
- Test: `internal/repl/repl_test.go` (add `TestREPLFloatGuardedPins`)

**Interfaces:**
- Consumes: Task 1 (the Std.Float guarded-tier surface).
- Produces: the pedagogical chapter + REPL pins (mandatory acceptance).

- [ ] **Step 1: Write ch577**

Create `listings/ch577_float_guarded.rune`. Unlike ch574-576 (which build their own preamble), this chapter DEMONSTRATES the shipped `Std.Float` guarded tier, so it starts with `import Std.Float` and uses the always-on prelude (grep an existing float listing, e.g. ch217 or examples/double.rune, for the exact `import Std.Float` form and how it references `Float`/`fadd`/`fromNat`). It must:
  - compute `+ - *` on Float via the operators (or `fadd`/`fsub`/`fmul`), checked by `feq`;
  - compute `fle`/`compareOf Float ordFloat`/`eqbOf Float decEqFloat`;
  - build a NaN via `fdiv (fromNat zero) (fromNat zero)` and show `isNaN` = true and `totalCompare` puts it last;
  - narrate the guarded-tier point: ops present, laws absent (NaN breaks order reflexivity; float `+` non-associative). Include a `main : Bool` folding several checks into one `true` so `TestListingsRun/ch577` can assert it runs to `true`.
  - OPTIONALLY a refutation witness: a `Bool` that is `false` showing a law fails, e.g. `assocFails : Bool is notB (Std.Float.feq (Std.Float.fadd (Std.Float.fadd a b) c) (Std.Float.fadd a (Std.Float.fadd b c)))` for a specific f64 triple a,b,c where float addition is non-associative (build a,b,c from fromNat/fdiv so no FloatLit is needed; pick a triple that genuinely reassociates differently - if a clean closed witness is hard to find without decimal literals, OMIT it and rely on the narrated comment + the ABSENCE of the laws records; the chapter must still RUN its `main` to `true`).

- [ ] **Step 2: Register the run-subtest**

In `harness/listings_test.go`, add to `TestListingsRun` (find the existing `t.Run("ch576", ...)` entry and mirror it):

```go
	t.Run("ch577", func(t *testing.T) {
		normalizesTo(t, s, "main", "true")
	})
```

Match the exact helper name/signature the ch576 entry uses (it may be `normalizesTo` or a run-and-compare helper - copy the ch576 line's shape verbatim, changing only the chapter and expected value).

- [ ] **Step 3: Run the listings gates**

Run: `go test ./harness/ -run 'TestListings' -count=1`
Expected: PASS (ch577 auto-discovered by `TestListingsElaborateAndCheck`; `TestListingsRun/ch577` -> `main` = true).

- [ ] **Step 4: REPL pins**

In `internal/repl/repl_test.go`, add (mirror an existing REPL script test for the harness - e.g. `TestREPLTowerComparisonPins` or `TestREPLScribeCorpus` - for `Run(in, &out)` + assertion shape):

```go
// v4 Float guarded tier: arithmetic + comparison + NaN behave in the REPL.
func TestREPLFloatGuardedPins(t *testing.T) {
	script := []string{
		"import Std.Float",
		"feq (fadd (fromNat (succ zero)) (fromNat (succ (succ zero)))) (fromNat (succ (succ (succ zero))))", // 1+2==3 -> true
		"fle (fromNat (succ zero)) (fromNat (succ (succ zero)))",                                            // 1<=2 -> true
		"compareOf Float ordFloat (fromNat (succ zero)) (fromNat (succ (succ zero)))",                       // lt
		"isNaN (fdiv (fromNat zero) (fromNat zero))",                                                        // true
		"totalCompare (fromNat (succ zero)) (fdiv (fromNat zero) (fromNat zero))",                           // lt (NaN last)
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := Run(in, &out); err != nil {
		t.Fatalf("Run: %v", err)
	}
	got := out.String()
	if strings.Contains(got, "error") {
		t.Errorf("unexpected error:\n%s", got)
	}
	if !strings.Contains(got, "lt : Ordering") {
		t.Errorf("compareOf/totalCompare did not print lt : Ordering\n%s", got)
	}
	if n := strings.Count(got, "true : Bool"); n != 3 {
		t.Errorf("want 3 true : Bool (feq, fle, isNaN), got %d\n%s", n, got)
	}
}
```

Confirm `import Std.Float` works as a REPL line (the REPL routes decls through the session per the script-ergonomics work; if `import` is not accepted at the REPL, qualify each name as `Std.Float.<name>` instead and drop the import line). Adjust the expected `lt : Ordering` count if both compareOf and totalCompare print `lt` (then assert count 2).

- [ ] **Step 5: Run REPL pins**

Run: `go test ./internal/repl/ -run TestREPLFloatGuardedPins -v`
Expected: PASS.

- [ ] **Step 6: Dash-scan + commit**

```bash
grep -nP '[\x{2013}\x{2014}]' listings/ch577_float_guarded.rune harness/listings_test.go internal/repl/repl_test.go && echo DASH || echo clean
git add listings/ch577_float_guarded.rune harness/listings_test.go internal/repl/repl_test.go
git commit -m "docs(listings): ch577 Float guarded-tier chapter + REPL pins"
```

---

## Self-Review (plan author)

- **Spec coverage:** Decision 1 (guarded-tier instances) + Decision 2 (derived compare/eq/isNaN/totalCompare) -> Task 1. Decision 4 semantic parity (all four types, discrete outputs, Float via feq) -> Task 2. Decision 5 (ch577 + REPL) -> Task 3. Decision 3 (display parity / Track B) is explicitly a SEPARATE plan (written after this ships). The class-hash audit stays unchanged (Float adds instances, no former) - no task touches tower_hash_test.go, correct.
- **Placeholder scan:** the spots requiring name confirmation (the in-package normalize helper in Task 1; Int/Frac ctor + accessor names in Task 2; the ch576 run-subtest helper name and `import Std.Float` REPL behavior in Task 3) are each flagged with an explicit "grep/confirm against the prelude" instruction and a concrete fallback. `mkSemiring` arg order is pinned verbatim (add, zero, mul, one) from prelude:103 - the one easy-to-get-wrong signature.
- **Type consistency:** `fle`/`feq`/`isNaN`/`totalCompare` and `semiringFloat`/`subRFloat`/`ordFloat`/`decEqFloat` (Task 1) are referenced by their qualified `Std.Float.` names in Tasks 2-3. `mkSemiring Float fadd (fromNat zero) fmul (fromNat (succ zero))` uses the confirmed arg order. `compareFromLe Float fle` matches its signature.
- **Guarded-tier discipline:** every instance in Task 1 has a comment naming the violated law; no laws record is added anywhere; the corpus and chapter verify ops COMPUTE, and the optional ch577 refutation makes "no laws" evidence.
