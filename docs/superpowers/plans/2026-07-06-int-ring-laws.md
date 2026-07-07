# Int RingLaws campaign Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Re-found Int junk-free (nonneg/negsucc), build the monus/order lemma library and the difference-pair transport (wsubEq + op spec lemmas), prove RingLaws/CommLaws/CommRingLaws for Int, cash the 1b bridge for Int's AbGroup, and ship chapters (ch570 update + new ch572).

**Architecture:** Prelude-only (kernel frozen; `data` is ordinary surface, no hash-format change). Laws are proven on the difference-pair model where they are Whole-semiring algebra, then transported through ONE respect lemma (wsubEq) and per-op spec lemmas; the junk-free representation makes the retraction exact.

**Tech Stack:** rune surface (internal/prelude/prelude.rune, listings/), Go tests (internal/session, internal/repl).

## Global Constraints

- Spec: `docs/superpowers/specs/2026-07-06-int-ring-laws-design.md` (binding).
- KERNEL FROZEN: only internal/prelude/prelude.rune, listings/, internal/session/tower_hash_test.go, internal/repl/repl_test.go may change.
- Public op names unchanged: intOf, ineg, iadd, isub, imul, wsub, divIF, isign, imag; instance names unchanged: semiringInt, ringInt, negWhole/negInt/negFrac, subWhole/subInt/subFrac, divInt. `mkInt` is DELETED (Rule 5).
- REPL pins unchanged as strings: `2 - 5` -> `-3 : Int`; `-3 + 1` -> `-2 : Int`; `-3 * 2` -> `-6 : Int`; `-1/3` : Frac; `(2 - 5) + 1` composes. Existing TestREPLIntTower / TestREPLNegationPromotes / TestREPLTowerArithmetic / TestREPLDecimalLiterals must stay green.
- Established layouts: Semiring s.1 add / s.2.1 zero / s.2.2.1 mul / s.2.2.2 one; Ring r.1 semiring / r.2 neg; RingLaws = And (SemiringLaws A r.1) (And negInvL negInvR); SemiringLaws 11-slot order addAssoc, addComm, addZeroL, addZeroR, mulAssoc, mulOneL, mulOneR, distribL, distribR, annihL, annihR; NegInvLT/RT param order (A, op, inv, e).
- Whole lemmas already shipped and reusable: addWAssoc, addWComm, addWZeroL/R, addWSuccR, addWSwapR, addWShiftSucc, mulWAssoc, mulWComm, mulWOneL/R, distribWL/R, mulWZeroL/R, mulWSuccR, congW, symEq, transEq, subWZeroL, subWSucc, lebEquiv, plus srl* extractors and mkAnd.
- CAUTION (rune strict case arms): keep `ih` in exactly one arm; precompute shared subterms with `let` outside the case.
- No em or en dashes anywhere (ASCII hyphen only) - hard rule.
- Conventional Commits, explicit pathspecs, trailer `Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>`.
- Branch `feat/int-ring-laws` in a worktree under `.worktrees/`. Full `go test -timeout 30m ./...` before finishing.

---

### Task 1: junk-free Int re-foundation

**Files:**
- Modify: `internal/prelude/prelude.rune` (the Int section; the tower-graph section; encodeInt/parseInt if they touch constructors)
- Modify: `internal/repl/repl_test.go` ONLY if a pin string legitimately needs no change but a test references removed names (none expected)
- Test: existing REPL/session suites + roundInt roundtrip

**Interfaces:**
- Produces (consumed by ALL later tasks): `data Int : U is nonneg : Whole -> Int | negsucc : Whole -> Int` (negsucc k denotes -(k+1)); ops per Step 2; `mkIntFalse` GONE; tower-graph lemmas restated.

- [ ] **Step 1: baseline the pins**

Run: `go test ./internal/repl/ -run 'TestREPLIntTower|TestREPLNegationPromotes|TestREPLTowerArithmetic|TestREPLDecimalLiterals' -count=1 -v`
Expected: PASS (this exact set must pass again after the rewrite; the pin STRINGS are the acceptance).

- [ ] **Step 2: rewrite the Int section**

Replace the current Int block (data Int, isign, imag, mkInt, intOf, builtin int, ineg, iadd, isub, imul, wsub, ringInt, semiringInt, divIF, divInt - keep the section banner, reworded for the new representation and WHY: negative zero was junk outside the normalized image and poisons law transport) with:

```
data Int : U is
  nonneg  : Whole -> Int
| negsucc : Whole -> Int
end
intOf : Whole -> Int is fn (n : Whole) is nonneg n end end
builtin int Int intOf
isign : Int -> Bool is
  fn (i : Int) is case i of | nonneg n -> false | negsucc k -> true end end
end
imag : Int -> Whole is
  fn (i : Int) is case i of | nonneg n -> n | negsucc k -> succ k end end
end
ineg : Int -> Int is
  fn (i : Int) is
    case i of
    | nonneg n -> case n of | zero -> nonneg zero | succ k -> negsucc k end
    | negsucc k -> nonneg (succ k)
    end
  end
end
wsub : Whole -> Whole -> Int is
  fn (a : Whole) (b : Whole) is
    case leb b a of
    | true -> nonneg (subW a b)
    | false -> negsucc (subW b (succ a))
    end
  end
end
iadd : Int -> Int -> Int is
  fn (x : Int) (y : Int) is
    case x of
    | nonneg a ->
        case y of
        | nonneg c -> nonneg (addW a c)
        | negsucc d -> wsub a (succ d)
        end
    | negsucc b ->
        case y of
        | nonneg c -> wsub c (succ b)
        | negsucc d -> negsucc (succ (addW b d))
        end
    end
  end
end
isub : Int -> Int -> Int is fn (x : Int) (y : Int) is iadd x (ineg y) end end
imul : Int -> Int -> Int is
  fn (x : Int) (y : Int) is
    case x of
    | nonneg a ->
        case y of
        | nonneg c -> nonneg (mulW a c)
        | negsucc d -> ineg (nonneg (mulW a (succ d)))
        end
    | negsucc b ->
        case y of
        | nonneg c -> ineg (nonneg (mulW (succ b) c))
        | negsucc d -> nonneg (mulW (succ b) (succ d))
        end
    end
  end
end
ringInt : Ring Int is
  mkRing Int (mkSemiring Int iadd (nonneg zero) imul (nonneg (succ zero))) ineg
end
instance semiringInt : Semiring Int is ringInt.1 end
```

ORDERING NOTE: wsub uses leb, so the Int block must sit BELOW leb (it already does). divIF and divInt keep their existing text (they consume isign/imag, which survive as accessors). `wneg` (the NegR Whole instance body) becomes `fn (n : Whole) is ineg (nonneg n) end`. Sweep the whole prelude for remaining uses of the old constructor `int ` and of `mkInt` (grep both) and migrate each: encodeInt/parseInt (if they build or match `int s m`, rewrite through isign/imag or the new constructors - preserve output strings exactly), and any other site. `mkInt` must be GONE.

- [ ] **Step 3: restate the tower graph**

In the tower-graph section: DELETE mkIntFalse (its comment too - the normalization it certified no longer exists; say so in the section banner). Restate:

```
intOfAdd : (a : Whole) -> (b : Whole) -> Eq Int (iadd (intOf a) (intOf b)) (intOf (addW a b)) is
  fn (a : Whole) (b : Whole) is refl (nonneg (addW a b)) end
end
intOfMul : (a : Whole) -> (b : Whole) -> Eq Int (imul (intOf a) (intOf b)) (intOf (mulW a b)) is
  fn (a : Whole) (b : Whole) is refl (nonneg (mulW a b)) end
end
intOfOne : Eq Int (intOf (succ zero)) (nonneg (succ zero)) is refl (nonneg (succ zero)) end
```

ratOfInt and ratOfIntOf keep their text (accessors preserved; ratOfIntOf's refl body may need its displayed value unchanged - verify it still checks, it should since isign (nonneg n) computes to false and imag (nonneg n) to n).

- [ ] **Step 4: gates**

Run: `go test ./internal/session/ ./internal/repl/ -count=1` -> ALL PASS (pins from Step 1 identical; presence tests for intOfAdd etc. still pass; mkIntFalse's presence entry in TestTowerGraphLemmasPresent must be REMOVED - do that in the same edit).
Run: `go test ./cmd/... ./harness/ -run 'TestExamples|TestREPL|Explain|TestFiveOutputs' -count=1 -timeout 20m` -> PASS (explain goldens: fix only if drifted).

- [ ] **Step 5: commit**

```bash
git add internal/prelude/prelude.rune internal/session/tower_hash_test.go
git commit -m "refactor(prelude)!: junk-free Int (nonneg/negsucc); mkInt deleted; graph lemmas now refl" -- internal/prelude/prelude.rune internal/session/tower_hash_test.go
# add internal/repl/repl_test.go / cmd goldens to the pathspec iff touched
```

---

### Task 2: the monus/order library + wsubEq

**Files:**
- Modify: `internal/prelude/prelude.rune` (new subsection in the laws region, before the tower graph)
- Modify: `internal/session/tower_hash_test.go` (presence)

**Interfaces:**
- Consumes: leb, subW, addW, the shipped Whole lemmas, subst/symEq/transEq, WholeElim.
- Produces (Task 3/4 consume): `succInj : (a b : Whole) -> Eq Whole (succ a) (succ b) -> Eq Whole a b` (via cong pred), `lebComplete : (a b : Whole) -> Eq Bool (leb a b) false -> Eq Bool (leb b a) true`, `subWAddCancel : (a b : Whole) -> Eq Bool (leb b a) true -> Eq Whole (addW b (subW a b)) a`, `addWCancelL : (k a b : Whole) -> Eq Whole (addW k a) (addW k b) -> Eq Whole a b`, and `wsubEq : (a b c d : Whole) -> Eq Whole (addW a d) (addW c b) -> Eq Int (wsub a b) (wsub c d)`.

- [ ] **Step 1: failing presence test**

Sibling test `TestMonusLibraryPresent` checking `"lebComplete", "subWAddCancel", "addWCancelL", "wsubEq"`. Run -> FAIL.

- [ ] **Step 2: the library**

Prove the four library lemmas by WholeElim induction (leb and subW both drive their recursion as documented at their definitions; subWZeroL/subWSucc/lebEquiv are the shipped shape guides). Then wsubEq by case analysis on `leb b a` and `leb d c`:
- true/true: goal reduces to `Eq Int (nonneg (subW a b)) (nonneg (subW c d))`; conclude subW a b = subW c d from the cross-equality via subWAddCancel + addWCancelL + addW shuffles, then congW-style congruence on nonneg.
- false/false: same shape at negsucc with subW b (succ a) / subW d (succ c).
- mixed cases are IMPOSSIBLE and are discharged by contradiction WITHOUT an Empty type: derive `Eq Bool true false` (from the cross-equality, one branch's leb fact transfers to the other side - an order-transfer helper, see below - and collides with the branch assumption), then subst along it with the Bool-case motive `fn (z : Bool) is case z of | true -> <trivially provable statement, e.g. Eq Int lhs lhs> | false -> <the goal> end end`, inhabiting the true side by refl. Document this ex-falso idiom in a comment; it recurs.
- The order-transfer helper(s) the mixed and same-branch cases demand (shape: cross-equality plus `leb b a = true` implies `leb d c = true`) must be NAMED, reusable lemmas in this library; their exact statements are discovered during the proof - report them. Candidates the proof may also need: leb-addW compatibility (`Eq Bool (leb (addW k a) (addW k b)) (leb a b)` by induction on k) - derive what is needed, keep each named.

- [ ] **Step 3: gates + commit**

Run: `go test ./internal/session/ ./internal/repl/ -count=1` -> PASS.

```bash
git add internal/prelude/prelude.rune internal/session/tower_hash_test.go
git commit -m "feat(prelude): monus/order library + wsubEq (wsub respects cross-equal differences)" -- internal/prelude/prelude.rune internal/session/tower_hash_test.go
```

---

### Task 3: the transport spec lemmas

**Files:**
- Modify: `internal/prelude/prelude.rune` (same region), `internal/session/tower_hash_test.go`

**Interfaces:**
- Consumes: Task 2's library; addWZeroR, addWSuccR, mulWZeroL/R, distribW*, the Whole shuffles.
- Produces (Task 4 consumes; exact statements):

```
posP : Int -> Whole   (nonneg n -> n | negsucc k -> zero)
negP : Int -> Whole   (nonneg n -> zero | negsucc k -> succ k)
wsubRep : (x : Int) -> Eq Int (wsub (posP x) (negP x)) x                    [both cases refl]
wsubPosNeg : (p : Whole) -> (n : Whole) ->
  Eq Whole (addW (posP (wsub p n)) n) (addW p (negP (wsub p n)))            [case leb n p; subWAddCancel both branches, lebComplete in the false branch]
inegNonneg : (m : Whole) -> Eq Int (ineg (nonneg m)) (wsub zero m)          [case m; refl branches]
iaddSpec : (x y : Int) -> Eq Int (iadd x y)
  (wsub (addW (posP x) (posP y)) (addW (negP x) (negP y)))
imulSpec : (x y : Int) -> Eq Int (imul x y)
  (wsub (addW (mulW (posP x) (posP y)) (mulW (negP x) (negP y)))
        (addW (mulW (posP x) (negP y)) (mulW (negP x) (posP y))))
inegSpec : (x : Int) -> Eq Int (ineg x) (wsub (negP x) (posP x))
iaddWsub : (p n : Whole) -> (z : Int) -> Eq Int (iadd (wsub p n) z)
  (wsub (addW p (posP z)) (addW n (negP z)))
imulWsub : (p n : Whole) -> (z : Int) -> Eq Int (imul (wsub p n) z)
  (wsub (addW (mulW p (posP z)) (mulW n (negP z)))
        (addW (mulW p (negP z)) (mulW n (posP z))))
```

- [ ] **Step 1: failing presence test** (`TestIntSpecLemmasPresent`: wsubRep, wsubPosNeg, iaddSpec, imulSpec, inegSpec, iaddWsub, imulWsub). Run -> FAIL.

- [ ] **Step 2: prove them**

Proof notes (verified shapes; report any that needed adjustment):
- iaddSpec by the four constructor cases: nonneg/nonneg reduces to refl after `leb zero _`/`subW _ zero`/`addW zero zero` compute; nonneg/negsucc and negsucc/nonneg need one addWZeroR rewrite each; negsucc/negsucc needs addWSuccR under a negsucc congruence.
- imulSpec: rewrite the RHS Whole polynomial with mulWZeroL/R + addWZeroL/R per case; the mixed-sign cases go through inegNonneg.
- inegSpec: three-way case (nonneg zero / nonneg succ / negsucc), each closing by computation or one rewrite.
- iaddWsub/imulWsub: chain the spec lemma at (wsub p n, z) with wsubEq; the cross-equality obligation reduces through wsubPosNeg (for imulWsub, multiplied out via distribWL/R and the addW shuffles - ch107's pmulRespL/R proofs are the exact algebraic shape guide, over its own Nat).
Extra shuffle helpers are allowed; name each and report.

- [ ] **Step 3: gates + commit**

Run: `go test ./internal/session/ ./internal/repl/ -count=1` -> PASS.

```bash
git add internal/prelude/prelude.rune internal/session/tower_hash_test.go
git commit -m "feat(prelude): difference-pair transport specs (iadd/imul/ineg as wsub of Whole polynomials)" -- internal/prelude/prelude.rune internal/session/tower_hash_test.go
```

---

### Task 4: the Int law values

**Files:**
- Modify: `internal/prelude/prelude.rune`, `internal/session/tower_hash_test.go`

**Interfaces:**
- Consumes: everything above + mkAnd + the law formers + abGroupLawsOfRing (1b bridge).
- Produces: the thirteen named Int law lemmas, `ringLawsInt : RingLaws Int ringInt`, `commLawsInt : CommLaws Int semiringInt`, `commRingLawsInt : CommRingLaws Int ringInt`, `abGroupLawsInt : AbGroupLaws Int (groupOfRing Int ringInt)`.

- [ ] **Step 1: failing presence test** (`TestIntLawsPresent`: ringLawsInt, commLawsInt, commRingLawsInt, abGroupLawsInt, iaddAssoc, imulComm, idistribL). Run -> FAIL.

- [ ] **Step 2: the thirteen lemmas**

Names and statement formers (all over `iadd`/`imul`/`ineg`/`nonneg zero`/`nonneg (succ zero)`): iaddAssoc (AssocT), iaddComm (CommT), iaddZeroL (IdLT), iaddZeroR (IdRT), imulAssoc (AssocT), imulOneL (IdLT), imulOneR (IdRT), idistribL (DistribLT), idistribR (DistribRT), imulZeroL (AnnihLT), imulZeroR (AnnihRT), inegInvL (NegInvLT Int iadd ineg (nonneg zero)), inegInvR (NegInvRT ...), plus imulComm (CommT, for CommLaws).

The uniform pipeline for each: rewrite every variable occurrence into wsub form via wsubRep (subst), collapse nested ops with iaddWsub/imulWsub/inegSpec left to right until each side is ONE wsub of a Whole polynomial, then close with wsubEq whose cross-equality obligation is Whole-semiring algebra (the shipped lemmas + Task 3 shuffles). Easy ones first (iaddZeroL/R, imulZeroL/R, inegInvL/R - small polynomials), then comm, then assoc/distrib (largest shuffles; imulAssoc's cross-equality is the eight-term rearrangement, ch107's zmulAssoc is the guide).

- [ ] **Step 3: assemble**

```
ringLawsInt : RingLaws Int ringInt is
  mkAnd (SemiringLaws Int (ringInt.1))
    (And (NegInvLT Int iadd ineg (nonneg zero)) (NegInvRT Int iadd ineg (nonneg zero)))
    (mkAnd ... the 11-slot chain, same shape as semiringLawsWhole ...)
    (mkAnd (NegInvLT Int iadd ineg (nonneg zero)) (NegInvRT Int iadd ineg (nonneg zero)) inegInvL inegInvR)
end
commLawsInt : CommLaws Int semiringInt is imulComm end
commRingLawsInt : CommRingLaws Int ringInt is
  mkAnd (RingLaws Int ringInt) (CommLaws Int (ringInt.1)) ringLawsInt commLawsInt
end
abGroupLawsInt : AbGroupLaws Int (groupOfRing Int ringInt) is
  abGroupLawsOfRing Int ringInt ringLawsInt
end
```

(Write the 11-slot chain fully; semiringLawsWhole is the template. The statements over ringInt.1's projections compute to the raw ops definitionally - the established mechanism. abGroupLawsInt is ONE LINE: the 1b bridge cashing, no new proof - keep it that way.)

- [ ] **Step 4: gates + commit**

Run: `go test ./internal/session/ ./internal/repl/ -count=1` -> PASS. Watch prelude load time; if REPL suite slows pathologically (more than ~30s over baseline), STOP and report with numbers.

```bash
git add internal/prelude/prelude.rune internal/session/tower_hash_test.go
git commit -m "feat(prelude): Int ring laws proven (RingLaws/CommLaws/CommRingLaws + AbGroup via the 1b bridge)" -- internal/prelude/prelude.rune internal/session/tower_hash_test.go
```

---

### Task 5: chapters (ch570 update + new ch572)

**Files:**
- Modify: `listings/ch570_tower_graph.rune` (mirror the new Int + restated lemmas; mkIntFalse section removed with a narration note on WHY the lemma died - the representation made normalization vacuous)
- Create: `listings/ch572_int_ring_laws.rune`

- [ ] **Step 1: update ch570** to mirror the prelude's new Int block and graph lemmas byte-identically (keep its own narrative frame; the diagram is unchanged). Its main witness must still run.

- [ ] **Step 2: write ch572**, self-contained, ch107-narration style: the junk-value argument (negative zero poisons transport - show the old representation in prose only), the new Int, the ZW story, wsubEq + the monus library, the spec-lemma pipeline (mirror the prelude verbatim for shared defs), a REPRESENTATIVE law subset fully proven in-chapter (at minimum iaddAssoc, imulComm, inegInvL, assembled into a partial demonstration; pulling ALL thirteen is licensed but optional - the chapter must be honest about what it contains vs what the prelude carries), and the bridge payoff narrated with abGroupLawsInt's one-liner. Runnable witness (e.g. `main : Int is iadd (negsucc 2) (nonneg 5) end` runs to `nonneg 2`).

- [ ] **Step 3: gates + commit**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck' -count=1 -timeout 20m` -> PASS with ch570 + ch572 swept. Witness check via `go run ./cmd/rune run listings/ch572_int_ring_laws.rune main --target js`.

```bash
git add listings/ch570_tower_graph.rune listings/ch572_int_ring_laws.rune
git commit -m "docs(listings): ch570 mirrors junk-free Int; ch572 the Int ring-laws campaign" -- listings/ch570_tower_graph.rune listings/ch572_int_ring_laws.rune
```

---

### Task 6: full suite + bookkeeping

- [ ] **Step 1:** `go test -timeout 30m ./...` (background + poll). ALL PASS.
- [ ] **Step 2:** roadmap status (three-majors-roadmap.md): append to the v4 paragraph: "Int law campaign done (junk-free Int re-foundation + RingLaws/CommLaws/CommRingLaws + AbGroup via the bridge, ch572); next: Frac campaign (design open: canonical-form vs quotient route)." 1a spec Decision 4: mark the Int deferral CLOSED with a pointer to 2026-07-06-int-ring-laws-design.md.
- [ ] **Step 3:** commit both docs with pathspec, message `docs: Int law campaign status`.
