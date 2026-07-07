# Frac quotient re-foundation Implementation Plan (Plan A of the Frac campaign)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Re-found Frac as a quotient of raw pairs (Quot QPair QRel) with UNREDUCED qlift ops whose respect proofs are pure Int ring algebra, canonical display done computationally (Go fold + rune reduce in the codec), and the to_radix pipeline ported - all REPL behavior string-identical. Laws are Plan B.

**Architecture:** One-shot prelude surgery (the old data Frac dies; every op re-lifts through qlift with its respect proof), plus the licensed Go display-folding site (DecConfig, the Int-campaign precedent). ch113/ch116 (in listings/ch116_rational_field_complete.rune, which redeclares ch113 verbatim) provide the double-qlift op pattern and respect-proof shapes; ch203 provides the to_radix-via-qlift port source. Our Int being canonical data (not a quotient) makes every respect proof strictly simpler than the chapters' versions.

**Tech Stack:** rune surface (internal/prelude/prelude.rune), Go display folding (surface/pretty.go + internal/session/session.go decConfig), Go tests.

## Global Constraints

- Spec: `docs/superpowers/specs/2026-07-07-frac-field-laws-design.md` (binding; this plan is its Plan A).
- KERNEL FROZEN: core/, store/, elaborate/ untouched. Licensed Go sites: surface/pretty.go (DecConfig folding ONLY) + internal/session/session.go (decConfig resolution ONLY). Otherwise: internal/prelude/prelude.rune, internal/session/tower_hash_test.go, internal/repl/repl_test.go (only if a test references a deleted name - report), listings/ untouched in Plan A.
- The kernel Quot machinery is shipped v2: `Quot A R : U`, `qin A R a : Quot A R`, `qsound A R a b (r : R a b) : Eq (Quot A R) (qin A R a) (qin A R b)`, `qlift A R B (f : A -> B) (resp : (a b : A) -> R a b -> Eq B (f a) (f b)) : Quot A R -> B` with the ι-rule `qlift ... (qin ... a) ~> f a`. READ a working consumer first (grep qlift in listings/ch116_rational_field_complete.rune) to confirm exact argument orders - the chapter is authoritative over this summary.
- REPL pins unchanged as strings: `1/3` -> `1/3 : Frac`; `-1/3` -> `-1/3 : Frac`; `1/3 + 2/3` -> `1`; `(1/2) * (2/3)` -> `1/3`; `1.3` -> `13/10`; `1/3 |> to_radix` -> `0.{3}`; `3/4 |> to_radix` -> `0.75`; the whole TestREPLTowerArithmetic / TestREPLNegationPromotes / TestREPLDecimalLiterals / TestREPLIntTower battery, plus whichever tests pin to_radix (grep to_radix internal/repl/repl_test.go).
- DIVISION-BY-ZERO BEHAVIOR: the new denominator is positive by construction; recip of a zero-numerator Frac is the junk 0/1. If any existing test pins an n/0-style display, STOP and report for a decision instead of changing the pin.
- Rule 5: the old `data Frac is frac : Bool -> Whole -> Whole`, fneg/fnum/fden, and reduce-as-representation-normalizer are DELETED. `reduceQ` survives only as the computational display/codec reducer.
- Int assets available: junk-free Int, ringLawsInt's thirteen lemmas (iaddAssoc/iaddComm/iaddZeroL/R, imulAssoc/imulComm/imulOneL/R, idistribL/R, imulZeroL/R, inegInvL/R), the transport toolkit, congruence helpers (congIadd*/congImul*/cong2W), symEq/transEq/congW.
- Rune gotchas: strict case arms; every `fn ... is` needs its own `end`; nested `let ... in` over seq for multi-step proofs.
- No em or en dashes anywhere. Conventional Commits, explicit pathspecs, trailer `Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>`.
- Branch `feat/frac-quotient` in a worktree under `.worktrees/`. Full `go test -timeout 30m ./...` before finishing.

---

### Task 1: the quotient re-foundation (prelude surgery + display + codec)

**Files:**
- Modify: `internal/prelude/prelude.rune` (the whole Frac section; the Div/NegR/SubR instance lines; encodeFrac/parseFrac; any fneg/fnum/fden consumer EXCEPT to_radix - see the to_radix note)
- Modify: `surface/pretty.go`, `internal/session/session.go` (DecConfig: the Frac fold becomes the qin/qpair fold)
- Modify: `internal/session/tower_hash_test.go` (presence)

**Interfaces:**
- Produces (Plan B and Task 2 consume):

```
data QPair : U is qpair : Int -> Whole -> QPair end       -- num, den = succ d
qnum  : QPair -> Int    (case projection)
qdpred : QPair -> Whole (case projection)
qden  : QPair -> Int is fn (p : QPair) is nonneg (succ (qdpred p)) end end
QRel : QPair -> QPair -> U is
  fn (p : QPair) (q : QPair) is Eq Int (imul (qnum p) (qden q)) (imul (qnum q) (qden p)) end
end
Frac : U is Quot QPair QRel end
fracOf : Whole -> Frac is fn (n : Whole) is qin QPair QRel (qpair (nonneg n) zero) end end
builtin rat Frac fracOf
```

Raw ops on representatives (UNREDUCED; denominators multiply as succ-encoded: the product den is `succ (addW d1 (addW d2 (mulW d1 d2)))` since (d1+1)(d2+1) = d1+d2+d1*d2+1 - define a named `dmul : Whole -> Whole -> Whole` for it and a lemma-free check that `nonneg (succ (dmul d1 d2))` equals `imul (qden p) (qden q)` DEFINITIONALLY or via a small named lemma `qdenMul`):

```
rawAdd (qpair i1 d1) (qpair i2 d2) = qpair (iadd (imul i1 (qden q2)) (imul i2 (qden q1))) (dmul d1 d2)
rawMul = qpair (imul i1 i2) (dmul d1 d2)
rawNeg = qpair (ineg i) d
rawRecip (qpair i d) = case-split i:
  nonneg zero      -> qpair (nonneg zero) zero            -- junk 0/1
  nonneg (succ k)  -> qpair (nonneg (succ d)) k           -- (k+1)/(d+1) -> (d+1)/(k+1)
  negsucc k        -> qpair (negsucc d) k                  -- -(k+1)/(d+1) -> -(d+1)/(k+1)
```

Lifted ops via the ch113 double-qlift pattern with respect proofs (exact statements discovered against the chapter; the content of each proof is cross-multiplication rearrangement closed by ringLawsInt lemmas): `addF`, `mulF`, `fnegate`, `recipF : Frac -> Frac`, then `subF x y = addF x (fnegate y)`, `divF x y = mulF x (recipF y)`, `divWF a b` (build the pair directly: qin (qpair (nonneg a) (pred-of-b handling: b may be zero - old divWF accepted any Whole; encode `divWF a b = case b of zero -> junk 0/1 | succ k -> qin (qpair (nonneg a) k)`), `divIF x y` (Int/Int to Frac, same shape via the Int sign). Instances rewired IN PLACE with unchanged names: divRingFrac (mkDivRing over mkRing/mkSemiring with zero = fracOf 0, one = fracOf 1, ops addF/mulF/fnegate/recipF), semiringFrac = divRingFrac.1.1, divWhole/divFrac/divInt, negFrac, subFrac.

- reduceQ (computational only): `reduceQ : QPair -> QPair` dividing |num| and den by their gcd via the existing `gcd`/`//` FUNCTIONS. It is never the subject of a proof; its consumers are the Go fold's rune-side mirror logic and (Plan B) whatever canonical-observer design lands.

**The Show/Binary resolution (binding):** every function OUT of a quotient needs a qlift respect proof, and a string-rendering (or canonical-pair) respect proof IS lowest-terms uniqueness - the exact number theory this spec forbids. There is no route around it inside the kernel (factoring through reduceQ needs `QRel p q -> Eq QPair (reduceQ p) (reduceQ q)`, the same fact). THEREFORE the `showFrac` and `binFrac` instances (with encodeFrac/parseFrac/roundFrac) are REMOVED in Plan A. Their return is a Plan B design question (candidates: a canonical-representative observer justified by a uniqueness proof if Plan B chooses to buy it, or rendering through to_radix once toRadixRespects lands, which changes the output format) - record the removal and the question in the spec status. Sweep consumers: grep `showFrac|binFrac|encodeFrac|parseFrac|roundFrac` - each either dies with them (the roundFrac round-trip entry), moves to the Whole/Int codecs, or gets a `-- Plan B` note. If any REPL pin or gate depends on Show-Frac output (e.g. string interpolation of a fraction), STOP and report before proceeding.
- REPL display does NOT go through Show: the Go DecConfig fold handles it (below), so all arithmetic pins survive without Show Frac.

Go display fold (surface/pretty.go + session.go):
- DecConfig drops `Frac core.Hash`, gains `Qin, Qpair core.Hash` (session resolves `s.refs["qin"]` - the quotient builtin is registered in every session - and `s.refs["qpair"]`; On-gating per the existing optional-extras pattern).
- The fold case: a saturated `qin _ _ (qpair i d)` where i is a folded Int (nonneg/negsucc numeral) and d a Whole numeral: compute num/den = |i| / (d+1), reduce by Go gcd (int arithmetic; bail to no-fold on bignum exactly as wholeVal already does), then render with the EXISTING output rules verbatim: zero unsigned "0"; den 1 renders signed whole; else signed "n/d".

- [ ] **Step 1: baseline the pin battery**

Run: `go test ./internal/repl/ -run 'TestREPLTowerArithmetic|TestREPLNegationPromotes|TestREPLDecimalLiterals|TestREPLIntTower' -count=1 -v` -> PASS (record). Also grep the repl tests for to_radix pins and for any Show-Frac/interpolated-fraction pin; report findings BEFORE surgery if a Show Frac consumer exists.

- [ ] **Step 2: failing presence test**

`TestFracQuotientPresent` in tower_hash_test.go: names `QPair, QRel, Frac, fracOf, addF, mulF, recipF, divF, reduceQ`. Run -> FAIL.

- [ ] **Step 3: the surgery**

Execute the full replacement per the Interfaces block: representation, raw ops, respect proofs, lifts, instances, reduceQ, Show/Binary removal per the binding resolution, old-Frac deletion (grep `frac false\|frac (\|fneg \|fnum \|fden ` -> the only survivors must be inside the to_radix section, which Task 2 owns - if to_radix cannot elaborate against the new Frac in the interim, gate it out the same way the section is reached: to_radix and its dependents may be TEMPORARILY COMMENTED with a `-- Task 2 rewires` banner, reported loudly; the REPL to_radix pins are then EXPECTED-FAIL until Task 2, and the Task 1 gate list excludes exactly them).

- [ ] **Step 4: the Go fold**

Rewrite the DecConfig Frac case per the Interfaces block; update decConfig in session.go. Run: `go test ./surface/ ./internal/session/ -count=1` -> PASS.

- [ ] **Step 5: gates**

Run: `go test ./internal/repl/ -run 'TestREPLTowerArithmetic|TestREPLNegationPromotes|TestREPLDecimalLiterals|TestREPLIntTower' -count=1 -v` -> PASS with identical strings. Run the full repl+session suites; to_radix-dependent tests are the ONLY licensed temporary failures (enumerate them in the report). Record the single-prelude-load wall time (time a one-expression `rune repl` script or the existing load benchmark pattern) - the spec's 3s budget gate.

- [ ] **Step 6: commit**

```bash
git add internal/prelude/prelude.rune surface/pretty.go internal/session/session.go internal/session/tower_hash_test.go
git commit -m "refactor(prelude)!: Frac re-founded as Quot QPair QRel (unreduced qlift ops, Go-side canonical display)" -- internal/prelude/prelude.rune surface/pretty.go internal/session/session.go internal/session/tower_hash_test.go
```

---

### Task 2: the to_radix port

**Files:**
- Modify: `internal/prelude/prelude.rune` (the to_radix section), `internal/session/tower_hash_test.go`

**Interfaces:**
- Consumes: Task 1's Frac + qnum/qdpred/qden; the existing longDiv/RDec machinery (representative-level, unchanged); ch203's toRadixQ + toRadixRespects (listings/ch203_rational_canonical.rune ~lines 5229-5340) as the port source - their Z-layer respect steps become direct canonical-Int algebra.
- Produces: `to_radix : Frac -> RDec is qlift QPair QRel RDec toRadixRep toRadixRespects end` (+ the sigplace/sigfig variants re-lifted the same way), with toRadixRep = the existing long-division expansion re-expressed over a QPair representative (sign from qnum's constructor, magnitudes from imag/qdpred).

- [ ] **Step 1:** failing presence pins (toRadixRep, toRadixRespects, to_radix). Re-enable anything Task 1 temporarily gated.
- [ ] **Step 2:** the port. toRadixRespects is the riskiest proof of Plan A: it shows the radix expansion is QRel-invariant (floor/mod uniqueness over cross-equal pairs). ch203 proved exactly this; port with our Int. If the port fights beyond its shape (a genuinely new obligation appears), STOP with the obligation stated - do NOT invent gcd theory to bridge it.
- [ ] **Step 3:** gates: the to_radix REPL pins (`1/3 |> to_radix` -> `0.{3}`, `3/4` -> `0.75`, `1/6` -> `0.1{6}`, sigplace/sigfig pins per the existing tests) + FULL repl/session suites -> ALL PASS (no licensed failures remain).
- [ ] **Step 4:** commit (pathspec: the two files).

---

### Task 3: sweep, measure, close Plan A

**Files:**
- Modify: `docs/superpowers/specs/2026-07-07-frac-field-laws-design.md` (status), `PARKING-LOT.md` (load numbers + the Show/Binary Plan-B note), roadmap.

- [ ] **Step 1:** full `go test -timeout 30m ./...` (background + poll) -> ALL PASS. Sweep examples/ + cmd/ + harness/ for old-Frac assumptions (grep `frac \|fnum\|fden\|fneg` outside listings) - listings are self-contained and stay green untouched.
- [ ] **Step 2:** record the measured single-load time in the spec status + PARKING-LOT (against the 3s budget); note the Show/Binary-for-Frac removal and its Plan-B design question.
- [ ] **Step 3:** roadmap status line: Plan A shipped; Plan B (laws) next. Commit docs.
