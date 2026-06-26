# Verified implementations — proofs and calculation, divorced honestly

**Status:** design note; rung one (binary numerals) shipped as
`listings/ch14_binary.rune`. Companion to `ref_docs/rune-numeric-tower.md`.

## The question

The kernel evaluator PROVES (it is the conversion judgment); the erased
shadow PERFORMS (BigInt under node). Can calculation be routed to fast
implementations while proofs keep their meaning — and what does it take for
an implementation to be "ready"?

Four answers, in ascending order of trust demanded:

1. **Shadow evaluation outside proofs** (`rune run`, REPL `:run`): the
   kernel still types everything; the shadow only prints values. A wrong
   shadow shows a wrong number, never certifies a false theorem. Trust
   gate: the listings corpus (emit-and-execute agreement with the checker).
2. **Hash-keyed native implementations, evaluation-only.** A registry
   `content hash → native function`, consulted by display/evaluation paths.
   Content addressing is the trust anchor: the hash pins the definition's
   entire meaning (its body's references, transitively), so editing
   anything makes trust LAPSE automatically — the proof cache's
   Frame-Lemma discipline, reused. Gate: differential property tests with
   the kernel as oracle, plus corpus replay; pass mints a store entry
   `(definition hash, implementation fingerprint)`. Testing promotes to
   *trusted*, never to *proved*; sound because evaluation-only.
3. **Native conversion inside proofs** (the `vm_compute` move): the
   registry answers during conversion, i.e. native code decides
   theoremhood. Conversion has no short certificate — checking IS
   computing — so this is all-or-nothing trust per implementation.
   If ever adopted: certificates extend their key to
   `(content-hash, unfolded-set, impl-set)` so proof-grade and
   computation-grade never blur, kernel-only re-certification stays
   possible, and the whole mode sits behind a flag, default off.
4. **The verified implementation: prove it instead** — shipped first,
   because it grows the trusted base by exactly nothing. Define the fast
   structure in rune, prove it agrees with the unary spec once, and let
   certificates route computation through it.

## Rung one: binary numerals (ch14)

`Pos` is 1 | 2p | 2p+1 — every number one representative, no leading-zero
junk — and `BN` adjoins zero. `psucc`/`padd`/`pmul` are single structural
recursions (the carry rides inside `psucc`). The agreement theorems

```
toNatP (padd p q) = toNatP p + toNatP q
toNatP (pmul p q) = toNatP p * toNatP q
toNat  (fromNat n) = n
```

are the verification layer as theorems: the implementation is ready
exactly when they check. The payoff certificate shape:

```
bigMul : 35 * 93 = 3255 is
  trans (cong2 (*) (sym toNat35) (sym toNat93))     -- literals to images
    (trans (sym (bnmulAgree …)) (refl 3255))        -- spec to impl; compute
end
```

The kernel never multiplies unary numbers here: the `cong2` leg is two
linear refls, the agreement theorem swaps spec for implementation, and the
final refl converts a BINARY multiplication plus one linear readback
against the literal. Conversion's fast syntactic path keeps the stated
`35 * 93` from ever being evaluated.

## Fixed: call-site erasure of the 0-fragment (2026-06)

Discovered building ch14: refl-headed proof BODIES erased to the unit token,
but applications still passed their arguments — so a proof mentioning a deep
numeral carried it into the JS shadow, and ~1600-deep nesting overflowed
node's parser. The original parking-lot framing — "drop the 0-quantity
arguments at application sites" — turned out to be **insufficient**: ch14's
proof helpers (`cong`, `trans`, `sym`) take UNANNOTATED proof parameters,
which are quantity ω, not 0, and the leaking numerals ride in their implicit
ω endpoint positions `{x}{y}`. Quantity is the wrong boundary for them.

The real boundary is **proof irrelevance**: a subterm whose TYPE is a Prop is
computationally vacuous however it was built (`Eq A x y : Prop`, and a Pi into
Prop is a Prop), so the whole subterm erases to the unit token — not only when
it is syntactically `refl`. This needs the type, which syntactic codegen.Erase
does not have, so the fix is a TYPE-DIRECTED pass (`elaborate.TypedEraser`)
that mirrors CheckCore: every definition body arrives in check mode against
its declared type, proof positions are caught where used, and 0-quantity
ARGUMENT positions (genuine erased data, e.g. `(0 n : Nat)`) are still unit'd
at the call site with their position kept (no arity surgery). Wired into the
session emit path (file commands + the REPL `:run` seam). Guarded by
internal/session/erase_test.go (a proof inlining a succ-chain erases to a bare
unit; the chain it mentions never reaches the shadow; honest computational
numerals survive at full depth). All fourteen chapters still emit and run.
ch14 no longer needs to size its showcase literals under the node limit.

## What stays parked

- **Binary literals — DONE (2026-06).** Numerals are now type-directed: a
  `builtin bin BN bn0 bnP Pos pH pO pI` binding lowers a numeral checked at
  BN/Pos to an O(log n) bit-spine (no 4096 cap), while the unary `builtin
  nat` default is unchanged and coexists — the expected type chooses per
  literal. ch14's binary constants are now `35`/`41`/`93`/`186` literals
  (byte-identical core to the old hand-written spines). See GRAMMAR §5.5,
  surface/numeral.go, internal/session/binlit_test.go.
- **Division/gcd in binary — DONE (2026-06), listings/ch15.** Long division
  recurses on the dividend's bit-spine (logarithmically many steps); each step
  shifts the running remainder, brings down one bit, and conditionally
  subtracts the divisor. The subtraction and comparison are NATIVE binary: the
  standard three-way `sub_mask` / `sub_mask_carry` construction, combined into
  one structurally-recursive `subBoth` (the eliminator gives no mutual
  recursion, so it returns the pair and recurses once). gcd is Euclid over the
  binary modulus, fuel-bounded exactly as ch11 (the fuel count is still unary —
  a parked optimization, same as ch11). Results are certified by `refl`
  (`divCert`, `modCert`, `divModInvariant`, `gcdCert`, …): the kernel runs the
  binary op and checks it against the unary answer, so "trusted implementation"
  stays a theorem. The GENERAL division-algorithm and gcd-divides theorems
  (for all inputs) remain course-of-values material — deferred, as ch11 left
  them; ch15 pins the readiness-gate form, not the universal proof. Gated by
  harness/listings_test.go (elaborate+check, normalize, emit+run under node).
- Rungs 2 and 3 above: wait for a workload the shadow and ch14 cannot
  serve.

## The division algorithm with Euclidean uniqueness — PROVED (2026-06), listings/ch16

The theorem chapters 11–13 repeatedly deferred to "the course-of-values
chapter" is now a checked listing. The flooring quotient and remainder are
built by STRUCTURAL recursion on the dividend (`divmod` counts up, resetting
the remainder and bumping the quotient exactly when the remainder would reach
the divisor), so totality is by construction. Proved over it:

- `divLaw : a = (a // b) * b + a % b` for ALL a, b (existence; at b = 0 it
  reads a = 0*b + a). This is ch11's deferred "general division-algorithm".
- `remBound : b > 0 → a % b < b` (the bound), via a boolean order `ltb` and
  its step lemma.
- `divUnique` / `remUnique`: any two bounded decompositions of the same number
  share quotient and remainder — the mismatched-quotient cases are impossible
  because the larger quotient forces a remainder ≥ b (`notLtSelfPlus`).
- `quotCanonical` / `remCanonical`: the headline EUCLIDEAN UNIQUENESS — any
  bounded `a = q*(b+1) + r` IS the flooring decomposition.

Proof technique worth recording: a function defined by a boolean test
(`eqNat (succ r) b`) is reasoned about by (1) refactoring the step into named
helpers (`stepByBool`, `divStep`) so the ι-reductions are clean, (2) rewriting
the stuck recursive call through surjective pairing (`dmEta`), and (3) a
`boolCase` convoy (motive `Eq Bool x z -> P`, discharged with `refl` at the
scrutinee) to read the deciding equation in each branch and `subst` it back
into the test. Gated by harness/listings_test.go.

This DISCHARGED the proof problem ch13's `//`/`%` at Rat raised (a floor is
well-defined on a quotient class only by this uniqueness), and those operators
are LIVE in ch13 and, on the canonical QPair representation, in ch203 (`floorQ`
/ `floorUnique`, the Rule-5 port). The division algorithm is specialised there to ch13's
arithmetic conventions, and `floorQ : Rat -> Rat` is a `qlift` whose respect
proof (`floorUnique`) is exactly the uniqueness theorem applied to the two
representatives' characterizations. The signed Int floor needed no sign-case
analysis: since `-b ≡ d·b (mod d+1)`, the floor numerator is
`(a + d·b) // (d+1) - b` with a single division, so `charEq`/`charLt` fall
straight out of `divLaw`/`remBound`. `// = floorQ ∘ (/)` and
`% = a - (a // b) · b`. Certified live: `floorPos` (7/2 // 1 = 3),
`floorNeg` (−5/2 // 1 = −3, rounding toward −∞), `fracHalf` (7/2 % 1 = 1/2).
The F-convention is earned, not decreed.
