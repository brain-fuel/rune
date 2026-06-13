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

## What stays parked

- **Call-site erasure of the 0-fragment.** Discovered building ch14:
  refl-headed proof BODIES erase to the unit token, but applications still
  pass their implicit/0-quantity arguments — so a proof mentioning a deep
  numeral carries it into the JS shadow, and ~1600-deep nesting overflows
  node's parser. ch14 sizes its showcase literals under that; the real fix
  is dropping 0-quantity arguments at application sites in codegen.Erase
  (the quantity is in the Pi type; erasure currently only consults Prop
  and type-hood).
- **Binary literals.** Numerals still parse to unary succ-chains (capped
  at 4096), so big constants must be written as `pO`/`pI` spines or
  computed. Parse-time expansion targeting a `builtin bin` binding — or
  core numerals proper — is the existing compressed-numerals parking-lot
  entry; ch14 makes it worth more.
- **Division/gcd in binary**, and using ch14 to discharge ch11's deferred
  course-of-values theorems: natural follow-ons, not needed to pin the
  pattern.
- Rungs 2 and 3 above: wait for a workload the shadow and ch14 cannot
  serve.
