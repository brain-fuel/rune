package harness

import "testing"

// These are DOCUMENTED, SKIPPED property stubs for the invariants Phase 1+ must
// satisfy. They are skipped, not absent, on purpose: they name the obligations now so
// the harness is the gate from day one, and they are the precise targets the
// mutation-testing layer will later hunt against. A surviving mutant in the region
// any of these guards is a hole in the CHECKER, not in the tests.
//
// Phase 0 ships no type checker, evaluator, conversion, or proof cache, so each stub
// records its invariant and its preconditions and skips.

// TestTypePreservation: elaboration and any reduction preserve well-typedness. If
// `check(t) = T` and `t` steps/normalizes to `t'`, then `check(t') = T`. The
// generator must produce WELL-TYPED core (the dual obligation is that ill-typed terms
// are rejected). Phase 1 (MLTT core with glued NbE).
func TestTypePreservation(t *testing.T) {
	t.Skip("phase 1: requires the type checker and NbE evaluator")
}

// TestConversionEquivalence: definitional conversion is an equivalence relation —
// reflexive, symmetric, transitive — on well-typed core. Phase 1 (conversion via
// glued NbE).
func TestConversionEquivalence(t *testing.T) {
	t.Skip("phase 1: requires conversion checking")
}

// TestConversionCongruence: conversion is a congruence — it is preserved by every
// term former (if a ≡ a' and b ≡ b' then App a b ≡ App a' b', and likewise under
// binders). Phase 1 (conversion via glued NbE).
func TestConversionCongruence(t *testing.T) {
	t.Skip("phase 1: requires conversion checking")
}

// TestProofCacheFrameLemma: a check's result is invariant under store changes that
// preserve the bodies it actually unfolded. Formally (ref_docs/
// rune-proof-cache-semantics.md): for well-typed core d, let (ok, U) = runConv(check d)
// in store S, where U is the set of hashes whose BODIES were unfolded via store.Unfold.
// For any S' agreeing with S on the bodies in U, runConv(check d) in S' returns
// (ok, U).
//
// PRECONDITION (recorded per the brief): checking on fixed, metavariable-free core is
// a DETERMINISTIC total function of its inputs and the bodies it unfolds. Determinism
// is what makes the frame property hold; elaboration's nondeterminism is deliberately
// kept out of this layer. Phase 1 wires store.Unfold into the conversion monad's
// write-only dependency log; this property guards that the barrier has no hole and
// that no certificate is minted without its DepSet.
func TestProofCacheFrameLemma(t *testing.T) {
	t.Skip("phase 1: requires the conversion monad's dependency log and proof cache")
}
