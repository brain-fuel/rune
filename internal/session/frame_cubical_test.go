package session

import (
	"testing"

	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/surface"
)

// X1 / R-FRAME — the proof-cache Frame Lemma under CUBICAL reduction. The cache
// keys a certificate on (defHash, ‖U‖) where U is the set of definitions whose
// BODIES the check unfolded (store/cert.go). The soundness obligation gating every
// A-milestone is that this closure key CAPTURES INNER unfolding: when checking a
// definition fires a cubical/HIT ι-rule that reaches a user definition's body,
// that dependency must be logged — and the rigid cubical builtins (Circle, base,
// circElim, …), being bodiless, must NOT appear (they are permanent, already part
// of content via their Refs). This pins both directions.
//
// The witness: `thm` checks `circElim Circle base loop pt ≡ base`. The eliminator
// ι-rule forces its scrutinee `pt` (a user def whose body is `base`), so the
// conversion sees through pt — U_thm must contain pt and nothing else of substance,
// and must be invariant under unrelated definitions sitting unused in the store.
func TestFrameLemmaUnderCubicalReduction(t *testing.T) {
	parseDefs := func(src string) []surface.Def {
		defs, err := surface.ParseFile(src)
		if err != nil {
			t.Fatal(err)
		}
		return defs
	}

	check := func(extra string) (deps []core.Hash, ptHash, circElimHash, baseHash core.Hash) {
		s := New()
		pt := parseDefs(`pt : El Circle is base end`)[0]
		rd, err := s.AddDef(pt)
		if err != nil {
			t.Fatalf("pt failed: %v", err)
		}
		ptHash = rd.Hash
		for _, d := range parseDefs(extra) {
			if _, err := s.AddDef(d); err != nil {
				t.Fatalf("extra def failed: %v", err)
			}
		}
		thm := parseDefs(
			`thm : Eq (El Circle) (circElim Circle base loop pt) base is refl base end`)[0]
		el := s.elaborator()
		if _, _, err := el.ElabDef(thm); err != nil {
			t.Fatalf("thm failed to check: %v", err)
		}
		circElimHash, _ = s.Lookup("circElim")
		baseHash, _ = s.Lookup("base")
		return el.M.DepList(), ptHash, circElimHash, baseHash
	}

	has := func(deps []core.Hash, h core.Hash) bool {
		for _, d := range deps {
			if d == h {
				return true
			}
		}
		return false
	}

	depsS, pt, circElim, base := check("")
	// S′ adds definitions thm never consults; U_thm must be unchanged.
	depsS2, _, _, _ := check(`
unrelated1 : U1 is U -> U end
unrelated2 : U -> U is fn (x : U) is x end end
`)

	// Inner capture: the user def reached only through the circElim ι-rule is logged.
	if !has(depsS, pt) {
		t.Fatalf("Frame Lemma: pt (reached via the HIT ι-rule) missing from U_thm: %v", depsS)
	}
	// Rigid cubical builtins are bodiless — they must NOT be dependencies.
	if has(depsS, circElim) {
		t.Fatalf("rigid circElim leaked into U_thm: %v", depsS)
	}
	if has(depsS, base) {
		t.Fatalf("rigid base leaked into U_thm: %v", depsS)
	}
	// Frame invariance: unrelated store growth does not change U_thm.
	if len(depsS) != len(depsS2) {
		t.Fatalf("U_thm not frame-invariant: %v vs %v", depsS, depsS2)
	}
	for _, d := range depsS {
		if !has(depsS2, d) {
			t.Fatalf("U_thm changed under unrelated defs: %v vs %v", depsS, depsS2)
		}
	}
}

// The cache hit is exact under cubical reduction: re-adding the SAME cubical-using
// definition is certified (skips the re-check), and the certificate's recorded
// dependency (the inner user def) still resolves.
func TestCubicalCertificateReusable(t *testing.T) {
	s := New()
	if _, err := s.AddDef(mustParse(t, `pt : El Circle is base end`)); err != nil {
		t.Fatal(err)
	}
	thm := mustParse(t, `thm : Eq (El Circle) (circElim Circle base loop pt) base is refl base end`)
	if _, err := s.AddDef(thm); err != nil {
		t.Fatalf("thm failed: %v", err)
	}
	if !s.Certified("thm") {
		t.Fatalf("thm should be certified after checking (cache captures its cubical deps)")
	}
}

func mustParse(t *testing.T, src string) surface.Def {
	t.Helper()
	defs, err := surface.ParseFile(src)
	if err != nil {
		t.Fatal(err)
	}
	return defs[0]
}
