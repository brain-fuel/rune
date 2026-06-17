package session

import (
	"testing"

	"goforge.dev/rune/v3/core"
)

// TestTranspGlueEndpointTotalComputes is the G1 gate: the general transp-over-Glue
// arm (transpGlueEndpointTotal) computes for the univalence shape — a Glue line
// with a CONSTANT base and a face φ that VARIES (φ i = (ieq0 i) ∨ (ieq1 i)) but is
// TOTAL at both endpoints. This is exactly the case transpGlueIntro does NOT cover
// (its input here is the neutral g0, and φ is not constant in i).
//
// With the glued equivalence the identity (idEquiv = pairF id (λy. ((y, refl), _))),
// transport along the line is the identity: unglue pulls g0 down to the base as
// equivFun(id) g0 = g0, and the i1 fibre-centre reconstruction
// fst (fst (equivProof id g0)) = g0. So the arm must reduce `transp line g0` to g0
// — the oracle that pins both the unglue-⊤ base step and the fibre-centre step.
func TestTranspGlueEndpointTotalComputes(t *testing.T) {
	s := New()
	m := s.wiredMachine()
	ref := func(h core.Hash) core.Val { return m.Eval(nil, core.Ref{Hash: h}) }
	ap := func(f core.Val, xs ...core.Val) core.Val {
		for _, x := range xs {
			f = m.Apply(f, x)
		}
		return f
	}

	glueH, ok1 := s.st.GlueHash(core.URoleGlue)
	eq0H, ok2 := s.st.FaceHash(core.CRoleEq0)
	eq1H, ok3 := s.st.FaceHash(core.CRoleEq1)
	orH, ok4 := s.st.FaceHash(core.CRoleOr)
	pairH, ok5 := s.st.SigmaHash(core.GRolePair)
	preflH, ok6 := s.st.FibHash(core.FRolePrefl)
	if !(ok1 && ok2 && ok3 && ok4 && ok5 && ok6) {
		t.Fatal("missing builtin group hashes")
	}

	// Free variables: B (base : UF) = lvl 0, g0 (the input) = lvl 1, dummy = lvl 2.
	B := core.VVar(0)
	g0 := core.VVar(1)
	dummy := core.VVar(2)

	idfun := core.VLam{Name: "x", Icit: core.Expl, Body: func(x core.Val) core.Val { return x }}
	pair := func(a, b core.Val) core.Val { return ap(ref(pairH), dummy, dummy, a, b) }
	prefl := func(y core.Val) core.Val { return ap(ref(preflH), B, y) }
	// idEquiv = (id, λy. ((y, refl y), _)): the second isContr component is never
	// forced by transport (the arm reads only the fibre centre), so a dummy suffices.
	idIsEquiv := core.VLam{Name: "y", Icit: core.Expl, Body: func(y core.Val) core.Val {
		return pair(pair(y, prefl(y)), dummy)
	}}
	idEquiv := pair(idfun, idIsEquiv)

	phi := func(i core.Val) core.Val { return ap(ref(orH), ap(ref(eq0H), i), ap(ref(eq1H), i)) }
	Tsys := core.VLam{Name: "h", Icit: core.Expl, Body: func(core.Val) core.Val { return B }}
	esys := core.VLam{Name: "h", Icit: core.Expl, Body: func(core.Val) core.Val { return idEquiv }}
	// line = λi. Glue B (φ i) (λh. B) (λh. idEquiv)
	line := core.VLam{Name: "i", Icit: core.Expl, Body: func(i core.Val) core.Val {
		return ap(ref(glueH), B, phi(i), Tsys, esys)
	}}

	res, fired := m.TranspGlueEndpointTotalForTest(line, g0)
	if !fired {
		t.Fatal("transpGlueEndpointTotal did not fire on the endpoint-total constant-base line")
	}
	// QuoteUnfold: the result is g0 up to δ (equivFun/fstF unfold to the identity),
	// which is exactly the equality conversion / refl-pins check.
	got := s.show(m.QuoteUnfold(3, res))
	want := s.show(m.QuoteUnfold(3, g0))
	if got != want {
		t.Fatalf("transport along an identity ua-line must be the identity: got %q, want %q", got, want)
	}
}
