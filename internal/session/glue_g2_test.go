package session

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/core"
)

// G2 / G3 value-level gates for retiring the postulated `ua` head.
//
// The univalence line `ua A B f g s t` is meant to be DERIVED, not postulated, as
//
//	pabsU (λi. Glue B ((i=0)∨(i=1)) [i=0↦A; i=1↦B] [i=0↦(f,…); i=1↦idEquiv B])
//
// and `castU` along it should compute the forward map `f` — exactly the reduct the
// postulate's fiat rule (eval.go castU-over-ua) bakes in. The G1 arm
// (transpGlueEndpointTotal) already covers this SHAPE; these tests pin that it
// computes the forward map for a GENERAL equivalence with a VARYING type/equiv
// system (T: A@i0 / B@i1, e: real@i0 / idEquiv@i1) — i.e. the actual `ua` line, not
// the constant-system identity case glue_g1_test already pins.
//
// They are VALUE-level (the systems are built as Go closures) because a CHECKED
// surface listing of this line additionally needs dependent UF- and El-valued
// system formers (A8 `sys`): T is UF-valued and e's type `El (Equiv (T h) B)`
// varies per face, both beyond the non-dependent element-valued `fsplit`. That
// packaging — not the reduction proven here — is the remaining residue for a
// postulate-free CHECKED ch-listing; the kernel reduction is what these gate.

// uaLineParts builds the machine, the builtin handles, and the canonical
// derived-`ua` Glue line for a general forward map `f`, reused by both tests.
// `proofI1` is the (never-forced) contraction component of the i1 idEquiv — a knob
// G3 uses to confirm the arm ignores proof content.
func uaLineParts(t *testing.T, proofI1 core.Val) (*Session, *core.Machine, core.Val, core.Val, core.Val) {
	t.Helper()
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

	// A (domain), B (base), g0 (input), dummy (irrelevant payloads), f (a GENERAL
	// forward map — a free variable, so equivFun(real) g0 stays the neutral f g0
	// and cannot be confused with the identity).
	A := core.VVar(0)
	B := core.VVar(1)
	g0 := core.VVar(2)
	dummy := core.VVar(3)
	f := core.VVar(4)

	id := core.VLam{Name: "x", Icit: core.Expl, Body: func(x core.Val) core.Val { return x }}
	pair := func(a, b core.Val) core.Val { return ap(ref(pairH), dummy, dummy, a, b) }
	prefl := func(y core.Val) core.Val { return ap(ref(preflH), B, y) }

	// realEquiv = (f, _): equivFun projects f; its isEquiv proof is never forced
	// (unglue reads only the forward map). idEquiv = (id, λy. ((y, refl), proofI1)):
	// the arm reads fst∘fst of its fibre centre = the point, never proofI1.
	realEquiv := pair(f, dummy)
	idEquiv := pair(id, core.VLam{Name: "y", Icit: core.Expl, Body: func(y core.Val) core.Val {
		return pair(pair(y, prefl(y)), proofI1)
	}})

	isI1 := func(iv core.Val) bool {
		return strings.TrimSpace(s.show(m.Quote(0, iv))) == "i1"
	}
	phi := func(i core.Val) core.Val { return ap(ref(orH), ap(ref(eq0H), i), ap(ref(eq1H), i)) }
	// line = λi. Glue B (φ i) (T i) (e i) with the VARYING ua systems:
	//   T i = λh. [i=0 ↦ A; i=1 ↦ B]      e i = λh. [i=0 ↦ realEquiv; i=1 ↦ idEquiv]
	line := core.VLam{Name: "i", Icit: core.Expl, Body: func(i core.Val) core.Val {
		Tsys := core.VLam{Name: "h", Icit: core.Expl, Body: func(core.Val) core.Val {
			if isI1(i) {
				return B
			}
			return A
		}}
		esys := core.VLam{Name: "h", Icit: core.Expl, Body: func(core.Val) core.Val {
			if isI1(i) {
				return idEquiv
			}
			return realEquiv
		}}
		return ap(ref(glueH), B, phi(i), Tsys, esys)
	}}
	return s, m, line, g0, m.Apply(f, g0)
}

// TestUaGlueGeneralForwardMap is the G2 gate: transport along the DERIVED ua line
// for a GENERAL equivalence computes the forward map `f` applied to the input — the
// reduct the `ua` postulate's fiat rule provides — through the real transp-over-Glue
// arm, with a varying type/equiv system (the genuine ua shape). This is the
// commit-gate the postulate retirement requires: the derived body is a drop-in.
func TestUaGlueGeneralForwardMap(t *testing.T) {
	s, m, line, g0, wantV := uaLineParts(t, core.VVar(5))

	res, fired := m.TranspGlueEndpointTotalForTest(line, g0)
	if !fired {
		t.Fatal("transp-over-Glue arm did not fire on the derived ua line")
	}
	got := s.show(m.QuoteUnfold(6, res))
	want := s.show(m.QuoteUnfold(6, wantV)) // f g0
	if got != want {
		t.Fatalf("derived ua transport must apply the forward map: got %q, want %q", got, want)
	}
	// X2 canonicity: the result is a fully-reduced application, no residual Kan/Glue
	// eliminator left stuck.
	for _, bad := range []string{"transp", "unglue", "equivProof", "hcomp"} {
		if strings.Contains(got, bad) {
			t.Fatalf("derived ua transport must fully reduce, residual %q in %q", bad, got)
		}
	}
}

// TestUaGlueArmIgnoresProofContent is the G3 / X1 (R-FRAME) exactness pin: the arm
// branches on the fibre POINT (fst∘fst of the contractible-fibre centre), never on
// the contractibility PROOF content. Building the i1 idEquiv with two DIFFERENT
// (irrelevant) contraction proofs must yield the identical normal form — so the
// proof-cache key (defHash, ‖U‖) stays exact on the new arm; no markImprecise.
func TestUaGlueArmIgnoresProofContent(t *testing.T) {
	s1, m1, line1, g0a, _ := uaLineParts(t, core.VVar(5))
	res1, ok1 := m1.TranspGlueEndpointTotalForTest(line1, g0a)
	s2, m2, line2, g0b, _ := uaLineParts(t, core.VVar(7)) // different proof payload
	res2, ok2 := m2.TranspGlueEndpointTotalForTest(line2, g0b)
	if !ok1 || !ok2 {
		t.Fatal("arm did not fire")
	}
	got1 := s1.show(m1.QuoteUnfold(8, res1))
	got2 := s2.show(m2.QuoteUnfold(8, res2))
	if got1 != got2 {
		t.Fatalf("arm result must be independent of fibre-proof content: %q vs %q", got1, got2)
	}
}
