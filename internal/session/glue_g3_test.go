package session

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/core"
)

// TestUaLineViaA8Primitives is the A8 e-system milestone: the FULL univalence Glue
// line is assembled from BOTH A8 split-formers — `sysU` for the UF-valued
// T-component (A@i0 / B@i1) and `fsplitD` for the dependent e-component (the general
// equivalence @i0 / idEquiv @i1) — and transport along it (the `castU`-over-ua
// route) computes the forward map. Unlike glue_g2 (whose T/e systems were Go
// closures standing in for the not-yet-built primitives), here the line is built
// from the genuine kernel `sysU`/`fsplitD` spines, so this pins that the two A8
// primitives COMPOSE into the real ua line and the reduction flows through them.
//
// The remaining residue for a fully CHECKED listing is purely elaboration: the
// fsplitD e-branch `realEquiv : Equiv A B` only checks against `Equiv (sysU…) B`
// under a face-restricted context (where sysU/T reduces) — the value the kernel
// already computes here, awaiting the face-restricted branch checker.
func TestUaLineViaA8Primitives(t *testing.T) {
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
	sysUH, ok7 := s.st.SysUHash()
	splitDH, ok8 := s.st.SplitDHash()
	if !(ok1 && ok2 && ok3 && ok4 && ok5 && ok6 && ok7 && ok8) {
		t.Fatal("missing builtin group hashes")
	}

	A := core.VVar(0)
	B := core.VVar(1)
	g0 := core.VVar(2)
	dummy := core.VVar(3)
	f := core.VVar(4)

	id := core.VLam{Name: "x", Icit: core.Expl, Body: func(x core.Val) core.Val { return x }}
	pair := func(a, b core.Val) core.Val { return ap(ref(pairH), dummy, dummy, a, b) }
	prefl := func(y core.Val) core.Val { return ap(ref(preflH), B, y) }
	realEquiv := pair(f, dummy) // equivFun projects f
	idEquiv := pair(id, core.VLam{Name: "y", Icit: core.Expl, Body: func(y core.Val) core.Val {
		return pair(pair(y, prefl(y)), dummy)
	}})

	konst := func(val core.Val) core.Val {
		return core.VLam{Name: "h", Icit: core.Expl, Body: func(core.Val) core.Val { return val }}
	}
	phi := func(i core.Val) core.Val { return ap(ref(orH), ap(ref(eq0H), i), ap(ref(eq1H), i)) }
	// line = λi. Glue B (φ i) (T i) (e i) with the A8-built systems:
	//   T i = λh. sysU    (ieq0 i) (ieq1 i) [λ_.A] [λ_.B] h          (A@i0, B@i1)
	//   e i = λh. fsplitD (ieq0 i) (ieq1 i) C [λ_.realEquiv] [λ_.idEquiv] h  (real@i0, id@i1)
	line := core.VLam{Name: "i", Icit: core.Expl, Body: func(i core.Val) core.Val {
		e0i, e1i := ap(ref(eq0H), i), ap(ref(eq1H), i)
		Tsys := core.VLam{Name: "h", Icit: core.Expl, Body: func(h core.Val) core.Val {
			return ap(ref(sysUH), e0i, e1i, konst(A), konst(B), h)
		}}
		esys := core.VLam{Name: "h", Icit: core.Expl, Body: func(h core.Val) core.Val {
			// motive is ι-irrelevant to the reduct; a dummy constant suffices.
			return ap(ref(splitDH), e0i, e1i, konst(dummy), konst(realEquiv), konst(idEquiv), h)
		}}
		return ap(ref(glueH), B, phi(i), Tsys, esys)
	}}

	res, fired := m.TranspGlueEndpointTotalForTest(line, g0)
	if !fired {
		t.Fatal("transp-over-Glue arm did not fire on the A8-built ua line")
	}
	got := s.show(m.QuoteUnfold(5, res))
	want := s.show(m.QuoteUnfold(5, m.Apply(f, g0))) // f g0
	if got != want {
		t.Fatalf("ua line via sysU+fsplitD must apply the forward map: got %q, want %q", got, want)
	}
	for _, bad := range []string{"transp", "unglue", "equivProof", "sysU", "fsplitD"} {
		if strings.Contains(got, bad) {
			t.Fatalf("ua transport must fully reduce, residual %q in %q", bad, got)
		}
	}
}

// TestTranspGlueSingleAtom pins the FRE-§7 increment: transp-over-Glue now fires for
// a SINGLE-ATOM face (definite-but-not-total-at-both-endpoints), not only the
// endpoint-total ua shape. Two cases, constant base B:
//   - φ i = ieq0 i  (⊤ at i0, ⊥ at i1): input is a T-element, output is the base;
//     transp ~> unglue g0 = equivFun(e i0 htop) g0 = f g0.
//   - φ i = ieq1 i  (⊥ at i0, ⊤ at i1): input is the base, output is the T-fibre at
//     i1; with e i1 = idEquiv the fibre centre is the input itself, transp ~> g0.
func TestTranspGlueSingleAtom(t *testing.T) {
	s := New()
	m := s.wiredMachine()
	ref := func(h core.Hash) core.Val { return m.Eval(nil, core.Ref{Hash: h}) }
	ap := func(f core.Val, xs ...core.Val) core.Val {
		for _, x := range xs {
			f = m.Apply(f, x)
		}
		return f
	}
	glueH, _ := s.st.GlueHash(core.URoleGlue)
	eq0H, _ := s.st.FaceHash(core.CRoleEq0)
	eq1H, _ := s.st.FaceHash(core.CRoleEq1)
	pairH, _ := s.st.SigmaHash(core.GRolePair)
	preflH, _ := s.st.FibHash(core.FRolePrefl)

	A := core.VVar(0)
	B := core.VVar(1)
	g0 := core.VVar(2)
	dummy := core.VVar(3)
	f := core.VVar(4)
	id := core.VLam{Name: "x", Icit: core.Expl, Body: func(x core.Val) core.Val { return x }}
	pair := func(a, b core.Val) core.Val { return ap(ref(pairH), dummy, dummy, a, b) }
	prefl := func(y core.Val) core.Val { return ap(ref(preflH), B, y) }
	realEquiv := pair(f, dummy)
	idEquiv := pair(id, core.VLam{Name: "y", Icit: core.Expl, Body: func(y core.Val) core.Val {
		return pair(pair(y, prefl(y)), dummy)
	}})
	konst := func(val core.Val) core.Val {
		return core.VLam{Name: "h", Icit: core.Expl, Body: func(core.Val) core.Val { return val }}
	}
	mkLine := func(face core.Hash, equiv core.Val) core.Val {
		return core.VLam{Name: "i", Icit: core.Expl, Body: func(i core.Val) core.Val {
			return ap(ref(glueH), B, ap(ref(face), i), konst(A), konst(equiv))
		}}
	}
	chk := func(name string, line, want core.Val) {
		res, fired := m.TranspGlueEndpointTotalForTest(line, g0)
		if !fired {
			t.Fatalf("%s: arm did not fire", name)
		}
		if got, w := s.show(m.QuoteUnfold(5, res)), s.show(m.QuoteUnfold(5, want)); got != w {
			t.Fatalf("%s: got %q, want %q", name, got, w)
		}
	}
	// ieq0 i: ⊤ at i0 / ⊥ at i1 → f g0.
	chk("ieq0", mkLine(eq0H, realEquiv), m.Apply(f, g0))
	// ieq1 i: ⊥ at i0 / ⊤ at i1, e=idEquiv → g0.
	chk("ieq1", mkLine(eq1H, idEquiv), g0)
}

// TestTranspGlueVaryingBase pins that the general arm no longer requires a CONSTANT
// base: with a genuinely-varying base line (A@i0 / B@i1, abstract) and a single-atom
// face ieq0 i (⊤@i0, ⊥@i1), transport fires and reconciles the base by transporting
// the unglued input along the base line — `transp (λi. base i) (f g0)` — rather than
// staying stuck. (For an abstract base the transp itself stays neutral, which is the
// correct compGlue reduct; the point is the arm FIRES and produces it.)
func TestTranspGlueVaryingBase(t *testing.T) {
	s := New()
	m := s.wiredMachine()
	ref := func(h core.Hash) core.Val { return m.Eval(nil, core.Ref{Hash: h}) }
	ap := func(f core.Val, xs ...core.Val) core.Val {
		for _, x := range xs {
			f = m.Apply(f, x)
		}
		return f
	}
	glueH, _ := s.st.GlueHash(core.URoleGlue)
	eq0H, _ := s.st.FaceHash(core.CRoleEq0)
	pairH, _ := s.st.SigmaHash(core.GRolePair)

	g0 := core.VVar(2)
	dummy := core.VVar(3)
	f := core.VVar(4)
	bfun := core.VVar(5) // a free I -> UF, so the base genuinely varies in i
	realEquiv := ap(ref(pairH), dummy, dummy, f, dummy)
	konst := func(v core.Val) core.Val {
		return core.VLam{Name: "h", Icit: core.Expl, Body: func(core.Val) core.Val { return v }}
	}
	// VARYING base: base i = bfun i (mentions i symbolically), face ieq0 i (⊤@i0,⊥@i1).
	line := core.VLam{Name: "i", Icit: core.Expl, Body: func(i core.Val) core.Val {
		base := m.Apply(bfun, i)
		return ap(ref(glueH), base, ap(ref(eq0H), i), konst(base), konst(realEquiv))
	}}
	res, fired := m.TranspGlueEndpointTotalForTest(line, g0)
	if !fired {
		t.Fatal("arm did not fire on a varying-base Glue line")
	}
	// φ i1 = ⊥, so the result is the base part f g0 transported along the base line —
	// `transp (λi. bfun i) (f g0)` — which stays neutral over an abstract base (the
	// correct compGlue reduct), confirming the base is reconciled, not assumed constant.
	if got := s.show(m.QuoteUnfold(6, res)); !strings.Contains(got, "transp") {
		t.Fatalf("varying-base transport must reconcile the base via transp, got %q", got)
	}
}
