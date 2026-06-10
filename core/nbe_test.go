package core

import "testing"

// fakeGlobals is a minimal Globals for NbE tests: a map of definitions.
type fakeGlobals struct {
	types  map[Hash]Tm
	bodies map[Hash]Tm
}

func (g fakeGlobals) TypeOf(h Hash) (Tm, bool) { t, ok := g.types[h]; return t, ok }
func (g fakeGlobals) Unfold(h Hash) (Tm, bool) { b, ok := g.bodies[h]; return b, ok }

func emptyGlobals() fakeGlobals {
	return fakeGlobals{types: map[Hash]Tm{}, bodies: map[Hash]Tm{}}
}

// lam is shorthand for a one-binder lambda.
func lam(body Tm) Tm { return Lam{Body: Scope{Name: "x", Body: body}} }

func TestNormalizeBeta(t *testing.T) {
	m := NewMachine(emptyGlobals())
	// (λx. x) U  ~>  U
	tm := App{Fn: lam(Var{Idx: 0}), Arg: Univ{}}
	if got := m.Normalize(tm); got != (Univ{}) {
		t.Fatalf("got %#v, want Univ", got)
	}
}

func TestNormalizeUnderBinder(t *testing.T) {
	m := NewMachine(emptyGlobals())
	// λy. (λx. x) y  ~>  λy. y
	tm := lam(App{Fn: lam(Var{Idx: 0}), Arg: Var{Idx: 0}})
	want := lam(Var{Idx: 0})
	if got := m.Normalize(tm); HashTerm(got) != HashTerm(want) {
		t.Fatalf("got %#v, want λ.0", got)
	}
}

func TestNormalizeLetIsTransparent(t *testing.T) {
	m := NewMachine(emptyGlobals())
	// let x = U in x  ~>  U
	tm := Let{Val: Univ{}, Body: Scope{Name: "x", Body: Var{Idx: 0}}}
	if got := m.Normalize(tm); got != (Univ{}) {
		t.Fatalf("got %#v, want Univ", got)
	}
}

func TestQuoteKeepsRefsFolded(t *testing.T) {
	g := emptyGlobals()
	var h Hash
	h[0] = 7
	g.bodies[h] = Univ{}
	g.types[h] = Univ{}
	m := NewMachine(g)
	// A bare reference normalizes (without δ) to itself, and the dependency log
	// stays empty: nothing was forced.
	if got := m.Normalize(Ref{Hash: h}); got != (Ref{Hash: h}) {
		t.Fatalf("got %#v, want Ref", got)
	}
	if len(m.Deps) != 0 {
		t.Fatalf("fast path logged deps: %v", m.DepList())
	}
	// The δ-normalizing variant unfolds it — and logs it.
	if got := m.NormalizeUnfold(Ref{Hash: h}); got != (Univ{}) {
		t.Fatalf("unfold: got %#v, want Univ", got)
	}
	if _, ok := m.Deps[h]; !ok || len(m.Deps) != 1 {
		t.Fatalf("unfold did not log exactly the forced ref: %v", m.DepList())
	}
}

func TestConvRefl(t *testing.T) {
	m := NewMachine(emptyGlobals())
	tm := Pi{Dom: Univ{}, Cod: Scope{Name: "A", Body: Pi{Dom: Var{Idx: 0}, Cod: Scope{Name: "_", Body: Var{Idx: 1}}}}}
	a := m.Eval(nil, tm)
	b := m.Eval(nil, tm)
	if !m.Conv(0, a, b) {
		t.Fatal("conversion not reflexive on (A : U) -> A -> A")
	}
}

func TestConvEta(t *testing.T) {
	m := NewMachine(emptyGlobals())
	// Under a binder f : A -> A, conv must equate f and λx. f x.
	f := VVar(0)
	etaExpanded := VLam{Body: func(v Val) Val { return m.Apply(f, v) }}
	if !m.Conv(1, f, etaExpanded) {
		t.Fatal("η: f ≢ λx. f x")
	}
}

func TestConvDeltaFastAndSlowPaths(t *testing.T) {
	g := emptyGlobals()
	var h1, h2 Hash
	h1[0], h2[0] = 1, 2
	// Two definitions with literally different hashes but the same body U.
	g.bodies[h1], g.bodies[h2] = Univ{}, Univ{}
	g.types[h1], g.types[h2] = Univ{}, Univ{}

	// Fast path: same ref converts with NO unfolding logged.
	m := NewMachine(g)
	if !m.Conv(0, m.Eval(nil, Ref{Hash: h1}), m.Eval(nil, Ref{Hash: h1})) {
		t.Fatal("same ref not convertible")
	}
	if len(m.Deps) != 0 {
		t.Fatalf("fast path logged deps: %v", m.DepList())
	}

	// Slow path: different refs with equal bodies convert only by forcing, and
	// both unfoldings are logged.
	m2 := NewMachine(g)
	if !m2.Conv(0, m2.Eval(nil, Ref{Hash: h1}), m2.Eval(nil, Ref{Hash: h2})) {
		t.Fatal("definitionally equal refs not convertible")
	}
	if len(m2.Deps) != 2 {
		t.Fatalf("slow path should log both unfolds, got %v", m2.DepList())
	}
}

func TestConvDistinguishes(t *testing.T) {
	m := NewMachine(emptyGlobals())
	a := m.Eval(nil, Pi{Dom: Univ{}, Cod: Scope{Body: Var{Idx: 0}}})
	b := m.Eval(nil, Pi{Dom: Univ{}, Cod: Scope{Body: Univ{}}})
	if m.Conv(0, a, b) {
		t.Fatal("(A : U) -> A and U -> U should not convert")
	}
}

func TestNormalizeIdempotent(t *testing.T) {
	m := NewMachine(emptyGlobals())
	tm := App{
		Fn:  lam(lam(App{Fn: Var{Idx: 1}, Arg: Var{Idx: 0}})),
		Arg: lam(Var{Idx: 0}),
	}
	nf1 := m.Normalize(tm)
	nf2 := m.Normalize(nf1)
	if HashTerm(nf1) != HashTerm(nf2) {
		t.Fatalf("normalize not idempotent: %#v vs %#v", nf1, nf2)
	}
}
