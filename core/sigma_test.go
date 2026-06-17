package core

import "testing"

// C1 / R-SUM: the negative (η-style) dependent pair in the core. β for the
// projections, definitional η for pairs, neutral projections staying stuck, and
// the hash-format bump.

func TestSigmaProjBeta(t *testing.T) {
	m := NewMachine(emptyGlobals())
	// Fst (Pair _ _ U Prop) ~> U ; Snd ~> Prop.
	pair := Pair{Dom: Univ{}, Cod: Scope{Name: "_", Body: Univ{}}, A: Univ{}, B: Prop{}}
	if got := m.Normalize(Fst{P: pair}); got != (Univ{}) {
		t.Fatalf("Fst (pair U Prop) = %#v, want Univ", got)
	}
	if got := m.Normalize(Snd{P: pair}); got != (Prop{}) {
		t.Fatalf("Snd (pair U Prop) = %#v, want Prop", got)
	}
}

func TestSigmaEta(t *testing.T) {
	m := NewMachine(emptyGlobals())
	// Under a binder p : Σ U (λ_. U), conv must equate p and (Fst p, Snd p).
	p := VVar(0)
	sig := VSig{Dom: VU{}, Cod: func(Val) Val { return VU{} }}
	_ = sig
	pair := VPair{
		Dom: VU{}, Cod: func(Val) Val { return VU{} },
		A: m.proj1(p), B: m.proj2(p),
	}
	if !m.Conv(1, p, pair) {
		t.Fatal("η: p ≢ (Fst p, Snd p)")
	}
	// symmetric trigger (pair on the left)
	if !m.Conv(1, pair, p) {
		t.Fatal("η (symmetric): (Fst p, Snd p) ≢ p")
	}
}

func TestSigmaNeutralProjStaysStuck(t *testing.T) {
	m := NewMachine(emptyGlobals())
	// Fst of a neutral variable quotes back to a Fst term (no β to fire).
	got := m.Quote(1, m.proj1(VVar(0)))
	if _, ok := got.(Fst); !ok {
		t.Fatalf("Fst of a neutral should stay Fst, got %#v", got)
	}
	got2 := m.Quote(1, m.proj2(VVar(0)))
	if _, ok := got2.(Snd); !ok {
		t.Fatalf("Snd of a neutral should stay Snd, got %#v", got2)
	}
}

func TestSigmaQuoteRoundTrip(t *testing.T) {
	m := NewMachine(emptyGlobals())
	// Σ (x : U), x  evaluates and quotes back to the same term (by hash).
	sig := Sig{Dom: Univ{}, Cod: Scope{Name: "x", Body: Var{Idx: 0}}}
	if got := m.Normalize(sig); HashTerm(got) != HashTerm(sig) {
		t.Fatalf("Sig round-trip changed the term: %#v", got)
	}
}

func TestSigmaConvCongruence(t *testing.T) {
	m := NewMachine(emptyGlobals())
	// Σ U (λ_. U) converts to itself; Σ U (λ_. U) does NOT convert to Σ Prop (λ_. U).
	a := m.Eval(nil, Sig{Dom: Univ{}, Cod: Scope{Body: Univ{}}})
	b := m.Eval(nil, Sig{Dom: Univ{}, Cod: Scope{Body: Univ{}}})
	if !m.Conv(0, a, b) {
		t.Fatal("Σ not reflexively convertible")
	}
	c := m.Eval(nil, Sig{Dom: Prop{}, Cod: Scope{Body: Univ{}}})
	if m.Conv(0, a, c) {
		t.Fatal("Σ U _ wrongly convertible to Σ Prop _")
	}
}

func TestHashFormatVersionIsSix(t *testing.T) {
	// 0x05 added Σ (this slice); 0x06 is the C7 / R-NUM default-NatLit-lowering
	// bump (see core/natlit_test.go TestHashFormatVersionBumped).
	if hashFormatVersion != 0x06 {
		t.Fatalf("hashFormatVersion = %#x, want 0x06", hashFormatVersion)
	}
}
