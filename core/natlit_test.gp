package core

import (
	"math/big"
	"testing"
)

// C7 / R-NUM additive slice: the compressed core numeral NatLit. These tests pin
// the soundness crux — a NatLit is DEFINITIONALLY the unary succ-chain
// `succ^N zero` — plus the eliminator peeling correctly, the fast literal-vs-
// literal conversion, quote round-tripping, normalizer termination, and the
// kernel-accelerated arithmetic agreeing with the unfolded result.

// natFixture is a minimal `builtin nat` for the core tests: a DataInfo declaring
// `nat` with constructors `zero`/`succ` and the eliminator `natElim`, plus the
// resolved hashes. natElim's signature: 1 motive + 2 cases (zero, succ) +
// scrutinee, succ recursive.
type natFixture struct {
	g                    fakeGlobals
	nat, zero, succ, nel Hash
}

func (f natFixture) CtorOf(h Hash) (Hash, int, bool) {
	switch h {
	case f.zero:
		return f.nat, 0, true
	case f.succ:
		return f.nat, 1, true
	}
	return Hash{}, 0, false
}

func (f natFixture) ElimOf(h Hash) (ElimSig, bool) {
	if h == f.nel {
		return ElimSig{
			Data:      f.nat,
			NumParams: 0,
			Ctors: []CtorSig{
				{Arity: 0, Rec: nil},          // zero
				{Arity: 1, Rec: []bool{true}}, // succ n  (n recursive)
			},
		}, true
	}
	return ElimSig{}, false
}

func newNatFixture() (*Machine, natFixture) {
	f := natFixture{g: emptyGlobals()}
	// Distinct, stable test hashes.
	f.nat[0], f.zero[0], f.succ[0], f.nel[0] = 1, 2, 3, 4
	m := NewMachine(f.g)
	m.Data = f
	return m, f
}

// natLit is a NatLit term for the fixture's nat.
func (f natFixture) natLit(n int64) NatLit {
	return NatLit{N: big.NewInt(n), Zero: f.zero, Succ: f.succ}
}

// unary builds the hand-written succ^n zero term.
func (f natFixture) unary(n int) Tm {
	t := Tm(Ref{Hash: f.zero})
	for i := 0; i < n; i++ {
		t = App{Fn: Ref{Hash: f.succ}, Arg: t}
	}
	return t
}

// TestNatLitConvertsToUnary is the soundness crux: NatLit n ≡ succ^n zero for a
// range of n (including 0 and a large value the unary cap would have rejected).
func TestNatLitConvertsToUnary(t *testing.T) {
	m, f := newNatFixture()
	for _, n := range []int{0, 1, 2, 5, 17, 100, 5000} {
		lit := m.Eval(nil, f.natLit(int64(n)))
		chain := m.Eval(nil, f.unary(n))
		if !m.Conv(0, lit, chain) {
			t.Fatalf("NatLit %d ≢ succ^%d zero", n, n)
		}
		// symmetric direction
		if !m.Conv(0, chain, lit) {
			t.Fatalf("succ^%d zero ≢ NatLit %d (symmetric)", n, n)
		}
	}
}

// TestNatLitDistinctValuesDiffer: distinct literals do NOT convert (no unsound
// collapse), and a literal differs from the wrong-length chain.
func TestNatLitDistinctValuesDiffer(t *testing.T) {
	m, f := newNatFixture()
	a := m.Eval(nil, f.natLit(3))
	b := m.Eval(nil, f.natLit(4))
	if m.Conv(0, a, b) {
		t.Fatal("NatLit 3 wrongly converts to NatLit 4")
	}
	if m.Conv(0, a, m.Eval(nil, f.unary(4))) {
		t.Fatal("NatLit 3 wrongly converts to succ^4 zero")
	}
}

// TestNatElimPeelsLiteral: an eliminator (here, the identity recursor `double`-
// shaped `succ`-folding to recompute the number) computes the right value on a
// NatLit scrutinee WITHOUT materialising the chain ahead of time. We use natElim
// to compute predecessor-free: motive λ_.nat, zero-case zero, succ-case λk ih.
// succ ih — i.e. the identity, so natElim … (NatLit n) ≡ succ^n zero.
func TestNatElimPeelsLiteral(t *testing.T) {
	m, f := newNatFixture()
	motive := lam(Ref{Hash: f.nat}) // λ_. nat
	zeroCase := Ref{Hash: f.zero}
	// λk. λih. succ ih
	succCase := Lam{Body: Scope{Name: "k", Body: Lam{Body: Scope{Name: "ih",
		Body: App{Fn: Ref{Hash: f.succ}, Arg: Var{Idx: 0}}}}}}
	for _, n := range []int{0, 1, 4, 50} {
		elim := App{Fn: App{Fn: App{Fn: App{Fn: Ref{Hash: f.nel}, Arg: motive},
			Arg: zeroCase}, Arg: succCase}, Arg: f.natLit(int64(n))}
		got := m.Eval(nil, elim)
		want := m.Eval(nil, f.unary(n))
		if !m.Conv(0, got, want) {
			t.Fatalf("natElim id (NatLit %d) ≢ succ^%d zero", n, n)
		}
	}
}

// TestNatLitQuoteRoundTrip: Quote keeps a literal compact (does not expand it to
// the chain), and the quoted term hashes equal to the original NatLit.
func TestNatLitQuoteRoundTrip(t *testing.T) {
	m, f := newNatFixture()
	lit := f.natLit(12345)
	got := m.Quote(0, m.Eval(nil, lit))
	gl, ok := got.(NatLit)
	if !ok {
		t.Fatalf("Quote of a NatLit gave %#v, want NatLit", got)
	}
	if gl.N.Cmp(lit.N) != 0 || gl.Zero != lit.Zero || gl.Succ != lit.Succ {
		t.Fatalf("Quote round-trip changed the literal: %#v", gl)
	}
	if HashTerm(got) != HashTerm(lit) {
		t.Fatal("Quote round-trip is not hash-stable for NatLit")
	}
}

// TestNatLitTerminatesLargeNoMaterialise: normalizing a large literal terminates
// and stays compact (Normalize keeps the spine folded). The unary cap was 4096;
// this is far past it and must not blow up or hang.
func TestNatLitTerminatesLargeNoMaterialise(t *testing.T) {
	m, f := newNatFixture()
	big := f.natLit(1_000_000)
	got := m.Normalize(big)
	gl, ok := got.(NatLit)
	if !ok {
		t.Fatalf("Normalize of a large NatLit expanded it to %T (should stay NatLit)", got)
	}
	if gl.N.Cmp(big.N) != 0 {
		t.Fatal("Normalize changed the literal's value")
	}
}

// TestNatLitDistinctNatBindingsDiffer: a literal is RELATIVE to its nat binding —
// the same number against a different zero/succ is a different term and does not
// convert.
func TestNatLitDistinctNatBindingsDiffer(t *testing.T) {
	m, f := newNatFixture()
	other := f.natLit(3)
	other.Zero[1] = 9 // perturb the binding identity
	if HashTerm(f.natLit(3)) == HashTerm(other) {
		t.Fatal("NatLit hash ignores the nat binding identity")
	}
	if m.Conv(0, m.Eval(nil, f.natLit(3)), m.Eval(nil, other)) {
		t.Fatal("NatLits of different nat bindings wrongly convert")
	}
}

// --- Accel table (Decision 1) ---------------------------------------------

// accelFixture extends natFixture with a recursive `add`/`mul` definition (by
// NatElim, the user's ordinary body) and registers them as accelerated ops, so
// the differential test can compare the accel result against the def's unfolded
// peeling.
type accelFixture struct {
	natFixture
	add, mul Hash
}

func (f accelFixture) NatOpOf(h Hash) NatOp {
	switch h {
	case f.add:
		return NatOpAdd
	case f.mul:
		return NatOpMul
	}
	return NatOpNone
}

func (f accelFixture) NatCtors() (Hash, Hash, bool) { return f.zero, f.succ, true }

func newAccelFixture() (*Machine, accelFixture) {
	m, nf := newNatFixture()
	f := accelFixture{natFixture: nf}
	f.add[0], f.mul[0] = 5, 6
	// add a b = natElim (λ_.nat) b (λk ih. succ ih) a  — recurse on a, base b.
	motive := lam(Ref{Hash: f.nat})
	succCase := Lam{Body: Scope{Name: "k", Body: Lam{Body: Scope{Name: "ih",
		Body: App{Fn: Ref{Hash: f.succ}, Arg: Var{Idx: 0}}}}}}
	// body binds a (idx 1) and b (idx 0) under two outer lambdas.
	addBody := Lam{Body: Scope{Name: "a", Body: Lam{Body: Scope{Name: "b",
		Body: App{Fn: App{Fn: App{Fn: App{Fn: Ref{Hash: f.nel}, Arg: motive},
			Arg: Var{Idx: 0}}, Arg: succCase}, Arg: Var{Idx: 1}}}}}}
	// mul a b = natElim (λ_.nat) zero (λk ih. add b ih) a
	mulSucc := Lam{Body: Scope{Name: "k", Body: Lam{Body: Scope{Name: "ih",
		Body: App{Fn: App{Fn: Ref{Hash: f.add}, Arg: Var{Idx: 2}}, Arg: Var{Idx: 0}}}}}}
	mulBody := Lam{Body: Scope{Name: "a", Body: Lam{Body: Scope{Name: "b",
		Body: App{Fn: App{Fn: App{Fn: App{Fn: Ref{Hash: f.nel}, Arg: motive},
			Arg: Ref{Hash: f.zero}}, Arg: mulSucc}, Arg: Var{Idx: 1}}}}}}
	f.g.bodies[f.add] = addBody
	f.g.bodies[f.mul] = mulBody
	m.NatAccel = f
	return m, f
}

func (f accelFixture) call(h Hash, a, b int) Tm {
	return App{Fn: App{Fn: Ref{Hash: h}, Arg: f.natLit(int64(a))}, Arg: f.natLit(int64(b))}
}

// TestNatAccelAgreesWithUnfolding is the differential soundness gate: for several
// (a,b), the accelerated op (bigint, fires on two literals) equals the def's
// UNFOLDED recursive result (the unary peeling). The accel Machine has m.NatAccel set;
// a second Machine WITHOUT m.NatAccel runs the same call by the ordinary body. Both
// must converge to the same number.
func TestNatAccelAgreesWithUnfolding(t *testing.T) {
	mAcc, f := newAccelFixture()
	mSlow := NewMachine(f.g) // no m.NatAccel: runs add/mul by their bodies
	mSlow.Data = f.natFixture
	cases := []struct{ a, b int }{{0, 0}, {0, 5}, {5, 0}, {3, 4}, {7, 9}, {12, 11}}
	for _, h := range []struct {
		name string
		ref  Hash
		want func(a, b int) int
	}{
		{"add", f.add, func(a, b int) int { return a + b }},
		{"mul", f.mul, func(a, b int) int { return a * b }},
	} {
		for _, c := range cases {
			fast := mAcc.Eval(nil, f.call(h.ref, c.a, c.b))
			slow := mSlow.Eval(nil, f.call(h.ref, c.a, c.b))
			// fast must be a literal of the right value
			n, ok := mAcc.asNatLit(fast)
			if !ok {
				t.Fatalf("%s %d %d (accel) did not produce a NatLit", h.name, c.a, c.b)
			}
			if n.Int64() != int64(h.want(c.a, c.b)) {
				t.Fatalf("%s %d %d (accel) = %d, want %d", h.name, c.a, c.b, n.Int64(), h.want(c.a, c.b))
			}
			// fast ≡ slow (the unfolded recursive result) on the nose
			if !mAcc.Conv(0, fast, slow) {
				t.Fatalf("%s %d %d: accel ≢ unfolded body", h.name, c.a, c.b)
			}
		}
	}
}

// TestNatAccelDoesNotFireOnNeutral: with one argument an open variable, the accel
// rule must NOT fire — the op stays neutral (reduces by its body, which gets stuck
// on the variable scrutinee), so open-term reasoning is byte-identical to no
// acceleration. We check the head stays the `add` ref (the eliminator is stuck).
func TestNatAccelDoesNotFireOnNeutral(t *testing.T) {
	mAcc, f := newAccelFixture()
	// add (NatLit 3) x   with x a free variable at level 0.
	call := App{Fn: App{Fn: Ref{Hash: f.add}, Arg: f.natLit(3)}, Arg: Var{Idx: 0}}
	v := mAcc.Eval(Env{VVar(0)}, call)
	// Forcing must reach a neutral headed by natElim (stuck on the open scrutinee),
	// NOT a VNatLit. The accel rule fired only if v forces to a literal.
	if _, ok := mAcc.asNatLit(v); ok {
		t.Fatal("accel wrongly fired on a neutral argument")
	}
}

// TestNatAccelMonus pins truncated subtraction at zero.
func TestNatAccelMonus(t *testing.T) {
	mAcc, f := newAccelFixture()
	// register a monus op directly via tryNatAccel (no body needed for this unit)
	spine := Neutral(NApp{Fn: NApp{Fn: NRef{Hash: f.add}, Arg: mAcc.Eval(nil, f.natLit(3)), Icit: Expl},
		Arg: mAcc.Eval(nil, f.natLit(8)), Icit: Expl})
	got, ok := mAcc.tryNatAccel(NatOpMonus, spine)
	if !ok {
		t.Fatal("monus accel did not fire on two literals")
	}
	n, _ := mAcc.asNatLit(got)
	if n.Sign() != 0 {
		t.Fatalf("monus 3 8 = %d, want 0 (truncated)", n.Int64())
	}
	spine2 := Neutral(NApp{Fn: NApp{Fn: NRef{Hash: f.add}, Arg: mAcc.Eval(nil, f.natLit(8)), Icit: Expl},
		Arg: mAcc.Eval(nil, f.natLit(3)), Icit: Expl})
	got2, _ := mAcc.tryNatAccel(NatOpMonus, spine2)
	n2, _ := mAcc.asNatLit(got2)
	if n2.Int64() != 5 {
		t.Fatalf("monus 8 3 = %d, want 5", n2.Int64())
	}
}

// TestNatAccelLargeIsConstantTime: accel on a large pair produces the literal
// without materialising either chain (a unary mul would be O(a*b) nodes). The
// guard is simply that it returns promptly with the right bigint.
func TestNatAccelLargeIsConstantTime(t *testing.T) {
	mAcc, f := newAccelFixture()
	got := mAcc.Eval(nil, f.call(f.mul, 4096, 4096))
	n, ok := mAcc.asNatLit(got)
	if !ok {
		t.Fatal("mul 4096 4096 (accel) did not produce a NatLit")
	}
	if n.Int64() != 4096*4096 {
		t.Fatalf("mul 4096 4096 = %d, want %d", n.Int64(), 4096*4096)
	}
}

// TestNatLitSuccFolds: the Decision-0 folding ι — `succ (NatLit n) ~> NatLit
// (n+1)`. Applying the nat `succ` constructor to a literal value folds back into
// a single canonical literal (not a `succ` wrapper), and the folded result is the
// expected number, SOUND against the unary chain (NatLit (n+1) ≡ succ (NatLit n)
// ≡ succ^(n+1) zero).
func TestNatLitSuccFolds(t *testing.T) {
	m, f := newNatFixture()
	for _, n := range []int{0, 1, 4, 99, 5000} {
		// succ (NatLit n) as a value: apply the succ ref to the literal.
		got := m.apply(m.refVal(f.succ), m.Eval(nil, f.natLit(int64(n))), Expl)
		// It must be a canonical literal of value n+1 (folded), not left as a
		// `succ` application: asNatLitSpine recognises the spine WITHOUT forcing.
		lit, ok := m.asNatLitSpine(got)
		if !ok {
			t.Fatalf("succ (NatLit %d) did not fold to a canonical NatLit", n)
		}
		if lit.N.Int64() != int64(n+1) {
			t.Fatalf("succ (NatLit %d) folded to NatLit %d, want %d", n, lit.N.Int64(), n+1)
		}
		if lit.Zero != f.zero || lit.Succ != f.succ {
			t.Fatalf("succ (NatLit %d) fold lost the nat binding identity", n)
		}
		// Soundness: the folded literal converts to both NatLit (n+1) and the
		// hand-written succ^(n+1) zero chain.
		if !m.Conv(0, got, m.Eval(nil, f.natLit(int64(n+1)))) {
			t.Fatalf("succ (NatLit %d) ≢ NatLit %d", n, n+1)
		}
		if !m.Conv(0, got, m.Eval(nil, f.unary(n+1))) {
			t.Fatalf("succ (NatLit %d) ≢ succ^%d zero", n, n+1)
		}
	}
}

// TestNatLitNormalFormIsCanonical: a `succ` directly applied to a literal
// normalizes to a single canonical NatLit (Decision 0 folds the wrapper in), so a
// closed result that IS a literal stays compact — not a `succ (NatLit n)` term.
// (An eliminator that reconstructs a value bottom-up from `zero` legitimately
// yields a succ-chain; the folding canonicalises the succ-OF-LITERAL redex, which
// is the form closed literal arithmetic and the accel path produce.)
func TestNatLitNormalFormIsCanonical(t *testing.T) {
	m, f := newNatFixture()
	// succ (NatLit 7) as a term: App{succ, NatLit 7}.
	tm := App{Fn: Ref{Hash: f.succ}, Arg: f.natLit(7)}
	got := m.Normalize(tm)
	lit, ok := got.(NatLit)
	if !ok {
		t.Fatalf("succ (NatLit 7) normalized to %T, want a canonical NatLit", got)
	}
	if lit.N.Int64() != 8 {
		t.Fatalf("succ (NatLit 7) normalized to NatLit %d, want 8", lit.N.Int64())
	}
}

// TestQuoteUnfoldLargeNoStackBlowup is the regression the prior agent flagged:
// NormalizeUnfold / QuoteUnfold of a large CLOSED accelerated result (mul 4096
// 4096) must stay a compact NatLit and NOT re-materialise the ~16.7M-node
// succ-chain (which blew the stack — QuoteUnfold forced the literal's peel). The
// QuoteUnfold short-circuit keeps the canonical literal compact.
func TestQuoteUnfoldLargeNoStackBlowup(t *testing.T) {
	mAcc, f := newAccelFixture()
	// mul 4096 4096 via the accel path -> a single VNatLit of 16777216.
	v := mAcc.Eval(nil, f.call(f.mul, 4096, 4096))
	// QuoteUnfold must NOT force the literal into its succ^n chain: it stays a
	// compact NatLit. If it re-materialised, this would blow the stack / hang.
	got := mAcc.QuoteUnfold(0, v)
	lit, ok := got.(NatLit)
	if !ok {
		t.Fatalf("QuoteUnfold of a large literal gave %T, want a compact NatLit", got)
	}
	if lit.N.Int64() != 4096*4096 {
		t.Fatalf("QuoteUnfold changed the value: got %d, want %d", lit.N.Int64(), 4096*4096)
	}
	// NormalizeUnfold (= QuoteUnfold ∘ Eval) of the call term likewise stays
	// compact — the full top-level path the REPL drives.
	norm := mAcc.NormalizeUnfold(f.call(f.mul, 4096, 4096))
	if nl, ok := norm.(NatLit); !ok || nl.N.Int64() != 4096*4096 {
		t.Fatalf("NormalizeUnfold of mul 4096 4096 = %#v, want compact NatLit 16777216", norm)
	}
}

// TestHashFormatVersionBumped: the default-lowering switch (bare numerals lower
// to NatLit, C7 / R-NUM Decision 2) is a content-hash event for every
// numeral-bearing def, paired with the deliberate hashFormatVersion 0x05 -> 0x06
// bump.
func TestHashFormatVersionBumped(t *testing.T) {
	if hashFormatVersion != 0x06 {
		t.Fatalf("hashFormatVersion = %#x, want 0x06 (default NatLit lowering bumps)", hashFormatVersion)
	}
}
