// Package equality is the EQUALITY STRATUM: the notion of equality, kept
// swappable so the roadmap can extend it (v2: quotients) and replace it (v3:
// two-level type theory) without a rewrite. As of v3.383.0 the stratum is a
// Go+ CLASS: the hooks are class operations, the implementation is an
// instance, and the stratum's defining reductions are LAWS with generated
// rapid tests — the first tests this package has ever had.
//
// The Machine's seam is unchanged: core.EqStratum stays the nil-able hook
// interface (nil means the formers are stuck, the pre-Phase-3 behavior), and
// Default() adapts the class witness into it via Bound.
package equality

import "goforge.dev/rune/v3/core"

// Stratum is the equality stratum as a class: the eval hooks the glued-NbE
// machine calls when it reaches an equality former, plus identification.
type Stratum[S any] class {
	// Name identifies the stratum implementation (e.g. "observational").
	Name(host S) string
	// Formers returns the core type-former names this stratum gives meaning to.
	Formers(host S) []string
	// EvalEq computes the value of Eq ty l r, applying the stratum's
	// type-directed reductions (e.g. funext: Eq over a Pi unfolds pointwise).
	EvalEq(host S, m *core.Machine, ty, l, r core.Val) core.Val
	// EvalCast computes cast a b p x, never inspecting p.
	EvalCast(host S, m *core.Machine, a, b, p, x core.Val) core.Val
	// EvalSubst computes subst a x y prf P px, never inspecting prf.
	EvalSubst(host S, m *core.Machine, a, x, y, prf, pmot, px core.Val) core.Val

	// A cast between definitionally equal endpoints is the identity — the
	// stratum may never get creative at a reflexive cast.
	law CastConvertibleIsIdentity(host S) {
		m := core.NewMachine(noGlobals{})
		x := core.VVar(0)
		got := EvalCast(host, m, core.VU(0), core.VU(0), core.VRefl(core.VU(0)), x)
		return m.Conv(1, got, x)
	}
	// Funext is a REDUCTION, not an axiom: Eq over a function type IS the
	// pointwise equality function type.
	law FunextReduces(host S) {
		m := core.NewMachine(noGlobals{})
		pi := core.VPi("x", core.Expl, core.QMany, true, core.VU(0),
			func(v core.Val) core.Val { return core.VU(0) })
		_, isPi := EvalEq(host, m, pi, core.VVar(0), core.VVar(1)).(core.VPi)
		return isPi
	}
	// The cast proof is IRRELEVANT: two casts along different proofs of the
	// same equality agree.
	law CastNeverInspectsProof(host S) {
		m := core.NewMachine(noGlobals{})
		x := core.VVar(0)
		p1 := core.VRefl(core.VU(0))
		p2 := core.VVar(1)
		a := EvalCast(host, m, core.VU(0), core.VU(0), p1, x)
		b := EvalCast(host, m, core.VU(0), core.VU(0), p2, x)
		return m.Conv(2, a, b)
	}
	// Leibniz transport computes to its subject at definitionally equal
	// endpoints (in particular at refl).
	law SubstComputesAtEqualEndpoints(host S) {
		m := core.NewMachine(noGlobals{})
		px := core.VVar(0)
		got := EvalSubst(host, m, core.VU(0), core.VU(1), core.VU(1),
			core.VRefl(core.VU(1)), core.VVar(1), px)
		return m.Conv(2, got, px)
	}
}

// noGlobals is the empty Globals the law machines run over: the laws exercise
// closed values only, so nothing is ever looked up or unfolded.
type noGlobals struct{}

func (noGlobals) TypeOf(h core.Hash) (core.Tm, bool) { return nil, false }
func (noGlobals) Unfold(h core.Hash) (core.Tm, bool) { return nil, false }

// PujetTabareau is the host of the v1 stratum (Pujet–Tabareau, *Observational
// Equality: Now For Good*): proof-irrelevant Prop, an Eq that computes on the
// structure of its type, and cast. UIP holds; univalence is refuted by design
// (NON-GOALS.md). What v1 computes:
//
//   - Eq (Pi (x:A) B) f g  ~>  (x : A) -> Eq B (f x) (g x)     [funext]
//   - cast A B p x  ~>  x                    when A ≡ B definitionally
//   - cast (Pi a1 b1) (Pi a2 b2) p f  ~>  fn (y : a2) is
//     cast (b1 (cast a2 a1 _ y)) (b2 y) _ (f (cast a2 a1 _ y)) end
//   - Eq U/Prop endpoints and proof-typed values are irrelevant: conversion
//     skips cast proofs and equates refl proofs (core/conv.gp).
//
// Deeper type-equality DECOMPOSITION (Eq U (Pi …) (Pi …) unfolding to a
// telescope of equalities) needs Sigma types and is parked until a listing
// needs it; a stuck Eq U is still provable by refl at convertible endpoints.
type PujetTabareau struct{}

// Observational is the v1 equality stratum witness.
instance Observational Stratum[PujetTabareau] {
	Name(host PujetTabareau) string { return "observational" }

	Formers(host PujetTabareau) []string { return []string{"Prop", "Eq", "refl", "cast"} }

	// EvalEq computes Eq ty l r. The load-bearing rule is funext-as-reduction:
	// an equality of functions IS the pointwise equality function type.
	EvalEq(host PujetTabareau, m *core.Machine, ty, l, r core.Val) core.Val {
		if pi, ok := m.Force(ty).(core.VPi); ok {
			return core.VPi{Name: pi.Name, Icit: pi.Icit, Dom: pi.Dom, Cod: func(x core.Val) core.Val {
				return m.EvalEq(pi.Cod(x), m.ApplyIcit(l, x, pi.Icit), m.ApplyIcit(r, x, pi.Icit))
			}}
		}
		return core.VEq{Ty: ty, L: l, R: r}
	}

	// EvalCast computes cast a b p x, never inspecting p (proof irrelevance):
	// the reduction is driven entirely by the endpoint types. The recursion
	// lives in obsCast (a witness value cannot name itself in its own
	// initializer — goplus rejects the cycle).
	EvalCast(host PujetTabareau, m *core.Machine, a, b, p, x core.Val) core.Val {
		return obsCast(m, a, b, p, x)
	}

	// EvalSubst computes Leibniz transport: when the equality's endpoints are
	// definitionally equal the transport is the identity on its subject;
	// otherwise it is stuck. The proof is never inspected.
	EvalSubst(host PujetTabareau, m *core.Machine, a, x, y, prf, pmot, px core.Val) core.Val {
		if m.Conv(0, x, y) {
			return px
		}
		return core.VNeu{Spine: core.NSubst{A: a, X: x, Y: y, Prf: prf, P: pmot, Px: px}}
	}
}

// obsCast is the observational cast reduction, recursive through the
// contravariant/covariant function-type case.
func obsCast(m *core.Machine, a, b, p, x core.Val) core.Val {
	fa, fb := m.Force(a), m.Force(b)

	// Convertible endpoints: the cast is the identity.
	if m.Conv(0, fa, fb) {
		return x
	}

	// Function types: cast contravariantly on the domain, covariantly on the
	// codomain. The proofs threaded into the recursive casts are irrelevant
	// and are simply reused.
	if pa, ok := fa.(core.VPi); ok {
		if pb, ok2 := fb.(core.VPi); ok2 && pa.Icit == pb.Icit {
			return core.VLam{Name: pb.Name, Icit: pb.Icit, Body: func(y core.Val) core.Val {
				yA := obsCast(m, pb.Dom, pa.Dom, p, y)
				return obsCast(m, pa.Cod(yA), pb.Cod(y), p, m.ApplyIcit(x, yA, pa.Icit))
			}}
		}
	}

	// Proof types: any inhabitant transports to any other proof type's
	// inhabitant — proofs are irrelevant, so the subject itself is fine.
	if _, ok := fa.(core.VEq); ok {
		if _, ok2 := fb.(core.VEq); ok2 {
			return x
		}
	}

	// Stuck: one endpoint is neutral or the shapes disagree (ill-typed casts
	// only reach here on open terms; the checker rejects them when closed).
	return core.VNeu{Spine: core.NCast{A: a, B: b, P: p, X: x}}
}

// Bound adapts a stratum witness and its host into core.EqStratum — the
// Machine's nil-able hook seam (nil = formers stuck) is unchanged by the
// class-ification.
type Bound[S any] struct {
	Host S
	W    Stratum[S]
}

func (b Bound[S]) EvalEq(m *core.Machine, ty, l, r core.Val) core.Val {
	return b.W.EvalEq(b.Host, m, ty, l, r)
}

func (b Bound[S]) EvalCast(m *core.Machine, a, bb, p, x core.Val) core.Val {
	return b.W.EvalCast(b.Host, m, a, bb, p, x)
}

func (b Bound[S]) EvalSubst(m *core.Machine, a, x, y, prf, pmot, px core.Val) core.Val {
	return b.W.EvalSubst(b.Host, m, a, x, y, prf, pmot, px)
}

// Default is the stratum v1 wires into every Machine: the Observational
// witness bound to its host.
func Default() core.EqStratum {
	return Bound[PujetTabareau]{Host: PujetTabareau{}, W: Observational}
}
