package equality

import "goforge.dev/rune/v3/core"

// Observational is the v1 equality stratum (Pujet–Tabareau, *Observational
// Equality: Now For Good*): proof-irrelevant Prop, an Eq that computes on the
// structure of its type, and cast. UIP holds; univalence is refuted by design
// (NON-GOALS.md). What v1 computes:
//
//   - Eq (Pi (x:A) B) f g  ~>  (x : A) -> Eq B (f x) (g x)     [funext]
//   - cast A B p x  ~>  x                    when A ≡ B definitionally
//   - cast (Pi a1 b1) (Pi a2 b2) p f  ~>  fn (y : a2) is
//     cast (b1 (cast a2 a1 _ y)) (b2 y) _ (f (cast a2 a1 _ y)) end
//   - Eq U/Prop endpoints and proof-typed values are irrelevant: conversion
//     skips cast proofs and equates refl proofs (core/conv.go).
//
// Deeper type-equality DECOMPOSITION (Eq U (Pi …) (Pi …) unfolding to a
// telescope of equalities) needs Sigma types and is parked until a listing
// needs it; a stuck Eq U is still provable by refl at convertible endpoints.
type Observational struct{}

// Default is the stratum instance v1 wires into every Machine.
func Default() Stratum { return Observational{} }

func (Observational) Name() string { return "observational" }

func (Observational) Formers() []string { return []string{"Prop", "Eq", "refl", "cast"} }

// EvalEq computes Eq ty l r. The load-bearing rule is funext-as-reduction: an
// equality of functions IS the pointwise equality function type.
func (Observational) EvalEq(m *core.Machine, ty, l, r core.Val) core.Val {
	if pi, ok := m.Force(ty).(core.VPi); ok {
		return core.VPi{Name: pi.Name, Icit: pi.Icit, Dom: pi.Dom, Cod: func(x core.Val) core.Val {
			return m.EvalEq(pi.Cod(x), m.ApplyIcit(l, x, pi.Icit), m.ApplyIcit(r, x, pi.Icit))
		}}
	}
	return core.VEq{Ty: ty, L: l, R: r}
}

// EvalCast computes cast a b p x, never inspecting p (proof irrelevance):
// the reduction is driven entirely by the endpoint types.
func (o Observational) EvalCast(m *core.Machine, a, b, p, x core.Val) core.Val {
	fa, fb := m.Force(a), m.Force(b)

	// Convertible endpoints: the cast is the identity.
	if m.Conv(0, fa, fb) {
		return x
	}

	// Function types: cast contravariantly on the domain, covariantly on the
	// codomain. The proofs threaded into the recursive casts are irrelevant and
	// are simply reused.
	if pa, ok := fa.(core.VPi); ok {
		if pb, ok2 := fb.(core.VPi); ok2 && pa.Icit == pb.Icit {
			return core.VLam{Name: pb.Name, Icit: pb.Icit, Body: func(y core.Val) core.Val {
				yA := o.EvalCast(m, pb.Dom, pa.Dom, p, y)
				return o.EvalCast(m, pa.Cod(yA), pb.Cod(y), p, m.ApplyIcit(x, yA, pa.Icit))
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

// EvalSubst computes Leibniz transport: when the equality's endpoints are
// definitionally equal the transport is the identity on its subject; otherwise
// it is stuck. The proof is never inspected.
func (Observational) EvalSubst(m *core.Machine, a, x, y, prf, pmot, px core.Val) core.Val {
	if m.Conv(0, x, y) {
		return px
	}
	return core.VNeu{Spine: core.NSubst{A: a, X: x, Y: y, Prf: prf, P: pmot, Px: px}}
}
