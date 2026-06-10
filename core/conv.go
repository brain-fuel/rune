package core

// Phase 1: definitional conversion over the glued Val domain.
//
// The glued fast path: two neutrals are compared SPINE-FIRST — syntactically, no
// unfolding, no dependency logged. Only on mismatch does conversion force the lazy
// unfoldings (δ), and forcing logs the unfolded definition into the Machine's
// dependency set. So the log records exactly the bodies the judgment consulted —
// the Frame Lemma's U — and the fast path stays free.

// Conv reports whether a and b are definitionally equal (βδη) at level lvl.
func (m *Machine) Conv(lvl int, a, b Val) bool {
	// η for functions: if either side is a lambda, compare both applied to a
	// fresh variable (Apply extends a neutral's spine, so neutral-vs-lambda
	// compares pointwise).
	if _, ok := a.(VLam); ok {
		return m.convApplied(lvl, a, b)
	}
	if _, ok := b.(VLam); ok {
		return m.convApplied(lvl, a, b)
	}

	switch x := a.(type) {
	case VU:
		if y, ok := b.(VU); ok {
			return x.Lvl == y.Lvl
		}
	case VProp:
		if _, ok := b.(VProp); ok {
			return true
		}
	case VEq:
		if y, ok := b.(VEq); ok {
			return m.Conv(lvl, x.Ty, y.Ty) && m.Conv(lvl, x.L, y.L) && m.Conv(lvl, x.R, y.R)
		}
	case VRefl:
		// Proof irrelevance at the canonical level: any two refl proofs are
		// definitionally equal regardless of payload.
		if _, ok := b.(VRefl); ok {
			return true
		}
	case VPi:
		if y, ok := b.(VPi); ok {
			if x.Icit != y.Icit || x.Qty != y.Qty {
				return false
			}
			v := VVar(lvl)
			return m.Conv(lvl, x.Dom, y.Dom) && m.Conv(lvl+1, x.Cod(v), y.Cod(v))
		}
	case VNeu:
		if y, ok := b.(VNeu); ok && m.convSpine(lvl, x.Spine, y.Spine) {
			return true // fast path: spines agree syntactically, nothing forced
		}
	}

	// Mismatch: δ-unfold whichever sides can, and retry. If neither can, the
	// values are genuinely different.
	af, aok := m.force1(a)
	bf, bok := m.force1(b)
	if !aok && !bok {
		return false
	}
	return m.Conv(lvl, af, bf)
}

func (m *Machine) convApplied(lvl int, a, b Val) bool {
	v := VVar(lvl)
	return m.Conv(lvl+1, m.Apply(a, v), m.Apply(b, v))
}

// convSpine compares two neutral spines structurally. Heads must agree (same
// variable level, same definition hash); arguments are compared with full Conv,
// which may itself force inside an argument.
func (m *Machine) convSpine(lvl int, p, q Neutral) bool {
	switch x := p.(type) {
	case NVar:
		y, ok := q.(NVar)
		return ok && x.Lvl == y.Lvl
	case NRef:
		y, ok := q.(NRef)
		return ok && x.Hash == y.Hash
	case NMeta:
		// Core conversion treats an unsolved meta as rigid: equal only to
		// itself. Solving-by-unification is elaborate/'s job, not conversion's.
		y, ok := q.(NMeta)
		return ok && x.ID == y.ID
	case NCast:
		// Casts compare on endpoints and subject; the PROOF is skipped —
		// definitional proof irrelevance at the cast site.
		y, ok := q.(NCast)
		return ok && m.Conv(lvl, x.A, y.A) && m.Conv(lvl, x.B, y.B) && m.Conv(lvl, x.X, y.X)
	case NSubst:
		// Likewise: everything but the proof.
		y, ok := q.(NSubst)
		return ok && m.Conv(lvl, x.A, y.A) && m.Conv(lvl, x.X, y.X) &&
			m.Conv(lvl, x.Y, y.Y) && m.Conv(lvl, x.P, y.P) && m.Conv(lvl, x.Px, y.Px)
	case NApp:
		y, ok := q.(NApp)
		return ok && x.Icit == y.Icit && m.convSpine(lvl, x.Fn, y.Fn) && m.Conv(lvl, x.Arg, y.Arg)
	default:
		panic("core.Conv: unknown Neutral constructor")
	}
}

// Sub reports a <: b — definitional conversion extended with cumulativity:
// Prop <: U, and function types are covariant in their codomain (domains stay
// invariant, which is sound and keeps the relation decidable without
// antisymmetry games). Used where a Prop-valued thing is supplied at a
// U-valued expectation (e.g. Prop-valued eliminator motives).
func (m *Machine) Sub(lvl int, a, b Val) bool {
	if m.Conv(lvl, a, b) {
		return true
	}
	af, bf := m.Force(a), m.Force(b)
	if _, ok := af.(VProp); ok {
		if _, ok2 := bf.(VU); ok2 {
			return true
		}
	}
	if ua, ok := af.(VU); ok {
		if ub, ok2 := bf.(VU); ok2 && ua.Lvl <= ub.Lvl {
			return true // cumulativity
		}
	}
	pa, ok1 := af.(VPi)
	pb, ok2 := bf.(VPi)
	if ok1 && ok2 && pa.Icit == pb.Icit && pa.Qty == pb.Qty && m.Conv(lvl, pa.Dom, pb.Dom) {
		v := VVar(lvl)
		return m.Sub(lvl+1, pa.Cod(v), pb.Cod(v))
	}
	return false
}
