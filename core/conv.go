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
		if _, ok := b.(VU); ok {
			return true
		}
	case VPi:
		if y, ok := b.(VPi); ok {
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
	case NApp:
		y, ok := q.(NApp)
		return ok && m.convSpine(lvl, x.Fn, y.Fn) && m.Conv(lvl, x.Arg, y.Arg)
	default:
		panic("core.Conv: unknown Neutral constructor")
	}
}
