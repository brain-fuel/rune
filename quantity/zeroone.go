package quantity

import "goforge.dev/rune/core"

// ZeroOneOmega is the default usage semiring: Atkey's 0/1/ω lattice. Zero
// means erased, One linear, Omega unrestricted. Addition saturates: two
// nonzero usages make ω. Multiplication is annihilated by 0 and saturates to
// ω unless an operand is 1.
type ZeroOneOmega struct{}

// Default returns the 0/1/ω semiring, Rune v1's sole quantity-stratum
// implementation.
func Default() Semiring { return ZeroOneOmega{} }

func (ZeroOneOmega) Zero() core.Qty { return core.QZero }
func (ZeroOneOmega) One() core.Qty  { return core.QOne }

func (ZeroOneOmega) Add(a, b core.Qty) core.Qty {
	if a == core.QZero {
		return b
	}
	if b == core.QZero {
		return a
	}
	return core.QMany
}

func (ZeroOneOmega) Mul(a, b core.Qty) core.Qty {
	if a == core.QZero || b == core.QZero {
		return core.QZero
	}
	if a == core.QOne {
		return b
	}
	if b == core.QOne {
		return a
	}
	return core.QMany
}

func (ZeroOneOmega) Compatible(declared, used core.Qty) bool {
	switch declared {
	case core.QMany:
		return true
	case core.QZero:
		return used == core.QZero
	case core.QOne:
		return used == core.QOne
	default:
		return false
	}
}
