package quantity

// The default usage semiring: the 0/1/ω lattice (Atkey's QTT resource semiring).
// Zero means erased, One means linear, Omega (ω) means unrestricted. This is the one
// instance v1 ships behind the Semiring interface.

type zeroOneOmega int

const (
	zero zeroOneOmega = iota
	one
	omega
)

func (zeroOneOmega) isQty() {}

// Zero, One, and Omega are the three usages of the default semiring.
var (
	Zero  Qty = zero
	One   Qty = one
	Omega Qty = omega
)

// ZeroOneOmega is the default usage semiring instance. Add is the lattice join with
// the rule that two distinct nonzero usages saturate to ω; Mul is annihilated by 0
// and saturates to ω when both operands are nonzero and not both 1.
type ZeroOneOmega struct{}

// Default returns the 0/1/ω semiring, Rune v1's sole quantity-stratum implementation.
func Default() Semiring { return ZeroOneOmega{} }

func (ZeroOneOmega) Zero() Qty { return Zero }
func (ZeroOneOmega) One() Qty  { return One }

func (ZeroOneOmega) Add(a, b Qty) Qty {
	x, y := a.(zeroOneOmega), b.(zeroOneOmega)
	if x == zero {
		return y
	}
	if y == zero {
		return x
	}
	return omega
}

func (ZeroOneOmega) Mul(a, b Qty) Qty {
	x, y := a.(zeroOneOmega), b.(zeroOneOmega)
	if x == zero || y == zero {
		return zero
	}
	if x == one {
		return y
	}
	if y == one {
		return x
	}
	return omega
}
