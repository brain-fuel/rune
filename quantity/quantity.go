// Package quantity is the QUANTITY STRATUM: the usage semiring that the
// quantitative type theory threads through binders (Phase 5), kept behind an
// interface so the lattice is a swappable module.
//
// The annotation DOMAIN lives in core (core.Qty: 0, 1, ω) because annotations
// are part of a term's identity and are hashed; this package supplies the
// RULES — how usages add across occurrences and multiply through nesting —
// and the compatibility judgment the elaborator's usage checker applies at
// each binder. The 0-fragment is the erasure boundary the codegen stratum
// reads.
package quantity

import "goforge.dev/rune/core"

// Semiring abstracts the usage arithmetic: an additive monoid (Zero, Add) for
// combining usages of distinct occurrences, and a multiplicative monoid (One,
// Mul) for composing usage through nested positions.
type Semiring interface {
	Zero() core.Qty
	One() core.Qty
	Add(a, b core.Qty) core.Qty
	Mul(a, b core.Qty) core.Qty
	// Compatible reports whether an observed usage satisfies a declared
	// annotation (0 requires unused; 1 requires exactly once; ω accepts any).
	Compatible(declared, used core.Qty) bool
}
