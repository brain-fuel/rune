// Package quantity is the QUANTITY STRATUM: the usage semiring that a quantitative
// type theory threads through binders, kept behind an interface so the lattice is a
// swappable module.
//
// What it abstracts: how resource usage is counted and combined. Pi/Lam binders will
// (Phase 5, "turn on QTT") carry a Qty; the 0-fragment is the erasure boundary that
// the codegen stratum reads. v1 ships ONE implementation, the 0/1/ω semiring below;
// the interface exists so a later version can swap in a different resource discipline
// without touching the store, hashing, codegen, or the surface/nameless split.
//
// Phase 0 defines the interface and the default instance only. Threading Qty through
// the core binders has no consumer yet and is therefore deferred (see PARKING-LOT.md
// and the Phase 5 gate in CLAUDE.md): scope is capped by demonstrated need.
package quantity

// Qty is one element of the usage semiring (a usage count).
type Qty interface {
	isQty()
}

// Semiring abstracts a usage semiring: an additive monoid (Zero, Add) for combining
// usages across multiple occurrences, and a multiplicative monoid (One, Mul) for
// composing usages through nested binders. v2+ may supply other instances.
type Semiring interface {
	// Zero is the additive identity — "used zero times" — and marks the erasure
	// boundary: a Zero-quantity binder is computationally irrelevant.
	Zero() Qty
	// One is the multiplicative identity — "used exactly once".
	One() Qty
	// Add combines the usages of distinct occurrences.
	Add(a, b Qty) Qty
	// Mul composes usage through nesting (e.g. a variable used n times inside a
	// function applied m times is used m·n times).
	Mul(a, b Qty) Qty
}
