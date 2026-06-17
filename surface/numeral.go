package surface

import (
	"fmt"
	"math/big"

	"goforge.dev/rune/v3/core"
)

// NumConfig holds the resolved constructor references a numeral literal expands
// against. A numeral is NOT expanded at parse time (a parser cannot know which
// type it means); it survives as ENum and is lowered here, by two consumers:
//
//   - the untyped Resolver, which has no expected type and so lowers to the
//     unary `builtin nat` binding — the established default;
//   - the typed elaborator, which lowers by the EXPECTED TYPE: a numeral checked
//     against (or defaulting to) the nat type becomes a compressed core numeral
//     (NatLit).
//
// As of C7 / R-NUM the default `builtin nat` lowering emits a single compressed
// core numeral `NatLit` (definitionally `succ^n zero`), NOT a materialised
// succ-chain. This removes the old 4096 unary cap entirely and lets bare
// numerals of any size elaborate and compute. NatLit inter-converts with the
// unary form (eval peels one succ layer on demand), so every induction proof
// transfers verbatim. The switch is a deliberate content-hash event paired with
// the hashFormatVersion 0x05 -> 0x06 bump (core/hash.go).
//
// The old binary-literal path (`builtin bin`, lowering a numeral checked at a
// binary `Pos`/`BN` type to an O(log n) bit-spine) is RETIRED (C7 / R-NUM,
// Decision 5; CLAUDE.md Rule 5): NatLit subsumes it — no cap, O(1) literals,
// and native/accelerated arithmetic the bit-spine never had. The hand-written
// `Pos`/`BN` datatypes and their agreement proofs remain ordinary user data.
//
// Hashes are resolved once, when the binding is registered.
type NumConfig struct {
	HasNat           bool
	NatZero, NatSucc core.Hash
}

// Nat lowers n to a compressed core numeral NatLit (definitionally succ^n zero).
// There is NO cap: a literal is one node of any size (C7 / R-NUM, Decision 2).
func (c NumConfig) Nat(n int) (core.Tm, error) {
	if !c.HasNat {
		return nil, fmt.Errorf("numeral %d has no meaning: no `builtin nat` declared", n)
	}
	if n < 0 {
		return nil, fmt.Errorf("numeral %d is negative; Nat has no negatives", n)
	}
	return core.NatLit{N: big.NewInt(int64(n)), Zero: c.NatZero, Succ: c.NatSucc}, nil
}
