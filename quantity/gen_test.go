package quantity

import (
	"pgregory.net/rapid"

	"goforge.dev/rune/v3/core"
)

// GenQty draws a VALID quantity — the semiring's carrier is the three-point
// 0/1/ω lattice, not an arbitrary byte. Test-only: rapid stays out of the
// stratum's runtime imports.
var GenQty = rapid.SampledFrom([]core.Qty{core.QZero, core.QOne, core.QMany})
