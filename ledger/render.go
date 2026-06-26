package ledger

import (
	"fmt"
	"io"

	"goforge.dev/rune/v3/core"
)

// RenderText writes an aligned one-line-per-entry table. The tier column is
// fixed-width so the ladder reads at a glance; proof shows "-" for bodiless.
func RenderText(es []Entry, w io.Writer) {
	for _, e := range es {
		proof := "-"
		if e.ProofHash != (core.Hash{}) {
			proof = e.ProofHash.Short()
		}
		why := ""
		if e.Why != "" {
			why = "  why: " + e.Why
		}
		fmt.Fprintf(w, "%-9s %-24s prop:%s  proof:%s%s\n",
			e.Tier.String(), e.Name, e.PropHash.Short(), proof, why)
	}
}
