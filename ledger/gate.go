package ledger

import "goforge.dev/rune/v3/elaborate"

// GateConfig is the CI policy: which controls must stay proven, and which
// postulates are signed off (allowed to remain a debt for now).
type GateConfig struct {
	Flagships         []string // must be Proven or the build fails
	AllowedPostulates []string // postulates that are signed off
}

func contains(xs []string, x string) bool {
	for _, s := range xs {
		if s == x {
			return true
		}
	}
	return false
}

// Gate returns one diagnostic per policy violation. Empty = pass. The baseline
// is accepted for future "no NEW postulate" deltas; the current policy gates on
// the present ledger (an unapproved postulate, a flagship off proven).
func Gate(current, baseline []Entry, cfg GateConfig) []error {
	var errs []error
	for _, e := range current {
		if e.Tier == Postulate && !contains(cfg.AllowedPostulates, e.Name) {
			errs = append(errs, &elaborate.Diagnostic{
				Summary: "Unapproved postulate in the assurance ledger.",
				Body: []string{
					"The control " + e.Name + " (proposition " + e.PropHash.Short() +
						") is a postulate: " + e.Why,
					"A postulate is an unproven debt. CI requires sign-off before a new one lands.",
				},
				Hints: []string{
					"Prove it (replace the postulate with a proof of the same proposition), " +
						"or add " + e.Name + " to the gate's AllowedPostulates with a reviewer sign-off.",
				},
			})
		}
		if contains(cfg.Flagships, e.Name) && e.Tier != Proven {
			errs = append(errs, &elaborate.Diagnostic{
				Summary: "Flagship control is not proven.",
				Body: []string{
					"The flagship control " + e.Name + " is at tier " + e.Tier.String() +
						", but flagship controls must stay proven.",
				},
				Hints: []string{
					"Restore the proof for " + e.Name + ", or remove it from the flagship set " +
						"if it is intentionally being demoted (a reviewed decision).",
				},
			})
		}
	}
	return errs
}
