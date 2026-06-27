// control/control.go
// Package control is the Wavelet control catalog registry: the single source of
// truth tying each blessed control's rune definition name to its control class
// and the CALM model element it attaches to. The Assurance Ledger gate (Plan 2)
// consumes Flagships()/AllowedPostulates() to enforce the assurance policy; the
// CALM emit (Plan 4) consumes Catalog() to attach each control to its node or
// relationship. The def names here MUST match the controls in
// listings/ch538_control_catalog.rune.
package control

// Control names one blessed control: its rune definition, its class, and the CALM
// model element (a node or a relationship id) it is an assurance about.
type Control struct {
	Name    string // the rune definition name in the catalog listing
	Kind    string // the control class ("in-region", "tail", ...)
	Element string // the CALM node/relationship id this control attaches to
}

// Catalog is the blessed control set proven (or postulated) on the demo model.
func Catalog() []Control {
	return []Control{
		{Name: "inRegionProof", Kind: "in-region", Element: "store"},
		{Name: "encryptedProof", Kind: "encrypted-in-transit", Element: "web->relay"},
		{Name: "leastPrivProof", Kind: "least-privilege-iam", Element: "relay"},
		{Name: "convergesProof", Kind: "convergence", Element: "store"},
		{Name: "liveInRegion", Kind: "tail", Element: "store"},
	}
}

// Flagships are the controls whose ledger tier must stay `proven` or CI fails.
func Flagships() []string {
	out := []string{}
	for _, c := range Catalog() {
		if c.Kind != "tail" {
			out = append(out, c.Name)
		}
	}
	return out
}

// AllowedPostulates are the tail controls signed off as labeled debts (allowed to
// remain `postulate` for now; the ledger still surfaces them honestly).
func AllowedPostulates() []string {
	out := []string{}
	for _, c := range Catalog() {
		if c.Kind == "tail" {
			out = append(out, c.Name)
		}
	}
	return out
}
