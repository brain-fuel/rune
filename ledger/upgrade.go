package ledger

import "goforge.dev/rune/v3/core"

// Upgrade is a strengthening of a claim's witness across two ledgers: the same
// proposition hash moving to a stronger tier (e.g. postulate -> proven).
type Upgrade struct {
	Name     string
	PropHash core.Hash
	From     Tier
	To       Tier
}

// Upgrades reports, per proposition hash present in both ledgers, a move toward a
// stronger tier (smaller Tier value). Downgrades are reported by gate.go, not here.
func Upgrades(baseline, current []Entry) []Upgrade {
	base := map[core.Hash]Entry{}
	for _, e := range baseline {
		base[e.PropHash] = e
	}
	var ups []Upgrade
	for _, e := range current {
		b, ok := base[e.PropHash]
		if !ok {
			continue
		}
		if e.Tier < b.Tier { // smaller = stronger
			ups = append(ups, Upgrade{Name: e.Name, PropHash: e.PropHash, From: b.Tier, To: e.Tier})
		}
	}
	return ups
}
