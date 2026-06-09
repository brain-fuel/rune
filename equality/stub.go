package equality

import "goforge.dev/rune/core"

// Observational is the Phase-3 observational-equality stratum (Pujet–Tabareau): the
// single implementation v1 ships. In Phase 0 it is a documented stub — its methods
// panic("phase 1") so that any accidental use during the skeleton is loud rather than
// silently wrong. It exists to prove the interface is inhabitable.
type Observational struct{}

func (Observational) Name() string      { return "observational" }
func (Observational) Formers() []string { return nil }

func (Observational) EvalFormer(core.Tm) core.Val  { panic("phase 1: equality stratum eval") }
func (Observational) QuoteFormer(core.Val) core.Tm { panic("phase 1: equality stratum quote") }
func (Observational) ConvertFormers(a, b core.Val) bool {
	panic("phase 1: equality stratum conversion")
}
