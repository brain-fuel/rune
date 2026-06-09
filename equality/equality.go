// Package equality is the EQUALITY STRATUM: the notion of equality, kept behind a
// bounded interface so the roadmap can extend it (v2: quotients) and swap it (v3:
// two-level type theory) without a rewrite.
//
// What it abstracts: the equality type formers and their future eval/quote/conversion
// hooks. This is the LOAD-BEARING interface of the roadmap — the store, hashing,
// semiring, codegen, and surface/nameless split are all deliberately ORTHOGONAL to
// it. v1 ships one implementation (observational equality: proof-irrelevant Prop, an
// observational Eq that computes on type structure, cast; funext and propext fall
// out; UIP holds). That implementation is Phase 3, not Phase 0.
//
// Phase 0 defines the interface (and a stub) only — there are NO equality type
// formers in the Phase-0 surface or core. The interface exists now so the Phase-1
// conversion checker plugs the stratum in rather than hardcoding a choice.
package equality

import "goforge.dev/rune/core"

// Stratum is the equality stratum interface. Its methods are the seams the Phase-1+
// conversion checker calls when it reaches an equality type former: how such formers
// evaluate, quote, and convert. They are intentionally left abstract in Phase 0;
// concrete signatures are fixed alongside the Phase-1 NbE machinery they hook into.
type Stratum interface {
	// Name identifies the stratum implementation (e.g. "observational").
	Name() string

	// Formers returns the core type-former tags this stratum introduces (e.g. Eq,
	// Refl, Cast for observational equality). Phase 0 ships none.
	Formers() []string

	// Eval, Quote, and Convert are the hooks the glued-NbE conversion checker invokes
	// for this stratum's formers. They are declared but not specified in Phase 0; the
	// signatures bind to core.Val / core.Tm once the Phase-1 evaluator exists.
	//
	// EvalFormer(t) -> the value of an equality former; QuoteFormer(v) -> its term;
	// ConvertFormers(a, b) -> whether two equality values are definitionally equal.
	EvalFormer(core.Tm) core.Val
	QuoteFormer(core.Val) core.Tm
	ConvertFormers(a, b core.Val) bool
}
