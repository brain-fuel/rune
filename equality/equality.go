// Package equality is the EQUALITY STRATUM: the notion of equality, kept behind a
// bounded interface so the roadmap can extend it (v2: quotients) and swap it (v3:
// two-level type theory) without a rewrite.
//
// The interface the stratum implements is core.EqStratum — the eval hooks the
// glued-NbE machine calls when it reaches an equality former. Phase 3 binds the
// signatures and ships the one v1 implementation, Observational (Pujet–Tabareau):
// a proof-irrelevant Prop, an observational Eq that computes on type structure
// (funext is a reduction), and cast, which computes on the endpoint types and
// never inspects its proof. The store, hashing, semiring, codegen, and
// surface/nameless split are all deliberately ORTHOGONAL to this package.
package equality

import "goforge.dev/rune/v3/core"

// Stratum is the equality stratum: core.EqStratum plus identification.
type Stratum interface {
	core.EqStratum
	// Name identifies the stratum implementation (e.g. "observational").
	Name() string
	// Formers returns the core type-former names this stratum gives meaning to.
	Formers() []string
}
