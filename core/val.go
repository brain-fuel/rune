package core

// Val is the glued NbE value domain. SHAPE ONLY in Phase 0: the constructors are
// fixed here so the Phase-1 evaluator and quoter have a target, but eval and quote
// are NOT implemented.
//
// "Glued" (after smalltt): a neutral value carries both its un-unfolded spine AND a
// lazy unfolding. Conversion takes a fast syntactic path over spines and forces the
// lazy unfolding only on mismatch, so errors print near the source the user wrote.
//
// The coincidence that earns its keep (see ref_docs/rune-proof-cache-semantics.md):
// forcing a neutral's lazy unfolding is the SAME operation as store.Unfold. The
// proof-cache dependency log hooks exactly there — the fast path compares spines and
// logs nothing; forcing logs the unfolded definition's hash. The instrumentation
// rides on the machinery built for speed. This is on purpose; keep them coincident.
type Val interface {
	isVal()
}

// VNeu is a neutral value: a variable or definition reference stuck under
// eliminators it cannot yet reduce.
//
// Spine is the un-unfolded neutral (the fast-path representative). Unfold is the
// lazy unfolding: forcing it is store.Unfold and is where the proof-cache dependency
// is logged. Phase 1 supplies real thunks; Phase 0 fixes the shape only.
type VNeu struct {
	Spine  Neutral
	Unfold func() Val
}

// VU is the universe as a value.
type VU struct{}

// VPi is a dependent function type value (x : Dom) -> Cod. Cod is a Go closure: the
// Phase-1 NbE meaning function. Unused at Phase 0.
type VPi struct {
	Dom Val
	Cod func(Val) Val
}

// VLam is a lambda value, carrying its meaning as a Go closure.
type VLam struct {
	Body func(Val) Val
}

func (VNeu) isVal() {}
func (VU) isVal()   {}
func (VPi) isVal()  {}
func (VLam) isVal() {}

// Neutral is the un-unfolded spine of a stuck computation: a head (a free variable
// at a de Bruijn level, or a definition reference) under a stack of eliminators.
type Neutral interface {
	isNeutral()
}

// NVar is a free variable, identified by de Bruijn LEVEL (not index) so the head is
// stable as the value is reused under deeper binders.
type NVar struct {
	Lvl int
}

// NRef is a stuck reference to a top-level definition (one whose body has not been
// unfolded on the fast path).
type NRef struct {
	Hash Hash
}

// NApp is a neutral applied to an argument value.
type NApp struct {
	Fn  Neutral
	Arg Val
}

func (NVar) isNeutral() {}
func (NRef) isNeutral() {}
func (NApp) isNeutral() {}
