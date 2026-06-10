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

// VProp is the universe of propositions as a value (Phase 3).
type VProp struct{}

// VEq is a STUCK observational equality type: the stratum's EvalEq could not
// reduce it further (Ty is not a function type, and not enough structure is
// known). Values of a VEq type are proofs and are definitionally irrelevant.
type VEq struct {
	Ty, L, R Val
}

// VRefl is the reflexivity proof as a value. Its payload is carried for
// quotation only; conversion never inspects it (proof irrelevance).
type VRefl struct {
	V Val
}

// VPi is a dependent function type value (x : Dom) -> Cod. Cod is a Go closure:
// the NbE meaning function. Name is a pretty-printing hint for the binder, like
// Scope.Name — never part of identity, never hashed (quote drops it into a Scope).
type VPi struct {
	Name string
	Icit Icit
	Dom  Val
	Cod  func(Val) Val
}

// VLam is a lambda value, carrying its meaning as a Go closure. Name is a display
// hint only, like VPi.Name.
type VLam struct {
	Name string
	Icit Icit
	Body func(Val) Val
}

func (VNeu) isVal()  {}
func (VU) isVal()    {}
func (VProp) isVal() {}
func (VEq) isVal()   {}
func (VRefl) isVal() {}
func (VPi) isVal()   {}
func (VLam) isVal()  {}

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

// NApp is a neutral applied to an argument value at the given plicity.
type NApp struct {
	Fn   Neutral
	Arg  Val
	Icit Icit
}

// NMeta is a stuck metavariable head (Phase 2, elaboration-internal). A neutral
// headed by an NMeta is FLEXIBLE: solving the meta can wake it. The Machine's
// Metas solver is consulted when forcing.
type NMeta struct {
	ID int
}

// NCast is a stuck cast: transport along an equality whose endpoint types are
// not yet concrete enough to reduce. P is carried for quotation only —
// conversion compares A, B, and X and SKIPS P (proof irrelevance).
type NCast struct {
	A, B, P, X Val
}

func (NVar) isNeutral()  {}
func (NRef) isNeutral()  {}
func (NApp) isNeutral()  {}
func (NMeta) isNeutral() {}
func (NCast) isNeutral() {}
