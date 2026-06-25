// Package store holds the content-addressed definition store and is the home of the
// body-abstraction barrier.
//
// THE BODY BARRIER (a soundness invariant, encoded in package structure — CLAUDE.md):
// a definition's TYPE is public, but its BODY is sealed inside this package. The Def
// struct's body field is unexported, so no code outside this package can read a body
// except through the single gateway Unfold. This makes "no code peeks a body around
// the barrier" a compile-time fact rather than a future vigilance task.
//
// Unfold is doc-commented as the sole gateway and the proof-cache instrumentation
// point: in Phase 1 it will log the dependency that the cache's Frame Lemma frames
// off (see ref_docs/rune-proof-cache-semantics.md). Forcing a glued value's lazy
// unfolding is the SAME operation; they coincide on purpose.
package store

import "goforge.dev/rune/v3/core"

// Def is a stored definition. Type is public; body is sealed. The pair (Type, body)
// is the definition's content, and its content hash summarizes both.
type Def struct {
	Type core.Tm // public: part of the content hash any referencing term already carries
	body core.Tm // SEALED: reachable only via Unfold
}

// NewDef constructs a definition from a public type (optionally nil in Phase 0,
// since there is no type checker yet) and a body. Construction is the only way to
// place a body into a Def; thereafter the barrier holds.
func NewDef(ty, body core.Tm) Def {
	return Def{Type: ty, body: body}
}

// Store is the content-addressed definition store plus the mutable name layer.
//
// The definition map is append-only and immutable in spirit: a content hash names a
// fixed (Type, body) forever. The name map is the only mutable, non-trusted part —
// it records which immutable object is "current" for a human-facing name, and
// editing source can only produce cache misses, never wrong hits.
type Store struct {
	defs  map[core.Hash]Def
	names map[string]core.Hash
	// The proof cache (cert.go): append-only certificates keyed by
	// hash(defHash, ‖U‖), plus an index by definition hash for lookup.
	certs      map[core.Hash]Cert
	certsByDef map[core.Hash][]core.Hash
	// Datatype roles (data.go): declaration groups and per-hash roles the
	// evaluator's ι-reduction consults through core.DataInfo.
	dataByHash map[core.Hash]dataEntry
	ctorRole   map[core.Hash]ctorRole
	elimRole   map[core.Hash]core.Hash
	// The quotient builtin group (quot.go), once registered.
	quot *quotEntry
	// The fibrant builtin group (fib.go), once registered.
	fib *fibEntry
	// The fibrant-universe-hierarchy group (ufh.go, R-UFH), once registered.
	uh *ufhEntry
	// The interval builtin group (interval.go, §F phase 1), once registered.
	iv *intervalEntry
	// The cubical path builtin group (path.go, §F phase 2), once registered.
	pa *pathEntry
	// The face-lattice builtin group (face.go, §F phase 3a), once registered.
	fc *faceEntry
	// The cofibration-validity group (sys.go, §F phase 3b), once registered.
	sy *sysEntry
	// The Kan-operation group (kan.go, §F phase 3c+), once registered.
	kn *kanEntry
	// The inner Sigma group (sigma.go, §F / R-SIGMA / A5), once registered.
	si *sigmaEntry
	// The coinductive group (coind.go, R-COIND / C5a), once registered.
	cn *coindEntry
	// The IO/effects group (io.go, R-EFFECT / C3), once registered.
	io *ioEntry
	// The equivalence group (equiv.go, §F / R-GLUE / A6), once registered.
	ev *equivEntry
	// The inner Glue group (glue.go, §F / R-GLUE / A6), once registered.
	gl *glueEntry
	// The face-split group (fsplit.go, §F / R-BOX / A3), once registered.
	fs *fsplitEntry
	// The UF-valued face-split group (sysu.go, §F / R-BOX / A8), once registered.
	syu *sysUEntry
	// The dependent face-split group (fsplitd.go, §F / R-BOX / A8), once registered.
	sd *splitDEntry
	// The ∀-cofibration group (forall.go, §F / R-GLUE / A6), once registered.
	fa *forallEntry
	// The type-level path-abstraction group (pathu.go, §F / R-UA / A7).
	pu *pabsUEntry
	// The type-level path-application group (pappu.go, §F / R-UA / A7).
	ppu *pappUEntry
	// The guarded-recursion modality group (guard.go, R-COIND / C5b).
	gd *guardEntry
	// The inner-HIT kit — the circle (hit.go, §F / R-HIT / A9), once registered.
	hi *hitEntry
	// The suspension HIT (susp.go, §F / R-HIT / A9), once registered.
	su *suspEntry
	// The fibrant quotient HIT (quotient.go, §F / R-HIT / A9), once registered.
	qh *quotHitEntry
	// The dependent-path group (pathp.go, §F / R-HIT / A9), once registered.
	pp *pathPEntry
	// The set-truncation HIT (trunc.go, §F / R-HIT / A9 dim-2), once registered.
	tr *truncEntry
	// The circle's dependent eliminator (circind.go, §F / R-HIT / A9).
	ci *circIndEntry
	// The suspension's dependent eliminator (suspind.go, §F / R-HIT / A9).
	sui *suspIndEntry
	// The fibrant quotient's dependent eliminator (quotind.go, §F / R-HIT / A9).
	qui *quotIndEntry
	// partial marks general-recursive `partial` definitions (C4): their heads are
	// permanently neutral in eval/conversion (the firewall), bodies reachable
	// only through codegen's direct Unfold.
	partial map[core.Hash]bool
	// partialGroup maps each member of a MUTUALLY-recursive `partial` group to the
	// full set of the group's member hashes (incl itself), so codegen's mutual-tail
	// trampoline (T4) knows which siblings a tail call may bounce to. A self-only
	// `partial` (AddPartial) is absent — its group is implicitly itself.
	partialGroup map[core.Hash][]core.Hash
	// assumed marks `foreign` axioms (R-FFI / B4): bodiless typed constants whose
	// meaning is supplied by the host. They are trusted, not proven — `rune
	// assumptions` reports them.
	assumed map[core.Hash]bool
}

// New returns an empty store.
func New() *Store {
	return &Store{
		defs:       map[core.Hash]Def{},
		names:      map[string]core.Hash{},
		certs:      map[core.Hash]Cert{},
		certsByDef: map[core.Hash][]core.Hash{},
		dataByHash: map[core.Hash]dataEntry{},
		ctorRole:   map[core.Hash]ctorRole{},
		elimRole:   map[core.Hash]core.Hash{},
		partial:      map[core.Hash]bool{},
		partialGroup: map[core.Hash][]core.Hash{},
		assumed:      map[core.Hash]bool{},
	}
}

// AddForeign stores a `foreign` axiom (R-FFI / B4): a bodiless definition whose
// public type is its contract, bound to name and recorded as an assumption. It
// has no body, so it is a permanently-neutral head in eval/conversion; codegen
// emits a reference to a host-provided global of the same name.
func (s *Store) AddForeign(name string, ty core.Tm) core.Hash {
	// A foreign axiom is NOMINAL (its meaning is the host function `name`), so
	// its identity is name + type — like a builtin group, not content-hashed
	// over a (nonexistent) body.
	g := newHasher()
	g.Write([]byte{defFormatVersion, 'F'})
	g.Write([]byte(name))
	th := core.HashTerm(ty)
	g.Write(th[:])
	var h core.Hash
	copy(h[:], g.Sum(nil))
	s.defs[h] = Def{Type: ty}
	s.names[name] = h
	s.assumed[h] = true
	return h
}

// IsAssumed reports whether a hash is a trusted `foreign` axiom.
func (s *Store) IsAssumed(h core.Hash) bool { return s.assumed[h] }

// Assumptions returns the hashes of all registered `foreign` axioms.
func (s *Store) Assumptions() []core.Hash {
	out := make([]core.Hash, 0, len(s.assumed))
	for h := range s.assumed {
		out = append(out, h)
	}
	return out
}

// AddPartial stores a general-recursive `partial` definition (C4). The body is
// elaborated with the self-reference standing at Placeholder(0); this substitutes
// that placeholder with the definition's own content hash (computed over the
// placeholder body, exactly as datatype groups self-reference) and marks the
// hash partial, so the evaluator keeps its head permanently neutral.
func (s *Store) AddPartial(name string, ty, body core.Tm) core.Hash {
	h := HashDef(ty, body)
	realBody := replaceRef(body, Placeholder(0), h)
	s.defs[h] = NewDef(ty, realBody)
	s.names[name] = h
	s.partial[h] = true
	return h
}

// AddPartialGroup stores a MUTUALLY-recursive `partial` group (mutual general
// recursion). Each member's body references its siblings (and itself) at
// Placeholder(i); this SCC-hashes the group (store/scc.go HashSCC), substitutes
// every placeholder with its derived content hash in every member's type and body,
// and marks each member partial (heads stay neutral). A singleton group delegates
// to AddPartial so a single `partial` def hashes byte-identically to today.
func (s *Store) AddPartialGroup(names []string, tys, bodies []core.Tm) []core.Hash {
	if len(names) == 1 {
		return []core.Hash{s.AddPartial(names[0], tys[0], bodies[0])}
	}
	group := make([]content, len(names))
	for i := range names {
		group[i] = content{Type: tys[i], Body: bodies[i]}
	}
	hs := HashSCC(group)
	for i := range names {
		ty, body := tys[i], bodies[i]
		for j, hj := range hs {
			ty = replaceRef(ty, Placeholder(j), hj)
			body = replaceRef(body, Placeholder(j), hj)
		}
		s.defs[hs[i]] = NewDef(ty, body)
		s.names[names[i]] = hs[i]
		s.partial[hs[i]] = true
	}
	for i := range hs {
		s.partialGroup[hs[i]] = hs // the full SCC group (T4 mutual-tail)
	}
	return hs
}

// IsPartial implements core.PartialInfo.
func (s *Store) IsPartial(h core.Hash) bool { return s.partial[h] }

// PartialGroupOf returns the hashes of a partial's MUTUAL-recursion group (incl
// itself) when it was declared in a `mutual` block; ok is false for a self-only
// `partial` (its group is implicitly just itself).
func (s *Store) PartialGroupOf(h core.Hash) ([]core.Hash, bool) {
	g, ok := s.partialGroup[h]
	return g, ok
}

// HashDef computes the content hash a definition (ty, body) would be stored
// under, without storing it. Identity is syntax (core.HashTerm); never modulo
// conversion.
func HashDef(ty, body core.Tm) core.Hash {
	return hashContent(content{Type: ty, Body: body})
}

// Add stores a single definition under its content hash, binds name to that hash,
// and returns the hash. Add is the acyclic path: it hashes (Type, body) directly as
// a singleton with no intra-group references (see HashSCC for the cyclic case).
func (s *Store) Add(name string, ty, body core.Tm) core.Hash {
	d := NewDef(ty, body)
	h := hashContent(content{Type: ty, Body: body})
	s.defs[h] = d
	s.names[name] = h
	return h
}

// Lookup returns the content hash currently bound to a name.
func (s *Store) Lookup(name string) (core.Hash, bool) {
	h, ok := s.names[name]
	return h, ok
}

// TypeOf returns a definition's public type. Reading a type logs NO dependency: a
// referenced definition's type is part of its content hash, already in-band. This is
// the pure, no-dependency counterpart of Unfold (see the Frame Lemma).
func (s *Store) TypeOf(h core.Hash) (core.Tm, bool) {
	d, ok := s.defs[h]
	if !ok {
		return nil, false
	}
	return d.Type, true
}

// Unfold is THE SOLE GATEWAY to a definition's body, and the proof-cache
// instrumentation point.
//
// Obtaining a body necessarily goes through here. In Phase 1, Unfold runs inside the
// conversion monad and logs the unfolded definition's hash into the write-only
// dependency set that keys the proof cache; the same call backs the lazy unfolding
// of a glued neutral (force == unfold). Phase 0 returns the body without logging,
// because there is no conversion yet — but the choke point exists now so that the
// instrumentation is a one-line change behind an already-enforced barrier, not a
// retrofit.
func (s *Store) Unfold(h core.Hash) (core.Tm, bool) {
	d, ok := s.defs[h]
	if !ok || d.body == nil {
		// Unknown, or a bodiless definition (a datatype former, constructor,
		// or eliminator): there is nothing to unfold — permanently neutral.
		return nil, false
	}
	return d.body, true
}
