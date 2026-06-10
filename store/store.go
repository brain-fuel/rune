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

import "goforge.dev/rune/core"

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
	}
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
