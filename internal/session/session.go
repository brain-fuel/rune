// Package session is the shared parse -> resolve -> hash pipeline that both the file
// commands (rune fmt / rune hash) and the REPL drive, so the pipeline lives in one
// place and is never duplicated. It owns a content-addressed store and the name
// environment that resolution looks references up in.
//
// It performs name resolution only — the single Phase-0 elaboration. There is no type
// checking or evaluation here; those are Phase 1.
package session

import (
	"fmt"

	"goforge.dev/rune/core"
	"goforge.dev/rune/store"
	"goforge.dev/rune/surface"
)

// Def is one definition after name resolution: its core type, body, and content hash.
type Def struct {
	Name string
	Ty   core.Tm
	Body core.Tm
	Hash core.Hash
}

// Session holds the store, the name->hash reference map that resolution consults, and
// the reverse hash->name map the pretty-printer uses to render references by name.
type Session struct {
	st       *store.Store
	refs     map[string]core.Hash
	refNames map[core.Hash]string
	order    []string
	byName   map[string]Def
}

// New returns an empty session.
func New() *Session {
	s := &Session{}
	s.Reset()
	return s
}

// Reset clears every definition from the session.
func (s *Session) Reset() {
	s.st = store.New()
	s.refs = map[string]core.Hash{}
	s.refNames = map[core.Hash]string{}
	s.order = nil
	s.byName = map[string]Def{}
}

func (s *Session) resolver() *surface.Resolver { return &surface.Resolver{Refs: s.refs} }

// ResolveExpr resolves a surface expression against the current session environment:
// free identifiers resolve to the content-hash references of session definitions.
func (s *Session) ResolveExpr(e surface.Exp) (core.Tm, error) {
	return s.resolver().ResolveExp(e)
}

// AddDef resolves a definition and adds it to the session. A reference to a
// not-yet-defined name is rejected — Phase 0 handles only the acyclic case. Redefining
// a name updates it in place (its position in the listing is kept).
func (s *Session) AddDef(d surface.Def) (Def, error) {
	r := s.resolver()
	var ty core.Tm
	if d.Ty != nil {
		t, err := r.ResolveExp(d.Ty)
		if err != nil {
			return Def{}, fmt.Errorf("%s: %w", d.Name, err)
		}
		ty = t
	}
	body, err := r.ResolveExp(d.Body)
	if err != nil {
		return Def{}, fmt.Errorf("%s: %w", d.Name, err)
	}
	h := s.st.Add(d.Name, ty, body)
	rd := Def{Name: d.Name, Ty: ty, Body: body, Hash: h}
	if _, exists := s.byName[d.Name]; !exists {
		s.order = append(s.order, d.Name)
	}
	s.refs[d.Name] = h
	s.refNames[h] = d.Name
	s.byName[d.Name] = rd
	return rd, nil
}

// LoadSource parses a file of definitions and adds each in order, returning the names
// added. On the first error the names added so far are returned alongside it.
func (s *Session) LoadSource(src string) ([]string, error) {
	defs, err := surface.ParseFile(src)
	if err != nil {
		return nil, err
	}
	var added []string
	for _, d := range defs {
		rd, err := s.AddDef(d)
		if err != nil {
			return added, err
		}
		added = append(added, rd.Name)
	}
	return added, nil
}

// Defs returns the session definitions in definition order.
func (s *Session) Defs() []Def {
	out := make([]Def, 0, len(s.order))
	for _, n := range s.order {
		out = append(out, s.byName[n])
	}
	return out
}

// RefNames returns the hash->name map for rendering references; callers must not
// mutate it.
func (s *Session) RefNames() map[core.Hash]string { return s.refNames }
