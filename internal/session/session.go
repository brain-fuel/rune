// Package session is the shared parse -> resolve -> check -> hash pipeline that
// both the file commands (rune fmt / rune hash) and the REPL drive, so the
// pipeline lives in one place and is never duplicated. It owns a content-addressed
// store, the name environment that resolution looks references up in, and (Phase 1)
// the typed pipeline: every definition is type checked on entry, and its
// well-typedness certificate — keyed by content hash and the exact set of bodies
// the check unfolded — lands in the store's proof cache.
package session

import (
	"fmt"

	"goforge.dev/rune/codegen"
	"goforge.dev/rune/core"
	"goforge.dev/rune/elaborate"
	"goforge.dev/rune/equality"
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

// AddDef resolves, TYPE CHECKS, and adds a definition to the session. A reference
// to a not-yet-defined name is rejected — recursion arrives with Phase-4 totality.
// Redefining a name updates it in place (its position in the listing is kept).
//
// The check is cached: the definition's content hash is computed first (resolution
// is cheap and untyped), and if the store already holds a certificate for that
// hash whose dependency hashes all resolve, the check is skipped — a proof-cache
// hit. Otherwise the definition is elaborated and checked, and a certificate is
// recorded with the exact unfolded-dependency set the machine logged.
func (s *Session) AddDef(d surface.Def) (Def, error) {
	// Fast path: untyped resolution. On plain programs (no holes, no implicit
	// insertion) it emits the same core elaboration would, so its content hash
	// can hit the certificate table and skip the check entirely. A certificate
	// speaks about CORE CONTENT, so a hit is sound no matter which surface
	// produced the content. If resolution fails (a hole) or the hash misses,
	// the typed elaborator is the authority.
	var ty, body core.Tm
	if d.Ty != nil {
		r := s.resolver()
		rty, errTy := r.ResolveExp(d.Ty)
		rbody, errBody := r.ResolveExp(d.Body)
		if errTy == nil && errBody == nil {
			h := store.HashDef(rty, rbody)
			if s.st.Certified(h) {
				ty, body = rty, rbody
			}
		}
	}
	if body == nil {
		el := s.elaborator()
		ety, ebody, err := el.ElabDef(d)
		if err != nil {
			return Def{}, err
		}
		ty, body = ety, ebody
		s.st.Certify(store.HashDef(ty, body), el.M.DepList())
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

// LoadSource parses a file of top-level items (definitions and datatype
// declarations) and adds each in order, returning the names added. On the
// first error the names added so far are returned alongside it.
func (s *Session) LoadSource(src string) ([]string, error) {
	items, err := surface.ParseProgram(src)
	if err != nil {
		return nil, err
	}
	var added []string
	for _, it := range items {
		switch d := it.(type) {
		case surface.Def:
			rd, err := s.AddDef(d)
			if err != nil {
				return added, err
			}
			added = append(added, rd.Name)
		case surface.DataDef:
			names, err := s.AddData(d)
			if err != nil {
				return added, err
			}
			added = append(added, names...)
		}
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

// elaborator returns a fresh per-run Elaborator over the session store.
func (s *Session) elaborator() *elaborate.Elaborator {
	el := elaborate.New(s.st, s.refs, s.refNames)
	el.M.Data = s.st
	return el
}

// ElabExpr elaborates a surface expression against the session environment,
// returning the zonked core term and its inferred type (quoted back to core).
// Unsolved metavariables (underconstrained holes/implicits) are an error.
func (s *Session) ElabExpr(e surface.Exp) (tm, ty core.Tm, err error) {
	el := s.elaborator()
	t, vty, err := el.Infer(&elaborate.Ctx{}, e)
	if err != nil {
		return nil, nil, err
	}
	t = el.Zonk(0, t)
	tyTm := el.Zonk(0, el.M.Quote(0, vty))
	if err := el.ErrUnsolved("expression"); err != nil {
		return nil, nil, err
	}
	return t, tyTm, nil
}

// NormalizeExpr fully normalizes (βδι) a closed, well-typed core term.
func (s *Session) NormalizeExpr(t core.Tm) core.Tm {
	m := core.NewMachine(s.st)
	m.EqS = equality.Default()
	m.Data = s.st
	return m.NormalizeUnfold(t)
}

// AddData elaborates and stores a datatype declaration, binding the former,
// the constructors, and the eliminator (Name + "Elim").
func (s *Session) AddData(d surface.DataDef) ([]string, error) {
	el := s.elaborator()
	decl, err := el.ElabData(d)
	if err != nil {
		return nil, err
	}
	dh, ctors, eh := s.st.AddData(decl)
	names := []string{decl.Name}
	s.bindName(decl.Name, dh)
	for i, ch := range ctors {
		s.bindName(decl.CtorNames[i], ch)
		names = append(names, decl.CtorNames[i])
	}
	elimName := decl.Name + "Elim"
	s.bindName(elimName, eh)
	names = append(names, elimName)
	return names, nil
}

// bindName binds a session name to a stored hash (definition order preserved).
func (s *Session) bindName(name string, h core.Hash) {
	ty, _ := s.st.TypeOf(h)
	rd := Def{Name: name, Ty: ty, Hash: h}
	if _, exists := s.byName[name]; !exists {
		s.order = append(s.order, name)
	}
	s.refs[name] = h
	s.refNames[h] = name
	s.byName[name] = rd
}

// Certified reports whether the definition currently bound to name has a valid
// proof-cache certificate in the session store.
func (s *Session) Certified(name string) bool {
	h, ok := s.refs[name]
	return ok && s.st.Certified(h)
}

// Lookup returns the hash bound to a session name.
func (s *Session) Lookup(name string) (core.Hash, bool) {
	h, ok := s.refs[name]
	return h, ok
}

// EmitProgram lowers the whole session to the erased IR program, in
// definition order. main, when non-empty, must name a session definition; the
// emitted program prints its value.
func (s *Session) EmitProgram(main string) (codegen.Program, error) {
	var p codegen.Program
	if main != "" {
		if _, ok := s.refs[main]; !ok {
			return p, fmt.Errorf("no definition named %q", main)
		}
		p.Main = main
	}
	// Datatype formers denote types: at runtime they erase to the unit token.
	typeRefs := map[core.Hash]bool{}
	for _, name := range s.order {
		if _, _, _, ok := s.st.DataDeclOf(s.refs[name]); ok {
			typeRefs[s.refs[name]] = true
		}
	}
	for _, name := range s.order {
		h := s.refs[name]
		// Datatype groups are emitted once, at the former; constructor and
		// eliminator name entries are covered by it.
		if decl, ctors, _, ok := s.st.DataDeclOf(h); ok {
			ds := codegen.DataSpec{ElimName: decl.Name + "Elim", NumParams: decl.NumParams}
			for i, cn := range decl.CtorNames {
				_ = ctors
				ds.Ctors = append(ds.Ctors, codegen.CtorSpec{
					Name:  cn,
					Tag:   i,
					Arity: decl.NumParams + decl.Sigs[i].Arity,
				})
				ds.Rec = append(ds.Rec, decl.Sigs[i].Rec)
			}
			p.Datas = append(p.Datas, ds)
			continue
		}
		if _, _, isCtor := s.st.CtorOf(h); isCtor {
			continue
		}
		if _, isElim := s.st.ElimOf(h); isElim {
			continue
		}
		body, ok := s.st.Unfold(h)
		if !ok {
			return p, fmt.Errorf("definition %q has no body to emit", name)
		}
		p.Defs = append(p.Defs, codegen.DefSpec{Name: name, Body: codegen.Erase(body, s.refNames, typeRefs)})
	}
	return p, nil
}
