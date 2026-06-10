// Package session is the shared parse -> resolve -> check -> hash pipeline that
// both the file commands (rune fmt / rune hash) and the REPL drive, so the
// pipeline lives in one place and is never duplicated. It owns a content-addressed
// store, the name environment that resolution looks references up in, and (Phase 1)
// the typed pipeline: every definition is type checked on entry, and its
// well-typedness certificate — keyed by content hash and the exact set of bodies
// the check unfolded — lands in the store's proof cache.
package session

import (
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
		el := elaborate.New(s.st, s.refs, s.refNames)
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

// elaborator returns a fresh per-run Elaborator over the session store.
func (s *Session) elaborator() *elaborate.Elaborator {
	return elaborate.New(s.st, s.refs, s.refNames)
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

// NormalizeExpr fully normalizes (βδ) a closed, well-typed core term.
func (s *Session) NormalizeExpr(t core.Tm) core.Tm {
	m := core.NewMachine(s.st)
	m.EqS = equality.Default()
	return m.NormalizeUnfold(t)
}

// Certified reports whether the definition currently bound to name has a valid
// proof-cache certificate in the session store.
func (s *Session) Certified(name string) bool {
	h, ok := s.refs[name]
	return ok && s.st.Certified(h)
}
