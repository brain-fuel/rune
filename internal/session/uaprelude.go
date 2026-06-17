package session

import (
	_ "embed"
	"sync"

	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/surface"
)

// preludeDefs is the once-elaborated derived-univalence library, injected into
// every session by injectPrelude.
var (
	preludeOnce sync.Once
	preludeDefs []ambientDef
)

// uaPreludeSrc is the derived-univalence library, ambient in every session. It
// supplies `ua` as a DERIVED definition (a Glue line via `uaGlue`) — the kernel
// no longer postulates a `ua` head or bakes in a `castU`-over-`ua` rule. Loaded
// through the ordinary elaboration pipeline, content-addressed exactly like a
// user definition, but not part of any user's definition order.
//
//go:embed uaprelude.rune
var uaPreludeSrc string

// ambientDef is a pre-elaborated prelude definition, cached so the cubical proofs
// (notably isoToEquiv's proper-face comp) are elaborated once per process, not
// once per session.
type ambientDef struct {
	name string
	ty   core.Tm
	body core.Tm
	deps []core.Hash
}

// compilePrelude elaborates the derived-univalence prelude exactly once, against
// a session that already carries the builtin groups, and snapshots the checked
// (type, body, dependency) of each definition. The hashes are deterministic
// functions of the builtin types, so the snapshot injects identically into every
// later session.
func compilePrelude() []ambientDef {
	s := &Session{}
	s.resetBuiltins()
	items, err := surface.ParseProgram(uaPreludeSrc)
	if err != nil {
		panic("ua prelude parse: " + err.Error())
	}
	var defs []ambientDef
	for _, it := range items {
		d, ok := it.(surface.Def)
		if !ok {
			panic("ua prelude: only plain definitions are allowed")
		}
		el := s.elaborator()
		ty, body, err := el.ElabDef(d)
		if err != nil {
			panic("ua prelude elaborate " + d.Name + ": " + err.Error())
		}
		deps := el.M.DepList()
		h := s.st.Add(d.Name, ty, body)
		s.st.Certify(h, deps)
		s.refs[d.Name] = h
		s.refNames[h] = d.Name
		s.byHash[h] = Def{Name: d.Name, Ty: ty, Body: body, Hash: h}
		defs = append(defs, ambientDef{name: d.Name, ty: ty, body: body, deps: deps})
	}
	return defs
}

// injectPrelude registers the cached derived-univalence definitions into a fresh
// session as ambient (order-free) bindings.
func (s *Session) injectPrelude() {
	preludeOnce.Do(func() { preludeDefs = compilePrelude() })
	for _, ad := range preludeDefs {
		h := s.st.Add(ad.name, ad.ty, ad.body)
		s.st.Certify(h, ad.deps)
		s.refs[ad.name] = h
		s.refNames[h] = ad.name
		s.byHash[h] = Def{Name: ad.name, Ty: ad.ty, Body: ad.body, Hash: h}
	}
}
