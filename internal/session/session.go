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

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/elaborate"
	"goforge.dev/rune/v3/equality"
	"goforge.dev/rune/v3/store"
	"goforge.dev/rune/v3/surface"
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
	order    []core.Hash
	byHash   map[core.Hash]Def
	// nat is the `builtin nat` binding of the loaded source, if any: it lets
	// REPL expressions use numerals and the printer fold succ-chains back.
	// natCtors records whether the binding is the data-constructor form,
	// the only shape the BigInt codegen shadow applies to.
	nat      *surface.BuiltinNat
	natCtors bool
}

// New returns an empty session.
func New() *Session {
	s := &Session{}
	s.Reset()
	return s
}

// Reset clears every definition from the session, then re-registers the
// quotient builtin group (v2): Quot, qin, qsound, qlift, qind are part of the
// ambient environment of every session, exactly as content-addressed as user
// definitions, but not part of the user's definition order.
func (s *Session) Reset() {
	s.st = store.New()
	s.refs = map[string]core.Hash{}
	s.refNames = map[core.Hash]string{}
	s.order = nil
	s.byHash = map[core.Hash]Def{}
	s.nat = nil
	s.natCtors = false
	hs := s.st.AddQuot()
	for i, n := range store.QuotNames() {
		s.refs[n] = hs[i]
		s.refNames[hs[i]] = n
	}
	fhs := s.st.AddFib()
	for i, n := range store.FibNames() {
		s.refs[n] = fhs[i]
		s.refNames[fhs[i]] = n
	}
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
	if _, exists := s.byHash[h]; !exists {
		s.order = append(s.order, h)
	}
	s.refs[d.Name] = h
	s.refNames[h] = d.Name
	s.byHash[h] = rd
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
		case surface.BuiltinNat:
			if err := s.AddBuiltinNat(d); err != nil {
				return added, err
			}
		}
	}
	return added, nil
}

// AddBuiltinNat validates and registers a `builtin nat` declaration. The
// binding accepts any bound TERMS z : T and s : T -> T, not only the
// constructors of a data declaration (the numeric-tower amendment, rung C4:
// numerals can mean an integer or a rational). When the binding IS the
// constructor form, the session remembers that — the BigInt codegen shadow
// applies only there. The declaration is session state only — nothing enters
// the store.
func (s *Session) AddBuiltinNat(b surface.BuiltinNat) error {
	if _, ok := s.refs[b.TyName]; !ok {
		return fmt.Errorf("builtin nat: unknown type %q", b.TyName)
	}
	for n, ty := range map[string]surface.Exp{
		b.Zero: surface.EVar{Name: b.TyName},
		b.Succ: surface.EPi{Param: "_", Dom: surface.EVar{Name: b.TyName}, Cod: surface.EVar{Name: b.TyName}},
	} {
		if _, ok := s.refs[n]; !ok {
			return fmt.Errorf("builtin nat: unknown name %q", n)
		}
		el := s.elaborator()
		if _, _, err := el.ElabDef(surface.Def{Name: "$builtin-nat", Ty: ty, Body: surface.EVar{Name: n}}); err != nil {
			return fmt.Errorf("builtin nat: %q does not fit the binding: %v", n, err)
		}
	}
	s.nat = &b
	s.natCtors = s.bindingIsCtors(b)
	return nil
}

// bindingIsCtors reports whether a builtin-nat binding names exactly the
// zero/succ constructors of a data-declared type — the shape the BigInt
// codegen shadow compiles to machine integers.
func (s *Session) bindingIsCtors(b surface.BuiltinNat) bool {
	tyH := s.refs[b.TyName]
	if _, _, _, isData := s.st.DataDeclOf(tyH); !isData {
		return false
	}
	for _, n := range []string{b.Zero, b.Succ} {
		dh, _, isCtor := s.st.CtorOf(s.refs[n])
		if !isCtor || dh != tyH {
			return false
		}
	}
	return true
}

// ParseSrcExpr parses a single expression against the session: when a `builtin
// nat` binding is registered, numerals in the expression expand against it.
func (s *Session) ParseSrcExpr(src string) (surface.Exp, error) {
	if s.nat != nil {
		return surface.ParseExprNat(src, s.nat.Zero, s.nat.Succ)
	}
	return surface.ParseExpr(src)
}

// Pretty renders a core term with the session's reference names; under a
// `builtin nat` binding, saturated succ-chains fold back to numerals.
func (s *Session) Pretty(t core.Tm) string {
	if s.nat != nil {
		return surface.PrettyNat(t, s.refNames, s.refs[s.nat.Zero], s.refs[s.nat.Succ])
	}
	return surface.PrettyWith(t, s.refNames)
}

// Defs returns the session definitions in definition order.
func (s *Session) Defs() []Def {
	out := make([]Def, 0, len(s.order))
	for _, h := range s.order {
		out = append(out, s.byHash[h])
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
	el.M.Quot = s.st
	el.M.Fib = s.st
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
	m.Quot = s.st
	m.Fib = s.st
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
	if _, exists := s.byHash[h]; !exists {
		s.order = append(s.order, h)
	}
	s.refs[name] = h
	s.refNames[h] = name
	s.byHash[h] = rd
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
	// A registered `builtin nat` group compiles to machine integers (rung 6)
	// — only in its data-constructor form. A generalized binding (numerals
	// meaning an integer, a rational, …) computes through its successor term.
	if s.nat != nil && s.natCtors {
		p.Nat = &codegen.NatSpec{Zero: s.nat.Zero, Succ: s.nat.Succ, ElimName: s.nat.TyName + "Elim"}
	}
	// Datatype formers denote types: at runtime they erase to the unit token.
	// So does the quotient former Quot (a quotient compiles to its carrier).
	typeRefs := map[core.Hash]bool{}
	for _, h := range s.order {
		if _, _, _, ok := s.st.DataDeclOf(h); ok {
			typeRefs[h] = true
		}
	}
	if hs, ok := s.st.QuotHashes(); ok {
		typeRefs[hs[0]] = true
	}
	// The fibrant layer's TYPE formers erase like any type: UF, El, fib, piF,
	// pathF, pathU. Its VALUE members must not deploy at all (below): v3's
	// criterion for the inner layer is "elaborates and checks", not "runs" —
	// computational inner univalence is the §F frontier.
	if fhs, ok := s.st.FibHashes(); ok {
		for _, i := range []int{0, 1, 2, 3, 4, 7} {
			typeRefs[fhs[i]] = true
		}
	}
	// A display name can be bound more than once (later definitions shadow
	// earlier ones); the emitted globals must stay distinct per HASH, with
	// the current binding keeping the plain name and shadowed ancestors
	// suffixed. References erase through this same map, so a body compiled
	// against the old binding keeps calling the old code.
	emitNames := map[core.Hash]string{}
	for _, h := range s.order {
		n := s.refNames[h]
		if s.refs[n] != h {
			n = n + "$" + h.Short()
		}
		emitNames[h] = n
	}
	// Builtin groups (quotients, the fibrant layer) live in refNames but not
	// in the definition order; bodies may reference them by hash.
	eraseNames := map[core.Hash]string{}
	for h, n := range s.refNames {
		eraseNames[h] = n
	}
	for h, n := range emitNames {
		eraseNames[h] = n
	}
	tainted := map[string]string{}
	for _, h := range s.order {
		name := emitNames[h]
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
		ir := codegen.Erase(body, eraseNames, typeRefs)
		// Inner-layer taint: a definition whose erased body references a
		// fibrant VALUE member (or another tainted definition) checks but
		// does not deploy in v3 — transport along a ua-path has no erased
		// meaning yet, and emitting it would silently compute the wrong
		// function. Tainted definitions are skipped; only a tainted MAIN is
		// an error (see ref_docs/rune-v3-design.md §F).
		if bad := innerTaint(ir, tainted); bad != "" {
			tainted[name] = bad
			if name == main {
				return p, fmt.Errorf(
					"definition %q uses the inner layer (%s): inner constructions check but do not deploy in v3 (see ref_docs/rune-v3-design.md §F)",
					name, bad)
			}
			continue
		}
		p.Defs = append(p.Defs, codegen.DefSpec{Name: name, Body: ir})
	}
	return p, nil
}

// innerTaint reports the inner-layer name (a fibrant value member, or a
// previously-tainted definition) an erased body references, or "".
func innerTaint(t codegen.Ir, tainted map[string]string) string {
	inner := map[string]bool{"preflF": true, "pathJ": true, "ureflU": true, "ua": true, "castU": true}
	var walk func(codegen.Ir) string
	walk = func(t codegen.Ir) string {
		switch x := t.(type) {
		case codegen.IGlobal:
			if inner[x.Name] {
				return x.Name
			}
			if via, ok := tainted[x.Name]; ok {
				return x.Name + " (via " + via + ")"
			}
		case codegen.ILam:
			return walk(x.Body)
		case codegen.IApp:
			if n := walk(x.Fn); n != "" {
				return n
			}
			return walk(x.Arg)
		case codegen.ILet:
			if n := walk(x.Val); n != "" {
				return n
			}
			return walk(x.Body)
		}
		return ""
	}
	return walk(t)
}
