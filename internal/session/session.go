// Package session is the shared parse -> resolve -> check -> hash pipeline that
// both the file commands (rune fmt / rune hash) and the REPL drive, so the
// pipeline lives in one place and is never duplicated. It owns a content-addressed
// store, the name environment that resolution looks references up in, and (Phase 1)
// the typed pipeline: every definition is type checked on entry, and its
// well-typedness certificate — keyed by content hash and the exact set of bodies
// the check unfolded — lands in the store's proof cache.
package session

import (
	"errors"
	"fmt"
	"math/big"
	"strings"

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
	// Postulate reports the def was written as `postulate ... because "..." end`
	// (an asserted debt), not `foreign`. Session metadata only - never hashed.
	Postulate bool
	// Why is the postulate's stated reason. Session metadata only - never hashed.
	Why string
	// UsesGuard reports that the definition's body used the `with post ... guard
	// ... blame ...` contract sugar at parse time. Session metadata only - never
	// part of the def's content hash.
	UsesGuard bool
	// Pos is the byte offset of the definition's name token in the source text.
	// Used by the ledger to compute the 1-based source line for git blame.
	// Zero when the source position is unknown (builtin group members, etc.).
	Pos int
}

// defMeta is per-name metadata the ledger needs but the store does NOT hash: it
// records how a def was WRITTEN at the surface (postulate-ness + reason +
// guard-sugar usage + source position), kept session-side so the kernel's
// content addressing stays untouched.
type defMeta struct {
	postulate bool
	why       string
	usesGuard bool
	pos       int // byte offset of the name token in the source text
}

// Session holds the store, the name->hash reference map that resolution consults, and
// the reverse hash->name map the pretty-printer uses to render references by name.
type Session struct {
	st       *store.Store
	refs     map[string]core.Hash
	refNames map[core.Hash]string
	order    []core.Hash
	byHash   map[core.Hash]Def
	// instances is the typeclass instance table (C2), keyed by
	// (class-former hash, last-argument-head hash) → the instance def's hash.
	instances map[[2]core.Hash]core.Hash
	// nat is the `builtin nat` binding of the loaded source, if any: it lets
	// REPL expressions use numerals and the printer fold succ-chains back.
	// natCtors records whether the binding is the data-constructor form,
	// the only shape the BigInt codegen shadow applies to.
	nat      *surface.BuiltinNat
	natCtors bool
	// numInjs are the registered typed numeral injections (numeric-tower rung C4):
	// `builtin int Z intOf` / `builtin rat Rat ratOf`. A numeral checked at an
	// injection's codomain lowers to inj(NatLit). Consulted only by the typed
	// elaborator (via numConfig); the untyped default stays the base Nat.
	numInjs []surface.BuiltinNumInj
	// natAccel is the C7 / R-NUM Decision-1 acceleration table: a `builtin natAdd
	// add` (etc.) maps the named def's content hash to a NatOp, so a call on two
	// compressed literals takes the one-bigint-step fast path (core.tryNatAccel)
	// instead of O(a·b) eliminator peeling. It is consulted by every Machine the
	// session builds (set as m.NatAccel). Empty means no acceleration.
	natAccel map[core.Hash]core.NatOp
	// meta is per-name surface metadata the ledger reads but the store never
	// hashes (postulate-ness + reason). Keyed by def name; absent => zero value.
	meta map[string]defMeta
}

// natAccelTable is the session's core.NatAccelInfo: it reports the accelerated
// op a stored hash names (the registered table) and the `builtin nat`
// constructor hashes the accel rule builds its VNatLit result against (a literal
// is relative to a particular `builtin nat`). It is a value snapshot of the
// session's registrations, safe to share across the per-run Machines.
type natAccelTable struct {
	ops        map[core.Hash]core.NatOp
	zero, succ core.Hash
	hasCtors   bool
}

func (t natAccelTable) NatOpOf(h core.Hash) core.NatOp {
	if t.ops == nil {
		return core.NatOpNone
	}
	return t.ops[h]
}

func (t natAccelTable) NatCtors() (zero, succ core.Hash, ok bool) {
	return t.zero, t.succ, t.hasCtors
}

// natAccelInfo returns the session's acceleration table as a core.NatAccelInfo,
// or nil when nothing is registered (so m.NatAccel stays nil and tryRules short-
// circuits exactly as before). The nat constructor hashes come from the active
// `builtin nat` binding — the accel rule needs them to fold its bigint result
// back into the right literal.
func (s *Session) natAccelInfo() core.NatAccelInfo {
	if len(s.natAccel) == 0 {
		return nil
	}
	t := natAccelTable{ops: s.natAccel}
	if s.nat != nil {
		t.zero, t.succ = s.refs[s.nat.Zero], s.refs[s.nat.Succ]
		t.hasCtors = true
	}
	return t
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
	s.resetBuiltins()
	s.injectPrelude()
}

// resetBuiltins clears the session and re-registers every ambient builtin group
// (quotients, the fibrant stratum, the cubical Kan/face/interval machinery, Σ, IO,
// equivalences, Glue, the HIT kit, …). It does NOT load the derived-univalence
// prelude — `compilePrelude` calls it to build a builtins-only session in which to
// elaborate that prelude, so the two must stay separable.
func (s *Session) resetBuiltins() {
	s.st = store.New()
	s.instances = map[[2]core.Hash]core.Hash{}
	s.refs = map[string]core.Hash{}
	s.refNames = map[core.Hash]string{}
	s.order = nil
	s.byHash = map[core.Hash]Def{}
	s.nat = nil
	s.natCtors = false
	s.natAccel = map[core.Hash]core.NatOp{}
	s.meta = map[string]defMeta{}
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
	uhs := s.st.AddUFH(fhs)
	for i, n := range store.UFHNames() {
		s.refs[n] = uhs[i]
		s.refNames[uhs[i]] = n
	}
	ihs := s.st.AddInterval()
	for i, n := range store.IntervalNames() {
		s.refs[n] = ihs[i]
		s.refNames[ihs[i]] = n
	}
	phs := s.st.AddPath(fhs, ihs)
	for i, n := range store.PathNames() {
		s.refs[n] = phs[i]
		s.refNames[phs[i]] = n
	}
	chs := s.st.AddFace(ihs)
	for i, n := range store.FaceNames() {
		s.refs[n] = chs[i]
		s.refNames[chs[i]] = n
	}
	shs := s.st.AddSystems(chs)
	for i, n := range store.SysNames() {
		s.refs[n] = shs[i]
		s.refNames[shs[i]] = n
	}
	khs := s.st.AddKan(fhs, ihs, chs, shs)
	for i, n := range store.KanNames() {
		s.refs[n] = khs[i]
		s.refNames[khs[i]] = n
	}
	ghs := s.st.AddSigma(fhs)
	for i, n := range store.SigmaNames() {
		s.refs[n] = ghs[i]
		s.refNames[ghs[i]] = n
	}
	nhs := s.st.AddCoind(fhs)
	for i, n := range store.CoindNames() {
		s.refs[n] = nhs[i]
		s.refNames[nhs[i]] = n
	}
	gdhs := s.st.AddGuard(fhs)
	for i, n := range store.GuardNames() {
		s.refs[n] = gdhs[i]
		s.refNames[gdhs[i]] = n
	}
	ohs := s.st.AddIO()
	for i, n := range store.IONames() {
		s.refs[n] = ohs[i]
		s.refNames[ohs[i]] = n
	}
	prhs := s.st.AddPar(ohs)
	for i, n := range store.ParNames() {
		s.refs[n] = prhs[i]
		s.refNames[prhs[i]] = n
	}
	evhs := s.st.AddEquiv(fhs, ghs)
	for i, n := range store.EquivNames() {
		s.refs[n] = evhs[i]
		s.refNames[evhs[i]] = n
	}
	glhs := s.st.AddGlue(fhs, chs, shs, evhs)
	for i, n := range store.GlueNames() {
		s.refs[n] = glhs[i]
		s.refNames[glhs[i]] = n
	}
	yhs := s.st.AddFsplit(fhs, chs, shs)
	for i, n := range store.FsplitNames() {
		s.refs[n] = yhs[i]
		s.refNames[yhs[i]] = n
	}
	zhs := s.st.AddSysU(fhs, chs, shs)
	for i, n := range store.SysUNames() {
		s.refs[n] = zhs[i]
		s.refNames[zhs[i]] = n
	}
	dhs := s.st.AddSplitD(fhs, chs, shs)
	for i, n := range store.SplitDNames() {
		s.refs[n] = dhs[i]
		s.refNames[dhs[i]] = n
	}
	vhs := s.st.AddForall(ihs, chs)
	for i, n := range store.ForallNames() {
		s.refs[n] = vhs[i]
		s.refNames[vhs[i]] = n
	}
	whs := s.st.AddPabsU(fhs, ihs)
	for i, n := range store.PabsUNames() {
		s.refs[n] = whs[i]
		s.refNames[whs[i]] = n
	}
	pahs := s.st.AddPappU(fhs, ihs)
	for i, n := range store.PappUNames() {
		s.refs[n] = pahs[i]
		s.refNames[pahs[i]] = n
	}
	hihs := s.st.AddHit(fhs)
	for i, n := range store.HitNames() {
		s.refs[n] = hihs[i]
		s.refNames[hihs[i]] = n
	}
	suhs := s.st.AddSusp(fhs)
	for i, n := range store.SuspNames() {
		s.refs[n] = suhs[i]
		s.refNames[suhs[i]] = n
	}
	qhhs := s.st.AddQuotHit(fhs)
	for i, n := range store.QuotHitNames() {
		s.refs[n] = qhhs[i]
		s.refNames[qhhs[i]] = n
	}
	pphs := s.st.AddPathP(fhs, ihs)
	for i, n := range store.PathPNames() {
		s.refs[n] = pphs[i]
		s.refNames[pphs[i]] = n
	}
	trhs := s.st.AddTrunc(fhs)
	for i, n := range store.TruncNames() {
		s.refs[n] = trhs[i]
		s.refNames[trhs[i]] = n
	}
	cihs := s.st.AddCircInd(hihs, phs, pphs, fhs, ihs)
	s.refs[store.CircIndName()] = cihs
	s.refNames[cihs] = store.CircIndName()
	suihs := s.st.AddSuspInd(suhs, phs, pphs, fhs)
	s.refs[store.SuspIndName()] = suihs
	s.refNames[suihs] = store.SuspIndName()
	quihs := s.st.AddQuotInd(qhhs, phs, pphs, fhs)
	s.refs[store.QuotIndName()] = quihs
	s.refNames[quihs] = store.QuotIndName()
}

func (s *Session) resolver() *surface.Resolver {
	return &surface.Resolver{Refs: s.refs, Num: s.numConfig()}
}

// numConfig resolves the registered numeral bindings to the constructor hashes a
// literal expands against. Names are looked up in the session's reference map;
// they are validated when the binding is registered (AddBuiltinNat), so the
// lookups here are total.
func (s *Session) numConfig() surface.NumConfig {
	var cfg surface.NumConfig
	if s.nat != nil {
		cfg.HasNat = true
		cfg.NatZero = s.refs[s.nat.Zero]
		cfg.NatSucc = s.refs[s.nat.Succ]
	}
	for _, inj := range s.numInjs {
		cfg.Injs = append(cfg.Injs, surface.NumInj{Kind: inj.Kind, Inj: s.refs[inj.InjName]})
	}
	return cfg
}

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
	if d.IsPartial {
		return s.addPartialDef(d)
	}
	if d.IsForeign {
		return s.addForeignDef(d)
	}
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
	// Record guard-sugar metadata and source position for the ledger.
	// Read-modify-write because Go map values are not addressable directly.
	m := s.meta[d.Name]
	m.usesGuard = d.UsesGuard
	m.pos = d.Pos
	s.meta[d.Name] = m
	if _, exists := s.byHash[h]; !exists {
		s.order = append(s.order, h)
	}
	s.refs[d.Name] = h
	s.refNames[h] = d.Name
	s.byHash[h] = rd
	// C2: register a typeclass instance under its class key, so a later
	// constrained use resolves the dictionary by implicit search.
	if d.IsInstance {
		if class, arg, ok := classKeyOfType(ty); ok {
			s.instances[[2]core.Hash{class, arg}] = h
		} else {
			return Def{}, fmt.Errorf("instance %q: its type must be a class application (e.g. Monoid Nat), or a parametric instance ending in one (e.g. {A : U} -> Eq A -> Eq (List A))", d.Name)
		}
	}
	return rd, nil
}

// addForeignDef elaborates a `foreign` axiom's type and stores it bodiless,
// recorded as an assumption (R-FFI / B4).
func (s *Session) addForeignDef(d surface.Def) (Def, error) {
	el := s.elaborator()
	ty, err := el.ElabForeign(d)
	if err != nil {
		return Def{}, err
	}
	h := s.st.AddForeign(d.Name, ty)
	// Record how the def was written (postulate + reason + source position) as
	// session-side metadata. A `postulate` registers through this same
	// bodiless/assumed path; only the metadata distinguishes it from a `foreign`.
	// Never hashed.
	s.meta[d.Name] = defMeta{postulate: d.IsPostulate, why: d.Why, pos: d.Pos}
	rd := Def{Name: d.Name, Ty: ty, Hash: h, Postulate: d.IsPostulate, Why: d.Why}
	if _, exists := s.byHash[h]; !exists {
		s.order = append(s.order, h)
	}
	s.refs[d.Name] = h
	s.refNames[h] = d.Name
	s.byHash[h] = rd
	return rd, nil
}

// Postulate reports whether name was declared as a `postulate` (an asserted
// debt) rather than a trusted `foreign` axiom. Session metadata only.
func (s *Session) Postulate(name string) bool { return s.meta[name].postulate }

// Why returns the stated reason of a postulated name (empty otherwise). Session
// metadata only - never part of the def's content hash.
func (s *Session) Why(name string) string { return s.meta[name].why }

// addPartialDef elaborates and stores a general-recursive `partial` definition
// (C4): the body is elaborated with the self-reference at Placeholder(0), then
// stored under its real content hash (placeholder substituted) and marked
// partial so its head stays neutral during type-checking.
func (s *Session) addPartialDef(d surface.Def) (Def, error) {
	el := s.elaborator()
	ty, body, err := el.ElabPartialDef(d, store.Placeholder(0))
	if err != nil {
		return Def{}, err
	}
	h := s.st.AddPartial(d.Name, ty, body)
	rd := Def{Name: d.Name, Ty: ty, Body: body, Hash: h}
	if _, exists := s.byHash[h]; !exists {
		s.order = append(s.order, h)
	}
	s.refs[d.Name] = h
	s.refNames[h] = d.Name
	s.byHash[h] = rd
	return rd, nil
}

// addPartialGroup elaborates and stores a MUTUALLY-recursive `partial` group: all
// members are elaborated with every member's name in scope (at Placeholder(i)),
// then SCC-hashed and bound. Each head stays neutral (the C4 firewall), so mutual
// recursion cannot diverge the checker.
func (s *Session) AddDefGroup(g surface.DefGroup) ([]string, error) {
	el := s.elaborator()
	names := make([]string, len(g.Members))
	for i, m := range g.Members {
		names[i] = m.Name
	}
	tys, bodies, err := el.ElabPartialGroup(g.Members)
	if err != nil {
		return nil, err
	}
	hs := s.st.AddPartialGroup(names, tys, bodies)
	for i, h := range hs {
		rd := Def{Name: names[i], Ty: tys[i], Body: bodies[i], Hash: h}
		if _, exists := s.byHash[h]; !exists {
			s.order = append(s.order, h)
		}
		s.refs[names[i]] = h
		s.refNames[h] = names[i]
		s.byHash[h] = rd
	}
	return names, nil
}

// LoadSource parses a file of top-level items (definitions and datatype
// declarations) and adds each in order, returning the names added. On the
// first error the names added so far are returned alongside it.
//
// Import and alias directives are scoped to this call: they reset per LoadSource
// invocation and are not session state. Each directive is validated at the point
// it appears (the imported/aliased module must have >= 1 definition in the session
// or defined later in this same source). After a directive, name references in
// subsequent items are rewritten through the active scope before elaboration.
func (s *Session) LoadSource(src string) ([]string, error) {
	items, err := surface.ParseProgram(src)
	if err != nil {
		// A file is never "incomplete" -- a parse failure here is terminal, so render
		// it with a source caret (REPL handles ErrIncomplete itself, before this).
		return nil, errors.New(surface.RenderParseError(src, err))
	}

	// Collect all names this source defines up front so that an 'import' or 'alias'
	// directive can reference a module defined LATER in the same file.
	local := definedNames(items)
	sc := newImportScope(s, local)

	// Self-import: for every module whose items appear in this source (detected by
	// qualified names in the local set), pre-install an implicit import so that
	// unqualified references inside module bodies resolve to their siblings.
	// Example: `module M is data Flag ... end; lit : Flag is on end end` -- `Flag`
	// and `on` in `lit`'s body resolve to `M.Flag`/`M.on` via import M.
	// We add self-modules BEFORE user imports so they have the first pick; ambiguity
	// with an explicit import is still reported (two distinct modules both defining a
	// name the user references unqualified).
	{
		seen := map[string]bool{}
		for n := range local {
			if i := strings.LastIndex(n, "."); i > 0 {
				mod := n[:i]
				if !seen[mod] {
					seen[mod] = true
					// Direct insert: no need to validate (local contains mod.* so it is known).
					sc.imports = append(sc.imports, mod)
				}
			}
		}
	}

	var added []string
	for _, it := range items {
		switch d := it.(type) {
		case surface.Import:
			if err := sc.addImport(d.Module); err != nil {
				return added, err
			}
		case surface.Alias:
			if err := sc.addAlias(d.Module, d.As); err != nil {
				return added, err
			}
		case surface.Def:
			d, err = sc.rewriteDef(d)
			if err != nil {
				return added, err
			}
			rd, err := s.AddDef(d)
			if err != nil {
				return added, err
			}
			added = append(added, rd.Name)
		case surface.DataDef:
			d, err = sc.rewriteDataDef(d)
			if err != nil {
				return added, err
			}
			names, err := s.AddData(d)
			if err != nil {
				return added, err
			}
			added = append(added, names...)
		case surface.DefGroup:
			d, err = sc.rewriteDefGroup(d)
			if err != nil {
				return added, err
			}
			names, err := s.AddDefGroup(d)
			if err != nil {
				return added, err
			}
			added = append(added, names...)
		case surface.DataGroup:
			d, err = sc.rewriteDataGroup(d)
			if err != nil {
				return added, err
			}
			names, err := s.AddDataGroup(d)
			if err != nil {
				return added, err
			}
			added = append(added, names...)
		case surface.BuiltinNat:
			if err := s.AddBuiltinNat(d); err != nil {
				return added, err
			}
		case surface.BuiltinNatOp:
			if err := s.AddBuiltinNatOp(d); err != nil {
				return added, err
			}
		case surface.BuiltinNumInj:
			if err := s.AddBuiltinNumInj(d); err != nil {
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

// AddBuiltinNumInj validates and registers a typed numeral injection (numeric-
// tower rung C4): `builtin int Z intOf` / `builtin rat Rat ratOf`. It requires an
// active `builtin nat` (the injection's domain is that Nat) and validates the
// named function has type `Nat -> Ty`. Thereafter a numeral CHECKED AT Ty lowers
// to `inj (NatLit n)` (elaborate/check_surface.go elabNum) — the inner literal is
// a genuine Nat (bignum + accel intact), and inj carries it into the tower type.
// Session state only; nothing enters the store. Multiple injections coexist,
// dispatched by the expected type; a re-declaration of the same kind replaces it.
func (s *Session) AddBuiltinNumInj(b surface.BuiltinNumInj) error {
	if s.nat == nil {
		return fmt.Errorf("builtin %s: requires a `builtin nat` binding (the injection's domain is that Nat); declare `builtin nat` first", b.Kind)
	}
	if _, ok := s.refs[b.TyName]; !ok {
		return fmt.Errorf("builtin %s: unknown type %q", b.Kind, b.TyName)
	}
	if _, ok := s.refs[b.InjName]; !ok {
		return fmt.Errorf("builtin %s: unknown name %q", b.Kind, b.InjName)
	}
	// Type validation: the injection must be `Nat -> Ty` against the active
	// `builtin nat` type — the rung-C4 discipline (validate by TYPE, not shape).
	natTy := surface.EVar{Name: s.nat.TyName}
	want := surface.EPi{Param: "_", Dom: natTy, Cod: surface.EVar{Name: b.TyName}}
	el := s.elaborator()
	if _, _, err := el.ElabDef(surface.Def{Name: "$builtin-numinj", Ty: want, Body: surface.EVar{Name: b.InjName}}); err != nil {
		return fmt.Errorf("builtin %s: %q is not a %s -> %s injection: %v", b.Kind, b.InjName, s.nat.TyName, b.TyName, err)
	}
	// Replace any prior injection of the same kind; otherwise append.
	for i := range s.numInjs {
		if s.numInjs[i].Kind == b.Kind {
			s.numInjs[i] = b
			return nil
		}
	}
	s.numInjs = append(s.numInjs, b)
	return nil
}

// natOpKinds maps a `builtin natAdd|natMul|natMonus` kind keyword to its core
// NatOp and the integer semantics the registration is validated against.
var natOpKinds = map[string]struct {
	op   core.NatOp
	eval func(a, b int64) int64
}{
	"natAdd": {core.NatOpAdd, func(a, b int64) int64 { return a + b }},
	"natMul": {core.NatOpMul, func(a, b int64) int64 { return a * b }},
	"natMonus": {core.NatOpMonus, func(a, b int64) int64 {
		if a < b {
			return 0
		}
		return a - b
	}},
	"natDiv": {core.NatOpDiv, func(a, b int64) int64 {
		if b == 0 {
			return 0 // a // 0 = 0 (matches the prelude's fuel divF)
		}
		return a / b
	}},
	"natMod": {core.NatOpMod, func(a, b int64) int64 {
		if b == 0 {
			return a // a % 0 = a (matches the prelude's fuel modF)
		}
		return a % b
	}},
}

// AddBuiltinNatOp validates and registers a kernel-acceleration declaration
// (C7 / R-NUM, Decision 1): `builtin natAdd add` (etc.) tags the named def's
// content hash as the accelerated arithmetic op, so a call on two compressed
// literals short-circuits to one bigint step instead of O(a·b) eliminator
// peeling. The accelerated op IS the user's own def, so the bridge to its
// recursive body is refl.
//
// Soundness gate (the metatheory-review event, made a REGISTRATION-TIME check
// rather than a trust assumption). A wrong registration — `builtin natAdd foo`
// where `foo` is actually multiplication — would let the accel produce `a+b`
// while `foo`'s body peels to `a*b`, two values that should be equal yet differ,
// i.e. UNSOUND. So registration is gated twice, layered:
//
//  1. TYPE validation (rung-C4): the def must have type `Nat -> Nat -> Nat`
//     against the active `builtin nat` type — the same shape check `builtin nat`
//     applies to succ.
//  2. SAMPLE / differential validation: for a fixed battery of (a,b) pairs, the
//     def's own UNFOLDED result (computed with acceleration OFF, i.e. by the
//     recursive body peeling the literals) must equal the bigint op. If the def
//     is not really that operation, the declaration is REJECTED here, so no
//     unsound masquerade can ever enter the kernel. This promotes the
//     differential test the design property-tests into a hard gate at the trust
//     boundary.
//
// Like `builtin nat`, the declaration is session state only — nothing enters the
// store. It requires an active `builtin nat` (the accel rule folds its bigint
// result into that binding's literal).
func (s *Session) AddBuiltinNatOp(b surface.BuiltinNatOp) error {
	kind, ok := natOpKinds[b.Kind]
	if !ok {
		return fmt.Errorf("builtin %s: unknown acceleration kind", b.Kind)
	}
	if _, ok := s.refs[b.DefName]; !ok {
		return fmt.Errorf("builtin %s: unknown name %q", b.Kind, b.DefName)
	}
	if s.nat == nil {
		return fmt.Errorf("builtin %s: requires a `builtin nat` binding (the accelerated literal is relative to it); declare `builtin nat` first", b.Kind)
	}
	natTy := surface.EVar{Name: s.nat.TyName}
	// (1) Type validation: the def must be Nat -> Nat -> Nat.
	want := surface.EPi{Param: "_", Dom: natTy, Cod: surface.EPi{Param: "_", Dom: natTy, Cod: natTy}}
	el := s.elaborator()
	if _, _, err := el.ElabDef(surface.Def{Name: "$builtin-natop", Ty: want, Body: surface.EVar{Name: b.DefName}}); err != nil {
		return fmt.Errorf("builtin %s: %q is not a Nat -> Nat -> Nat function: %v", b.Kind, b.DefName, err)
	}
	// (2) Sample / differential validation: the def's UNFOLDED result must agree
	// with the integer semantics on a battery of pairs (incl. boundary and an
	// asymmetric large-ish case). Acceleration is OFF here (the def's recursive
	// body peels the literals), so this compares the body against the bigint op —
	// exactly the agreement the kernel will later assume when accel fires.
	samples := [][2]int64{{0, 0}, {0, 1}, {1, 0}, {1, 1}, {2, 3}, {3, 2}, {5, 7}, {7, 5}}
	for _, p := range samples {
		want := kind.eval(p[0], p[1])
		got, err := s.unfoldedNatCall(b.DefName, p[0], p[1])
		if err != nil {
			return fmt.Errorf("builtin %s: validating %q on (%d,%d): %v", b.Kind, b.DefName, p[0], p[1], err)
		}
		if got != want {
			return fmt.Errorf(
				"builtin %s: %q does not compute %s — on (%d,%d) it yields %d, the op requires %d; refusing the registration (an accelerated op MUST agree with its own recursive body, or the kernel would be unsound)",
				b.Kind, b.DefName, b.Kind, p[0], p[1], got, want)
		}
	}
	s.natAccel[s.refs[b.DefName]] = kind.op
	return nil
}

// unfoldedNatCall evaluates `def a b` for nat literals a,b by the def's ORDINARY
// recursive body — acceleration explicitly OFF — and reads the resulting numeral
// back as an int64. It is the registration-time differential oracle: what the
// kernel computes when the accel rule does NOT fire. Returns an error if the
// result is not a concrete numeral (e.g. the def gets stuck).
func (s *Session) unfoldedNatCall(defName string, a, b int64) (int64, error) {
	cfg := s.numConfig()
	if !cfg.HasNat {
		return 0, fmt.Errorf("no `builtin nat`")
	}
	litA, err := cfg.Nat(big.NewInt(a))
	if err != nil {
		return 0, err
	}
	litB, err := cfg.Nat(big.NewInt(b))
	if err != nil {
		return 0, err
	}
	call := core.App{Fn: core.App{Fn: core.Ref{Hash: s.refs[defName]}, Arg: litA}, Arg: litB}
	// A machine with the FULL ι wiring but with acceleration OFF (m.NatAccel nil), so
	// the call reduces by the def's recursive body / eliminator peeling.
	m := s.normalizerNoAccel()
	nf := m.NormalizeUnfold(call)
	n, ok := s.natLitValueOf(nf)
	if !ok {
		return 0, fmt.Errorf("the def did not reduce to a numeral (it may not be a total nat function): %s", s.Pretty(nf))
	}
	return n, nil
}

// natLitValueOf reads a normalized nat term back as an int64: either a compressed
// NatLit, or a saturated succ-chain over zero (the generalized `builtin nat`
// form, which does not lower to NatLit). Returns ok=false on anything else.
func (s *Session) natLitValueOf(t core.Tm) (int64, bool) {
	if lit, ok := t.(core.NatLit); ok {
		return lit.N.Int64(), true
	}
	zero, succ := s.refs[s.nat.Zero], s.refs[s.nat.Succ]
	var n int64
	for {
		switch x := t.(type) {
		case core.Ref:
			if x.Hash == zero {
				return n, true
			}
			return 0, false
		case core.App:
			fn, ok := x.Fn.(core.Ref)
			if !ok || fn.Hash != succ {
				return 0, false
			}
			n++
			t = x.Arg
		case core.NatLit:
			return n + x.N.Int64(), true
		default:
			return 0, false
		}
	}
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

// ParseSrcExpr parses a single expression against the session. Numerals parse
// to unexpanded ENum and are lowered during elaboration against the session's
// `builtin nat` binding (see NumConfig), so the meaning is fixed by the
// expected type rather than at parse time.
func (s *Session) ParseSrcExpr(src string) (surface.Exp, error) {
	return surface.ParseExpr(src)
}

// Pretty renders a core term with the session's reference names; under a
// `builtin nat` binding, saturated succ-chains fold back to numerals.
func (s *Session) Pretty(t core.Tm) string {
	if s.nat != nil {
		return surface.PrettyNatDec(t, s.refNames, s.refs[s.nat.Zero], s.refs[s.nat.Succ], s.decConfig())
	}
	return surface.PrettyWith(t, s.refNames)
}

// decConfig resolves the prelude's fraction/decimal constructor hashes so the
// printer can fold them to positional notation (1/3, 0.{3}). It is On only when
// every name resolves — a bare or non-prelude session leaves folding off.
func (s *Session) decConfig() surface.DecConfig {
	frac, ok1 := s.refs["frac"]
	rdec, ok2 := s.refs["rdec"]
	wcons, ok3 := s.refs["wcons"]
	wnil, ok4 := s.refs["wnil"]
	tru, ok5 := s.refs["true"]
	// Int / Result folding is optional extra polish: resolve when present, but do
	// not gate `On` on them (a session may have fractions but no Int rung).
	return surface.DecConfig{
		Frac: frac, RDec: rdec, Wcons: wcons, Wnil: wnil, True: tru,
		Int: s.refs["int"], Ok: s.refs["ok"], Err: s.refs["err"],
		DivByZero:   s.refs["divByZero"],
		NotIntegral: s.refs["notIntegral"],
		Negative:    s.refs["negative"],
		NotCounting: s.refs["notCounting"],
		Bytes:       s.refs["bytes"],
		On:          ok1 && ok2 && ok3 && ok4 && ok5,
	}
}

// Defs returns the session definitions in definition order.
func (s *Session) Defs() []Def {
	out := make([]Def, 0, len(s.order))
	for _, h := range s.order {
		d := s.byHash[h]
		m := s.meta[d.Name]
		d.Postulate = m.postulate
		d.Why = m.why
		d.UsesGuard = m.usesGuard
		d.Pos = m.pos
		out = append(out, d)
	}
	return out
}

// RefNames returns the hash->name map for rendering references; callers must not
// mutate it.
func (s *Session) RefNames() map[core.Hash]string { return s.refNames }

// elaborator returns a fresh per-run Elaborator over the session store.
func (s *Session) elaborator() *elaborate.Elaborator {
	el := elaborate.New(s.st, s.refs, s.refNames)
	el.Num = s.numConfig()
	el.M.Data = s.st
	el.M.Quot = s.st
	el.M.Fibrant = s.st
	el.M.FibUniverse = s.st
	el.M.Interval = s.st
	el.M.Path = s.st
	el.M.Face = s.st
	el.M.System = s.st
	el.M.Kan = s.st
	el.M.Sigma = s.st
	el.M.Coind = s.st
	el.M.Guard = s.st
	el.M.Glue = s.st
	el.M.Fsplit = s.st
	el.M.SystemU = s.st
	el.M.FsplitD = s.st
	el.M.Forall = s.st
	el.M.PabsU = s.st
	el.M.PappU = s.st
	el.M.Hit = s.st
	el.M.Susp = s.st
	el.M.QuotHit = s.st
	el.M.PathP = s.st
	el.M.CircInd = s.st
	el.M.SuspInd = s.st
	el.M.QuotInd = s.st
	el.M.Trunc = s.st
	el.M.Partial = s.st
	el.M.NatAccel = s.natAccelInfo()
	el.InstanceFor = func(class, arg core.Hash) (core.Tm, bool) {
		if h, ok := s.instances[[2]core.Hash{class, arg}]; ok {
			return core.Ref{Hash: h}, true
		}
		return nil, false
	}
	return el
}

// classKeyOfType extracts (class-former, last-arg-head) from an instance's type
// `C … T` (e.g. Monoid Nat → (Monoid, Nat)) so it can be registered in the
// instance table. PARAMETRIC instances (C2b) carry leading Pis — `{A : U} -> Eq A
// -> Eq (List A)` — which are peeled first so the key is read off the CODOMAIN's
// class application (here (Eq, List)); the premises are resolved by recursive
// search at the use site (elaborate/resolveClass). Returns ok=false if the
// codomain is not a ref-headed class application.
func classKeyOfType(ty core.Tm) (class, arg core.Hash, ok bool) {
	for {
		if pi, isPi := ty.(core.Pi); isPi {
			ty = pi.Cod.Body
			continue
		}
		break
	}
	var args []core.Tm
	for {
		if app, isApp := ty.(core.App); isApp {
			args = append([]core.Tm{app.Arg}, args...)
			ty = app.Fn
			continue
		}
		break
	}
	ref, isRef := ty.(core.Ref)
	if !isRef || len(args) == 0 {
		return core.Hash{}, core.Hash{}, false
	}
	last := args[len(args)-1]
	for {
		if app, isApp := last.(core.App); isApp {
			last = app.Fn
			continue
		}
		break
	}
	lref, isLRef := last.(core.Ref)
	if !isLRef {
		return core.Hash{}, core.Hash{}, false
	}
	return ref.Hash, lref.Hash, true
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
	if err := el.ResolvePending(); err != nil {
		return nil, nil, err
	}
	t = el.Zonk(0, t)
	tyTm := el.Zonk(0, el.M.Quote(0, vty))
	if err := el.ErrUnsolved("expression"); err != nil {
		return nil, nil, err
	}
	return t, tyTm, nil
}

// newNormalizer builds a fresh per-run core Machine with the full ι wiring over
// the session store. When accel is true it also installs the C7 acceleration
// table (m.NatAccel); with accel false the nat ops reduce by their ordinary recursive
// bodies — the latter is the registration-time differential oracle
// (unfoldedNatCall) the soundness gate compares the accelerated rule against.
func (s *Session) newNormalizer(accel bool) *core.Machine {
	m := core.NewMachine(s.st)
	m.Equality = equality.Default()
	m.Data = s.st
	m.Quot = s.st
	m.Fibrant = s.st
	m.FibUniverse = s.st
	m.Interval = s.st
	m.Path = s.st
	m.Face = s.st
	m.System = s.st
	m.Kan = s.st
	m.Sigma = s.st
	m.Coind = s.st
	m.Guard = s.st
	m.Glue = s.st
	m.Fsplit = s.st
	m.SystemU = s.st
	m.FsplitD = s.st
	m.Forall = s.st
	m.PabsU = s.st
	m.PappU = s.st
	m.Hit = s.st
	m.Susp = s.st
	m.QuotHit = s.st
	m.PathP = s.st
	m.CircInd = s.st
	m.SuspInd = s.st
	m.QuotInd = s.st
	m.Trunc = s.st
	m.Partial = s.st
	if accel {
		m.NatAccel = s.natAccelInfo()
	}
	return m
}

// normalizerNoAccel is the machine the soundness gate uses: full ι, acceleration
// OFF, so an accelerated op runs by its recursive body.
func (s *Session) normalizerNoAccel() *core.Machine { return s.newNormalizer(false) }

// NormalizeExpr fully normalizes (βδι) a closed, well-typed core term, with the
// C7 acceleration table active so closed nat arithmetic takes the one-bigint-step
// fast path.
func (s *Session) NormalizeExpr(t core.Tm) core.Tm {
	return s.newNormalizer(true).NormalizeUnfold(t)
}

// NormalizeExprFolded reduces redexes (incl. stuck projections like `Fst subWhole`)
// but leaves top-level definition references FOLDED — so a type displays with its
// names intact (`Result Nat ArithErr`, not `Result (Sig Whole …) ArithErr`) while a
// resolved-dictionary projection still collapses to the underlying type.
func (s *Session) NormalizeExprFolded(t core.Tm) core.Tm {
	return s.newNormalizer(true).Normalize(t)
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

// AddDataGroup elaborates and stores a MUTUALLY-recursive `mutual data … end` group
// (MB1). The members are elaborated together (each former in scope in every member's
// constructors), content-addressed as one group (store.AddDataGroup, tag 'G'), and
// every former/constructor/eliminator is bound in declaration order — so codegen
// emits each member's eliminator (same-type IH) and constructors like any datatype.
func (s *Session) AddDataGroup(g surface.DataGroup) ([]string, error) {
	el := s.elaborator()
	decls, err := el.ElabDataGroup(g)
	if err != nil {
		return nil, err
	}
	datas, ctorsPer, elims := s.st.AddDataGroup(decls)
	var names []string
	for m, decl := range decls {
		s.bindName(decl.Name, datas[m])
		names = append(names, decl.Name)
		for j, ch := range ctorsPer[m] {
			s.bindName(decl.CtorNames[j], ch)
			names = append(names, decl.CtorNames[j])
		}
		elimName := decl.Name + "Elim"
		s.bindName(elimName, elims[m])
		names = append(names, elimName)
	}
	return names, nil
}

// BindResult binds a REPL expression result under a synthesized name (e.g. the
// internal name behind a $N reference) so later input can name it. It stores
// (ty, tm) and registers the name for RESOLUTION ONLY — deliberately not in
// refNames/byHash/order — so the binding resolves and unfolds without cluttering
// :list or hijacking value pretty-printing. tm and ty must be zonked and meta-free,
// which is exactly what ElabExpr returns.
func (s *Session) BindResult(name string, tm, ty core.Tm) {
	s.refs[name] = s.st.Add(name, ty, tm)
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

// Assumed reports whether name resolves to a bodiless assumed definition (a
// foreign axiom or a postulate). It is the read seam the assurance ledger uses
// to classify the assume/postulate tiers.
func (s *Session) Assumed(name string) bool {
	h, ok := s.refs[name]
	if !ok {
		return false
	}
	return s.st.IsAssumed(h)
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
	if main != "" {
		if _, ok := s.refs[main]; !ok {
			return codegen.Program{}, fmt.Errorf("no definition named %q", main)
		}
	}
	p, env, err := s.emitDefs()
	if err != nil {
		return p, err
	}
	if main != "" {
		if bad, isTainted := env.tainted[main]; isTainted {
			return p, fmt.Errorf(
				"definition %q uses the inner layer (%s): inner constructions check but do not deploy in v3 (see ref_docs/rune-v3-design.md §F)",
				main, bad)
		}
		p.Main = main
		p.IOMain = s.isIOType(s.byHash[s.refs[main]].Ty)
	}
	// Tree-shake: prune the program to only the definitions reachable from Main.
	// This runs POST-p.Main assignment and BEFORE CheckPrimCollisions so that the
	// collision guard sees only reachable foreigns -- an unreachable colliding
	// foreign in the prelude must not block a program that never uses it.
	p = codegen.Shake(p)
	// Guard against two distinct qualified foreigns sharing a prim segment that
	// maps to a known ioPrim -- that would silently gate the wrong host body.
	if err := codegen.CheckPrimCollisions(p); err != nil {
		return p, err
	}
	return p, nil
}

// isIOType reports whether a definition's type is headed by the IO former (C3),
// so an IO main is RUN (its world thunk forced) rather than printed as a value.
func (s *Session) isIOType(ty core.Tm) bool {
	ohs, ok := s.st.IOHashes()
	if !ok || ty == nil {
		return false
	}
	t := ty
	for {
		if app, isApp := t.(core.App); isApp {
			t = app.Fn
			continue
		}
		break
	}
	ref, isRef := t.(core.Ref)
	return isRef && ref.Hash == ohs[1] // ohs[1] = IO
}

// EmitExpr elaborates a surface expression against the session (the kernel
// stays the authority on typing) and lowers it to an erased program whose Main
// is a synthetic entry — WITHOUT registering the expression as a session
// definition. This is the REPL's `:run` seam: the kernel evaluator proves, the
// erased shadow performs.
func (s *Session) EmitExpr(e surface.Exp) (codegen.Program, error) {
	tm, ty, err := s.ElabExpr(e)
	if err != nil {
		return codegen.Program{}, err
	}
	p, env, err := s.emitDefs()
	if err != nil {
		return p, err
	}
	ir, err := env.eraser.Expr(tm)
	if err != nil {
		return codegen.Program{}, err
	}
	// Inner-layer taint refuses exactly as a tainted main does: transport
	// along a ua-path has no erased meaning in v3.
	if bad := innerTaint(ir, env.tainted); bad != "" {
		return p, fmt.Errorf(
			"expression uses the inner layer (%s): inner constructions check but do not deploy in v3 (see ref_docs/rune-v3-design.md §F)",
			bad)
	}
	// "$it" cannot collide with a session definition (no legal surface
	// identifier starts with '$') and survives codegen name mangling intact.
	const entry = "$it"
	p.Defs = append(p.Defs, codegen.DefSpec{Name: entry, Body: ir})
	p.Main = entry
	// An IO-typed expression is RUN (its world thunk forced), not printed as a
	// neutral value — so `:run do ... end` (and `:run printNat 1`) performs its
	// effects rather than showing the unforced thunk.
	p.IOMain = s.isIOType(ty)
	// Tree-shake and guard against prim collisions, mirroring EmitProgram.
	p = codegen.Shake(p)
	if err := codegen.CheckPrimCollisions(p); err != nil {
		return p, err
	}
	return p, nil
}

// emitEnv is the shared erasure environment emitDefs builds: the per-hash
// emit names (shadow-suffixed where a name was rebound), the type-denoting
// hashes that erase to the unit token, and the inner-tainted definitions.
type emitEnv struct {
	eraseNames map[core.Hash]string
	typeRefs   map[core.Hash]bool
	tainted    map[string]string
	eraser     *elaborate.TypedEraser
}

// emitDefs lowers every session definition to the erased IR program (no Main)
// and returns the erasure environment so callers can erase one more body
// against it.
func (s *Session) emitDefs() (codegen.Program, emitEnv, error) {
	var p codegen.Program
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
		// A `foreign` axiom whose TYPE is a universe is a foreign TYPE former (e.g.
		// `foreign Bin : U`, `foreign Float : U`): it denotes a type at runtime and
		// erases to the unit token, exactly like a datatype former. Without this, a
		// bare reference to it in an erased position (an eliminator motive, an
		// explicit type argument) would leak as a bodiless IForeign accessor —
		// undefined on the backends that require every accessor to be defined.
		if s.st.IsAssumed(h) {
			if d, ok := s.byHash[h]; ok {
				if _, isU := d.Ty.(core.Univ); isU {
					typeRefs[h] = true
				}
			}
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
	// The interval I is a type former (erases to the unit token); its value
	// members (i0, i1, ineg, imin, imax) have no runtime meaning yet — paths
	// get a shadow at §F phase 5 — so they are inner-tainted (innerTaint below).
	if ihs, ok := s.st.IntervalHashes(); ok {
		typeRefs[ihs[0]] = true
	}
	// The face lattice type F is a type former (erases to the unit token); its
	// value members (ieq0/ieq1/fand/for/ftop/fbot) are inner — cofibrations
	// drive Kan operations, with no runtime meaning yet (innerTaint below).
	if chs, ok := s.st.FaceHashes(); ok {
		typeRefs[chs[0]] = true
	}
	// holds : F -> Prop is a (Prop-valued) type former; its proofs (htop/hand/…)
	// are Prop and erase via the type-directed eraser anyway.
	if shs, ok := s.st.SysHashes(); ok {
		typeRefs[shs[0]] = true
	}
	// The inner Sigma former sigmaF erases like any type former; pairF/fstF/sndF
	// are value members with no runtime meaning yet (inner-tainted below).
	if ghs, ok := s.st.SigmaHashes(); ok {
		typeRefs[ghs[0]] = true
	}
	// The coinductive former Nu erases like any type former; out/unfold are value
	// members with no runtime meaning yet (inner-tainted below).
	if nhs, ok := s.st.CoindHashes(); ok {
		typeRefs[nhs[0]] = true
	}
	// World and IO are type formers; pureIO/bindIO have no runtime meaning yet
	// (the codegen IForeign lowering is the C3 follow-up).
	if ohs, ok := s.st.IOHashes(); ok {
		typeRefs[ohs[0]] = true
		typeRefs[ohs[1]] = true
	}
	// Equiv and the Glue former are type formers (erase to the unit token);
	// equivFun/glue/unglue are inner value members with no runtime meaning yet
	// (inner-tainted below).
	if evhs, ok := s.st.EquivHashes(); ok {
		typeRefs[evhs[0]] = true
	}
	if glhs, ok := s.st.GlueHashes(); ok {
		typeRefs[glhs[0]] = true
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
	// C7 / R-NUM Decision 4: thread the registered accel-op def NAMES into the
	// NatSpec so each backend can emit a call to a registered natAdd/natMul/
	// natMonus def as the host's native arithmetic (mirroring how ElimName flows
	// to the emitNat fast path). natAccel keys are def hashes; the accel def is
	// the current (unshadowed) binding, so its emit name is the plain name. Only
	// done when the builtin-nat data group itself compiles to native integers
	// (p.Nat set above) — otherwise there is no native representation to add on.
	if p.Nat != nil && len(s.natAccel) > 0 {
		ops := map[string]core.NatOp{}
		for h, op := range s.natAccel {
			name, ok := emitNames[h]
			if !ok {
				name = s.refNames[h]
			}
			if name != "" {
				ops[name] = op
			}
		}
		p.Nat.Ops = ops
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
	// The eraser is type-directed (elaborate.TypedEraser): it reads the
	// declared type of each body so proof-irrelevant subterms and 0-quantity
	// arguments erase to the unit token, instead of leaking deep proof-only
	// numerals into the shadow (ref_docs/rune-verified-implementations.md).
	eraser := elaborate.NewEraser(s.elaborator(), eraseNames, typeRefs)
	// `foreign` axioms (R-FFI) erase to IForeign host accessors, not IGlobals —
	// the host links the implementation under the same accessor on every backend.
	foreign := map[core.Hash]bool{}
	for _, h := range s.order {
		if s.st.IsAssumed(h) {
			foreign[h] = true
		}
	}
	eraser.SetForeign(foreign)
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
		// A `foreign` axiom (R-FFI) is host-provided: the emitted program
		// references it by name; the host links the implementation.
		if s.st.IsAssumed(h) {
			continue
		}
		body, ok := s.st.Unfold(h)
		if !ok {
			return p, emitEnv{}, fmt.Errorf("definition %q has no body to emit", name)
		}
		ty, ok := s.st.TypeOf(h)
		if !ok {
			return p, emitEnv{}, fmt.Errorf("definition %q has no stored type to emit against", name)
		}
		ir, err := eraser.Def(ty, body)
		if err != nil {
			return p, emitEnv{}, fmt.Errorf("erasing %q: %w", name, err)
		}
		// Inner-layer taint: a definition whose erased body references a
		// fibrant VALUE member (or another tainted definition) checks but
		// does not deploy in v3 — transport along a ua-path has no erased
		// meaning yet, and emitting it would silently compute the wrong
		// function. Tainted definitions are skipped; only a tainted MAIN is
		// an error (the callers' check; see ref_docs/rune-v3-design.md §F).
		if bad := innerTaint(ir, tainted); bad != "" {
			// B5 / R-ERASE2 (deploy-ban slice): inner cubical scaffolding that
			// COMPUTES AWAY to an outer value has a genuine runtime meaning — its
			// NORMAL FORM (definitionally equal by NbE) is taint-free. Retry the
			// erasure on the normalized body; if the inner ops reduced off (e.g.
			// `transp (λ_. fib Nat) zero ~> zero`, `papp … (preflF) i0 ~> x`), the
			// result deploys soundly. If the normal form still carries inner heads
			// it stays tainted (skipped) exactly as before — so this only ever ADDS
			// sound deployments, never emits a guessed erasure.
			nf := s.NormalizeExpr(body)
			if irNF, err2 := eraser.Def(ty, nf); err2 == nil && innerTaint(irNF, tainted) == "" {
				ir = irNF
			} else {
				tainted[name] = bad
				continue
			}
		}
		if s.st.IsPartial(h) {
			if p.Partials == nil {
				p.Partials = map[string]bool{}
			}
			p.Partials[name] = true
		}
		p.Defs = append(p.Defs, codegen.DefSpec{Name: name, Body: ir, Arity: codegen.LeadingLamCount(ir)})
	}
	// Record the MUTUAL-recursion groups (T4): a `mutual partial` group's members,
	// so the trampoline bounces a tail call to ANY emitted sibling, not just self.
	// Each group is recorded once; only members actually emitted as partials count.
	seenGroup := map[core.Hash]bool{}
	for _, h := range s.order {
		if !s.st.IsPartial(h) || !p.Partials[emitNames[h]] {
			continue
		}
		grp, ok := s.st.PartialGroupOf(h)
		if !ok || seenGroup[grp[0]] {
			continue
		}
		seenGroup[grp[0]] = true
		var members []string
		for _, gh := range grp {
			if n := emitNames[gh]; n != "" && p.Partials[n] {
				members = append(members, n)
			}
		}
		if len(members) >= 2 {
			p.PartialGroups = append(p.PartialGroups, members)
		}
	}
	// Trampoline: mark tail-position calls to a partial's recursion group (self in
	// v1, the whole SCC group in T4) so the backends with a driver flatten deep
	// (mutual) tail recursion onto the heap. A no-op when nothing is partial.
	codegen.MarkTailBounces(&p)
	return p, emitEnv{eraseNames: eraseNames, typeRefs: typeRefs, tainted: tainted, eraser: eraser}, nil
}

// innerTaint reports the inner-layer name (a fibrant value member, or a
// previously-tainted definition) an erased body references, or "".
func innerTaint(t codegen.Ir, tainted map[string]string) string {
	inner := map[string]bool{"preflF": true, "pathJ": true, "ureflU": true, "castU": true,
		// The derived-univalence prelude defs produce inner values (pathU / El of an
		// inner type) with no erased meaning AS VALUES — a def that references one is
		// provisionally tainted, then B5 retries on its normal form (so `castU`-through
		// -`ua` that computes to an outer Bool still deploys, while a bare `ua` path
		// stays refused).
		"ua": true, "uaGlue": true, "isoToEquiv": true, "equivFromIso": true,
		"idEquivF": true, "eqToPathF": true, "triangleCoh": true,
		"i0": true, "i1": true, "ineg": true, "imin": true, "imax": true,
		"pabs": true, "papp": true,
		"ieq0": true, "ieq1": true, "fand": true, "for": true, "ftop": true, "fbot": true,
		"htop": true, "hand": true, "horl": true, "horr": true,
		"transp": true, "hcomp": true, "comp": true, "transpG": true,
		"sigmaF": true, "pairF": true, "fstF": true, "sndF": true,
		"Nu": true, "out": true, "unfold": true,
		"Clock": true, "k0": true, "Later": true, "next": true, "dfix": true, "lmap": true, "lap": true, "force": true, "gfix": true, "forceD": true, "gfixF": true, "laterApp": true, "lapD": true,
		"equivFun": true, "glue": true, "unglue": true, "unglueT": true, "fsplit": true, "forallF": true, "pabsU": true,
		"sysU": true, "fsplitD": true, "pappU": true,
		"base": true, "loop": true, "circElim": true,
		"north": true, "south": true, "merid": true, "suspElim": true,
		"qinc": true, "qrel": true, "quotElim": true,
		"pathP": true, "pabsP": true, "pappP": true, "circInd": true,
		"suspInd": true, "quotInd": true,
		"Trunc0": true, "inc": true, "squash": true, "trunc0Rec": true,
	}
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
