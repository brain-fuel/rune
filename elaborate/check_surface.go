package elaborate

import (
	"fmt"

	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/store"
	"goforge.dev/rune/v3/surface"
)

// The surface elaborator: bidirectional Infer/Check from named surface Exp to
// core Tm. It uses the binder annotation the grammar requires on every fn (so a
// bare lambda infers), inserts implicit arguments as fresh metavariables, solves
// holes, and unifies instead of merely converting. On plain programs — no holes,
// no implicits — it emits exactly the core that name resolution emits, so those
// content hashes are unchanged.

// Infer elaborates e, returning the core term and its inferred type.
func (e *Elaborator) Infer(c *Ctx, x surface.Exp) (core.Tm, core.Val, error) {
	switch s := x.(type) {
	case surface.EVar:
		if i, ty, ok := c.lookup(s.Name); ok {
			e.useVar(c, i)
			return core.Var{Idx: i}, ty, nil
		}
		if h, ok := e.Refs[s.Name]; ok {
			ty, err := e.refType(h)
			if err != nil {
				return nil, nil, err
			}
			return core.Ref{Hash: h}, ty, nil
		}
		return nil, nil, e.unboundError(c, s.Name)
	case surface.EUniv:
		return core.Univ{Lvl: s.Lvl}, core.VU{Lvl: s.Lvl + 1}, nil // U_i : U_{i+1}
	case surface.EProp:
		return core.Prop{}, core.VU{}, nil // Prop : U_0
	case surface.ENum:
		// In infer position a numeral takes its default (unary) meaning; the
		// expected type, when there is one, redirects it in Check.
		tm, err := e.Num.Nat(s.Val)
		if err != nil {
			return nil, nil, err
		}
		natTy, err := e.natType()
		if err != nil {
			return nil, nil, err
		}
		return tm, natTy, nil
	case surface.EEq, surface.ERefl, surface.ECast, surface.ESubst:
		return nil, nil, fmt.Errorf("an equality former needs its arguments (Eq T l r · refl x · cast A B p x · subst A x y p P px)")
	case surface.ESig, surface.EPair:
		return nil, nil, fmt.Errorf("a Σ former needs its arguments (Sig A B · Pair A B a b)")
	case surface.EFst:
		p, pty, err := e.Infer(c, s.P)
		if err != nil {
			return nil, nil, err
		}
		p, pty = e.insertImplicits(c, p, pty)
		sig, ok := e.M.Force(pty).(core.VSig)
		if !ok {
			return nil, nil, fmt.Errorf("fst (.1) applied to a non-Σ of type %s", e.pretty(c, pty))
		}
		return core.Fst{P: p}, sig.Dom, nil
	case surface.ESnd:
		p, pty, err := e.Infer(c, s.P)
		if err != nil {
			return nil, nil, err
		}
		p, pty = e.insertImplicits(c, p, pty)
		sig, ok := e.M.Force(pty).(core.VSig)
		if !ok {
			return nil, nil, fmt.Errorf("snd (.2) applied to a non-Σ of type %s", e.pretty(c, pty))
		}
		return core.Snd{P: p}, sig.Cod(e.Eval(c, core.Fst{P: p})), nil
	case surface.ECase:
		return nil, nil, &Diagnostic{
			Summary: "I need to know what type this `case` should produce.",
			Body: []string{
				"A `case` becomes a use of the datatype's eliminator, and that needs a motive " +
					"— the result type — which I cannot guess from the branches alone (they may " +
					"each have a different-looking type that only agree up to the scrutinee).",
			},
			Hints: []string{
				"Ascribe the result type: `(case … end : T)`, or put the `case` somewhere its " +
					"expected type is already known (a definition's declared type, a function " +
					"argument position).",
			},
		}
	case surface.EHole:
		// A hole is a fresh meta; its type is another fresh meta.
		tyM := e.freshMeta(c, "type of _")
		tm := e.freshMeta(c, "_")
		return tm, e.Eval(c, tyM), nil
	case surface.EPi:
		m0 := e.pushZero()
		defer e.popMult(m0)
		dom, domSort, err := e.checkType(c, s.Dom)
		if err != nil {
			return nil, nil, err
		}
		vdom := e.Eval(c, dom)
		inner := c.bind(s.Param, vdom)
		cod, codSort, err := e.checkType(inner, s.Cod)
		if err != nil {
			return nil, nil, err
		}
		if err := e.checkBinderUse(inner, core.QMany, s.Param); err != nil {
			return nil, nil, err
		}
		return core.Pi{Icit: s.Icit, Qty: s.Qty, Dom: dom, Cod: core.Scope{Name: s.Param, Body: cod}},
			piSort(domSort, codSort), nil
	case surface.ELam:
		m0 := e.pushZero()
		dom, _, err := e.checkType(c, s.Dom)
		e.popMult(m0)
		if err != nil {
			return nil, nil, err
		}
		vdom := e.Eval(c, dom)
		inner := c.bind(s.Param, vdom)
		body, bodyTy, err := e.Infer(inner, s.Body)
		if err != nil {
			return nil, nil, err
		}
		if err := e.checkBinderUse(inner, s.Qty, s.Param); err != nil {
			return nil, nil, err
		}
		// Close the inferred codomain over the binder by quoting one level up,
		// then re-evaluating it under the argument the Pi closure receives.
		codTm := e.M.Quote(inner.Lvl(), bodyTy)
		env := c.env
		pi := core.VPi{Name: s.Param, Icit: s.Icit, Qty: s.Qty, Dom: vdom, Cod: func(v core.Val) core.Val {
			return e.M.Eval(env.Extend(v), codTm)
		}}
		return core.Lam{Icit: s.Icit, Qty: s.Qty, Body: core.Scope{Name: s.Param, Body: body}}, pi, nil
	case surface.EApp:
		if head, args := surface.SpineOf(x); len(args) > 0 {
			if tm, vty, handled, err := e.elabFormer(c, head, args); handled {
				return tm, vty, err
			}
		}
		fn, fnTy, err := e.Infer(c, s.Fn)
		if err != nil {
			return nil, nil, err
		}
		// An explicit argument first consumes any leading implicit Pis by
		// inserting fresh metas; an implicit argument {e} expects them intact.
		if s.Icit == core.Expl {
			fn, fnTy = e.insertImplicits(c, fn, fnTy)
		}
		pi, ok := e.M.Force(fnTy).(core.VPi)
		if !ok {
			// The function type may itself be a meta: unify it with a fresh Pi.
			var err2 error
			pi, err2 = e.expectPi(c, fnTy, s.Icit)
			if err2 != nil {
				return nil, nil, e.applyNonFunctionError(c, fn, fnTy, err2)
			}
		}
		if pi.Icit != s.Icit {
			return nil, nil, fmt.Errorf("%s application to a function expecting an %s argument",
				icitName(s.Icit), icitName(pi.Icit))
		}
		ma := e.pushMul(pi.Qty)
		arg, err := e.Check(c, s.Arg, pi.Dom)
		e.popMult(ma)
		if err != nil {
			return nil, nil, err
		}
		// The result type needs the argument VALUE only when the Pi is
		// dependent. Skipping the eval otherwise keeps deep application chains
		// (numeral succ-towers) linear: evaluating every argument subterm from
		// scratch at each node is O(n²) in the chain depth.
		var av core.Val
		if !pi.NonDep {
			av = e.Eval(c, arg)
		}
		return core.App{Fn: fn, Arg: arg, Icit: s.Icit}, pi.Cod(av), nil
	case surface.ELet:
		tm, _, inner, err := e.elabLet(c, s)
		if err != nil {
			return nil, nil, err
		}
		body, bodyTy, err := e.Infer(inner, s.Body)
		if err != nil {
			return nil, nil, err
		}
		delete(e.uses, inner.Lvl()-1) // let binders are ω; clear the level
		tm.Body = core.Scope{Name: s.Name, Body: body}
		return tm, bodyTy, nil
	case surface.ESeqBind:
		return e.elabSeqBind(c, s, func(inner *Ctx) (core.Tm, core.Val, error) {
			return e.Infer(inner, s.Body)
		})
	case surface.EAnn:
		ty, _, err := e.checkType(c, s.Ty)
		if err != nil {
			return nil, nil, err
		}
		want := e.Eval(c, ty)
		tm, err := e.Check(c, s.Term, want)
		if err != nil {
			return nil, nil, err
		}
		return core.Ann{Term: tm, Ty: ty}, want, nil
	default:
		return nil, nil, fmt.Errorf("infer: unknown surface expression %T", x)
	}
}

// insertImplicits applies tm to fresh metas while its type is an implicit Pi.
func (e *Elaborator) insertImplicits(c *Ctx, tm core.Tm, ty core.Val) (core.Tm, core.Val) {
	for {
		pi, ok := e.M.Force(ty).(core.VPi)
		if !ok || pi.Icit != core.Impl {
			return tm, ty
		}
		// C2/C2b: if the implicit's domain is a registered typeclass, resolve the
		// dictionary by (recursive) instance search instead of inserting a meta.
		if dict, ok := e.resolveClass(c, pi.Dom); ok {
			tm = core.App{Fn: tm, Arg: dict, Icit: core.Impl}
			ty = pi.Cod(e.Eval(c, dict))
			continue
		}
		m := e.freshMeta(c, "implicit argument")
		// If the domain is a class constraint whose argument head is not yet a
		// rigid type (an unsolved metavariable), POSTPONE: record the dictionary
		// meta and retry instance search once the head is solved (ResolvePending).
		if e.isClassConstraint(c, pi.Dom) {
			e.pending = append(e.pending, pendingDict{meta: m, want: pi.Dom, c: c})
		}
		tm = core.App{Fn: tm, Arg: m, Icit: core.Impl}
		ty = pi.Cod(e.Eval(c, m))
	}
}

// isClassConstraint reports whether dom is shaped like a class application
// `C … T` (a ref-headed application with at least one argument), regardless of
// whether the last argument's head is rigid yet. Decides which inserted
// implicits are worth retrying by instance search once their type is solved.
func (e *Elaborator) isClassConstraint(c *Ctx, dom core.Val) bool {
	t := e.M.Quote(c.Lvl(), dom)
	nargs := 0
	for {
		if app, isApp := t.(core.App); isApp {
			nargs++
			t = app.Fn
			continue
		}
		break
	}
	_, isRef := t.(core.Ref)
	return isRef && nargs > 0
}

// ResolvePending retries every postponed typeclass-dictionary resolution. After
// the value arguments of an overloaded use have solved its inferred type, the
// class argument's head is rigid, so instance search now succeeds; the found
// dictionary is unified with the placeholder meta. The constraint is ZONKED
// first — Quote leaves solved metas folded, so the class argument's head only
// becomes visible to instance search after substituting the solutions. Iterates
// to a fixpoint (one resolution can solve metas that unblock another); left-over
// entries whose head never became rigid stay ordinary unsolved metas (caught by
// ErrUnsolved).
func (e *Elaborator) ResolvePending() error {
	// One sweep resolving every dictionary whose class argument is now rigid.
	resolveDicts := func() (bool, error) {
		prog := false
		var still []pendingDict
		for _, p := range e.pending {
			want := e.zonkVal(p.c, p.want)
			if dict, ok := e.resolveClass(p.c, want); ok {
				if err := e.Unify(p.c.Lvl(), e.Eval(p.c, p.meta), e.Eval(p.c, dict)); err != nil {
					return false, err
				}
				prog = true
			} else {
				still = append(still, p)
			}
		}
		e.pending = still
		return prog, nil
	}
	// One sweep lowering every numeral whose expected type is now rigid (so it
	// injects into the tower — Int, Frac — rather than defaulting to Whole).
	lowerRigid := func() (bool, error) {
		prog := false
		var still []pendingNumeral
		for _, pn := range e.pendingNum {
			if e.isFlexibleMeta(pn.want) {
				still = append(still, pn)
				continue
			}
			tm, err := e.lowerNum(pn.c, pn.s, pn.want)
			if err != nil {
				return false, err
			}
			if err := e.Unify(pn.c.Lvl(), e.Eval(pn.c, pn.meta), e.Eval(pn.c, tm)); err != nil {
				return false, err
			}
			prog = true
		}
		e.pendingNum = still
		return prog, nil
	}
	fixpoint := func() error {
		for {
			d, err := resolveDicts()
			if err != nil {
				return err
			}
			n, err := lowerRigid()
			if err != nil {
				return err
			}
			if !d && !n {
				return nil
			}
		}
	}
	if err := fixpoint(); err != nil {
		return err
	}
	// Stalled: numerals whose type is still flexible must default to Whole — EXCEPT
	// those a pending dictionary will yet determine (the result parameter of an
	// overloaded `-`, say). Default one such numeral, then re-run the fixpoint (the
	// default may pin a dictionary key, which pins another numeral's type, …).
	for len(e.pendingNum) > 0 {
		det := e.dictDeterminedMetas()
		idx := 0
		for i, pn := range e.pendingNum {
			if id, ok := e.flexMetaID(pn.want); ok && det[id] {
				continue // a pending dictionary will determine this numeral's type
			}
			idx = i
			break
		}
		pn := e.pendingNum[idx]
		e.pendingNum = append(e.pendingNum[:idx:idx], e.pendingNum[idx+1:]...)
		tm, err := e.lowerNum(pn.c, pn.s, pn.want) // flexible want defaults to Whole
		if err != nil {
			return err
		}
		if err := e.Unify(pn.c.Lvl(), e.Eval(pn.c, pn.meta), e.Eval(pn.c, tm)); err != nil {
			return err
		}
		if err := fixpoint(); err != nil {
			return err
		}
	}
	// Any dictionaries left have keys that never became rigid: their metas surface
	// as unsolved (ErrUnsolved). A final sweep resolves whatever became resolvable.
	for {
		d, err := resolveDicts()
		if err != nil {
			return err
		}
		if !d {
			return nil
		}
	}
}

// zonkVal substitutes solved metas into a value (Quote folds them, Zonk replaces
// them, Eval rebuilds), exposing a now-rigid head for instance search.
func (e *Elaborator) zonkVal(c *Ctx, v core.Val) core.Val {
	return e.Eval(c, e.Zonk(c.Lvl(), e.M.Quote(c.Lvl(), v)))
}

// flexMetaID returns the unsolved metavariable heading v, if any.
func (e *Elaborator) flexMetaID(v core.Val) (int, bool) {
	id, _, ok := flexHead(e.M.Force(v))
	if !ok {
		return 0, false
	}
	if _, solved := e.metas.Solution(id); solved {
		return 0, false
	}
	return id, true
}

// dictDeterminedMetas collects the metavariables that a pending dictionary will
// SOLVE when it resolves: every meta in a class constraint except the one heading
// its last argument (the search KEY, which must be solved from elsewhere — e.g. a
// value argument). A numeral whose type is such a meta must NOT be defaulted: its
// type is the result the instance fixes (the promoted Int of an overloaded `-`).
func (e *Elaborator) dictDeterminedMetas() map[int]bool {
	det := map[int]bool{}
	for _, p := range e.pending {
		t := e.M.Quote(p.c.Lvl(), e.zonkVal(p.c, p.want))
		var args []core.Tm
		for {
			if app, isApp := t.(core.App); isApp {
				args = append([]core.Tm{app.Arg}, args...)
				t = app.Fn
				continue
			}
			break
		}
		if _, isRef := t.(core.Ref); !isRef || len(args) == 0 {
			continue
		}
		key := -1
		last := args[len(args)-1]
		if m, ok := last.(core.Meta); ok {
			key = m.ID
		}
		for _, a := range args {
			for _, id := range collectMetaIDs(a) {
				if id != key {
					det[id] = true
				}
			}
		}
	}
	return det
}

// collectMetaIDs gathers metavariable ids occurring in a term (App spines and
// their arguments — enough for the ref-headed class-constraint shapes here).
func collectMetaIDs(t core.Tm) []int {
	switch x := t.(type) {
	case core.Meta:
		return []int{x.ID}
	case core.App:
		return append(collectMetaIDs(x.Fn), collectMetaIDs(x.Arg)...)
	default:
		return nil
	}
}

// classKey extracts the (class-former, last-argument-head) hashes from a class
// application type value `C … T` (e.g. Monoid Nat → (Monoid, Nat)), the key the
// instance table is indexed by. Returns ok=false for any non-(ref-headed) type,
// so ordinary implicits (domain U, a variable, …) fall through to a meta.
func (e *Elaborator) classKey(c *Ctx, dom core.Val) (class, arg core.Hash, ok bool) {
	t := e.M.Quote(c.Lvl(), dom)
	var args []core.Tm
	for {
		if app, isApp := t.(core.App); isApp {
			args = append([]core.Tm{app.Arg}, args...)
			t = app.Fn
			continue
		}
		break
	}
	ref, isRef := t.(core.Ref)
	if !isRef || len(args) == 0 {
		return core.Hash{}, core.Hash{}, false
	}
	// head of the last argument (the indexing type).
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

// resolveClass resolves a typeclass dictionary for the constraint type `want`
// (e.g. `Eq (List Nat)`) by instance search (C2/C2b). It looks up the instance
// registered under (class-former, last-argument-head), then drives that
// instance through its OWN leading implicits: type/value parameters (domain a
// sort) become metas solved by unifying the instance's codomain with `want`,
// and constraint premises (domain a class application, e.g. `Eq A`) are
// discharged by RECURSIVE search once that unification has fixed the parameters.
// So `instance {A} -> Eq A -> Eq (List A)` resolves `Eq (List Nat)` to
// `listEq Nat eqNat`, building the dictionary bottom-up. The non-parametric case
// (an instance with no leading implicits, e.g. `Eq Nat`) falls straight through:
// no premises, the codomain IS the instance type, unify-and-return. Overlap /
// priority and cycle detection stay parked (no consumer); first registered
// instance per key wins. Returns ok=false when `want` is not a class application
// or no instance resolves, so ordinary implicits fall through to a metavariable.
func (e *Elaborator) resolveClass(c *Ctx, want core.Val) (core.Tm, bool) {
	if e.InstanceFor == nil {
		return nil, false
	}
	class, arg, ok := e.classKey(c, want)
	if !ok {
		return nil, false
	}
	instTm, found := e.InstanceFor(class, arg)
	if !found {
		return nil, false
	}
	ref, isRef := instTm.(core.Ref)
	if !isRef {
		return nil, false
	}
	instTy, err := e.refType(ref.Hash)
	if err != nil {
		return nil, false
	}
	tm := core.Tm(ref)
	ty := instTy
	// Insert a meta for each leading implicit; record the constraint premises
	// (domain not a sort) to discharge after the parameters are fixed.
	var premiseMetas []core.Tm
	var premiseDoms []core.Val
	for {
		pi, ok := e.M.Force(ty).(core.VPi)
		if !ok || pi.Icit != core.Impl {
			break
		}
		m := e.freshMeta(c, "instance argument")
		if !isSortVal(e.M.Force(pi.Dom)) {
			premiseMetas = append(premiseMetas, m)
			premiseDoms = append(premiseDoms, pi.Dom)
		}
		tm = core.App{Fn: tm, Arg: m, Icit: core.Impl}
		ty = pi.Cod(e.Eval(c, m))
	}
	// Fix the type/value parameters: the instance codomain must be `want`.
	if e.Unify(c.Lvl(), ty, want) != nil {
		return nil, false
	}
	// Discharge each premise recursively on its now-concrete type, solving the
	// premise's meta to the resolved dictionary.
	for i, dom := range premiseDoms {
		// Re-zonk: the codomain unification solved the parameter metas, so the
		// premise type (e.g. `Self ?A`) must have them substituted (`Self Bool`)
		// before its class key is read — Quote alone does not replace solved metas.
		concrete := e.Eval(c, e.Zonk(c.Lvl(), e.M.Quote(c.Lvl(), dom)))
		dict, ok := e.resolveClass(c, concrete)
		if !ok {
			return nil, false
		}
		if e.Unify(c.Lvl(), e.Eval(c, premiseMetas[i]), e.Eval(c, dict)) != nil {
			return nil, false
		}
	}
	return tm, true
}

// isSortVal reports whether v is a universe or Prop — the domains of an
// instance's type/value PARAMETERS, as opposed to its constraint premises.
func isSortVal(v core.Val) bool {
	switch v.(type) {
	case core.VU, core.VProp:
		return true
	default:
		return false
	}
}

// expectPi unifies a (typically meta-headed) type with a fresh Pi of the given
// plicity, so application can proceed.
func (e *Elaborator) expectPi(c *Ctx, ty core.Val, icit core.Icit) (core.VPi, error) {
	domM := e.freshMeta(c, "domain")
	vdom := e.Eval(c, domM)
	inner := c.bind("x", vdom)
	codM := e.freshMeta(inner, "codomain")
	env := c.env
	em := e.M
	pi := core.VPi{Name: "x", Icit: icit, Dom: vdom, Cod: func(v core.Val) core.Val {
		return em.Eval(env.Extend(v), codM)
	}}
	if err := e.Unify(c.Lvl(), ty, pi); err != nil {
		return core.VPi{}, err
	}
	return pi, nil
}

// Check elaborates e against the expected type want.
func (e *Elaborator) Check(c *Ctx, x surface.Exp, want core.Val) (core.Tm, error) {
	fw := e.M.Force(want)

	// If the expected type is an implicit Pi and the term is not itself an
	// implicit lambda, insert an implicit lambda binder.
	if pi, ok := fw.(core.VPi); ok && pi.Icit == core.Impl {
		if lam, isLam := x.(surface.ELam); !isLam || lam.Icit != core.Impl {
			inner := c.bind(pi.Name, pi.Dom)
			body, err := e.Check(inner, x, pi.Cod(core.VVar(c.Lvl())))
			if err != nil {
				return nil, err
			}
			if err := e.checkBinderUse(inner, pi.Qty, pi.Name); err != nil {
				return nil, err
			}
			name := pi.Name
			if name == "" {
				name = "x"
			}
			return core.Lam{Icit: core.Impl, Qty: pi.Qty, Body: core.Scope{Name: name, Body: body}}, nil
		}
	}

	switch s := x.(type) {
	case surface.EHole:
		return e.freshMeta(c, "_"), nil
	case surface.ECase:
		return e.elabCase(c, s, fw)
	case surface.ERefl:
		// Bare refl in checking position: the expected type must be (or reduce
		// to) an equality with convertible endpoints — including the pointwise
		// expansion of a function equality, handled by reflProof's recursion
		// arriving here under binders.
		eq, ok := fw.(core.VEq)
		if !ok {
			return nil, e.reflNonEqError(c, want)
		}
		if !e.M.Conv(c.Lvl(), eq.L, eq.R) {
			return nil, e.reflMismatchError(c, eq)
		}
		return core.Refl{Tm: e.M.Quote(c.Lvl(), eq.L)}, nil
	case surface.ELam:
		pi, ok := fw.(core.VPi)
		if !ok {
			// Checking a lambda against a flexible type: unify with a fresh Pi.
			var err error
			pi, err = e.expectPi(c, want, s.Icit)
			if err != nil {
				return nil, e.lambdaNonFunctionError(c, want)
			}
		}
		if pi.Icit != s.Icit {
			return nil, fmt.Errorf("%s lambda checked against an %s function type",
				icitName(s.Icit), icitName(pi.Icit))
		}
		// An unannotated binder adopts the expected quantity; an explicit 0/1
		// must match it.
		qty := s.Qty
		if qty == core.QMany {
			qty = pi.Qty
		} else if qty != pi.Qty {
			return nil, fmt.Errorf("binder %s annotated quantity %s, but the function type expects %s",
				s.Param, qtyName(s.Qty), qtyName(pi.Qty))
		}
		// The annotation must agree with the expected domain.
		m0 := e.pushZero()
		dom, _, err := e.checkType(c, s.Dom)
		e.popMult(m0)
		if err != nil {
			return nil, err
		}
		vdom := e.Eval(c, dom)
		if err := e.Unify(c.Lvl(), vdom, pi.Dom); err != nil {
			return nil, fmt.Errorf("binder %s annotated %s, but the expected domain is %s",
				s.Param, e.pretty(c, vdom), e.pretty(c, pi.Dom))
		}
		inner := c.bind(s.Param, pi.Dom)
		body, err := e.Check(inner, s.Body, pi.Cod(core.VVar(c.Lvl())))
		if err != nil {
			return nil, err
		}
		if err := e.checkBinderUse(inner, qty, s.Param); err != nil {
			return nil, err
		}
		return core.Lam{Icit: s.Icit, Qty: qty, Body: core.Scope{Name: s.Param, Body: body}}, nil
	case surface.ENum:
		return e.elabNum(c, s, fw)
	case surface.ELet:
		tm, _, inner, err := e.elabLet(c, s)
		if err != nil {
			return nil, err
		}
		body, err := e.Check(inner, s.Body, want)
		if err != nil {
			return nil, err
		}
		delete(e.uses, inner.Lvl()-1) // let binders are ω; clear the level
		tm.Body = core.Scope{Name: s.Name, Body: body}
		return tm, nil
	case surface.ESeqBind:
		tm, _, err := e.elabSeqBind(c, s, func(inner *Ctx) (core.Tm, core.Val, error) {
			body, err := e.Check(inner, s.Body, want)
			if err != nil {
				return nil, nil, err
			}
			return body, want, nil
		})
		if err != nil {
			return nil, err
		}
		return tm, nil
	default:
		tm, got, err := e.Infer(c, x)
		if err != nil {
			return nil, err
		}
		// Insert any leading implicits the inferred type still carries, unless
		// the expected type wants them.
		if pi, ok := fw.(core.VPi); !ok || pi.Icit == core.Expl {
			tm, got = e.insertImplicits(c, tm, got)
		}
		if err := e.Unify(c.Lvl(), got, want); err != nil {
			if e.M.Sub(c.Lvl(), got, want) {
				return tm, nil // cumulativity: Prop <: U
			}
			return nil, e.typeMismatchError(c, want, got, err)
		}
		return tm, nil
	}
}

// elabNum lowers a numeral literal to a compressed core numeral (NatLit) and
// unifies its `builtin nat` type against the expectation, so a genuine mismatch
// is still reported. (The old binary-literal lowering — a numeral checked at a
// `builtin bin` Pos/BN type — is retired; C7 / R-NUM, Decision 5.)
func (e *Elaborator) elabNum(c *Ctx, s surface.ENum, want core.Val) (core.Tm, error) {
	// If the expected type is still a flexible metavariable, POSTPONE the lowering:
	// committing now would default it to Whole (unify natTy with the meta), but the
	// surrounding elaboration may yet pin it to a tower type — e.g. `1 / (3 * 2)`,
	// where the fraction builder pushes Frac into the product only after the
	// numerals are seen. Record a placeholder meta and lower it in ResolvePending
	// once the type is known (defaulting to Whole if it never gets pinned).
	if e.isFlexibleMeta(want) {
		m := e.freshMeta(c, "numeral")
		e.pendingNum = append(e.pendingNum, pendingNumeral{meta: m, want: want, s: s, c: c})
		return m, nil
	}
	return e.lowerNum(c, s, want)
}

// isFlexibleMeta reports whether v is headed by an unsolved metavariable.
func (e *Elaborator) isFlexibleMeta(v core.Val) bool {
	id, _, ok := flexHead(e.M.Force(v))
	if !ok {
		return false
	}
	_, solved := e.metas.Solution(id)
	return !solved
}

// lowerNum lowers a numeral against a rigid (non-metavariable) expected type: the
// `builtin nat` default, or a registered tower injection's codomain (Z, Frac …).
func (e *Elaborator) lowerNum(c *Ctx, s surface.ENum, want core.Val) (core.Tm, error) {
	tm, err := e.Num.Nat(s.Val)
	if err != nil {
		return nil, err
	}
	natTy, err := e.natType()
	if err != nil {
		return nil, err
	}
	// Base case: a Nat (or a flexible expectation, which unify solves to Nat — the
	// established numeral default).
	if e.Unify(c.Lvl(), natTy, want) == nil {
		return tm, nil
	}
	// Typed injection (numeric-tower rung C4): a numeral checked at a registered
	// `Nat -> T` injection's codomain (the integers Z, the rationals Rat) lowers to
	// `inj (NatLit n)`. The inner NatLit is a genuine Nat (bignum + accel intact);
	// the injection carries it into the tower type. Dispatch is by `want`'s rigid
	// head — the base unify above already consumed the flexible/Nat case.
	for _, inj := range e.Num.Injs {
		injTy, err := e.refType(inj.Inj)
		if err != nil {
			continue
		}
		pi, ok := e.M.Force(injTy).(core.VPi)
		if !ok {
			continue
		}
		cod := pi.Cod(e.M.Eval(nil, tm)) // Nat -> T is non-dependent; arg is ignored
		if e.Unify(c.Lvl(), cod, want) == nil {
			return core.App{Fn: core.Ref{Hash: inj.Inj}, Arg: tm, Icit: core.Expl}, nil
		}
	}
	return nil, fmt.Errorf("numeral %d is a %s, but %s was expected",
		s.Val, e.pretty(c, natTy), e.pretty(c, want))
}

// natType is the type a unary numeral inhabits — the type of the registered
// `builtin nat` zero constructor.
func (e *Elaborator) natType() (core.Val, error) {
	if !e.Num.HasNat {
		return nil, fmt.Errorf("a numeral has no meaning here: no `builtin nat` is in scope")
	}
	return e.refType(e.Num.NatZero)
}

// checkType elaborates e as a TYPE: its type must be a sort (U or Prop). A
// flexible type defaults to U by unification.
func (e *Elaborator) checkType(c *Ctx, x surface.Exp) (core.Tm, core.Val, error) {
	m0 := e.pushZero()
	defer e.popMult(m0)
	tm, got, err := e.Infer(c, x)
	if err != nil {
		return nil, nil, err
	}
	tm2, got2 := e.insertImplicits(c, tm, got)
	switch sort := e.M.Force(got2).(type) {
	case core.VU:
		return tm2, sort, nil
	case core.VProp:
		return tm2, core.VProp{}, nil
	default:
		if err := e.Unify(c.Lvl(), got2, core.VU{}); err != nil {
			return nil, nil, &Diagnostic{
				Summary: "A type was expected here, but this is a value.",
				Body: []string{
					"`" + e.prettyTm(tm2) + "` has type `" + e.pretty(c, got2) + "`, so it is " +
						"an ordinary value — but this position needs a TYPE (something in `U` or " +
						"`Prop`), the kind of thing that can stand to the right of a `:`.",
				},
				Hints: []string{
					"Did you mean a type with a similar name, or to apply this to arguments " +
						"until it becomes one? A type is a member of `U`; `" + e.prettyTm(tm2) +
						"` is a member of `" + e.pretty(c, got2) + "`.",
				},
			}
		}
		return tm2, core.VU{}, nil
	}
}

// reflProof builds the proof of Eq ty x x, eta-expanding through function
// types: funext computes, so an equality at (x : A) -> B IS the pointwise
// equality function, and its canonical proof is a lambda of refls.
func (e *Elaborator) reflProof(c *Ctx, ty core.Val, x core.Tm) core.Tm {
	if pi, ok := e.M.Force(ty).(core.VPi); ok {
		inner := c.bind(pi.Name, pi.Dom)
		v := core.VVar(c.Lvl())
		body := e.reflProof(inner, pi.Cod(v),
			core.App{Fn: weaken(x), Arg: core.Var{Idx: 0}, Icit: pi.Icit})
		name := pi.Name
		if name == "" {
			name = "x"
		}
		return core.Lam{Icit: pi.Icit, Body: core.Scope{Name: name, Body: body}}
	}
	return core.Refl{Tm: x}
}

// weaken shifts a term's free de Bruijn indices up by one (it is used under
// exactly one new binder). Implemented by quote∘eval would normalize; the
// simple structural shift keeps the term intact.
func weaken(t core.Tm) core.Tm { return shift(t, 0, 1) }

func shift(t core.Tm, cutoff, by int) core.Tm {
	switch tm := t.(type) {
	case core.Var:
		if tm.Idx >= cutoff {
			return core.Var{Idx: tm.Idx + by}
		}
		return tm
	case core.Ref, core.Univ, core.Prop, core.Meta:
		return t
	case core.Pi:
		return core.Pi{Icit: tm.Icit, Qty: tm.Qty, Dom: shift(tm.Dom, cutoff, by),
			Cod: core.Scope{Name: tm.Cod.Name, Body: shift(tm.Cod.Body, cutoff+1, by)}}
	case core.Lam:
		return core.Lam{Icit: tm.Icit, Qty: tm.Qty,
			Body: core.Scope{Name: tm.Body.Name, Body: shift(tm.Body.Body, cutoff+1, by)}}
	case core.App:
		return core.App{Fn: shift(tm.Fn, cutoff, by), Arg: shift(tm.Arg, cutoff, by), Icit: tm.Icit}
	case core.Let:
		var ty core.Tm
		if tm.Ty != nil {
			ty = shift(tm.Ty, cutoff, by)
		}
		return core.Let{Ty: ty, Val: shift(tm.Val, cutoff, by),
			Body: core.Scope{Name: tm.Body.Name, Body: shift(tm.Body.Body, cutoff+1, by)}}
	case core.Ann:
		return core.Ann{Term: shift(tm.Term, cutoff, by), Ty: shift(tm.Ty, cutoff, by)}
	case core.Eq:
		return core.Eq{Ty: shift(tm.Ty, cutoff, by), L: shift(tm.L, cutoff, by), R: shift(tm.R, cutoff, by)}
	case core.Refl:
		return core.Refl{Tm: shift(tm.Tm, cutoff, by)}
	case core.Cast:
		return core.Cast{A: shift(tm.A, cutoff, by), B: shift(tm.B, cutoff, by),
			P: shift(tm.P, cutoff, by), X: shift(tm.X, cutoff, by)}
	case core.Subst:
		return core.Subst{A: shift(tm.A, cutoff, by), X: shift(tm.X, cutoff, by),
			Y: shift(tm.Y, cutoff, by), Prf: shift(tm.Prf, cutoff, by),
			P: shift(tm.P, cutoff, by), Px: shift(tm.Px, cutoff, by)}
	case core.Sig:
		return core.Sig{Qty: tm.Qty, Dom: shift(tm.Dom, cutoff, by),
			Cod: core.Scope{Name: tm.Cod.Name, Body: shift(tm.Cod.Body, cutoff+1, by)}}
	case core.Pair:
		return core.Pair{Dom: shift(tm.Dom, cutoff, by),
			Cod: core.Scope{Name: tm.Cod.Name, Body: shift(tm.Cod.Body, cutoff+1, by)},
			A:   shift(tm.A, cutoff, by), B: shift(tm.B, cutoff, by)}
	case core.Fst:
		return core.Fst{P: shift(tm.P, cutoff, by)}
	case core.Snd:
		return core.Snd{P: shift(tm.P, cutoff, by)}
	case core.NatLit:
		// A compressed numeral binds and mentions no variable: shift is identity.
		return tm
	default:
		panic(fmt.Sprintf("shift: unknown core term %T", t))
	}
}

// elabFormer elaborates a saturated equality-former application spine, or
// reports that the expression is not one (handled == false).
func (e *Elaborator) elabFormer(c *Ctx, head surface.Exp, args []surface.Exp) (core.Tm, core.Val, bool, error) {
	switch head.(type) {
	case surface.EEq:
		if len(args) != 3 {
			return nil, nil, true, fmt.Errorf("Eq takes exactly 3 arguments (Eq T l r), got %d", len(args))
		}
		m0 := e.pushZero()
		defer e.popMult(m0)
		ty, _, err := e.checkType(c, args[0])
		if err != nil {
			return nil, nil, true, err
		}
		vty := e.Eval(c, ty)
		l, err := e.Check(c, args[1], vty)
		if err != nil {
			return nil, nil, true, err
		}
		r, err := e.Check(c, args[2], vty)
		if err != nil {
			return nil, nil, true, err
		}
		return core.Eq{Ty: ty, L: l, R: r}, core.VProp{}, true, nil
	case surface.ERefl:
		if len(args) != 1 {
			return nil, nil, true, fmt.Errorf("refl takes exactly 1 argument here, got %d", len(args))
		}
		m0 := e.pushZero()
		defer e.popMult(m0)
		x, xty, err := e.Infer(c, args[0])
		if err != nil {
			return nil, nil, true, err
		}
		x, xty = e.insertImplicits(c, x, xty)
		vx := e.Eval(c, x)
		proof := e.reflProof(c, xty, x)
		return proof, e.M.EvalEq(xty, vx, vx), true, nil
	case surface.ESubst:
		if len(args) != 6 {
			return nil, nil, true, fmt.Errorf("subst takes exactly 6 arguments (subst A x y p P px), got %d", len(args))
		}
		a, _, err := e.checkType(c, args[0])
		if err != nil {
			return nil, nil, true, err
		}
		va := e.Eval(c, a)
		mz := e.pushZero()
		xx, err := e.Check(c, args[1], va)
		if err != nil {
			e.popMult(mz)
			return nil, nil, true, err
		}
		yy, err := e.Check(c, args[2], va)
		if err != nil {
			e.popMult(mz)
			return nil, nil, true, err
		}
		vx, vy := e.Eval(c, xx), e.Eval(c, yy)
		pr, err := e.Check(c, args[3], e.M.EvalEq(va, vx, vy))
		if err != nil {
			e.popMult(mz)
			return nil, nil, true, err
		}
		motiveTy := core.VPi{Name: "z", Dom: va, Cod: func(core.Val) core.Val { return core.VU{} }}
		pm, err := e.Check(c, args[4], motiveTy)
		e.popMult(mz)
		if err != nil {
			return nil, nil, true, err
		}
		vp := e.Eval(c, pm)
		px, err := e.Check(c, args[5], e.M.Apply(vp, vx))
		if err != nil {
			return nil, nil, true, err
		}
		return core.Subst{A: a, X: xx, Y: yy, Prf: pr, P: pm, Px: px}, e.M.Apply(vp, vy), true, nil
	case surface.ECast:
		if len(args) != 4 {
			return nil, nil, true, fmt.Errorf("cast takes exactly 4 arguments (cast A B p x), got %d", len(args))
		}
		a, sa, err := e.checkType(c, args[0])
		if err != nil {
			return nil, nil, true, err
		}
		b, sb, err := e.checkType(c, args[1])
		if err != nil {
			return nil, nil, true, err
		}
		if !e.M.Conv(c.Lvl(), sa, sb) {
			return nil, nil, true, fmt.Errorf("cast endpoints live in different sorts: %s vs %s",
				e.pretty(c, sa), e.pretty(c, sb))
		}
		va, vb := e.Eval(c, a), e.Eval(c, b)
		mp := e.pushZero()
		pr, err := e.Check(c, args[2], core.VEq{Ty: sa, L: va, R: vb})
		e.popMult(mp)
		if err != nil {
			return nil, nil, true, err
		}
		x, err := e.Check(c, args[3], va)
		if err != nil {
			return nil, nil, true, err
		}
		return core.Cast{A: a, B: b, P: pr, X: x}, vb, true, nil
	case surface.ESig:
		if len(args) != 2 {
			return nil, nil, true, fmt.Errorf("Sig takes exactly 2 arguments (Sig A B), got %d", len(args))
		}
		m0 := e.pushZero()
		defer e.popMult(m0)
		a, sa, err := e.checkType(c, args[0])
		if err != nil {
			return nil, nil, true, err
		}
		cod, codSort, err := e.elabFamily(c, e.Eval(c, a), args[1], "Sig")
		if err != nil {
			return nil, nil, true, err
		}
		return core.Sig{Dom: a, Cod: cod}, sigSort(sa, codSort), true, nil
	case surface.EPair:
		if len(args) != 4 {
			return nil, nil, true, fmt.Errorf("pair takes exactly 4 arguments (pair A B a b), got %d", len(args))
		}
		a, _, err := e.checkType(c, args[0])
		if err != nil {
			return nil, nil, true, err
		}
		cod, _, err := e.elabFamily(c, e.Eval(c, a), args[1], "pair")
		if err != nil {
			return nil, nil, true, err
		}
		vsig, ok := e.Eval(c, core.Sig{Dom: a, Cod: cod}).(core.VSig)
		if !ok {
			return nil, nil, true, fmt.Errorf("pair: internal — Σ did not evaluate to a Σ value")
		}
		av, err := e.Check(c, args[2], vsig.Dom)
		if err != nil {
			return nil, nil, true, err
		}
		bv, err := e.Check(c, args[3], vsig.Cod(e.Eval(c, av)))
		if err != nil {
			return nil, nil, true, err
		}
		return core.Pair{Dom: a, Cod: cod, A: av, B: bv}, vsig, true, nil
	}
	return nil, nil, false, nil
}

// elabFamily elaborates a Σ family argument (a function literal `fn (x:A) is B
// end`) and returns its body as a Scope (for a core Sig/Pair Cod) and the
// codomain sort. The family's parameter is re-bound at the REAL domain vA —
// the lambda's own (printed-as-U) annotation is ignored — so dependent families
// type-check correctly (this mirrors EPi's codomain elaboration).
func (e *Elaborator) elabFamily(c *Ctx, vA core.Val, fam surface.Exp, who string) (core.Scope, core.Val, error) {
	lam, ok := fam.(surface.ELam)
	if !ok {
		return core.Scope{}, nil, fmt.Errorf("%s's family must be a function literal: %s A (fn (x : A) is B end)", who, who)
	}
	inner := c.bind(lam.Param, vA)
	cod, codSort, err := e.checkType(inner, lam.Body)
	if err != nil {
		return core.Scope{}, nil, err
	}
	return core.Scope{Name: lam.Param, Body: cod}, codSort, nil
}

// sigSort gives the universe of Σ (x : A), B: the max of the two component
// levels (Σ always lands in a U; it is never impredicatively a Prop here).
func sigSort(a, b core.Val) core.Val {
	la, lb := 0, 0
	if u, ok := a.(core.VU); ok {
		la = u.Lvl
	}
	if u, ok := b.(core.VU); ok {
		lb = u.Lvl
	}
	if lb > la {
		return core.VU{Lvl: lb}
	}
	return core.VU{Lvl: la}
}

// elabLet elaborates the binding part of a let (annotation and value), returning
// the partially-built core Let (Body unset), the binder's type, and the extended
// context in which to elaborate the body.
func (e *Elaborator) elabLet(c *Ctx, s surface.ELet) (core.Let, core.Val, *Ctx, error) {
	var tyTm core.Tm
	var vty core.Val
	var val core.Tm
	if s.Ty != nil {
		t, _, err := e.checkType(c, s.Ty)
		if err != nil {
			return core.Let{}, nil, nil, err
		}
		tyTm = t
		vty = e.Eval(c, t)
		v, err := e.Check(c, s.Val, vty)
		if err != nil {
			return core.Let{}, nil, nil, err
		}
		val = v
	} else {
		v, ty, err := e.Infer(c, s.Val)
		if err != nil {
			return core.Let{}, nil, nil, err
		}
		val, vty = v, ty
	}
	inner := c.define(s.Name, vty, e.Eval(c, val))
	return core.Let{Ty: tyTm, Val: val}, vty, inner, nil
}

// ioArg reports whether ty (a type value) is headed by the IO former, returning
// the argument A from `IO A`. The IO former's hash is e.Refs["IO"], registered
// ambiently by every session (store.AddIO). The type value is forced first so a
// glued/neutral IO application is seen through; a non-IO type returns ok=false.
func (e *Elaborator) ioArg(ty core.Val) (a core.Val, ok bool) {
	ioHash, hasIO := e.Refs["IO"]
	if !hasIO {
		return nil, false
	}
	forced := e.M.Force(ty)
	neu, isNeu := forced.(core.VNeu)
	if !isNeu {
		return nil, false
	}
	app, isApp := neu.Spine.(core.NApp)
	if !isApp {
		return nil, false
	}
	ref, isRef := app.Fn.(core.NRef)
	if !isRef || ref.Hash != ioHash {
		return nil, false
	}
	return app.Arg, true
}

// elabSeqBind elaborates a seq-origin binding (surface.ESeqBind) TYPE-AWARELY.
// It infers the binding's value, honours an optional annotation, then:
//   - if the value's type is IO A, lowers the binding to a bindIO application
//     `bindIO A B Val (fn Name is Body end)` (an ORDERED effect) where B is the
//     body's inferred type; the body is elaborated under a fresh binder Name : A;
//   - otherwise lowers exactly like a plain let (a lazy value binding).
//
// elabBody elaborates the seq body in the given extended context (it is the
// caller's Infer/Check closure so the same routine serves both directions). It
// returns the body's core term and its type. The returned term is the whole
// binding's core; bodyTy is its type.
func (e *Elaborator) elabSeqBind(c *Ctx, s surface.ESeqBind, elabBody func(inner *Ctx) (core.Tm, core.Val, error)) (core.Tm, core.Val, error) {
	var val core.Tm
	var valTy core.Val
	if s.Ty != nil {
		// With an annotation, CHECK the value against it (as elabLet does), so a
		// checking-only value form (a bare `case`, which needs its motive from the
		// expected type) elaborates. The annotation is the binding's type.
		tyTm, _, err := e.checkType(c, s.Ty)
		if err != nil {
			return nil, nil, err
		}
		valTy = e.Eval(c, tyTm)
		v, err := e.Check(c, s.Val, valTy)
		if err != nil {
			return nil, nil, err
		}
		val = v
	} else {
		v, ty, err := e.Infer(c, s.Val)
		if err != nil {
			return nil, nil, err
		}
		val, valTy = v, ty
	}
	// IO item: lower to bindIO so the effect is sequenced.
	if aTy, ok := e.ioArg(valTy); ok {
		inner := c.bind(s.Name, aTy)
		bodyTm, bodyTy, err := elabBody(inner)
		if err != nil {
			return nil, nil, err
		}
		delete(e.uses, inner.Lvl()-1) // the bindIO continuation Pi is ω; clear the level
		aTyTm := e.M.Quote(c.Lvl(), aTy)
		bTyTm := e.M.Quote(c.Lvl(), bodyTy)
		bindIOHash := e.Refs["bindIO"]
		result := core.App{
			Fn: core.App{
				Fn: core.App{
					Fn:   core.App{Fn: core.Ref{Hash: bindIOHash}, Arg: aTyTm, Icit: core.Expl},
					Arg:  bTyTm,
					Icit: core.Expl,
				},
				Arg:  val,
				Icit: core.Expl,
			},
			Arg: core.Lam{
				Icit: core.Expl,
				Qty:  core.QMany,
				Body: core.Scope{Name: s.Name, Body: bodyTm},
			},
			Icit: core.Expl,
		}
		return result, bodyTy, nil
	}
	// Pure item: a plain (lazy) let binding.
	inner := c.define(s.Name, valTy, e.Eval(c, val))
	bodyTm, bodyTy, err := elabBody(inner)
	if err != nil {
		return nil, nil, err
	}
	delete(e.uses, inner.Lvl()-1) // let binders are ω; clear the level
	let := core.Let{Val: val, Body: core.Scope{Name: s.Name, Body: bodyTm}}
	return let, bodyTy, nil
}

// ElabDef elaborates one surface definition (type and body) to core, ZONKED and
// meta-free: every metavariable the run created must have been solved.
func (e *Elaborator) ElabDef(d surface.Def) (ty, body core.Tm, err error) {
	c := &Ctx{}
	if d.Ty == nil {
		return nil, nil, fmt.Errorf("%s: definition has no type", d.Name)
	}
	ty, _, err = e.checkType(c, d.Ty)
	if err != nil {
		return nil, nil, fmt.Errorf("%s: %w", d.Name, err)
	}
	body, err = e.Check(c, d.Body, e.Eval(c, ty))
	if err != nil {
		return nil, nil, fmt.Errorf("%s: %w", d.Name, err)
	}
	if err := e.ResolvePending(); err != nil {
		return nil, nil, fmt.Errorf("%s: %w", d.Name, err)
	}
	ty, body = e.Zonk(0, ty), e.Zonk(0, body)
	if err := e.ErrUnsolved(d.Name); err != nil {
		return nil, nil, err
	}
	if !MetaFree(ty) || !MetaFree(body) {
		return nil, nil, fmt.Errorf("%s: internal: metavariable survived zonking", d.Name)
	}
	return ty, body, nil
}

// ElabForeign elaborates a `foreign` axiom's type (R-FFI / B4): there is no
// body — the type is the contract, checked and returned zonked and meta-free.
func (e *Elaborator) ElabForeign(d surface.Def) (core.Tm, error) {
	c := &Ctx{}
	if d.Ty == nil {
		return nil, fmt.Errorf("%s: foreign declaration has no type", d.Name)
	}
	ty, _, err := e.checkType(c, d.Ty)
	if err != nil {
		return nil, fmt.Errorf("%s: %w", d.Name, err)
	}
	ty = e.Zonk(0, ty)
	if err := e.ErrUnsolved(d.Name); err != nil {
		return nil, err
	}
	if !MetaFree(ty) {
		return nil, fmt.Errorf("%s: internal: metavariable survived zonking", d.Name)
	}
	return ty, nil
}

// ElabPartialDef elaborates a `partial` (general-recursive) definition (C4). The
// definition's own name is in scope in its body, resolving to selfHash with the
// declared type — a neutral self-reference. The caller hashes the result over
// selfHash and substitutes the real content hash, marking it partial (so the
// evaluator keeps the head neutral; the body runs only through codegen).
func (e *Elaborator) ElabPartialDef(d surface.Def, selfHash core.Hash) (ty, body core.Tm, err error) {
	c := &Ctx{}
	if d.Ty == nil {
		return nil, nil, fmt.Errorf("%s: definition has no type", d.Name)
	}
	ty, _, err = e.checkType(c, d.Ty)
	if err != nil {
		return nil, nil, fmt.Errorf("%s: %w", d.Name, err)
	}
	e.SelfHash = selfHash
	e.SelfType = ty
	e.Refs[d.Name] = selfHash
	body, err = e.Check(c, d.Body, e.Eval(c, ty))
	if err != nil {
		return nil, nil, fmt.Errorf("%s: %w", d.Name, err)
	}
	if err := e.ResolvePending(); err != nil {
		return nil, nil, fmt.Errorf("%s: %w", d.Name, err)
	}
	ty, body = e.Zonk(0, ty), e.Zonk(0, body)
	if err := e.ErrUnsolved(d.Name); err != nil {
		return nil, nil, err
	}
	if !MetaFree(ty) || !MetaFree(body) {
		return nil, nil, fmt.Errorf("%s: internal: metavariable survived zonking", d.Name)
	}
	return ty, body, nil
}

// ElabPartialGroup elaborates a MUTUALLY-recursive `partial` group (mutual general
// recursion). Every member's name is in scope in every member's body, resolving to
// Placeholder(i) with the declared type — neutral, bodiless self/sibling references.
// Types are elaborated first (a member's body may need a sibling's type); then every
// body is checked against the shared overlay. The caller (store.AddPartialGroup)
// SCC-hashes the result and substitutes the real content hashes for the placeholders.
func (e *Elaborator) ElabPartialGroup(ds []surface.Def) (tys, bodies []core.Tm, err error) {
	n := len(ds)
	tys = make([]core.Tm, n)
	for i, d := range ds {
		if d.Ty == nil {
			return nil, nil, fmt.Errorf("%s: definition has no type", d.Name)
		}
		c := &Ctx{}
		ty, _, err := e.checkType(c, d.Ty)
		if err != nil {
			return nil, nil, fmt.Errorf("%s: %w", d.Name, err)
		}
		tys[i] = ty
	}
	// Bind every member name to its placeholder, and expose its type, for ALL bodies.
	e.SelfTypes = make(map[core.Hash]core.Tm, n)
	for i, d := range ds {
		ph := store.Placeholder(i)
		e.Refs[d.Name] = ph
		e.SelfTypes[ph] = tys[i]
	}
	bodies = make([]core.Tm, n)
	for i, d := range ds {
		c := &Ctx{}
		body, err := e.Check(c, d.Body, e.Eval(c, tys[i]))
		if err != nil {
			return nil, nil, fmt.Errorf("%s: %w", d.Name, err)
		}
		if err := e.ResolvePending(); err != nil {
			return nil, nil, fmt.Errorf("%s: %w", d.Name, err)
		}
		bodies[i] = e.Zonk(0, body)
		if err := e.ErrUnsolved(d.Name); err != nil {
			return nil, nil, err
		}
		if !MetaFree(bodies[i]) {
			return nil, nil, fmt.Errorf("%s: internal: metavariable survived zonking", d.Name)
		}
	}
	for i := range tys {
		tys[i] = e.Zonk(0, tys[i])
	}
	e.SelfTypes = nil
	return tys, bodies, nil
}

// piSort is the sort of a Pi from its domain's and codomain's sorts: a
// function into a proposition is a proposition (Prop is impredicative);
// otherwise the Pi lives at the max of the levels (predicative U hierarchy).
func piSort(dom, cod core.Val) core.Val {
	if _, ok := cod.(core.VProp); ok {
		return core.VProp{}
	}
	ld, lc := 0, 0
	if u, ok := dom.(core.VU); ok {
		ld = u.Lvl
	}
	if u, ok := cod.(core.VU); ok {
		lc = u.Lvl
	}
	if lc > ld {
		return core.VU{Lvl: lc}
	}
	return core.VU{Lvl: ld}
}
