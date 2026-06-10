package elaborate

import (
	"fmt"

	"goforge.dev/rune/core"
	"goforge.dev/rune/store"
	"goforge.dev/rune/surface"
)

// Datatype declaration elaboration (Phase 4). A declaration
//
//	data D : (p1 : A1) -> … -> (pk : Ak) -> U is
//	  C1 : (p1 : A1) -> … -> (pk : Ak) -> T1 -> … -> D p1 … pk
//	  | …
//	end
//
// is checked for well-formedness — every constructor repeats the parameter
// telescope, every argument type is either a recursive occurrence `D p*`
// exactly or mentions D not at all (STRICT POSITIVITY, the simple form), and
// the return type is `D p*` — and compiled to a store.DataDecl carrying the
// constructor signatures and the generated eliminator type. Totality is by
// construction: the eliminator is the only recursion principle, so there is
// nothing for a termination checker to reject.

// ElabData elaborates one surface datatype declaration.
func (e *Elaborator) ElabData(d surface.DataDef) (store.DataDecl, error) {
	fail := func(format string, args ...any) (store.DataDecl, error) {
		return store.DataDecl{}, fmt.Errorf("data %s: %s", d.Name, fmt.Sprintf(format, args...))
	}

	// The former's type: a closed explicit Pi telescope ending in U.
	c := &Ctx{}
	ty, _, err := e.checkType(c, d.Ty)
	if err != nil {
		return fail("%v", err)
	}
	ty = e.Zonk(0, ty)
	doms, ret := telescope(ty)
	if _, ok := ret.(core.Univ); !ok {
		return fail("the former's type must end in U, not %s", e.prettyTm(ret))
	}
	for _, dom := range doms {
		if dom.icit != core.Expl {
			return fail("implicit datatype parameters are not supported")
		}
	}
	k := len(doms)

	// Make D visible (as the positional placeholder) while the constructor
	// types are elaborated.
	ph := store.Placeholder(0)
	refs := overlayRefs(e.Refs, d.Name, ph)
	g := overlayGlobals{base: e.M.G, hash: ph, ty: ty}
	inner := New(g, refs, e.RefNames)
	inner.M.EqS = e.M.EqS
	inner.M.Data = e.M.Data
	inner.M.Quot = e.M.Quot

	decl := store.DataDecl{Name: d.Name, Ty: ty, NumParams: k}
	for _, ctor := range d.Ctors {
		cc := &Ctx{}
		cty, _, err := inner.checkType(cc, ctor.Ty)
		if err != nil {
			return fail("constructor %s: %v", ctor.Name, err)
		}
		cty = inner.Zonk(0, cty)
		if err := inner.ErrUnsolved("constructor " + ctor.Name); err != nil {
			return fail("%v", err)
		}
		cdoms, cret := telescope(cty)
		if len(cdoms) < k {
			return fail("constructor %s must repeat the %d parameter binder(s)", ctor.Name, k)
		}
		for i := 0; i < k; i++ {
			if core.HashTerm(cdoms[i].dom) != core.HashTerm(doms[i].dom) {
				return fail("constructor %s: parameter %d differs from the former's telescope", ctor.Name, i+1)
			}
		}
		arity := len(cdoms) - k
		rec := make([]bool, arity)
		for j := 0; j < arity; j++ {
			dom := cdoms[k+j].dom
			if core.HashTerm(dom) == core.HashTerm(selfApplied(ph, k, j)) {
				rec[j] = true
				continue
			}
			if mentionsRef(dom, ph) {
				return fail("constructor %s: argument %d violates strict positivity (D may appear only as exactly `%s params`)",
					ctor.Name, j+1, d.Name)
			}
		}
		if core.HashTerm(cret) != core.HashTerm(selfApplied(ph, k, arity)) {
			return fail("constructor %s must return `%s` applied to the parameters in order", ctor.Name, d.Name)
		}
		decl.CtorNames = append(decl.CtorNames, ctor.Name)
		decl.CtorTys = append(decl.CtorTys, cty)
		decl.Sigs = append(decl.Sigs, core.CtorSig{Arity: arity, Rec: rec})
	}
	if len(decl.CtorTys) == 0 {
		return fail("a datatype needs at least one constructor (the empty type arrives when a listing needs it)")
	}

	decl.ElimTy = elimType(decl, ph)
	return decl, nil
}

// binder is one telescope entry.
type binder struct {
	name string
	icit core.Icit
	dom  core.Tm
}

// telescope splits a Pi chain into its binders and final codomain.
func telescope(t core.Tm) ([]binder, core.Tm) {
	var out []binder
	for {
		pi, ok := t.(core.Pi)
		if !ok {
			return out, t
		}
		out = append(out, binder{name: pi.Cod.Name, icit: pi.Icit, dom: pi.Dom})
		t = pi.Cod.Body
	}
}

// selfApplied is `D p1 … pk` at a position with `extra` binders between the
// parameter telescope and here: parameter i (outermost first) has de Bruijn
// index k-1-i+extra.
func selfApplied(d core.Hash, k, extra int) core.Tm {
	var t core.Tm = core.Ref{Hash: d}
	for i := 0; i < k; i++ {
		t = core.App{Fn: t, Arg: core.Var{Idx: k - 1 - i + extra}, Icit: core.Expl}
	}
	return t
}

// mentionsRef reports whether t contains a Ref to h.
func mentionsRef(t core.Tm, h core.Hash) bool {
	found := false
	var walk func(core.Tm)
	walk = func(t core.Tm) {
		if found || t == nil {
			return
		}
		switch x := t.(type) {
		case core.Ref:
			if x.Hash == h {
				found = true
			}
		case core.Pi:
			walk(x.Dom)
			walk(x.Cod.Body)
		case core.Lam:
			walk(x.Body.Body)
		case core.App:
			walk(x.Fn)
			walk(x.Arg)
		case core.Let:
			walk(x.Ty)
			walk(x.Val)
			walk(x.Body.Body)
		case core.Ann:
			walk(x.Term)
			walk(x.Ty)
		case core.Eq:
			walk(x.Ty)
			walk(x.L)
			walk(x.R)
		case core.Refl:
			walk(x.Tm)
		case core.Cast:
			walk(x.A)
			walk(x.B)
			walk(x.P)
			walk(x.X)
		case core.Subst:
			walk(x.A)
			walk(x.X)
			walk(x.Y)
			walk(x.Prf)
			walk(x.P)
			walk(x.Px)
		}
	}
	walk(t)
	return found
}

// elimType generates the eliminator's type:
//
//	(p1:A1) … (pk:Ak)
//	-> (m : D p* -> U)
//	-> (c_i : (a1:T1) … (an:Tn) -> [m a_r ->]* m (C_i p* a*))  per constructor
//	-> (x : D p*) -> m x
//
// All index arithmetic is explicit; the motive lands in U, and Prop-valued
// motives are admitted by Prop <: U cumulativity at the use site.
func elimType(d store.DataDecl, ph core.Hash) core.Tm {
	doms, _ := telescope(d.Ty)
	k := d.NumParams
	n := len(d.CtorTys)

	// Innermost first: build the final `(x : D p*) -> m x`, then wrap cases,
	// motive, and parameters around it.
	// Depth map at the final binder position: params at 0..k-1 (outermost),
	// then motive, then n cases, then x. Total binders before `m x`: k+1+n+1.
	motiveIdxAtEnd := n + 1 // from inside (x): cases are 1..n, motive n+1
	end := core.Pi{
		Dom: selfApplied(ph, k, 1+n), // D p* with motive+cases between
		Cod: core.Scope{Name: "x", Body: core.App{
			Fn:   core.Var{Idx: motiveIdxAtEnd},
			Arg:  core.Var{Idx: 0},
			Icit: core.Expl,
		}},
	}

	// Wrap constructor cases, LAST first.
	body := core.Tm(end)
	for i := n - 1; i >= 0; i-- {
		caseTy := caseType(d, ph, i)
		// At case i's binder, the binders inside between it and the motive are
		// the LATER cases (i+1..n-1)? No: cases are wrapped outside-in, so at
		// case i's position the in-scope binders are params + motive + cases
		// 0..i-1. caseType computes against exactly that scope.
		body = core.Pi{Dom: caseTy, Cod: core.Scope{Name: "c" + d.CtorNames[i], Body: body}}
	}

	// The motive: (m : D p* -> U).
	motive := core.Pi{
		Dom: core.Pi{Dom: selfApplied(ph, k, 0), Cod: core.Scope{Name: "x", Body: core.Univ{}}},
		Cod: core.Scope{Name: "m", Body: body},
	}

	// The parameter telescope.
	out := core.Tm(motive)
	for i := k - 1; i >= 0; i-- {
		out = core.Pi{Icit: doms[i].icit, Dom: doms[i].dom, Cod: core.Scope{Name: doms[i].name, Body: out}}
	}
	return out
}

// caseType builds the type of the i-th constructor's case, in a scope of
// k params + 1 motive + i earlier cases:
//
//	(a1:T1) … (an:Tn) -> [IH: m a_r ->]* -> m (C_i p* a*)
func caseType(d store.DataDecl, ph core.Hash, i int) core.Tm {
	k := d.NumParams
	sig := d.Sigs[i]
	cdoms, _ := telescope(d.CtorTys[i])
	argDoms := cdoms[k:]
	pre := 1 + i // motive + earlier cases sit between the params and this case

	nIH := 0
	for _, r := range sig.Rec {
		if r {
			nIH++
		}
	}

	// Innermost scope (inside all arg and IH binders), innermost first:
	// nIH IHs, sig.Arity args, then — outside this case's type — the i
	// earlier cases, the motive, and the k params.
	depthInside := sig.Arity + nIH
	motiveIdx := depthInside + i

	// The result: m (C_i p* a*). Params: param j (outermost first) has index
	// k-1-j + pre + depthInside. Args: arg j has index (arity-1-j) + nIH.
	var capp core.Tm = core.Ref{Hash: ctorPlaceholder(i)}
	for j := 0; j < k; j++ {
		capp = core.App{Fn: capp, Arg: core.Var{Idx: k - 1 - j + pre + depthInside}, Icit: core.Expl}
	}
	for j := 0; j < sig.Arity; j++ {
		capp = core.App{Fn: capp, Arg: core.Var{Idx: (sig.Arity - 1 - j) + nIH}, Icit: core.Expl}
	}
	result := core.Tm(core.App{Fn: core.Var{Idx: motiveIdx}, Arg: capp, Icit: core.Expl})

	// Wrap IH binders, last recursive argument first. The IH for arg r (with
	// h IHs already wrapped INSIDE this one — i.e., later recursive args) is
	// m a_r where a_r's index skips those h inner IHs.
	ihWrapped := 0
	for j := sig.Arity - 1; j >= 0; j-- {
		if !sig.Rec[j] {
			continue
		}
		// Scope at this IH binder, innermost first: the IHs already wrapped
		// inside it, then all args, then earlier cases, then the motive.
		inner := ihWrapped
		argIdx := (sig.Arity - 1 - j) + inner
		mIdx := inner + sig.Arity + i
		result = core.Pi{
			Dom: core.App{Fn: core.Var{Idx: mIdx}, Arg: core.Var{Idx: argIdx}, Icit: core.Expl},
			Cod: core.Scope{Name: "ih", Body: result},
		}
		ihWrapped++
	}

	// Wrap argument binders, last first. Arg j's dom was elaborated in the
	// scope (params, args 0..j-1); here the scope is (params, motive, earlier
	// cases, args 0..j-1) — indices ≥ j must skip motive + i cases.
	for j := sig.Arity - 1; j >= 0; j-- {
		dom := shift(argDoms[j].dom, j, pre)
		result = core.Pi{Dom: dom, Cod: core.Scope{Name: argDoms[j].name, Body: result}}
	}
	return result
}

// ctorPlaceholder is the positional placeholder for the i-th constructor in a
// declaration group (the former is Placeholder(0); constructors follow).
func ctorPlaceholder(i int) core.Hash { return store.Placeholder(i + 1) }

// overlayRefs extends a name map with one binding, without mutating it.
func overlayRefs(base map[string]core.Hash, name string, h core.Hash) map[string]core.Hash {
	out := make(map[string]core.Hash, len(base)+1)
	for k, v := range base {
		out[k] = v
	}
	out[name] = h
	return out
}

// overlayGlobals exposes one extra (bodiless) definition over a base Globals.
type overlayGlobals struct {
	base core.Globals
	hash core.Hash
	ty   core.Tm
}

func (o overlayGlobals) TypeOf(h core.Hash) (core.Tm, bool) {
	if h == o.hash {
		return o.ty, true
	}
	return o.base.TypeOf(h)
}

func (o overlayGlobals) Unfold(h core.Hash) (core.Tm, bool) {
	if h == o.hash {
		return nil, false
	}
	return o.base.Unfold(h)
}
