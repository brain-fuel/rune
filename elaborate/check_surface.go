package elaborate

import (
	"fmt"

	"goforge.dev/rune/core"
	"goforge.dev/rune/surface"
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
		return nil, nil, fmt.Errorf("unbound identifier %q", s.Name)
	case surface.EUniv:
		return core.Univ{}, core.VU{}, nil // type : type until Phase 6
	case surface.EProp:
		return core.Prop{}, core.VU{}, nil
	case surface.EEq, surface.ERefl, surface.ECast, surface.ESubst:
		return nil, nil, fmt.Errorf("an equality former needs its arguments (Eq T l r · refl x · cast A B p x · subst A x y p P px)")
	case surface.EHole:
		// A hole is a fresh meta; its type is another fresh meta.
		tyM := e.freshMeta(c, "type of _")
		tm := e.freshMeta(c, "_")
		return tm, e.Eval(c, tyM), nil
	case surface.EPi:
		m0 := e.pushZero()
		defer e.popMult(m0)
		dom, _, err := e.checkType(c, s.Dom)
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
		// A function into a proposition is a proposition (Prop is closed
		// under Pi); otherwise the Pi lives in U.
		return core.Pi{Icit: s.Icit, Qty: s.Qty, Dom: dom, Cod: core.Scope{Name: s.Param, Body: cod}}, codSort, nil
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
				return nil, nil, fmt.Errorf("applying a non-function of type %s (%v)", e.pretty(c, fnTy), err2)
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
		return core.App{Fn: fn, Arg: arg, Icit: s.Icit}, pi.Cod(e.Eval(c, arg)), nil
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
		m := e.freshMeta(c, "implicit argument")
		tm = core.App{Fn: tm, Arg: m, Icit: core.Impl}
		ty = pi.Cod(e.Eval(c, m))
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
	case surface.ERefl:
		// Bare refl in checking position: the expected type must be (or reduce
		// to) an equality with convertible endpoints — including the pointwise
		// expansion of a function equality, handled by reflProof's recursion
		// arriving here under binders.
		eq, ok := fw.(core.VEq)
		if !ok {
			return nil, fmt.Errorf("refl checked against %s, which is not an equality type", e.pretty(c, want))
		}
		if !e.M.Conv(c.Lvl(), eq.L, eq.R) {
			return nil, fmt.Errorf("refl does not prove %s: the sides are not definitionally equal", e.pretty(c, fw))
		}
		return core.Refl{Tm: e.M.Quote(c.Lvl(), eq.L)}, nil
	case surface.ELam:
		pi, ok := fw.(core.VPi)
		if !ok {
			// Checking a lambda against a flexible type: unify with a fresh Pi.
			var err error
			pi, err = e.expectPi(c, want, s.Icit)
			if err != nil {
				return nil, fmt.Errorf("lambda has non-function type %s", e.pretty(c, want))
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
			return nil, fmt.Errorf("type mismatch: expected %s, got %s (%v)",
				e.pretty(c, want), e.pretty(c, got), err)
		}
		return tm, nil
	}
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
	switch e.M.Force(got2).(type) {
	case core.VU:
		return tm2, core.VU{}, nil
	case core.VProp:
		return tm2, core.VProp{}, nil
	default:
		if err := e.Unify(c.Lvl(), got2, core.VU{}); err != nil {
			return nil, nil, fmt.Errorf("not a type: %s has type %s",
				e.prettyTm(tm2), e.pretty(c, got2))
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
		a, _, err := e.checkType(c, args[0])
		if err != nil {
			return nil, nil, true, err
		}
		b, _, err := e.checkType(c, args[1])
		if err != nil {
			return nil, nil, true, err
		}
		va, vb := e.Eval(c, a), e.Eval(c, b)
		mp := e.pushZero()
		pr, err := e.Check(c, args[2], core.VEq{Ty: core.VU{}, L: va, R: vb})
		e.popMult(mp)
		if err != nil {
			return nil, nil, true, err
		}
		x, err := e.Check(c, args[3], va)
		if err != nil {
			return nil, nil, true, err
		}
		return core.Cast{A: a, B: b, P: pr, X: x}, vb, true, nil
	}
	return nil, nil, false, nil
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
	ty, body = e.Zonk(0, ty), e.Zonk(0, body)
	if err := e.ErrUnsolved(d.Name); err != nil {
		return nil, nil, err
	}
	if !MetaFree(ty) || !MetaFree(body) {
		return nil, nil, fmt.Errorf("%s: internal: metavariable survived zonking", d.Name)
	}
	return ty, body, nil
}
