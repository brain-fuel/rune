package elaborate

import (
	"fmt"

	"goforge.dev/rune/core"
	"goforge.dev/rune/surface"
)

// The surface elaborator: bidirectional Infer/Check from named surface Exp to
// core Tm. Unlike the core checker it can USE the binder annotation the grammar
// requires on every fn — so a bare `fn (x : A) is e end` infers — while emitting
// exactly the core that name resolution would (the annotation is consulted, then
// discarded; the core lambda stays un-annotated, so content hashes are unchanged).
//
// PHASE-2 INSERTION SITE: metavariables, pattern unification, and implicit
// insertion extend these two functions. Phase 1 has no metas, so elaboration here
// is deterministic.

// Infer elaborates e, returning the core term and its inferred type.
func (e *Elaborator) Infer(c *Ctx, x surface.Exp) (core.Tm, core.Val, error) {
	switch s := x.(type) {
	case surface.EVar:
		if i, ty, ok := c.lookup(s.Name); ok {
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
	case surface.EPi:
		dom, err := e.Check(c, s.Dom, core.VU{})
		if err != nil {
			return nil, nil, err
		}
		vdom := e.Eval(c, dom)
		cod, err := e.Check(c.bind(s.Param, vdom), s.Cod, core.VU{})
		if err != nil {
			return nil, nil, err
		}
		return core.Pi{Dom: dom, Cod: core.Scope{Name: s.Param, Body: cod}}, core.VU{}, nil
	case surface.ELam:
		dom, err := e.Check(c, s.Dom, core.VU{})
		if err != nil {
			return nil, nil, err
		}
		vdom := e.Eval(c, dom)
		inner := c.bind(s.Param, vdom)
		body, bodyTy, err := e.Infer(inner, s.Body)
		if err != nil {
			return nil, nil, err
		}
		// Close the inferred codomain over the binder by quoting it one level up,
		// then re-evaluating it under the argument the Pi closure receives.
		codTm := e.M.Quote(inner.Lvl(), bodyTy)
		env := c.env
		pi := core.VPi{Name: s.Param, Dom: vdom, Cod: func(v core.Val) core.Val {
			return e.M.Eval(env.Extend(v), codTm)
		}}
		return core.Lam{Body: core.Scope{Name: s.Param, Body: body}}, pi, nil
	case surface.EApp:
		fn, fnTy, err := e.Infer(c, s.Fn)
		if err != nil {
			return nil, nil, err
		}
		pi, ok := e.M.Force(fnTy).(core.VPi)
		if !ok {
			return nil, nil, fmt.Errorf("applying a non-function of type %s", e.pretty(c, fnTy))
		}
		arg, err := e.Check(c, s.Arg, pi.Dom)
		if err != nil {
			return nil, nil, err
		}
		return core.App{Fn: fn, Arg: arg}, pi.Cod(e.Eval(c, arg)), nil
	case surface.ELet:
		tm, vty, inner, err := e.elabLet(c, s)
		if err != nil {
			return nil, nil, err
		}
		body, bodyTy, err := e.Infer(inner, s.Body)
		if err != nil {
			return nil, nil, err
		}
		_ = vty
		tm.Body = core.Scope{Name: s.Name, Body: body}
		return tm, bodyTy, nil
	case surface.EAnn:
		ty, err := e.Check(c, s.Ty, core.VU{})
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

// Check elaborates e against the expected type want.
func (e *Elaborator) Check(c *Ctx, x surface.Exp, want core.Val) (core.Tm, error) {
	switch s := x.(type) {
	case surface.ELam:
		pi, ok := e.M.Force(want).(core.VPi)
		if !ok {
			return nil, fmt.Errorf("lambda has non-function type %s", e.pretty(c, want))
		}
		// The annotation must agree with the expected domain.
		dom, err := e.Check(c, s.Dom, core.VU{})
		if err != nil {
			return nil, err
		}
		vdom := e.Eval(c, dom)
		if !e.M.Conv(c.Lvl(), vdom, pi.Dom) {
			return nil, fmt.Errorf("binder %s annotated %s, but the expected domain is %s",
				s.Param, e.pretty(c, vdom), e.pretty(c, pi.Dom))
		}
		inner := c.bind(s.Param, pi.Dom)
		body, err := e.Check(inner, s.Body, pi.Cod(core.VVar(c.Lvl())))
		if err != nil {
			return nil, err
		}
		return core.Lam{Body: core.Scope{Name: s.Param, Body: body}}, nil
	case surface.ELet:
		tm, vty, inner, err := e.elabLet(c, s)
		if err != nil {
			return nil, err
		}
		_ = vty
		body, err := e.Check(inner, s.Body, want)
		if err != nil {
			return nil, err
		}
		tm.Body = core.Scope{Name: s.Name, Body: body}
		return tm, nil
	default:
		tm, got, err := e.Infer(c, x)
		if err != nil {
			return nil, err
		}
		if !e.M.Conv(c.Lvl(), got, want) {
			return nil, fmt.Errorf("type mismatch: expected %s, got %s",
				e.pretty(c, want), e.pretty(c, got))
		}
		return tm, nil
	}
}

// elabLet elaborates the binding part of a let (annotation and value), returning
// the partially-built core Let (Body unset), the binder's type, and the extended
// context in which to elaborate the body.
func (e *Elaborator) elabLet(c *Ctx, s surface.ELet) (core.Let, core.Val, *Ctx, error) {
	var tyTm core.Tm
	var vty core.Val
	var val core.Tm
	if s.Ty != nil {
		t, err := e.Check(c, s.Ty, core.VU{})
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

// ElabDef elaborates one surface definition (type and body) to core. The grammar
// makes the type mandatory; the body is checked against it.
func (e *Elaborator) ElabDef(d surface.Def) (ty, body core.Tm, err error) {
	c := &Ctx{}
	if d.Ty == nil {
		return nil, nil, fmt.Errorf("%s: definition has no type", d.Name)
	}
	ty, err = e.Check(c, d.Ty, core.VU{})
	if err != nil {
		return nil, nil, fmt.Errorf("%s: %w", d.Name, err)
	}
	body, err = e.Check(c, d.Body, e.Eval(c, ty))
	if err != nil {
		return nil, nil, fmt.Errorf("%s: %w", d.Name, err)
	}
	return ty, body, nil
}
