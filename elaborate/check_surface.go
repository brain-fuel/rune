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
	case surface.EHole:
		// A hole is a fresh meta; its type is another fresh meta.
		tyM := e.freshMeta(c, "type of _")
		tm := e.freshMeta(c, "_")
		return tm, e.Eval(c, tyM), nil
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
		return core.Pi{Icit: s.Icit, Dom: dom, Cod: core.Scope{Name: s.Param, Body: cod}}, core.VU{}, nil
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
		// Close the inferred codomain over the binder by quoting one level up,
		// then re-evaluating it under the argument the Pi closure receives.
		codTm := e.M.Quote(inner.Lvl(), bodyTy)
		env := c.env
		pi := core.VPi{Name: s.Param, Icit: s.Icit, Dom: vdom, Cod: func(v core.Val) core.Val {
			return e.M.Eval(env.Extend(v), codTm)
		}}
		return core.Lam{Icit: s.Icit, Body: core.Scope{Name: s.Param, Body: body}}, pi, nil
	case surface.EApp:
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
		arg, err := e.Check(c, s.Arg, pi.Dom)
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
			name := pi.Name
			if name == "" {
				name = "x"
			}
			return core.Lam{Icit: core.Impl, Body: core.Scope{Name: name, Body: body}}, nil
		}
	}

	switch s := x.(type) {
	case surface.EHole:
		return e.freshMeta(c, "_"), nil
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
		// The annotation must agree with the expected domain.
		dom, err := e.Check(c, s.Dom, core.VU{})
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
		return core.Lam{Icit: s.Icit, Body: core.Scope{Name: s.Param, Body: body}}, nil
	case surface.ELet:
		tm, _, inner, err := e.elabLet(c, s)
		if err != nil {
			return nil, err
		}
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
		// Insert any leading implicits the inferred type still carries, unless
		// the expected type wants them.
		if pi, ok := fw.(core.VPi); !ok || pi.Icit == core.Expl {
			tm, got = e.insertImplicits(c, tm, got)
		}
		if err := e.Unify(c.Lvl(), got, want); err != nil {
			return nil, fmt.Errorf("type mismatch: expected %s, got %s (%v)",
				e.pretty(c, want), e.pretty(c, got), err)
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

// ElabDef elaborates one surface definition (type and body) to core, ZONKED and
// meta-free: every metavariable the run created must have been solved.
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
	ty, body = e.Zonk(0, ty), e.Zonk(0, body)
	if err := e.ErrUnsolved(d.Name); err != nil {
		return nil, nil, err
	}
	if !MetaFree(ty) || !MetaFree(body) {
		return nil, nil, fmt.Errorf("%s: internal: metavariable survived zonking", d.Name)
	}
	return ty, body, nil
}
