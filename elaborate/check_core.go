package elaborate

import (
	"fmt"

	"goforge.dev/rune/core"
)

// CheckCore checks core term t against type want (a value). This is the cached
// judgment: deterministic on its inputs plus the bodies it unfolds, all of which
// the Machine logs. Core lambdas are un-annotated, so they check (against a Pi)
// rather than infer — definition bodies always arrive here with their full
// declared type, so check mode reaches every binder.
func (e *Elaborator) CheckCore(c *Ctx, t core.Tm, want core.Val) error {
	switch tm := t.(type) {
	case core.Lam:
		pi, ok := e.M.Force(want).(core.VPi)
		if !ok {
			return fmt.Errorf("lambda has non-function type %s", e.pretty(c, want))
		}
		if pi.Icit != tm.Icit {
			return fmt.Errorf("%s lambda checked against an %s function type",
				icitName(tm.Icit), icitName(pi.Icit))
		}
		v := core.VVar(c.Lvl())
		return e.CheckCore(c.bind(tm.Body.Name, pi.Dom), tm.Body.Body, pi.Cod(v))
	case core.Let:
		var vty core.Val
		if tm.Ty != nil {
			if _, err := e.checkTypeCore(c, tm.Ty); err != nil {
				return err
			}
			vty = e.Eval(c, tm.Ty)
			if err := e.CheckCore(c, tm.Val, vty); err != nil {
				return err
			}
		} else {
			ty, err := e.InferCore(c, tm.Val)
			if err != nil {
				return err
			}
			vty = ty
		}
		vval := e.Eval(c, tm.Val)
		return e.CheckCore(c.define(tm.Body.Name, vty, vval), tm.Body.Body, want)
	default:
		got, err := e.InferCore(c, t)
		if err != nil {
			return err
		}
		if !e.M.Conv(c.Lvl(), got, want) {
			return fmt.Errorf("type mismatch: expected %s, got %s",
				e.pretty(c, want), e.pretty(c, got))
		}
		return nil
	}
}

// InferCore infers the type of core term t as a value.
func (e *Elaborator) InferCore(c *Ctx, t core.Tm) (core.Val, error) {
	switch tm := t.(type) {
	case core.Var:
		if tm.Idx < 0 || tm.Idx >= len(c.types) {
			return nil, fmt.Errorf("unbound variable index %d", tm.Idx)
		}
		return c.types[tm.Idx], nil
	case core.Ref:
		return e.refType(tm.Hash)
	case core.Univ:
		return core.VU{}, nil // type : type until the Phase-6 hierarchy
	case core.Prop:
		return core.VU{}, nil
	case core.Meta:
		return nil, fmt.Errorf("metavariable in core term: the checker judges only zonked, meta-free core")
	case core.Eq:
		if _, err := e.checkTypeCore(c, tm.Ty); err != nil {
			return nil, err
		}
		vty := e.Eval(c, tm.Ty)
		if err := e.CheckCore(c, tm.L, vty); err != nil {
			return nil, err
		}
		if err := e.CheckCore(c, tm.R, vty); err != nil {
			return nil, err
		}
		return core.VProp{}, nil
	case core.Refl:
		xty, err := e.InferCore(c, tm.Tm)
		if err != nil {
			return nil, err
		}
		vx := e.Eval(c, tm.Tm)
		eqTy := e.M.EvalEq(xty, vx, vx)
		if _, stillEq := e.M.Force(eqTy).(core.VEq); !stillEq {
			// Funext computed the equality away: a refl at a function type
			// must be eta-expanded (elaboration does this; raw core must too).
			return nil, fmt.Errorf("refl at a function type; eta-expand the proof")
		}
		return eqTy, nil
	case core.Cast:
		if _, err := e.checkTypeCore(c, tm.A); err != nil {
			return nil, err
		}
		if _, err := e.checkTypeCore(c, tm.B); err != nil {
			return nil, err
		}
		va, vb := e.Eval(c, tm.A), e.Eval(c, tm.B)
		if err := e.CheckCore(c, tm.P, core.VEq{Ty: core.VU{}, L: va, R: vb}); err != nil {
			return nil, err
		}
		if err := e.CheckCore(c, tm.X, va); err != nil {
			return nil, err
		}
		return vb, nil
	case core.Pi:
		if _, err := e.checkTypeCore(c, tm.Dom); err != nil {
			return nil, err
		}
		dom := e.Eval(c, tm.Dom)
		sort, err := e.checkTypeCore(c.bind(tm.Cod.Name, dom), tm.Cod.Body)
		if err != nil {
			return nil, err
		}
		return sort, nil
	case core.Lam:
		return nil, fmt.Errorf("cannot infer the type of an un-annotated lambda; ascribe it: (fn … : T)")
	case core.App:
		fnTy, err := e.InferCore(c, tm.Fn)
		if err != nil {
			return nil, err
		}
		pi, ok := e.M.Force(fnTy).(core.VPi)
		if !ok {
			return nil, fmt.Errorf("applying a non-function of type %s", e.pretty(c, fnTy))
		}
		if pi.Icit != tm.Icit {
			return nil, fmt.Errorf("%s application to a function expecting an %s argument",
				icitName(tm.Icit), icitName(pi.Icit))
		}
		if err := e.CheckCore(c, tm.Arg, pi.Dom); err != nil {
			return nil, err
		}
		return pi.Cod(e.Eval(c, tm.Arg)), nil
	case core.Let:
		var vty core.Val
		if tm.Ty != nil {
			if _, err := e.checkTypeCore(c, tm.Ty); err != nil {
				return nil, err
			}
			vty = e.Eval(c, tm.Ty)
			if err := e.CheckCore(c, tm.Val, vty); err != nil {
				return nil, err
			}
		} else {
			ty, err := e.InferCore(c, tm.Val)
			if err != nil {
				return nil, err
			}
			vty = ty
		}
		vval := e.Eval(c, tm.Val)
		return e.InferCore(c.define(tm.Body.Name, vty, vval), tm.Body.Body)
	case core.Ann:
		if _, err := e.checkTypeCore(c, tm.Ty); err != nil {
			return nil, err
		}
		want := e.Eval(c, tm.Ty)
		if err := e.CheckCore(c, tm.Term, want); err != nil {
			return nil, err
		}
		return want, nil
	default:
		return nil, fmt.Errorf("infer: unknown core term %T", t)
	}
}

// checkTypeCore checks that t is a type, returning its sort (VU or VProp).
func (e *Elaborator) checkTypeCore(c *Ctx, t core.Tm) (core.Val, error) {
	got, err := e.InferCore(c, t)
	if err != nil {
		return nil, err
	}
	switch s := e.M.Force(got).(type) {
	case core.VU:
		return core.VU{}, nil
	case core.VProp:
		return core.VProp{}, nil
	default:
		_ = s
		return nil, fmt.Errorf("not a type: %s has type %s", e.prettyTm(t), e.pretty(c, got))
	}
}

// CheckDef checks one definition: ty must be a type, body must have type ty.
// This is the judgment a proof-cache certificate records; run it on a fresh
// Elaborator and read the Machine's Deps afterward for the certificate's U.
func (e *Elaborator) CheckDef(ty, body core.Tm) error {
	c := &Ctx{}
	if _, err := e.checkTypeCore(c, ty); err != nil {
		return err
	}
	return e.CheckCore(c, body, e.Eval(c, ty))
}
