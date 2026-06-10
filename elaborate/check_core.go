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
			if err := e.CheckCore(c, tm.Ty, core.VU{}); err != nil {
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
	case core.Meta:
		return nil, fmt.Errorf("metavariable in core term: the checker judges only zonked, meta-free core")
	case core.Pi:
		if err := e.CheckCore(c, tm.Dom, core.VU{}); err != nil {
			return nil, err
		}
		dom := e.Eval(c, tm.Dom)
		if err := e.CheckCore(c.bind(tm.Cod.Name, dom), tm.Cod.Body, core.VU{}); err != nil {
			return nil, err
		}
		return core.VU{}, nil
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
			if err := e.CheckCore(c, tm.Ty, core.VU{}); err != nil {
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
		if err := e.CheckCore(c, tm.Ty, core.VU{}); err != nil {
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

// CheckDef checks one definition: ty must be a type, body must have type ty.
// This is the judgment a proof-cache certificate records; run it on a fresh
// Elaborator and read the Machine's Deps afterward for the certificate's U.
func (e *Elaborator) CheckDef(ty, body core.Tm) error {
	c := &Ctx{}
	if err := e.CheckCore(c, ty, core.VU{}); err != nil {
		return err
	}
	return e.CheckCore(c, body, e.Eval(c, ty))
}
