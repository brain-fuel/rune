package elaborate

import (
	"fmt"

	"goforge.dev/rune/v3/core"
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
		if !e.M.Conv(c.Lvl(), got, want) && !e.M.Sub(c.Lvl(), got, want) {
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
		return core.VU{Lvl: tm.Lvl + 1}, nil // U_i : U_{i+1}
	case core.Prop:
		return core.VU{}, nil // Prop : U_0
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
		saCast, err := e.checkTypeCore(c, tm.A)
		if err != nil {
			return nil, err
		}
		if err := e.CheckCore(c, tm.P, core.VEq{Ty: saCast, L: va, R: vb}); err != nil {
			return nil, err
		}
		if err := e.CheckCore(c, tm.X, va); err != nil {
			return nil, err
		}
		return vb, nil
	case core.Subst:
		if _, err := e.checkTypeCore(c, tm.A); err != nil {
			return nil, err
		}
		va := e.Eval(c, tm.A)
		if err := e.CheckCore(c, tm.X, va); err != nil {
			return nil, err
		}
		if err := e.CheckCore(c, tm.Y, va); err != nil {
			return nil, err
		}
		vx, vy := e.Eval(c, tm.X), e.Eval(c, tm.Y)
		if err := e.CheckCore(c, tm.Prf, e.M.EvalEq(va, vx, vy)); err != nil {
			return nil, err
		}
		motiveTy := core.VPi{Name: "z", Dom: va, Cod: func(core.Val) core.Val { return core.VU{} }}
		if err := e.CheckCore(c, tm.P, motiveTy); err != nil {
			return nil, err
		}
		vp := e.Eval(c, tm.P)
		if err := e.CheckCore(c, tm.Px, e.M.Apply(vp, vx)); err != nil {
			return nil, err
		}
		return e.M.Apply(vp, vy), nil
	case core.Pi:
		domSort, err := e.checkTypeCore(c, tm.Dom)
		if err != nil {
			return nil, err
		}
		dom := e.Eval(c, tm.Dom)
		codSort, err := e.checkTypeCore(c.bind(tm.Cod.Name, dom), tm.Cod.Body)
		if err != nil {
			return nil, err
		}
		return piSort(domSort, codSort), nil
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
		// As in the surface elaborator: a non-dependent Pi's codomain ignores
		// the argument value, and skipping its evaluation keeps n-deep
		// application chains linear instead of O(n²).
		var av core.Val
		if !pi.NonDep {
			av = e.Eval(c, tm.Arg)
		}
		return pi.Cod(av), nil
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
	case core.Sig:
		domSort, err := e.checkTypeCore(c, tm.Dom)
		if err != nil {
			return nil, err
		}
		dom := e.Eval(c, tm.Dom)
		codSort, err := e.checkTypeCore(c.bind(tm.Cod.Name, dom), tm.Cod.Body)
		if err != nil {
			return nil, err
		}
		return sigSort(domSort, codSort), nil
	case core.Pair:
		if _, err := e.checkTypeCore(c, tm.Dom); err != nil {
			return nil, err
		}
		vdom := e.Eval(c, tm.Dom)
		if _, err := e.checkTypeCore(c.bind(tm.Cod.Name, vdom), tm.Cod.Body); err != nil {
			return nil, err
		}
		if err := e.CheckCore(c, tm.A, vdom); err != nil {
			return nil, err
		}
		vsig, ok := e.Eval(c, core.Sig{Dom: tm.Dom, Cod: tm.Cod}).(core.VSig)
		if !ok {
			return nil, fmt.Errorf("pair: Σ did not evaluate to a Σ value")
		}
		if err := e.CheckCore(c, tm.B, vsig.Cod(e.Eval(c, tm.A))); err != nil {
			return nil, err
		}
		return vsig, nil
	case core.Fst:
		pty, err := e.InferCore(c, tm.P)
		if err != nil {
			return nil, err
		}
		sig, ok := e.M.Force(pty).(core.VSig)
		if !ok {
			return nil, fmt.Errorf("fst of a non-Σ of type %s", e.pretty(c, pty))
		}
		return sig.Dom, nil
	case core.Snd:
		pty, err := e.InferCore(c, tm.P)
		if err != nil {
			return nil, err
		}
		sig, ok := e.M.Force(pty).(core.VSig)
		if !ok {
			return nil, fmt.Errorf("snd of a non-Σ of type %s", e.pretty(c, pty))
		}
		return sig.Cod(e.Eval(c, core.Fst{P: tm.P})), nil
	case core.NatLit:
		// A compressed numeral inhabits its nat binding's type — the type of its
		// zero constructor (NatLit is definitionally succ^N zero of that nat).
		return e.refType(tm.Zero)
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
