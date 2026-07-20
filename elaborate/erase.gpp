package elaborate

import (
	"fmt"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/core"
)

// TypedEraser lowers checked, meta-free core to the erased IR WITH the types in
// hand. The syntactic codegen.Erase cannot see two erasure boundaries, so it
// leaked them into the shadow:
//
//   - Proof irrelevance. A subterm whose TYPE is a Prop is computationally
//     vacuous however it was built — not only when it is syntactically `refl`.
//     codegen.Erase units only Refl, so a proof assembled by applying ordinary
//     ω helpers (`cong succ p`, `trans a b`) was walked, and any deep numeral
//     it mentioned (an equation's endpoints) was emitted — node's parser dies
//     ~1600-deep. A proof argument's expected type forces to a Prop-sorted type
//     (`Eq A x y : Prop`), so we unit the whole subterm and never descend.
//   - 0-quantity arguments. A `(0 x : A)` position is erased data; its argument
//     is a unit at the call site. Positions are kept (no arity surgery — the
//     erased binder still receives a unit), matching the IR discipline.
//
// This plugs the 0-fragment call-site leak recorded in
// ref_docs/rune-verified-implementations.md. The pass mirrors CheckCore: every
// definition body arrives in CHECK mode against its declared type, so each
// binder's domain type is known and proof positions are caught where they are
// used. It logs dependencies into the Machine exactly as the checker does;
// that is harmless at emit time.
type TypedEraser struct {
	el       *Elaborator
	names    map[core.Hash]string
	typeRefs map[core.Hash]bool
	foreign  map[core.Hash]bool
	lower    map[core.Hash]core.Hash
}

// NewEraser builds a type-directed eraser over el's store, using the same
// emit-name and type-former maps codegen.Erase consumes.
func NewEraser(el *Elaborator, names map[core.Hash]string, typeRefs map[core.Hash]bool) *TypedEraser {
	return &TypedEraser{el: el, names: names, typeRefs: typeRefs}
}

// SetForeign marks the hashes that denote `foreign` axioms (R-FFI): a reference
// to one erases to a codegen.IForeign host accessor, not an emitted IGlobal.
func (z *TypedEraser) SetForeign(foreign map[core.Hash]bool) { z.foreign = foreign }

// SetLowering installs the proof-gated lowering table (slow-hash -> fast-hash,
// v4 Ord Plan C): a top-level reference whose hash appears as a key redirects
// to its verified native twin BEFORE the typeRefs/foreign/names lookup, so
// every backend inherits the rewrite from this single choke point. This is the
// erasure the session emit path actually drives (see codegen.Erase's doc
// comment); a nil or unset table disables redirection.
func (z *TypedEraser) SetLowering(lower map[core.Hash]core.Hash) { z.lower = lower }

// Def erases a definition body checked against its declared type ty.
func (z *TypedEraser) Def(ty, body core.Tm) (codegen.Ir, error) {
	c := &Ctx{}
	want := z.el.Eval(c, ty)
	return z.check(c, body, want)
}

// Expr erases a standalone expression (the REPL `:run` seam). Its type is
// inferred so proof irrelevance still applies at the top.
func (z *TypedEraser) Expr(t core.Tm) (codegen.Ir, error) {
	c := &Ctx{}
	want, err := z.el.InferCore(c, t)
	if err != nil {
		return nil, err
	}
	return z.check(c, t, want)
}

// check erases t whose expected type is want. A term whose type is a Prop
// erases to the unit token by proof irrelevance, full stop.
func (z *TypedEraser) check(c *Ctx, t core.Tm, want core.Val) (codegen.Ir, error) {
	if want != nil && z.isProp(c, want) {
		return codegen.IUnit{}, nil
	}
	switch tm := t.(type) {
	case core.Var:
		return codegen.IVar{Idx: tm.Idx}, nil
	case core.Ref:
		h := tm.Hash
		if z.lower != nil {
			if to, ok := z.lower[h]; ok {
				h = to
			}
		}
		if z.typeRefs[h] {
			return codegen.IUnit{}, nil
		}
		n, ok := z.names[h]
		if !ok {
			n = "$" + h.Short()
		}
		if z.foreign[h] {
			return codegen.IForeign{Name: n}, nil
		}
		return codegen.IGlobal{Name: n}, nil
	case core.Univ, core.Prop, core.Pi, core.Eq, core.Refl, core.Sig:
		// Types and proofs are build-time discipline; the shadow keeps a unit.
		return codegen.IUnit{}, nil
	case core.Lam:
		pi, ok := z.el.M.Force(want).(core.VPi)
		if !ok {
			return nil, fmt.Errorf("codegen: lambda erased against non-function type")
		}
		v := core.VVar(c.Lvl())
		body, err := z.check(c.bind(tm.Body.Name, pi.Dom), tm.Body.Body, pi.Cod(v))
		if err != nil {
			return nil, err
		}
		return codegen.ILam{Name: tm.Body.Name, Body: body}, nil
	case core.App:
		fnTy, err := z.el.InferCore(c, tm.Fn)
		if err != nil {
			return nil, err
		}
		fn, err := z.check(c, tm.Fn, fnTy)
		if err != nil {
			return nil, err
		}
		if _, isUnit := fn.(codegen.IUnit); isUnit {
			// A unit head is an erased type former (List, Quot, …): the whole
			// application denotes a type and erases with it.
			return codegen.IUnit{}, nil
		}
		pi, ok := z.el.M.Force(fnTy).(core.VPi)
		if !ok {
			return nil, fmt.Errorf("codegen: applying a non-function of inferred type during erasure")
		}
		var arg codegen.Ir
		if pi.Qty == core.QZero {
			// 0-quantity argument: erased data, a unit at the call site. The
			// position is kept (no arity surgery), so the erased binder still
			// receives this unit.
			arg = codegen.IUnit{}
		} else {
			arg, err = z.check(c, tm.Arg, pi.Dom)
			if err != nil {
				return nil, err
			}
		}
		return codegen.IApp{Fn: fn, Arg: arg}, nil
	case core.Let:
		var vty core.Val
		if tm.Ty != nil {
			vty = z.el.Eval(c, tm.Ty)
		} else {
			inferred, err := z.el.InferCore(c, tm.Val)
			if err != nil {
				return nil, err
			}
			vty = inferred
		}
		val, err := z.check(c, tm.Val, vty)
		if err != nil {
			return nil, err
		}
		vval := z.el.Eval(c, tm.Val)
		body, err := z.check(c.define(tm.Body.Name, vty, vval), tm.Body.Body, want)
		if err != nil {
			return nil, err
		}
		return codegen.ILet{Name: tm.Body.Name, Val: val, Body: body}, nil
	case core.Ann:
		return z.check(c, tm.Term, z.el.Eval(c, tm.Ty))
	case core.Cast:
		// cast computes on types, which are gone: the subject is the payload,
		// and it has the source type A.
		return z.check(c, tm.X, z.el.Eval(c, tm.A))
	case core.Subst:
		// Transport is the identity on its computational payload Px : P x.
		vp := z.el.Eval(c, tm.P)
		vx := z.el.Eval(c, tm.X)
		return z.check(c, tm.Px, z.el.M.Apply(vp, vx))
	case core.Pair:
		// A dependent pair erases to a 2-tuple of its components.
		vsig, ok := z.el.Eval(c, core.Sig{Dom: tm.Dom, Cod: tm.Cod}).(core.VSig)
		if !ok {
			return nil, fmt.Errorf("codegen: pair at a non-Σ type")
		}
		a, err := z.check(c, tm.A, vsig.Dom)
		if err != nil {
			return nil, err
		}
		b, err := z.check(c, tm.B, vsig.Cod(z.el.Eval(c, tm.A)))
		if err != nil {
			return nil, err
		}
		return codegen.IPair{A: a, B: b}, nil
	case core.Fst:
		pty, err := z.el.InferCore(c, tm.P)
		if err != nil {
			return nil, err
		}
		p, err := z.check(c, tm.P, pty)
		if err != nil {
			return nil, err
		}
		return codegen.IFst{P: p}, nil
	case core.Snd:
		pty, err := z.el.InferCore(c, tm.P)
		if err != nil {
			return nil, err
		}
		p, err := z.check(c, tm.P, pty)
		if err != nil {
			return nil, err
		}
		return codegen.ISnd{P: p}, nil
	case core.NatLit:
		// C7 / R-NUM Decision 4: a compressed numeral erases to the backend's
		// NATIVE integer in O(1) source, NOT the O(n) `succ^N zero` IGlobal chain
		// it lowered to before. The builtin-nat group is already compiled to the
		// host's native integer by NatSpec (`emitNat*`: JS BigInt, Python int, Go
		// int, Rust/JVM V::Int, BEAM integer), and the pre-C7 succ-chain only
		// EVALUATED to that native integer at runtime — so emitting it directly is
		// observationally identical (same `$show`, same behavior under succ /
		// NatElim / accelerated arithmetic, all of which act on the native int),
		// just compact. A source `5000` deploys as `5000n` (JS) instead of a
		// 5000-deep chain that crashes the host parser. The magnitude rides as a
		// canonical decimal string so an arbitrary-precision numeral survives to
		// the arbitrary-precision backends. (`LitNat`, not `LitInt`: nat is a
		// BigInt on JS, where `x + 1n` rejects a plain Number — LitNat matches the
		// NatSpec representation per backend.)
		return codegen.ILit{Kind: codegen.LitNat, Nat: tm.N.String()}, nil
	default:
		return nil, fmt.Errorf("codegen typed erase: unexpected core term %T (metavariable or unknown constructor)", t)
	}
}

// isProp reports whether the type ty is Prop-sorted — i.e. its inhabitants are
// proofs, erased by proof irrelevance. ty is a checked type value, so taking
// its sort cannot fail in practice; a defensive error reads as "not a Prop".
func (z *TypedEraser) isProp(c *Ctx, ty core.Val) bool {
	sort, err := z.el.checkTypeCore(c, z.el.M.Quote(c.Lvl(), ty))
	if err != nil {
		return false
	}
	_, prop := sort.(core.VProp)
	return prop
}
