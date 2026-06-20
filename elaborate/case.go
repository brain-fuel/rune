package elaborate

import (
	"fmt"

	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/store"
	"goforge.dev/rune/v3/surface"
)

// Case elaboration (ergonomics rung 4). `case s of | C a* [with ih*] -> e | … end`
// is sugar for ONE saturated application of the scrutinee type's eliminator:
//
//	DElim params* motive branch* s
//
// The clause set must be exactly the constructors of the scrutinee's data type
// (coverage stays by construction), each branch becomes the eliminator's case
// lambda (constructor arguments in order, then the induction hypotheses the
// eliminator already provides for recursive arguments), and the motive comes
// from the EXPECTED type — generalized over the scrutinee when it is a bound
// variable, constant otherwise. The output is a plain core application: the
// store, the proof cache, and codegen never learn that case exists. Totality
// remains by construction — no recursion enters the language here.

// dataDecls is the view of the store the case elaborator needs: the full
// declaration of a data former. *store.Store implements it; it reaches the
// elaborator through the Machine's DataInfo.
type dataDecls interface {
	DataDeclOf(core.Hash) (store.DataDecl, []core.Hash, core.Hash, bool)
}

func (e *Elaborator) elabCase(c *Ctx, s surface.ECase, want core.Val) (core.Tm, error) {
	fail := func(format string, args ...any) (core.Tm, error) {
		return nil, fmt.Errorf("case: %s", fmt.Sprintf(format, args...))
	}

	dd, ok := e.M.Data.(dataDecls)
	if !ok {
		return fail("no datatype declarations available in this elaborator")
	}

	// The scrutinee, inferred first: its type names the eliminator.
	stm, sty, err := e.Infer(c, s.Scrut)
	if err != nil {
		return nil, err
	}
	styTm := e.M.Quote(c.Lvl(), e.M.Force(sty))
	head, params := appSpine(styTm)
	ref, isRef := head.(core.Ref)
	if !isRef {
		return fail("the scrutinee has type %s, which is not a datatype", e.pretty(c, sty))
	}
	decl, _, elimHash, isData := dd.DataDeclOf(ref.Hash)
	if !isData {
		return fail("the scrutinee has type %s, which is not a datatype", e.pretty(c, sty))
	}
	if len(params) != decl.NumParams {
		return fail("the scrutinee's type %s does not saturate %s's parameters", e.pretty(c, sty), decl.Name)
	}

	// The clause set must be the constructor set, exactly.
	byCtor := map[string]*surface.CaseClause{}
	for i := range s.Clauses {
		cl := &s.Clauses[i]
		if byCtor[cl.Ctor] != nil {
			return fail("duplicate clause for constructor %s", cl.Ctor)
		}
		byCtor[cl.Ctor] = cl
	}
	for i := range s.Clauses {
		name := s.Clauses[i].Ctor
		if !containsName(decl.CtorNames, name) {
			return nil, caseUnknownCtorError(name, decl.Name, decl.CtorNames)
		}
	}
	var missing []string
	for _, name := range decl.CtorNames {
		if byCtor[name] == nil {
			missing = append(missing, name)
		}
	}
	if len(missing) > 0 {
		return nil, caseMissingClausesError(decl.Name, decl.CtorNames, missing)
	}

	// The motive, from the expected type: λx. want, generalized over the
	// scrutinee when it is a lambda/Pi-bound variable, constant otherwise.
	wantTm := e.M.Quote(c.Lvl(), want)
	var motive core.Tm
	motiveName := "x"
	if v, isVar := s.Scrut.(surface.EVar); isVar {
		if idx, _, bound := c.lookup(v.Name); bound && c.bound[idx] {
			motive = core.Lam{Body: core.Scope{Name: v.Name, Body: abstractVar(wantTm, idx)}}
			motiveName = v.Name
		}
	}
	if motive == nil {
		motive = core.Lam{Body: core.Scope{Name: motiveName, Body: shift(wantTm, 0, 1)}}
	}

	// Assemble the saturated eliminator application, checking each branch
	// against the eliminator's own case type as the application advances.
	fnTm := core.Tm(core.Ref{Hash: elimHash})
	fnTy, err := e.refType(elimHash)
	if err != nil {
		return nil, err
	}
	applyCore := func(arg core.Tm) error {
		pi, ok := e.M.Force(fnTy).(core.VPi)
		if !ok {
			return fmt.Errorf("case: eliminator over-applied (impossible)")
		}
		fnTm = core.App{Fn: fnTm, Arg: arg, Icit: core.Expl}
		fnTy = pi.Cod(e.Eval(c, arg))
		return nil
	}
	for _, paramTm := range params {
		if err := applyCore(paramTm); err != nil {
			return nil, err
		}
	}
	if err := applyCore(motive); err != nil {
		return nil, err
	}
	for i, ctorName := range decl.CtorNames {
		cl := byCtor[ctorName]
		sig := decl.Sigs[i]
		if len(cl.Binders) != sig.Arity {
			return fail("constructor %s takes %d argument(s), the clause binds %d",
				ctorName, sig.Arity, len(cl.Binders))
		}
		nIH := 0
		for _, r := range sig.Rec {
			if r {
				nIH++
			}
		}
		if len(cl.IHs) > nIH {
			if nIH == 0 {
				return fail("constructor %s has no recursive argument: 'with' binds nothing here", ctorName)
			}
			return fail("constructor %s has %d recursive argument(s), 'with' names %d", ctorName, nIH, len(cl.IHs))
		}
		// The branch: binders for the constructor arguments, then the IHs the
		// eliminator provides (argument order). Unnamed IHs bind fresh and
		// unused. Domains are holes — checking against the case type fills them.
		branch := cl.Body
		for j := nIH - 1; j >= 0; j-- {
			name := "_"
			if j < len(cl.IHs) {
				name = cl.IHs[j]
			}
			branch = surface.ELam{Param: name, Dom: surface.EHole{}, Body: branch}
		}
		for j := len(cl.Binders) - 1; j >= 0; j-- {
			branch = surface.ELam{Param: cl.Binders[j], Dom: surface.EHole{}, Body: branch}
		}
		pi, ok := e.M.Force(fnTy).(core.VPi)
		if !ok {
			return fail("eliminator over-applied (impossible)")
		}
		brTm, err := e.Check(c, branch, pi.Dom)
		if err != nil {
			return nil, fmt.Errorf("case: clause %s: %w", ctorName, err)
		}
		fnTm = core.App{Fn: fnTm, Arg: brTm, Icit: core.Expl}
		fnTy = pi.Cod(e.Eval(c, brTm))
	}
	if err := applyCore(stm); err != nil {
		return nil, err
	}

	// By construction fnTy is the motive at the scrutinee, i.e. the expected
	// type; the unification is cheap insurance, not a new obligation.
	if err := e.Unify(c.Lvl(), fnTy, want); err != nil {
		return fail("the result type %s does not meet the expected %s (%v)",
			e.pretty(c, fnTy), e.pretty(c, want), err)
	}
	return fnTm, nil
}

// appSpine splits a core application chain into its head and explicit arguments.
func appSpine(t core.Tm) (core.Tm, []core.Tm) {
	var args []core.Tm
	for {
		app, ok := t.(core.App)
		if !ok {
			return t, args
		}
		args = append([]core.Tm{app.Arg}, args...)
		t = app.Fn
	}
}

// abstractVar closes t over the variable with de Bruijn index idx, producing a
// body suitable for a new binder: occurrences of idx become the bound variable,
// every other free index shifts up by one.
func abstractVar(t core.Tm, idx int) core.Tm {
	var walk func(t core.Tm, depth int) core.Tm
	walk = func(t core.Tm, depth int) core.Tm {
		switch x := t.(type) {
		case nil:
			return nil
		case core.Var:
			switch {
			case x.Idx == idx+depth:
				return core.Var{Idx: depth}
			case x.Idx >= depth:
				return core.Var{Idx: x.Idx + 1}
			default:
				return x
			}
		case core.Lam:
			return core.Lam{Icit: x.Icit, Qty: x.Qty,
				Body: core.Scope{Name: x.Body.Name, Body: walk(x.Body.Body, depth+1)}}
		case core.Pi:
			return core.Pi{Icit: x.Icit, Qty: x.Qty, Dom: walk(x.Dom, depth),
				Cod: core.Scope{Name: x.Cod.Name, Body: walk(x.Cod.Body, depth+1)}}
		case core.App:
			return core.App{Fn: walk(x.Fn, depth), Arg: walk(x.Arg, depth), Icit: x.Icit}
		case core.Let:
			return core.Let{Ty: walk(x.Ty, depth), Val: walk(x.Val, depth),
				Body: core.Scope{Name: x.Body.Name, Body: walk(x.Body.Body, depth+1)}}
		case core.Ann:
			return core.Ann{Term: walk(x.Term, depth), Ty: walk(x.Ty, depth)}
		case core.Eq:
			return core.Eq{Ty: walk(x.Ty, depth), L: walk(x.L, depth), R: walk(x.R, depth)}
		case core.Refl:
			return core.Refl{Tm: walk(x.Tm, depth)}
		case core.Cast:
			return core.Cast{A: walk(x.A, depth), B: walk(x.B, depth),
				P: walk(x.P, depth), X: walk(x.X, depth)}
		case core.Subst:
			return core.Subst{A: walk(x.A, depth), X: walk(x.X, depth), Y: walk(x.Y, depth),
				Prf: walk(x.Prf, depth), P: walk(x.P, depth), Px: walk(x.Px, depth)}
		default:
			// Ref, Univ, Prop, Meta carry no variables.
			return t
		}
	}
	return walk(t, 0)
}

func containsName(xs []string, s string) bool {
	for _, x := range xs {
		if x == s {
			return true
		}
	}
	return false
}
