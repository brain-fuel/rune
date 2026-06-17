package codegen

import "fmt"

// LowerElim lowers a datatype's eliminator to the erased IR ONCE — the R-IR / B2
// keystone. Today each backend re-implements the fold-with-induction-hypotheses by
// hand (emitElim); the lowered form expresses it as ordinary IR (curried lambdas
// over the parameters, motive, cases, and scrutinee; an ICase tag dispatch whose
// arms project the scrutinee's fields with IField and call the eliminator
// recursively for the induction hypotheses) so a backend only needs to render
// ICase/IField — no eliminator-specific code. The recursion references the
// eliminator by its own global name (as the per-backend emit already does).
//
// Binder order (outer→inner) is p0…p_{NP-1}, m, c0…c_{n-1}, x, matching the curried
// application order the call sites use. De Bruijn indices below are relative to the
// body, where x is innermost (index 0).
func LowerElim(d DataSpec) Ir {
	np := d.NumParams
	n := len(d.Ctors)
	// Index of each binder as seen from the body (x innermost = 0).
	xIdx := 0
	cIdx := func(i int) int { return n - i }          // c0 = n … c_{n-1} = 1
	mIdx := n + 1                                     //
	pIdx := func(k int) int { return n + 1 + np - k } // p0 = n+1+np … p_{np-1} = n+2

	arms := make([]ICaseArm, n)
	for i, c := range d.Ctors {
		own := c.Arity - np
		expr := Ir(IVar{Idx: cIdx(i)}) // the case for this constructor
		// Apply the case to the constructor's own arguments…
		for j := 0; j < own; j++ {
			expr = IApp{Fn: expr, Arg: IField{Scrut: IVar{Idx: xIdx}, Index: np + j}}
		}
		// …then, for each recursive argument, to its induction hypothesis (the
		// eliminator applied recursively to that field).
		for j := 0; j < own; j++ {
			if d.Rec[i][j] {
				rec := Ir(IGlobal{Name: d.ElimName})
				for k := 0; k < np; k++ {
					rec = IApp{Fn: rec, Arg: IVar{Idx: pIdx(k)}}
				}
				rec = IApp{Fn: rec, Arg: IVar{Idx: mIdx}}
				for k := 0; k < n; k++ {
					rec = IApp{Fn: rec, Arg: IVar{Idx: cIdx(k)}}
				}
				ih := IApp{Fn: rec, Arg: IField{Scrut: IVar{Idx: xIdx}, Index: np + j}}
				expr = IApp{Fn: expr, Arg: ih}
			}
		}
		arms[i] = ICaseArm{Tag: i, Body: expr}
	}

	body := Ir(ICase{Scrut: IVar{Idx: xIdx}, Arms: arms})
	// Wrap the binders inside-out: x first (innermost), then the cases, motive,
	// and parameters.
	body = ILam{Name: "x", Body: body}
	for i := n - 1; i >= 0; i-- {
		body = ILam{Name: fmt.Sprintf("c%d", i), Body: body}
	}
	body = ILam{Name: "m", Body: body}
	for k := np - 1; k >= 0; k-- {
		body = ILam{Name: fmt.Sprintf("p%d", k), Body: body}
	}
	return body
}
