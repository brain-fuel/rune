package elaborate

import (
	"fmt"

	"goforge.dev/rune/core"
)

// Pattern unification (Phase 2, Miller fragment). Unify is conversion's
// elaboration-time sibling: where core.Conv answers yes/no on meta-free values,
// Unify additionally SOLVES metavariables — a flexible value (a neutral headed
// by an unsolved meta applied to distinct bound variables) is solved by
// inverting its spine and reading the other side back under that inversion,
// with an occurs check. Forcing during unification logs dependencies exactly
// like conversion; the judgment's U is unaffected by who asks.

// Unify makes a and b definitionally equal at level lvl, solving metas as
// needed, or reports why it cannot.
func (e *Elaborator) Unify(lvl int, a, b core.Val) error {
	m := e.M
	a, b = e.forceFlex(a), e.forceFlex(b)

	// η for functions: if either side is a lambda, compare applied.
	_, aLam := a.(core.VLam)
	_, bLam := b.(core.VLam)
	if aLam || bLam {
		v := core.VVar(lvl)
		return e.Unify(lvl+1, m.Apply(a, v), m.Apply(b, v))
	}

	// Flexible sides: try to solve. In the flex-flex case the two metas may
	// have different spines; if solving one direction falls outside the pattern
	// fragment (e.g. the right side mentions a variable the left spine lacks),
	// the other direction may still be a pattern — try both.
	aID, aSpine, aFlex := flexHead(a)
	bID, bSpine, bFlex := flexHead(b)
	switch {
	case aFlex && bFlex && aID == bID:
		// Same meta on both sides: succeed if the spines unify.
		return e.unifySpine(lvl, a.(core.VNeu).Spine, b.(core.VNeu).Spine)
	case aFlex && bFlex:
		if err := e.solve(lvl, aID, aSpine, b); err != nil {
			return e.solve(lvl, bID, bSpine, a)
		}
		return nil
	case aFlex:
		return e.solve(lvl, aID, aSpine, b)
	case bFlex:
		return e.solve(lvl, bID, bSpine, a)
	}

	switch x := a.(type) {
	case core.VU:
		if _, ok := b.(core.VU); ok {
			return nil
		}
	case core.VProp:
		if _, ok := b.(core.VProp); ok {
			return nil
		}
	case core.VEq:
		if y, ok := b.(core.VEq); ok {
			if err := e.Unify(lvl, x.Ty, y.Ty); err != nil {
				return err
			}
			if err := e.Unify(lvl, x.L, y.L); err != nil {
				return err
			}
			return e.Unify(lvl, x.R, y.R)
		}
	case core.VRefl:
		// Proof irrelevance: any two refl proofs are equal.
		if _, ok := b.(core.VRefl); ok {
			return nil
		}
	case core.VPi:
		if y, ok := b.(core.VPi); ok {
			if x.Icit != y.Icit {
				return fmt.Errorf("plicity mismatch: %s vs %s", icitName(x.Icit), icitName(y.Icit))
			}
			if err := e.Unify(lvl, x.Dom, y.Dom); err != nil {
				return err
			}
			v := core.VVar(lvl)
			return e.Unify(lvl+1, x.Cod(v), y.Cod(v))
		}
	case core.VNeu:
		if y, ok := b.(core.VNeu); ok {
			// Fast path: rigid spines that agree syntactically.
			if err := e.unifySpine(lvl, x.Spine, y.Spine); err == nil {
				return nil
			}
			// δ: force whichever sides unfold and retry, like conversion.
			af, aok := e.delta(a)
			bf, bok := e.delta(b)
			if aok || bok {
				return e.Unify(lvl, af, bf)
			}
			return fmt.Errorf("cannot unify %s with %s",
				e.prettyV(lvl, a), e.prettyV(lvl, b))
		}
	}

	// Rigid mismatch — δ-unfold a neutral side if possible.
	af, aok := e.delta(a)
	bf, bok := e.delta(b)
	if aok || bok {
		return e.Unify(lvl, af, bf)
	}
	return fmt.Errorf("cannot unify %s with %s", e.prettyV(lvl, a), e.prettyV(lvl, b))
}

// forceFlex forces solved-meta unfoldings (and nothing else is lost by forcing
// δ here: glued unfoldings are forced lazily in delta instead).
func (e *Elaborator) forceFlex(v core.Val) core.Val {
	for {
		n, ok := v.(core.VNeu)
		if !ok {
			return v
		}
		id, _, isFlex := flexHead(v)
		if !isFlex {
			return v
		}
		if _, solved := e.metas.Solution(id); !solved {
			return v
		}
		if n.Unfold == nil {
			return v
		}
		v = n.Unfold()
	}
}

// delta unfolds one glued (definition-headed) layer if possible.
func (e *Elaborator) delta(v core.Val) (core.Val, bool) {
	n, ok := v.(core.VNeu)
	if !ok || n.Unfold == nil {
		return v, false
	}
	if _, _, isFlex := flexHead(v); isFlex {
		return v, false // unsolved metas don't δ-unfold
	}
	return n.Unfold(), true
}

// flexHead reports the unsolved-or-solved meta heading a neutral, with its
// argument spine outermost-last.
func flexHead(v core.Val) (id int, spine []spineArg, ok bool) {
	n, isNeu := v.(core.VNeu)
	if !isNeu {
		return 0, nil, false
	}
	cur := n.Spine
	for {
		switch s := cur.(type) {
		case core.NApp:
			spine = append([]spineArg{{val: s.Arg, icit: s.Icit}}, spine...)
			cur = s.Fn
		case core.NMeta:
			return s.ID, spine, true
		default:
			return 0, nil, false
		}
	}
}

type spineArg struct {
	val  core.Val
	icit core.Icit
}

// unifySpine unifies two rigid spines structurally.
func (e *Elaborator) unifySpine(lvl int, p, q core.Neutral) error {
	switch x := p.(type) {
	case core.NVar:
		if y, ok := q.(core.NVar); ok && x.Lvl == y.Lvl {
			return nil
		}
	case core.NRef:
		if y, ok := q.(core.NRef); ok && x.Hash == y.Hash {
			return nil
		}
	case core.NMeta:
		if y, ok := q.(core.NMeta); ok && x.ID == y.ID {
			return nil
		}
	case core.NCast:
		// Conversion skips the proof (irrelevance).
		if y, ok := q.(core.NCast); ok {
			if err := e.Unify(lvl, x.A, y.A); err != nil {
				return err
			}
			if err := e.Unify(lvl, x.B, y.B); err != nil {
				return err
			}
			return e.Unify(lvl, x.X, y.X)
		}
	case core.NApp:
		if y, ok := q.(core.NApp); ok && x.Icit == y.Icit {
			if err := e.unifySpine(lvl, x.Fn, y.Fn); err != nil {
				return err
			}
			return e.Unify(lvl, x.Arg, y.Arg)
		}
	}
	return fmt.Errorf("spine mismatch")
}

// solve solves meta id with rhs, given the meta's argument spine. The spine
// must be distinct bound variables (the pattern condition); rhs is read back
// under the inverted spine with an occurs check, wrapped in one lambda per
// spine entry, and recorded.
func (e *Elaborator) solve(lvl int, id int, spine []spineArg, rhs core.Val) error {
	ren, err := invert(e, lvl, spine)
	if err != nil {
		return fmt.Errorf("cannot solve ?%d: %w", id, err)
	}
	body, err := e.rename(id, ren, lvl, rhs)
	if err != nil {
		return fmt.Errorf("cannot solve ?%d: %w", id, err)
	}
	for i := len(spine) - 1; i >= 0; i-- {
		body = core.Lam{Icit: spine[i].icit, Body: core.Scope{Name: "x", Body: body}}
	}
	e.metas.entries[id].solution = e.M.Eval(nil, body)
	return nil
}

// renaming maps source de Bruijn levels to target levels.
type renaming struct {
	dom int // source depth (lvl at the solve site)
	cod int // target depth (number of spine binders)
	m   map[int]int
}

// invert builds the partial renaming from a pattern spine: each argument must
// force to a distinct rigid variable.
func invert(e *Elaborator, lvl int, spine []spineArg) (renaming, error) {
	ren := renaming{dom: lvl, cod: len(spine), m: map[int]int{}}
	for i, a := range spine {
		v := e.M.Force(a.val)
		n, ok := v.(core.VNeu)
		if !ok {
			return ren, fmt.Errorf("non-variable in meta spine (outside the pattern fragment)")
		}
		nv, ok := n.Spine.(core.NVar)
		if !ok {
			return ren, fmt.Errorf("non-variable in meta spine (outside the pattern fragment)")
		}
		if _, dup := ren.m[nv.Lvl]; dup {
			return ren, fmt.Errorf("duplicate variable in meta spine (outside the pattern fragment)")
		}
		ren.m[nv.Lvl] = i
	}
	return ren, nil
}

// rename reads rhs back as a term in the meta's solution scope, applying the
// renaming to free variables, failing on escapees and on occurrences of the
// meta being solved (the occurs check).
func (e *Elaborator) rename(id int, ren renaming, srcLvl int, v core.Val) (core.Tm, error) {
	v = e.forceFlex(v)
	switch x := v.(type) {
	case core.VU:
		return core.Univ{}, nil
	case core.VProp:
		return core.Prop{}, nil
	case core.VEq:
		ty, err := e.rename(id, ren, srcLvl, x.Ty)
		if err != nil {
			return nil, err
		}
		l, err := e.rename(id, ren, srcLvl, x.L)
		if err != nil {
			return nil, err
		}
		r, err := e.rename(id, ren, srcLvl, x.R)
		if err != nil {
			return nil, err
		}
		return core.Eq{Ty: ty, L: l, R: r}, nil
	case core.VRefl:
		tm, err := e.rename(id, ren, srcLvl, x.V)
		if err != nil {
			return nil, err
		}
		return core.Refl{Tm: tm}, nil
	case core.VPi:
		dom, err := e.rename(id, ren, srcLvl, x.Dom)
		if err != nil {
			return nil, err
		}
		ren2 := ren.lift(srcLvl)
		cod, err := e.rename(id, ren2, srcLvl+1, x.Cod(core.VVar(srcLvl)))
		if err != nil {
			return nil, err
		}
		return core.Pi{Icit: x.Icit, Dom: dom, Cod: core.Scope{Name: x.Name, Body: cod}}, nil
	case core.VLam:
		ren2 := ren.lift(srcLvl)
		body, err := e.rename(id, ren2, srcLvl+1, x.Body(core.VVar(srcLvl)))
		if err != nil {
			return nil, err
		}
		return core.Lam{Icit: x.Icit, Body: core.Scope{Name: x.Name, Body: body}}, nil
	case core.VNeu:
		return e.renameSpine(id, ren, srcLvl, x.Spine)
	default:
		return nil, fmt.Errorf("unknown value in rename")
	}
}

func (e *Elaborator) renameSpine(id int, ren renaming, srcLvl int, n core.Neutral) (core.Tm, error) {
	switch s := n.(type) {
	case core.NVar:
		tgt, ok := ren.m[s.Lvl]
		if !ok {
			return nil, fmt.Errorf("solution would capture a variable that escapes the meta's scope")
		}
		return core.Var{Idx: ren.cod - 1 - tgt}, nil
	case core.NRef:
		return core.Ref{Hash: s.Hash}, nil
	case core.NMeta:
		if s.ID == id {
			return nil, fmt.Errorf("occurs check: ?%d would appear in its own solution", id)
		}
		return core.Meta{ID: s.ID}, nil
	case core.NCast:
		a, err := e.rename(id, ren, srcLvl, s.A)
		if err != nil {
			return nil, err
		}
		b, err := e.rename(id, ren, srcLvl, s.B)
		if err != nil {
			return nil, err
		}
		pr, err := e.rename(id, ren, srcLvl, s.P)
		if err != nil {
			return nil, err
		}
		x2, err := e.rename(id, ren, srcLvl, s.X)
		if err != nil {
			return nil, err
		}
		return core.Cast{A: a, B: b, P: pr, X: x2}, nil
	case core.NApp:
		fn, err := e.renameSpine(id, ren, srcLvl, s.Fn)
		if err != nil {
			return nil, err
		}
		arg, err := e.rename(id, ren, srcLvl, s.Arg)
		if err != nil {
			return nil, err
		}
		return core.App{Fn: fn, Arg: arg, Icit: s.Icit}, nil
	default:
		return nil, fmt.Errorf("unknown neutral in rename")
	}
}

// lift extends a renaming under one new binder: source level srcLvl maps to the
// next target slot.
func (r renaming) lift(srcLvl int) renaming {
	m := make(map[int]int, len(r.m)+1)
	for k, v := range r.m {
		m[k] = v
	}
	m[srcLvl] = r.cod
	return renaming{dom: r.dom + 1, cod: r.cod + 1, m: m}
}

func icitName(i core.Icit) string {
	if i == core.Impl {
		return "implicit"
	}
	return "explicit"
}

// prettyV renders a value for an error message.
func (e *Elaborator) prettyV(lvl int, v core.Val) string {
	return e.prettyTm(e.M.Quote(lvl, v))
}
