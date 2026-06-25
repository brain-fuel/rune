package codegen_test

import (
	"fmt"
	"testing"

	cg "goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/session"
)

// closure_test.go — closure conversion SEMANTICS PRESERVATION (the M4 gate).
//
// For each conformance-corpus program we evaluate the ORIGINAL erased IR and the
// CLOSURE-CONVERTED IR with two independent direct evaluators and require an
// IDENTICAL observable value. This proves ClosureConvert preserves meaning
// without leaning on any host backend. We also check the lifted code blocks are
// CLOSED — the invariant a native (LLVM/Cranelift) backend depends on.

type corpusEntry struct {
	code string
	main string
	want string
}

var natDefs = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
add : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) n (fn (k : Nat) (ih : Nat) is succ ih end) m
  end
end
`

var listDefs = natDefs + `
data List : U -> U is
  nil : (A : U) -> List A
| cons : (A : U) -> A -> List A -> List A
end
length : (A : U) -> List A -> Nat is
  fn (A : U) (xs : List A) is
    ListElim A (fn (x : List A) is Nat end)
      zero
      (fn (x : A) (rest : List A) (ih : Nat) is succ ih end)
      xs
  end
end
`

var closureCorpus = []corpusEntry{
	{natDefs + `three : Nat is add (succ zero) (succ (succ zero)) end`, "three", "succ (succ (succ zero))"},
	{listDefs + `two : Nat is length Nat (cons Nat zero (cons Nat (succ zero) (nil Nat))) end`, "two", "succ (succ zero)"},
	{natDefs + `
lemma : (n : Nat) -> Eq Nat n n is fn (n : Nat) is refl n end end
carried : Nat -> Nat is
  fn (n : Nat) is
    subst Nat n n (lemma n) (fn (z : Nat) is Nat end) (succ n)
  end
end
one : Nat is carried zero end`, "one", "succ zero"},
}

func mustEmitProgram(t *testing.T, src, main string) cg.Program {
	t.Helper()
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load: %v", err)
	}
	p, err := s.EmitProgram(main)
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	return p
}

func TestClosureConvertPreservesSemantics(t *testing.T) {
	for _, c := range closureCorpus {
		t.Run(c.main, func(t *testing.T) {
			p := mustEmitProgram(t, c.code, c.main)
			cp := cg.ClosureConvert(p)

			so := showVal(evalProgram(t, p))
			sc := showVal(evalCProgram(t, cp))
			if so != sc {
				t.Fatalf("semantics differ: original=%q converted=%q", so, sc)
			}
			if c.want != "" && so != c.want {
				t.Fatalf("got %q, want %q", so, c.want)
			}
		})
	}
}

// TestCodeBlocksAreClosed: after conversion every lifted code block references
// nothing beyond its argument (CVar, shifted by inner CLet binders) and its
// environment (CEnv) — a code block compiles to a standalone native function.
func TestCodeBlocksAreClosed(t *testing.T) {
	for _, c := range closureCorpus {
		cp := cg.ClosureConvert(mustEmitProgram(t, c.code, c.main))
		for _, blk := range cp.Blocks {
			checkClosed(t, blk.Name, blk.Body)
		}
	}
}

func checkClosed(t *testing.T, blk string, body cg.CIr) {
	t.Helper()
	var walk func(cg.CIr, int)
	walk = func(t2 cg.CIr, depth int) {
		switch x := t2.(type) {
		case cg.CVar:
			if x.Idx >= depth {
				t.Fatalf("block %s: CVar{%d} escapes its %d binders (not closed)", blk, x.Idx, depth)
			}
		case cg.CEnv, cg.CGlobal, cg.CForeign, cg.CUnit, cg.CLit:
		case cg.MkClosure:
			for _, e := range x.Env {
				walk(e, depth)
			}
		case cg.AppClosure:
			walk(x.Clo, depth)
			walk(x.Arg, depth)
		case cg.CLet:
			walk(x.Val, depth)
			walk(x.Body, depth+1)
		case cg.CPair:
			walk(x.A, depth)
			walk(x.B, depth)
		case cg.CFst:
			walk(x.P, depth)
		case cg.CSnd:
			walk(x.P, depth)
		case cg.CField:
			walk(x.Scrut, depth)
		case cg.CCase:
			walk(x.Scrut, depth)
			for _, arm := range x.Arms {
				walk(arm.Body, depth)
			}
		default:
			t.Fatalf("checkClosed: unknown CIr %T", t2)
		}
	}
	walk(body, 1) // the argument is the one binder visible at a block's top
}

// ---- evaluator value domain (shared by both evaluators) ----

type val interface{ isVal() }

type vClos struct {
	env  []val
	body cg.Ir
}
type cClos struct {
	code string
	env  []val
}
type vCon struct {
	tag  int
	name string
	args []val
}
type vUnit struct{}
type vNat struct{ n int }
type goClosure struct{ f func(val) val }

func (vClos) isVal()     {}
func (cClos) isVal()     {}
func (vCon) isVal()      {}
func (vUnit) isVal()     {}
func (vNat) isVal()      {}
func (goClosure) isVal() {}

func goClos(f func(val) val) val { return goClosure{f: f} }

func prependVal(v val, env []val) []val {
	out := make([]val, 0, len(env)+1)
	out = append(out, v)
	return append(out, env...)
}

// evaluator carries the shared metadata both evaluators need.
type evaluator struct {
	ctors   map[string]cg.CtorSpec
	natSpec *cg.NatSpec
	defs    map[string]cg.Ir
	cdefs   map[string]cg.CIr
	cblocks map[string]cg.CodeBlock
}

// ---- reference evaluator over the original Ir ----

func evalProgram(t *testing.T, p cg.Program) val {
	e := &evaluator{ctors: map[string]cg.CtorSpec{}, natSpec: p.Nat, defs: map[string]cg.Ir{}}
	for _, d := range p.Datas {
		for _, c := range d.Ctors {
			e.ctors[c.Name] = c
		}
		if p.Nat == nil || d.ElimName != p.Nat.ElimName {
			e.defs[d.ElimName] = cg.LowerElim(d)
		}
	}
	var main cg.Ir
	for _, def := range p.Defs {
		e.defs[def.Name] = def.Body
		if def.Name == p.Main {
			main = def.Body
		}
	}
	if main == nil {
		t.Fatalf("main %q not found", p.Main)
	}
	return e.evalIr(main, nil)
}

func (e *evaluator) evalIr(t cg.Ir, env []val) val {
	switch x := t.(type) {
	case cg.IVar:
		return env[x.Idx]
	case cg.IGlobal:
		return e.global(x.Name, e.globalIr)
	case cg.IUnit:
		return vUnit{}
	case cg.ILam:
		return vClos{env: env, body: x.Body}
	case cg.IApp:
		return e.apply(e.evalIr(x.Fn, env), e.evalIr(x.Arg, env))
	case cg.ILet:
		return e.evalIr(x.Body, prependVal(e.evalIr(x.Val, env), env))
	case cg.IField:
		return e.evalIr(x.Scrut, env).(vCon).args[x.Index]
	case cg.ICase:
		s := e.evalIr(x.Scrut, env).(vCon)
		for _, arm := range x.Arms {
			if arm.Tag == s.tag {
				return e.evalIr(arm.Body, env)
			}
		}
		panic("evalIr: unmatched tag")
	case cg.IBounce:
		return e.evalIr(x.Call, env) // a bounce is semantically its deferred call
	default:
		panic(fmt.Sprintf("evalIr: unsupported %T", t))
	}
}

func (e *evaluator) globalIr(name string) val { return e.evalIr(e.defs[name], nil) }

// ---- evaluator over the closure-converted CIr ----

func evalCProgram(t *testing.T, cp cg.ClosureProgram) val {
	e := &evaluator{ctors: map[string]cg.CtorSpec{}, natSpec: cp.Nat, cdefs: map[string]cg.CIr{}, cblocks: map[string]cg.CodeBlock{}}
	for _, d := range cp.Datas {
		for _, c := range d.Ctors {
			e.ctors[c.Name] = c
		}
	}
	var main cg.CIr
	for _, def := range cp.Defs {
		e.cdefs[def.Name] = def.Body
		if def.Name == cp.Main {
			main = def.Body
		}
	}
	for _, blk := range cp.Blocks {
		e.cblocks[blk.Name] = blk
	}
	if main == nil {
		t.Fatalf("converted main %q not found", cp.Main)
	}
	return e.evalCIr(main, nil, nil)
}

func (e *evaluator) evalCIr(t cg.CIr, locals, recEnv []val) val {
	switch x := t.(type) {
	case cg.CVar:
		return locals[x.Idx]
	case cg.CEnv:
		return recEnv[x.Idx]
	case cg.CGlobal:
		return e.global(x.Name, e.globalCIr)
	case cg.CUnit:
		return vUnit{}
	case cg.MkClosure:
		env := make([]val, len(x.Env))
		for i, et := range x.Env {
			env[i] = e.evalCIr(et, locals, recEnv)
		}
		return cClos{code: x.Code, env: env}
	case cg.AppClosure:
		return e.apply(e.evalCIr(x.Clo, locals, recEnv), e.evalCIr(x.Arg, locals, recEnv))
	case cg.CLet:
		return e.evalCIr(x.Body, prependVal(e.evalCIr(x.Val, locals, recEnv), locals), recEnv)
	case cg.CField:
		return e.evalCIr(x.Scrut, locals, recEnv).(vCon).args[x.Index]
	case cg.CCase:
		s := e.evalCIr(x.Scrut, locals, recEnv).(vCon)
		for _, arm := range x.Arms {
			if arm.Tag == s.tag {
				return e.evalCIr(arm.Body, locals, recEnv)
			}
		}
		panic("evalCIr: unmatched tag")
	case cg.CBounce:
		return e.evalCIr(x.Call, locals, recEnv) // a bounce is semantically its deferred call
	default:
		panic(fmt.Sprintf("evalCIr: unsupported %T", t))
	}
}

func (e *evaluator) globalCIr(name string) val { return e.evalCIr(e.cdefs[name], nil, nil) }

// ---- shared global resolution / application ----

// global resolves nat/ctor primitives uniformly, falling back to a definition
// resolver (Ir or CIr) for user globals.
func (e *evaluator) global(name string, def func(string) val) val {
	if e.natSpec != nil {
		switch name {
		case e.natSpec.Zero:
			return vNat{0}
		case e.natSpec.Succ:
			return goClos(func(a val) val { return vNat{a.(vNat).n + 1} })
		case e.natSpec.ElimName:
			return e.natElim()
		}
	}
	if c, ok := e.ctors[name]; ok {
		return e.ctorClosure(c, nil)
	}
	return def(name)
}

func (e *evaluator) natElim() val {
	return goClos(func(_ val) val { // motive (erased)
		return goClos(func(c0 val) val {
			return goClos(func(c1 val) val {
				return goClos(func(x val) val {
					acc := c0
					for k := 0; k < x.(vNat).n; k++ {
						acc = e.apply(e.apply(c1, vNat{k}), acc)
					}
					return acc
				})
			})
		})
	})
}

func (e *evaluator) ctorClosure(c cg.CtorSpec, have []val) val {
	if len(have) == c.Arity {
		return vCon{tag: c.Tag, name: c.Name, args: have}
	}
	return goClos(func(a val) val {
		return e.ctorClosure(c, append(append([]val{}, have...), a))
	})
}

func (e *evaluator) apply(fn, arg val) val {
	switch f := fn.(type) {
	case vClos:
		return e.evalIr(f.body, prependVal(arg, f.env))
	case cClos:
		blk := e.cblocks[f.code]
		return e.evalCIr(blk.Body, []val{arg}, f.env)
	case goClosure:
		return f.f(arg)
	}
	panic(fmt.Sprintf("apply: not a function: %T", fn))
}

// ---- the conformance oracle ----

func showVal(v val) string {
	switch x := v.(type) {
	case vUnit:
		return "()"
	case vNat:
		return showNat(x.n)
	case vCon:
		s := x.name
		for _, a := range x.args {
			if _, isUnit := a.(vUnit); isUnit {
				continue
			}
			r := showVal(a)
			if hasSpace(r) {
				s += " (" + r + ")"
			} else {
				s += " " + r
			}
		}
		return s
	case vClos, cClos, goClosure:
		return "<function>"
	}
	return "?"
}

func showNat(n int) string {
	if n == 0 {
		return "zero"
	}
	return "succ (" + showNat(n-1) + ")"
}

func hasSpace(s string) bool {
	for i := 0; i < len(s); i++ {
		if s[i] == ' ' {
			return true
		}
	}
	return false
}
