package codegen

import "goforge.dev/rune/core"

// The erased intermediate representation: an untyped lambda calculus with
// globals, constructors, and eliminators. Types, proofs, and 0-quantity
// content erase to the unit token — positions are preserved (no arity
// surgery), so an erased argument is simply a unit at the call site and an
// erased binder receives one. THE SHADOW RULE (CLAUDE.md): this tree is
// throwaway codegen shadow; the core it came from is the immutable truth.

// Ir is an erased term. Sealed like core.Tm.
type Ir interface {
	isIr()
}

// IVar is a bound variable (de Bruijn index, as in core).
type IVar struct {
	Idx int
}

// IGlobal is a reference to a top-level definition, constructor, or
// eliminator, by the name the emitted program binds it to.
type IGlobal struct {
	Name string
}

// IUnit is the erased token: every type, proof, and 0-quantity value.
type IUnit struct{}

// ILam is a one-parameter function.
type ILam struct {
	Name string
	Body Ir
}

// IApp is application.
type IApp struct {
	Fn  Ir
	Arg Ir
}

// ILet is a local binding.
type ILet struct {
	Name string
	Val  Ir
	Body Ir
}

func (IVar) isIr()    {}
func (IGlobal) isIr() {}
func (IUnit) isIr()   {}
func (ILam) isIr()    {}
func (IApp) isIr()    {}
func (ILet) isIr()    {}

// CtorSpec is one constructor of an emitted datatype: its bound name, its
// 0-based tag, and the total number of curried parameters it takes (datatype
// parameters + its own arguments; parameters arrive erased).
type CtorSpec struct {
	Name  string
	Tag   int
	Arity int
}

// DataSpec is one datatype: its eliminator's bound name, the number of
// datatype parameters, and its constructors in declaration order. Rec mirrors
// core.CtorSig.Rec for building recursive calls in the eliminator.
type DataSpec struct {
	ElimName  string
	NumParams int
	Ctors     []CtorSpec
	Rec       [][]bool
}

// DefSpec is one emitted definition.
type DefSpec struct {
	Name string
	Body Ir
}

// Program is a whole erased program in definition order (acyclic, so the
// order is also the emission order).
type Program struct {
	Datas []DataSpec
	Defs  []DefSpec
	// Main, when non-empty, names the definition whose value the emitted
	// program prints on run.
	Main string
}

// Erase lowers elaborated, meta-free core to the erased IR. names maps a
// definition hash to its emitted global name; typeRefs holds the hashes that
// denote types at runtime (datatype formers), which erase to the unit token.
func Erase(t core.Tm, names map[core.Hash]string, typeRefs map[core.Hash]bool) Ir {
	switch tm := t.(type) {
	case core.Var:
		return IVar{Idx: tm.Idx}
	case core.Ref:
		if typeRefs[tm.Hash] {
			return IUnit{}
		}
		n, ok := names[tm.Hash]
		if !ok {
			n = "$" + tm.Hash.Short()
		}
		return IGlobal{Name: n}
	case core.Univ, core.Prop, core.Pi, core.Eq, core.Refl:
		// Types and proofs are build-time discipline; the shadow keeps a unit.
		return IUnit{}
	case core.Lam:
		return ILam{Name: tm.Body.Name, Body: Erase(tm.Body.Body, names, typeRefs)}
	case core.App:
		fn := Erase(tm.Fn, names, typeRefs)
		if _, isUnit := fn.(IUnit); isUnit {
			// A unit head is an erased type former (List, Quot, …): the whole
			// application denotes a type and erases with it.
			return IUnit{}
		}
		return IApp{Fn: fn, Arg: Erase(tm.Arg, names, typeRefs)}
	case core.Let:
		return ILet{Name: tm.Body.Name, Val: Erase(tm.Val, names, typeRefs),
			Body: Erase(tm.Body.Body, names, typeRefs)}
	case core.Ann:
		return Erase(tm.Term, names, typeRefs)
	case core.Cast:
		// cast computes on types, which are gone: the shadow is the subject.
		return Erase(tm.X, names, typeRefs)
	case core.Subst:
		// Transport is the identity on its computational payload.
		return Erase(tm.Px, names, typeRefs)
	default:
		panic("codegen.Erase: unexpected core term (metavariable or unknown constructor)")
	}
}
