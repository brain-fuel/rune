// Package surface is Rune's named layer: the lexer, parser, named AST, the
// pretty-printer (core -> named surface), and name resolution (named surface ->
// locally-nameless core).
//
// Names live here and only here. Resolution is the only "elaboration" in Phase 0:
// it sends bound identifiers to de Bruijn indices and free identifiers to
// definition references (content hashes). There is no type elaboration in Phase 0.
package surface

import "goforge.dev/rune/core"

// Exp is a named surface expression. Like core.Tm it is a sealed interface matched
// by type switch; the marker keeps the constructor set closed.
type Exp interface {
	isExp()
}

// EVar is an identifier occurrence: either a bound variable or a free reference to a
// top-level definition. Resolution decides which.
type EVar struct {
	Name string
}

// EUniv is a universe: `U` is U_0, `U1`…`U9` the higher levels (Phase 6).
type EUniv struct {
	Lvl int
}

// EHole is `_`: a hole for elaboration to solve with a fresh metavariable
// (Phase 2). Holes never survive elaboration; the untyped resolver rejects them.
type EHole struct{}

// EProp is the universe of propositions, Prop (Phase 3).
type EProp struct{}

// EEq is the equality former head, `Eq`. It is applied like a function and must
// be saturated with three arguments: Eq T l r.
type EEq struct{}

// ERefl is the reflexivity proof head, `refl`. Used bare (in checking position)
// or applied to one argument.
type ERefl struct{}

// ECast is the cast head, `cast`, saturated with four arguments: cast A B p x.
type ECast struct{}

// ESubst is the transport head, `subst`, saturated with six arguments:
// subst A x y p P px — from p : Eq A x y and px : P x, conclude P y.
type ESubst struct{}

// ELam is one parameter of a lambda: fn (Param : Dom) is Body end. The parser
// desugars a curried fn (x : A) (y : B) is e end into nested ELam. Dom is the binder's
// domain annotation; resolution scope-checks it and then discards it, since the
// Phase-0 core lambda is un-annotated (see GRAMMAR.md §6). Dom is never nil.
type ELam struct {
	Param string
	Icit  core.Icit // Impl when written fn {x : A}
	Qty   core.Qty  // 0 or 1 when annotated (fn (0 x : A) …); ω otherwise
	Dom   Exp
	Body  Exp
}

// EApp is application Fn Arg, left-associative.
type EApp struct {
	Fn   Exp
	Arg  Exp
	Icit core.Icit // Impl when written f {e}
}

// EPi is a dependent function type (Param : Dom) -> Cod. A non-dependent arrow
// A -> B parses to EPi with Param "_".
type EPi struct {
	Param string
	Icit  core.Icit // Impl when written {x : A} -> B
	Qty   core.Qty  // 0 or 1 when annotated ((0 x : A) -> B); ω otherwise
	Dom   Exp
	Cod   Exp
}

// ELet is let Name (: Ty)? = Val in Body. Ty is nil when the annotation is absent.
type ELet struct {
	Name string
	Ty   Exp
	Val  Exp
	Body Exp
}

// EAnn is a type annotation (Term : Ty).
type EAnn struct {
	Term Exp
	Ty   Exp
}

func (EVar) isExp()  {}
func (EHole) isExp() {}
func (EProp) isExp() {}
func (EEq) isExp()   {}
func (ERefl) isExp() {}
func (ECast) isExp() {}
func (EUniv) isExp() {}
func (ELam) isExp()  {}
func (EApp) isExp()  {}
func (EPi) isExp()   {}
func (ELet) isExp()  {}
func (EAnn) isExp()  {}

// Def is one top-level definition: Name (: Ty)? = Body. The file is a flat list of
// these (no modules in Phase 0). Ty is nil when omitted.
type Def struct {
	Name string
	Ty   Exp
	Body Exp
}

// Ctor is one constructor of a datatype declaration: Name : Ty.
type Ctor struct {
	Name string
	Ty   Exp
}

// DataDef is a datatype declaration (Phase 4):
//
//	data Name : Ty is C1 : T1 | C2 : T2 | … end
type DataDef struct {
	Name  string
	Ty    Exp
	Ctors []Ctor
}

// BuiltinNat is a builtin-binding declaration (ergonomics rung 2):
//
//	builtin nat Nat zero succ
//
// It registers which data type numeric literals mean: from its position to the
// end of the file, a numeral n parses as the n-fold application of Succ to
// Zero. The declaration is surface state only — it desugars every literal at
// parse time and leaves no trace of its own in the core or the store.
type BuiltinNat struct {
	TyName string
	Zero   string
	Succ   string
}

// Item is one top-level program item: a Def, a DataDef, or a BuiltinNat.
type Item interface {
	isItem()
}

func (Def) isItem()        {}
func (DataDef) isItem()    {}
func (BuiltinNat) isItem() {}

func (ESubst) isExp() {}
