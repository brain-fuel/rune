// Package surface is Rune's named layer: the lexer, parser, named AST, the
// pretty-printer (core -> named surface), and name resolution (named surface ->
// locally-nameless core).
//
// Names live here and only here. Resolution is the only "elaboration" in Phase 0:
// it sends bound identifiers to de Bruijn indices and free identifiers to
// definition references (content hashes). There is no type elaboration in Phase 0.
package surface

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

// EUniv is the universe U.
type EUniv struct{}

// ELam is one parameter of a lambda: fn (Param : Dom) is Body end. The parser
// desugars a curried fn (x : A) (y : B) is e end into nested ELam. Dom is the binder's
// domain annotation; resolution scope-checks it and then discards it, since the
// Phase-0 core lambda is un-annotated (see GRAMMAR.md §6). Dom is never nil.
type ELam struct {
	Param string
	Dom   Exp
	Body  Exp
}

// EApp is application Fn Arg, left-associative.
type EApp struct {
	Fn  Exp
	Arg Exp
}

// EPi is a dependent function type (Param : Dom) -> Cod. A non-dependent arrow
// A -> B parses to EPi with Param "_".
type EPi struct {
	Param string
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
