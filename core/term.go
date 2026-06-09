// Package core holds Rune's locally-nameless core: the elaborated term language
// (Tm), the glued NbE value domain (Val, shape only in Phase 0), and structural
// Merkle hashing.
//
// Three representations, one rule (see CLAUDE.md): the surface is named, the core
// is locally nameless, and the pretty-printer turns core back into named surface
// for display. Bound variables are de Bruijn indices; references to top-level
// definitions are content hashes (Ref), never names.
//
// Phase 0 implements only the term shapes, name resolution's target, and hashing.
// Type checking, normalization, conversion, and type-directed elaboration are
// Phase 1 and MUST NOT appear here.
package core

// Tm is an elaborated core term. It is a sealed interface: the unexported marker
// method isTm makes the constructor set closed, so every consumer matches by type
// switch and the compiler flags an unhandled case. This is the one AST encoding
// used throughout Rune; do not mix in another.
type Tm interface {
	isTm()
}

// Var is a bound variable, a de Bruijn index. Index 0 is the nearest enclosing
// binder. Var never names a top-level definition; that is Ref.
type Var struct {
	Idx int
}

// Ref is a reference to a top-level definition, named by the content hash of that
// definition's elaborated core (a Pitts constant). References are by hash, never
// by name, so the core carries no nominal dependency.
type Ref struct {
	Hash Hash
}

// Univ is the single universe U. Phase 0 has one universe (type : type is the
// Phase 1+ stance until the Phase 6 hierarchy); there is no level field yet.
type Univ struct{}

// Pi is the dependent function type (x : Dom) -> Cod. Cod is a Scope: it binds one
// variable (the argument), reachable as Var{0} inside it.
type Pi struct {
	Dom Tm
	Cod Scope
}

// Lam is a lambda abstraction \x -> Body. Body is a Scope binding the parameter.
type Lam struct {
	Body Scope
}

// App is an application Fn Arg.
type App struct {
	Fn  Tm
	Arg Tm
}

// Let is let x = Val in Body, with an optional annotation Ty (nil when absent).
// Body is a Scope binding x. The bound value is in-band: it lives in the term and
// is part of the term's content hash (it is not a global definition, so it is not
// reached through store.Unfold).
type Let struct {
	Ty   Tm // optional; nil if the let had no annotation
	Val  Tm
	Body Scope
}

// Ann is a type annotation (Term : Ty). Phase 0 keeps annotations in the core so
// that the surface round-trips; Phase 1 elaboration will consume them.
type Ann struct {
	Term Tm
	Ty   Tm
}

// Scope is the body of a binder. Under locally-nameless representation a Scope is
// just a term with one additional bound variable in scope (reachable as Var{0}).
//
// Name is a pretty-printing hint ONLY. It is NOT part of a term's identity and is
// never hashed: alpha-equivalent terms are de Bruijn-equal and therefore hash
// equal. The pretty-printer may freshen Name to avoid capture.
type Scope struct {
	Name string
	Body Tm
}

func (Var) isTm()  {}
func (Ref) isTm()  {}
func (Univ) isTm() {}
func (Pi) isTm()   {}
func (Lam) isTm()  {}
func (App) isTm()  {}
func (Let) isTm()  {}
func (Ann) isTm()  {}
