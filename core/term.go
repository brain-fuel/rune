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

// Univ is a universe U_Lvl in the predicative hierarchy (Phase 6):
// U_i : U_{i+1}, with cumulativity U_i <: U_j for i ≤ j supplied by Sub. The
// surface writes `U` for U_0 and `U1`, `U2`, … for higher levels. The
// type-in-type stance is gone.
type Univ struct {
	Lvl int
}

// Qty is a binder's usage annotation, drawn from the 0/1/ω semiring the
// quantity stratum supplies (Phase 5). The zero value is QMany (ω,
// unrestricted) so unannotated binders cost nothing. The 0-fragment is the
// erasure boundary codegen reads. Quantities are part of a Pi type's identity
// and are hashed.
type Qty byte

const (
	// QMany is ω: unrestricted use (the default).
	QMany Qty = iota
	// QZero marks an erased binder: usable in types, absent at runtime.
	QZero
	// QOne marks a linear binder: used exactly once.
	QOne
)

// Icit is the plicity of a binder or application: explicit (the default) or
// implicit (Phase 2; surface syntax {x : A}). Implicit arguments are inserted by
// elaboration, so the core is always fully applied and plicity is part of a
// term's identity (it is hashed).
type Icit byte

const (
	// Expl marks an explicit binder or argument.
	Expl Icit = iota
	// Impl marks an implicit binder or argument.
	Impl
)

// Prop is the universe of propositions (Phase 3, the observational equality
// stratum): proof-irrelevant, with Prop : U. Equality types live here.
type Prop struct{}

// Eq is the observational equality type former: Eq Ty L R, the proposition
// that L and R are equal elements of Ty. It computes on the structure of Ty
// (Eq over a function type unfolds to pointwise equality — funext is a
// REDUCTION, not an axiom). Lives in Prop.
type Eq struct {
	Ty Tm
	L  Tm
	R  Tm
}

// Refl is the reflexivity proof: Refl x : Eq A x x. Proofs are definitionally
// irrelevant; Refl's payload exists for type inference, not for computation.
type Refl struct {
	Tm Tm
}

// Cast transports a term along a type equality: Cast A B P X : B, given
// A B : U, P : Eq U A B, X : A. Cast computes on the structure of A and B and
// never inspects P (proof irrelevance).
type Cast struct {
	A Tm
	B Tm
	P Tm
	X Tm
}

// Subst is Leibniz transport along an equality (Phase 4's induction
// workhorse): Subst A X Y Prf P Px : P Y, given X Y : A, Prf : Eq A X Y,
// P : A -> s, Px : P X. It computes to Px when X ≡ Y (in particular at refl)
// and never inspects Prf.
type Subst struct {
	A   Tm
	X   Tm
	Y   Tm
	Prf Tm
	P   Tm
	Px  Tm
}

// Meta is an unsolved metavariable, identified within one elaboration run. It is
// ELABORATION-INTERNAL: a meta must never reach the store or the hash boundary —
// HashTerm panics on it, and the session asserts elaborated definitions are
// meta-free (zonked) before storing.
type Meta struct {
	ID int
}

// Pi is the dependent function type (x : Dom) -> Cod (explicit) or
// {x : Dom} -> Cod (implicit). Cod is a Scope: it binds one variable (the
// argument), reachable as Var{0} inside it.
type Pi struct {
	Icit Icit
	Qty  Qty
	Dom  Tm
	Cod  Scope
}

// Lam is a lambda abstraction \x -> Body. Body is a Scope binding the parameter.
type Lam struct {
	Icit Icit
	Qty  Qty
	Body Scope
}

// App is an application Fn Arg, at the given plicity.
type App struct {
	Fn   Tm
	Arg  Tm
	Icit Icit
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

func (Var) isTm()   {}
func (Meta) isTm()  {}
func (Prop) isTm()  {}
func (Eq) isTm()    {}
func (Refl) isTm()  {}
func (Cast) isTm()  {}
func (Subst) isTm() {}
func (Ref) isTm()   {}
func (Univ) isTm()  {}
func (Pi) isTm()    {}
func (Lam) isTm()   {}
func (App) isTm()   {}
func (Let) isTm()   {}
func (Ann) isTm()   {}
