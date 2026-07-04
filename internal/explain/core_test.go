package explain

import "testing"

// TestExplainCoreDouble: the core walk of the double demo flattens the
// bindIO chain but shows raw core (no English templates), with the bound
// value's type after the binder.
func TestExplainCoreDouble(t *testing.T) {
	s := load(t, doubleSrc)
	want := "[Entrypoint: main]\n" +
		"[Do (getFloat) as `x` : Float]\n" +
		"[Do (printFloat (fmul x (fromNat 2)))]\n"
	if got := explainText(t, s, "main", Options{Core: true}); got != want {
		t.Errorf("core double:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainCoreImplicits: implicit binders are visible in the core walk.
func TestExplainCoreImplicits(t *testing.T) {
	src := "idi : {A : U} -> A -> A is fn {A : U} (x : A) is x end end\n"
	s := load(t, src)
	want := "[Entrypoint: idi]\n" +
		"[Given implicit `A`:]\n" +
		"  [Given `x`:]\n" +
		"    [Compute (x)]\n"
	if got := explainText(t, s, "idi", Options{Core: true}); got != want {
		t.Errorf("core implicits:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainExpCore: the $N path elaborates the expression and walks its
// core term.
func TestExplainExpCore(t *testing.T) {
	s := load(t, "data Nat : U is zero : Nat | succ : Nat -> Nat end\n")
	e, err := s.ParseSrcExpr("succ zero")
	if err != nil {
		t.Fatalf("ParseSrcExpr: %v", err)
	}
	root, err := ExplainExp(s, e, Options{Core: true})
	if err != nil {
		t.Fatalf("ExplainExp: %v", err)
	}
	want := "[Expression]\n" +
		"[Compute (succ zero)]\n"
	if got := RenderText(root); got != want {
		t.Errorf("core expression:\ngot:\n%swant:\n%s", got, want)
	}
}
