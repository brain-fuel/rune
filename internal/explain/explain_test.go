package explain

import (
	"testing"

	"goforge.dev/rune/v3/internal/session"
)

// doubleSrc is a prelude-free replica of examples/double.rune: the spec's
// normative four-line target output is asserted against it exactly.
const doubleSrc = `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
foreign Float : U end
foreign fromNat : Nat -> Float end
foreign fmul : Float -> Float -> Float end
foreign getFloat : IO Float end
foreign printFloat : Float -> IO Float end
main : IO Float is
  bindIO Float Float getFloat
    (fn (x : Float) is printFloat (fmul x (fromNat 2)) end)
end
`

func load(t *testing.T, src string) *session.Session {
	t.Helper()
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("LoadSource: %v", err)
	}
	return s
}

func explainText(t *testing.T, s *session.Session, name string, opts Options) string {
	t.Helper()
	root, err := Explain(s, name, opts)
	if err != nil {
		t.Fatalf("Explain(%s): %v", name, err)
	}
	return RenderText(root)
}

// TestExplainDoubleDepth0 is acceptance item 1: the spec's four-line English
// view of the double demo, exactly.
func TestExplainDoubleDepth0(t *testing.T) {
	s := load(t, doubleSrc)
	want := "[Entrypoint: main]\n" +
		"[Get Float `x` from Command Line]\n" +
		"[Apply Function (fmul x (fromNat 2))]\n" +
		"[Print Result to Command Line]\n"
	if got := explainText(t, s, "main", Options{}); got != want {
		t.Errorf("double depth 0:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainCaseAndPure: case clauses render as When steps with indented
// kids; pureIO renders Give Back; an atomic prim argument stays inline.
func TestExplainCaseAndPure(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Option : U -> U is none : (A : U) -> Option A | some : (A : U) -> A -> Option A end
foreign Float : U end
foreign parseFloat : Nat -> Option Float end
foreign printNat : Nat -> IO Nat end
probe : IO Nat is
  case parseFloat 302 of
  | none -> printNat 999
  | some x -> pureIO Nat zero
  end
end
`
	s := load(t, src)
	want := "[Entrypoint: probe]\n" +
		"[When (parseFloat 302) is (none):]\n" +
		"  [Print (999) to Command Line]\n" +
		"[When (parseFloat 302) is (some x):]\n" +
		"  [Give Back (zero)]\n"
	if got := explainText(t, s, "probe", Options{}); got != want {
		t.Errorf("case/pure:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainSeq: a seq binding names its value step and continues as
// siblings (the Then-list).
func TestExplainSeq(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
foreign printNat : Nat -> IO Nat end
m : IO Nat is
  seq
    let r = printNat 1;
    printNat r
  end
end
`
	s := load(t, src)
	want := "[Entrypoint: m]\n" +
		"[Print (1) to Command Line as `r`]\n" +
		"[Print (r) to Command Line]\n"
	if got := explainText(t, s, "m", Options{}); got != want {
		t.Errorf("seq:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainLambdaFallback: fn renders Given with kids; a bound variable
// leaf renders through the atom fallback.
func TestExplainLambdaFallback(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
idn : Nat -> Nat is fn (n : Nat) is n end end
`
	s := load(t, src)
	want := "[Entrypoint: idn]\n" +
		"[Given `n`:]\n" +
		"  [Result: (n)]\n"
	if got := explainText(t, s, "idn", Options{}); got != want {
		t.Errorf("lambda/fallback:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainExp: the REPL $N path renders a bare expression; a data
// constructor call is typed via the elaborator.
func TestExplainExp(t *testing.T) {
	s := load(t, "data Nat : U is zero : Nat | succ : Nat -> Nat end\n")
	e, err := s.ParseSrcExpr("succ zero")
	if err != nil {
		t.Fatalf("ParseSrcExpr: %v", err)
	}
	root, err := ExplainExp(s, e, Options{})
	if err != nil {
		t.Fatalf("ExplainExp: %v", err)
	}
	want := "[Expression]\n" +
		"[Apply `succ` to (zero) (takes Nat, gives Nat)]\n"
	if got := RenderText(root); got != want {
		t.Errorf("expression:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestExplainErrors: unknown names and bodiless targets report cleanly.
func TestExplainErrors(t *testing.T) {
	s := load(t, "foreign F : U end\n")
	if _, err := Explain(s, "nosuch", Options{}); err == nil {
		t.Error("Explain(nosuch): want error")
	}
	if _, err := Explain(s, "F", Options{}); err == nil {
		t.Error("Explain(F): want error (foreign axiom has no surface body)")
	}
}
