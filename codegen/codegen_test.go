package codegen_test

import (
	"os"
	"os/exec"
	"strings"
	"testing"

	"goforge.dev/rune/codegen"
	"goforge.dev/rune/internal/session"
)

// emitJS loads source and emits the program with the given main.
func emitJS(t *testing.T, src, main string) string {
	t.Helper()
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load: %v", err)
	}
	p, err := s.EmitProgram(main)
	if err != nil {
		t.Fatal(err)
	}
	out, err := codegen.Default().Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	return string(out)
}

// runNode executes emitted JS and returns its stdout, skipping when node is
// not installed.
func runNode(t *testing.T, js string) string {
	t.Helper()
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not in PATH")
	}
	f, err := os.CreateTemp(t.TempDir(), "*.js")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := f.WriteString(js); err != nil {
		t.Fatal(err)
	}
	f.Close()
	out, err := exec.Command("node", f.Name()).CombinedOutput()
	if err != nil {
		t.Fatalf("node: %v\n%s\n--- emitted ---\n%s", err, out, js)
	}
	return strings.TrimSpace(string(out))
}

const natSrc = `
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

func TestEmitAndRunNat(t *testing.T) {
	src := natSrc + `three : Nat is add (succ zero) (succ (succ zero)) end`
	got := runNode(t, emitJS(t, src, "three"))
	if got != "succ (succ (succ zero))" {
		t.Fatalf("got %q", got)
	}
}

func TestEmitListLength(t *testing.T) {
	src := natSrc + `
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
two : Nat is length Nat (cons Nat zero (cons Nat (succ zero) (nil Nat))) end
`
	got := runNode(t, emitJS(t, src, "two"))
	if got != "succ (succ zero)" {
		t.Fatalf("got %q", got)
	}
}

// Proofs, casts, and transports erase: the emitted program never mentions
// equality machinery and still computes its payload.
func TestErasureDropsProofs(t *testing.T) {
	src := natSrc + `
lemma : (n : Nat) -> Eq Nat n n is fn (n : Nat) is refl n end end
carried : Nat -> Nat is
  fn (n : Nat) is
    subst Nat n n (lemma n) (fn (z : Nat) is Nat end) (succ n)
  end
end
one : Nat is carried zero end
`
	js := emitJS(t, src, "one")
	if strings.Contains(js, "Eq") || strings.Contains(js, "refl") {
		t.Fatalf("equality machinery leaked into the shadow:\n%s", js)
	}
	got := runNode(t, js)
	if got != "succ zero" {
		t.Fatalf("got %q", got)
	}
}

// The 0-fragment receives units: erased binders cost a null at call sites,
// not an arity change — and the value still computes.
func TestErasedQuantityBecomesUnit(t *testing.T) {
	src := natSrc + `
constN : (0 A : U1) -> Nat -> Nat is
  fn (0 A : U1) (n : Nat) is succ n end
end
one : Nat is constN U zero end
`
	got := runNode(t, emitJS(t, src, "one"))
	if got != "succ zero" {
		t.Fatalf("got %q", got)
	}
}
