package elaborate_test

import (
	"strings"
	"testing"

	"goforge.dev/rune/internal/session"
)

const natPrelude = `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
+ : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    case m of
    | zero -> n
    | succ k with ih -> succ ih
    end
  end
end
`

// TestCaseElaborates pins rung 4 end to end: case desugars to the eliminator,
// computes (2+3=5 by refl), and supports the dependent motive (induction with
// the scrutinee generalized in the expected type).
func TestCaseElaborates(t *testing.T) {
	src := natPrelude + `
check : 2 + 3 = 5 is refl 5 end
addZeroRight : (n : Nat) -> n + 0 = n is
  fn (n : Nat) is
    case n of
    | zero -> refl 0
    | succ k with ih ->
        subst Nat (k + 0) k ih (fn (z : Nat) is succ (k + 0) = succ z end) (refl (succ (k + 0)))
    end
  end
end`
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load: %v", err)
	}
}

// TestCaseErrors pins the §5.6 well-formedness errors.
func TestCaseErrors(t *testing.T) {
	cases := []struct{ body, want string }{
		{"case m of | zero -> zero end", "missing clause for constructor succ"},
		{"case m of | zero -> zero | succ k -> k | zero -> zero end", "duplicate clause"},
		{"case m of | zero -> zero | succ k j -> k end", "takes 1 argument(s), the clause binds 2"},
		{"case m of | zero with h -> zero | succ k -> k end", "no recursive argument"},
		{"case m of | foo -> zero | succ k -> k end", "not a constructor of Nat"},
		{"case U of | zero -> zero | succ k -> k end", "not a datatype"},
	}
	for _, c := range cases {
		src := "data Nat : U is zero : Nat | succ : Nat -> Nat end\n" +
			"bad : Nat -> Nat is fn (m : Nat) is " + c.body + " end end"
		s := session.New()
		_, err := s.LoadSource(src)
		if err == nil || !strings.Contains(err.Error(), c.want) {
			t.Errorf("%s: want error containing %q, got %v", c.body, c.want, err)
		}
	}
}
