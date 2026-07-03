package session

import (
	"strings"
	"testing"
)

// natFileSrc defines the Nat datatype. No external references.
const natFileSrc = `
data Nat : U is zero : Nat | succ : Nat -> Nat end
`

// answerFileSrc defines a value that uses Nat, succ, and zero.
// It references names from natFileSrc, so it depends on natFileSrc.
const answerFileSrc = `
answer : Nat is succ zero end
`

// TestLoadSetTopoSorts feeds two files in the WRONG order (answer before nat),
// which would fail with sequential LoadSource because Nat/succ/zero are not yet
// defined when answer is processed. LoadSet must topo-sort them and succeed.
func TestLoadSetTopoSorts(t *testing.T) {
	s := New()
	err := LoadSet(s, []NamedSource{
		{Name: "main.rune", Src: answerFileSrc},
		{Name: "nat.rune", Src: natFileSrc},
	})
	if err != nil {
		t.Fatalf("LoadSet: %v", err)
	}
	if _, ok := s.Lookup("answer"); !ok {
		t.Fatal("answer was not loaded into the session")
	}
}

// TestLoadSetCycleError verifies that a mutual dependency between two files
// is reported as a cycle error naming both files.
func TestLoadSetCycleError(t *testing.T) {
	// a.rune: defines x, references Nat (from b) and y (from b)
	a := `x : Nat is y end`
	// b.rune: defines Nat/zero/succ/y, references x (from a)
	b := `data Nat : U is zero : Nat | succ : Nat -> Nat end
y : Nat is x end`
	s := New()
	err := LoadSet(s, []NamedSource{{Name: "a.rune", Src: a}, {Name: "b.rune", Src: b}})
	if err == nil {
		t.Fatal("want cycle error, got nil")
	}
	if !strings.Contains(err.Error(), "a.rune") || !strings.Contains(err.Error(), "b.rune") {
		t.Errorf("cycle error should name both files, got: %v", err)
	}
}
