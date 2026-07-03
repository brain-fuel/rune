package session

import (
	"strings"
	"testing"
)

// modSrc defines a module Geom with a constant pi.
const modSrc = `
module Geom is
  pi : (A : U) -> A -> A is fn (A : U) (x : A) is x end end
end
`

// importSrc imports Geom and uses Geom.pi — so it depends on modSrc.
const importSrc = `
import Geom

usePi : (A : U) -> A -> A is Geom.pi end
`

// aliasSrc aliases Geom as G and uses G.pi — so it depends on modSrc.
const aliasSrc = `
alias Geom as G

usePiAlias : (A : U) -> A -> A is G.pi end
`

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

// TestLoadSetImportEdge verifies that a file with `import M` is topo-sorted
// AFTER the file that defines M.name, even when the importing file is given
// FIRST in the input slice (the unlucky order directory expansion can produce
// when filenames sort before the module file alphabetically).
func TestLoadSetImportEdge(t *testing.T) {
	s := New()
	err := LoadSet(s, []NamedSource{
		{Name: "a_use.rune", Src: importSrc}, // comes first — wrong naive order
		{Name: "m_mod.rune", Src: modSrc},
	})
	if err != nil {
		t.Fatalf("LoadSet with import edge: %v", err)
	}
	if _, ok := s.Lookup("usePi"); !ok {
		t.Fatal("usePi was not loaded into the session")
	}
}

// TestLoadSetAliasEdge verifies the same cross-file ordering for `alias M as G`.
func TestLoadSetAliasEdge(t *testing.T) {
	s := New()
	err := LoadSet(s, []NamedSource{
		{Name: "a_use.rune", Src: aliasSrc}, // comes first — wrong naive order
		{Name: "m_mod.rune", Src: modSrc},
	})
	if err != nil {
		t.Fatalf("LoadSet with alias edge: %v", err)
	}
	if _, ok := s.Lookup("usePiAlias"); !ok {
		t.Fatal("usePiAlias was not loaded into the session")
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
