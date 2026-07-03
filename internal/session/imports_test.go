package session

import (
	"strings"
	"testing"
)

const geo = `
data Nat : U is zero : Nat | succ : Nat -> Nat end
module Math.Geometry is
  three : Nat is succ (succ (succ zero)) end
end
`

func TestImportUnqualifies(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(geo); err != nil {
		t.Fatal(err)
	}
	src := "import Math.Geometry\nanswer : Nat is three end\n"
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("import did not resolve three -> Math.Geometry.three: %v", err)
	}
}

func TestAliasShortens(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(geo); err != nil {
		t.Fatal(err)
	}
	src := "alias Math.Geometry as G\nanswer : Nat is G.three end\n"
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("alias G did not resolve G.three: %v", err)
	}
}

func TestImportAmbiguityErrors(t *testing.T) {
	s := New()
	two := geo + `
module Other is
  three : Nat is zero end
end
`
	if _, err := s.LoadSource(two); err != nil {
		t.Fatal(err)
	}
	src := "import Math.Geometry\nimport Other\nanswer : Nat is three end\n"
	_, err := s.LoadSource(src)
	if err == nil {
		t.Fatal("want ambiguity error")
	}
	msg := err.Error()
	if !strings.Contains(msg, "Math.Geometry.three") || !strings.Contains(msg, "Other.three") {
		t.Errorf("ambiguity error should list both candidates, got: %v", msg)
	}
}

func TestImportUnknownModuleErrors(t *testing.T) {
	s := New()
	_, err := s.LoadSource("import No.Such.Module\n")
	if err == nil || !strings.Contains(err.Error(), "No.Such.Module") {
		t.Fatalf("want unknown-module error naming it, got: %v", err)
	}
}

// TestLoadSetWithImport is a smoke test proving that compset (LoadSet) and
// imports compose: file A defines a module M, file B has `import M` and uses
// an M name unqualified.
func TestLoadSetWithImport(t *testing.T) {
	fileA := `
data Nat : U is zero : Nat | succ : Nat -> Nat end
module M is
  one : Nat is succ zero end
end
`
	fileB := `
import M
two : Nat is succ one end
`
	s := New()
	err := LoadSet(s, []NamedSource{
		{Name: "a.rune", Src: fileA},
		{Name: "b.rune", Src: fileB},
	})
	if err != nil {
		t.Fatalf("LoadSet with import failed: %v", err)
	}
	if _, ok := s.Lookup("two"); !ok {
		t.Fatal("two was not loaded into the session")
	}
}
