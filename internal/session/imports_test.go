package session

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/surface"
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

const geoNat = `
data Nat : U is zero : Nat | succ : Nat -> Nat end
module M is
  three : Nat is succ (succ (succ zero)) end
end
`

// TestImportLambdaBinderNotRewritten verifies that a lambda parameter whose name
// matches an imported module member is NOT rewritten inside the lambda body.
// Regression for the binder-capture bug: MapExpNames previously rewrote every
// EVar occurrence of "three" to "M.three", even when "three" was the lambda's
// own bound parameter.
func TestImportLambdaBinderNotRewritten(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(geoNat); err != nil {
		t.Fatal(err)
	}
	// "three" inside fn body must refer to the lambda parameter, not M.three.
	const src = `
import M
id : Nat -> Nat is fn (three : Nat) is three end end
`
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load failed: %v", err)
	}
	// id zero must be zero (identity), not M.three (= succ (succ (succ zero))).
	e, err := s.ParseSrcExpr("id zero")
	if err != nil {
		t.Fatal(err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatal(err)
	}
	nf := s.NormalizeExpr(tm)
	got := surface.PrettyWith(nf, s.RefNames())
	if got != "zero" {
		t.Errorf("id zero = %q, want %q (import lambda binder capture?)", got, "zero")
	}
}

// TestImportCaseBinderNotRewritten verifies that clause binders in a case
// expression are not rewritten when an imported module member has the same name.
func TestImportCaseBinderNotRewritten(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(geoNat); err != nil {
		t.Fatal(err)
	}
	// The "three" binder in "| succ three -> three" must refer to the
	// predecessor, not to M.three.
	const src = `
import M
pred : Nat -> Nat is
  fn (n : Nat) is
    case n of
      | zero -> zero
      | succ three -> three
    end
  end
end
`
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load failed: %v", err)
	}
	// pred (succ zero) must be zero (predecessor), not M.three (= 3).
	e, err := s.ParseSrcExpr("pred (succ zero)")
	if err != nil {
		t.Fatal(err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatal(err)
	}
	nf := s.NormalizeExpr(tm)
	got := surface.PrettyWith(nf, s.RefNames())
	if got != "zero" {
		t.Errorf("pred (succ zero) = %q, want %q (import case binder capture?)", got, "zero")
	}
}

// TestModuleSelfImport checks that a module's own definitions can reference
// sibling names unqualified within the module body (the self-import mechanism).
// "lit : Flag is on end" inside "module M is ... end" should elaborate because
// `on` is resolved to `M.on` via the implicit self-import of M.
func TestModuleSelfImport(t *testing.T) {
	src := `module M is
data Flag : U is on : Flag | off : Flag end
lit : Flag is on end
end
`
	s := New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("module with self-referencing body should load: %v", err)
	}
	if _, ok := s.Lookup("M.lit"); !ok {
		t.Fatal("M.lit was not added to session")
	}
}

// TestModuleForeignQualified checks that a foreign inside a module block
// elaborates to a qualified name in the session.
func TestModuleForeignQualified(t *testing.T) {
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
module M is
foreign mystery : Nat end
end
`
	s := New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("module with foreign should load: %v", err)
	}
	if _, ok := s.Lookup("M.mystery"); !ok {
		t.Fatal("M.mystery was not added to session")
	}
}

// TestModuleDataForeignSelfImport checks that a module containing both a
// datatype and a foreign whose type references a sibling data type elaborates
// correctly (the self-import lets the foreign's type expression see the
// qualified data type without the user spelling out M.Flag).
func TestModuleDataForeignSelfImport(t *testing.T) {
	src := `module M is
data Flag : U is on : Flag | off : Flag end
foreign mystery : Flag end
end
`
	s := New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("module with data+foreign should load: %v", err)
	}
	if _, ok := s.Lookup("M.mystery"); !ok {
		t.Fatal("M.mystery was not added to session")
	}
}
