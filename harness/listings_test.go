package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/codegen"
	"goforge.dev/rune/internal/session"
	"goforge.dev/rune/surface"
)

// The v1.0.0 freeze criterion (ref_docs/rune-v1-design.md): every listing in
// the book ELABORATES, CHECKS, and RUNS against this core. listings/ holds the
// book's code; this file is the gate that enforces the criterion. A listing
// that stops loading, a marked expression that stops normalizing to its
// expected form, or an emitted chapter that stops running is a v1 regression.

// loadListing reads and loads one listing file into a fresh session.
func loadListing(t *testing.T, name string) *session.Session {
	t.Helper()
	src, err := os.ReadFile(filepath.Join("..", "listings", name))
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("%s does not check: %v", name, err)
	}
	return s
}

// normalizesTo asserts that expr, elaborated against the session, normalizes
// to want.
func normalizesTo(t *testing.T, s *session.Session, expr, want string) {
	t.Helper()
	e, err := surface.ParseExpr(expr)
	if err != nil {
		t.Fatal(err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatalf("%s does not elaborate: %v", expr, err)
	}
	got := surface.PrettyWith(s.NormalizeExpr(tm), s.RefNames())
	if got != want {
		t.Fatalf("%s normalized to %q, want %q", expr, got, want)
	}
}

func TestListingsElaborateAndCheck(t *testing.T) {
	entries, err := os.ReadDir(filepath.Join("..", "listings"))
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) < 5 {
		t.Fatalf("expected the book's chapters in listings/, found %d files", len(entries))
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".rune") {
			continue
		}
		t.Run(e.Name(), func(t *testing.T) { loadListing(t, e.Name()) })
	}
}

func TestListingsRun(t *testing.T) {
	t.Run("ch01", func(t *testing.T) {
		s := loadListing(t, "ch01_functions.rune")
		// idU maps small types; applied to nothing it δβ-normalizes to the
		// identity lambda (U itself is not a member of U).
		normalizesTo(t, s, `idU`, "fn (x : U) is x end")
	})
	t.Run("ch02", func(t *testing.T) {
		s := loadListing(t, "ch02_implicits.rune")
		normalizesTo(t, s, `id U`, "U")
	})
	t.Run("ch03", func(t *testing.T) {
		s := loadListing(t, "ch03_equality.rune")
		// cast between convertible endpoints reduces away under the binder.
		normalizesTo(t, s, `transport U U (refl U)`, "fn (x : U) is x end")
	})
	t.Run("ch04", func(t *testing.T) {
		s := loadListing(t, "ch04_data.rune")
		normalizesTo(t, s, `four`, "succ (succ (succ (succ zero)))")
		normalizesTo(t, s, `not (not true)`, "true")
		normalizesTo(t, s, `length Nat (cons Nat zero (nil Nat))`, "succ zero")
		// The induction proof COMPUTES at canonical numerals.
		normalizesTo(t, s, `addZeroRight (succ zero)`, "refl (succ zero)")
	})
	t.Run("ch05", func(t *testing.T) {
		s := loadListing(t, "ch05_quantities.rune")
		normalizesTo(t, s, `eid U`, "U")
	})
}

// TestListingsEmitAndExecute: the data chapter survives erasure and runs on
// the JS backend (the "runs" of the freeze criterion, in the deployed sense).
func TestListingsEmitAndExecute(t *testing.T) {
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not in PATH")
	}
	s := loadListing(t, "ch04_data.rune")
	p, err := s.EmitProgram("four")
	if err != nil {
		t.Fatal(err)
	}
	out, err := codegen.Default().Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	f, err := os.CreateTemp(t.TempDir(), "*.js")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := f.WriteString(string(out)); err != nil {
		t.Fatal(err)
	}
	f.Close()
	got, err := exec.Command("node", f.Name()).CombinedOutput()
	if err != nil {
		t.Fatalf("node: %v\n%s", err, got)
	}
	if strings.TrimSpace(string(got)) != "succ (succ (succ (succ zero)))" {
		t.Fatalf("emitted chapter printed %q", got)
	}
}
