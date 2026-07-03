package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/internal/session"
)

// TestParseEmitArgsSingleFile verifies the classic single-file form is unchanged.
func TestParseEmitArgsSingleFile(t *testing.T) {
	// Create a real temp file so pathExists returns true.
	dir := t.TempDir()
	f := filepath.Join(dir, "foo.rune")
	if err := os.WriteFile(f, []byte{}, 0o644); err != nil {
		t.Fatal(err)
	}

	files, main, _, _, err := parseEmitArgs([]string{f})
	if err != nil {
		t.Fatal(err)
	}
	if len(files) != 1 || files[0] != f {
		t.Errorf("single file: want [%s], got %v", f, files)
	}
	if main != "" {
		t.Errorf("single file: want empty main, got %q", main)
	}
}

// TestParseEmitArgsFileAndName verifies <file> <name> form: name is extracted
// when the first positional exists and the last does not.
func TestParseEmitArgsFileAndName(t *testing.T) {
	dir := t.TempDir()
	f := filepath.Join(dir, "foo.rune")
	if err := os.WriteFile(f, []byte{}, 0o644); err != nil {
		t.Fatal(err)
	}

	files, main, _, _, err := parseEmitArgs([]string{f, "answer"})
	if err != nil {
		t.Fatal(err)
	}
	if len(files) != 1 || files[0] != f {
		t.Errorf("file+name: want files=[%s], got %v", f, files)
	}
	if main != "answer" {
		t.Errorf("file+name: want main=%q, got %q", "answer", main)
	}
}

// TestParseEmitArgsDirExpansion verifies a directory expands to its *.rune files
// sorted by name (non-recursive), and that the last positional is treated as the
// main name when it does not exist as a file/dir.
func TestParseEmitArgsDirExpansion(t *testing.T) {
	dir := t.TempDir()
	// Create two .rune files and one non-.rune file.
	for _, name := range []string{"b.rune", "a.rune", "c.txt"} {
		if err := os.WriteFile(filepath.Join(dir, name), []byte{}, 0o644); err != nil {
			t.Fatal(err)
		}
	}

	files, main, _, _, err := parseEmitArgs([]string{dir, "myMain"})
	if err != nil {
		t.Fatal(err)
	}
	// .rune files must be sorted by name; c.txt must be excluded.
	want := []string{filepath.Join(dir, "a.rune"), filepath.Join(dir, "b.rune")}
	if len(files) != len(want) {
		t.Fatalf("dir expansion: want %v, got %v", want, files)
	}
	for i, w := range want {
		if files[i] != w {
			t.Errorf("dir expansion [%d]: want %s, got %s", i, w, files[i])
		}
	}
	if main != "myMain" {
		t.Errorf("dir expansion: want main=%q, got %q", "myMain", main)
	}
}

// TestParseEmitArgsMultiFile verifies multiple explicit file paths are accepted.
func TestParseEmitArgsMultiFile(t *testing.T) {
	dir := t.TempDir()
	f1 := filepath.Join(dir, "nat.rune")
	f2 := filepath.Join(dir, "main.rune")
	for _, f := range []string{f1, f2} {
		if err := os.WriteFile(f, []byte{}, 0o644); err != nil {
			t.Fatal(err)
		}
	}

	files, main, _, _, err := parseEmitArgs([]string{f1, f2, "answer"})
	if err != nil {
		t.Fatal(err)
	}
	if len(files) != 2 {
		t.Fatalf("multi-file: want 2 files, got %v", files)
	}
	if files[0] != f1 || files[1] != f2 {
		t.Errorf("multi-file: wrong files %v", files)
	}
	if main != "answer" {
		t.Errorf("multi-file: want main=%q, got %q", "answer", main)
	}
}

// TestNoPreludeFlag verifies that --no-prelude is parsed and returned correctly.
func TestNoPreludeFlag(t *testing.T) {
	dir := t.TempDir()
	f := filepath.Join(dir, "foo.rune")
	if err := os.WriteFile(f, []byte{}, 0o644); err != nil {
		t.Fatal(err)
	}

	_, _, _, noPrelude, err := parseEmitArgs([]string{f, "--no-prelude"})
	if err != nil {
		t.Fatal(err)
	}
	if !noPrelude {
		t.Error("--no-prelude flag not returned")
	}
}

// TestSourcesHaveBuiltinNat verifies auto-disable detection.
func TestSourcesHaveBuiltinNat(t *testing.T) {
	with := []session.NamedSource{
		{Name: "nat.rune", Src: "data Nat : U is zero : Nat | succ : Nat -> Nat end\nbuiltin nat Nat zero succ\n"},
	}
	without := []session.NamedSource{
		{Name: "demo.rune", Src: "import Std.Float\nmain : IO Float is getFloat end\n"},
	}
	if !sourcesHaveBuiltinNat(with) {
		t.Error("expected builtin nat detected in 'with' set")
	}
	if sourcesHaveBuiltinNat(without) {
		t.Error("false positive: 'without' set should not trigger auto-disable")
	}
}

// TestPreludeAutoDisable verifies that a source declaring `builtin nat` loads
// correctly through the CLI path (programForSet with withPrelude=false).
func TestPreludeAutoDisable(t *testing.T) {
	// ch211 declares its own Nat + builtin nat and echoes a number.
	// It must load and elaborate when prelude is NOT loaded alongside it.
	src := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
foreign printNat : Nat -> IO Nat end
foreign getNat : IO Nat end
main : IO Nat is bindIO Nat Nat getNat (fn (n : Nat) is printNat n end) end
`
	sources := []session.NamedSource{{Name: "test.rune", Src: src}}
	if sourcesHaveBuiltinNat(sources) {
		// Auto-disable: do not load prelude.
		_, err := programForSet(sources, "main", false)
		if err != nil {
			t.Fatalf("auto-disabled path: %v", err)
		}
	} else {
		t.Fatal("test setup error: expected builtin nat in source")
	}
}

// TestSourcesHaveBuiltinNatSubstring guards against false positives on
// comments or identifiers that contain "builtin nat" as a substring.
func TestSourcesHaveBuiltinNatSubstring(t *testing.T) {
	// A comment containing the text should still trigger (conservative).
	src := []session.NamedSource{{Name: "c.rune", Src: "-- builtin nat defined below\n"}}
	if !sourcesHaveBuiltinNat(src) {
		t.Error("conservative: comment containing 'builtin nat' should trigger")
	}
	// A source without the token sequence at all should not trigger.
	clean := []session.NamedSource{{Name: "c.rune", Src: "data Bool : U is true : Bool | false : Bool end\n"}}
	if sourcesHaveBuiltinNat(clean) {
		t.Error("false positive on clean source")
	}
	_ = strings.Contains // keep import
}
