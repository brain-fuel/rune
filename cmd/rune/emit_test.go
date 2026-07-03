package main

import (
	"os"
	"path/filepath"
	"testing"
)

// TestParseEmitArgsSingleFile verifies the classic single-file form is unchanged.
func TestParseEmitArgsSingleFile(t *testing.T) {
	// Create a real temp file so pathExists returns true.
	dir := t.TempDir()
	f := filepath.Join(dir, "foo.rune")
	if err := os.WriteFile(f, []byte{}, 0o644); err != nil {
		t.Fatal(err)
	}

	files, main, _, err := parseEmitArgs([]string{f})
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

	files, main, _, err := parseEmitArgs([]string{f, "answer"})
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

	files, main, _, err := parseEmitArgs([]string{dir, "myMain"})
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

	files, main, _, err := parseEmitArgs([]string{f1, f2, "answer"})
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
