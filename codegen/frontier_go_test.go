package codegen_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/session"
)

// emitAndRunGo loads the named listing from listings/, emits it through the Go
// backend with the given main, writes it to a temp file, and go-runs it,
// returning the trimmed stdout.
func emitAndRunGo(t *testing.T, listing, mainName string) string {
	t.Helper()
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go not in PATH")
	}
	root, err := findModuleRoot()
	if err != nil {
		t.Fatalf("cannot find module root: %v", err)
	}
	src, err := os.ReadFile(filepath.Join(root, "listings", listing))
	if err != nil {
		t.Fatalf("read listing %s: %v", listing, err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("load listing %s: %v", listing, err)
	}
	p, err := s.EmitProgram(mainName)
	if err != nil {
		t.Fatalf("emit program %s: %v", mainName, err)
	}
	out, err := codegen.Go{}.Emit(p)
	if err != nil {
		t.Fatalf("go emit: %v", err)
	}
	dir := t.TempDir()
	f := filepath.Join(dir, "main.go")
	if err := os.WriteFile(f, []byte(out), 0o644); err != nil {
		t.Fatal(err)
	}
	result, runErr := exec.Command("go", "run", f).CombinedOutput()
	if runErr != nil {
		t.Fatalf("go run failed: %v\n--- emitted ---\n%s\n--- output ---\n%s", runErr, out, result)
	}
	return strings.TrimSpace(string(result))
}

// findModuleRoot walks up from the test binary's working directory to find the
// go.mod root (the rune module root).
func findModuleRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return "", os.ErrNotExist
		}
		dir = parent
	}
}

// TestFrontierGoRunsBothSeeded checks that a do-block of two prints runs BOTH
// effects exactly once. Under seed 0 the order is fixed and reproducible (the
// conformance contract). Both "hello" and "world" must appear in stdout.
func TestFrontierGoRunsBothSeeded(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go not in PATH")
	}
	out := emitAndRunGo(t, "ch_frontier_two_prints.rune", "main")
	// Both effects must appear exactly once, regardless of order.
	if strings.Count(out, "hello") != 1 || strings.Count(out, "world") != 1 {
		t.Fatalf("frontier dropped or duplicated an effect: %q", out)
	}
}
