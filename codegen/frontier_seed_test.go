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

// emitAndRunGoSeed is like emitAndRunGo but injects WAVELET_SEED=seed into the
// child process environment so the frontier scheduler uses a specific seed.
func emitAndRunGoSeed(t *testing.T, listing, mainName, seed string) string {
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
	cmd := exec.Command("go", "run", f)
	cmd.Env = append(os.Environ(), "WAVELET_SEED="+seed)
	result, runErr := cmd.CombinedOutput()
	if runErr != nil {
		t.Fatalf("go run failed (seed=%s): %v\n--- emitted ---\n%s\n--- output ---\n%s", seed, runErr, out, result)
	}
	return strings.TrimSpace(string(result))
}

// TestFrontierSeedReorders asserts that SOME pair of seeds produces a different
// interleaving order for the two independent prints in ch_frontier_two_prints.rune.
// If no seed reorders, the LCG / WAVELET_SEED plumbing is broken.
func TestFrontierSeedReorders(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go not in PATH")
	}
	a := emitAndRunGoSeed(t, "ch_frontier_two_prints.rune", "main", "1")
	b := emitAndRunGoSeed(t, "ch_frontier_two_prints.rune", "main", "4")
	if a == b {
		// Try a small sweep before declaring no reordering.
		found := false
		for _, s := range []string{"2", "3", "5", "6", "7"} {
			if emitAndRunGoSeed(t, "ch_frontier_two_prints.rune", "main", s) != a {
				found = true
				break
			}
		}
		if !found {
			t.Fatalf("no seed reordered the two prints; scheduler is not interleaving")
		}
	}
}
