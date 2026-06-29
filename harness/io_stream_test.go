package harness

import (
	"os/exec"
	"strings"
	"testing"
)

// runRune runs `rune run FILE NAME --target T` from the repo root and returns trimmed stdout.
func runRune(t *testing.T, file, name, target string) string {
	t.Helper()
	cmd := exec.Command("go", "run", "./cmd/rune", "run", file, name, "--target", target)
	cmd.Dir = ".."
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("rune run %s %s --target %s failed: %v\n%s", file, name, target, err, out)
	}
	return strings.TrimSpace(string(out))
}

func TestStreamSplitOn(t *testing.T) {
	for _, target := range []string{"js", "go"} {
		if got := runRune(t, "listings/ch548_string_split.rune", "partsLen", target); got != "3" {
			t.Errorf("[%s] partsLen = %q, want 3", target, got)
		}
		if got := runRune(t, "listings/ch548_string_split.rune", "firstLen", target); got != "3" {
			t.Errorf("[%s] firstLen = %q, want 3", target, got)
		}
	}
}
