package harness

import (
	"os/exec"
	"strings"
	"testing"
)

// runBibleOp runs `rune run FILE NAME --target T` from the repo root, returns trimmed stdout.
func runBibleOp(t *testing.T, file, name, target string) string {
	t.Helper()
	cmd := exec.Command("go", "run", "./cmd/rune", "run", file, name, "--target", target)
	cmd.Dir = ".."
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("rune run %s %s --target %s failed: %v\n%s", file, name, target, err, out)
	}
	return strings.TrimSpace(string(out))
}

func TestBibleJsonStrField(t *testing.T) {
	for _, tg := range []string{"js", "go"} {
		if got := runBibleOp(t, "listings/ch551_json_field.rune", "strongLen", tg); got != "5" {
			t.Errorf("[%s] strongLen = %q, want 5", tg, got)
		}
		if got := runBibleOp(t, "listings/ch551_json_field.rune", "missingLen", tg); got != "0" {
			t.Errorf("[%s] missingLen = %q, want 0", tg, got)
		}
	}
}
