package ledger

import (
	"os/exec"
	"strings"
	"testing"
)

// TestLineOf validates the lineOf helper that Build uses to map byte offsets to
// 1-based line numbers.
func TestLineOf(t *testing.T) {
	src := "a\nbb\nccc"
	if got := lineOf(src, 0); got != 1 {
		t.Fatalf("offset 0 want line 1, got %d", got)
	}
	if got := lineOf(src, 2); got != 2 {
		t.Fatalf("offset 2 want line 2, got %d", got)
	}
	if got := lineOf(src, 5); got != 3 {
		t.Fatalf("offset 5 want line 3, got %d", got)
	}
}

// TestGitBlameDegradesGracefully confirms that blaming a nonexistent path returns
// a non-nil error and a zero-valued Provenance without panicking.
func TestGitBlameDegradesGracefully(t *testing.T) {
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git not in PATH")
	}
	p, err := GitBlame("/nonexistent/file.rune", 1)
	if err == nil {
		t.Fatalf("expected an error blaming a nonexistent file")
	}
	if p.Author != "" || p.Commit != "" {
		t.Fatalf("failed blame must be zero-valued, got author=%q commit=%q",
			strings.TrimSpace(p.Author), strings.TrimSpace(p.Commit))
	}
}
