package harness

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/internal/session"
)

// TestExamplesLoad guards every committed examples/*.rune: each elaborates and
// type-checks in a fresh session (the examples are self-contained — CRDT specs and
// data-plane demos that `rune simulate`/`rune run` consume without the REPL prelude).
// A rotted example fails here.
func TestExamplesLoad(t *testing.T) {
	entries, err := os.ReadDir(filepath.Join("..", "examples"))
	if err != nil {
		t.Fatal(err)
	}
	n := 0
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".rune") {
			continue
		}
		n++
		t.Run(e.Name(), func(t *testing.T) {
			src, err := os.ReadFile(filepath.Join("..", "examples", e.Name()))
			if err != nil {
				t.Fatal(err)
			}
			s := session.New()
			if _, err := s.LoadSource(string(src)); err != nil {
				t.Errorf("examples/%s did not load: %v", e.Name(), err)
			}
		})
	}
	if n == 0 {
		t.Fatal("no examples/*.rune found")
	}
}
