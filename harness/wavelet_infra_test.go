package harness

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/internal/session"
)

// TestWaveletInfraInterfaces is the acceptance gate for the agnostic infra surfaces
// (lib/infra/{queue,kv,object}.rune): each elaborates and type-checks on top of the
// REPL prelude, so the wavelet abstractions are real, checked wootz — not sketches.
// The foreign ops are assume-tier axioms here; `rune deploy` binds them per backend.
func TestWaveletInfraInterfaces(t *testing.T) {
	prelude, err := os.ReadFile(filepath.Join("..", "internal", "repl", "prelude.rune"))
	if err != nil {
		t.Fatalf("read prelude: %v", err)
	}
	cases := []struct {
		file string
		want []string
	}{
		{"queue.rune", []string{"enqueue", "dequeue", "QMsg"}},
		{"kv.rune", []string{"put", "get", "del", "KVal"}},
		{"object.rune", []string{"put", "get", "del", "OObj"}},
	}
	for _, c := range cases {
		t.Run(c.file, func(t *testing.T) {
			s := session.New() // fresh session per interface (each carries its own Unit)
			if _, err := s.LoadSource(string(prelude)); err != nil {
				t.Fatalf("prelude failed to load: %v", err)
			}
			src, err := os.ReadFile(filepath.Join("..", "lib", "infra", c.file))
			if err != nil {
				t.Fatalf("read %s: %v", c.file, err)
			}
			names, err := s.LoadSource(string(src))
			if err != nil {
				t.Fatalf("lib/infra/%s did not type-check: %v", c.file, err)
			}
			got := strings.Join(names, " ")
			for _, want := range c.want {
				if !strings.Contains(got, want) {
					t.Errorf("%s missing %q (bound: %s)", c.file, want, got)
				}
			}
		})
	}
}
