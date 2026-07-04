package harness

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/internal/prelude"
	"goforge.dev/rune/v3/internal/session"
)

// TestExamplesLoad guards every committed examples/*.rune: each elaborates and
// type-checks in a fresh session. Examples that use `import` declarations (e.g.
// double.rune importing Std.Float) get the shared prelude loaded first, matching
// the CLI's always-on prelude behaviour. Self-contained CRDT examples that define
// their own numeric tower are loaded bare (they may shadow prelude names such as
// `succ`/`zero` and would conflict if the prelude were loaded first).
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
			// Infra MANIFESTS share the .rune extension but are the manifest
			// language (`<kind> <name> [key=val...]`, `#` comments), not rune
			// source. They are gated by cmd/rune's manifest/deploy/calm tests
			// (TestExampleManifest, TestDeployDemoHCL, TestFiveOutputs*).
			if strings.HasPrefix(strings.TrimSpace(string(src)), "#") {
				t.Skipf("examples/%s is an infra manifest, not rune source", e.Name())
			}
			s := session.New()
			// Examples that use `import` declarations need the prelude; bare
			// examples (e.g. CRDT specs that define their own Nat tower) do not.
			if strings.Contains(string(src), "import ") {
				if _, err := s.LoadSource(prelude.Source()); err != nil {
					t.Fatalf("examples/%s: loading prelude: %v", e.Name(), err)
				}
			}
			if _, err := s.LoadSource(string(src)); err != nil {
				t.Errorf("examples/%s did not load: %v", e.Name(), err)
			}
		})
	}
	if n == 0 {
		t.Fatal("no examples/*.rune found")
	}
}
