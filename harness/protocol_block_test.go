package harness

import (
	"os"
	"path/filepath"
	"testing"

	"goforge.dev/rune/v3/internal/session"
)

// TestProtocolBlockProvenCvRDT is the acceptance gate for the `protocol … end` block
// (E4 / wavelet): a proven G-Counter CvRDT wrapped in a protocol block elaborates and
// type-checks, with its members passing through bare (so `rune simulate` and the
// serveG projection consume them unchanged). The block enforces the CvRDT contract
// structurally — see surface/protocol_test.go for the rejection of a missing proof.
func TestProtocolBlockProvenCvRDT(t *testing.T) {
	src, err := os.ReadFile(filepath.Join("..", "examples", "gcounter_protocol.rune"))
	if err != nil {
		t.Fatalf("read gcounter_protocol.rune: %v", err)
	}
	s := session.New()
	names, err := s.LoadSource(string(src))
	if err != nil {
		t.Fatalf("protocol-wrapped G-Counter did not type-check: %v", err)
	}
	// The contract members pass through bare (not namespaced), so the simulator and
	// the projection find them.
	for _, want := range []string{"init", "merge", "value", "op0", "mergeComm", "mergeIdem", "mergeAssoc"} {
		if !contains(names, want) {
			t.Errorf("contract member %q is not a bare top-level name", want)
		}
	}
}

func contains(xs []string, x string) bool {
	for _, s := range xs {
		if s == x {
			return true
		}
	}
	return false
}
