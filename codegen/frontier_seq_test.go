package codegen_test

import (
	"os/exec"
	"strings"
	"testing"
)

// TestSeqInsideDoIsOrdered asserts that a seq block used as a single do-item
// always prints "hello" before "world", for every seed in the test set.
//
// The listing ch_frontier_seq_order.rune places a single bindIO-sequenced
// IO Unit value (built via a seq block) as the sole item in a do block.
// Because do sees ONE frontier atom, par cannot reorder the internal steps.
// If this test fails, seq is being flattened into the frontier (a desugar bug).
func TestSeqInsideDoIsOrdered(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go not in PATH")
	}
	for _, seed := range []string{"0", "1", "2", "3", "7", "42"} {
		out := emitAndRunGoSeed(t, "ch_frontier_seq_order.rune", "main", seed)
		hi := strings.Index(out, "hello")
		wi := strings.Index(out, "world")
		if hi < 0 || wi < 0 {
			t.Fatalf("seed %s: missing hello or world in output: %q", seed, out)
		}
		if hi > wi {
			t.Fatalf("seq did not order effects under seed %s: %q", seed, out)
		}
	}
}
