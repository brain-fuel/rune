package codegen_test

import (
	"os/exec"
	"strings"
	"testing"
)

// TestSeqOrdersIOEffects asserts that a bare seq (no manual bindIO) with two IO
// actions prints hello before world under every seed in the test set.
// Before the fix this FAILS: seq desugars to a lazy let, so the bound first
// print is never forced and "hello" is missing from the output.
func TestSeqOrdersIOEffects(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go not in PATH")
	}
	for _, seed := range []string{"0", "1", "2", "3", "7", "42"} {
		out := emitAndRunGoSeed(t, "ch_seq_io_order.rune", "main", seed)
		hi := strings.Index(out, "hello")
		wi := strings.Index(out, "world")
		if hi < 0 || wi < 0 {
			t.Fatalf("seed %s: missing hello or world in output: %q", seed, out)
		}
		if hi > wi {
			t.Fatalf("seq did not order IO effects under seed %s: %q", seed, out)
		}
	}
}
