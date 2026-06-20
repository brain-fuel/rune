package main

import (
	"os"
	"strings"
	"testing"
)

// TestRunSimulateGCounter drives the `simulate` verb over the shipped G-Counter
// example: it must diverge under the partition and converge after the heal.
func TestRunSimulateGCounter(t *testing.T) {
	src, err := os.ReadFile("../../examples/gcounter.rune")
	if err != nil {
		t.Fatalf("read example: %v", err)
	}
	var out strings.Builder
	if err := runSimulate(string(src), 2, &out); err != nil {
		t.Fatalf("runSimulate: %v", err)
	}
	got := out.String()
	if !strings.Contains(got, "verdict: CONVERGED") {
		t.Errorf("G-Counter should converge:\n%s", got)
	}
	if !strings.Contains(got, "[diverged]") {
		t.Errorf("G-Counter should visibly diverge under the partition:\n%s", got)
	}
}

// TestRunSimulateLWW drives it over the LWW example: a register with no join must
// be reported as non-convergent.
func TestRunSimulateLWW(t *testing.T) {
	src, err := os.ReadFile("../../examples/lww.rune")
	if err != nil {
		t.Fatalf("read example: %v", err)
	}
	var out strings.Builder
	if err := runSimulate(string(src), 2, &out); err != nil {
		t.Fatalf("runSimulate: %v", err)
	}
	if got := out.String(); !strings.Contains(got, "did NOT converge") {
		t.Errorf("LWW should be reported non-convergent:\n%s", got)
	}
}
