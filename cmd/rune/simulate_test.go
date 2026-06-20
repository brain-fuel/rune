package main

import (
	"os"
	"strings"
	"testing"

	"goforge.dev/rune/v3/internal/session"
)

// TestGCounterExampleTriad confirms one verified source PROVES, SIMULATES, and
// DEPLOYS: the example loads (so convergedCorrect, the convergence proof, checks),
// rune simulate converges on it, and `converged` (the deploy scenario value) reduces
// to 3 - the value codegen emits and `rune run` executes on a backend.
func TestGCounterExampleTriad(t *testing.T) {
	src, err := os.ReadFile("../../examples/gcounter.rune")
	if err != nil {
		t.Fatalf("read example: %v", err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("PROVE: example failed to load/check (proofs included): %v", err)
	}
	e, err := s.ParseSrcExpr("converged")
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatalf("elab: %v", err)
	}
	if got := s.Pretty(s.NormalizeExpr(tm)); got != "succ (succ (succ zero))" {
		t.Errorf("DEPLOY: converged should reduce to 3, got %q", got)
	}
}

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

// TestRunSimulateThreeReplicas checks the simulator scales past two nodes: the
// three-replica G-Counter converges after the partition heals.
func TestRunSimulateThreeReplicas(t *testing.T) {
	src, err := os.ReadFile("../../examples/gcounter3.rune")
	if err != nil {
		t.Fatalf("read example: %v", err)
	}
	var out strings.Builder
	if err := runSimulate(string(src), 3, &out); err != nil {
		t.Fatalf("runSimulate: %v", err)
	}
	if got := out.String(); !strings.Contains(got, "verdict: CONVERGED") {
		t.Errorf("3-replica G-Counter should converge:\n%s", got)
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
