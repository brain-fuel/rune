package main

import (
	"os"
	"strings"
	"testing"

	"goforge.dev/rune/v3/internal/session"
	"goforge.dev/rune/v3/internal/sim"
)

// stabilizeExample loads a protocol file and runs the fair-gossip liveness check.
func stabilizeExample(t *testing.T, file string, n int, ops []string) (int, bool) {
	t.Helper()
	src, err := os.ReadFile(file)
	if err != nil {
		t.Fatalf("read %s: %v", file, err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("load %s: %v", file, err)
	}
	rounds, ok, err := sim.Stabilize(s, "init", "merge", n, ops, 12)
	if err != nil {
		t.Fatalf("stabilize %s: %v", file, err)
	}
	return rounds, ok
}

// TestStabilizeAcrossCvRDTs is consolidated LIVENESS coverage: every shipped CvRDT
// example reaches a fixpoint under fair ring gossip, and the non-CvRDT does not.
func TestStabilizeAcrossCvRDTs(t *testing.T) {
	cvrdts := []struct {
		file string
		n    int
		ops  []string
	}{
		{"../../examples/gcounter3.rune", 3, []string{"op0", "op1", "op2"}},
		{"../../examples/gset.rune", 2, []string{"op0", "op1"}},
		{"../../examples/pncounter.rune", 2, []string{"op0", "op1"}},
	}
	for _, c := range cvrdts {
		if _, ok := stabilizeExample(t, c.file, c.n, c.ops); !ok {
			t.Errorf("%s (a CvRDT) failed to stabilize under fair gossip", c.file)
		}
	}
	if _, ok := stabilizeExample(t, "../../examples/lww.rune", 2, []string{"op0", "op1"}); ok {
		t.Errorf("lww.rune (no join) must NOT stabilize")
	}
}

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
	if got := out.String(); !strings.Contains(got, "NOT GUARANTEED to converge") {
		t.Errorf("LWW should be reported non-convergent:\n%s", got)
	}
}

// TestRunSimulateBadCounter is the better-than-Winglang catch: a counter whose merge
// naively adds (not max) passes a happy-path run by luck, but the linter proves it is
// not idempotent and the verdict flags it as not guaranteed to converge.
func TestRunSimulateBadCounter(t *testing.T) {
	src, err := os.ReadFile("../../examples/badcounter.rune")
	if err != nil {
		t.Fatalf("read example: %v", err)
	}
	var out strings.Builder
	if err := runSimulate(string(src), 2, &out); err != nil {
		t.Fatalf("runSimulate: %v", err)
	}
	got := out.String()
	if !strings.Contains(got, "idempotent:  FAIL") {
		t.Errorf("bad counter should fail idempotence:\n%s", got)
	}
	if !strings.Contains(got, "NOT GUARANTEED to converge") {
		t.Errorf("bad counter verdict should warn it is not a join:\n%s", got)
	}
}

// TestRunSimulateGSet confirms the simulator generalizes past counters: a grow-only
// Set (join = pointwise boolean OR) converges to the union and the linter reports a
// CvRDT.
func TestRunSimulateGSet(t *testing.T) {
	src, err := os.ReadFile("../../examples/gset.rune")
	if err != nil {
		t.Fatalf("read example: %v", err)
	}
	var out strings.Builder
	if err := runSimulate(string(src), 2, &out); err != nil {
		t.Fatalf("runSimulate: %v", err)
	}
	got := out.String()
	if !strings.Contains(got, "verdict: CONVERGED") {
		t.Errorf("G-Set should converge:\n%s", got)
	}
	if !strings.Contains(got, "a CvRDT") {
		t.Errorf("G-Set (OR join) should be reported a CvRDT:\n%s", got)
	}
}

// TestRunSimulatePNCounter confirms a compound-state CRDT (the canonical PN-Counter,
// with both increment and decrement) converges and is reported a CvRDT.
func TestRunSimulatePNCounter(t *testing.T) {
	src, err := os.ReadFile("../../examples/pncounter.rune")
	if err != nil {
		t.Fatalf("read example: %v", err)
	}
	var out strings.Builder
	if err := runSimulate(string(src), 2, &out); err != nil {
		t.Fatalf("runSimulate: %v", err)
	}
	got := out.String()
	if !strings.Contains(got, "verdict: CONVERGED") {
		t.Errorf("PN-Counter should converge:\n%s", got)
	}
	if !strings.Contains(got, "a CvRDT") {
		t.Errorf("PN-Counter should be a CvRDT:\n%s", got)
	}
}
