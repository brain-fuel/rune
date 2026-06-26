package harness

import "testing"

// TestFrontierOrderIndependentResult is the verification tie-in (Task 7): the
// converged value of a commutative CRDT merge is identical under two different
// frontier seeds. The seeds reorder the two gossip effects, but the listing's
// `mergeValueComm`/`convergedOrderInvariant` proofs certify the result cannot
// depend on the schedule; this test confirms it operationally.
func TestFrontierOrderIndependentResult(t *testing.T) {
	a := emitRunSeed(t, "go", "ch_frontier_order_independent.rune", "main", "1")
	b := emitRunSeed(t, "go", "ch_frontier_order_independent.rune", "main", "9")
	if a != b {
		t.Fatalf("merge result depended on schedule: seed1=%q seed9=%q", a, b)
	}
}
