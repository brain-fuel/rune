package sim

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/internal/session"
)

// natPrelude + the two protocols are ordinary verified Rune: the simulator runs the
// SAME source a proof or a deployment would.

const gcounterSrc = `
data Nat : U is zero : Nat | succ : Nat -> Nat end
data GC : U is gc : Nat -> Nat -> GC end

max : Nat -> Nat -> Nat is
  fn (a : Nat) is
    NatElim (fn (w : Nat) is Nat -> Nat end)
      (fn (b : Nat) is b end)
      (fn (k : Nat) (ih : Nat -> Nat) is
         fn (b : Nat) is
           NatElim (fn (w : Nat) is Nat end)
             (succ k)
             (fn (j : Nat) (ihj : Nat) is succ (ih j) end)
             b
         end
       end)
      a
  end
end

add : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (w : Nat) is Nat end) b (fn (k : Nat) (ih : Nat) is succ ih end) a
  end
end

g0 : GC -> Nat is fn (s : GC) is GCElim (fn (w : GC) is Nat end) (fn (a : Nat) (b : Nat) is a end) s end end
g1 : GC -> Nat is fn (s : GC) is GCElim (fn (w : GC) is Nat end) (fn (a : Nat) (b : Nat) is b end) s end end

inc0 : GC -> GC is fn (s : GC) is gc (succ (g0 s)) (g1 s) end end
inc1 : GC -> GC is fn (s : GC) is gc (g0 s) (succ (g1 s)) end end

mergeGC : GC -> GC -> GC is fn (x : GC) (y : GC) is gc (max (g0 x) (g0 y)) (max (g1 x) (g1 y)) end end
valueGC : GC -> Nat is fn (s : GC) is add (g0 s) (g1 s) end end
`

const lwwSrc = `
data Nat : U is zero : Nat | succ : Nat -> Nat end
data LW : U is lw : Nat -> LW end

getLW : LW -> Nat is fn (s : LW) is LWElim (fn (w : LW) is Nat end) (fn (a : Nat) is a end) s end end

w2 : LW -> LW is fn (s : LW) is lw (succ (succ zero)) end end
w1 : LW -> LW is fn (s : LW) is lw (succ zero) end end

mergeLW : LW -> LW -> LW is fn (x : LW) (y : LW) is y end end
valueLW : LW -> Nat is fn (s : LW) is getLW s end end
`

func loadSession(t *testing.T, src string) *session.Session {
	t.Helper()
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load source: %v", err)
	}
	return s
}

// observedAt returns the per-replica observed values recorded at a given step.
func observedAt(run *Run, step, n int) []string {
	out := make([]string, n)
	for _, e := range run.Events {
		if e.Kind == "observe" && e.Step == step {
			out[e.Replica] = e.Detail
		}
	}
	return out
}

// TestGCounterDivergesThenConverges is the headline Lambert gate: under a network
// partition the two replicas diverge (one sees 2, the other 1), and once the
// partition heals the join (pairwise max) re-converges them to 3. Watched, not
// asserted in the abstract.
func TestGCounterDivergesThenConverges(t *testing.T) {
	s := loadSession(t, gcounterSrc)
	rounds := []Round{
		{Local: map[int]string{0: "inc0"}},                      // step 0: r0 += 1
		{Local: map[int]string{0: "inc0", 1: "inc1"}, Gossip: true}, // step 1: r0 += 1, r1 += 1, gossip PARTITIONED
		{Gossip: true},                                          // step 2: gossip heals
	}
	pol := FaultPolicy{Partitioned: func(step, i, j int) bool { return step == 1 }}

	run, err := Simulate(s, "gc zero zero", "mergeGC", "valueGC", 2, rounds, pol)
	if err != nil {
		t.Fatalf("simulate: %v", err)
	}

	// During the partition (step 1) the replicas disagree: r0 = 2, r1 = 1.
	during := observedAt(run, 1, 2)
	if during[0] == during[1] {
		t.Errorf("expected divergence under partition, both saw %q", during[0])
	}
	if !strings.Contains(during[0], "succ (succ zero)") {
		t.Errorf("replica 0 should observe 2 during partition, got %q", during[0])
	}

	// After healing, the CRDT converges, and to the CORRECT value (3 = no lost increments).
	if !run.Converged() {
		t.Errorf("G-Counter failed to converge after heal: final = %v", run.Final)
	}
	if !strings.Contains(run.Final[0], "succ (succ (succ zero))") {
		t.Errorf("converged value should be 3, got %q", run.Final[0])
	}

	// A drop event was actually recorded (the partition bit fired).
	var dropped bool
	for _, e := range run.Events {
		if e.Kind == "drop" {
			dropped = true
		}
	}
	if !dropped {
		t.Errorf("expected a dropped-gossip event during the partition")
	}
}

// TestGCounterRobustToDropAndDup shows the CvRDT magic operationally: the failure
// model is a hypothesis the result is robust to. With gossip dropped on one round
// and DUPLICATED on the next, the counter converges to the SAME value a healthy run
// reaches (no lost increments, no double-counting) because merge is an idempotent
// join. The simulator exhibits what the convergence proof asserts.
func TestGCounterRobustToDropAndDup(t *testing.T) {
	s := loadSession(t, gcounterSrc)
	rounds := []Round{
		{Local: map[int]string{0: "inc0", 1: "inc1"}, Gossip: true},
		{Gossip: true},
		{Gossip: true},
	}

	healthy, err := Simulate(s, "gc zero zero", "mergeGC", "valueGC", 2, rounds, FaultPolicy{})
	if err != nil {
		t.Fatalf("healthy simulate: %v", err)
	}
	faulty, err := Simulate(s, "gc zero zero", "mergeGC", "valueGC", 2, rounds, FaultPolicy{
		Partitioned: func(step, i, j int) bool { return step == 0 }, // drop round 0
		Duplicate:   func(step, i, j int) bool { return step == 1 }, // double-deliver round 1
	})
	if err != nil {
		t.Fatalf("faulty simulate: %v", err)
	}

	if !faulty.Converged() {
		t.Errorf("G-Counter must converge despite drop+dup, got %v", faulty.Final)
	}
	if faulty.Final[0] != healthy.Final[0] {
		t.Errorf("drop+dup changed the outcome: healthy=%q faulty=%q", healthy.Final[0], faulty.Final[0])
	}
}

// TestLWWStaysDivergent is the contrast: a last-writer-wins register has no join
// (merge just takes the peer), so even on a HEALTHY network concurrent writes are
// never reconciled. The simulator distinguishes it from the CvRDT with no appeal to
// any proof, purely by observed behaviour.
func TestLWWStaysDivergent(t *testing.T) {
	s := loadSession(t, lwwSrc)
	rounds := []Round{
		{Local: map[int]string{0: "w2", 1: "w1"}}, // r0 writes 2, r1 writes 1
		{Gossip: true},                             // healthy gossip, but no join
	}
	run, err := Simulate(s, "lw zero", "mergeLW", "valueLW", 2, rounds, FaultPolicy{})
	if err != nil {
		t.Fatalf("simulate: %v", err)
	}
	if run.Converged() {
		t.Errorf("LWW must NOT converge (no join), but did: final = %v", run.Final)
	}
}
