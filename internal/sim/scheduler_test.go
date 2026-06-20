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

const gcounter3Src = `
data Nat : U is zero : Nat | succ : Nat -> Nat end
data GC : U is gc : Nat -> Nat -> Nat -> GC end

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

h0 : GC -> Nat is fn (s : GC) is GCElim (fn (w : GC) is Nat end) (fn (a : Nat) (b : Nat) (c : Nat) is a end) s end end
h1 : GC -> Nat is fn (s : GC) is GCElim (fn (w : GC) is Nat end) (fn (a : Nat) (b : Nat) (c : Nat) is b end) s end end
h2 : GC -> Nat is fn (s : GC) is GCElim (fn (w : GC) is Nat end) (fn (a : Nat) (b : Nat) (c : Nat) is c end) s end end

op0 : GC -> GC is fn (s : GC) is gc (succ (h0 s)) (h1 s) (h2 s) end end
op1 : GC -> GC is fn (s : GC) is gc (h0 s) (succ (h1 s)) (h2 s) end end
op2 : GC -> GC is fn (s : GC) is gc (h0 s) (h1 s) (succ (h2 s)) end end

mergeGC : GC -> GC -> GC is
  fn (x : GC) (y : GC) is gc (max (h0 x) (h0 y)) (max (h1 x) (h1 y)) (max (h2 x) (h2 y)) end
end
valueGC : GC -> Nat is fn (s : GC) is add (h0 s) (add (h1 s) (h2 s)) end end
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

// TestRenderShowsTheStory checks the teachable surface: the rendered trace marks
// the partition step as diverged and the healed step as converged, and prints it so
// the behaviour is visible (run with -v).
func TestRenderShowsTheStory(t *testing.T) {
	s := loadSession(t, gcounterSrc)
	rounds := []Round{
		{Local: map[int]string{0: "inc0"}},
		{Local: map[int]string{0: "inc0", 1: "inc1"}, Gossip: true},
		{Gossip: true},
	}
	pol := FaultPolicy{Partitioned: func(step, i, j int) bool { return step == 1 }}
	run, err := Simulate(s, "gc zero zero", "mergeGC", "valueGC", 2, rounds, pol)
	if err != nil {
		t.Fatalf("simulate: %v", err)
	}
	out := Render(run, 2)
	t.Logf("\n%s", out)
	lines := strings.Split(strings.TrimRight(out, "\n"), "\n")
	if len(lines) != 3 {
		t.Fatalf("expected 3 step lines, got %d:\n%s", len(lines), out)
	}
	if !strings.Contains(lines[1], "DROP{") || !strings.Contains(lines[1], "[diverged]") {
		t.Errorf("partition step should show a drop and divergence: %q", lines[1])
	}
	if !strings.Contains(lines[2], "[converged]") {
		t.Errorf("healed step should show convergence: %q", lines[2])
	}
}

// TestGCounterToleratesCrashAndRecovery shows fault tolerance and durability: three
// replicas each increment once; replica 2 then CRASHES across the gossip rounds, so
// the two survivors converge among themselves (to 2) while the crashed node is
// frozen; once replica 2 recovers, gossip catches it up and all three converge to 3
// - replica 2's pre-crash increment is NOT lost, because the join is durable.
func TestGCounterToleratesCrashAndRecovery(t *testing.T) {
	s := loadSession(t, gcounter3Src)
	rounds := []Round{
		{Local: map[int]string{0: "op0", 1: "op1", 2: "op2"}}, // all act
		{Gossip: true}, // replica 2 crashed (see policy): survivors converge to 2
		{Gossip: true}, // replica 2 recovered: all converge to 3
	}
	pol := FaultPolicy{Crashed: func(step, i int) bool { return step == 1 && i == 2 }}
	run, err := Simulate(s, "gc zero zero zero", "mergeGC", "valueGC", 3, rounds, pol)
	if err != nil {
		t.Fatalf("simulate: %v", err)
	}

	// While replica 2 is down, the survivors (0,1) agree but replica 2 lags.
	during := observedAt(run, 1, 3)
	if during[0] != during[1] {
		t.Errorf("survivors should agree during the crash, got r0=%q r1=%q", during[0], during[1])
	}
	if during[2] == during[0] {
		t.Errorf("crashed replica should lag the survivors, both at %q", during[0])
	}

	// After recovery, everyone converges and replica 2's increment survived (total 3).
	if !run.Converged() {
		t.Errorf("must converge after recovery, got %v", run.Final)
	}
	if !strings.Contains(run.Final[0], "succ (succ (succ zero))") {
		t.Errorf("converged total should be 3 (no lost increment), got %q", run.Final[0])
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

// TestDiagnoseDistinguishesByLaws checks the observational CvRDT linter: the
// G-Counter passes all three join laws; the LWW register fails commutativity, and
// Diagnose names that failure.
func TestDiagnoseDistinguishesByLaws(t *testing.T) {
	gc := loadSession(t, gcounterSrc)
	rep, err := Diagnose(gc, "gc zero zero", "mergeGC", []string{"inc0", "inc1"})
	if err != nil {
		t.Fatalf("diagnose gc: %v", err)
	}
	if !rep.IsCvRDT() {
		t.Errorf("G-Counter should pass all join laws, got %s", RenderReport(rep))
	}

	lww := loadSession(t, lwwSrc)
	rep2, err := Diagnose(lww, "lw zero", "mergeLW", []string{"w2", "w1"})
	if err != nil {
		t.Fatalf("diagnose lww: %v", err)
	}
	if rep2.Commutative {
		t.Errorf("LWW merge (take-peer) is not commutative, but linter said it was:\n%s", RenderReport(rep2))
	}
	if rep2.IsCvRDT() {
		t.Errorf("LWW must not be reported a CvRDT:\n%s", RenderReport(rep2))
	}
	t.Logf("LWW report:\n%s", RenderReport(rep2))
}

// TestStabilizeLiveness checks eventual consistency under fair ring gossip: the
// 3-replica G-Counter reaches a global fixpoint within n-1 rounds, while the LWW
// register never stabilizes (ring delivery rotates its values forever).
func TestStabilizeLiveness(t *testing.T) {
	gc := loadSession(t, gcounter3Src)
	rounds, ok, err := Stabilize(gc, "gc zero zero zero", "mergeGC", 3, []string{"op0", "op1", "op2"}, 10)
	if err != nil {
		t.Fatalf("stabilize gc: %v", err)
	}
	if !ok {
		t.Errorf("G-Counter must stabilize under fair gossip")
	}
	if rounds > 2 {
		t.Errorf("3-node ring should stabilize within n-1=2 rounds, took %d", rounds)
	}

	lww := loadSession(t, lwwSrc)
	_, ok2, err := Stabilize(lww, "lw zero", "mergeLW", 2, []string{"w2", "w1"}, 10)
	if err != nil {
		t.Fatalf("stabilize lww: %v", err)
	}
	if ok2 {
		t.Errorf("LWW must NOT stabilize (no join)")
	}
}
