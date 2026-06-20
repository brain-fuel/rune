package sim

import (
	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/internal/session"
)

// Stabilize models the LIVENESS property "eventually converges under fair gossip".
// Where Simulate runs a fixed schedule, Stabilize asks a question: starting from one
// local action per replica, and gossiping on a RING (each replica hears only its
// left neighbour once per round, the weakest fair topology), does the system reach a
// global FIXPOINT - every replica in the identical state - and in how many rounds?
//
// A CvRDT (an idempotent, commutative, associative join) always stabilizes: on a
// ring of n nodes every update reaches every node within n-1 rounds, and idempotence
// makes the fixpoint absorbing. A protocol without a join (a last-writer-wins
// register) never stabilizes - ring delivery just rotates the values forever. So the
// round count is a witness of eventual consistency, and a false return is a witness
// that the protocol is not live.
func Stabilize(sess *session.Session, init, mergeFn string, n int, ops []string, maxRounds int) (rounds int, converged bool, err error) {
	d := &driver{sess: sess}
	mergeRef, err := d.ref(mergeFn)
	if err != nil {
		return 0, false, err
	}
	base, err := d.eval(init)
	if err != nil {
		return 0, false, err
	}
	states := make([]core.Tm, n)
	for i := range states {
		states[i] = base
	}
	// One local action per replica (op i on replica i), where provided.
	for i := 0; i < n && i < len(ops); i++ {
		if ops[i] == "" {
			continue
		}
		ref, err := d.ref(ops[i])
		if err != nil {
			return 0, false, err
		}
		states[i] = d.apply1(ref, states[i])
	}

	fixed := func() bool {
		first := sess.Pretty(states[0])
		for i := 1; i < n; i++ {
			if sess.Pretty(states[i]) != first {
				return false
			}
		}
		return true
	}
	if fixed() {
		return 0, true, nil
	}

	for r := 1; r <= maxRounds; r++ {
		snap := make([]core.Tm, n)
		copy(snap, states)
		for i := 0; i < n; i++ {
			left := (i - 1 + n) % n
			states[i] = d.apply2(mergeRef, states[i], snap[left])
		}
		if fixed() {
			return r, true, nil
		}
	}
	return maxRounds, false, nil
}
