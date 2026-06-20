// Package sim is the "better-than-Winglang" simulator (E4 / C-INFRA), the first
// slice of the infra-as-code surface. It is SHADOW TOOLING, a consumer of the
// kernel, never part of core/store: it drives a replicated protocol forward by
// folding the protocol's OWN verified Rune operations (merge, the local actions,
// the observable value) under a pluggable fault policy, and surfaces each step as
// an Event the REPL/CLI can print.
//
// The point (Lambert's teachable gate): the SAME verified source that carries the
// convergence proof is what the simulator runs, so a learner watches replicas
// diverge under a network partition and then re-converge after it heals, and the
// simulator DISTINGUISHES a convergent protocol (a CvRDT whose merge is a join)
// from a non-convergent one (a last-writer-wins register), with no appeal to the
// proof, just the observed behaviour.
package sim

import (
	"sort"
	"strconv"
	"strings"

	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/internal/session"
	"goforge.dev/rune/v3/surface"
)

// Event is one observable moment of a run, for printing or assertion.
type Event struct {
	Step    int
	Kind    string // "local" | "gossip" | "drop" | "observe"
	Replica int    // the replica acted on (-1 for whole-system events)
	Peer    int    // the gossip peer (-1 when not applicable)
	Detail  string // a human note, e.g. the observed value
}

// FaultPolicy perturbs the network, deterministically (so a run is reproducible).
// Partitioned reports whether, at a given step, replica i cannot hear from replica j
// (anti-entropy gossip i<-j is dropped). Duplicate reports whether that same gossip
// is delivered TWICE — harmless to a CvRDT, since a join is idempotent, which the
// simulator then shows. A nil function is the benign default.
type FaultPolicy struct {
	Partitioned func(step, i, j int) bool
	Duplicate   func(step, i, j int) bool
	// Crashed reports whether replica i is down at a step: it performs no local
	// action and neither sends nor receives gossip. A crash can be transient (true
	// for some steps, false later), modelling recovery: when it comes back, the join
	// brings it up to date without losing the work it did before crashing.
	Crashed func(step, i int) bool
}

func (p FaultPolicy) cut(step, i, j int) bool {
	return p.Partitioned != nil && p.Partitioned(step, i, j)
}

func (p FaultPolicy) dup(step, i, j int) bool {
	return p.Duplicate != nil && p.Duplicate(step, i, j)
}

func (p FaultPolicy) crashed(step, i int) bool {
	return p.Crashed != nil && p.Crashed(step, i)
}

// Round is one simulated time step: each replica optionally performs a local
// action (a unary Rune function applied to its own state), then, if Gossip is set,
// an anti-entropy round merges every replica's state with each peer it can hear.
type Round struct {
	Local  map[int]string // replica index -> unary op function name (the local action)
	Gossip bool
}

// Run is the outcome of a simulation: the event log plus the final observed value
// at each replica (pretty-printed through the protocol's `value` function).
type Run struct {
	Events []Event
	Final  []string
}

// Converged reports whether every replica ended at the same observed value.
func (r *Run) Converged() bool {
	for i := 1; i < len(r.Final); i++ {
		if r.Final[i] != r.Final[0] {
			return false
		}
	}
	return len(r.Final) > 0
}

// Simulate drives n replicas through the given rounds. init is a closed Rune
// expression for a replica's initial state; mergeFn is a binary join, valueFn a
// unary observable, all resolved from the session. Every state transition is the
// kernel normalizing the protocol's own terms, so the simulator runs exactly the
// verified source.
func Simulate(sess *session.Session, init, mergeFn, valueFn string, n int, rounds []Round, pol FaultPolicy) (*Run, error) {
	d := &driver{sess: sess}
	mergeRef, err := d.ref(mergeFn)
	if err != nil {
		return nil, err
	}
	valueRef, err := d.ref(valueFn)
	if err != nil {
		return nil, err
	}
	state0, err := d.eval(init)
	if err != nil {
		return nil, err
	}
	states := make([]core.Tm, n)
	for i := range states {
		states[i] = state0
	}

	run := &Run{}
	observe := func(step int) {
		for i := 0; i < n; i++ {
			run.Events = append(run.Events, Event{
				Step: step, Kind: "observe", Replica: i, Peer: -1,
				Detail: sess.Pretty(d.apply1(valueRef, states[i])),
			})
		}
	}

	for step, rd := range rounds {
		// Local actions first (a crashed replica does nothing this step).
		for i := 0; i < n; i++ {
			if pol.crashed(step, i) {
				if _, ok := rd.Local[i]; ok {
					run.Events = append(run.Events, Event{Step: step, Kind: "crash", Replica: i, Peer: -1})
				}
				continue
			}
			fn, ok := rd.Local[i]
			if !ok {
				continue
			}
			ref, err := d.ref(fn)
			if err != nil {
				return nil, err
			}
			states[i] = d.apply1(ref, states[i])
			run.Events = append(run.Events, Event{Step: step, Kind: "local", Replica: i, Peer: -1, Detail: fn})
		}
		// Anti-entropy gossip: snapshot, then each replica merges in every peer it
		// can hear (a partitioned peer's update is dropped).
		if rd.Gossip {
			snap := make([]core.Tm, n)
			copy(snap, states)
			for i := 0; i < n; i++ {
				for j := 0; j < n; j++ {
					if i == j {
						continue
					}
					// A crashed replica neither receives (i) nor sends (j).
					if pol.crashed(step, i) || pol.crashed(step, j) {
						continue
					}
					if pol.cut(step, i, j) {
						run.Events = append(run.Events, Event{Step: step, Kind: "drop", Replica: i, Peer: j})
						continue
					}
					states[i] = d.apply2(mergeRef, states[i], snap[j])
					run.Events = append(run.Events, Event{Step: step, Kind: "gossip", Replica: i, Peer: j})
					if pol.dup(step, i, j) {
						// Re-deliver the same update; a join absorbs it (idempotence).
						states[i] = d.apply2(mergeRef, states[i], snap[j])
						run.Events = append(run.Events, Event{Step: step, Kind: "dup", Replica: i, Peer: j})
					}
				}
			}
		}
		observe(step)
	}

	run.Final = make([]string, n)
	for i := range states {
		run.Final[i] = sess.Pretty(d.apply1(valueRef, states[i]))
	}
	return run, nil
}

// Render turns a run into a human-readable trace: one line per step showing the
// local actions, any dropped or duplicated gossip, and the observed value at each
// replica, with a note when the replicas are diverged or (re)converged. This is the
// teachable surface, the learner watching the protocol behave, not an abstraction.
func Render(run *Run, n int) string {
	steps := map[int][]Event{}
	var order []int
	for _, e := range run.Events {
		if _, seen := steps[e.Step]; !seen {
			order = append(order, e.Step)
		}
		steps[e.Step] = append(steps[e.Step], e)
	}
	sort.Ints(order)

	var b strings.Builder
	for _, step := range order {
		var locals, drops, dups []string
		obs := make([]string, n)
		gossiped := false
		for _, e := range steps[step] {
			switch e.Kind {
			case "local":
				locals = append(locals, "r"+strconv.Itoa(e.Replica)+":"+e.Detail)
			case "drop":
				drops = append(drops, "r"+strconv.Itoa(e.Replica)+"<-r"+strconv.Itoa(e.Peer))
			case "dup":
				dups = append(dups, "r"+strconv.Itoa(e.Replica)+"<-r"+strconv.Itoa(e.Peer))
			case "gossip":
				gossiped = true
			case "observe":
				obs[e.Replica] = e.Detail
			}
		}
		b.WriteString("step " + strconv.Itoa(step) + " ")
		if len(locals) > 0 {
			b.WriteString("[" + strings.Join(locals, " ") + "] ")
		}
		if gossiped {
			b.WriteString("gossip ")
		}
		if len(drops) > 0 {
			b.WriteString("DROP{" + strings.Join(drops, ",") + "} ")
		}
		if len(dups) > 0 {
			b.WriteString("DUP{" + strings.Join(dups, ",") + "} ")
		}
		cells := make([]string, n)
		for i := 0; i < n; i++ {
			cells[i] = "r" + strconv.Itoa(i) + "=" + obs[i]
		}
		b.WriteString("=> " + strings.Join(cells, " "))
		if allEqual(obs) {
			b.WriteString("   [converged]")
		} else {
			b.WriteString("   [diverged]")
		}
		b.WriteString("\n")
	}
	return b.String()
}

func allEqual(xs []string) bool {
	for i := 1; i < len(xs); i++ {
		if xs[i] != xs[0] {
			return false
		}
	}
	return len(xs) > 0
}

// driver bundles the session-evaluation helpers the simulator uses to run Rune
// terms: resolving a name to its reference, evaluating a closed source expression,
// and applying a function reference to already-normalized argument terms.
type driver struct {
	sess *session.Session
}

func (d *driver) ref(name string) (core.Tm, error) {
	return d.sess.ResolveExpr(surface.EVar{Name: name})
}

func (d *driver) eval(src string) (core.Tm, error) {
	e, err := d.sess.ParseSrcExpr(src)
	if err != nil {
		return nil, err
	}
	tm, _, err := d.sess.ElabExpr(e)
	if err != nil {
		return nil, err
	}
	return d.sess.NormalizeExpr(tm), nil
}

func (d *driver) apply1(f, x core.Tm) core.Tm {
	return d.sess.NormalizeExpr(core.App{Fn: f, Arg: x, Icit: core.Expl})
}

func (d *driver) apply2(f, x, y core.Tm) core.Tm {
	inner := core.App{Fn: f, Arg: x, Icit: core.Expl}
	return d.sess.NormalizeExpr(core.App{Fn: inner, Arg: y, Icit: core.Expl})
}
