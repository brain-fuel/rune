package sim

import (
	"strconv"
	"strings"

	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/internal/session"
)

// LawReport is the observational verdict on whether a protocol's merge is a join
// semilattice (the CvRDT convergence criterion): commutative, idempotent, and
// associative. These are exactly the laws examples/gcounter.rune PROVES; Diagnose
// checks them by sampling concrete states, so a protocol whose author has not
// proved them still gets told WHICH one fails - the better-than-Winglang linter.
type LawReport struct {
	Commutative bool
	Idempotent  bool
	Associative bool
	// Inflationary records whether every local op only GROWS the state in the
	// merge-induced order (merge s (op s) = op s). A join merge alone does not
	// guarantee convergence: if an update can move a replica DOWN the lattice, two
	// replicas can diverge permanently. This is the op-side companion to the three
	// merge laws, and a violation the merge-law linter alone would miss.
	Inflationary bool
	Notes        []string
}

// IsCvRDT reports whether all three join laws held on the sampled states.
func (r *LawReport) IsCvRDT() bool {
	return r.Commutative && r.Idempotent && r.Associative
}

// IsConvergent reports whether the protocol has BOTH a join merge and inflationary
// ops - the full state-based convergence criterion (Shapiro et al.): a monotone
// semilattice of states with monotone updates.
func (r *LawReport) IsConvergent() bool {
	return r.IsCvRDT() && r.Inflationary
}

// Diagnose samples states (the initial state and the result of each local op, plus
// a couple of merges) and checks merge commutativity, idempotence, and
// associativity observationally over those samples. A failure is reported with a
// concrete witness, so the author sees exactly why convergence would not hold. This
// is a NECESSARY-condition check (a counterexample is conclusive; passing samples is
// strong evidence, not a proof - for the proof, see examples/gcounter.rune).
func Diagnose(sess *session.Session, init, mergeFn string, ops []string) (*LawReport, error) {
	d := &driver{sess: sess}
	mergeRef, err := d.ref(mergeFn)
	if err != nil {
		return nil, err
	}
	base, err := d.eval(init)
	if err != nil {
		return nil, err
	}

	// Sample set: init, each op applied to init, and a merge of the first two ops.
	samples := []core.Tm{base}
	opRefs := make([]core.Tm, 0, len(ops))
	for _, op := range ops {
		ref, err := d.ref(op)
		if err != nil {
			return nil, err
		}
		opRefs = append(opRefs, ref)
		samples = append(samples, d.apply1(ref, base))
	}
	if len(samples) >= 3 {
		samples = append(samples, d.apply2(mergeRef, samples[1], samples[2]))
	}

	show := func(t core.Tm) string { return sess.Pretty(t) }
	rep := &LawReport{Commutative: true, Idempotent: true, Associative: true, Inflationary: true}

	// Inflation: every op must only GROW the state in the merge order - merge s (op s)
	// must equal op s. A violation means an update can move a replica down the lattice,
	// breaking convergence even when merge is a perfect join.
	for oi, op := range opRefs {
		for si := range samples {
			os := d.apply1(op, samples[si])
			grown := d.apply2(mergeRef, samples[si], os)
			if show(grown) != show(os) {
				rep.Inflationary = false
				rep.Notes = append(rep.Notes, "not inflationary: op"+strconv.Itoa(oi)+" on s"+strconv.Itoa(si)+
					" gives "+show(os)+", but merge s"+strconv.Itoa(si)+" (op"+strconv.Itoa(oi)+" s"+strconv.Itoa(si)+
					") = "+show(grown)+" (an update moved the state down the lattice)")
			}
		}
	}

	for i := range samples {
		// Idempotence: merge a a = a.
		aa := d.apply2(mergeRef, samples[i], samples[i])
		if show(aa) != show(samples[i]) {
			rep.Idempotent = false
			rep.Notes = append(rep.Notes, "not idempotent: merge s"+strconv.Itoa(i)+" s"+strconv.Itoa(i)+
				" = "+show(aa)+", but s"+strconv.Itoa(i)+" = "+show(samples[i]))
		}
		for j := range samples {
			// Commutativity: merge a b = merge b a.
			ab := d.apply2(mergeRef, samples[i], samples[j])
			ba := d.apply2(mergeRef, samples[j], samples[i])
			if show(ab) != show(ba) {
				rep.Commutative = false
				rep.Notes = append(rep.Notes, "not commutative: merge s"+strconv.Itoa(i)+" s"+strconv.Itoa(j)+
					" = "+show(ab)+", but merge s"+strconv.Itoa(j)+" s"+strconv.Itoa(i)+" = "+show(ba))
			}
			for k := range samples {
				// Associativity: merge (merge a b) c = merge a (merge b c).
				l := d.apply2(mergeRef, d.apply2(mergeRef, samples[i], samples[j]), samples[k])
				r := d.apply2(mergeRef, samples[i], d.apply2(mergeRef, samples[j], samples[k]))
				if show(l) != show(r) {
					rep.Associative = false
					rep.Notes = append(rep.Notes, "not associative at (s"+strconv.Itoa(i)+
						",s"+strconv.Itoa(j)+",s"+strconv.Itoa(k)+")")
				}
			}
		}
	}
	// Keep the notes short and unique-ish: cap to a few witnesses.
	if len(rep.Notes) > 4 {
		rep.Notes = append(rep.Notes[:4], "(further violations elided)")
	}
	return rep, nil
}

// RenderReport formats a LawReport as a short human verdict.
func RenderReport(r *LawReport) string {
	var b strings.Builder
	mark := func(ok bool) string {
		if ok {
			return "ok "
		}
		return "FAIL"
	}
	b.WriteString("merge laws (CvRDT join):\n")
	b.WriteString("  commutative: " + mark(r.Commutative) + "\n")
	b.WriteString("  idempotent:  " + mark(r.Idempotent) + "\n")
	b.WriteString("  associative: " + mark(r.Associative) + "\n")
	b.WriteString("  inflationary:" + mark(r.Inflationary) + " (updates only grow the state)\n")
	for _, n := range r.Notes {
		b.WriteString("  - " + n + "\n")
	}
	if r.IsConvergent() {
		b.WriteString("verdict: a CvRDT (join merge + inflationary updates) - will converge\n")
	} else if r.IsCvRDT() {
		b.WriteString("verdict: NOT convergent - merge is a join, but an update is not inflationary\n")
	} else {
		b.WriteString("verdict: NOT a CvRDT on the samples - convergence is not guaranteed\n")
	}
	return b.String()
}
