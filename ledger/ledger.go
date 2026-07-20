// Package ledger computes the assurance ledger: a tier (postulate/assume/guard/
// proven) plus a content-hashed proposition identity and proof hash for every
// definition in a session. It is pure read-side tooling over the session and the
// store; it adds no kernel machinery and hashes nothing new (the proposition is
// the type hash, the proof is the body hash, both already computed by core).
package ledger

import (
	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/internal/session"
)

// Tier is the assurance level of a claim, strongest first.
type Tier int

const (
	Proven    Tier = iota // a checked proof term (body present + certified)
	Guard                 // a runtime contract (the with-post-guard sugar)
	Assume                // a trusted foreign/host binding (bodiless)
	Postulate             // an asserted axiom, a debt to be paid down (bodiless)
	Unproven              // has a body but no proof certificate yet
)

func (t Tier) String() string {
	switch t {
	case Proven:
		return "proven"
	case Guard:
		return "guard"
	case Assume:
		return "assume"
	case Postulate:
		return "postulate"
	default:
		return "unproven"
	}
}

// Provenance is the version-control attribution of a claim's witness.
type Provenance struct {
	Author string
	Date   string
	Commit string
}

// Entry is one definition's ledger row.
type Entry struct {
	Name       string
	Tier       Tier
	PropHash   core.Hash // identity of the CLAIM = content hash of the type
	ProofHash  core.Hash // identity of the WITNESS = content hash of the body (zero if bodiless)
	Why        string    // postulate/assume reason (Task 4)
	File       string    // source file (Task 6)
	Line       int       // 1-based source line (Task 6)
	Provenance Provenance
}

// Build classifies every definition in the session.
// Kernel-generated bodiless furniture (datatype formers, constructors,
// eliminators, and builtin group members) is excluded: these are not
// control claims and cluttering the ledger with them obscures the real
// assurance picture. The filter is: skip any def where Body==nil and
// session.Assumed reports false (assumed == foreign or postulate only).
func Build(s *session.Session) []Entry {
	defs := s.Defs()
	out := make([]Entry, 0, len(defs))
	for _, d := range defs {
		// Skip kernel-generated bodiless furniture: datatype formers,
		// constructors, eliminators, and builtins. Only assumed bodiless defs
		// (foreign/postulate) are real control claims.
		if d.Body == nil && !s.Assumed(d.Name) {
			continue
		}
		e := Entry{
			Name:     d.Name,
			PropHash: core.HashTerm(d.Ty),
		}
		if d.Body != nil {
			e.ProofHash = core.HashTerm(d.Body)
		}
		e.Tier = classify(s, d)
		e.Why = d.Why
		out = append(out, e)
	}
	return out
}

// classify picks the tier from the facts available so far.
//
// Order: bodiless -> Postulate/Assume; body + UsesGuard -> Guard; body +
// Certified -> Proven; else Unproven.
//
// Bodiless note: Build filters out all non-assumed bodiless defs (datatype
// formers/constructors/eliminators/builtins) before calling classify, so the
// bodiless branch here handles only assumed defs (foreign or postulate).
//
// Guard-before-Certified is intentional: a definition resting on a runtime
// contract (the with-post-guard sugar) is guard-tier even though its own
// type-check certifies. Because every successfully loaded definition is
// certified, a Certified-first order would make the Guard tier permanently
// unreachable. The current order is deliberate, not a bug.
func classify(s *session.Session, d session.Def) Tier {
	if d.Body == nil {
		// Only assumed defs reach here (non-assumed bodiless filtered by Build).
		if d.Postulate {
			return Postulate
		}
		return Assume
	}
	// Guard checked before Certified: see comment above.
	if d.UsesGuard {
		return Guard
	}
	if s.Certified(d.Name) {
		return Proven
	}
	return Unproven
}
