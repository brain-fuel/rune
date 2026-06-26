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
	PropHash   core.Hash  // identity of the CLAIM = content hash of the type
	ProofHash  core.Hash  // identity of the WITNESS = content hash of the body (zero if bodiless)
	Why        string     // postulate/assume reason (Task 4)
	File       string     // source file (Task 6)
	Line       int        // 1-based source line (Task 6)
	Provenance Provenance
}

// Build classifies every definition in the session.
func Build(s *session.Session) []Entry {
	defs := s.Defs()
	out := make([]Entry, 0, len(defs))
	for _, d := range defs {
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

// classify picks the tier from the facts available so far. Later tasks extend
// this (guard in Task 5).
func classify(s *session.Session, d session.Def) Tier {
	if d.Body == nil {
		// bodiless: a postulate (an asserted debt) if written as one, else a
		// trusted foreign/host binding.
		if d.Postulate {
			return Postulate
		}
		return Assume
	}
	if s.Certified(d.Name) {
		return Proven
	}
	return Unproven
}
