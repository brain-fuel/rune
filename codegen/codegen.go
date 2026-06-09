// Package codegen is the CODEGEN STRATUM: erased intermediate representation to
// target source, kept behind the Backend interface so a new deployment target is one
// plugin, not a fork. Dependent types are a build-time discipline; what deploys is
// the erasure — the shadow.
//
// THE SHADOW RULE (CLAUDE.md): any optimization IR is built on erased, throwaway
// codegen output, NEVER on the immutable core/store. The core is the source of truth
// and identity; codegen mutates only its own shadow.
//
// v1 ships one Backend; the interface exists so v2 extends and v3 swaps targets
// without a rewrite. Phase 0 defines the interface and stubs only — erasure and any
// backend are Phase 7.
package codegen

// ErasedIR is the target-independent, erased intermediate representation: core with
// the 0-quantity fragment removed (types, proofs, irrelevant arguments gone), ready
// for a Backend to emit. Phase 7 fills in its shape; Phase 0 fixes only the name and
// the boundary.
type ErasedIR struct {
	// Phase 7: the erased program. Intentionally empty in Phase 0 — adding fields
	// before there is a consumer would violate the demonstrated-need cap.
}

// TargetSource is emitted source for some concrete backend target (e.g. Chez Scheme
// or JavaScript in v1).
type TargetSource string

// Backend is the codegen stratum interface: it turns erased IR into target source.
// One implementation ships per supported target.
type Backend interface {
	// Target names the backend (e.g. "scheme", "js").
	Target() string
	// Emit lowers erased IR to target source.
	Emit(ir ErasedIR) (TargetSource, error)
}

// Erase produces the erased IR shadow of a checked program. THE SHADOW RULE applies:
// the result is throwaway and downstream-mutable; the core it came from is not.
// Phase 7 implements erasure (drop the 0-quantity fragment); Phase 0 stubs it.
func Erase() ErasedIR {
	panic("phase 7: erasure")
}
