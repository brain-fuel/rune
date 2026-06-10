// Package codegen is the CODEGEN STRATUM: erased intermediate representation to
// target source, kept behind the Backend interface so a new deployment target is
// one plugin, not a fork. Dependent types are a build-time discipline; what
// deploys is the erasure — the shadow.
//
// THE SHADOW RULE (CLAUDE.md): any optimization IR is built on erased, throwaway
// codegen output, NEVER on the immutable core/store. The core is the source of
// truth and identity; codegen mutates only its own shadow.
//
// v1 ships one Backend (JavaScript); the interface exists so a later version
// adds targets without a rewrite.
package codegen

// TargetSource is emitted source for some concrete backend target.
type TargetSource string

// Backend is the codegen stratum interface: it turns an erased Program into
// target source. One implementation ships per supported target.
type Backend interface {
	// Target names the backend (e.g. "js").
	Target() string
	// Emit lowers an erased program to target source.
	Emit(p Program) (TargetSource, error)
}

// Default returns the v1 backend.
func Default() Backend { return JS{} }
