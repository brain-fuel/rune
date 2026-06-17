// Package codegen is the CODEGEN STRATUM: erased intermediate representation to
// target source, kept behind the Backend interface so a new deployment target is
// one plugin, not a fork. Dependent types are a build-time discipline; what
// deploys is the erasure — the shadow.
//
// THE SHADOW RULE (CLAUDE.md): any optimization IR is built on erased, throwaway
// codegen output, NEVER on the immutable core/store. The core is the source of
// truth and identity; codegen mutates only its own shadow.
//
// v1 shipped one Backend (JavaScript); the interface always existed so later
// versions add targets without a rewrite. The B-track now ships EIGHT — six source
// emitters (JavaScript, Python, Go, Rust, BEAM, JVM 25) plus two NATIVE backends
// (C and LLVM-IR over the closure-converted IR + a shared C runtime) — over the one
// shared erased IR; cross-backend conformance (harness/backend_conformance_test.go,
// ffi_conformance_test.go) is the portability guarantee.
//
// B1/B2 (R-IR portable erased IR + the Backend refactor) are DONE: every backend
// renders the SAME erased IR (codegen/ir.go), and a datatype's eliminator is lowered
// to that IR ONCE (codegen/lower.go LowerElim — ICase/IField), so no backend carries
// eliminator-specific code. The sole hand-rolled eliminator is the builtin Nat's
// (per-backend natElim/natDispatch): a justified ABI exception, because Nat uses the
// compressed-literal NatLit representation (C7/R-NUM) with a bigint-accelerated fast
// path that a generic ICase/IField fold over a tagged record cannot express without
// regressing the 4096-cap fix. Everything else flows through LowerElim.
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

// Default returns the v1 backend (JavaScript), the `rune emit`/`rune run` default.
func Default() Backend { return JS{} }

// All returns every backend the codegen stratum ships, in registration order.
// C is the first NATIVE backend (telos-2 / M4): it emits portable C over the
// closure-converted IR + an embedded closure runtime, compiled by the system cc.
func All() []Backend { return []Backend{JS{}, Py{}, Go{}, Rust{}, Beam{}, JVM{}, C{}, LL{}} }

// targetAliases maps friendly names to the canonical Target() of a backend, so
// `--target python` and `--target rust` work alongside `py`/`rs`.
var targetAliases = map[string]string{
	"javascript": "js", "node": "js",
	"python": "py", "python3": "py",
	"golang": "go",
	"rust":   "rs", "rustc": "rs",
	"beam": "erl", "erlang": "erl", "escript": "erl",
	"java": "jvm", "java25": "jvm", "javac": "jvm",
	"native": "c", "cc": "c", "gcc": "c",
	"llvm": "ll", "clang": "ll", "ir": "ll",
}

// ByTarget returns the backend selected by a target name (canonical Target() or
// a friendly alias), and whether one matched.
func ByTarget(name string) (Backend, bool) {
	if canon, ok := targetAliases[name]; ok {
		name = canon
	}
	for _, b := range All() {
		if b.Target() == name {
			return b, true
		}
	}
	return nil, false
}

// Targets lists the canonical target names of every shipped backend.
func Targets() []string {
	out := make([]string, 0, len(All()))
	for _, b := range All() {
		out = append(out, b.Target())
	}
	return out
}
