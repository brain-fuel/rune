package codegen

// perceus.go -- the OPT-IN Perceus ownership-insertion pass over the closure-
// converted CIr. It inserts CDup/CDrop so heap values are reference-counted by
// the Plan 6a ARC runtime (rt_retain/rt_release). Only the WASM backend runs it
// (Wasm.Emit); the C/LLVM and source backends consume the un-annotated CIr.
//
// Soundness rests on the erased IR being immutable, total, and acyclic (R-ARC):
// reference counting with no cycle collector is sound and complete. v1 inserts
// dup/drop only; the in-place reuse optimization is a later plan. See
// ref_docs/wootz/R-PERCEUS.md for the ownership rules and the receiver map.
//
// In this task Perceus is the IDENTITY -- the nodes, the emitter rendering, and
// the steady-state gate land first so later tasks build the algorithm against a
// working gate.
func Perceus(p ClosureProgram) ClosureProgram {
	return p
}
