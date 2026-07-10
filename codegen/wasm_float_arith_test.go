package codegen_test

import (
	"testing"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/prelude"
	"goforge.dev/rune/v3/internal/session"
)

// wasm_float_arith_test.go -- Float Track A, WASM backend: fadd/fsub/fdiv/fleqN now have
// a WAT body (wasm_float.go's emitFaddWasm/emitFsubWasm/emitFdivWasm/emitFleqNWasm) and
// are whitelisted in wasmSupportedForeign (wasm.go), closing the last gap in 9-backend
// Float parity -- previously any program using these four foreign ops failed at emit time
// with "foreign ... is not supported", even though fmul already worked.

// wasmFloatArithCorpus mirrors jsFloatGuardedCorpus's shape (Std.Float module-qualified
// names, loaded on top of the real shared prelude) but targets the arithmetic ops this
// task adds, not the ones already shipped. Every sub-check is verified BY COMPARISON
// (Std.Float.feq / Std.Float.fle / Std.Float.isNaN) and folded into one Bool -- no float
// is ever printed, so the observable output stays the discrete "true"/"false" every other
// backend produces:
//   - 1.0 + 2.0 == 3.0 (fadd, checked via feq)
//   - 3.0 - 1.0 == 2.0 (fsub, checked via feq; the non-commutative argument order matters)
//   - 1.0 <= 2.0 (fle, which derives from fleqN)
//   - isNaN (0.0 / 0.0) (fdiv's native-IEEE754 NaN result, with no special-casing)
const wasmFloatArithCorpus = `
one   : Std.Float.Float is Std.Float.fromNat (succ zero) end
two   : Std.Float.Float is Std.Float.fromNat (succ (succ zero)) end
three : Std.Float.Float is Std.Float.fromNat (succ (succ (succ zero))) end
nanV  : Std.Float.Float is Std.Float.fdiv (Std.Float.fromNat zero) (Std.Float.fromNat zero) end

addOk : Bool is Std.Float.feq (Std.Float.fadd one two) three end
subOk : Bool is Std.Float.feq (Std.Float.fsub three one) two end
leOk  : Bool is Std.Float.fle one two end
nanOk : Bool is Std.Float.isNaN nanV end

main : Bool is and addOk (and subOk (and leOk nanOk)) end
`

// TestWasmFloatArithComputes is the WASM-backend acceptance gate for Float Track A: the
// four newly-baked ops (fadd/fsub/fdiv/fleqN) emit successfully (no "not supported"
// error) and, when run under wasmtime, agree with every other backend's IEEE-754
// semantics -- 1+2==3, 3-1==2, 1<=2, and 0.0/0.0 is NaN. Skips cleanly (not a false pass)
// if wasmtime is not on PATH; the emit step itself is asserted unconditionally, so a
// missing wasmtime still catches a regression in the WAT body.
func TestWasmFloatArithComputes(t *testing.T) {
	s := session.New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	if _, err := s.LoadSource(wasmFloatArithCorpus); err != nil {
		t.Fatalf("loading wasmFloatArithCorpus: %v", err)
	}
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatalf("EmitProgram(main): %v", err)
	}
	src, err := codegen.Wasm{}.Emit(p)
	if err != nil {
		t.Fatalf("wasm emit: %v (fadd/fsub/fdiv/fleqN must be supported)", err)
	}
	if got := runWasm(t, string(src)); got != "true" {
		t.Fatalf("wasmFloatArithCorpus main = %q, want %q", got, "true")
	}
}
