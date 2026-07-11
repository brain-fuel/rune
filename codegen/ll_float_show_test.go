package codegen_test

import (
	"os"
	"os/exec"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// ll_float_show_test.go -- Float Track B, Task 2. The exact LL mirror of
// c_float_show_test.go's Task 1: a Float-valued main with NO float IO (no
// printFloat/getFloat/parseFloat) reaches the LLVM backend's linked runtime.c
// show() K_FLOAT case, which used to render via printf("%g", ...): six
// significant digits by default, so 1234567.0 printed as "1.23457e+06".
// ECMAScript Number::toString renders "1234567". This test pins native LL's
// show() to the ECMAScript rendering (via __fmtf, now moved into the base LL
// runtime -- llRuntimeC -- so it is always in scope, not just under float IO).
//
// floatShowProgram (the codegen.Program builder) is reused verbatim from
// c_float_show_test.go -- same package (codegen_test), Task 1 already wrote it.

// emitAndRunLL emits the LLVM backend for `p`, links the .ll with the C
// runtime via EmitRuntimeFor(p) (needed because `main` calls the Float
// foreign `fromNat`, whose host body emitFloatPrimsLL bakes in ONLY under
// EmitRuntimeFor -- the base EmitRuntime() omits float/BLAS bodies entirely
// to avoid -lopenblas), compiles+links with clang, runs the binary, and
// returns trimmed stdout. Modeled on ll_test.go's runLL and
// ll_arc_test.go's buildAndRunLLWithReport (both reused the clang lookup +
// skip + temp-dir + link shape); this helper differs from runLL only in
// linking EmitRuntimeFor(p) instead of the base EmitRuntime(), since fromNat
// (a non-BLAS float host body) is not present in the base runtime.
func emitAndRunLL(t *testing.T, p codegen.Program) string {
	t.Helper()
	if _, err := exec.LookPath("clang"); err != nil {
		t.Skip("clang not in PATH")
	}
	llOut, err := codegen.LL{}.Emit(p)
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	llsrc := string(llOut)
	dir := t.TempDir()
	llf := dir + "/program.ll"
	rtf := dir + "/runtime.c"
	bin := dir + "/main"
	if err := os.WriteFile(llf, []byte(llsrc), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(rtf, []byte(codegen.LL{}.EmitRuntimeFor(p)), 0o644); err != nil {
		t.Fatal(err)
	}
	if out, err := exec.Command("clang", llf, rtf, "-o", bin).CombinedOutput(); err != nil {
		t.Fatalf("clang: %v\n%s\n--- emitted .ll ---\n%s", err, out, llsrc)
	}
	out, err := exec.Command(bin).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v\n%s", err, out)
	}
	return strings.TrimSpace(string(out))
}

// TestLLShowFloatIsEcmascript is the Task 2 acceptance test: a Float main with
// no float IO reaches show()'s K_FLOAT case on the LLVM backend. %g would
// render 1234567.0 as "1.23457e+06"; ECMAScript Number::toString renders
// "1234567". This pins native LL show to the ECMAScript rendering.
func TestLLShowFloatIsEcmascript(t *testing.T) {
	prog := floatShowProgram(t, 1234567)
	out := emitAndRunLL(t, prog)
	if out != "1234567" {
		t.Fatalf("LL show of 1234567.0 = %q, want \"1234567\" (got %%g divergence?)", out)
	}
}
