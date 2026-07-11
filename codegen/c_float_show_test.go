package codegen_test

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/prelude"
	"goforge.dev/rune/v3/internal/session"
)

// c_float_show_test.go -- Float Track B, Task 1. A Float-valued main with NO
// float IO (no printFloat/getFloat/parseFloat) reaches the native C runtime's
// show() K_FLOAT case, which used to render via printf("%g", ...): six
// significant digits by default, so 1234567.0 printed as "1.23457e+06".
// ECMAScript Number::toString renders "1234567". This test pins native C's
// show() to the ECMAScript rendering (via the __fmtf helper, now moved into
// the base runtime so it is always in scope, not just under float IO).

// floatShowProgram builds a checked Program whose `main : Std.Float.Float` is
// `Std.Float.fromNat k` (k as a builtin-nat literal, so it need not be a
// succ-chain), loaded on top of the real shared prelude. Mirrors the
// session.New() -> LoadSource(prelude.Source()) -> LoadSource(corpus) ->
// EmitProgram("main") pipeline used throughout codegen/*_test.go (see
// beam_float_specials_test.go's emitBeamFloat and
// internal/session/float_guarded_test.go's runFloatGuardedMain).
func floatShowProgram(t *testing.T, k int) codegen.Program {
	t.Helper()
	s := session.New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	corpus := fmt.Sprintf("main : Std.Float.Float is Std.Float.fromNat %d end\n", k)
	if _, err := s.LoadSource(corpus); err != nil {
		t.Fatalf("loading corpus: %v", err)
	}
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatalf("EmitProgram(main): %v", err)
	}
	return p
}

// emitAndRunC emits C, writes main.c, compiles with cc, runs, returns stdout.
func emitAndRunC(t *testing.T, p codegen.Program) string {
	t.Helper()
	dir := t.TempDir()
	src, err := (codegen.C{}).Emit(p)
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	cpath := filepath.Join(dir, "main.c")
	if err := os.WriteFile(cpath, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	bin := filepath.Join(dir, "a.out")
	if out, err := exec.Command("cc", "-o", bin, cpath).CombinedOutput(); err != nil {
		t.Fatalf("cc: %v\n%s", err, out)
	}
	out, err := exec.Command(bin).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v\n%s", err, out)
	}
	return string(out)
}

// TestCShowFloatIsEcmascript is the Task 1 acceptance test: a Float main with
// no float IO reaches show()'s K_FLOAT case. %g would render 1234567.0 as
// "1.23457e+06"; ECMAScript Number::toString renders "1234567". This pins
// native C show to ECMAScript.
func TestCShowFloatIsEcmascript(t *testing.T) {
	if _, err := exec.LookPath("cc"); err != nil {
		t.Skip("cc not in PATH")
	}
	prog := floatShowProgram(t, 1234567)
	out := emitAndRunC(t, prog)
	if strings.TrimSpace(out) != "1234567" {
		t.Fatalf("C show of 1234567.0 = %q, want \"1234567\" (got %%g divergence?)", out)
	}
}
