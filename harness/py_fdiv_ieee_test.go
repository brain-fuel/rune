package harness

import (
	"os"
	"os/exec"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/prelude"
	"goforge.dev/rune/v3/internal/session"
)

// pyFdivIeeeCorpusSrc is the focused acceptance corpus for the Python
// backend's fdiv IEEE754 fix (codegen/py.go): Python's native `a / b` raises
// ZeroDivisionError on a zero denominator, unlike every other rune backend
// (js/go/rust/c/ll), which follow IEEE754 (0.0/0.0 -> NaN, x/0.0 -> +-
// Infinity). This corpus is loaded ON TOP OF the real shared prelude
// (prelude.Source(), the same source the REPL/CLI load) and checks THREE
// things via Std.Float's guarded-tier ops, folded into one Bool so a single
// "true"/"false" line is the whole result - never a printed float:
//
//   - divZeroIsNaN: isNaN (fdiv 0.0 0.0) - the 0/0 -> NaN case.
//   - divByZeroIsInf: a finite value fle (fdiv 1.0 0.0) - 1/0 is +Infinity,
//     and any finite value is <= +Infinity.
//   - finiteArithStillWorks: feq (1.0 + 2.0) 3.0 - confirms the fdiv fix did
//     not disturb fadd (untouched by this change).
const pyFdivIeeeCorpusSrc = `
divZeroIsNaN : Bool is Std.Float.isNaN (Std.Float.fdiv (Std.Float.fromNat zero) (Std.Float.fromNat zero)) end

divByZeroIsInf : Bool is
  Std.Float.fle (Std.Float.fromNat (succ zero)) (Std.Float.fdiv (Std.Float.fromNat (succ zero)) (Std.Float.fromNat zero))
end

finiteArithStillWorks : Bool is
  Std.Float.feq (Std.Float.fadd (Std.Float.fromNat (succ zero)) (Std.Float.fromNat (succ (succ zero))))
                (Std.Float.fromNat (succ (succ (succ zero))))
end

main : Bool is
  and divZeroIsNaN (and divByZeroIsInf finiteArithStillWorks)
end
`

// TestPyFdivIeeeParity emits+runs pyFdivIeeeCorpusSrc's main on the Python
// backend only (codegen.Py{}.Emit, run under python3), mirroring the
// emit-write-run shape runComparisonBackend/runNumericParityBackend use for
// the full bibleBackends() sweep (comparison_conformance_test.go,
// numeric_parity_test.go), but scoped to the one backend this fix touches.
// Skips (does not fail) if python3 is not on PATH, per the same convention.
func TestPyFdivIeeeParity(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not in PATH")
	}

	s := session.New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	if _, err := s.LoadSource(pyFdivIeeeCorpusSrc); err != nil {
		t.Fatalf("loading fdiv corpus: %v", err)
	}
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatalf("EmitProgram(main): %v", err)
	}
	src, err := codegen.Py{}.Emit(p)
	if err != nil {
		t.Fatalf("emit: %v", err)
	}

	dir := t.TempDir()
	f := dir + "/main.py"
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}

	cmd := exec.Command("python3", f)
	cmd.Dir = dir
	out, err := cmd.Output()
	if err != nil {
		stderr := ""
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		t.Fatalf("python3 run failed: %v\n%s\n--- emitted ---\n%s", err, stderr, src)
	}

	got := strings.TrimSpace(string(out))
	if got != "true" {
		t.Errorf("main = %q, want %q\n--- emitted ---\n%s", got, "true", src)
	}
}
