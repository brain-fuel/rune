package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// FFI runtime (R-FFI / D-track): a `foreign` axiom RUNS when its host impl is
// linked. The compiler emits a reference to a host global; here the harness
// supplies the implementation (as a real backend's FFI would) and the program
// executes — typed contract (ch40) to running interop, end to end.
func TestForeignRunsWhenLinked(t *testing.T) {
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not in PATH")
	}
	s := loadListing(t, "ch44_ffi_run.rune")
	p, err := s.EmitProgram("answer")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.Default().Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	// Link the host implementation of the foreign axiom (identity). A foreign
	// axiom erases to the uniform `hostId()` accessor (IForeign), so the host
	// binds a zero-arg accessor returning the value.
	linked := "function hostId(){ return a => a; }\n" + string(src)
	f := filepath.Join(t.TempDir(), "m.js")
	if err := os.WriteFile(f, []byte(linked), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command("node", f).CombinedOutput()
	if err != nil {
		t.Fatalf("node failed: %v\n%s", err, out)
	}
	if got := strings.TrimSpace(string(out)); got != "succ (succ zero)" {
		t.Fatalf("foreign-linked run gave %q, want %q", got, "succ (succ zero)")
	}
}
