package harness

import (
	"os/exec"
	"testing"
)

// TestExec is the Phase-1 os/exec gate: procRun runs `echo hi` and captures its
// stdout ("hi\n", shown as "hi\x0a") across all 8 backends (argv-exec on
// Go/Py/Rust/JVM, os:cmd on BEAM, popen on C/LLVM, child_process on JS).
func TestExec(t *testing.T) {
	const want = "\"hi\\x0a\"\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch495_exec.rune", "main", ""); got != want {
				t.Fatalf("[%s] exec gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch495_exec.rune")
	prog, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	t.Run("c", func(t *testing.T) {
		if got := runInDir(t, exec.Command(compileC(t, prog)), t.TempDir()); got != want {
			t.Fatalf("[c] exec gave %q, want %q", got, want)
		}
	})
	t.Run("ll", func(t *testing.T) {
		if got := runInDir(t, exec.Command(compileLL(t, prog)), t.TempDir()); got != want {
			t.Fatalf("[ll] exec gave %q, want %q", got, want)
		}
	})
	t.Run("jvm", func(t *testing.T) {
		javac25, java25, ok := findJava25()
		if !ok {
			t.Skip("no JDK 25 (asdf temurin-25)")
		}
		dir := compileJVM(t, "ch495_exec.rune", javac25)
		if got := runInDir(t, exec.Command(java25, "-cp", dir, "main"), t.TempDir()); got != want {
			t.Fatalf("[jvm] exec gave %q, want %q", got, want)
		}
	})
}

// TestContext is the Phase-1 context gate: the pure cancellation-token model (ch496)
// — ctxErr propagation + ctxValue lookup over a context tree — runs byte-identically
// on all 8 backends (pure wootz, no FFI).
func TestContext(t *testing.T) {
	const want = "0\n1\n42\n42\n0\n0"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch496_context.rune", "main", ""); got != want {
				t.Fatalf("[%s] context gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch496_context.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch496_context.rune", want) })
}
