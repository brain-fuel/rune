package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// floatIOBackends returns the 5 source backends covered by float IO host ops:
// the four CLI backends (js, py, go, erl) plus Rust (compile step).
// JVM is exercised separately by TestIOFloatStdinJVM / TestIOFloatStdinCRLFJVM
// because it requires javac -d <dir> rather than rustc -o <bin>.
func floatIOBackends() []ioBackend {
	return append(append([]ioBackend{}, ioCLIBackends...), ioOSBackends[3]) // js, py, go, erl, rs
}

func TestIOFloatStdinConformance(t *testing.T) {
	const want = "6.28\n999\n999\n999"
	for _, bk := range floatIOBackends() {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch566_float_io.rune", "main", "3.14\n"); got != want {
				t.Errorf("[%s] float stdin run gave %q, want %q", bk.name, got, want)
			}
		})
	}
}

func TestIOFloatStdinCRLF(t *testing.T) {
	const want = "6.28\n999\n999\n999"
	for _, bk := range floatIOBackends() {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch566_float_io.rune", "main", "3.14\r\n"); got != want {
				t.Errorf("[%s] float stdin CRLF run gave %q, want %q", bk.name, got, want)
			}
		})
	}
}

// TestIOFloatStdinJVM and TestIOFloatStdinCRLFJVM are the JVM parity gates for the
// float IO operations (parseFloat/getFloat/printFloat). Separate from the main loop
// because the JVM backend needs a javac -d <dir> compile step. Skips without JDK 25.
func runIOListingJVM(t *testing.T, listing, stdin string) string {
	t.Helper()
	javac25, java25, ok := findJava25()
	if !ok {
		t.Skip("no JDK 25 (asdf temurin-25); the JVM backend targets Java 25")
	}
	s := loadListing(t, listing)
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.JVM{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	f := filepath.Join(dir, "main.java")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	if out, err := exec.Command(javac25, "--release", "25", "-d", dir, f).CombinedOutput(); err != nil {
		t.Fatalf("javac: %v\n%s\n--- emitted ---\n%s", err, out, src)
	}
	cmd := exec.Command(java25, "-cp", dir, "main")
	cmd.Stdin = strings.NewReader(stdin)
	out, err := cmd.Output()
	if err != nil {
		t.Fatalf("java run: %v", err)
	}
	return strings.TrimSpace(string(out))
}

func TestIOFloatStdinJVM(t *testing.T) {
	const want = "6.28\n999\n999\n999"
	if got := runIOListingJVM(t, "ch566_float_io.rune", "3.14\n"); got != want {
		t.Errorf("[jvm] float stdin run gave %q, want %q", got, want)
	}
}

func TestIOFloatStdinCRLFJVM(t *testing.T) {
	const want = "6.28\n999\n999\n999"
	if got := runIOListingJVM(t, "ch566_float_io.rune", "3.14\r\n"); got != want {
		t.Errorf("[jvm] float stdin CRLF run gave %q, want %q", got, want)
	}
}

// runNativeListingStdin is like runNativeListing (bible_conformance_test.go) but
// also pipes stdin. Used for ch566 which reads a float from stdin.
func runNativeListingStdin(t *testing.T, backend, listing, main, stdin string) (string, bool) {
	t.Helper()
	switch backend {
	case "c":
		if _, err := exec.LookPath("cc"); err != nil {
			return "", false
		}
	case "ll":
		if _, err := exec.LookPath("clang"); err != nil {
			return "", false
		}
	default:
		t.Fatalf("runNativeListingStdin: unknown backend %q", backend)
	}

	s := loadListing(t, listing)
	p, err := s.EmitProgram(main)
	if err != nil {
		t.Fatalf("[%s] %s EmitProgram: %v", backend, listing, err)
	}

	dir := t.TempDir()
	bin := filepath.Join(dir, "main.bin")

	switch backend {
	case "c":
		src, err := codegen.C{}.Emit(p)
		if err != nil {
			t.Fatalf("[c] %s Emit: %v", listing, err)
		}
		f := filepath.Join(dir, "main.c")
		if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
			t.Fatal(err)
		}
		if out, err := exec.Command("cc", "-o", bin, f).CombinedOutput(); err != nil {
			t.Fatalf("[c] %s compile: %v\n%s\n--- src ---\n%s", listing, err, out, src)
		}
	case "ll":
		ll, err := codegen.LL{}.Emit(p)
		if err != nil {
			t.Fatalf("[ll] %s Emit: %v", listing, err)
		}
		rt := codegen.LL{}.EmitRuntimeFor(p)
		llf := filepath.Join(dir, "program.ll")
		rtf := filepath.Join(dir, "runtime.c")
		if err := os.WriteFile(llf, []byte(ll), 0o644); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(rtf, []byte(rt), 0o644); err != nil {
			t.Fatal(err)
		}
		if out, err := exec.Command("clang", llf, rtf, "-o", bin).CombinedOutput(); err != nil {
			t.Fatalf("[ll] %s compile: %v\n%s\n--- .ll ---\n%s", listing, err, out, ll)
		}
	}

	cmd := exec.Command(bin)
	if stdin != "" {
		cmd.Stdin = strings.NewReader(stdin)
	}
	out, err := cmd.Output()
	if err != nil {
		t.Fatalf("[%s] %s run: %v", backend, listing, err)
	}
	return strings.TrimSpace(string(out)), true
}

// TestIOFloatFormatConformance is the 8-way (source + JVM + C + LL) divergence
// lock for ch567_float_format.rune. No stdin needed. Expected output is the
// ECMAScript Number::toString shortest-round-trip result for 11 valid inputs,
// plus 999 for each of the 3 reject probes, plus a final 999 from the runtime
// driver printing the return value of main (which is the last probe's Nat result).
func TestIOFloatFormatConformance(t *testing.T) {
	const want = "0\n2\n3.14\n-0.5\n10000000\n100000000000000000000\n1e+21\n0.000001\n1e-7\n1.5e-9\n0.1\n999\n999\n999\n999"
	// Source backends (js, py, go, erl, rs).
	for _, bk := range floatIOBackends() {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch567_float_format.rune", "main", ""); got != want {
				t.Errorf("[%s] format lock gave %q, want %q", bk.name, got, want)
			}
		})
	}
	// JVM backend.
	t.Run("jvm", func(t *testing.T) {
		if got := runIOListingJVM(t, "ch567_float_format.rune", ""); got != want {
			t.Errorf("[jvm] format lock gave %q, want %q", got, want)
		}
	})
	// C native backend.
	t.Run("c", func(t *testing.T) {
		got, ok := runNativeListing(t, "c", "ch567_float_format.rune", "main", "")
		if !ok {
			t.Skip("cc not in PATH")
		}
		if got != want {
			t.Errorf("[c] format lock gave %q, want %q", got, want)
		}
	})
	// LL (LLVM) native backend.
	t.Run("ll", func(t *testing.T) {
		got, ok := runNativeListing(t, "ll", "ch567_float_format.rune", "main", "")
		if !ok {
			t.Skip("clang not in PATH")
		}
		if got != want {
			t.Errorf("[ll] format lock gave %q, want %q", got, want)
		}
	})
}

// TestIOFloatStdinC is the C native backend gate for ch566_float_io.rune (stdin).
func TestIOFloatStdinC(t *testing.T) {
	const want = "6.28\n999\n999\n999"
	got, ok := runNativeListingStdin(t, "c", "ch566_float_io.rune", "main", "3.14\n")
	if !ok {
		t.Skip("cc not in PATH")
	}
	if got != want {
		t.Errorf("[c] float stdin gave %q, want %q", got, want)
	}
}

// TestIOFloatStdinLL is the LL (LLVM) native backend gate for ch566_float_io.rune (stdin).
func TestIOFloatStdinLL(t *testing.T) {
	const want = "6.28\n999\n999\n999"
	got, ok := runNativeListingStdin(t, "ll", "ch566_float_io.rune", "main", "3.14\n")
	if !ok {
		t.Skip("clang not in PATH")
	}
	if got != want {
		t.Errorf("[ll] float stdin gave %q, want %q", got, want)
	}
}
