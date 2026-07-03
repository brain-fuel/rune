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
