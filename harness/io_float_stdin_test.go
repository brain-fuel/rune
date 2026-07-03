package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/prelude"
	"goforge.dev/rune/v3/internal/session"
)

// loadExampleWithPrelude loads examples/<name> into a session that already has
// the shared prelude (Std.Float, Option, numeric tower, etc.) installed.
func loadExampleWithPrelude(t *testing.T, name string) *session.Session {
	t.Helper()
	src, err := os.ReadFile(filepath.Join("..", "examples", name))
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude for %s: %v", name, err)
	}
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("examples/%s did not load: %v", name, err)
	}
	return s
}

// runExampleWithPrelude emits mainName from examples/<example> (with prelude) on
// bk, feeds stdin, and returns trimmed stdout. Mirrors runIOListing but sources
// from examples/ and pre-loads the prelude.
func runExampleWithPrelude(t *testing.T, bk ioBackend, example, mainName, stdin string) string {
	t.Helper()
	s := loadExampleWithPrelude(t, example)
	p, err := s.EmitProgram(mainName)
	if err != nil {
		t.Fatal(err)
	}
	src, err := bk.emit(p)
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	f := filepath.Join(dir, "main."+bk.ext)
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	runFile := f
	if bk.compile != nil {
		bin := filepath.Join(dir, "main.bin")
		if out, err := bk.compile(f, bin).CombinedOutput(); err != nil {
			t.Fatalf("[%s] compile failed: %v\n%s\n--- src ---\n%s", bk.name, err, out, src)
		}
		runFile = bin
	}
	cmd := bk.run(runFile)
	if stdin != "" {
		cmd.Stdin = strings.NewReader(stdin)
	}
	out, err := cmd.Output()
	if err != nil {
		stderr := ""
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		t.Fatalf("[%s] run failed: %v\n%s", bk.name, err, stderr)
	}
	return strings.TrimSpace(string(out))
}

// TestIOFloatDoubleDemo is the 9-backend acceptance gate for examples/double.rune:
// reads a float from stdin, doubles it, prints and returns it.
// Feed "3.14" → expect "6.28\n6.28" (printFloat side-effect + the shown IO result).
// Source backends: js, py, go, erl (CLI); rs (compile step); jvm (needs JDK 25);
// WASM under wasmtime; native C (cc) and LL (clang) compiled binaries with stdin
// piped, mirroring TestIOFloatStdinC/TestIOFloatStdinLL. Clean skips when a
// toolchain is missing. All nine backends, byte-identical.
func TestIOFloatDoubleDemo(t *testing.T) {
	const (
		input = "3.14\n"
		want  = "6.28\n6.28"
	)
	// Source backends: js, py, go, erl, rs.
	for _, bk := range floatIOBackends() {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runExampleWithPrelude(t, bk, "double.rune", "main", input); got != want {
				t.Errorf("[%s] double demo gave %q, want %q", bk.name, got, want)
			}
		})
	}
	// JVM backend (needs javac --release 25).
	t.Run("jvm", func(t *testing.T) {
		javac25, java25, ok := findJava25()
		if !ok {
			t.Skip("no JDK 25 (asdf temurin-25)")
		}
		s := loadExampleWithPrelude(t, "double.rune")
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
			t.Fatalf("javac: %v\n%s", err, out)
		}
		cmd := exec.Command(java25, "-cp", dir, "main")
		cmd.Stdin = strings.NewReader(input)
		out, err := cmd.Output()
		if err != nil {
			t.Fatalf("java run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[jvm] double demo gave %q, want %q", got, want)
		}
	})
	// WASM (ninth) backend.
	t.Run("wasm", func(t *testing.T) {
		wt := wasmtimePathHarness()
		if wt == "" {
			t.Skip("wasmtime not available")
		}
		s := loadExampleWithPrelude(t, "double.rune")
		p, err := s.EmitProgram("main")
		if err != nil {
			t.Fatal(err)
		}
		src, err := codegen.Wasm{}.Emit(p)
		if err != nil {
			t.Fatal(err)
		}
		dir := t.TempDir()
		f := filepath.Join(dir, "module.wat")
		if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
			t.Fatal(err)
		}
		cmd := exec.Command(wt, "run", f)
		cmd.Stdin = strings.NewReader(input)
		out, err := cmd.Output()
		if err != nil {
			t.Fatalf("wasmtime run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[wasm] double demo gave %q, want %q", got, want)
		}
	})
	// Native C and LL (LLVM) backends: compiled binaries with stdin piped.
	for _, backend := range []string{"c", "ll"} {
		backend := backend
		t.Run(backend, func(t *testing.T) {
			got, ok := runNativeExampleStdin(t, backend, "double.rune", "main", input)
			if !ok {
				t.Skipf("[%s] native toolchain not in PATH", backend)
			}
			if got != want {
				t.Errorf("[%s] double demo gave %q, want %q", backend, got, want)
			}
		})
	}
}

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
	s := loadListing(t, listing)
	return runNativeSessionStdin(t, backend, listing, s, main, stdin)
}

// runNativeExampleStdin is runNativeListingStdin over examples/<example> with the
// shared prelude pre-loaded (Std.Float etc.). Used by the double-demo native gate.
func runNativeExampleStdin(t *testing.T, backend, example, main, stdin string) (string, bool) {
	t.Helper()
	s := loadExampleWithPrelude(t, example)
	return runNativeSessionStdin(t, backend, "examples/"+example, s, main, stdin)
}

// runNativeSessionStdin compiles main from an already-loaded session on the given
// native backend (c via cc, ll via clang), runs the binary with stdin piped, and
// returns trimmed stdout. ok=false means the toolchain is missing (caller skips).
func runNativeSessionStdin(t *testing.T, backend, label string, s *session.Session, main, stdin string) (string, bool) {
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
		t.Fatalf("runNativeSessionStdin: unknown backend %q", backend)
	}

	p, err := s.EmitProgram(main)
	if err != nil {
		t.Fatalf("[%s] %s EmitProgram: %v", backend, label, err)
	}

	dir := t.TempDir()
	bin := filepath.Join(dir, "main.bin")

	switch backend {
	case "c":
		src, err := codegen.C{}.Emit(p)
		if err != nil {
			t.Fatalf("[c] %s Emit: %v", label, err)
		}
		f := filepath.Join(dir, "main.c")
		if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
			t.Fatal(err)
		}
		if out, err := exec.Command("cc", "-o", bin, f).CombinedOutput(); err != nil {
			t.Fatalf("[c] %s compile: %v\n%s\n--- src ---\n%s", label, err, out, src)
		}
	case "ll":
		ll, err := codegen.LL{}.Emit(p)
		if err != nil {
			t.Fatalf("[ll] %s Emit: %v", label, err)
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
			t.Fatalf("[ll] %s compile: %v\n%s\n--- .ll ---\n%s", label, err, out, ll)
		}
	}

	cmd := exec.Command(bin)
	if stdin != "" {
		cmd.Stdin = strings.NewReader(stdin)
	}
	out, err := cmd.Output()
	if err != nil {
		t.Fatalf("[%s] %s run: %v", backend, label, err)
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
	// WASM (ninth) backend, run under wasmtime. No stdin, no files.
	t.Run("wasm", func(t *testing.T) {
		got, ok := runWasmListing(t, "ch567_float_format.rune", "main", "")
		if !ok {
			t.Skip("wasmtime not available")
		}
		if got != want {
			t.Errorf("[wasm] format lock gave %q, want %q", got, want)
		}
	})
}

// runWasmListingFloatStdin emits (listing, main) to WAT (codegen.Wasm), writes it, and
// runs it under wasmtime with stdin piped. Mirrors runWasmListing (bible_conformance_test.go)
// but pipes stdin for ch566 (getFloat reads fd 0). No --dir preopen: the float IO listings
// touch no files. Returns trimmed stdout + ok=true; ok=false when wasmtime is absent.
func runWasmListingFloatStdin(t *testing.T, listing, main, stdin string) (string, bool) {
	t.Helper()
	wt := wasmtimePathHarness()
	if wt == "" {
		return "", false
	}
	s := loadListing(t, listing)
	p, err := s.EmitProgram(main)
	if err != nil {
		t.Fatalf("%s emit-program: %v", listing, err)
	}
	src, err := codegen.Wasm{}.Emit(p)
	if err != nil {
		t.Fatalf("%s wasm emit: %v", listing, err)
	}
	dir := t.TempDir()
	f := filepath.Join(dir, "module.wat")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command(wt, "run", f)
	cmd.Stdin = strings.NewReader(stdin)
	out, err := cmd.Output()
	if err != nil {
		t.Fatalf("%s wasmtime run: %v", listing, err)
	}
	return strings.TrimSpace(string(out)), true
}

// TestIOFloatStdinWasm is the WASM (ninth) backend gate for ch566_float_io.rune (stdin):
// feed "3.14\n", expect "6.28\n999\n999\n999". Skips cleanly when wasmtime is absent.
func TestIOFloatStdinWasm(t *testing.T) {
	const want = "6.28\n999\n999\n999"
	got, ok := runWasmListingFloatStdin(t, "ch566_float_io.rune", "main", "3.14\n")
	if !ok {
		t.Skip("wasmtime not available")
	}
	if got != want {
		t.Errorf("[wasm] float stdin gave %q, want %q", got, want)
	}
}

// TestIOFloatStdinCRLFWasm is the WASM CRLF variant: getFloat must strip the trailing
// '\r' from "3.14\r\n" before validating/parsing.
func TestIOFloatStdinCRLFWasm(t *testing.T) {
	const want = "6.28\n999\n999\n999"
	got, ok := runWasmListingFloatStdin(t, "ch566_float_io.rune", "main", "3.14\r\n")
	if !ok {
		t.Skip("wasmtime not available")
	}
	if got != want {
		t.Errorf("[wasm] float stdin CRLF gave %q, want %q", got, want)
	}
}

// TestIOFloatStdinGarbage is the cross-backend garbage-line contract lock:
// feeding "3.14 junk\n" (a line that has valid float prefix followed by garbage)
// must produce "0\n999\n999\n999" on every backend (getFloat -> 0.0; doubled 0.0
// prints as "0"; the two parseFloat probes each print 999; driver shows 999).
func TestIOFloatStdinGarbage(t *testing.T) {
	const want = "0\n999\n999\n999"
	for _, bk := range floatIOBackends() {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch566_float_io.rune", "main", "3.14 junk\n"); got != want {
				t.Errorf("[%s] garbage stdin gave %q, want %q", bk.name, got, want)
			}
		})
	}
}

func TestIOFloatStdinGarbageJVM(t *testing.T) {
	const want = "0\n999\n999\n999"
	if got := runIOListingJVM(t, "ch566_float_io.rune", "3.14 junk\n"); got != want {
		t.Errorf("[jvm] garbage stdin gave %q, want %q", got, want)
	}
}

func TestIOFloatStdinGarbageC(t *testing.T) {
	const want = "0\n999\n999\n999"
	got, ok := runNativeListingStdin(t, "c", "ch566_float_io.rune", "main", "3.14 junk\n")
	if !ok {
		t.Skip("cc not in PATH")
	}
	if got != want {
		t.Errorf("[c] garbage stdin gave %q, want %q", got, want)
	}
}

func TestIOFloatStdinGarbageLL(t *testing.T) {
	const want = "0\n999\n999\n999"
	got, ok := runNativeListingStdin(t, "ll", "ch566_float_io.rune", "main", "3.14 junk\n")
	if !ok {
		t.Skip("clang not in PATH")
	}
	if got != want {
		t.Errorf("[ll] garbage stdin gave %q, want %q", got, want)
	}
}

func TestIOFloatStdinGarbageWasm(t *testing.T) {
	const want = "0\n999\n999\n999"
	got, ok := runWasmListingFloatStdin(t, "ch566_float_io.rune", "main", "3.14 junk\n")
	if !ok {
		t.Skip("wasmtime not available")
	}
	if got != want {
		t.Errorf("[wasm] garbage stdin gave %q, want %q", got, want)
	}
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
