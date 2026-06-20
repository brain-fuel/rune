package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

type ioBackend struct {
	name    string
	bin     string
	ext     string
	emit    func(codegen.Program) (codegen.TargetSource, error)
	run     func(file string) *exec.Cmd
	compile func(src, out string) *exec.Cmd
}

// ioOSBackends are the five source backends D6's baked-in IO host bodies cover
// (matching TestIOEffectConformance). No host injection — the ops ship WITH the
// compiler, so the emitted source runs as-is.
var ioOSBackends = []ioBackend{
	{"js", "node", "js", codegen.JS{}.Emit,
		func(f string) *exec.Cmd { return exec.Command("node", f) }, nil},
	{"py", "python3", "py", codegen.Py{}.Emit,
		func(f string) *exec.Cmd { return exec.Command("python3", f) }, nil},
	{"go", "go", "go", codegen.Go{}.Emit,
		func(f string) *exec.Cmd { return exec.Command("go", "run", f) }, nil},
	{"rs", "rustc", "rs", codegen.Rust{}.Emit,
		func(bin string) *exec.Cmd { return exec.Command(bin) },
		func(src, out string) *exec.Cmd { return exec.Command("rustc", "--edition", "2021", "-o", out, src) }},
	{"erl", "escript", "erl", codegen.Beam{}.Emit,
		func(f string) *exec.Cmd { return exec.Command("escript", f) }, nil},
}

// runIOListing emits `mainName` from `listing` on `bk`, compiling if needed, feeds
// `stdin` (empty for none), and returns trimmed stdout.
func runIOListing(t *testing.T, bk ioBackend, listing, mainName, stdin string) string {
	t.Helper()
	s := loadListing(t, listing)
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
	// stdout only — the program's observable output (escript warns on stderr).
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

// D6 / R-EFFECT — the standard OS/IO primitive vocabulary RUNS, unaided, across
// backends. Unlike ch59 (whose host op is injected by the test harness), the D6
// ops `printNat`/`timeNanos` are baked into each backend's runtime and ship WITH
// the compiler (like D5's beamOTPRuntime) — so this test does NOT inject any host
// body. ch210 reads the OS clock (timeNanos, result discarded for determinism),
// then prints 1 and 2 in sequence; the observable stdout is "1\n2\n2" (the two
// prints, then the shown IO result, the returned 2) on every backend.
func TestIOOSConformance(t *testing.T) {
	const want = "1\n2\n2"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch210_io_os.rune", "main", ""); got != want {
				t.Fatalf("[%s] D6 IO run gave %q, want %q", bk.name, got, want)
			}
		})
	}
}

// D6 stdin INPUT (ch211): getNat reads a decimal from stdin, printNat echoes it. We
// feed "7" and observe "7" printed then the shown IO result (the returned 7) on
// every backend — the input half of the basic in+out+time vocabulary, baked in.
func TestIOStdinConformance(t *testing.T) {
	const want = "7\n7"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch211_io_stdin.rune", "main", "7\n"); got != want {
				t.Fatalf("[%s] D6 stdin run gave %q, want %q", bk.name, got, want)
			}
		})
	}
}

// ioCLIBackends are the source backends whose nat bignum has native division
// (so the div/mod accel emits real div/mod). Rust's hand-rolled base-1e9 bignum
// has no division yet (a parked tail), so the compiled CLI parser excludes it.
var ioCLIBackends = []ioBackend{
	{"js", "node", "js", codegen.JS{}.Emit,
		func(f string) *exec.Cmd { return exec.Command("node", f) }, nil},
	{"py", "python3", "py", codegen.Py{}.Emit,
		func(f string) *exec.Cmd { return exec.Command("python3", f) }, nil},
	{"go", "go", "go", codegen.Go{}.Emit,
		func(f string) *exec.Cmd { return exec.Command("go", "run", f) }, nil},
	{"erl", "escript", "erl", codegen.Beam{}.Emit,
		func(f string) *exec.Cmd { return exec.Command("escript", f) }, nil},
}

// D6+B4 compiled CLI: a deployed program reads a line from stdin as a PACKED-bytes
// String (readLineCode), parses it TOTALLY into a Whole over the kernel-accelerated
// // % (codegen emits native div/mod, so uncons is O(1) at runtime), and prints it
// — or a 999 sentinel on a malformed token (never a crash). "42" → 42; "4x" → 999.
func TestIOParseCLIConformance(t *testing.T) {
	cases := []struct{ in, want string }{
		{"42\n", "42\n42"},
		{"4x\n", "999\n999"},
		{"1000000\n", "1000000\n1000000"},
	}
	for _, bk := range ioCLIBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			for _, c := range cases {
				if got := runIOListing(t, bk, "ch213_parse_cli.rune", "main", c.in); got != c.want {
					t.Errorf("[%s] input %q gave %q, want %q", bk.name, c.in, got, c.want)
				}
			}
		})
	}
}

// TestIOFileEnvConformance is the D6 net/fs gate: the file + environment vocabulary
// (writeFileCode/readFileCode/getEnvCode/printStrCode) runs on a real OS, marshalling
// packed `String` paths/contents across the host boundary in BOTH directions. ch215
// writes "hello, wootz" to a relative file, reads it back and prints it, then prints
// the env var RUNE_D6 — so with the program's cwd set to a temp dir and RUNE_D6=ok the
// observable output is byte-identical across js/py/go/erl. (Rust is excluded for the
// same reason as ch213: it has no packed-String host body yet — parked.)
func TestIOFileEnvConformance(t *testing.T) {
	for _, bk := range ioCLIBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			dir := t.TempDir()
			s := loadListing(t, "ch215_io_fs_env.rune")
			p, err := s.EmitProgram("main")
			if err != nil {
				t.Fatal(err)
			}
			src, err := bk.emit(p)
			if err != nil {
				t.Fatal(err)
			}
			f := filepath.Join(dir, "main."+bk.ext)
			if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
				t.Fatal(err)
			}
			cmd := bk.run(f)
			cmd.Dir = dir // relative path "d6.tmp" resolves here
			cmd.Env = append(os.Environ(), "RUNE_D6=ok")
			out, err := cmd.Output()
			if err != nil {
				stderr := ""
				if ee, ok := err.(*exec.ExitError); ok {
					stderr = string(ee.Stderr)
				}
				t.Fatalf("[%s] run failed: %v\n%s\n--- src ---\n%s", bk.name, err, stderr, src)
			}
			if want := "hello, wootz\nok\nunit"; strings.TrimSpace(string(out)) != want {
				t.Errorf("[%s] D6 fs+env run gave %q, want %q", bk.name, strings.TrimSpace(string(out)), want)
			}
		})
	}
}

// TestIOArgvExitConformance is the D6 argv + process gate: a program reads its
// command-line arguments (argCountCode/argAtCode) and terminates with an explicit
// status (exitWith). ch216 prints argc then argv[0]/argv[1], then exits with status
// = argc. Run with `alpha beta` the stdout is "2\nalpha\nbeta" and the exit status is
// 2 on js/py/go/erl. Go is BUILT to a binary (not `go run`, which masks the child's
// exit code); the others run their interpreter directly so the status passes through.
func TestIOArgvExitConformance(t *testing.T) {
	for _, bk := range ioCLIBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			dir := t.TempDir()
			s := loadListing(t, "ch216_io_argv_exit.rune")
			p, err := s.EmitProgram("main")
			if err != nil {
				t.Fatal(err)
			}
			src, err := bk.emit(p)
			if err != nil {
				t.Fatal(err)
			}
			f := filepath.Join(dir, "main."+bk.ext)
			if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
				t.Fatal(err)
			}
			var cmd *exec.Cmd
			if bk.name == "go" {
				// `go run` reports a child non-zero exit as its own status 1, masking
				// the real code — build a binary and run it directly.
				bin := filepath.Join(dir, "main.bin")
				if out, err := exec.Command("go", "build", "-o", bin, f).CombinedOutput(); err != nil {
					t.Fatalf("[go] build failed: %v\n%s", err, out)
				}
				cmd = exec.Command(bin, "alpha", "beta")
			} else {
				cmd = exec.Command(bk.bin, f, "alpha", "beta")
			}
			out, err := cmd.Output()
			exitCode := 0
			if err != nil {
				if ee, ok := err.(*exec.ExitError); ok {
					exitCode = ee.ExitCode()
				} else {
					t.Fatalf("[%s] run failed: %v", bk.name, err)
				}
			}
			if got, want := strings.TrimSpace(string(out)), "2\nalpha\nbeta"; got != want {
				t.Errorf("[%s] argv stdout = %q, want %q", bk.name, got, want)
			}
			if exitCode != 2 {
				t.Errorf("[%s] exit status = %d, want 2", bk.name, exitCode)
			}
		})
	}
}

// TestIOFloatBlasConformance is the D3 machine-float gate: the IEEE-754 f64 element
// type + the native dot-product BLAS kernel compute byte-identically across
// js/py/go/erl. ch217 prints the dot product 3·1+4·2 = 11, a div/mul/sub chain
// (7/2*4-1) = 13, a contract-guarded dot within budget (11, ok), and one over budget
// (blame → 0). The guard is the R-FFI contract-GUARD tier: the foreign dot2 kernel is
// assumed, its postcondition (≤ 100) checked at the boundary, the kernel blamed on
// violation. Rust is now INCLUDED — its value domain gained a V::Float(f64) variant
// and the float kernels are baked (fromNat/fadd/.../floatToNat/fleqN/dot2), so the
// f64 element type is byte-identical across all five source backends.
func TestIOFloatBlasConformance(t *testing.T) {
	const want = "11\n13\n11\n0\nunit"
	floatBackends := append(append([]ioBackend{}, ioCLIBackends...), ioOSBackends[3]) // + rust
	for _, bk := range floatBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch217_float_blas.rune", "main", ""); got != want {
				t.Errorf("[%s] D3 float/BLAS run gave %q, want %q", bk.name, got, want)
			}
		})
	}
}

// TestD3OpenBLASTolerance is the D3 OpenBLAS-swap gate: on the SOURCE backends
// (js/py/go/erl) `dot2` is the portable reference loop; on the NATIVE backends
// (C/LLVM) it is SWAPPED for cblas_ddot (real OpenBLAS, linked -lopenblas). ch218's
// tolerance contract binds them — the OpenBLAS result must be within `eps` of the
// reference, configurable with `defaultEps` (1e-9). The observable is byte-identical
// everywhere ("11\n11\n0\nunit": the dot, a within-default-tolerance ok, and a
// negative-tolerance blame), so the swap holds parity at the CONTRACT, not the bits.
func TestD3OpenBLASTolerance(t *testing.T) {
	const want = "11\n11\n0\nunit"
	// Source backends: the portable reference path (dot2 = naive loop).
	for _, bk := range ioCLIBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch218_blas_tolerance.rune", "main", ""); got != want {
				t.Errorf("[%s] gave %q, want %q", bk.name, got, want)
			}
		})
	}
	// Native backends: the OpenBLAS swap (dot2 = cblas_ddot), linked -lopenblas.
	s := loadListing(t, "ch218_blas_tolerance.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	skipNoBLAS := func(t *testing.T, out []byte, err error) {
		if err != nil && strings.Contains(string(out)+err.Error(), "cblas") {
			t.Skip("OpenBLAS/cblas.h not available")
		}
	}
	t.Run("c", func(t *testing.T) {
		if _, err := exec.LookPath("cc"); err != nil {
			t.Skip("cc not in PATH")
		}
		dir := t.TempDir()
		src, _ := codegen.C{}.Emit(p)
		f := filepath.Join(dir, "main.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(src), 0o644)
		if out, err := exec.Command("cc", "-o", bin, f, "-lopenblas").CombinedOutput(); err != nil {
			skipNoBLAS(t, out, err)
			t.Fatalf("[c] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[c] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[c] OpenBLAS gave %q, want %q", got, want)
		}
	})
	t.Run("ll", func(t *testing.T) {
		if _, err := exec.LookPath("clang"); err != nil {
			t.Skip("clang not in PATH")
		}
		dir := t.TempDir()
		ll, _ := codegen.LL{}.Emit(p)
		rt := codegen.LL{}.EmitRuntimeFor(p)
		f := filepath.Join(dir, "main.ll")
		rtf := filepath.Join(dir, "runtime.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(ll), 0o644)
		os.WriteFile(rtf, []byte(rt), 0o644)
		if out, err := exec.Command("clang", f, rtf, "-o", bin, "-lopenblas").CombinedOutput(); err != nil {
			skipNoBLAS(t, out, err)
			t.Fatalf("[ll] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[ll] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[ll] OpenBLAS gave %q, want %q", got, want)
		}
	})
}

// TestD3BLASVector is the arbitrary-length BLAS gate: a Rune FList of floats is
// MARSHALLED across the FFI into a C double[] and run through cblas_ddot (real
// OpenBLAS) on the native backends (C/LLVM), bound to an in-language reference fold
// by the tolerance contract (ch219). [1,2,3,4]·[1,1,1,1] = 10; within-default ok;
// negative-tolerance blame: "10\n10\n0\nunit". On the NATIVE backends (C/LLVM) the
// marshaller routes through cblas_ddot (real OpenBLAS); on the SOURCE backends
// (js/py/go/erl/rust) dotList is the portable reference loop over the same FList — the
// observable is byte-identical, parity at the CONTRACT across all seven backends.
func TestD3BLASVector(t *testing.T) {
	const want = "10\n10\n0\nunit"
	// Source backends: the portable dotList reference (no OpenBLAS link needed).
	srcBackends := append(append([]ioBackend{}, ioCLIBackends...), ioOSBackends[3]) // + rust
	for _, bk := range srcBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch219_blas_vector.rune", "main", ""); got != want {
				t.Errorf("[%s] dotList reference gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch219_blas_vector.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	skipNoBLAS := func(t *testing.T, out []byte, err error) {
		if err != nil && strings.Contains(string(out)+err.Error(), "cblas") {
			t.Skip("OpenBLAS/cblas.h not available")
		}
	}
	t.Run("c", func(t *testing.T) {
		if _, err := exec.LookPath("cc"); err != nil {
			t.Skip("cc not in PATH")
		}
		dir := t.TempDir()
		src, _ := codegen.C{}.Emit(p)
		f := filepath.Join(dir, "main.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(src), 0o644)
		if out, err := exec.Command("cc", "-o", bin, f, "-lopenblas").CombinedOutput(); err != nil {
			skipNoBLAS(t, out, err)
			t.Fatalf("[c] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[c] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[c] marshalled ddot gave %q, want %q", got, want)
		}
	})
	t.Run("ll", func(t *testing.T) {
		if _, err := exec.LookPath("clang"); err != nil {
			t.Skip("clang not in PATH")
		}
		dir := t.TempDir()
		ll, _ := codegen.LL{}.Emit(p)
		rt := codegen.LL{}.EmitRuntimeFor(p)
		f := filepath.Join(dir, "main.ll")
		rtf := filepath.Join(dir, "runtime.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(ll), 0o644)
		os.WriteFile(rtf, []byte(rt), 0o644)
		if out, err := exec.Command("clang", f, rtf, "-o", bin, "-lopenblas").CombinedOutput(); err != nil {
			skipNoBLAS(t, out, err)
			t.Fatalf("[ll] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[ll] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[ll] marshalled ddot gave %q, want %q", got, want)
		}
	})
}

// TestD3GemmMatrix is the matrix-BLAS gate: two flat row-major Rune FLists (a 2×2 A and
// a 2×2 B) are MARSHALLED across the FFI into C double[]s and run through cblas_dgemm
// (real OpenBLAS), the product reduced to the SUM of its entries (scalar observable, no
// host→Rune matrix construction). The tolerance contract binds the dgemm sum to a
// portable in-language reference (ch220). A·B = [[19,22],[43,50]], entry sum 134; within
// default ok; negative-tolerance blame: "134\n134\n0\nunit". Native-only (the dgemm
// kernel + 2-D marshaller live in the C/LLVM runtimes).
func TestD3GemmMatrix(t *testing.T) {
	const want = "134\n134\n0\nunit"
	// Source backends: the portable triple-loop gemmSum reference (no OpenBLAS link).
	srcBackends := append(append([]ioBackend{}, ioCLIBackends...), ioOSBackends[3]) // + rust
	for _, bk := range srcBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch220_gemm_matrix.rune", "main", ""); got != want {
				t.Errorf("[%s] gemmSum reference gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch220_gemm_matrix.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	skipNoBLAS := func(t *testing.T, out []byte, err error) {
		if err != nil && strings.Contains(string(out)+err.Error(), "cblas") {
			t.Skip("OpenBLAS/cblas.h not available")
		}
	}
	t.Run("c", func(t *testing.T) {
		if _, err := exec.LookPath("cc"); err != nil {
			t.Skip("cc not in PATH")
		}
		dir := t.TempDir()
		src, _ := codegen.C{}.Emit(p)
		f := filepath.Join(dir, "main.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(src), 0o644)
		if out, err := exec.Command("cc", "-o", bin, f, "-lopenblas").CombinedOutput(); err != nil {
			skipNoBLAS(t, out, err)
			t.Fatalf("[c] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[c] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[c] dgemm sum gave %q, want %q", got, want)
		}
	})
	t.Run("ll", func(t *testing.T) {
		if _, err := exec.LookPath("clang"); err != nil {
			t.Skip("clang not in PATH")
		}
		dir := t.TempDir()
		ll, _ := codegen.LL{}.Emit(p)
		rt := codegen.LL{}.EmitRuntimeFor(p)
		f := filepath.Join(dir, "main.ll")
		rtf := filepath.Join(dir, "runtime.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(ll), 0o644)
		os.WriteFile(rtf, []byte(rt), 0o644)
		if out, err := exec.Command("clang", f, rtf, "-o", bin, "-lopenblas").CombinedOutput(); err != nil {
			skipNoBLAS(t, out, err)
			t.Fatalf("[ll] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[ll] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[ll] dgemm sum gave %q, want %q", got, want)
		}
	})
}

// TestD4NumpyInterop is the D4 interop gate: npDot is ONE uniform Rune capability with
// per-backend impls — REAL NumPy (numpy.dot) on the py backend, OpenBLAS (cblas_ddot)
// on the native backends (C/LLVM), a portable reference loop on js/go/erl/rust. ch221's
// contract guards every impl against an in-language reference fold and BLAMES any that
// drifts past eps, so the foreign library is checked, never trusted blind. [1,2,3,4]·
// [4,3,2,1] = 20; within-default ok; negative-tolerance blame: "20\n20\n0\nunit". The
// observable is byte-identical across all seven backends — parity at the CONTRACT.
func TestD4NumpyInterop(t *testing.T) {
	const want = "20\n20\n0\nunit"
	// Source backends: py = real NumPy; js/go/erl/rust = the reference floor.
	srcBackends := append(append([]ioBackend{}, ioCLIBackends...), ioOSBackends[3]) // + rust
	for _, bk := range srcBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if bk.name == "py" {
				if err := exec.Command("python3", "-c", "import numpy").Run(); err != nil {
					t.Skip("numpy not installed")
				}
			}
			if got := runIOListing(t, bk, "ch221_numpy_interop.rune", "main", ""); got != want {
				t.Errorf("[%s] npDot gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch221_numpy_interop.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	skipNoBLAS := func(t *testing.T, out []byte, err error) {
		if err != nil && strings.Contains(string(out)+err.Error(), "cblas") {
			t.Skip("OpenBLAS/cblas.h not available")
		}
	}
	t.Run("c", func(t *testing.T) {
		if _, err := exec.LookPath("cc"); err != nil {
			t.Skip("cc not in PATH")
		}
		dir := t.TempDir()
		src, _ := codegen.C{}.Emit(p)
		f := filepath.Join(dir, "main.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(src), 0o644)
		if out, err := exec.Command("cc", "-o", bin, f, "-lopenblas").CombinedOutput(); err != nil {
			skipNoBLAS(t, out, err)
			t.Fatalf("[c] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[c] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[c] OpenBLAS npDot gave %q, want %q", got, want)
		}
	})
	t.Run("ll", func(t *testing.T) {
		if _, err := exec.LookPath("clang"); err != nil {
			t.Skip("clang not in PATH")
		}
		dir := t.TempDir()
		ll, _ := codegen.LL{}.Emit(p)
		rt := codegen.LL{}.EmitRuntimeFor(p)
		f := filepath.Join(dir, "main.ll")
		rtf := filepath.Join(dir, "runtime.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(ll), 0o644)
		os.WriteFile(rtf, []byte(rt), 0o644)
		if out, err := exec.Command("clang", f, rtf, "-o", bin, "-lopenblas").CombinedOutput(); err != nil {
			skipNoBLAS(t, out, err)
			t.Fatalf("[ll] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[ll] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[ll] OpenBLAS npDot gave %q, want %q", got, want)
		}
	})
}

// TestD4NumpyMean shows the contract-guarded interop pattern GENERALISES to a second,
// different NumPy API (numpy.mean, not a dot in disguise): py binds real numpy.mean, the
// other backends a portable hand sum/count floor (no BLAS — so C/LLVM link plainly). The
// same ch221 contract shape guards the library against an in-language reference
// (refSum/refLen) and blames drift. mean[2,4,6,8] = 5; within-default ok; negative-
// tolerance blame: "5\n5\n0\nunit" byte-identical across all seven backends.
func TestD4NumpyMean(t *testing.T) {
	const want = "5\n5\n0\nunit"
	srcBackends := append(append([]ioBackend{}, ioCLIBackends...), ioOSBackends[3]) // + rust
	for _, bk := range srcBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if bk.name == "py" {
				if err := exec.Command("python3", "-c", "import numpy").Run(); err != nil {
					t.Skip("numpy not installed")
				}
			}
			if got := runIOListing(t, bk, "ch222_numpy_mean.rune", "main", ""); got != want {
				t.Errorf("[%s] npMean gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch222_numpy_mean.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	t.Run("c", func(t *testing.T) {
		if _, err := exec.LookPath("cc"); err != nil {
			t.Skip("cc not in PATH")
		}
		dir := t.TempDir()
		src, _ := codegen.C{}.Emit(p)
		f := filepath.Join(dir, "main.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(src), 0o644)
		if out, err := exec.Command("cc", "-o", bin, f).CombinedOutput(); err != nil {
			t.Fatalf("[c] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[c] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[c] npMean gave %q, want %q", got, want)
		}
	})
	t.Run("ll", func(t *testing.T) {
		if _, err := exec.LookPath("clang"); err != nil {
			t.Skip("clang not in PATH")
		}
		dir := t.TempDir()
		ll, _ := codegen.LL{}.Emit(p)
		rt := codegen.LL{}.EmitRuntimeFor(p)
		f := filepath.Join(dir, "main.ll")
		rtf := filepath.Join(dir, "runtime.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(ll), 0o644)
		os.WriteFile(rtf, []byte(rt), 0o644)
		if out, err := exec.Command("clang", f, rtf, "-o", bin).CombinedOutput(); err != nil {
			t.Fatalf("[ll] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[ll] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[ll] npMean gave %q, want %q", got, want)
		}
	})
}

// TestD4NumpyMatmul is the 2-D interop gate: npMatSum reshapes two flat row-major Rune
// FLists into 2-D arrays and multiplies — REAL numpy matmul ((A@B).sum()) on py, OpenBLAS
// (cblas_dgemm) on C/LLVM, a portable triple loop on js/go/erl/rust — all guarded by the
// same contract against an in-language reference. A·B for [[1,2],[3,4]]·[[5,6],[7,8]]
// sums to 134; within-default ok; negative-tolerance blame: "134\n134\n0\nunit".
func TestD4NumpyMatmul(t *testing.T) {
	const want = "134\n134\n0\nunit"
	srcBackends := append(append([]ioBackend{}, ioCLIBackends...), ioOSBackends[3]) // + rust
	for _, bk := range srcBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if bk.name == "py" {
				if err := exec.Command("python3", "-c", "import numpy").Run(); err != nil {
					t.Skip("numpy not installed")
				}
			}
			if got := runIOListing(t, bk, "ch223_numpy_matmul.rune", "main", ""); got != want {
				t.Errorf("[%s] npMatSum gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch223_numpy_matmul.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	skipNoBLAS := func(t *testing.T, out []byte, err error) {
		if err != nil && strings.Contains(string(out)+err.Error(), "cblas") {
			t.Skip("OpenBLAS/cblas.h not available")
		}
	}
	t.Run("c", func(t *testing.T) {
		if _, err := exec.LookPath("cc"); err != nil {
			t.Skip("cc not in PATH")
		}
		dir := t.TempDir()
		src, _ := codegen.C{}.Emit(p)
		f := filepath.Join(dir, "main.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(src), 0o644)
		if out, err := exec.Command("cc", "-o", bin, f, "-lopenblas").CombinedOutput(); err != nil {
			skipNoBLAS(t, out, err)
			t.Fatalf("[c] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[c] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[c] OpenBLAS npMatSum gave %q, want %q", got, want)
		}
	})
	t.Run("ll", func(t *testing.T) {
		if _, err := exec.LookPath("clang"); err != nil {
			t.Skip("clang not in PATH")
		}
		dir := t.TempDir()
		ll, _ := codegen.LL{}.Emit(p)
		rt := codegen.LL{}.EmitRuntimeFor(p)
		f := filepath.Join(dir, "main.ll")
		rtf := filepath.Join(dir, "runtime.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(ll), 0o644)
		os.WriteFile(rtf, []byte(rt), 0o644)
		if out, err := exec.Command("clang", f, rtf, "-o", bin, "-lopenblas").CombinedOutput(); err != nil {
			skipNoBLAS(t, out, err)
			t.Fatalf("[ll] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[ll] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[ll] OpenBLAS npMatSum gave %q, want %q", got, want)
		}
	})
}

// TestD4NumpyVar adds a statistic whose reference takes two passes (mean, then mean of
// squared deviations): py binds real numpy.var, the others a hand 2-pass floor (no
// BLAS). Same contract guards it against refVar. var[2,4,6,8] = 5; within-default ok;
// negative-tolerance blame: "5\n5\n0\nunit" byte-identical across all seven backends.
func TestD4NumpyVar(t *testing.T) {
	const want = "5\n5\n0\nunit"
	srcBackends := append(append([]ioBackend{}, ioCLIBackends...), ioOSBackends[3]) // + rust
	for _, bk := range srcBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if bk.name == "py" {
				if err := exec.Command("python3", "-c", "import numpy").Run(); err != nil {
					t.Skip("numpy not installed")
				}
			}
			if got := runIOListing(t, bk, "ch224_numpy_var.rune", "main", ""); got != want {
				t.Errorf("[%s] npVar gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch224_numpy_var.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	t.Run("c", func(t *testing.T) {
		if _, err := exec.LookPath("cc"); err != nil {
			t.Skip("cc not in PATH")
		}
		dir := t.TempDir()
		src, _ := codegen.C{}.Emit(p)
		f := filepath.Join(dir, "main.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(src), 0o644)
		if out, err := exec.Command("cc", "-o", bin, f).CombinedOutput(); err != nil {
			t.Fatalf("[c] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[c] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[c] npVar gave %q, want %q", got, want)
		}
	})
	t.Run("ll", func(t *testing.T) {
		if _, err := exec.LookPath("clang"); err != nil {
			t.Skip("clang not in PATH")
		}
		dir := t.TempDir()
		ll, _ := codegen.LL{}.Emit(p)
		rt := codegen.LL{}.EmitRuntimeFor(p)
		f := filepath.Join(dir, "main.ll")
		rtf := filepath.Join(dir, "runtime.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(ll), 0o644)
		os.WriteFile(rtf, []byte(rt), 0o644)
		if out, err := exec.Command("clang", f, rtf, "-o", bin).CombinedOutput(); err != nil {
			t.Fatalf("[ll] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[ll] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[ll] npVar gave %q, want %q", got, want)
		}
	})
}

// TestD4NumpyMax adds an order (non-arithmetic) reduction: py binds real numpy.max, the
// others a fold-max floor. Same contract guards it against refMax. max[3,1,4,1,5,9,2,6]
// = 9; within-default ok; negative-tolerance blame: "9\n9\n0\nunit" across all seven.
func TestD4NumpyMax(t *testing.T) {
	const want = "9\n9\n0\nunit"
	srcBackends := append(append([]ioBackend{}, ioCLIBackends...), ioOSBackends[3]) // + rust
	for _, bk := range srcBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if bk.name == "py" {
				if err := exec.Command("python3", "-c", "import numpy").Run(); err != nil {
					t.Skip("numpy not installed")
				}
			}
			if got := runIOListing(t, bk, "ch225_numpy_max.rune", "main", ""); got != want {
				t.Errorf("[%s] npMax gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch225_numpy_max.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	t.Run("c", func(t *testing.T) {
		if _, err := exec.LookPath("cc"); err != nil {
			t.Skip("cc not in PATH")
		}
		dir := t.TempDir()
		src, _ := codegen.C{}.Emit(p)
		f := filepath.Join(dir, "main.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(src), 0o644)
		if out, err := exec.Command("cc", "-o", bin, f).CombinedOutput(); err != nil {
			t.Fatalf("[c] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[c] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[c] npMax gave %q, want %q", got, want)
		}
	})
	t.Run("ll", func(t *testing.T) {
		if _, err := exec.LookPath("clang"); err != nil {
			t.Skip("clang not in PATH")
		}
		dir := t.TempDir()
		ll, _ := codegen.LL{}.Emit(p)
		rt := codegen.LL{}.EmitRuntimeFor(p)
		f := filepath.Join(dir, "main.ll")
		rtf := filepath.Join(dir, "runtime.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(ll), 0o644)
		os.WriteFile(rtf, []byte(rt), 0o644)
		if out, err := exec.Command("clang", f, rtf, "-o", bin).CombinedOutput(); err != nil {
			t.Fatalf("[ll] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[ll] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[ll] npMax gave %q, want %q", got, want)
		}
	})
}

// TestD4NumpyNorm reaches genuine numpy.linalg: py binds numpy.linalg.norm, the others
// compute sqrt(sum of squares); the contract checks against an fsqrt-based reference.
// ‖[3,4]‖ = 5; within-default ok; negative-tolerance blame: "5\n5\n0\nunit". Native -lm.
func TestD4NumpyNorm(t *testing.T) {
	const want = "5\n5\n0\nunit"
	srcBackends := append(append([]ioBackend{}, ioCLIBackends...), ioOSBackends[3]) // + rust
	for _, bk := range srcBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if bk.name == "py" {
				if err := exec.Command("python3", "-c", "import numpy").Run(); err != nil {
					t.Skip("numpy not installed")
				}
			}
			if got := runIOListing(t, bk, "ch227_numpy_norm.rune", "main", ""); got != want {
				t.Errorf("[%s] npNorm gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch227_numpy_norm.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	t.Run("c", func(t *testing.T) {
		if _, err := exec.LookPath("cc"); err != nil {
			t.Skip("cc not in PATH")
		}
		dir := t.TempDir()
		src, _ := codegen.C{}.Emit(p)
		f := filepath.Join(dir, "main.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(src), 0o644)
		if out, err := exec.Command("cc", "-o", bin, f, "-lm").CombinedOutput(); err != nil {
			t.Fatalf("[c] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[c] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[c] npNorm gave %q, want %q", got, want)
		}
	})
	t.Run("ll", func(t *testing.T) {
		if _, err := exec.LookPath("clang"); err != nil {
			t.Skip("clang not in PATH")
		}
		dir := t.TempDir()
		ll, _ := codegen.LL{}.Emit(p)
		rt := codegen.LL{}.EmitRuntimeFor(p)
		f := filepath.Join(dir, "main.ll")
		rtf := filepath.Join(dir, "runtime.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(ll), 0o644)
		os.WriteFile(rtf, []byte(rt), 0o644)
		if out, err := exec.Command("clang", f, rtf, "-o", bin, "-lm").CombinedOutput(); err != nil {
			t.Fatalf("[ll] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[ll] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[ll] npNorm gave %q, want %q", got, want)
		}
	})
}

// TestD3FloatSqrt gates the IEEE-754 square-root primitive: a pure host sqrt baked per
// backend (the native backends link -lm). sqrt 16/81/144 = 4/9/12, byte-identical
// "4\n9\n12\nunit" across all seven backends. fsqrt is the building block for norms/std.
func TestD3FloatSqrt(t *testing.T) {
	const want = "4\n9\n12\nunit"
	srcBackends := append(append([]ioBackend{}, ioCLIBackends...), ioOSBackends[3]) // + rust
	for _, bk := range srcBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch226_float_sqrt.rune", "main", ""); got != want {
				t.Errorf("[%s] fsqrt gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch226_float_sqrt.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	t.Run("c", func(t *testing.T) {
		if _, err := exec.LookPath("cc"); err != nil {
			t.Skip("cc not in PATH")
		}
		dir := t.TempDir()
		src, _ := codegen.C{}.Emit(p)
		f := filepath.Join(dir, "main.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(src), 0o644)
		if out, err := exec.Command("cc", "-o", bin, f, "-lm").CombinedOutput(); err != nil {
			t.Fatalf("[c] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[c] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[c] fsqrt gave %q, want %q", got, want)
		}
	})
	t.Run("ll", func(t *testing.T) {
		if _, err := exec.LookPath("clang"); err != nil {
			t.Skip("clang not in PATH")
		}
		dir := t.TempDir()
		ll, _ := codegen.LL{}.Emit(p)
		rt := codegen.LL{}.EmitRuntimeFor(p)
		f := filepath.Join(dir, "main.ll")
		rtf := filepath.Join(dir, "runtime.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(ll), 0o644)
		os.WriteFile(rtf, []byte(rt), 0o644)
		if out, err := exec.Command("clang", f, rtf, "-o", bin, "-lm").CombinedOutput(); err != nil {
			t.Fatalf("[ll] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[ll] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Errorf("[ll] fsqrt gave %q, want %q", got, want)
		}
	})
}
