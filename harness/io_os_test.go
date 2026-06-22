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

// TestD6FileEnvRust brings Rust to D6 fs+env conformance (the cross-backend parity
// companion to TestIOFileEnvConformance's js/py/go/erl). Rust needs a compile step, so
// like the JVM float gate it gets its own test rather than the interpret-or-prebuilt
// ioCLIBackends loop. ch215 reads $RUNE_D6, writes+reads a temp file, prints the same
// "hello, wootz\nok\nunit" the other backends do — proving getEnvCode/readFileCode/
// writeFileCode/printStrCode are byte-identical on Rust (std::env + std::fs).
func TestD6FileEnvRust(t *testing.T) {
	rustc, err := exec.LookPath("rustc")
	if err != nil {
		t.Skip("rustc not in PATH")
	}
	dir := t.TempDir()
	s := loadListing(t, "ch215_io_fs_env.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.Rust{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	rf := filepath.Join(dir, "main.rs")
	if err := os.WriteFile(rf, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	bin := filepath.Join(dir, "main")
	if out, err := exec.Command(rustc, "--edition", "2021", rf, "-o", bin).CombinedOutput(); err != nil {
		t.Fatalf("[rust] rustc: %v\n%s", err, out)
	}
	cmd := exec.Command(bin)
	cmd.Dir = dir // relative path "d6.tmp" resolves here
	cmd.Env = append(os.Environ(), "RUNE_D6=ok")
	out, err := cmd.Output()
	if err != nil {
		t.Fatalf("[rust] run: %v", err)
	}
	if want := "hello, wootz\nok\nunit"; strings.TrimSpace(string(out)) != want {
		t.Errorf("[rust] D6 fs+env run gave %q, want %q", strings.TrimSpace(string(out)), want)
	}
}

// TestD6ArgvExitRust brings Rust to D6 argv+process conformance: ch216 reads its argv
// (argCountCode/argAtCode) and exits with status = argc (exitWith). Run with `alpha beta`,
// stdout is "2\nalpha\nbeta" and the exit status is 2, identical to js/py/go/erl. Rust is
// compiled to a binary (like the go case), so the child's exit code passes through.
func TestD6ArgvExitRust(t *testing.T) {
	rustc, err := exec.LookPath("rustc")
	if err != nil {
		t.Skip("rustc not in PATH")
	}
	dir := t.TempDir()
	s := loadListing(t, "ch216_io_argv_exit.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.Rust{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	rf := filepath.Join(dir, "main.rs")
	if err := os.WriteFile(rf, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	bin := filepath.Join(dir, "main")
	if out, err := exec.Command(rustc, "--edition", "2021", rf, "-o", bin).CombinedOutput(); err != nil {
		t.Fatalf("[rust] rustc: %v\n%s", err, out)
	}
	cmd := exec.Command(bin, "alpha", "beta")
	out, err := cmd.Output()
	exitCode := 0
	if err != nil {
		if ee, ok := err.(*exec.ExitError); ok {
			exitCode = ee.ExitCode()
		} else {
			t.Fatalf("[rust] run failed: %v", err)
		}
	}
	if got, want := strings.TrimSpace(string(out)), "2\nalpha\nbeta"; got != want {
		t.Errorf("[rust] argv stdout = %q, want %q", got, want)
	}
	if exitCode != 2 {
		t.Errorf("[rust] exit status = %d, want 2", exitCode)
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
// TestD3FloatJVM closes the float backend matrix: the f64 element type + the BLAS
// kernel now run on Java 25 too (the JVM `V` gained a VFloat variant + the float
// host bodies fromNat/fadd/.../floatToNat/fleqN/dot2 + printNat as `static V`
// methods). ch217 prints the SAME 11/13/11/0/unit the source backends do — float
// parity across all of js/py/go/erl/rust/C/LLVM/JVM.
// TestD4PlottingPy is the D4 PLOTTING-reach gate (telos 3): ch441 renders a float
// vector through REAL matplotlib on the py backend and writes a PNG, returning the
// point count. We run it in a temp dir and assert both the observable count (4) and
// a non-empty wavelet_plot.png. Skips when matplotlib is absent (the plotting reach
// is py-specific; bin/setup.sh pip-installs it).
func TestD4PlottingPy(t *testing.T) {
	py, err := exec.LookPath("python3")
	if err != nil {
		t.Skip("python3 not in PATH")
	}
	if err := exec.Command(py, "-c", "import matplotlib").Run(); err != nil {
		t.Skip("matplotlib not installed (run bin/setup.sh)")
	}
	s := loadListing(t, "ch441_plotting.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.Py{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	f := filepath.Join(dir, "main.py")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command(py, f)
	cmd.Dir = dir
	out, err := cmd.Output()
	if err != nil {
		t.Fatalf("python run: %v", err)
	}
	if !strings.Contains(string(out), "4") {
		t.Errorf("plotSave count = %q, want 4", strings.TrimSpace(string(out)))
	}
	png := filepath.Join(dir, "wavelet_plot.png")
	fi, err := os.Stat(png)
	if err != nil {
		t.Fatalf("no plot written: %v", err)
	}
	if fi.Size() == 0 {
		t.Errorf("wavelet_plot.png is empty")
	}
}

// TestD4ShapeMatVec is the D4 2-D shape-safety gate: the shape-checked matrix×vector
// (ch446) elaborates — the dimension contract `Eq Nat (cols M) (len v)` type-checks —
// and computes the product. [[1,2],[3,4]]·[5,6] = [17,39]; main prints the head, 17.
// (A length-3 vector would be a COMPILE ERROR, the shape mismatch the type rejects.)
func TestD4ShapeMatVec(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go not in PATH")
	}
	s := loadListing(t, "ch446_shape_matvec.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.Go{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	f := filepath.Join(t.TempDir(), "main.go")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command("go", "run", f).Output()
	if err != nil {
		t.Fatalf("go run: %v", err)
	}
	got := strings.TrimSpace(string(out))
	if first, _, _ := strings.Cut(got, "\n"); first != "17" {
		t.Errorf("shape-checked M·v head = %q, want first line 17", got)
	}
}

// TestD4ShapeMatMul is the D4 2-D shape-safety gate, part II: the shape-checked
// matrix×matrix (ch447) elaborates — the inner-dimension contract `Eq Nat (cols A)
// (rows B)` type-checks — and computes the product. A·B for [[1,2],[3,4]]·[[5,6],[7,8]]
// = [[19,22],[43,50]]; main prints the (0,0) entry, 19. (A B with rows ≠ cols A would be
// a COMPILE ERROR at `refl`, the non-conformable shape the type rejects.)
func TestD4ShapeMatMul(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go not in PATH")
	}
	s := loadListing(t, "ch447_shape_matmul.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.Go{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	f := filepath.Join(t.TempDir(), "main.go")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command("go", "run", f).Output()
	if err != nil {
		t.Fatalf("go run: %v", err)
	}
	got := strings.TrimSpace(string(out))
	if first, _, _ := strings.Cut(got, "\n"); first != "19" {
		t.Errorf("shape-checked A·B (0,0) = %q, want first line 19", got)
	}
}

// TestD4MatMulRows gates ch448: the machine-checked theorem rows(matMul A B) = rows A
// (the product preserves the left operand's row count, for ALL A and B — induction over
// A, cong succ on the IH). The proof type-checks under TestListingsElaborateAndCheck;
// here the concrete witness runs and prints rows A = 2.
func TestD4MatMulRows(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go not in PATH")
	}
	s := loadListing(t, "ch448_matmul_rows.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.Go{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	f := filepath.Join(t.TempDir(), "main.go")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command("go", "run", f).Output()
	if err != nil {
		t.Fatalf("go run: %v", err)
	}
	got := strings.TrimSpace(string(out))
	if first, _, _ := strings.Cut(got, "\n"); first != "2" {
		t.Errorf("rows(A·B) = %q, want first line 2", got)
	}
}

// TestD4MatVecLen gates ch450: the machine-checked theorem len(matVec M v) = rows M (the
// product vector has exactly one entry per row of M, for ALL M, v — induction over M,
// cong succ on the IH), the output-shape dual of ch446's input contract. The proof
// type-checks under TestListingsElaborateAndCheck; the witness runs and prints rows M = 2.
func TestD4MatVecLen(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go not in PATH")
	}
	s := loadListing(t, "ch450_matvec_len.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.Go{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	f := filepath.Join(t.TempDir(), "main.go")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command("go", "run", f).Output()
	if err != nil {
		t.Fatalf("go run: %v", err)
	}
	got := strings.TrimSpace(string(out))
	if first, _, _ := strings.Cut(got, "\n"); first != "2" {
		t.Errorf("len(matVec M v) = %q, want first line 2", got)
	}
}

// TestE3VisibleRunPrefix gates ch452: the machine-checked adequacy lemma prefixMono —
// the k-step runtime observation is a PREFIX of the (k+1)-step one, for all fuel and all
// processes (induction on fuel; consVisPrefix preserves the prefix under the shared first
// step). It is the monotonicity bridging the bounded runs (ch207) to the coinductive
// stream (ch209). The proof type-checks under TestListingsElaborateAndCheck; the witness
// (a supervised crash) computes isPrefix(run 1, run 2) = true and prints 1.
func TestE3VisibleRunPrefix(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go not in PATH")
	}
	s := loadListing(t, "ch452_visiblerun_prefix.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.Go{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	f := filepath.Join(t.TempDir(), "main.go")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command("go", "run", f).Output()
	if err != nil {
		t.Fatalf("go run: %v", err)
	}
	got := strings.TrimSpace(string(out))
	if first, _, _ := strings.Cut(got, "\n"); first != "1" {
		t.Errorf("isPrefix(visibleRun 1, visibleRun 2) witness = %q, want first line 1", got)
	}
}

// TestE3MaxRegister gates ch453: the grow-only max-register is a CvRDT — merge = natMax,
// proven commutative/idempotent/associative (the convergence ch416's naive take-peer LWW
// FAILS), lifted from ch369–373's lattice laws by cong reg. The proofs type-check under
// TestListingsElaborateAndCheck; the witness merges replicas {3,7} both orders and prints
// 7 twice — convergence to the max.
func TestE3MaxRegister(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go not in PATH")
	}
	s := loadListing(t, "ch453_max_register_crdt.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.Go{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	f := filepath.Join(t.TempDir(), "main.go")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command("go", "run", f).Output()
	if err != nil {
		t.Fatalf("go run: %v", err)
	}
	if got := strings.TrimSpace(string(out)); !strings.HasPrefix(got, "7\n7") {
		t.Errorf("max-register convergence = %q, want it to start 7\\n7", got)
	}
}

// TestE3MaxRegisterNative carries ch453's max-register CvRDT to the NATIVE backends: the
// same proven-convergent register RUNS on C (self-contained emit) and LLVM-IR (emit +
// EmitRuntimeFor runtime, clang), converging {3,7}->7 both orders identically to the source
// backends. So the verified CvRDT runs across source + native — the Lambert cross-backend bar.
func TestE3MaxRegisterNative(t *testing.T) {
	s := loadListing(t, "ch453_max_register_crdt.rune")
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
		if got := strings.TrimSpace(string(out)); !strings.HasPrefix(got, "7\n7") {
			t.Errorf("[c] max-register = %q, want it to start 7\\n7", got)
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
		if got := strings.TrimSpace(string(out)); !strings.HasPrefix(got, "7\n7") {
			t.Errorf("[ll] max-register = %q, want it to start 7\\n7", got)
		}
	})
}

// TestE3MaxRegisterInflation gates ch455: the monotonicity half of CvRDT correctness for
// the max-register — mergeInflationary, leb(val x)(val(merge x y)) = true, a replica's
// value never decreases under merge (state climbs the lattice). Rests on the structural
// max bound lebMaxL. With ch453's convergence this completes the correctness pair. The
// proof type-checks under TestListingsElaborateAndCheck; the witness prints 1.
func TestE3MaxRegisterInflation(t *testing.T) {
	if _, err := exec.LookPath("go"); err != nil {
		t.Skip("go not in PATH")
	}
	s := loadListing(t, "ch455_max_register_inflationary.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.Go{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	f := filepath.Join(t.TempDir(), "main.go")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command("go", "run", f).Output()
	if err != nil {
		t.Fatalf("go run: %v", err)
	}
	if first, _, _ := strings.Cut(strings.TrimSpace(string(out)), "\n"); first != "1" {
		t.Errorf("max-register inflation witness = %q, want first line 1", strings.TrimSpace(string(out)))
	}
}

func TestD3FloatJVM(t *testing.T) {
	javac25, java25, ok := findJava25()
	if !ok {
		t.Skip("no JDK 25 (asdf temurin-25) — the JVM backend targets Java 25")
	}
	s := loadListing(t, "ch217_float_blas.rune")
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
	out, err := exec.Command(java25, "-cp", dir, "main").Output()
	if err != nil {
		t.Fatalf("java run: %v", err)
	}
	if want := "11\n13\n11\n0\nunit"; strings.TrimSpace(string(out)) != want {
		t.Errorf("JVM float/BLAS run gave %q, want %q", strings.TrimSpace(string(out)), want)
	}
}

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

// TestD4NumpyPipeline is the interop capstone: a composed pipeline (mean -> center ->
// norm) where stage outputs feed the next and the final foreign result is contract-
// guarded. py runs real numpy.mean + numpy.linalg.norm; the others the reference floor.
// xs=[2,4,6,8]: mean 5, ‖centered‖=sqrt(20)~4.47 truncated to 4: "5\n4\nunit". Native -lm.
func TestD4NumpyPipeline(t *testing.T) {
	const want = "5\n4\nunit"
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
			if got := runIOListing(t, bk, "ch228_numpy_pipeline.rune", "main", ""); got != want {
				t.Errorf("[%s] pipeline gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch228_numpy_pipeline.rune")
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
			t.Errorf("[c] pipeline gave %q, want %q", got, want)
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
			t.Errorf("[ll] pipeline gave %q, want %q", got, want)
		}
	})
}

// TestStdlibSafeHead checks the total safe-head: head guarded by a non-emptiness proof
// runs on a non-empty list (a nnil call is a compile error, verified separately). Pure
// Rune, all seven backends: safeHead [7,8] = 7, "7\nunit".
func TestStdlibSafeHead(t *testing.T) {
	const want = "7\nunit"
	srcBackends := append(append([]ioBackend{}, ioCLIBackends...), ioOSBackends[3]) // + rust
	for _, bk := range srcBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch249_safe_head.rune", "main", ""); got != want {
				t.Errorf("[%s] safeHead gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch249_safe_head.rune")
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
			t.Errorf("[c] safeHead gave %q, want %q", got, want)
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
			t.Errorf("[ll] safeHead gave %q, want %q", got, want)
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

// TestD3FloatPow gates the floating-point power primitive: a pure host pow baked per
// backend (native link -lm). 2^10/3^4/5^3 = 1024/81/125, byte-identical
// "1024\n81\n125\nunit" across all seven backends.
func TestD3FloatPow(t *testing.T) {
	const want = "1024\n81\n125\nunit"
	srcBackends := append(append([]ioBackend{}, ioCLIBackends...), ioOSBackends[3]) // + rust
	for _, bk := range srcBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch229_float_pow.rune", "main", ""); got != want {
				t.Errorf("[%s] fpow gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch229_float_pow.rune")
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
			t.Errorf("[c] fpow gave %q, want %q", got, want)
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
			t.Errorf("[ll] fpow gave %q, want %q", got, want)
		}
	})
}

// TestFurnaceFloatLaws exercises the furnace (the property-test spine): three foreign
// float laws (pow b 2 ~ b*b; sqrt(b*b) ~ b; sqrt(pow b 2) ~ b) checked within tolerance
// against in-language references, on every backend. All hold: "1\n1\n1\nunit". Native -lm.
func TestFurnaceFloatLaws(t *testing.T) {
	const want = "1\n1\n1\nunit"
	srcBackends := append(append([]ioBackend{}, ioCLIBackends...), ioOSBackends[3]) // + rust
	for _, bk := range srcBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch230_furnace_float_laws.rune", "main", ""); got != want {
				t.Errorf("[%s] furnace gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch230_furnace_float_laws.rune")
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
			t.Errorf("[c] furnace gave %q, want %q", got, want)
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
			t.Errorf("[ll] furnace gave %q, want %q", got, want)
		}
	})
}

// TestFurnaceNumpy proves the furnace is not vacuous: it runs a FALSE numpy law beside
// true ones and the verdict distinguishes them (1 true, 0 false, 1 true). P1 npMean ~
// refMean (1); P2 npMean ~ refSum (0, the lie caught); P3 npNorm ~ refNorm (1). For
// xs=[2,4,6,8]: "1\n0\n1\nunit" across all seven backends. Native -lm.
func TestFurnaceNumpy(t *testing.T) {
	const want = "1\n0\n1\nunit"
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
			if got := runIOListing(t, bk, "ch231_furnace_numpy.rune", "main", ""); got != want {
				t.Errorf("[%s] furnace-numpy gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch231_furnace_numpy.rune")
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
			t.Errorf("[c] furnace-numpy gave %q, want %q", got, want)
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
			t.Errorf("[ll] furnace-numpy gave %q, want %q", got, want)
		}
	})
}

// TestFurnaceTierBridge ties the two numeric tiers: the dot product computed by FOREIGN
// numpy (npDot) is checked against the SAME quantity computed in EXACT proven Nat
// arithmetic (natAdd/natMul, lifted to Float). A 1 means the fast path agrees with the
// proven path. [1,2,3]·[4,5,6] = 32: "32\n1\nunit". Native uses cblas (-lopenblas).
func TestFurnaceTierBridge(t *testing.T) {
	const want = "32\n1\nunit"
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
			if got := runIOListing(t, bk, "ch232_furnace_tier_bridge.rune", "main", ""); got != want {
				t.Errorf("[%s] tier-bridge gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch232_furnace_tier_bridge.rune")
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
			t.Errorf("[c] tier-bridge gave %q, want %q", got, want)
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
			t.Errorf("[ll] tier-bridge gave %q, want %q", got, want)
		}
	})
}

// TestFurnaceMatMul ties the two tiers for 2-D linear algebra: the entry-SUM of A·B
// computed by FOREIGN numpy matmul (npMatSum, real np `@` on py / reference loop elsewhere)
// is checked against the SAME sum computed by the EXACT proven shape-safe product (ch447's
// matMul, summed in Nat). A 1 means the fast numpy path agrees with the machine-checked
// product. [[1,2],[3,4]]·[[5,6],[7,8]] has Σ entries 134: "134\n1\nunit" on js/py/go/erl/rust.
func TestFurnaceMatMul(t *testing.T) {
	const want = "134\n1\nunit"
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
			if got := runIOListing(t, bk, "ch449_furnace_matmul.rune", "main", ""); got != want {
				t.Errorf("[%s] furnace-matmul gave %q, want %q", bk.name, got, want)
			}
		})
	}
}

// TestFurnaceMax ties the two tiers for a REDUCTION-BY-ORDER op: the maximum of a list
// computed by FOREIGN numpy (npMax) is checked against the SAME max by the EXACT proven
// natMax fold (ch369–373's lattice operation). A 1 means the fast order-reduction agrees
// with the machine-checked lattice max. max [3,7,2,5] = 7: "7\n1\nunit" on js/py/go/erl/rust.
func TestFurnaceMax(t *testing.T) {
	const want = "7\n1\nunit"
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
			if got := runIOListing(t, bk, "ch451_furnace_max.rune", "main", ""); got != want {
				t.Errorf("[%s] furnace-max gave %q, want %q", bk.name, got, want)
			}
		})
	}
}

// TestFurnaceCRDT carries the Savage furnace on-ramp to the DISTRIBUTED track: a runnable
// property suite for max-register convergence (ch453's CvRDT) — merge commutativity,
// idempotence, associativity tested on concrete divergent states, reported 1/0 per law.
// The tested tier whose proven tier is ch453's mergeComm/mergeIdem/mergeAssoc. Pure Rune
// (no numpy): "1\n1\n1\nunit" on js/py/go/erl/rust.
func TestFurnaceCRDT(t *testing.T) {
	const want = "1\n1\n1\nunit"
	srcBackends := append(append([]ioBackend{}, ioCLIBackends...), ioOSBackends[3]) // + rust
	for _, bk := range srcBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch454_furnace_crdt.rune", "main", ""); got != want {
				t.Errorf("[%s] furnace-crdt gave %q, want %q", bk.name, got, want)
			}
		})
	}
}

// TestD4ShapeProven realizes the R-INTEROP headline (shapes proven, not checked) on the
// current substrate: safeDot requires a proof Eq Nat (len xs) (len ys), so a mismatched
// call is a compile error (verified manually: "refl does not prove the equation"), and the dot is
// computed in exact Nat arithmetic. [1,2,3]·[4,5,6] = 32: "32\nunit" on all seven
// backends — pure Rune, no foreign libs (plain compile, no -lm/-lopenblas).
func TestD4ShapeProven(t *testing.T) {
	const want = "32\nunit"
	srcBackends := append(append([]ioBackend{}, ioCLIBackends...), ioOSBackends[3]) // + rust
	for _, bk := range srcBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch233_shape_proven.rune", "main", ""); got != want {
				t.Errorf("[%s] safeDot gave %q, want %q", bk.name, got, want)
			}
		})
	}
	s := loadListing(t, "ch233_shape_proven.rune")
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
			t.Errorf("[c] safeDot gave %q, want %q", got, want)
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
			t.Errorf("[ll] safeDot gave %q, want %q", got, want)
		}
	})
}

// TestStringDeployConformance deploys the self-contained string program ch439 (concatenate
// with the `++` operator, print via printStrCode) on every source backend. The point of the
// string story's runtime half: `++` works in real, compiled code, not just the REPL.
func TestStringDeployConformance(t *testing.T) {
	const want = "Hello, wootz!\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		// rust's packed-String host body (printStrCode) is parked (PARKING-LOT, like ch213),
		// so the string-marshalling IO prims do not deploy there yet.
		if bk.name == "rs" {
			continue
		}
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch439_string_deploy.rune", "main", ""); got != want {
				t.Errorf("[%s] string deploy printed %q, want %q", bk.name, got, want)
			}
		})
	}
}
