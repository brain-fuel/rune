package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// findJava25 locates a JDK 25+ javac/java pair (the JVM backend targets Java 25).
// It prefers an asdf-installed temurin-25 toolchain, then a PATH `javac` that
// reports major version ≥ 25. Returns ("", "", false) if none is available.
func findJava25() (javac, java string, ok bool) {
	home, _ := os.UserHomeDir()
	for _, jc := range globFirst(filepath.Join(home, ".asdf/installs/java/temurin-25*/bin/javac")) {
		jdir := filepath.Dir(jc)
		jv := filepath.Join(jdir, "java")
		if fileExists(jc) && fileExists(jv) {
			return jc, jv, true
		}
	}
	if jc, err := exec.LookPath("javac"); err == nil {
		out, _ := exec.Command(jc, "-version").CombinedOutput()
		if javacMajorAtLeast(string(out), 25) {
			if jv, err := exec.LookPath("java"); err == nil {
				return jc, jv, true
			}
		}
	}
	return "", "", false
}

func globFirst(pattern string) []string {
	m, _ := filepath.Glob(pattern)
	return m
}

func fileExists(p string) bool {
	_, err := os.Stat(p)
	return err == nil
}

// javacMajorAtLeast parses `javac 25.0.1` (stdout/stderr) and reports major >= want.
func javacMajorAtLeast(verLine string, want int) bool {
	f := strings.Fields(strings.TrimSpace(verLine))
	if len(f) < 2 {
		return false
	}
	v := f[1] // "25.0.1"
	if i := strings.IndexByte(v, '.'); i > 0 {
		v = v[:i]
	}
	maj := 0
	for _, c := range v {
		if c < '0' || c > '9' {
			break
		}
		maj = maj*10 + int(c-'0')
	}
	return maj >= want
}

// Cross-backend conformance (B-track / multi-backend): the SAME checked program
// must observe equal results on every backend. This is the codegen stratum's
// core guarantee — the erased IR is portable, not JS-specific. JS is exercised by
// the main run gate; here Python and Go run the same corpus and must agree.
//
// NOTE: the expression-only Python emit hits CPython's parser nesting cap on
// deeply-nested terms (ch11's unrolled gcd); the Go emit (statement bodies +
// thunks) does not. The corpus below stays within both. The Rust emit compiles
// first (rustc) then runs the binary — a `compile` step the interpreted backends
// leave nil.
func TestBackendConformance(t *testing.T) {
	corpus := []struct{ listing, main, want string }{
		{"ch04_data.rune", "four", "succ (succ (succ (succ zero)))"},
		{"ch06_quotients.rune", "parityOfTwo", "true"},
		{"ch32_sigma_run.rune", "answer", "false"},
		{"ch39_partial.rune", "answer", "zero"},
		{"ch43_io_run.rune", "prog", "succ (succ zero)"},
		// ch71: the DEPLOYED distributed protocol must observe the SAME rendezvous
		// result on every backend — incl. BEAM (erl), the natural distributed
		// target. Cross-backend parity for the M0 vertical slice.
		{"ch71_distributed.rune", "answer", "some (par halt halt)"},
		{"ch71_distributed.rune", "final", "par halt halt"},
		// ch72: the replicated counter's converged value (3) must agree on every
		// backend — a CRDT deployed with cross-backend parity.
		{"ch72_replicated_counter.rune", "converged", "succ (succ (succ zero))"},
		// ch188: the replicated state machine (foldl of the SMR step over the log)
		// applies [1,2,3] to state 0 → 6, identically on every backend.
		{"ch188_smr.rune", "smrState", "succ (succ (succ (succ (succ (succ zero)))))"},
		// ch191: the replica actor's applied state lives in a Σ (log, applied); replica B
		// reaches 6 by a mid-stream catch-up. This certifies the Σ-state erases to a tuple
		// and runs byte-identically across ALL backends (incl. JVM 25 / BEAM / Rust) —
		// portable, proof-carrying replicated consensus.
		{"ch191_actor_agreement.rune", "deployedB", "succ (succ (succ (succ (succ (succ zero)))))"},
	}
	type backend struct {
		name    string
		bin     string
		ext     string
		emit    func(codegen.Program) (codegen.TargetSource, error)
		run     func(file string) *exec.Cmd
		compile func(src, out string) *exec.Cmd // nil for interpreted backends
		// runtime, when non-nil, is a C runtime shim written beside the emitted
		// source as `runtime.c` (in the same temp dir) and linked by `compile`. The
		// LLVM backend emits the program LOGIC as genuine LLVM IR (.ll) and links
		// LL{}.EmitRuntime() — the tagged-word Value rep, the mark-sweep GC, apply,
		// and $show — via `clang program.ll runtime.c -o exe`.
		runtime func() string
	}
	backends := []backend{
		{name: "py", bin: "python3", ext: "py", emit: codegen.Py{}.Emit,
			run: func(f string) *exec.Cmd { return exec.Command("python3", f) }},
		{name: "go", bin: "go", ext: "go", emit: codegen.Go{}.Emit,
			run: func(f string) *exec.Cmd { return exec.Command("go", "run", f) }},
		{name: "rs", bin: "rustc", ext: "rs", emit: codegen.Rust{}.Emit,
			run:     func(bin string) *exec.Cmd { return exec.Command(bin) },
			compile: func(src, out string) *exec.Cmd { return exec.Command("rustc", "--edition", "2021", "-o", out, src) }},
		{name: "erl", bin: "escript", ext: "erl", emit: codegen.Beam{}.Emit,
			run: func(f string) *exec.Cmd { return exec.Command("escript", f) }},
		// c: the first NATIVE backend (telos-2 / M4) — closure-converted IR + an
		// embedded closure runtime, compiled to machine code by the system cc, then
		// the binary runs. The SAME corpus observes byte-identical $show as the six
		// source backends (the conformance gate extended to a real native artifact).
		{name: "c", bin: "cc", ext: "c", emit: codegen.C{}.Emit,
			run:     func(bin string) *exec.Cmd { return exec.Command(bin) },
			compile: func(src, out string) *exec.Cmd { return exec.Command("cc", "-o", out, src) }},
		// ll: the SECOND NATIVE backend (telos-2 / M4, B3+ fan-out) — the SAME
		// closure-converted IR emitted as genuine LLVM IR text, linking the C runtime
		// (LL{}.EmitRuntime(), the external-linkage twin of c's runtime). clang
		// compiles `program.ll runtime.c -o exe`. The full conformance corpus must be
		// byte-identical to C (the oracle) and the source backends — a real LLVM
		// artifact in the cross-backend gate, not just a hand-picked subset.
		{name: "ll", bin: "clang", ext: "ll", emit: codegen.LL{}.Emit,
			run: func(bin string) *exec.Cmd { return exec.Command(bin) },
			compile: func(src, out string) *exec.Cmd {
				rt := filepath.Join(filepath.Dir(src), "runtime.c")
				return exec.Command("clang", src, rt, "-o", out)
			},
			runtime: func() string { return codegen.LL{}.EmitRuntime() }},
	}
	// JVM (Java 25+): records/sealed/pattern-switch/virtual-threads need a JDK 25.
	// PATH's `java` may be older, so resolve the toolchain explicitly and skip if
	// absent. compile writes main.class to the temp dir; run names the class.
	if javac25, java25, ok := findJava25(); ok {
		backends = append(backends, backend{name: "jvm", bin: javac25, ext: "java", emit: codegen.JVM{}.Emit,
			run: func(out string) *exec.Cmd { return exec.Command(java25, "-cp", filepath.Dir(out), "main") },
			compile: func(src, out string) *exec.Cmd {
				return exec.Command(javac25, "--release", "25", "-d", filepath.Dir(out), src)
			}})
	}
	for _, bk := range backends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			for _, tc := range corpus {
				s := loadListing(t, tc.listing)
				p, err := s.EmitProgram(tc.main)
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
				// Native backends that link a separate C runtime (the LLVM backend)
				// write it beside the emitted source as `runtime.c`; compile links both.
				if bk.runtime != nil {
					if err := os.WriteFile(filepath.Join(dir, "runtime.c"), []byte(bk.runtime()), 0o644); err != nil {
						t.Fatal(err)
					}
				}
				runFile := f
				if bk.compile != nil {
					bin := filepath.Join(dir, "main.bin")
					if out, err := bk.compile(f, bin).CombinedOutput(); err != nil {
						t.Fatalf("[%s] %s: compile failed: %v\n%s\n--- emitted ---\n%s", bk.name, tc.listing, err, out, src)
					}
					runFile = bin
				}
				// Capture stdout only — the program's result. Some toolchains (escript)
				// print compile warnings to stderr; those are not the observed output.
				out, err := bk.run(runFile).Output()
				if err != nil {
					stderr := ""
					if ee, ok := err.(*exec.ExitError); ok {
						stderr = string(ee.Stderr)
					}
					t.Fatalf("[%s] %s: run failed: %v\n%s", bk.name, tc.listing, err, stderr)
				}
				if got := strings.TrimSpace(string(out)); got != tc.want {
					t.Fatalf("[%s] %s: gave %q, want %q (must match JS)", bk.name, tc.listing, got, tc.want)
				}
			}
		})
	}
}
