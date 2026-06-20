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
