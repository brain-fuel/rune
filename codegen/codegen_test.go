package codegen_test

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/session"
)

// filepathDir is filepath.Dir, named locally to keep the JVM backend wiring terse.
func filepathDir(p string) string { return filepath.Dir(p) }

// findJDK25 locates a JDK 25+ javac/java pair for the JVM backend (it targets Java
// 25). Prefers an asdf temurin-25 toolchain, then a PATH javac reporting major ≥ 25.
// Mirrors harness.findJava25 (the harness copy stays the conformance gate's own).
func findJDK25() (javac, java string, ok bool) {
	home, _ := os.UserHomeDir()
	matches, _ := filepath.Glob(filepath.Join(home, ".asdf/installs/java/temurin-25*/bin/javac"))
	for _, jc := range matches {
		jv := filepath.Join(filepath.Dir(jc), "java")
		if fileExists(jc) && fileExists(jv) {
			return jc, jv, true
		}
	}
	if jc, err := exec.LookPath("javac"); err == nil {
		out, _ := exec.Command(jc, "-version").CombinedOutput()
		if javacMajor(string(out)) >= 25 {
			if jv, err := exec.LookPath("java"); err == nil {
				return jc, jv, true
			}
		}
	}
	return "", "", false
}

func fileExists(p string) bool { _, err := os.Stat(p); return err == nil }

// javacMajor parses the major version from `javac 25.0.1` (stdout/stderr).
func javacMajor(verLine string) int {
	f := strings.Fields(strings.TrimSpace(verLine))
	if len(f) < 2 {
		return 0
	}
	v := f[1]
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
	return maj
}

// emitWith loads source and emits the program with the given main through a
// chosen backend.
func emitWith(t *testing.T, bk codegen.Backend, src, main string) string {
	t.Helper()
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load: %v", err)
	}
	p, err := s.EmitProgram(main)
	if err != nil {
		t.Fatal(err)
	}
	out, err := bk.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	return string(out)
}

// runRust compiles emitted Rust with rustc and returns its stdout, skipping when
// rustc is not installed.
func runRust(t *testing.T, src string) string {
	t.Helper()
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not in PATH")
	}
	dir := t.TempDir()
	f := dir + "/main.rs"
	bin := dir + "/main"
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	if out, err := exec.Command("rustc", "--edition", "2021", "-o", bin, f).CombinedOutput(); err != nil {
		t.Fatalf("rustc: %v\n%s\n--- emitted ---\n%s", err, out, src)
	}
	out, err := exec.Command(bin).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v\n%s", err, out)
	}
	return strings.TrimSpace(string(out))
}

// runC emits, compiles (cc), and runs the C backend's output, returning stdout.
// The C backend is the first NATIVE target (telos-2 / M4): closure-converted IR +
// an embedded closure runtime, compiled to machine code by the system cc. Skips
// gracefully when no C compiler is on PATH (cc IS present in the build env).
func runC(t *testing.T, src string) string {
	t.Helper()
	cc := ""
	for _, cand := range []string{"cc", "gcc", "clang"} {
		if _, err := exec.LookPath(cand); err == nil {
			cc = cand
			break
		}
	}
	if cc == "" {
		t.Skip("no C compiler (cc/gcc/clang) in PATH")
	}
	dir := t.TempDir()
	f := dir + "/main.c"
	bin := dir + "/main"
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	if out, err := exec.Command(cc, "-o", bin, f).CombinedOutput(); err != nil {
		t.Fatalf("%s: %v\n%s\n--- emitted ---\n%s", cc, err, out, src)
	}
	out, err := exec.Command(bin).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v\n%s", err, out)
	}
	return strings.TrimSpace(string(out))
}

// TestCEmitListLength is the first-native-backend keystone: a recursive ListElim
// (the eliminator lowered through the shared IR, then CLOSURE-CONVERTED and
// emitted as native C functions over a {code,env} closure runtime) computes the
// same fold as every source backend — byte-identical $show, compiled by cc.
func TestCEmitListLength(t *testing.T) {
	src := natSrc + `
data List : U -> U is
  nil : (A : U) -> List A
| cons : (A : U) -> A -> List A -> List A
end
length : (A : U) -> List A -> Nat is
  fn (A : U) (xs : List A) is
    ListElim A (fn (x : List A) is Nat end)
      zero
      (fn (x : A) (rest : List A) (ih : Nat) is succ ih end)
      xs
  end
end
two : Nat is length Nat (cons Nat zero (cons Nat (succ zero) (nil Nat))) end
`
	if got := runC(t, emitWith(t, codegen.C{}, src, "two")); got != "succ (succ zero)" {
		t.Fatalf("c list length: got %q", got)
	}
}

// TestCEmitAndRunNat pins native nat arithmetic: zero/succ/NatElim become the
// immediate-int rep + a fold loop in the closure runtime, and add computes the
// same successor chain the other backends show.
func TestCEmitAndRunNat(t *testing.T) {
	src := natSrc + `three : Nat is add (succ zero) (succ (succ zero)) end`
	if got := runC(t, emitWith(t, codegen.C{}, src, "three")); got != "succ (succ (succ zero))" {
		t.Fatalf("c nat: got %q", got)
	}
}

// TestCNatLitAndAccelRun pins C7 / R-NUM on the native backend: compressed
// numerals deploy as immediate ints and the accel add/mul/monus become native C
// arithmetic — byte-identical to the eliminator-loop result the other backends
// give (incl. the saturating Peano monus 0-floor `monusN 3 5 = 0`).
func TestCNatLitAndAccelRun(t *testing.T) {
	for _, tc := range []struct{ src, main, want string }{
		{bigNatSrc, "big", "5000"},
		{bigNatSrc, "bigger", "5002"},
		{bigNatSrc, "product", "10000"},
		{accelNatSrc, "sum", "8000"},
		{accelNatSrc, "prod", "10000"},
		{accelNatSrc, "diff", "5"},
		{accelNatSrc, "diffZero", "0"},
	} {
		if got := runC(t, emitWith(t, codegen.C{}, tc.src, tc.main)); got != tc.want {
			t.Fatalf("c %s: got %q, want %q", tc.main, got, tc.want)
		}
	}
}

// runCGC is runC but compiles with a chosen GC threshold (and GC stats on) so the
// non-moving mark-sweep collector can be forced to fire MANY times during the run.
// Returns (stdout, number-of-collections). The collector must produce output
// byte-identical to a no-collection run regardless of how often it fires.
func runCGC(t *testing.T, src string, thresholdBytes int) (string, int) {
	t.Helper()
	cc := ""
	for _, cand := range []string{"cc", "gcc", "clang"} {
		if _, err := exec.LookPath(cand); err == nil {
			cc = cand
			break
		}
	}
	if cc == "" {
		t.Skip("no C compiler (cc/gcc/clang) in PATH")
	}
	dir := t.TempDir()
	f := dir + "/main.c"
	bin := dir + "/main"
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	def := "-DRUNE_GC_THRESHOLD=" + strconv.Itoa(thresholdBytes)
	if out, err := exec.Command(cc, def, "-DRUNE_GC_STATS", "-o", bin, f).CombinedOutput(); err != nil {
		t.Fatalf("%s: %v\n%s\n--- emitted ---\n%s", cc, err, out, src)
	}
	cmd := exec.Command(bin)
	var stdout, stderr strings.Builder
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("run: %v\nstderr=%s", err, stderr.String())
	}
	// parse "rune-c: gc collections=N" from stderr
	n := 0
	if i := strings.Index(stderr.String(), "collections="); i >= 0 {
		fmt.Sscanf(stderr.String()[i:], "collections=%d", &n)
	}
	return strings.TrimSpace(stdout.String()), n
}

// TestCGCForcesCollection is the GC SOUNDNESS gate (telos-2 / M4). It builds a
// long boxed list (each cons/succ is a heap object) and folds it back, under a
// TINY GC threshold so the mark-sweep collector fires repeatedly DURING both
// construction and folding. A correct result proves the collector preserved every
// reachable survivor across many collections (conservative roots find the live
// list on the stack; precise slot tracing keeps its spine + elements alive) and
// never freed a reachable object. The same program with an effectively-infinite
// threshold (no collection) must give the IDENTICAL answer.
func TestCGCForcesCollection(t *testing.T) {
	// replicate N builds [zero, zero, ...] of length N via NatElim; length folds it
	// back to N. zero/succ/cons are all BOXED (no builtin-nat here), so the run
	// allocates the succ^N nat result + N cons cells + the eliminator closures —
	// many thousands of objects for N in the dozens.
	src := natSrc + `
data List : U -> U is
  nil : (A : U) -> List A
| cons : (A : U) -> A -> List A -> List A
end
replicate : Nat -> List Nat is
  fn (n : Nat) is
    NatElim (fn (x : Nat) is List Nat end)
      (nil Nat)
      (fn (k : Nat) (ih : List Nat) is cons Nat zero ih end)
      n
  end
end
length : (A : U) -> List A -> Nat is
  fn (A : U) (xs : List A) is
    ListElim A (fn (x : List A) is Nat end)
      zero
      (fn (x : A) (rest : List A) (ih : Nat) is succ ih end)
      xs
  end
end
two : Nat is succ (succ zero) end
four : Nat is add two two end
eight : Nat is add four four end
big : Nat is length Nat (replicate eight) end
`
	// big = length(replicate 8) = succ^8 zero. The magnitude is kept modest (the
	// C backend's eliminator recursion is not tail-call-optimized, so a very deep
	// nat blows the native stack — unrelated to GC); the heap PRESSURE that forces
	// collections comes from the tiny threshold below, not the list length. Each
	// zero/succ/cons/closure is a boxed object, so even N=8 allocates hundreds of
	// objects and a 1 KiB heap collects many times.
	want := succChain(8)
	emitted := emitWith(t, codegen.C{}, src, "big")
	// 1 KiB threshold => collection fires many times during the run.
	got, ncol := runCGC(t, emitted, 1024)
	if got != want {
		t.Fatalf("c gc-stress (tiny heap): got %q, want %q", got, want)
	}
	if ncol == 0 {
		t.Fatalf("c gc-stress: expected the collector to FIRE under a 4 KiB heap, but it never collected")
	}
	// And byte-identical with an effectively-infinite threshold (no collection).
	got2, ncol2 := runCGC(t, emitted, 1<<30)
	if got2 != want {
		t.Fatalf("c gc-stress (no collection): got %q, want %q", got2, want)
	}
	if ncol2 != 0 {
		t.Fatalf("c gc-stress: did not expect collections under a 1 GiB heap, got %d", ncol2)
	}
}

// TestCGCConformanceUnderTinyHeap is the "corpus byte-identical WITH the GC
// actively collecting" gate. It runs several conformance-shaped programs (nat
// arithmetic, list length, a constructor with multiple fields) under a TINY heap
// (so the mark-sweep collector fires during each run) and requires the SAME
// output as the default (no-collection) compile — proving the GC is transparent.
func TestCGCConformanceUnderTinyHeap(t *testing.T) {
	cases := []struct{ src, main string }{
		{natSrc + `three : Nat is add (succ zero) (succ (succ zero)) end`, "three"},
		{natSrc + `
data List : U -> U is
  nil : (A : U) -> List A
| cons : (A : U) -> A -> List A -> List A
end
length : (A : U) -> List A -> Nat is
  fn (A : U) (xs : List A) is
    ListElim A (fn (x : List A) is Nat end)
      zero
      (fn (x : A) (rest : List A) (ih : Nat) is succ ih end)
      xs
  end
end
two : Nat is length Nat (cons Nat zero (cons Nat (succ zero) (nil Nat))) end
`, "two"},
		{natSrc + `
data Pairing : U -> U -> U is
  mk : (A : U) -> (B : U) -> A -> B -> Pairing A B
end
p : Pairing Nat Nat is mk Nat Nat (succ zero) (succ (succ zero)) end
`, "p"},
	}
	for _, tc := range cases {
		emitted := emitWith(t, codegen.C{}, tc.src, tc.main)
		// default (effectively no-collection) reference output
		ref := runC(t, emitted)
		// same program under a tiny heap so the collector fires
		got, ncol := runCGC(t, emitted, 1024)
		if got != ref {
			t.Fatalf("c gc-conformance %q: tiny-heap output %q != reference %q", tc.main, got, ref)
		}
		_ = ncol // collection may or may not fire per program; the byte-identity is the gate
	}
}

// succChain renders succ^n zero in the surface $show style (n nested succ's).
func succChain(n int) string {
	if n == 0 {
		return "zero"
	}
	var b strings.Builder
	for i := 0; i < n; i++ {
		b.WriteString("succ ")
		if i < n-1 {
			b.WriteString("(")
		}
	}
	b.WriteString("zero")
	for i := 0; i < n-1; i++ {
		b.WriteString(")")
	}
	return b.String()
}

// The Rust backend runs the same erased IR as JS: a recursion over an inductive
// list (length) computes the same payload, the eliminator's (arg, ih) shape and
// the curried Rc<dyn Fn> closures all wired correctly.
func TestRustEmitListLength(t *testing.T) {
	src := natSrc + `
data List : U -> U is
  nil : (A : U) -> List A
| cons : (A : U) -> A -> List A -> List A
end
length : (A : U) -> List A -> Nat is
  fn (A : U) (xs : List A) is
    ListElim A (fn (x : List A) is Nat end)
      zero
      (fn (x : A) (rest : List A) (ih : Nat) is succ ih end)
      xs
  end
end
two : Nat is length Nat (cons Nat zero (cons Nat (succ zero) (nil Nat))) end
`
	got := runRust(t, emitWith(t, codegen.Rust{}, src, "two"))
	if got != "succ (succ zero)" {
		t.Fatalf("got %q", got)
	}
}

// The JS eliminator is now lowered through the shared IR (B2 / R-IR LowerElim):
// the emitted ListElim is an ordinary definition whose body renders an ICase tag
// dispatch (a `switch`) with IField projections (`.args[`), and it still computes
// the recursive fold correctly — proving the lowering is faithful, JS = reference.
func TestEmitElimLoweredJS(t *testing.T) {
	src := natSrc + `
data List : U -> U is
  nil : (A : U) -> List A
| cons : (A : U) -> A -> List A -> List A
end
length : (A : U) -> List A -> Nat is
  fn (A : U) (xs : List A) is
    ListElim A (fn (x : List A) is Nat end)
      zero
      (fn (x : A) (rest : List A) (ih : Nat) is succ ih end)
      xs
  end
end
two : Nat is length Nat (cons Nat zero (cons Nat (succ zero) (nil Nat))) end
`
	js := emitJS(t, src, "two")
	if !strings.Contains(js, "switch") || !strings.Contains(js, ".args[") {
		t.Fatalf("lowered eliminator should render an ICase switch with IField projections:\n%s", js)
	}
	if got := runNode(t, js); got != "succ (succ zero)" {
		t.Fatalf("lowered ListElim gave %q", got)
	}
}

// The B2 / R-IR keystone is complete on ALL backends: every backend now renders the
// lowered eliminator (ICase tag dispatch + IField projections) rather than its own
// hand-written emitElim. A recursive (non-nat) ListElim must compute the same fold
// on js/py/go/rust — the per-backend emitElim deletions are sound.
func TestLowerElimRecursiveAllBackends(t *testing.T) {
	src := natSrc + `
data List : U -> U is
  nil : (A : U) -> List A
| cons : (A : U) -> A -> List A -> List A
end
length : (A : U) -> List A -> Nat is
  fn (A : U) (xs : List A) is
    ListElim A (fn (x : List A) is Nat end)
      zero
      (fn (x : A) (rest : List A) (ih : Nat) is succ ih end)
      xs
  end
end
two : Nat is length Nat (cons Nat zero (cons Nat (succ zero) (nil Nat))) end
`
	const want = "succ (succ zero)"
	if got := runNode(t, emitWith(t, codegen.JS{}, src, "two")); got != want {
		t.Fatalf("js: got %q", got)
	}
	if got := runRust(t, emitWith(t, codegen.Rust{}, src, "two")); got != want {
		t.Fatalf("rs: got %q", got)
	}
	for _, bk := range []struct {
		name, bin, ext string
		emit           codegen.Backend
		run            func(string) *exec.Cmd
	}{
		{"py", "python3", "py", codegen.Py{}, func(f string) *exec.Cmd { return exec.Command("python3", f) }},
		{"go", "go", "go", codegen.Go{}, func(f string) *exec.Cmd { return exec.Command("go", "run", f) }},
	} {
		if _, err := exec.LookPath(bk.bin); err != nil {
			t.Logf("%s not in PATH, skipping", bk.bin)
			continue
		}
		f := t.TempDir() + "/main." + bk.ext
		if err := os.WriteFile(f, []byte(emitWith(t, bk.emit, src, "two")), 0o644); err != nil {
			t.Fatal(err)
		}
		out, err := bk.run(f).CombinedOutput()
		if err != nil {
			t.Fatalf("%s: run failed: %v\n%s", bk.name, err, out)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Fatalf("%s: got %q, want %q", bk.name, got, want)
		}
	}
}

// bigNatSrc is a builtin-nat binding (so numerals lower to core.NatLit and the
// backends compile nat to native integers via NatSpec) plus the eliminator-defined
// add/mul. It is the C7 / R-NUM Decision-4 fixture: `big = 5000` is a LARGE closed
// numeral, `bigger = succ (succ 5000)` exercises succ-OF-a-literal (the
// representation-consistency gate — a literal and a hand-written succ must show the
// same), and `product = mul 100 100` is closed arithmetic over literals.
const bigNatSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
addN : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) n (fn (k : Nat) (ih : Nat) is succ ih end) m
  end
end
mulN : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is addN n ih end) m
  end
end
big : Nat is 5000 end
bigger : Nat is succ (succ 5000) end
product : Nat is mulN 100 100 end
`

// TestNatLitCompactEmission pins C7 / R-NUM Decision 4 with NO external toolchain:
// a large numeral erases to the backend's NATIVE integer in O(1) source, NOT the
// pre-C7 O(n) `succ^N zero` chain. Before this change `big = 5000` emitted a
// 5000-deep `succ(succ(...zero))` that crashes the host parser; now it is a single
// native literal. The check is per-backend because each NatSpec representation
// differs (JS BigInt, Python/BEAM arbitrary int, Go/Rust/JVM machine int).
func TestNatLitCompactEmission(t *testing.T) {
	cases := []struct {
		name string
		bk   codegen.Backend
		want string // the native literal the big numeral must deploy as
	}{
		{"js", codegen.JS{}, "5000n"},
		{"py", codegen.Py{}, "5000"},
		{"go", codegen.Go{}, "any(5000)"},
		{"rs", codegen.Rust{}, "V::Int(5000)"},
		{"jvm", codegen.JVM{}, "new VInt(5000L)"},
		{"erl", codegen.Beam{}, "5000"},
	}
	for _, c := range cases {
		c := c
		t.Run(c.name, func(t *testing.T) {
			src := emitWith(t, c.bk, bigNatSrc, "big")
			if !strings.Contains(src, c.want) {
				t.Fatalf("%s: emitted source missing native literal %q (NatLit not lowered to native int):\n%s",
					c.name, c.want, src)
			}
			// The literal's DEFINITION must not be a succ-chain. The succ helper is
			// emitted once in the NatSpec preamble; the `big` definition body must
			// carry the native literal alone, no nested succ applications. (We scan
			// the line that BINDS `big` and carries its native literal — distinct
			// from `bigger`, which legitimately conses two succ over the literal.)
			for _, line := range strings.Split(src, "\n") {
				if !bindsBigDef(line, c.want) {
					continue
				}
				if strings.Count(line, "succ") > 0 {
					t.Fatalf("%s: `big` definition still emits a succ-chain (O(n) source):\n%s", c.name, line)
				}
			}
		})
	}
}

// bindsBigDef reports whether a line is the emitted definition of `big` (the
// large-numeral def, NOT `bigger`): it carries the native literal `lit` and binds
// the name `big` — the emitted binding is `big`/`big_d`/`big()`, never `bigger…`.
func bindsBigDef(line, lit string) bool {
	return strings.Contains(line, lit) && strings.Contains(line, "big") && !strings.Contains(line, "bigger")
}

// TestNatLitCrossBackendRuns is the C7 / R-NUM Decision-4 RUNTIME conformance gate:
// the same program with a large literal, succ-of-a-literal, and closed literal
// arithmetic produces byte-identical `$show` output on EVERY runnable backend. This
// is the soundness gate — a NatLit erased to a native integer must be observationally
// equal to the succ-chain it replaced (`big` shows as the digit, `succ (succ 5000)`
// shows as 5002 NOT differently, `mul 100 100` shows as 10000).
func TestNatLitCrossBackendRuns(t *testing.T) {
	cases := []struct{ main, want string }{
		{"big", "5000"},      // a large closed numeral
		{"bigger", "5002"},   // succ (succ 5000): rep-consistency of succ-over-literal
		{"product", "10000"}, // mul 100 100: closed literal arithmetic
	}
	type backend struct {
		name, bin, ext string
		emit           codegen.Backend
		run            func(string) *exec.Cmd
		compile        func(src, out string) *exec.Cmd // nil for interpreted backends
	}
	backends := []backend{
		{"js", "node", "js", codegen.JS{}, func(f string) *exec.Cmd { return exec.Command("node", f) }, nil},
		{"py", "python3", "py", codegen.Py{}, func(f string) *exec.Cmd { return exec.Command("python3", f) }, nil},
		{"go", "go", "go", codegen.Go{}, func(f string) *exec.Cmd { return exec.Command("go", "run", f) }, nil},
		{"rs", "rustc", "rs", codegen.Rust{},
			func(bin string) *exec.Cmd { return exec.Command(bin) },
			func(src, out string) *exec.Cmd { return exec.Command("rustc", "--edition", "2021", "-o", out, src) }},
		{"erl", "escript", "erl", codegen.Beam{}, func(f string) *exec.Cmd { return exec.Command("escript", f) }, nil},
	}
	// JVM (Java 25+): the NatSpec representation is a VInt(long); resolve a JDK 25
	// toolchain explicitly (PATH java may be older) and run main from the class dir.
	if javac25, java25, ok := findJDK25(); ok {
		backends = append(backends, backend{"jvm", javac25, "java", codegen.JVM{},
			func(out string) *exec.Cmd {
				return exec.Command(java25, "-cp", filepathDir(out), "main")
			},
			func(src, out string) *exec.Cmd {
				return exec.Command(javac25, "--release", "25", "-d", filepathDir(out), src)
			}})
	}
	for _, bk := range backends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			for _, tc := range cases {
				src := emitWith(t, bk.emit, bigNatSrc, tc.main)
				dir := t.TempDir()
				f := dir + "/main." + bk.ext
				if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
					t.Fatal(err)
				}
				runFile := f
				if bk.compile != nil {
					bin := dir + "/main.bin"
					if out, err := bk.compile(f, bin).CombinedOutput(); err != nil {
						t.Fatalf("[%s] %s: compile failed: %v\n%s", bk.name, tc.main, err, out)
					}
					runFile = bin
				}
				out, err := bk.run(runFile).Output()
				if err != nil {
					stderr := ""
					if ee, ok := err.(*exec.ExitError); ok {
						stderr = string(ee.Stderr)
					}
					t.Fatalf("[%s] %s: run failed: %v\n%s", bk.name, tc.main, err, stderr)
				}
				if got := strings.TrimSpace(string(out)); got != tc.want {
					t.Fatalf("[%s] %s: gave %q, want %q (NatLit must show identical to the succ-chain)",
						bk.name, tc.main, got, tc.want)
				}
			}
		})
	}
}

// accelNatSrc registers the C7 / R-NUM Decision-1 acceleration ops on top of a
// builtin-nat binding: `builtin natAdd addN` / `natMul mulN` / `natMonus monusN`
// tag each eliminator-defined op's hash so codegen emits a 2-arg call as the
// host's NATIVE arithmetic (Decision 4) instead of unfolding the NatElim loop.
// The registration is itself gated by the session's differential soundness check
// (the def's peeled result must equal the integer op), so reaching emit means the
// native op is sound. sum/prod/diff/diffZero exercise add/mul/monus on values
// (incl. the monus 0-floor: `monusN 3 5 = 0`, never negative).
const accelNatSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
addN : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) n (fn (k : Nat) (ih : Nat) is succ ih end) m
  end
end
mulN : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is addN n ih end) m
  end
end
predN : Nat -> Nat is
  fn (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is k end) n
  end
end
monusN : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) m (fn (k : Nat) (ih : Nat) is predN ih end) n
  end
end
builtin natAdd addN
builtin natMul mulN
builtin natMonus monusN
sum : Nat is addN 5000 3000 end
prod : Nat is mulN 100 100 end
diff : Nat is monusN 8 3 end
diffZero : Nat is monusN 3 5 end
`

// TestAccelNativeEmission pins C7 / R-NUM Decision 4 (the accel-op native emit)
// with NO external toolchain: a 2-arg call to a registered natAdd/natMul/natMonus
// def emits the host's NATIVE op, NOT an unfolded `_natElim` loop over the operand.
// The check is per-backend because the native op spelling differs (BigInt `+` on
// js, runtime helper calls on go/rust/jvm, `max(..,0)` for erl monus). The
// negative gate (no `_natElim`/`nat_elim` reference in the accel def's body line)
// is what distinguishes Decision 4 from the pre-accel loop deployment.
func TestAccelNativeEmission(t *testing.T) {
	cases := []struct {
		name     string
		bk       codegen.Backend
		mainName string
		want     string // a substring the native-op emission must contain
		noElim   string // a substring (the eliminator-loop call) it must NOT contain on the def line
	}{
		{"js-add", codegen.JS{}, "sum", "(5000n + 3000n)", "_natElim"},
		{"js-mul", codegen.JS{}, "prod", "(100n * 100n)", "_natElim"},
		{"js-monus", codegen.JS{}, "diff", "8n", "_natElim"}, // saturating ternary, BigInt operands
		{"py-add", codegen.Py{}, "sum", "(5000 + 3000)", "_natElim"},
		{"py-mul", codegen.Py{}, "prod", "(100 * 100)", "_natElim"},
		{"go-add", codegen.Go{}, "sum", "_natAdd(any(5000), any(3000))", "_natElim"},
		{"go-mul", codegen.Go{}, "prod", "_natMul(any(100), any(100))", "_natElim"},
		{"go-monus", codegen.Go{}, "diff", "_natMonus(", "_natElim"},
		{"rs-add", codegen.Rust{}, "sum", "_nat_add(", "_nat_elim"},
		{"rs-monus", codegen.Rust{}, "diff", "_nat_monus(", "_nat_elim"},
		{"jvm-add", codegen.JVM{}, "sum", "_nat_add(", "_nat_elim"},
		{"jvm-monus", codegen.JVM{}, "diff", "_nat_monus(", "_nat_elim"},
		{"erl-add", codegen.Beam{}, "sum", "(5000 + 3000)", "nat_elim"},
		{"erl-monus", codegen.Beam{}, "diff", "max((8) - (3), 0)", "nat_elim"},
	}
	for _, c := range cases {
		c := c
		t.Run(c.name, func(t *testing.T) {
			src := emitWith(t, c.bk, accelNatSrc, c.mainName)
			if !strings.Contains(src, c.want) {
				t.Fatalf("%s: emitted source missing native accel op %q:\n%s", c.name, c.want, src)
			}
			// The MAIN def's binding line must not unfold the eliminator loop —
			// it must be the native op alone.
			for _, line := range strings.Split(src, "\n") {
				if strings.Contains(line, c.want) && strings.Contains(line, c.noElim) {
					t.Fatalf("%s: accel def still unfolds the %q loop instead of the native op:\n%s",
						c.name, c.noElim, line)
				}
			}
		})
	}
}

// TestAccelCrossBackendRuns is the C7 / R-NUM Decision-4 RUNTIME conformance gate:
// add/mul/monus on values, emitted as native ops, produce byte-identical `$show`
// output on EVERY runnable backend. This is the soundness gate — the native op
// MUST agree with the NatElim-loop result the accel replaces (`addN 5000 3000 =
// 8000`, `mulN 100 100 = 10000`, `monusN 8 3 = 5`, and the 0-floor `monusN 3 5 =
// 0`, never negative).
func TestAccelCrossBackendRuns(t *testing.T) {
	cases := []struct{ main, want string }{
		{"sum", "8000"},
		{"prod", "10000"},
		{"diff", "5"},
		{"diffZero", "0"}, // Peano monus floors at 0 (never negative)
	}
	type backend struct {
		name, bin, ext string
		emit           codegen.Backend
		run            func(string) *exec.Cmd
		compile        func(src, out string) *exec.Cmd
	}
	backends := []backend{
		{"js", "node", "js", codegen.JS{}, func(f string) *exec.Cmd { return exec.Command("node", f) }, nil},
		{"py", "python3", "py", codegen.Py{}, func(f string) *exec.Cmd { return exec.Command("python3", f) }, nil},
		{"go", "go", "go", codegen.Go{}, func(f string) *exec.Cmd { return exec.Command("go", "run", f) }, nil},
		{"rs", "rustc", "rs", codegen.Rust{},
			func(bin string) *exec.Cmd { return exec.Command(bin) },
			func(src, out string) *exec.Cmd { return exec.Command("rustc", "--edition", "2021", "-o", out, src) }},
		{"erl", "escript", "erl", codegen.Beam{}, func(f string) *exec.Cmd { return exec.Command("escript", f) }, nil},
	}
	if javac25, java25, ok := findJDK25(); ok {
		backends = append(backends, backend{"jvm", javac25, "java", codegen.JVM{},
			func(out string) *exec.Cmd { return exec.Command(java25, "-cp", filepathDir(out), "main") },
			func(src, out string) *exec.Cmd {
				return exec.Command(javac25, "--release", "25", "-d", filepathDir(out), src)
			}})
	}
	for _, bk := range backends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			for _, tc := range cases {
				src := emitWith(t, bk.emit, accelNatSrc, tc.main)
				dir := t.TempDir()
				f := dir + "/main." + bk.ext
				if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
					t.Fatal(err)
				}
				runFile := f
				if bk.compile != nil {
					bin := dir + "/main.bin"
					if out, err := bk.compile(f, bin).CombinedOutput(); err != nil {
						t.Fatalf("[%s] %s: compile failed: %v\n%s", bk.name, tc.main, err, out)
					}
					runFile = bin
				}
				out, err := bk.run(runFile).Output()
				if err != nil {
					stderr := ""
					if ee, ok := err.(*exec.ExitError); ok {
						stderr = string(ee.Stderr)
					}
					t.Fatalf("[%s] %s: run failed: %v\n%s", bk.name, tc.main, err, stderr)
				}
				if got := strings.TrimSpace(string(out)); got != tc.want {
					t.Fatalf("[%s] %s: gave %q, want %q (native accel op must agree with the NatElim loop)",
						bk.name, tc.main, got, tc.want)
				}
			}
		})
	}
}

// emitJS loads source and emits the program with the given main.
func emitJS(t *testing.T, src, main string) string {
	t.Helper()
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load: %v", err)
	}
	p, err := s.EmitProgram(main)
	if err != nil {
		t.Fatal(err)
	}
	out, err := codegen.Default().Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	return string(out)
}

// runNode executes emitted JS and returns its stdout, skipping when node is
// not installed.
func runNode(t *testing.T, js string) string {
	t.Helper()
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not in PATH")
	}
	f, err := os.CreateTemp(t.TempDir(), "*.js")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := f.WriteString(js); err != nil {
		t.Fatal(err)
	}
	f.Close()
	out, err := exec.Command("node", f.Name()).CombinedOutput()
	if err != nil {
		t.Fatalf("node: %v\n%s\n--- emitted ---\n%s", err, out, js)
	}
	return strings.TrimSpace(string(out))
}

const natSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
add : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) n (fn (k : Nat) (ih : Nat) is succ ih end) m
  end
end
`

func TestEmitAndRunNat(t *testing.T) {
	src := natSrc + `three : Nat is add (succ zero) (succ (succ zero)) end`
	got := runNode(t, emitJS(t, src, "three"))
	if got != "succ (succ (succ zero))" {
		t.Fatalf("got %q", got)
	}
}

func TestEmitListLength(t *testing.T) {
	src := natSrc + `
data List : U -> U is
  nil : (A : U) -> List A
| cons : (A : U) -> A -> List A -> List A
end
length : (A : U) -> List A -> Nat is
  fn (A : U) (xs : List A) is
    ListElim A (fn (x : List A) is Nat end)
      zero
      (fn (x : A) (rest : List A) (ih : Nat) is succ ih end)
      xs
  end
end
two : Nat is length Nat (cons Nat zero (cons Nat (succ zero) (nil Nat))) end
`
	got := runNode(t, emitJS(t, src, "two"))
	if got != "succ (succ zero)" {
		t.Fatalf("got %q", got)
	}
}

// Proofs, casts, and transports erase: the emitted program never mentions
// equality machinery and still computes its payload.
func TestErasureDropsProofs(t *testing.T) {
	src := natSrc + `
lemma : (n : Nat) -> Eq Nat n n is fn (n : Nat) is refl n end end
carried : Nat -> Nat is
  fn (n : Nat) is
    subst Nat n n (lemma n) (fn (z : Nat) is Nat end) (succ n)
  end
end
one : Nat is carried zero end
`
	js := emitJS(t, src, "one")
	if strings.Contains(js, "Eq") || strings.Contains(js, "refl") {
		t.Fatalf("equality machinery leaked into the shadow:\n%s", js)
	}
	got := runNode(t, js)
	if got != "succ zero" {
		t.Fatalf("got %q", got)
	}
}

// The 0-fragment receives units: erased binders cost a null at call sites,
// not an arity change — and the value still computes.
func TestErasedQuantityBecomesUnit(t *testing.T) {
	src := natSrc + `
constN : (0 A : U1) -> Nat -> Nat is
  fn (0 A : U1) (n : Nat) is succ n end
end
one : Nat is constN U zero end
`
	got := runNode(t, emitJS(t, src, "one"))
	if got != "succ zero" {
		t.Fatalf("got %q", got)
	}
}

// A8 deploy (R-ERASE2): a `fsplitD` (the dependent face split) on a ⊤ face computes
// to its branch value, so a definition using it has a taint-free normal form and
// DEPLOYS — the shadow must contain the computed value, never a dangling `fsplitD`
// runtime reference. (Before fsplitD/sysU/pappU joined the inner-taint set, such a
// body would have emitted a broken reference instead of deploying via its normal
// form.)
func TestEmitFsplitDComputesAway(t *testing.T) {
	src := `
data Bool : U is true : Bool | false : Bool end
pickD : Bool is
  fsplitD ftop fbot (fn (h : holds (for ftop fbot)) is fib Bool end)
          (fn (hp : holds ftop) is true end) (fn (hq : holds fbot) is false end) htop
end`
	js := emitJS(t, src, "pickD")
	if strings.Contains(js, "fsplitD") {
		t.Fatalf("a fsplitD that computes away must not leak into the shadow:\n%s", js)
	}
	if got := runNode(t, js); got != "true" {
		t.Fatalf("pickD must run to true, got %q", got)
	}
}
