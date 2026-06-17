package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// Cross-backend FFI conformance (R-FFI / B4 × the multi-backend story): a
// `foreign` axiom erases to the uniform IForeign accessor `<name>()` on EVERY
// backend, so the same host implementation links and runs identically across
// JS, Python, Go, and Rust. ch44's `answer = hostId (succ (succ zero))` with the
// host `hostId` bound to the identity must observe "succ (succ zero)" everywhere.
func TestForeignConformance(t *testing.T) {
	s := loadListing(t, "ch44_ffi_run.rune")
	p, err := s.EmitProgram("answer")
	if err != nil {
		t.Fatal(err)
	}
	const want = "succ (succ zero)"
	backends := []struct {
		name    string
		bin     string
		ext     string
		emit    func(codegen.Program) (codegen.TargetSource, error)
		link    func(src string) string // inject the host `hostId` accessor
		run     func(file string) *exec.Cmd
		compile func(src, out string) *exec.Cmd
		// runtime, when non-nil, is a C runtime shim written beside the emitted
		// source as `runtime.c` (same temp dir) and linked by `compile`. The native
		// backends carry their tagged-word Value rep + GC here; for the FFI gate it
		// ALSO carries the host `hostId` accessor (the LLVM `.ll` calls an external
		// `@hostId`, so the impl must live in the linked C, not the emitted IR).
		runtime func() string
	}{
		{"js", "node", "js", codegen.JS{}.Emit,
			func(src string) string { return "function hostId(){ return a => a; }\n" + src },
			func(f string) *exec.Cmd { return exec.Command("node", f) }, nil, nil},
		{"py", "python3", "py", codegen.Py{}.Emit,
			func(src string) string { return "def hostId():\n    return lambda a: a\n" + src },
			func(f string) *exec.Cmd { return exec.Command("python3", f) }, nil, nil},
		{"go", "go", "go", codegen.Go{}.Emit,
			func(src string) string { return src + "\nfunc hostId() any { return func(a any) any { return a } }\n" },
			func(f string) *exec.Cmd { return exec.Command("go", "run", f) }, nil, nil},
		{"rs", "rustc", "rs", codegen.Rust{}.Emit,
			func(src string) string {
				return src + "\nfn hostId() -> Rc<V> { vfun(|a: Rc<V>| -> Rc<V> { a.clone() }) }\n"
			},
			func(bin string) *exec.Cmd { return exec.Command(bin) },
			func(src, out string) *exec.Cmd { return exec.Command("rustc", "--edition", "2021", "-o", out, src) }, nil},
		{"erl", "escript", "erl", codegen.Beam{}.Emit,
			func(src string) string { return src + "\nff_hostId() -> fun(A) -> A end.\n" },
			func(f string) *exec.Cmd { return exec.Command("escript", f) }, nil, nil},
		// c: the FIRST NATIVE backend in the FFI gate. The `IForeign` lowers to a
		// call to `Value hostId(void)`; the host supplies it as an identity closure
		// built from the runtime's `mkclo` (static, same translation unit), injected
		// before the first definition thunk that references it. cc compiles the one
		// TU; the foreign call runs and `$show`s byte-identically to the six source
		// backends.
		{"c", "cc", "c", codegen.C{}.Emit,
			func(src string) string {
				shim := "/* FFI gate host accessor: hostId = identity, via the runtime's mkclo. */\n" +
					"static Value hostId_code(Value arg, Value* env) { (void)env; return arg; }\n" +
					"static Value hostId(void) { return mkclo(&hostId_code, 0); }\n"
				i := strings.Index(src, "static Value def_")
				return src[:i] + shim + src[i:]
			},
			func(bin string) *exec.Cmd { return exec.Command(bin) },
			func(src, out string) *exec.Cmd { return exec.Command("cc", "-o", out, src) }, nil},
		// ll: the SECOND NATIVE backend in the FFI gate. The emitted `.ll` now emits a
		// `declare i64 @hostId()` (foreignNames in codegen/ll.go) and calls it; the
		// host accessor — an identity closure over the external-linkage `rt_mkclo` —
		// is appended to the linked runtime.c (LL{}.EmitRuntime()). clang compiles
		// `program.ll runtime.c -o exe`; the foreign call runs, matching C and the
		// source backends.
		{"ll", "clang", "ll", codegen.LL{}.Emit,
			func(src string) string { return src },
			func(bin string) *exec.Cmd { return exec.Command(bin) },
			func(src, out string) *exec.Cmd {
				rt := filepath.Join(filepath.Dir(src), "runtime.c")
				return exec.Command("clang", src, rt, "-o", out)
			},
			func() string {
				return codegen.LL{}.EmitRuntime() +
					"\n/* FFI gate host accessor: hostId = identity, via the runtime's rt_mkclo. */\n" +
					"static Value hostId_code(Value arg, Value* env) { (void)env; return arg; }\n" +
					"Value hostId(void) { return rt_mkclo(&hostId_code, 0); }\n"
			}},
	}
	// JVM (Java 25+): the host `hostId` is a method of class `main`, so it is
	// inserted before the class-closing brace, not appended. Skips without a JDK 25.
	if javac25, java25, ok := findJava25(); ok {
		backends = append(backends, struct {
			name    string
			bin     string
			ext     string
			emit    func(codegen.Program) (codegen.TargetSource, error)
			link    func(src string) string
			run     func(file string) *exec.Cmd
			compile func(src, out string) *exec.Cmd
			runtime func() string
		}{"jvm", javac25, "java", codegen.JVM{}.Emit,
			func(src string) string {
				i := strings.LastIndex(src, "}")
				return src[:i] + "  static V hostId() { return fun(a -> a); }\n" + src[i:]
			},
			func(out string) *exec.Cmd { return exec.Command(java25, "-cp", filepath.Dir(out), "main") },
			func(src, out string) *exec.Cmd {
				return exec.Command(javac25, "--release", "25", "-d", filepath.Dir(out), src)
			}, nil})
	}
	for _, bk := range backends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			src, err := bk.emit(p)
			if err != nil {
				t.Fatal(err)
			}
			linked := bk.link(string(src))
			dir := t.TempDir()
			f := filepath.Join(dir, "main."+bk.ext)
			if err := os.WriteFile(f, []byte(linked), 0o644); err != nil {
				t.Fatal(err)
			}
			// Native backends that link a separate C runtime (the LLVM backend) write
			// it beside the emitted source as `runtime.c` (carrying the host accessor
			// for the FFI gate); compile links both.
			if bk.runtime != nil {
				if err := os.WriteFile(filepath.Join(dir, "runtime.c"), []byte(bk.runtime()), 0o644); err != nil {
					t.Fatal(err)
				}
			}
			runFile := f
			if bk.compile != nil {
				bin := filepath.Join(dir, "main.bin")
				if out, err := bk.compile(f, bin).CombinedOutput(); err != nil {
					t.Fatalf("[%s] compile failed: %v\n%s\n--- linked ---\n%s", bk.name, err, out, linked)
				}
				runFile = bin
			}
			// stdout only — escript prints compile warnings to stderr.
			out, err := bk.run(runFile).Output()
			if err != nil {
				stderr := ""
				if ee, ok := err.(*exec.ExitError); ok {
					stderr = string(ee.Stderr)
				}
				t.Fatalf("[%s] run failed: %v\n%s", bk.name, err, stderr)
			}
			if got := strings.TrimSpace(string(out)); got != want {
				t.Fatalf("[%s] foreign-linked run gave %q, want %q", bk.name, got, want)
			}
		})
	}
}

// Cross-backend MULTI-FOREIGN + NON-IDENTITY conformance (R-FFI / B4, the C-ABI FFI
// deepening past ch44's single identity foreign). ch197 links TWO distinct foreign
// symbols — `hostId` (unary identity) and `hostConst` (the binary first-projection
// `λa b. a`, genuinely non-identity and curried across the boundary) — and computes
// `hostConst (hostId (succ (succ zero))) zero`. The result must be `succ (succ zero)`
// on every backend: both accessors resolve, the curried foreign applies twice, and
// the second argument (`zero`) is correctly discarded. A dropped link, a missing
// second accessor, or a second-projection host would observably differ.
func TestForeignMultiConformance(t *testing.T) {
	s := loadListing(t, "ch197_ffi_multi.rune")
	p, err := s.EmitProgram("answer")
	if err != nil {
		t.Fatal(err)
	}
	const want = "succ (succ zero)"
	backends := []struct {
		name    string
		bin     string
		ext     string
		emit    func(codegen.Program) (codegen.TargetSource, error)
		link    func(src string) string // inject BOTH host accessors
		run     func(file string) *exec.Cmd
		compile func(src, out string) *exec.Cmd
		runtime func() string
	}{
		{"js", "node", "js", codegen.JS{}.Emit,
			func(src string) string {
				return "function hostId(){ return a => a; }\n" +
					"function hostConst(){ return a => b => a; }\n" + src
			},
			func(f string) *exec.Cmd { return exec.Command("node", f) }, nil, nil},
		{"py", "python3", "py", codegen.Py{}.Emit,
			func(src string) string {
				return "def hostId():\n    return lambda a: a\n" +
					"def hostConst():\n    return lambda a: lambda b: a\n" + src
			},
			func(f string) *exec.Cmd { return exec.Command("python3", f) }, nil, nil},
		{"go", "go", "go", codegen.Go{}.Emit,
			func(src string) string {
				return src + "\nfunc hostId() any { return func(a any) any { return a } }\n" +
					"func hostConst() any { return func(a any) any { return func(b any) any { return a } } }\n"
			},
			func(f string) *exec.Cmd { return exec.Command("go", "run", f) }, nil, nil},
		{"rs", "rustc", "rs", codegen.Rust{}.Emit,
			func(src string) string {
				return src + "\nfn hostId() -> Rc<V> { vfun(|a: Rc<V>| -> Rc<V> { a.clone() }) }\n" +
					"fn hostConst() -> Rc<V> { vfun(|a: Rc<V>| -> Rc<V> { let a2 = a.clone(); vfun(move |_b: Rc<V>| -> Rc<V> { a2.clone() }) }) }\n"
			},
			func(bin string) *exec.Cmd { return exec.Command(bin) },
			func(src, out string) *exec.Cmd { return exec.Command("rustc", "--edition", "2021", "-o", out, src) }, nil},
		{"erl", "escript", "erl", codegen.Beam{}.Emit,
			func(src string) string {
				return src + "\nff_hostId() -> fun(A) -> A end.\n" +
					"ff_hostConst() -> fun(A) -> fun(_B) -> A end end.\n"
			},
			func(f string) *exec.Cmd { return exec.Command("escript", f) }, nil, nil},
		{"c", "cc", "c", codegen.C{}.Emit,
			func(src string) string {
				shim := "/* FFI gate host accessors: hostId = identity, hostConst = first projection. */\n" +
					"static Value hostId_code(Value arg, Value* env) { (void)env; return arg; }\n" +
					"static Value hostId(void) { return mkclo(&hostId_code, 0); }\n" +
					"static Value hostConst_inner(Value b, Value* env) { (void)b; return env[0]; }\n" +
					"static Value hostConst_code(Value a, Value* env) { (void)env; Value c = mkclo(&hostConst_inner, 1); clo_set(c, 0, a); return c; }\n" +
					"static Value hostConst(void) { return mkclo(&hostConst_code, 0); }\n"
				i := strings.Index(src, "static Value def_")
				return src[:i] + shim + src[i:]
			},
			func(bin string) *exec.Cmd { return exec.Command(bin) },
			func(src, out string) *exec.Cmd { return exec.Command("cc", "-o", out, src) }, nil},
		{"ll", "clang", "ll", codegen.LL{}.Emit,
			func(src string) string { return src },
			func(bin string) *exec.Cmd { return exec.Command(bin) },
			func(src, out string) *exec.Cmd {
				rt := filepath.Join(filepath.Dir(src), "runtime.c")
				return exec.Command("clang", src, rt, "-o", out)
			},
			func() string {
				return codegen.LL{}.EmitRuntime() +
					"\n/* FFI gate host accessors: hostId = identity, hostConst = first projection. */\n" +
					"static Value hostId_code(Value arg, Value* env) { (void)env; return arg; }\n" +
					"Value hostId(void) { return rt_mkclo(&hostId_code, 0); }\n" +
					"static Value hostConst_inner(Value b, Value* env) { (void)b; return env[0]; }\n" +
					"static Value hostConst_code(Value a, Value* env) { (void)env; Value c = rt_mkclo(&hostConst_inner, 1); rt_clo_set(c, 0, a); return c; }\n" +
					"Value hostConst(void) { return rt_mkclo(&hostConst_code, 0); }\n"
			}},
	}
	if javac25, java25, ok := findJava25(); ok {
		backends = append(backends, struct {
			name    string
			bin     string
			ext     string
			emit    func(codegen.Program) (codegen.TargetSource, error)
			link    func(src string) string
			run     func(file string) *exec.Cmd
			compile func(src, out string) *exec.Cmd
			runtime func() string
		}{"jvm", javac25, "java", codegen.JVM{}.Emit,
			func(src string) string {
				i := strings.LastIndex(src, "}")
				return src[:i] + "  static V hostId() { return fun(a -> a); }\n" +
					"  static V hostConst() { return fun(a -> fun(b -> a)); }\n" + src[i:]
			},
			func(out string) *exec.Cmd { return exec.Command(java25, "-cp", filepath.Dir(out), "main") },
			func(src, out string) *exec.Cmd {
				return exec.Command(javac25, "--release", "25", "-d", filepath.Dir(out), src)
			}, nil})
	}
	for _, bk := range backends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			src, err := bk.emit(p)
			if err != nil {
				t.Fatal(err)
			}
			linked := bk.link(string(src))
			dir := t.TempDir()
			f := filepath.Join(dir, "main."+bk.ext)
			if err := os.WriteFile(f, []byte(linked), 0o644); err != nil {
				t.Fatal(err)
			}
			if bk.runtime != nil {
				if err := os.WriteFile(filepath.Join(dir, "runtime.c"), []byte(bk.runtime()), 0o644); err != nil {
					t.Fatal(err)
				}
			}
			runFile := f
			if bk.compile != nil {
				bin := filepath.Join(dir, "main.bin")
				if out, err := bk.compile(f, bin).CombinedOutput(); err != nil {
					t.Fatalf("[%s] compile failed: %v\n%s\n--- linked ---\n%s", bk.name, err, out, linked)
				}
				runFile = bin
			}
			out, err := bk.run(runFile).Output()
			if err != nil {
				stderr := ""
				if ee, ok := err.(*exec.ExitError); ok {
					stderr = string(ee.Stderr)
				}
				t.Fatalf("[%s] run failed: %v\n%s", bk.name, err, stderr)
			}
			if got := strings.TrimSpace(string(out)); got != want {
				t.Fatalf("[%s] multi-foreign run gave %q, want %q", bk.name, got, want)
			}
		})
	}
}
