package codegen_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// B4 FFI marshalling: a native host literal (scalar / String) crosses the C-ABI
// boundary AS the host's own representation, not as a unary Peano record, and is
// observably IDENTICAL on every backend. The IR carries the marshalled value in an
// ILit node; each backend renders it natively and `$show` prints it natively. This
// is the cross-backend conformance gate for the marshalling primitive (the lowering
// from a surface literal is owned upstream; here we pin the codegen boundary).
//
// The program is built directly on the erased IR (no surface): `main = id <lit>`,
// where `id` is a host-linked `foreign` (the C-ABI boundary) — so the literal is
// MARSHALLED INTO the foreign call and the result MARSHALLED BACK OUT, exactly the
// scalar/String round trip B4 exists to make observable.
func TestFFIMarshalLiteralConformance(t *testing.T) {
	cases := []struct {
		name string
		lit  codegen.Ir
		want string
	}{
		{"int", codegen.ILit{Kind: codegen.LitInt, Int: 42}, "42"},
		{"negint", codegen.ILit{Kind: codegen.LitInt, Int: -7}, "-7"},
		{"str", codegen.ILit{Kind: codegen.LitStr, Str: "hi"}, "hi"},
		{"strspace", codegen.ILit{Kind: codegen.LitStr, Str: "a b"}, "a b"},
		// An opaque host pointer survives the round trip but is NEVER shown
		// structurally — every backend boxes it and `$show` prints the same fixed
		// marker (not its handle), so a foreign that hands back a host object is
		// observably opaque and identical cross-backend.
		{"ptr", codegen.ILit{Kind: codegen.LitPtr, Int: 0xBEEF}, "<ptr>"},
	}

	// id is a `foreign` axiom (the host links the identity accessor). main applies
	// it to the native literal, so the value is marshalled across the boundary.
	mkProg := func(lit codegen.Ir) codegen.Program {
		return codegen.Program{
			Defs: []codegen.DefSpec{{
				Name: "main",
				Body: codegen.IApp{Fn: codegen.IForeign{Name: "id"}, Arg: lit},
			}},
			Main: "main",
		}
	}

	// Each backend: emit, link a host `id = identity`, compile/run, compare stdout.
	type backend struct {
		name, bin, ext string
		emit           func(codegen.Program) (codegen.TargetSource, error)
		link           func(src string) string
		run            func(file string) *exec.Cmd
		compile        func(src, out string) *exec.Cmd
		// runtime, when non-nil, is written beside the emitted source as `runtime.c`
		// (LLVM links it). For this marshalling gate it carries the host `id`
		// accessor too, since the `.ll` calls an external `@id`.
		runtime func() string
	}
	backends := []backend{
		{"js", "node", "js", codegen.JS{}.Emit,
			func(src string) string { return "function id(){ return a => a; }\n" + src },
			func(f string) *exec.Cmd { return exec.Command("node", f) }, nil, nil},
		{"py", "python3", "py", codegen.Py{}.Emit,
			func(src string) string { return "def id():\n    return lambda a: a\n" + src },
			func(f string) *exec.Cmd { return exec.Command("python3", f) }, nil, nil},
		{"go", "go", "go", codegen.Go{}.Emit,
			func(src string) string { return src + "\nfunc id() any { return func(a any) any { return a } }\n" },
			func(f string) *exec.Cmd { return exec.Command("go", "run", f) }, nil, nil},
		{"rs", "rustc", "rs", codegen.Rust{}.Emit,
			func(src string) string {
				return src + "\nfn id() -> Rc<V> { vfun(|a: Rc<V>| -> Rc<V> { a.clone() }) }\n"
			},
			func(bin string) *exec.Cmd { return exec.Command(bin) },
			func(src, out string) *exec.Cmd { return exec.Command("rustc", "--edition", "2021", "-o", out, src) }, nil},
		{"erl", "escript", "erl", codegen.Beam{}.Emit,
			func(src string) string { return src + "\nff_id() -> fun(A) -> A end.\n" },
			func(f string) *exec.Cmd { return exec.Command("escript", f) }, nil, nil},
		// c / ll: the two NATIVE backends marshal LitInt/LitStr/LitPtr across the FFI
		// boundary byte-identically to the source backends. C injects the host `id`
		// identity closure (built from the runtime's `mkclo`) before the first def
		// thunk; LL emits a `declare i64 @id()` (foreignNames) and links the accessor
		// in the appended runtime.c (`rt_mkclo`).
		{"c", "cc", "c", codegen.C{}.Emit,
			func(src string) string {
				shim := "static Value id_code(Value arg, Value* env) { (void)env; return arg; }\n" +
					"static Value id(void) { return mkclo(&id_code, 0); }\n"
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
					"\nstatic Value id_code(Value arg, Value* env) { (void)env; return arg; }\n" +
					"Value id(void) { return rt_mkclo(&id_code, 0); }\n"
			}},
	}

	for _, c := range cases {
		c := c
		t.Run(c.name, func(t *testing.T) {
			p := mkProg(c.lit)
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
					if got := strings.TrimSpace(string(out)); got != c.want {
						t.Fatalf("[%s] marshalled literal gave %q, want %q", bk.name, got, c.want)
					}
				})
			}
		})
	}
}

// The emitter renders ILit natively (not as a Peano record or a unit): a quick
// pure-string check that does not need any external toolchain, so it runs in every
// environment and pins the rendered shape per backend.
func TestFFIMarshalLiteralEmitsNative(t *testing.T) {
	prog := func(lit codegen.Ir) codegen.Program {
		return codegen.Program{Defs: []codegen.DefSpec{{Name: "x", Body: lit}}}
	}
	cases := []struct {
		name    string
		lit     codegen.Ir
		bk      codegen.Backend
		want    string // substring the emitted source must contain
		notWant string // substring it must NOT contain
	}{
		{"js-int", codegen.ILit{Kind: codegen.LitInt, Int: 5}, codegen.JS{}, "= 5;", "tag"},
		{"js-str", codegen.ILit{Kind: codegen.LitStr, Str: "yo"}, codegen.JS{}, `"yo"`, "tag"},
		{"py-int", codegen.ILit{Kind: codegen.LitInt, Int: 5}, codegen.Py{}, "x = 5\n", ""},
		{"go-str", codegen.ILit{Kind: codegen.LitStr, Str: "yo"}, codegen.Go{}, `any("yo")`, ""},
		{"rs-str", codegen.ILit{Kind: codegen.LitStr, Str: "yo"}, codegen.Rust{}, "V::Str(", ""},
		{"jvm-str", codegen.ILit{Kind: codegen.LitStr, Str: "yo"}, codegen.JVM{}, "new VStr(", ""},
		{"erl-str", codegen.ILit{Kind: codegen.LitStr, Str: "yo"}, codegen.Beam{}, `<<"yo"/utf8>>`, ""},
		// An opaque pointer is BOXED, never a bare scalar: the emitted source must
		// carry the dedicated boxed form and must NOT contain a bare handle literal
		// (e.g. `= 99;`), so the handle can never leak as a transparent int.
		{"js-ptr", codegen.ILit{Kind: codegen.LitPtr, Int: 99}, codegen.JS{}, "{ptr: 99}", "= 99;"},
		{"py-ptr", codegen.ILit{Kind: codegen.LitPtr, Int: 99}, codegen.Py{}, `{"ptr": 99}`, "x = 99\n"},
		{"go-ptr", codegen.ILit{Kind: codegen.LitPtr, Int: 99}, codegen.Go{}, "_ptr(99)", "any(99)"},
		{"rs-ptr", codegen.ILit{Kind: codegen.LitPtr, Int: 99}, codegen.Rust{}, "V::Ptr(99)", "V::Int(99)"},
		{"jvm-ptr", codegen.ILit{Kind: codegen.LitPtr, Int: 99}, codegen.JVM{}, "new VPtr(99L)", "new VInt(99L)"},
		{"erl-ptr", codegen.ILit{Kind: codegen.LitPtr, Int: 99}, codegen.Beam{}, "{ptr, 99}", ""},
	}
	for _, c := range cases {
		c := c
		t.Run(c.name, func(t *testing.T) {
			out, err := c.bk.Emit(prog(c.lit))
			if err != nil {
				t.Fatal(err)
			}
			s := string(out)
			if !strings.Contains(s, c.want) {
				t.Fatalf("emitted source missing %q:\n%s", c.want, s)
			}
			if c.notWant != "" && strings.Contains(s, c.notWant) {
				t.Fatalf("emitted source unexpectedly contains %q:\n%s", c.notWant, s)
			}
		})
	}
}
