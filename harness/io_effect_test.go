package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// Cross-backend side-effecting IO (R-EFFECT host op × R-FFI × multi-backend): a
// `foreign` IO axiom performs a real host effect (here: print "[io]") and returns
// its argument; sequenced under bindIO and forced as the IO main, it runs the
// effect THEN shows the result. The pure IO monad and the IForeign accessor both
// deploy on every backend, so the composed effectful program observes the same
// stdout — the marker line then "succ (succ zero)" — on js/py/go/rust.
func TestIOEffectConformance(t *testing.T) {
	s := loadListing(t, "ch59_io_effect.rune")
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	const want = "[io]\nsucc (succ zero)"
	// Host impls of `emitNat : Nat -> IO Nat`. JS encodes an IO thunk as a zero-arg
	// closure; py/go/rust encode it as a unit-argument closure (matching each
	// backend's pureIO/bindIO), so the host thunk is forced the same way the monad
	// forces its actions.
	const (
		jsHost  = "function emitNat(){ return n => () => { console.log(\"[io]\"); return n; }; }\n"
		pyHost  = "def emitNat():\n    def f(n):\n        def thunk(_u):\n            print(\"[io]\")\n            return n\n        return thunk\n    return f\n"
		goHost  = "\nfunc emitNat() any { return func(n any) any { return func(_u any) any { fmt.Println(\"[io]\"); return n } } }\n"
		rsHost  = "\nfn emitNat() -> Rc<V> { vfun(|n: Rc<V>| -> Rc<V> { let n2 = n.clone(); vfun(move |_u: Rc<V>| -> Rc<V> { println!(\"[io]\"); n2.clone() }) }) }\n"
		erlHost = "\nff_emitNat() -> fun(N) -> fun(_U) -> io:format(\"[io]~n\", []), N end end.\n"
	)
	backends := []struct {
		name    string
		bin     string
		ext     string
		emit    func(codegen.Program) (codegen.TargetSource, error)
		link    func(src string) string
		run     func(file string) *exec.Cmd
		compile func(src, out string) *exec.Cmd
	}{
		{"js", "node", "js", codegen.JS{}.Emit,
			func(src string) string { return jsHost + src },
			func(f string) *exec.Cmd { return exec.Command("node", f) }, nil},
		{"py", "python3", "py", codegen.Py{}.Emit,
			func(src string) string { return pyHost + src },
			func(f string) *exec.Cmd { return exec.Command("python3", f) }, nil},
		{"go", "go", "go", codegen.Go{}.Emit,
			func(src string) string { return src + goHost },
			func(f string) *exec.Cmd { return exec.Command("go", "run", f) }, nil},
		{"rs", "rustc", "rs", codegen.Rust{}.Emit,
			func(src string) string { return src + rsHost },
			func(bin string) *exec.Cmd { return exec.Command(bin) },
			func(src, out string) *exec.Cmd { return exec.Command("rustc", "--edition", "2021", "-o", out, src) }},
		{"erl", "escript", "erl", codegen.Beam{}.Emit,
			func(src string) string { return src + erlHost },
			func(f string) *exec.Cmd { return exec.Command("escript", f) }, nil},
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
			runFile := f
			if bk.compile != nil {
				bin := filepath.Join(dir, "main.bin")
				if out, err := bk.compile(f, bin).CombinedOutput(); err != nil {
					t.Fatalf("[%s] compile failed: %v\n%s\n--- linked ---\n%s", bk.name, err, out, linked)
				}
				runFile = bin
			}
			// stdout only — the program's observable output (escript warns on stderr).
			out, err := bk.run(runFile).Output()
			if err != nil {
				stderr := ""
				if ee, ok := err.(*exec.ExitError); ok {
					stderr = string(ee.Stderr)
				}
				t.Fatalf("[%s] run failed: %v\n%s", bk.name, err, stderr)
			}
			if got := strings.TrimSpace(string(out)); got != want {
				t.Fatalf("[%s] effectful run gave %q, want %q", bk.name, got, want)
			}
		})
	}
}
