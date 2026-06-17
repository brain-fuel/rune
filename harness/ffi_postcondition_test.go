package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// Verified FFI COMPOSITION (R-FFI / B4, guarantee tier): `trustedNonzero` returns a
// Σ of its result with a host-guaranteed nonzero proof; that proof discharges
// `guardedNat`'s precondition. Both proofs are erased; the value threads through. The
// composed program runs identically on every backend (Σ erases to the backend's pair;
// the trusted result echoes through `guardedNat`).
func TestFFIPostconditionComposes(t *testing.T) {
	s := loadListing(t, "ch61_ffi_postcondition.rune")
	p, err := s.EmitProgram("chained")
	if err != nil {
		t.Fatal(err)
	}
	const want = "succ zero"
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
			func(src string) string {
				return "function trustedNonzero(){ return n => [n, $unit]; }\nfunction guardedNat(){ return d => _pf => d; }\n" + src
			},
			func(f string) *exec.Cmd { return exec.Command("node", f) }, nil},
		{"py", "python3", "py", codegen.Py{}.Emit,
			func(src string) string {
				return "def trustedNonzero():\n    return lambda n: (n, _unit)\ndef guardedNat():\n    return lambda d: lambda _pf: d\n" + src
			},
			func(f string) *exec.Cmd { return exec.Command("python3", f) }, nil},
		{"go", "go", "go", codegen.Go{}.Emit,
			func(src string) string {
				return src + "\nfunc trustedNonzero() any { return func(n any) any { return []any{n, nil} } }\nfunc guardedNat() any { return func(d any) any { return func(_pf any) any { return d } } }\n"
			},
			func(f string) *exec.Cmd { return exec.Command("go", "run", f) }, nil},
		{"rs", "rustc", "rs", codegen.Rust{}.Emit,
			func(src string) string {
				return src + "\nfn trustedNonzero() -> Rc<V> { vfun(|n: Rc<V>| -> Rc<V> { Rc::new(V::Pair(n.clone(), unit())) }) }\nfn guardedNat() -> Rc<V> { vfun(|d: Rc<V>| -> Rc<V> { let d2 = d.clone(); vfun(move |_pf: Rc<V>| -> Rc<V> { d2.clone() }) }) }\n"
			},
			func(bin string) *exec.Cmd { return exec.Command(bin) },
			func(src, out string) *exec.Cmd { return exec.Command("rustc", "--edition", "2021", "-o", out, src) }},
		{"erl", "escript", "erl", codegen.Beam{}.Emit,
			func(src string) string {
				return src + "\nff_trustedNonzero() -> fun(N) -> {pair, N, unit} end.\nff_guardedNat() -> fun(D) -> fun(_Pf) -> D end end.\n"
			},
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
			if strings.Contains(string(src), "refl") || strings.Contains(string(src), "Eq") {
				t.Fatalf("[%s] contract proofs leaked into the shadow:\n%s", bk.name, src)
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
					t.Fatalf("[%s] compile failed: %v\n%s", bk.name, err, out)
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
				t.Fatalf("[%s] composed verified foreigns gave %q, want %q", bk.name, got, want)
			}
		})
	}
}
