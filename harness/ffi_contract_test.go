package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/session"
)

// Verified FFI contracts (R-FFI / B4): a `foreign` axiom with a 0-quantity proof
// precondition. The POSITIVE direction shows the contract is checked, erased, and
// the program runs identically on every backend (the proof passes as the unit token,
// the host gets only the real arguments). The NEGATIVE direction shows the contract
// is the type: a caller that cannot discharge the precondition fails to elaborate.
func TestFFIContractRunsAndErases(t *testing.T) {
	s := loadListing(t, "ch60_ffi_contract.rune")
	p, err := s.EmitProgram("answer")
	if err != nil {
		t.Fatal(err)
	}
	const want = "succ (succ zero)"
	// Host impls of `guardedNat : (d : Nat) -> (0 pf) -> Nat` (echo d; the host may
	// trust d is nonzero because the caller proved it). The erased proof arrives as
	// each backend's unit token and is ignored.
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
			func(src string) string { return "function guardedNat(){ return d => _pf => d; }\n" + src },
			func(f string) *exec.Cmd { return exec.Command("node", f) }, nil},
		{"py", "python3", "py", codegen.Py{}.Emit,
			func(src string) string { return "def guardedNat():\n    return lambda d: lambda _pf: d\n" + src },
			func(f string) *exec.Cmd { return exec.Command("python3", f) }, nil},
		{"go", "go", "go", codegen.Go{}.Emit,
			func(src string) string {
				return src + "\nfunc guardedNat() any { return func(d any) any { return func(_pf any) any { return d } } }\n"
			},
			func(f string) *exec.Cmd { return exec.Command("go", "run", f) }, nil},
		{"rs", "rustc", "rs", codegen.Rust{}.Emit,
			func(src string) string {
				return src + "\nfn guardedNat() -> Rc<V> { vfun(|d: Rc<V>| -> Rc<V> { let d2 = d.clone(); vfun(move |_pf: Rc<V>| -> Rc<V> { d2.clone() }) }) }\n"
			},
			func(bin string) *exec.Cmd { return exec.Command(bin) },
			func(src, out string) *exec.Cmd { return exec.Command("rustc", "--edition", "2021", "-o", out, src) }},
		{"erl", "escript", "erl", codegen.Beam{}.Emit,
			func(src string) string { return src + "\nff_guardedNat() -> fun(D) -> fun(_Pf) -> D end end.\n" },
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
			// The erased proof must not leak equality machinery into the shadow.
			if strings.Contains(string(src), "refl") || strings.Contains(string(src), "Eq") {
				t.Fatalf("[%s] contract proof leaked into the shadow:\n%s", bk.name, src)
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
				t.Fatalf("[%s] guarded foreign gave %q, want %q", bk.name, got, want)
			}
		})
	}
}

// The contract is the type: a call that cannot discharge the precondition (passing
// `zero`, where isZero zero ~> true) fails to elaborate — the proof `refl false`
// does not check against `Eq Bool true false`.
func TestFFIContractEnforced(t *testing.T) {
	src := `
data Bool : U is true : Bool | false : Bool end
data Nat : U is zero : Nat | succ : Nat -> Nat end
isZero : Nat -> Bool is
  fn (n : Nat) is NatElim (fn (x : Nat) is Bool end) true (fn (k : Nat) (ih : Bool) is false end) n end
end
foreign guardedNat : (d : Nat) -> (0 pf : Eq Bool (isZero d) false) -> Nat end
bad : Nat is guardedNat zero (refl false) end
`
	s := session.New()
	if _, err := s.LoadSource(src); err == nil {
		t.Fatal("expected the unproven FFI precondition (isZero zero = false) to be rejected, but it elaborated")
	}
}
