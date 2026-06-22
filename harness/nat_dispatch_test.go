package harness

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/session"
)

// beqNatSrc is the decidable-equality shape: NatElim whose MOTIVE is a function
// type (`Nat -> Bool`) and whose INNER eliminator's step ignores its IH. On the
// native C/LL backends this used to compile to the eager bottom-up fold, which
// evaluates the step at every index and compounds multiplicatively under nesting —
// super-exponential (n=20 took ~9s, n=24 hung). With the IH-ignoring nat dispatch
// (StepIgnoresIH + the `_case` one-peel form) it is linear, like js/go/rust.
func beqNatSrc(main string) string {
	return `data Bool : U is true : Bool | false : Bool end
data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
beqNat : Nat -> Nat -> Bool is
  fn (a : Nat) is
    NatElim (fn (x : Nat) is Nat -> Bool end)
      (fn (b : Nat) is NatElim (fn (y : Nat) is Bool end) true (fn (j : Nat) (ih : Bool) is false end) b end)
      (fn (k : Nat) (ihk : Nat -> Bool) is
         fn (b : Nat) is
           NatElim (fn (y : Nat) is Bool end) false (fn (j : Nat) (ih2 : Bool) is ihk j end) b
         end end)
      a
  end
end
eqBig : Nat is case beqNat 64 64 of | true -> 1 | false -> 0 end end
neqBig : Nat is case beqNat 40 64 of | true -> 1 | false -> 0 end end
main : Nat is ` + main + ` end
`
}

// TestNatDispatchNativeNotExponential is the regression gate for the curried
// function-returning NatElim exponential on the C/LL backends. It compiles and runs
// `beqNat 64 64` (and a 40-vs-64 negative) on both native backends and requires each
// to finish FAST and CORRECT — pre-fix, n=64 would not terminate in any reasonable
// time. Skips cleanly without cc/clang.
func TestNatDispatchNativeNotExponential(t *testing.T) {
	cases := []struct {
		main string
		want string
	}{
		{"eqBig", "1"},  // beqNat 64 64 = true  (the would-hang case)
		{"neqBig", "0"}, // beqNat 40 64 = false
	}
	natives := []struct {
		name, bin, ext string
		emit           func(codegen.Program) (codegen.TargetSource, error)
		needsRuntime   bool
	}{
		{"c", "cc", "c", codegen.C{}.Emit, false},
		{"ll", "clang", "ll", codegen.LL{}.Emit, true},
	}
	for _, bk := range natives {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			for _, tc := range cases {
				s := session.New()
				if _, err := s.LoadSource(beqNatSrc(tc.main)); err != nil {
					t.Fatalf("source does not check: %v", err)
				}
				p, err := s.EmitProgram("main")
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
				bin := filepath.Join(dir, "main.bin")
				var compile *exec.Cmd
				if bk.needsRuntime {
					if err := os.WriteFile(filepath.Join(dir, "runtime.c"), []byte(codegen.LL{}.EmitRuntime()), 0o644); err != nil {
						t.Fatal(err)
					}
					compile = exec.Command("clang", f, filepath.Join(dir, "runtime.c"), "-O2", "-o", bin)
				} else {
					compile = exec.Command("cc", "-O2", "-o", bin, f)
				}
				if out, err := compile.CombinedOutput(); err != nil {
					t.Fatalf("[%s] compile failed: %v\n%s", bk.name, err, out)
				}
				// The perf gate: pre-fix this binary would run for minutes/hours at
				// n=64. A few seconds is a generous linear-time bound.
				ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
				out, err := exec.CommandContext(ctx, bin).Output()
				cancel()
				if ctx.Err() == context.DeadlineExceeded {
					t.Fatalf("[%s] %s: timed out — the IH-ignoring nat dispatch did not fire (still exponential)", bk.name, tc.main)
				}
				if err != nil {
					t.Fatalf("[%s] %s: run failed: %v", bk.name, tc.main, err)
				}
				if got := strings.TrimSpace(string(out)); got != tc.want {
					t.Errorf("[%s] %s = %q, want %q", bk.name, tc.main, got, tc.want)
				}
			}
		})
	}
}
