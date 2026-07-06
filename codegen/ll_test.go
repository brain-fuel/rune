package codegen_test

import (
	"os"
	"os/exec"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// ll_test.go — the SECOND NATIVE backend (telos-2 / M4, B3+ fan-out) conformance
// gate. The LLVM backend emits the program LOGIC as LLVM IR text and links the C
// runtime (LL{}.EmitRuntime()); clang compiles `program.ll runtime.c -o exe`. The
// gate is byte-identity with the C backend (the ORACLE) on the supported corpus
// subset — a 2nd native target running the arithmetic+data+closure fragment
// byte-identical is the fan-out milestone.

// runLL emits LLVM IR + the C runtime, compiles+links both with clang, runs the
// binary, and returns trimmed stdout. Skips gracefully when clang is absent.
func runLL(t *testing.T, ll string) string {
	t.Helper()
	if _, err := exec.LookPath("clang"); err != nil {
		t.Skip("clang not in PATH")
	}
	dir := t.TempDir()
	llf := dir + "/program.ll"
	rtf := dir + "/runtime.c"
	bin := dir + "/main"
	if err := os.WriteFile(llf, []byte(ll), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(rtf, []byte(codegen.LL{}.EmitRuntime()), 0o644); err != nil {
		t.Fatal(err)
	}
	if out, err := exec.Command("clang", llf, rtf, "-o", bin).CombinedOutput(); err != nil {
		t.Fatalf("clang: %v\n%s\n--- emitted .ll ---\n%s", err, out, ll)
	}
	out, err := exec.Command(bin).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v\n%s", err, out)
	}
	return strings.TrimSpace(string(out))
}

// TestLLEmitListLength is the second-native-backend keystone: a recursive ListElim
// (lowered through the shared IR, closure-converted, then emitted as LLVM functions
// over a {code,env} closure runtime) computes the same fold as every other backend —
// byte-identical $show, compiled by clang from genuine LLVM IR.
func TestLLEmitListLength(t *testing.T) {
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
	if got := runLL(t, emitWith(t, codegen.LL{}, src, "two")); got != "succ (succ zero)" {
		t.Fatalf("ll list length: got %q", got)
	}
}

// TestLLEmitAndRunNat pins native nat arithmetic on the LLVM backend: zero/succ/
// NatElim become the immediate-int rep + an LLVM fold loop, add computes the same
// successor chain the other backends show.
func TestLLEmitAndRunNat(t *testing.T) {
	src := natSrc + `three : Nat is add (succ zero) (succ (succ zero)) end`
	if got := runLL(t, emitWith(t, codegen.LL{}, src, "three")); got != "succ (succ (succ zero))" {
		t.Fatalf("ll nat: got %q", got)
	}
}

// TestLLNatLitAndAccelRun pins C7 / R-NUM on the LLVM backend: compressed numerals
// deploy as immediate ints and the accel add/mul/monus become runtime native
// arithmetic — byte-identical to the eliminator-loop result (incl. the saturating
// Peano monus 0-floor monusN 3 5 = 0).
func TestLLNatLitAndAccelRun(t *testing.T) {
	for _, tc := range []struct{ src, main, want string }{
		{bigNatSrc, "big", "5000"},
		{bigNatSrc, "bigger", "5002"},
		{bigNatSrc, "product", "10000"},
		{accelNatSrc, "sum", "8000"},
		{accelNatSrc, "prod", "10000"},
		{accelNatSrc, "diff", "5"},
		{accelNatSrc, "diffZero", "0"},
	} {
		if got := runLL(t, emitWith(t, codegen.LL{}, tc.src, tc.main)); got != tc.want {
			t.Fatalf("ll %s: got %q, want %q", tc.main, got, tc.want)
		}
	}
}

// TestLLConformsToC is the CROSS-BACKEND conformance gate: the LLVM backend's
// observable $show output is byte-identical to the C backend (the ORACLE) on the
// supported corpus subset — nat arithmetic, recursive ListElim, a multi-field
// constructor, native literals, and accel arithmetic. This is the fan-out
// milestone's guarantee: a second native target, byte-for-byte.
func TestLLConformsToC(t *testing.T) {
	const listSrc = natSrc + `
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
	const pairSrc = natSrc + `
data Pairing : U -> U -> U is
  mk : (A : U) -> (B : U) -> A -> B -> Pairing A B
end
p : Pairing Nat Nat is mk Nat Nat (succ zero) (succ (succ zero)) end
`
	cases := []struct{ src, main string }{
		{natSrc + `three : Nat is add (succ zero) (succ (succ zero)) end`, "three"},
		{listSrc, "two"},
		{pairSrc, "p"},
		{bigNatSrc, "big"},
		{bigNatSrc, "bigger"},
		{bigNatSrc, "product"},
		{accelNatSrc, "sum"},
		{accelNatSrc, "prod"},
		{accelNatSrc, "diff"},
		{accelNatSrc, "diffZero"},
	}
	for _, tc := range cases {
		cOut := runC(t, emitWith(t, codegen.C{}, tc.src, tc.main))
		llOut := runLL(t, emitWith(t, codegen.LL{}, tc.src, tc.main))
		if cOut != llOut {
			t.Fatalf("%s: LLVM output %q != C oracle %q", tc.main, llOut, cOut)
		}
	}
}
