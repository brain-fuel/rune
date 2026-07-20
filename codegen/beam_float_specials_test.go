package codegen_test

import (
	"os"
	"os/exec"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/prelude"
	"goforge.dev/rune/v3/internal/session"
)

// beam_float_specials_test.go -- Float Track A, BEAM backend. The Erlang VM's native
// float() cannot represent IEEE754 NaN or Infinity (any arithmetic that would produce
// them raises error:badarith), so every other backend produced NaN/Inf while the BEAM
// crashed. beam.go now reboxes a rune Float as EITHER a finite raw float() OR one of
// three atoms (nan / pos_inf / neg_inf), and fadd/fsub/fmul/fdiv/fleqN plus the display
// path implement the IEEE rules over that rebox. These tests emit AND RUN a Float program
// under escript to prove the specials are now representable and correct -- true parity.

// emitBeamFloat loads the real shared prelude, then the given corpus on top, and returns
// the emitted Erlang source for `main`. Std.Float's foreign axioms need the full prelude,
// so (unlike emitWith) this loads both layers, mirroring the WASM Track A test.
func emitBeamFloat(t *testing.T, corpus, main string) string {
	t.Helper()
	s := session.New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	if _, err := s.LoadSource(corpus); err != nil {
		t.Fatalf("loading corpus: %v", err)
	}
	p, err := s.EmitProgram(main)
	if err != nil {
		t.Fatalf("EmitProgram(%s): %v", main, err)
	}
	src, err := codegen.Beam{}.Emit(p)
	if err != nil {
		t.Fatalf("beam emit(%s): %v", main, err)
	}
	return string(src)
}

// runBeamFloat runs an emitted escript and returns trimmed stdout, skipping cleanly (not a
// false pass) if escript is not on PATH. The emit step itself is asserted unconditionally
// by the caller, so a missing escript still catches an emit-time regression.
func runBeamFloat(t *testing.T, src string) string {
	t.Helper()
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not in PATH")
	}
	dir := t.TempDir()
	f := dir + "/main.erl"
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command("escript", f).Output()
	if err != nil {
		stderr := ""
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		t.Fatalf("escript run failed: %v\n%s\n--- emitted .erl ---\n%s", err, stderr, src)
	}
	return strings.TrimSpace(string(out))
}

// beamFloatSpecialsCorpus checks the WHOLE POINT of the rebox: values the BEAM's native
// float() cannot hold. Every sub-check is folded into one Bool via and-chain so the
// observable output is the discrete "true" every backend produces -- no float is printed.
//   - isNaN (0.0 / 0.0)            -> nan               (fdiv 0/0)
//   - 1.0 <= (1.0 / 0.0)           -> 1 <= +inf = true  (fle over fleqN, +inf)
//   - isNaN (+inf + -inf)          -> nan               (fadd, opposite infinities;
//     -inf built as (0-1)/0)
//   - (1.0 + 2.0) == 3.0           -> finite still works (fadd/feq unchanged path)
const beamFloatSpecialsCorpus = `
zeroF : Std.Float.Float is Std.Float.fromNat zero end
oneF  : Std.Float.Float is Std.Float.fromNat (succ zero) end
twoF  : Std.Float.Float is Std.Float.fromNat (succ (succ zero)) end
threeF : Std.Float.Float is Std.Float.fromNat (succ (succ (succ zero))) end

posInf : Std.Float.Float is Std.Float.fdiv oneF zeroF end
negInf : Std.Float.Float is Std.Float.fdiv (Std.Float.fsub zeroF oneF) zeroF end

nanZeroOk : Bool is Std.Float.isNaN (Std.Float.fdiv zeroF zeroF) end
leInfOk   : Bool is Std.Float.fle oneF posInf end
nanSumOk  : Bool is Std.Float.isNaN (Std.Float.fadd posInf negInf) end
finiteOk  : Bool is Std.Float.feq (Std.Float.fadd oneF twoF) threeF end

main : Bool is and nanZeroOk (and leInfOk (and nanSumOk finiteOk)) end
`

// TestBeamFloatSpecialsCompute is the BEAM acceptance gate for Float Track A: NaN and
// Infinity are now representable (as atoms) and the IEEE rules hold -- 0/0 is NaN, 1 <=
// +inf, +inf + -inf is NaN, and finite arithmetic still works. Before the rebox this
// program crashed with error:badarith at the first 0/0. Emit is asserted unconditionally;
// the run skips cleanly if escript is absent.
func TestBeamFloatSpecialsCompute(t *testing.T) {
	src := emitBeamFloat(t, beamFloatSpecialsCorpus, "main")
	if got := runBeamFloat(t, src); got != "true" {
		t.Fatalf("beamFloatSpecialsCorpus main = %q, want %q", got, "true")
	}
}

// beamFloatSignXorCorpus checks fmul's sign-XOR rule when one or both operands are
// infinite: the result's sign is the product of the two operand signs, exactly like
// finite IEEE multiplication. Both directions are exercised so a swapped-argument bug
// in ff___fsign or ff___fmul2 would flip a conjunct to false and fail the test.
//   - neg_inf * (a negative finite, -2)  -> pos_inf  (sign -1 * -1 = +1)
//   - pos_inf * (a negative finite, -1)  -> neg_inf  (sign +1 * -1 = -1)
//
// Both infinities involved are built from arithmetic only (no FloatLit): posInf =
// 1/0, negInf = (0-1)/0. feq on two identical infinity atoms is true because fleqN
// treats pos_inf<=pos_inf (and neg_inf<=neg_inf) as 1 -- see fleqN2 in beam.go -- so
// feq is a sound discrete check here, not just for finite values.
const beamFloatSignXorCorpus = `
zeroF : Std.Float.Float is Std.Float.fromNat zero end
oneF  : Std.Float.Float is Std.Float.fromNat (succ zero) end
twoF  : Std.Float.Float is Std.Float.fromNat (succ (succ zero)) end

negOneF : Std.Float.Float is Std.Float.fsub zeroF oneF end
negTwoF : Std.Float.Float is Std.Float.fsub zeroF twoF end

posInf : Std.Float.Float is Std.Float.fdiv oneF zeroF end
negInf : Std.Float.Float is Std.Float.fdiv negOneF zeroF end

mulNegInfByNegOk : Bool is Std.Float.feq (Std.Float.fmul negInf negTwoF) posInf end
mulPosInfByNegOk : Bool is Std.Float.feq (Std.Float.fmul posInf negOneF) negInf end

main : Bool is and mulNegInfByNegOk mulPosInfByNegOk end
`

// TestBeamFloatSignXor is the BEAM acceptance gate for fmul's sign-XOR path over
// infinities: neg_inf times a negative finite is pos_inf, and pos_inf times a negative
// finite is neg_inf. Before this rebox any arithmetic reaching an infinite operand
// crashed with error:badarith, so this path was entirely unexercised.
func TestBeamFloatSignXor(t *testing.T) {
	src := emitBeamFloat(t, beamFloatSignXorCorpus, "main")
	if got := runBeamFloat(t, src); got != "true" {
		t.Fatalf("beamFloatSignXorCorpus main = %q, want %q", got, "true")
	}
}

// beamFloatFleqInfCorpus checks Std.Float.fle (the Bool wrapper over the foreign
// fleqN : Float -> Float -> Whole) across every infinity/finite/NaN combination the
// rebox's fleqN2 special-cases in beam.go:
//   - neg_inf <= pos_inf            -> true  (fleqN2(neg_inf, _) -> 1)
//   - pos_inf <= finite (1.0)       -> false (fleqN2(pos_inf, _) -> 0, not pos_inf/pos_inf)
//   - finite (1.0) <= pos_inf       -> true  (fleqN2(_, pos_inf) -> 1)
//   - NaN <= finite (1.0)           -> false (fleqN2(nan, _) -> 0)
//
// Each conjunct isolates one branch, so a wrong sign or a swapped comparison direction
// flips exactly that conjunct to false and fails the and-chain.
const beamFloatFleqInfCorpus = `
zeroF : Std.Float.Float is Std.Float.fromNat zero end
oneF  : Std.Float.Float is Std.Float.fromNat (succ zero) end

negOneF : Std.Float.Float is Std.Float.fsub zeroF oneF end

posInf : Std.Float.Float is Std.Float.fdiv oneF zeroF end
negInf : Std.Float.Float is Std.Float.fdiv negOneF zeroF end
nanF   : Std.Float.Float is Std.Float.fdiv zeroF zeroF end

leNegInfPosInfOk    : Bool is Std.Float.fle negInf posInf end
notLePosInfFiniteOk : Bool is notB (Std.Float.fle posInf oneF) end
leFinitePosInfOk    : Bool is Std.Float.fle oneF posInf end
notLeNanFiniteOk    : Bool is notB (Std.Float.fle nanF oneF) end

main : Bool is
  and leNegInfPosInfOk
  (and notLePosInfFiniteOk
  (and leFinitePosInfOk
       notLeNanFiniteOk))
end
`

// TestBeamFloatFleqInf is the BEAM acceptance gate for fleqN's infinity/NaN branches
// via the fle Bool wrapper: neg_inf <= pos_inf is true, pos_inf <= a finite is false,
// a finite <= pos_inf is true, and NaN <= a finite is false. Before this rebox any of
// these comparisons crashed with error:badarith the moment an infinite/NaN operand was
// produced.
func TestBeamFloatFleqInf(t *testing.T) {
	src := emitBeamFloat(t, beamFloatFleqInfCorpus, "main")
	if got := runBeamFloat(t, src); got != "true" {
		t.Fatalf("beamFloatFleqInfCorpus main = %q, want %q", got, "true")
	}
}

// beamInfDisplayCorpus prints +inf via printFloat : Float -> IO Float. The display rebox
// must render pos_inf as "Infinity" (ECMAScript Number::toString), not crash or show a
// bare Erlang atom.
const beamInfDisplayCorpus = `
zeroF : Std.Float.Float is Std.Float.fromNat zero end
oneF  : Std.Float.Float is Std.Float.fromNat (succ zero) end
main : IO Std.Float.Float is Std.Float.printFloat (Std.Float.fdiv oneF zeroF) end
`

// beamNaNDisplayCorpus prints NaN via printFloat; the display rebox must render nan as
// "NaN".
const beamNaNDisplayCorpus = `
zeroF : Std.Float.Float is Std.Float.fromNat zero end
main : IO Std.Float.Float is Std.Float.printFloat (Std.Float.fdiv zeroF zeroF) end
`

// beamNegInfDisplayCorpus prints -inf (built as (0-1)/0, no FloatLit) via printFloat.
// The display rebox must render neg_inf as "-Infinity" (ECMAScript Number::toString),
// not bare Erlang atom `neg_inf` or a crash. This is the only one of the three atoms
// the pre-existing display test did not cover.
const beamNegInfDisplayCorpus = `
zeroF : Std.Float.Float is Std.Float.fromNat zero end
oneF  : Std.Float.Float is Std.Float.fromNat (succ zero) end
negOneF : Std.Float.Float is Std.Float.fsub zeroF oneF end
main : IO Std.Float.Float is Std.Float.printFloat (Std.Float.fdiv negOneF zeroF) end
`

// TestBeamFloatSpecialsDisplay proves the display rebox: printFloat of +inf contains
// "Infinity" and printFloat of NaN contains "NaN". (The result Float is also shown by the
// runner, so both the printFloat line and the show line carry the canonical spelling.)
func TestBeamFloatSpecialsDisplay(t *testing.T) {
	infSrc := emitBeamFloat(t, beamInfDisplayCorpus, "main")
	if got := runBeamFloat(t, infSrc); !strings.Contains(got, "Infinity") {
		t.Fatalf("printFloat(+inf) = %q, want it to contain %q", got, "Infinity")
	}
	nanSrc := emitBeamFloat(t, beamNaNDisplayCorpus, "main")
	if got := runBeamFloat(t, nanSrc); !strings.Contains(got, "NaN") {
		t.Fatalf("printFloat(NaN) = %q, want it to contain %q", got, "NaN")
	}
}

// TestBeamFloatNegInfDisplay is the BEAM acceptance gate for the neg_inf display path:
// printFloat of -inf must contain "-Infinity", not the bare atom `neg_inf`. The
// pre-existing display test only exercised pos_inf and nan; the sign-bit case was
// unexercised until now.
func TestBeamFloatNegInfDisplay(t *testing.T) {
	src := emitBeamFloat(t, beamNegInfDisplayCorpus, "main")
	if got := runBeamFloat(t, src); !strings.Contains(got, "-Infinity") {
		t.Fatalf("printFloat(-inf) = %q, want it to contain %q", got, "-Infinity")
	}
}
