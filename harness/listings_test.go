package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/session"
	"goforge.dev/rune/v3/surface"
)

// The v1.0.0 freeze criterion (ref_docs/rune-v1-design.md): every listing in
// the book ELABORATES, CHECKS, and RUNS against this core. listings/ holds the
// book's code; this file is the gate that enforces the criterion. A listing
// that stops loading, a marked expression that stops normalizing to its
// expected form, or an emitted chapter that stops running is a v1 regression.

// loadListing reads and loads one listing file into a fresh session.
func loadListing(t *testing.T, name string) *session.Session {
	t.Helper()
	src, err := os.ReadFile(filepath.Join("..", "listings", name))
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("%s does not check: %v", name, err)
	}
	return s
}

// normalizesTo asserts that expr, elaborated against the session, normalizes
// to want.
func normalizesTo(t *testing.T, s *session.Session, expr, want string) {
	t.Helper()
	e, err := s.ParseSrcExpr(expr)
	if err != nil {
		t.Fatal(err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatalf("%s does not elaborate: %v", expr, err)
	}
	got := surface.PrettyWith(s.NormalizeExpr(tm), s.RefNames())
	if got != want {
		t.Fatalf("%s normalized to %q, want %q", expr, got, want)
	}
}

func TestListingsElaborateAndCheck(t *testing.T) {
	entries, err := os.ReadDir(filepath.Join("..", "listings"))
	if err != nil {
		t.Fatal(err)
	}
	if len(entries) < 5 {
		t.Fatalf("expected the book's chapters in listings/, found %d files", len(entries))
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".rune") {
			continue
		}
		t.Run(e.Name(), func(t *testing.T) { loadListing(t, e.Name()) })
	}
}

func TestListingsRun(t *testing.T) {
	t.Run("ch01", func(t *testing.T) {
		s := loadListing(t, "ch01_functions.rune")
		// idU maps small types; applied to nothing it δβ-normalizes to the
		// identity lambda (U itself is not a member of U).
		normalizesTo(t, s, `idU`, "fn (x : U) is x end")
	})
	t.Run("ch02", func(t *testing.T) {
		s := loadListing(t, "ch02_implicits.rune")
		normalizesTo(t, s, `id U`, "U")
	})
	t.Run("ch03", func(t *testing.T) {
		s := loadListing(t, "ch03_equality.rune")
		// cast between convertible endpoints reduces away under the binder.
		normalizesTo(t, s, `transport U U (refl U)`, "fn (x : U) is x end")
	})
	t.Run("ch04", func(t *testing.T) {
		s := loadListing(t, "ch04_data.rune")
		normalizesTo(t, s, `four`, "succ (succ (succ (succ zero)))")
		normalizesTo(t, s, `not (not true)`, "true")
		normalizesTo(t, s, `length Nat (cons Nat zero (nil Nat))`, "succ zero")
		// The induction proof COMPUTES at canonical numerals.
		normalizesTo(t, s, `addZeroRight (succ zero)`, "refl (succ zero)")
	})
	t.Run("ch05", func(t *testing.T) {
		s := loadListing(t, "ch05_quantities.rune")
		normalizesTo(t, s, `eid U`, "U")
	})
	t.Run("ch06", func(t *testing.T) {
		s := loadListing(t, "ch06_quotients.rune")
		// The quotient ι-rule: a lift computes on points.
		normalizesTo(t, s, `parityOfTwo`, "true")
		normalizesTo(t, s, `parity odd`, "false")
	})
	t.Run("ch07", func(t *testing.T) {
		s := loadListing(t, "ch07_integers.rune")
		// Lifted arithmetic reduces through the quotient on representatives.
		normalizesTo(t, s, `fst (padd (npair (succ zero) zero) (npair (succ zero) zero))`,
			"succ (succ zero)")
	})
	t.Run("ch08", func(t *testing.T) {
		s := loadListing(t, "ch08_truncation.rune")
		// The squash eliminator is plain application: it β-reduces away. (The
		// printer's lambda annotations are canonically U — GRAMMAR.md §8.)
		normalizesTo(t, s, `squashElim (Squash Nat) (fn (x : Nat) is squash x end) someNat`,
			"fn (P : U) (k : U) is k zero end")
	})
	t.Run("ch09", func(t *testing.T) {
		s := loadListing(t, "ch09_two_level.rune")
		// Decoding computes: an inner function is a plain function.
		normalizesTo(t, s, `four`, "succ (succ (succ (succ zero)))")
		// The inner J computes on preflF: sym(refl) is refl, one level in.
		normalizesTo(t, s, `psymComputes`, "preflF (fib Nat) (succ (succ zero))")
	})
	t.Run("ch10", func(t *testing.T) {
		s := loadListing(t, "ch10_univalence.rune")
		// Transport THROUGH a postulated ua-path computes (the v3 idiom)...
		normalizesTo(t, s, `flipped`, "false")
		normalizesTo(t, s, `backAgain`, "true")
		// ...and along reflexivity it is the identity.
		normalizesTo(t, s, `same`, "true")
	})
	t.Run("ch11", func(t *testing.T) {
		s := loadListing(t, "ch11_arithmetic.rune")
		// The whole ergonomics ladder computing at once: literals, infix,
		// case, fuel-style Euclid — conversion does arithmetic.
		normalizesTo(t, s, `17 // 5`, "succ (succ (succ zero))")
		normalizesTo(t, s, `gcd 12 18`,
			"succ (succ (succ (succ (succ (succ zero)))))")
	})
	t.Run("ch12", func(t *testing.T) {
		s := loadListing(t, "ch12_integer_division.rune")
		// Floor vs truncate, live: −7 // 2 is −4 (canonical pair (0,4)),
		// quot −7 2 is −3 — conversion computing through the quotient.
		normalizesTo(t, s, `obs (zneg (intOf 7) // intOf 2)`,
			"npair zero (succ (succ (succ (succ zero))))")
		normalizesTo(t, s, `obs (quot (zneg (intOf 7)) (intOf 2))`,
			"npair zero (succ (succ (succ zero)))")
		normalizesTo(t, s, `natAbs (zneg (intOf 7) % intOf 2)`, "succ zero")
	})
	t.Run("ch13", func(t *testing.T) {
		s := loadListing(t, "ch13_rationals.rune")
		// Exact division through the quotient: (1/2) / (1/2) computes to
		// the representative 2/2 — the class of one.
		normalizesTo(t, s, `posR (rdivP (rp 1 0 1) (rp 1 0 1))`,
			"succ (succ zero)")
		normalizesTo(t, s, `denR (rdivP (rp 1 0 1) (rp 1 0 1))`,
			"succ zero")
	})
	t.Run("ch14", func(t *testing.T) {
		s := loadListing(t, "ch14_binary.rune")
		// Binary multiplication agrees with the unary spec by THEOREM
		// (bigMul checked on load); here conversion computes a small
		// product through the binary side and reads it back.
		normalizesTo(t, s, `toNatP (pmul (pI (pO pH)) (pI pH))`,
			"succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ zero))))))))))))))")
	})
}

// TestInnerLayerDoesNotDeploy: the v3 release criterion for the fibrant
// layer is "elaborates and checks", not "runs" — transport along a ua-path
// has no erased meaning yet (§F), so emission must refuse rather than
// silently compute the wrong function.
func TestInnerLayerDoesNotDeploy(t *testing.T) {
	s := loadListing(t, "ch10_univalence.rune")
	if _, err := s.EmitProgram("flipped"); err == nil {
		t.Fatal("emitting an inner-layer construction must be refused in v3")
	}
}

// TestListingsEmitAndExecute: the data and quotient chapters survive erasure
// and run on the JS backend (the "runs" of the freeze criterion, in the
// deployed sense). For the quotient chapters this is also the shadow rule's
// promise made visible: qin is the identity at runtime, a lift is a plain
// call, and every respect proof is gone.
func TestListingsEmitAndExecute(t *testing.T) {
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not in PATH")
	}
	cases := []struct {
		listing, main, want string
	}{
		{"ch04_data.rune", "four", "succ (succ (succ (succ zero)))"},
		{"ch06_quotients.rune", "parityOfTwo", "true"},
		{"ch07_integers.rune", "zresult", "npair (succ (succ zero)) (succ zero)"},
		// ch11 runs on the BigInt shadow: gcd 252 105 in milliseconds, with
		// case-shaped eliminations emitted as constant-time dispatch.
		{"ch11_arithmetic.rune", "answer", "21"},
		// ch12: the floor convention through the quotient and the shadow
		// agree — |−7 // 2| is 4, not 3.
		{"ch12_integer_division.rune", "answer", "4"},
		// ch13: the numerator representative of (7/2) / (3/5) under the
		// erased shadow — division through the flip, no proofs at runtime.
		{"ch13_rationals.rune", "answer", "105"},
		// ch14: 35 · 186 computed in binary, read back through toNat.
		{"ch14_binary.rune", "answer", "6510"},
	}
	for _, tc := range cases {
		t.Run(tc.listing, func(t *testing.T) {
			s := loadListing(t, tc.listing)
			p, err := s.EmitProgram(tc.main)
			if err != nil {
				t.Fatal(err)
			}
			out, err := codegen.Default().Emit(p)
			if err != nil {
				t.Fatal(err)
			}
			f, err := os.CreateTemp(t.TempDir(), "*.js")
			if err != nil {
				t.Fatal(err)
			}
			if _, err := f.WriteString(string(out)); err != nil {
				t.Fatal(err)
			}
			f.Close()
			got, err := exec.Command("node", f.Name()).CombinedOutput()
			if err != nil {
				t.Fatalf("node: %v\n%s", err, got)
			}
			if strings.TrimSpace(string(got)) != tc.want {
				t.Fatalf("emitted chapter printed %q, want %q", got, tc.want)
			}
		})
	}
}
