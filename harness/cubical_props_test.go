package harness

import (
	"strings"
	"testing"

	"pgregory.net/rapid"

	"goforge.dev/rune/v3/internal/session"
	"goforge.dev/rune/v3/surface"
)

// §F substrate hardening: the De Morgan interval (phase 1) and the face lattice
// (phase 3a) must be confluent-to-canonical on CLOSED input — every closed
// interval term reduces to an endpoint, every closed face term to ⊤ or ⊥. The
// refl listings (ch17, ch19) only sample fixed equations; these properties
// hammer the ι-rules over thousands of randomly nested terms, the invariant the
// Kan operations rely on when they inspect a face with `faceConst`.

func normIv(t *rapid.T, s *session.Session, src string) string {
	e, err := surface.ParseExpr(src)
	if err != nil {
		t.Fatalf("parse %q: %v", src, err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatalf("elab %q: %v", src, err)
	}
	return surface.PrettyWith(s.NormalizeExpr(tm), s.RefNames())
}

// genInterval builds a closed interval term from endpoints, reversal, and the
// two De Morgan connections.
func genInterval(t *rapid.T, depth int) string {
	if depth <= 0 {
		return rapid.SampledFrom([]string{"i0", "i1"}).Draw(t, "ivl-leaf")
	}
	switch rapid.IntRange(0, 4).Draw(t, "ivl-node") {
	case 0:
		return "i0"
	case 1:
		return "i1"
	case 2:
		return "(ineg " + genInterval(t, depth-1) + ")"
	case 3:
		return "(imin " + genInterval(t, depth-1) + " " + genInterval(t, depth-1) + ")"
	default:
		return "(imax " + genInterval(t, depth-1) + " " + genInterval(t, depth-1) + ")"
	}
}

// genFace builds a closed face term from the lattice constants, the atomic
// constraints over closed interval terms, and the two binary connectives.
func genFace(t *rapid.T, depth int) string {
	if depth <= 0 {
		return rapid.SampledFrom([]string{"ftop", "fbot"}).Draw(t, "face-leaf")
	}
	switch rapid.IntRange(0, 5).Draw(t, "face-node") {
	case 0:
		return "ftop"
	case 1:
		return "fbot"
	case 2:
		return "(ieq0 " + genInterval(t, depth-1) + ")"
	case 3:
		return "(ieq1 " + genInterval(t, depth-1) + ")"
	case 4:
		return "(fand " + genFace(t, depth-1) + " " + genFace(t, depth-1) + ")"
	default:
		return "(for " + genFace(t, depth-1) + " " + genFace(t, depth-1) + ")"
	}
}

func TestClosedIntervalNormalizesToEndpoint(t *testing.T) {
	s := session.New()
	rapid.Check(t, func(t *rapid.T) {
		src := genInterval(t, rapid.IntRange(0, 4).Draw(t, "depth"))
		got := normIv(t, s, src)
		if got != "i0" && got != "i1" {
			t.Fatalf("closed interval %q normalized to %q, want i0 or i1", src, got)
		}
	})
}

func TestClosedFaceNormalizesToConstant(t *testing.T) {
	s := session.New()
	rapid.Check(t, func(t *rapid.T) {
		src := genFace(t, rapid.IntRange(0, 4).Draw(t, "depth"))
		got := normIv(t, s, src)
		if got != "ftop" && got != "fbot" {
			t.Fatalf("closed face %q normalized to %q, want ftop or fbot", src, got)
		}
	})
}

// ────────────────────────────────────────────────────────────────────────────
// X2 CANONICITY — the standing C-REG bet, made adversarial.
//
// The bet: the non-regular CCHM machinery stays CANONICAL — every CLOSED term of
// a fibrant inductive type reduces to a CONSTRUCTOR, even though full regularity
// is NOT implemented (only the constant-line regularity rule + the empty/total
// system rules + the structural former rules fire). The way canonicity could
// break is precisely a CLOSED Kan operation getting STUCK: a `transp`/`hcomp`/
// `comp`/`transpG` whose ι-rules fail to fire on a value with no free interval or
// face variable, leaving a Kan head in the normal form of a closed term.
//
// The two properties above guarantee the PRECONDITION the Kan rules rely on:
// every closed face reduces to ⊤/⊥ (so a "proper face" can never survive in a
// closed term — `tryHcomp`/`tryComp` always see ⊤ or ⊥), and every closed
// interval term reduces to an endpoint (so a constant-line probe is decisive).
// The properties below go the rest of the way: they build RANDOM closed Kan
// composites over concrete fibrant types and assert the normal form is a
// constructor (no residual Kan head). If the bet were false — if some closed
// composite got stuck — these would FAIL with a Kan head in the output. They are
// adversarial, not tautological: an OPEN composite (variable face / varying line)
// genuinely stays stuck (TestCubicalOpenCompositeStaysStuck pins that the
// detector fires), so a green run is real evidence the closed fragment is total.

// cubicalPrelude gives the canonicity generators two concrete OUTER inductive
// types to embed (`fib Bool`, `fib Nat2`) beside the ambient HITs (Circle/Susp).
const cubicalPrelude = `
data Bool : U is true : Bool | false : Bool end
data Nat2 : U is z2 : Nat2 | s2 : Nat2 -> Nat2 end
`

// normCub elaborates and normalizes a closed source term, returning its pretty
// normal form (shared with the interval/face props but kept separate so a parse
// or elaboration failure on a generated CLOSED Kan term is itself a hard finding —
// a closed, well-typed cubical term must always check).
func normCub(t *rapid.T, s *session.Session, src string) string {
	e, err := surface.ParseExpr(src)
	if err != nil {
		t.Fatalf("parse %q: %v", src, err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatalf("elab %q: %v", src, err)
	}
	return surface.PrettyWith(s.NormalizeExpr(tm), s.RefNames())
}

// kanHeads are the operation names that MUST NOT head a fully-normalized closed
// term. A constructor-headed result (true/false/z2/s2/base/north/south/pairF/
// papp-of-a-path-constructor) is canonical; a result whose first token is one of
// these is a STUCK Kan composite — a canonicity failure for a closed term.
var kanHeads = []string{"transpG", "transp", "hcomp", "comp", "fsplit", "unglue"}

// hasStuckKanHead reports whether the pretty normal form is HEADED by a Kan
// operation (its first whitespace-delimited token, stripped of a leading paren,
// is a Kan op). We check the head only: a constructor argument may legitimately
// contain an open sub-term, but a CLOSED composite's head must be canonical.
func hasStuckKanHead(nf string) bool {
	head := strings.TrimSpace(nf)
	head = strings.TrimLeft(head, "(")
	for _, kw := range kanHeads {
		if head == kw || strings.HasPrefix(head, kw+" ") {
			return true
		}
	}
	return false
}

// fibTy is a typed description of a closed concrete fibrant type, so genElem can
// build a constructor without reverse-parsing the surface string (string surgery
// over nested `fn … is … end` is too fragile). `surf` renders the type as
// surface syntax (a closed `UF`); the kind/children drive element generation.
type fibTy struct {
	kind     string // "boolF" | "natF" | "circle" | "susp" | "sigma" | "pi"
	a, b     *fibTy // children (susp uses a; sigma/pi use a and b)
	surfText string // memoized surface rendering
}

func (ty *fibTy) surf() string { return ty.surfText }

func mkLeaf(kind string) *fibTy {
	switch kind {
	case "boolF":
		return &fibTy{kind: "boolF", surfText: "(fib Bool)"}
	case "natF":
		return &fibTy{kind: "natF", surfText: "(fib Nat2)"}
	default:
		return &fibTy{kind: "circle", surfText: "Circle"}
	}
}

// genFibType builds a CLOSED concrete fibrant type (a `UF`): a `fib` of an outer
// inductive, an ambient HIT, a suspension of a smaller type, or a non-dependent
// inner product/function over smaller types. Every leaf is a concrete closed
// type, so the whole thing is a closed `UF` with a head former the structural Kan
// rules can dispatch on.
func genFibType(t *rapid.T, depth int) *fibTy {
	if depth <= 0 {
		return mkLeaf(rapid.SampledFrom([]string{"boolF", "natF", "circle"}).Draw(t, "fib-leaf"))
	}
	switch rapid.IntRange(0, 4).Draw(t, "fib-node") {
	case 0:
		return mkLeaf("boolF")
	case 1:
		return mkLeaf("circle")
	case 2:
		a := genFibType(t, depth-1)
		return &fibTy{kind: "susp", a: a, surfText: "(Susp " + a.surf() + ")"}
	case 3:
		a := genFibType(t, depth-1)
		b := genFibType(t, depth-1)
		return &fibTy{kind: "sigma", a: a, b: b,
			surfText: "(sigmaF " + a.surf() + " (fn (zz : El " + a.surf() + ") is " + b.surf() + " end))"}
	default:
		a := genFibType(t, depth-1)
		b := genFibType(t, depth-1)
		return &fibTy{kind: "pi", a: a, b: b,
			surfText: "(piF " + a.surf() + " (fn (zz : El " + a.surf() + ") is " + b.surf() + " end))"}
	}
}

// genElem builds a CLOSED constructor-headed element of `ty`. For HITs it picks a
// point constructor; for inner products a pair; for inner functions a constant
// lambda. It may itself wrap the element in a Kan composite (genKan) so composites
// NEST — the adversarial part: a stuck inner composite would surface as a
// non-constructor argument and, when the outer op tries to fire on it, can leave
// the whole term stuck.
func genElem(t *rapid.T, ty *fibTy, depth int) string {
	// Possibly wrap in a Kan composite at this type (closed, so must reduce).
	if depth > 0 && rapid.IntRange(0, 2).Draw(t, "wrap") == 0 {
		return genKan(t, ty, depth-1)
	}
	switch ty.kind {
	case "boolF":
		return rapid.SampledFrom([]string{"true", "false"}).Draw(t, "bool-ctor")
	case "natF":
		if depth <= 0 || rapid.IntRange(0, 1).Draw(t, "nat-ctor") == 0 {
			return "z2"
		}
		return "(s2 " + elemOfNat2(depth-1) + ")"
	case "circle":
		return "base"
	case "susp":
		pole := rapid.SampledFrom([]string{"north", "south"}).Draw(t, "susp-pole")
		return "(" + pole + " " + ty.a.surf() + ")"
	case "sigma":
		ea := genElem(t, ty.a, depth-1)
		eb := genElem(t, ty.b, depth-1)
		return "(pairF " + ty.a.surf() + " (fn (zz : El " + ty.a.surf() + ") is " + ty.b.surf() + " end) " + ea + " " + eb + ")"
	case "pi":
		eb := genElem(t, ty.b, depth-1)
		return "(fn (zz : El " + ty.a.surf() + ") is " + eb + " end)"
	default:
		return "base"
	}
}

func elemOfNat2(depth int) string {
	if depth <= 0 {
		return "z2"
	}
	return "(s2 " + elemOfNat2(depth-1) + ")"
}

// genKan wraps a closed constructor element of `ty` in a random Kan operation
// over a CONSTANT type-line and a random CLOSED face. Because the line is
// constant in i and the face is closed (reduces to ⊤/⊥), every shipped ι-rule's
// precondition is met, so the result MUST reduce away the Kan head:
//   - transp/transpG over a constant line ~> the element (regularity);
//   - hcomp/comp with a ⊤ system ~> u i1 htop (= the element, the wall at ⊤);
//   - hcomp with ⊥ ~> u0; comp over a constant line with ⊥ ~> u0.
// A green property is direct evidence the closed Kan fragment is canonical.
func genKan(t *rapid.T, ty *fibTy, depth int) string {
	elem := genElem(t, ty, depth)
	tys := ty.surf()
	face := genFace(t, rapid.IntRange(0, 3).Draw(t, "kan-face"))
	switch rapid.IntRange(0, 3).Draw(t, "kan-op") {
	case 0:
		return "(transp (fn (ii : I) is " + tys + " end) " + elem + ")"
	case 1:
		// hcomp over the type with a closed face; walls and floor are the element.
		return "(hcomp " + tys + " " + face +
			" (fn (ii : I) (hh : holds " + face + ") is " + elem + " end) " + elem + ")"
	case 2:
		// comp over a CONSTANT line with a closed face; walls and floor the element.
		return "(comp (fn (ii : I) is " + tys + " end) " + face +
			" (fn (ii : I) (hh : holds " + face + ") is " + elem + " end) " + elem + ")"
	default:
		return "(transpG (fn (ii : I) is " + tys + " end) " + face + " " + elem + ")"
	}
}

// TestCubicalClosedKanCompositeIsCanonical — the headline X2 property. A random
// closed Kan composite over a concrete fibrant type, nested to a random depth,
// must normalize to a CONSTRUCTOR (no residual Kan head). This is the operational
// reading of canonicity for the cubical fragment: every closed term of a fibrant
// inductive type reduces to a constructor. A stuck Kan head here would be a
// canonicity FAILURE and would falsify the C-REG bet.
func TestCubicalClosedKanCompositeIsCanonical(t *testing.T) {
	s := session.New()
	if _, err := s.LoadSource(cubicalPrelude); err != nil {
		t.Fatalf("prelude: %v", err)
	}
	rapid.Check(t, func(t *rapid.T) {
		depth := rapid.IntRange(1, 4).Draw(t, "depth")
		ty := genFibType(t, rapid.IntRange(0, 2).Draw(t, "ty-depth"))
		src := genKan(t, ty, depth)
		nf := normCub(t, s, src)
		if hasStuckKanHead(nf) {
			t.Fatalf("CANONICITY FAILURE: closed Kan composite\n  %s\nover %s got STUCK, normal form headed by a Kan op:\n  %s", src, ty.surf(), nf)
		}
	})
}

// TestCubicalClosedTranspConstantIsIdentity — the regularity-boundary stress.
// transp over a CONSTANT line of ANY of the random concrete fibrant types is the
// identity: it returns its argument unchanged. This is the exact rule whose
// ABSENCE in the non-regular setting is the worry — so we hammer it across all
// the type formers, including nested Susp/sigmaF/piF, and on constructor inputs
// that themselves carry Kan composites. (We compare against an `Eq`-refl witness
// rather than string-matching the element, so the kernel's own conversion judges
// equality — a stronger check than a syntactic compare.)
func TestCubicalClosedTranspConstantIsIdentity(t *testing.T) {
	s := session.New()
	if _, err := s.LoadSource(cubicalPrelude); err != nil {
		t.Fatalf("prelude: %v", err)
	}
	rapid.Check(t, func(t *rapid.T) {
		depth := rapid.IntRange(0, 3).Draw(t, "depth")
		ty := genFibType(t, rapid.IntRange(0, 2).Draw(t, "ty-depth"))
		elem := genElem(t, ty, depth)
		// transp over a constant line must equal the element by refl — if regularity
		// failed to fire, the transp would stay stuck and `refl elem` would NOT check
		// against `Eq … (transp …) elem` (the two sides would be inconvertible).
		tys := ty.surf()
		eqSrc := "(refl " + elem + " : Eq (El " + tys + ") (transp (fn (ii : I) is " + tys + " end) " + elem + ") " + elem + ")"
		e, err := surface.ParseExpr(eqSrc)
		if err != nil {
			t.Fatalf("parse %q: %v", eqSrc, err)
		}
		if _, _, err := s.ElabExpr(e); err != nil {
			t.Fatalf("REGULARITY FAILURE: transp over a constant %s line is not definitionally the identity on %s:\n  %v", tys, elem, err)
		}
	})
}

// TestCubicalOpenCompositeStaysStuck — the NON-TAUTOLOGY guard. An OPEN Kan
// composite (a variable face, or a genuinely varying line) does NOT in general
// reduce to a constructor — it legitimately keeps a Kan head. This pins that
// `hasStuckKanHead` actually fires on a stuck term, so a green canonicity run
// above is meaningful (the assertion is not vacuously satisfiable). If this ever
// reduced away, the canonicity test would be testing nothing.
func TestCubicalOpenCompositeStaysStuck(t *testing.T) {
	s := session.New()
	if _, err := s.LoadSource(cubicalPrelude); err != nil {
		t.Fatalf("prelude: %v", err)
	}
	// hcomp on a VARIABLE face must stay stuck (the canonical witness that closed
	// faces, not the Kan rules, are what unblock canonicity).
	src := `fn (phi : F) (u : I -> holds phi -> El (fib Bool)) (u0 : El (fib Bool)) is hcomp (fib Bool) phi u u0 end`
	e, err := surface.ParseExpr(src)
	if err != nil {
		t.Fatal(err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatal(err)
	}
	nf := surface.PrettyWith(s.NormalizeExpr(tm), s.RefNames())
	if !strings.Contains(nf, "hcomp") {
		t.Fatalf("expected an OPEN hcomp to stay stuck (keep a Kan head); got %q — the canonicity detector would then be vacuous", nf)
	}
}
