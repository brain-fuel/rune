package harness

import (
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
