package session

import (
	"os"
	"testing"

	"goforge.dev/rune/surface"
)

func BenchmarkDiv(b *testing.B) {
	src, _ := os.ReadFile("/tmp/c4.rune")
	s := New()
	if _, err := s.LoadSource(string(src)); err != nil {
		b.Fatal(err)
	}
	e, _ := surface.ParseExprNat("9 / 5", "zero", "succ")
	for i := 0; i < b.N; i++ {
		tm, _, err := s.ElabExpr(e)
		if err != nil {
			b.Fatal(err)
		}
		_ = s.NormalizeExpr(tm)
	}
}
