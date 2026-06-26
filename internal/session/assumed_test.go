// Package session
package session

import "testing"

func TestSessionAssumed(t *testing.T) {
	s := New()
	src := "foreign hostThing : (A : U) -> A -> A end\n" +
		"idU : (A : U) -> A -> A is fn (A : U) (x : A) is x end end"
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load: %v", err)
	}
	if !s.Assumed("hostThing") {
		t.Fatalf("foreign def should be assumed")
	}
	if s.Assumed("idU") {
		t.Fatalf("a defined function must not be assumed")
	}
}
