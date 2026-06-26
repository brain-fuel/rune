package session

import "testing"

func TestParBuiltinResolves(t *testing.T) {
	s := New()
	// par is ambient: a program may reference it with no foreign decl.
	src := "main : (A : U) -> (B : U) -> IO A -> IO B -> IO B is par end"
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("par did not resolve as an ambient builtin: %v", err)
	}
}
