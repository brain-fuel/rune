package repl

import (
	_ "embed"
	"fmt"

	"goforge.dev/rune/v3/internal/session"
)

// preludeSrc is ordinary surface rune loaded through the same pipeline as a
// :load — the prelude has no special status in the core or the cache. It is
// what makes a fresh prompt behave like a calculator: Whole (the numeric
// tower's foundation) with the numeral binding, the five infix operators, gcd,
// and the counting type Nat with its wholeOf injection.
//
//go:embed prelude.rune
var preludeSrc string

// loadPrelude installs the prelude into s. A failure is a broken build, not a
// user error: the prelude is checked in and gated by the test suite.
func loadPrelude(s *session.Session) error {
	if _, err := s.LoadSource(preludeSrc); err != nil {
		return fmt.Errorf("loading the repl prelude: %w", err)
	}
	return nil
}
