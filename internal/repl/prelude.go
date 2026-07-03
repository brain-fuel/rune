package repl

import (
	"fmt"

	"goforge.dev/rune/v3/internal/prelude"
	"goforge.dev/rune/v3/internal/session"
)

// loadPrelude installs the prelude into s. A failure is a broken build, not a
// user error: the prelude is checked in and gated by the test suite.
func loadPrelude(s *session.Session) error {
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		return fmt.Errorf("loading the repl prelude: %w", err)
	}
	return nil
}
