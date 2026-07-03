// Package prelude ships the standard prelude source. It is ordinary
// surface rune with no special status; the REPL and the CLI both load it
// through the same pipeline as any user file.
package prelude

import _ "embed"

//go:embed prelude.rune
var src string

// Source returns the prelude source.
func Source() string { return src }
