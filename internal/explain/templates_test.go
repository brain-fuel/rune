package explain

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// TestPrimTemplateCoverage is the spec's acceptance gate: every prim declared
// in codegen/ioprims.go must carry an English template. It also pins template
// hygiene: slot counts match, bound templates are 0-arg with one slot, and no
// template contains an em or en dash (the ASCII-hyphen-only rule).
func TestPrimTemplateCoverage(t *testing.T) {
	for _, n := range codegen.IOPrimNames() {
		tpl, ok := primTemplates[n]
		if !ok {
			t.Errorf("prim %q has no English template; add a row to primTemplates", n)
			continue
		}
		if got := strings.Count(tpl.verb, "%s"); got != tpl.args {
			t.Errorf("prim %q: verb %q has %d slots, args = %d", n, tpl.verb, got, tpl.args)
		}
		if tpl.bound != "" {
			if tpl.args != 0 {
				t.Errorf("prim %q: bound template requires args == 0", n)
			}
			if strings.Count(tpl.bound, "%s") != 1 {
				t.Errorf("prim %q: bound template %q must have exactly one slot", n, tpl.bound)
			}
		}
		// U+2014 em dash, U+2013 en dash: written as escapes so neither
		// the test source nor the plan carries the characters literally.
		for _, bad := range []string{"\u2014", "\u2013"} {
			if strings.Contains(tpl.verb, bad) || strings.Contains(tpl.bound, bad) {
				t.Errorf("prim %q: template contains an em/en dash (ASCII hyphen only)", n)
			}
		}
	}
}
