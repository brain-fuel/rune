package codegen

import (
	"sort"
	"testing"
)

// TestIOPrimNames pins the exported prim enumeration: it must cover every
// prim family declared in ioprims.go, sorted and without duplicates, so the
// explainer's template-coverage gate can rely on it.
func TestIOPrimNames(t *testing.T) {
	names := IOPrimNames()
	if !sort.StringsAreSorted(names) {
		t.Errorf("IOPrimNames is not sorted: %v", names)
	}
	seen := map[string]bool{}
	for _, n := range names {
		if seen[n] {
			t.Errorf("IOPrimNames contains duplicate %q", n)
		}
		seen[n] = true
	}
	for n := range ioPrims {
		if !seen[n] {
			t.Errorf("ioPrims key %q missing from IOPrimNames", n)
		}
	}
	fams := [][]string{
		fileEnvPrims, streamPrims, dataPlanePrims, binPrims, netPrims, fsPrims,
		{"procRun", "sha256", "tlsGet"},
	}
	for _, fam := range fams {
		for _, n := range fam {
			if !seen[n] {
				t.Errorf("family prim %q missing from IOPrimNames", n)
			}
		}
	}
}
