package harness

import (
	"os/exec"
	"testing"
)

// Task 1 added js; Task 2 adds py and go.
func floatIOBackends() []ioBackend {
	return ioCLIBackends[:3] // js, py, go
}

func TestIOFloatStdinConformance(t *testing.T) {
	const want = "6.28\n999\n999"
	for _, bk := range floatIOBackends() {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch566_float_io.rune", "main", "3.14\n"); got != want {
				t.Errorf("[%s] float stdin run gave %q, want %q", bk.name, got, want)
			}
		})
	}
}
