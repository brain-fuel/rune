package harness

import (
	"os/exec"
	"testing"
)

// TestRand is a Phase-2 tail gate: a seeded LCG PRNG (ch508), deterministic so
// byte-identical on all 8 backends. The big multiply rides the arithmetic accel.
func TestRand(t *testing.T) {
	const want = "90\n75\n84\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch508_rand.rune", "main", ""); got != want {
				t.Fatalf("[%s] rand gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch508_rand.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch508_rand.rune", want) })
}

// TestCSV is a Phase-2 tail gate: split/join a CSV record over Bin (ch509),
// pure-wootz, round-trip joinComma(split x)=x, byte-identical on all 8 backends.
func TestCSV(t *testing.T) {
	const want = "3\n\"name\"\n1\n\"id,name,age\"\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch509_csv.rune", "main", ""); got != want {
				t.Fatalf("[%s] csv gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch509_csv.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch509_csv.rune", want) })
}
