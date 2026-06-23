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

// TestHeap is a Phase-2 tail gate: a min-priority queue (ch510) — push/findMin/
// deleteOne over a Nat list so pops extract in sorted order. Byte-identical all 8.
func TestHeap(t *testing.T) {
	const want = "1\n3\n4\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch510_heap.rune", "main", ""); got != want {
				t.Fatalf("[%s] heap gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch510_heap.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch510_heap.rune", want) })
}

// TestSlicesMaps is a Phase-2 tail gate: generic slice ops (Contains/Index/Max/
// Reverse) + an assoc map (Get/Len) over Nat (ch511). Byte-identical on all 8.
func TestSlicesMaps(t *testing.T) {
	const want = "1\n4\n5\n5\n20\n2\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch511_slicesmaps.rune", "main", ""); got != want {
				t.Fatalf("[%s] slicesmaps gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch511_slicesmaps.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch511_slicesmaps.rune", want) })
}

// TestBignum is a Phase-2 tail gate: math/big arbitrary precision (ch512) —
// 2^64, 20!, 25! computed exactly via the kernel bignum accel, full decimal,
// byte-identical on all 8 backends.
func TestBignum(t *testing.T) {
	const want = "18446744073709551616\n2432902008176640000\n15511210043330985984000000\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch512_bignum.rune", "main", ""); got != want {
				t.Fatalf("[%s] bignum gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch512_bignum.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch512_bignum.rune", want) })
}
