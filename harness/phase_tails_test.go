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

// TestJSON is a Phase-2 tail gate: encoding/json Marshal of a nested value
// (ch513) — arrays/objects folded into a single recursive JVal as cons-chains,
// serialized to canonical JSON over Bin. Byte-identical on all 8 backends.
// (printBin escapes the quote bytes as \x22 in its display; the bytes are JSON.)
func TestJSON(t *testing.T) {
	const want = "\"{\\x22name\\x22:\\x22Ada\\x22,\\x22tags\\x22:[1,2,3]}\"\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch513_json.rune", "main", ""); got != want {
				t.Fatalf("[%s] json gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch513_json.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch513_json.rune", want) })
}

// TestSha256Pure is a Phase-3 tail gate: SHA-256 in PURE wootz (ch514), no host
// crypto. It reproduces the NIST vector sha256("abc"). Its reason to exist is the
// crypto-LESS backends — rust + native C/LLVM ship no digest, and the host-FFI
// sha256 (ch497) does not cover them; this does, plus go as a fast oracle. The
// 32-bit-word math runs over the builtin bignum, so it is SLOW (the int64
// fast-lane that would make divmod-by-2 cheap is parked in the kernel); a bit-list
// representation is the documented speedup. Skipped under -short.
func TestSha256Pure(t *testing.T) {
	if testing.Short() {
		t.Skip("ch514 pure sha256 is slow over the bignum tower; -short skips it")
	}
	const want = "\"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad\"\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		if bk.name != "go" && bk.name != "rs" {
			continue // go = fast oracle; rs = a crypto-less target. others have host sha256.
		}
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch514_sha256.rune", "main", ""); got != want {
				t.Fatalf("[%s] sha256 gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch514_sha256.rune", want) })
}
