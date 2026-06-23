package harness

import (
	"os/exec"
	"testing"
)

// TestEncoding is the Phase-2 encoding gate: hex + base64 codecs over Bin (ch491),
// verified against known vectors (hex "hi"=6869; base64 "foo"/"hi"/"f") byte-
// identically across all 8 backends. The div/mod args are small (<=256), so the
// native/Rust backends run fine even without div/mod accel.
func TestEncoding(t *testing.T) {
	const want = "\"6869\"\n\"Zm9v\"\n\"aGk=\"\n\"Zg==\"\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch491_encoding.rune", "main", ""); got != want {
				t.Fatalf("[%s] encoding gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch491_encoding.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch491_encoding.rune", want) })
}

// TestHash is the Phase-2 hash gate: FNV-1a 32-bit over Bin (ch492), verified against
// the standard vectors ("" / "a" / "foobar"). The per-byte step multiplies+mods a
// ~2^32 value, so it needs the div/mod (and add/mul) accel. Now on ALL 8 backends:
// js/py/go/erl + JVM (BigInteger), and Rust + native C/LLVM after adding schoolbook
// bignum long-division to their runtimes (closing the old numeric-tower div gap).
func TestHash(t *testing.T) {
	const want = "2166136261\n3826002220\n3214735720\n3214735720"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch492_hash.rune", "main", ""); got != want {
				t.Fatalf("[%s] fnv1a gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch492_hash.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch492_hash.rune", want) })
}

// TestDivMod validates the bignum long-division accel across base-1e9 limb boundaries
// (10^12/7 = 142857142857 r1; 10^18/10^9 = 10^9 r0) on all 8 backends — js/py/go/erl
// (host bigint), JVM (BigInteger), Rust + native C/LLVM (the new schoolbook divmod).
func TestDivMod(t *testing.T) {
	const want = "142857142857\n1\n1000000000\n0\n0"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch501_divmod.rune", "main", ""); got != want {
				t.Fatalf("[%s] divmod gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch501_divmod.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch501_divmod.rune", want) })
}

// TestSort is the Phase-2 sort gate: insertion sort over Bin (ch493) sorts "dbca" to
// "abcd" and confirms sortedness, byte-identically on all 8 backends. Uses only
// leb/monus (no division), so the native/Rust backends run it directly.
func TestSort(t *testing.T) {
	const want = "\"abcd\"\n1\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch493_sort.rune", "main", ""); got != want {
				t.Fatalf("[%s] sort gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch493_sort.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch493_sort.rune", want) })
}

// TestMathBits is the Phase-2 math/bits gate: popcount + bitwise and/or/xor built
// from div/mod by 2 (ch494), small operands so all 8 backends run it. popcount 11=3,
// 255=8; and/or/xor 12 10 = 8/14/6.
func TestMathBits(t *testing.T) {
	const want = "3\n8\n8\n14\n6\n6"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch494_mathbits.rune", "main", ""); got != want {
				t.Fatalf("[%s] math/bits gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch494_mathbits.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch494_mathbits.rune", want) })
}
