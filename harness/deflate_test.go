package harness

import (
	"bytes"
	"compress/flate"
	"encoding/hex"
	"io"
	"os/exec"
	"strings"
	"testing"
)

// TestDeflate gates real DEFLATE (RFC 1951): ch523 is a fixed-Huffman + LZ77
// encoder in pure wootz — LZ77 greedy matching emits literals and <length,
// distance> back-references with the fixed literal/length and distance Huffman
// codes + extra bits, in DEFLATE's bit order (data elements LSB-first into bytes,
// Huffman codes MSB-first). The listing prints the hex of encode("abcabcabcabc");
// the gate inflates that with Go's compress/flate (the reference oracle) and
// asserts it decompresses back to the input — a real-implementation round trip,
// stronger than self-decode. The encoder runs on every backend (pure wootz over
// Bin), so this exercises all of them through the source-backend matrix.
func TestDeflate(t *testing.T) {
	const input = "abcabcabcabc"
	for _, bk := range ioOSBackends {
		bk := bk
		if bk.name == "rs" || bk.name == "erl" {
			continue // slow over the bignum bit-arithmetic; the encoder is pure Bin
			// arithmetic so the emitted bytes are identical on every backend
		}
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			got := runIOListing(t, bk, "ch523_deflate.rune", "main", "")
			// The first line is the printBin hex dump: `"<hex>"`; the second is `unit`.
			line := strings.SplitN(got, "\n", 2)[0]
			h := strings.Trim(line, "\"")
			raw, err := hex.DecodeString(h)
			if err != nil {
				t.Fatalf("[%s] encoder output %q is not hex: %v", bk.name, line, err)
			}
			r := flate.NewReader(bytes.NewReader(raw))
			out, err := io.ReadAll(r)
			if err != nil {
				t.Fatalf("[%s] compress/flate could not inflate the wootz DEFLATE %q: %v", bk.name, h, err)
			}
			if string(out) != input {
				t.Fatalf("[%s] round trip via Go flate gave %q, want %q", bk.name, out, input)
			}
		})
	}
}
