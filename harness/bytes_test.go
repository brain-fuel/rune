package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

const binWant = "\"h\\x00\\xff\\xc3\\xa9\"\n5\n255\nunit"

// runBytesNative emits, compiles, and runs `listing` on the C and LLVM backends,
// asserting `want` on each (the native half of a Bin conformance gate).
func runBytesNative(t *testing.T, listing, want string) {
	t.Helper()
	s := loadListing(t, listing)
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	t.Run("c", func(t *testing.T) {
		if _, err := exec.LookPath("cc"); err != nil {
			t.Skip("cc not in PATH")
		}
		dir := t.TempDir()
		src, _ := codegen.C{}.Emit(p)
		f := filepath.Join(dir, "main.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(src), 0o644)
		if out, err := exec.Command("cc", "-o", bin, f).CombinedOutput(); err != nil {
			t.Fatalf("[c] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[c] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Fatalf("[c] gave %q, want %q", got, want)
		}
	})
	t.Run("ll", func(t *testing.T) {
		if _, err := exec.LookPath("clang"); err != nil {
			t.Skip("clang not in PATH")
		}
		dir := t.TempDir()
		ll, _ := codegen.LL{}.Emit(p)
		rt := codegen.LL{}.EmitRuntimeFor(p)
		f := filepath.Join(dir, "main.ll")
		rtf := filepath.Join(dir, "runtime.c")
		bin := filepath.Join(dir, "main.bin")
		os.WriteFile(f, []byte(ll), 0o644)
		os.WriteFile(rtf, []byte(rt), 0o644)
		if out, err := exec.Command("clang", f, rtf, "-o", bin).CombinedOutput(); err != nil {
			t.Fatalf("[ll] compile: %v\n%s", err, out)
		}
		out, err := exec.Command(bin).Output()
		if err != nil {
			t.Fatalf("[ll] run: %v", err)
		}
		if got := strings.TrimSpace(string(out)); got != want {
			t.Fatalf("[ll] gave %q, want %q", got, want)
		}
	})
}

// runBytesJVM emits, compiles, and runs `listing` on the JVM backend (Java 25).
func runBytesJVM(t *testing.T, listing, want string) {
	t.Helper()
	javac25, java25, ok := findJava25()
	if !ok {
		t.Skip("no JDK 25 (asdf temurin-25)")
	}
	s := loadListing(t, listing)
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatal(err)
	}
	src, err := codegen.JVM{}.Emit(p)
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	f := filepath.Join(dir, "main.java")
	if err := os.WriteFile(f, []byte(src), 0o644); err != nil {
		t.Fatal(err)
	}
	if out, err := exec.Command(javac25, "--release", "25", "-d", dir, f).CombinedOutput(); err != nil {
		t.Fatalf("javac: %v\n%s\n--- emitted ---\n%s", err, out, src)
	}
	out, err := exec.Command(java25, "-cp", dir, "main").Output()
	if err != nil {
		t.Fatalf("java run: %v", err)
	}
	if got := strings.TrimSpace(string(out)); got != want {
		t.Fatalf("[jvm] gave %q, want %q", got, want)
	}
}

// TestBytesOps is the Phase-0b gate: the bytes/strings library (bytesEq/bytesConcat,
// ch484) written in wootz over the Bin carrier runs byte-identically on all 8
// backends — proving Bin is programmable, not just a value the runtime ships.

// textWant is ch484_text's observable: Concat ("foobar"), Atoi (42), Itoa ("255"),
// a fmt-style formatted string ("1+2=3"), RuneCount of "héllo" (5), bufio line count
// of "a\nbb\nccc" (3), the IO unit.
const textWant = "\"foobar\"\n42\n\"255\"\n\"1+2=3\"\n5\n3\nunit"

// TestText is the Phase-0b gate: the consolidated text stdlib (bytes/strings/strconv/
// unicode/bufio + the io/fmt/errors tail, ch484_text) — one library with the NatElim
// arithmetic kit defined ONCE and called by every op — runs byte-identically on all 8
// backends. Proves Bin is programmable, not just a value the runtime ships.
func TestText(t *testing.T) {
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch484_text.rune", "main", ""); got != textWant {
				t.Fatalf("[%s] text stdlib gave %q, want %q", bk.name, got, textWant)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch484_text.rune", textWant) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch484_text.rune", textWant) })
}

// TestBytesConformance is the Phase-0 real-byte-string gate (Go-stdlib parity):
// the `Bin` carrier — a length-prefixed sequence of ARBITRARY bytes, the honest
// counterpart to the packed-Nat String — and its host ops (binEmpty/binCons/
// binLen/binAt/printBin) run byte-identically across the source backends. ch483
// builds the bytes h, NUL, 0xFF (invalid UTF-8), 0xC3 0xA9 ("é") — the NUL /
// invalid / multibyte cases a naive carrier mishandles — then shows them, the
// length (5), the byte at index 2 (255), and the IO unit. The observable is
// identical on js/py/go/rs/erl, proving the canonical $show and the byte carrier
// agree on every backend. (JVM + native C/LLVM follow in their own gates.)
func TestBytesConformance(t *testing.T) {
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch483_bytes.rune", "main", ""); got != binWant {
				t.Fatalf("[%s] Bin run gave %q, want %q", bk.name, got, binWant)
			}
		})
	}
}

// TestBytesNative carries the Phase-0 Bin carrier to the NATIVE backends: the same
// byte string (incl NUL / invalid UTF-8 / multibyte) runs on C (a K_BYTES GC object,
// self-contained) and LLVM-IR (the same K_BYTES in the linked runtime.c from
// EmitRuntimeFor, clang), byte-identical to the source backends.
func TestBytesNative(t *testing.T) { runBytesNative(t, "ch483_bytes.rune", binWant) }

// TestBytesJVM carries the Phase-0 Bin carrier to the JVM backend (the value domain
// gained a VBytes(int[]) record): ch483 prints the same bytes / length / index / unit
// the source + native backends do. Needs Java 25 (sealed records, virtual threads).
func TestBytesJVM(t *testing.T) { runBytesJVM(t, "ch483_bytes.rune", binWant) }

// TestBytesWasm gates the Bin vocabulary on the WASM backend (6c / Task 3): ch483 under
// wasmtime must print byte-identically to the 8-way reference (binWant). ch483 exercises
// printBin (the K_BIN `$show` arm's only path here; a raw Bin never reaches `$show` via
// main's return value since main's result is Unit) plus binLen/binAt over a Bin built by
// a binCons chain (the surface `b"..."` literal desugars to binCons/binEmpty, not to a
// LitBytes IR node -- see wasm.go's emitLit for the LitBytes path, landed defensively
// with no current consumer).
func TestBytesWasm(t *testing.T) {
	wt := wasmtimePathHarness()
	if wt == "" {
		t.Skip("wasmtime not found")
	}
	got, _ := runWasmListing(t, "ch483_bytes.rune", "main", "")
	if got != binWant {
		t.Fatalf("wasm bin divergence:\n got %q\nwant %q", got, binWant)
	}
}
