package harness

import (
	"os/exec"
	"testing"
)

// TestRegexp is the Phase-4 regexp gate: a Brzozowski-derivative regex engine
// over Bin (ch502), pure-wootz (no FFI), byte-identical on all 8 backends. It
// matches re1 = a(b|c)*d against "abccbd"/"ad"/"abx" (1/1/0) and re2 = a.c
// against "axc"/"ac" (1/0).
func TestRegexp(t *testing.T) {
	const want = "1\n1\n0\n1\n0\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch502_regexp.rune", "main", ""); got != want {
				t.Fatalf("[%s] regexp gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch502_regexp.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch502_regexp.rune", want) })
}

// TestLog is the Phase-4 log/slog gate: a structured logfmt logger over Bin
// (ch503), pure-wootz, byte-identical on all 8 backends. It formats two records
// to `level=info msg=started count=3` and `level=warn msg=retrying attempt=2 max=5`.
func TestLog(t *testing.T) {
	const want = "\"level=info msg=started count=3\"\n" +
		"\"level=warn msg=retrying attempt=2 max=5\"\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch503_log.rune", "main", ""); got != want {
				t.Fatalf("[%s] log gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch503_log.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch503_log.rune", want) })
}
