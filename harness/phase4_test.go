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

// TestTemplate is the Phase-4 text/template gate: a template as a Node sum
// (literal | substitution) rendered against an Env of key=value data (ch504),
// pure-wootz, byte-identical on all 8 backends. One template renders against two
// bindings: "Hello, Ada! You have 3 messages." / "Hello, Bob! You have 7 messages.".
func TestTemplate(t *testing.T) {
	const want = "\"Hello, Ada! You have 3 messages.\"\n" +
		"\"Hello, Bob! You have 7 messages.\"\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch504_template.rune", "main", ""); got != want {
				t.Fatalf("[%s] template gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch504_template.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch504_template.rune", want) })
}

// TestFlag is the Phase-4 flag gate: command-line flag parsing over an arg list
// (ch505) — `-name value` pairs walked into an Env, bare tokens positional.
// Pure-wootz, byte-identical on all 8 backends. Parses [-name Ada -count 7
// file.txt] and looks up name/count/missing -> "Ada"/"7"/"".
func TestFlag(t *testing.T) {
	const want = "\"Ada\"\n\"7\"\n\"\"\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch505_flag.rune", "main", ""); got != want {
				t.Fatalf("[%s] flag gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch505_flag.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch505_flag.rune", want) })
}

// TestCompress is the Phase-4 compress gate: a run-length codec over Bin (ch506)
// whose contract is the round-trip identity decode(encode x) = x. Pure-wootz,
// byte-identical on all 8 backends. Compresses "aaaabbbcca" (10 -> 8 bytes) and
// confirms the round-trip (1) + the decoded string.
func TestCompress(t *testing.T) {
	const want = "8\n10\n1\n\"aaaabbbcca\"\nunit"
	for _, bk := range ioOSBackends {
		bk := bk
		t.Run(bk.name, func(t *testing.T) {
			if _, err := exec.LookPath(bk.bin); err != nil {
				t.Skipf("%s not in PATH", bk.bin)
			}
			if got := runIOListing(t, bk, "ch506_compress.rune", "main", ""); got != want {
				t.Fatalf("[%s] compress gave %q, want %q", bk.name, got, want)
			}
		})
	}
	t.Run("native", func(t *testing.T) { runBytesNative(t, "ch506_compress.rune", want) })
	t.Run("jvm", func(t *testing.T) { runBytesJVM(t, "ch506_compress.rune", want) })
}
