package explain

import (
	"strings"
	"testing"
)

// TestRenderAnnotateNarrow: below the width threshold the English rides a
// `--` comment line above each code fragment.
func TestRenderAnnotateNarrow(t *testing.T) {
	s := load(t, doubleSrc)
	root, err := Explain(s, "main", Options{})
	if err != nil {
		t.Fatal(err)
	}
	want := "-- [Entrypoint: main]\n" +
		"main : IO Float\n" +
		"-- [Get Float `x` from Command Line]\n" +
		"getFloat\n" +
		"-- [Apply Function (fmul x (fromNat 2))]\n" +
		"fmul x (fromNat 2)\n" +
		"-- [Print Result to Command Line]\n" +
		"printFloat (fmul x (fromNat 2))\n"
	if got := RenderAnnotate(root, 60); got != want {
		t.Errorf("narrow:\ngot:\n%swant:\n%s", got, want)
	}
}

// TestRenderAnnotateWide: at or above the threshold, two columns aligned by
// line; the column is the longest code line (capped at half the width).
func TestRenderAnnotateWide(t *testing.T) {
	s := load(t, doubleSrc)
	root, err := Explain(s, "main", Options{})
	if err != nil {
		t.Fatal(err)
	}
	got := RenderAnnotate(root, 120)
	lines := strings.Split(strings.TrimRight(got, "\n"), "\n")
	if len(lines) != 4 {
		t.Fatalf("wide: got %d lines, want 4:\n%s", len(lines), got)
	}
	// Every English bracket opens at the same column: colW (31) + 2.
	for _, ln := range lines {
		if idx := strings.Index(ln, "["); idx != 33 {
			t.Errorf("wide: bracket at col %d, want 33 in %q", idx, ln)
		}
	}
	if !strings.HasPrefix(lines[0], "main : IO Float") {
		t.Errorf("wide first line = %q", lines[0])
	}
}

// TestRenderAnnotateWrap: a code fragment longer than the capped column
// hard-wraps onto continuation lines; the English stays on the first line.
func TestRenderAnnotateWrap(t *testing.T) {
	long := strings.Repeat("abcdefghij", 7) // 70 chars
	root := Step{Text: "Entrypoint: w", Code: "w : T", Kids: []Step{
		{Text: "Apply Function (long)", Code: long},
	}}
	got := RenderAnnotate(root, 100) // cap = (100-2)/2 = 49
	lines := strings.Split(strings.TrimRight(got, "\n"), "\n")
	if len(lines) != 3 {
		t.Fatalf("wrap: got %d lines, want 3 (2 rows + 1 continuation):\n%s", len(lines), got)
	}
	if lines[2] != long[49:] {
		t.Errorf("continuation = %q, want %q", lines[2], long[49:])
	}
	if !strings.Contains(lines[1], "[Apply Function (long)]") {
		t.Errorf("English missing from first chunk line: %q", lines[1])
	}
}
