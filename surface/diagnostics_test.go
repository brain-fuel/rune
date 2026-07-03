package surface

import (
	"errors"
	"strings"
	"testing"
)

func TestRenderParseErrorCaret(t *testing.T) {
	src := "foo : U is U U U )"
	err := errors.New("expected 'end', found ')' at offset 17")
	got := RenderParseError(src, err)
	// The "at offset N" tail is dropped in favour of the caret.
	if strings.Contains(got, "offset") {
		t.Errorf("offset tail not stripped:\n%s", got)
	}
	if !strings.Contains(got, "expected 'end', found ')'") {
		t.Errorf("message lost:\n%s", got)
	}
	if !strings.Contains(got, "1 | foo : U is U U U )") {
		t.Errorf("source line not shown:\n%s", got)
	}
	// The caret must sit under column 17 (the ')'): 4-space prefix ("  1 | ") then 17.
	lines := strings.Split(got, "\n")
	last := lines[len(lines)-1]
	caretCol := strings.IndexByte(last, '^')
	if caretCol < 0 {
		t.Fatalf("no caret in:\n%s", got)
	}
	// gutter "  1 | " is 6 chars; caret should be 6 + 17.
	if caretCol != 6+17 {
		t.Errorf("caret at col %d, want %d:\n%s", caretCol, 6+17, got)
	}
}

func TestRenderParseErrorMultiline(t *testing.T) {
	src := "a : U is U\nb : U is )"
	// offset of ')' = len("a : U is U\nb : U is ") = 20
	err := errors.New("unexpected ')' at offset 20")
	got := RenderParseError(src, err)
	if !strings.Contains(got, "2 | b : U is )") {
		t.Errorf("wrong line shown (want line 2):\n%s", got)
	}
}

func TestRenderParseErrorNoOffsetPassthrough(t *testing.T) {
	err := errors.New("some error with no position")
	if got := RenderParseError("x", err); got != err.Error() {
		t.Errorf("passthrough failed: got %q", got)
	}
	if got := RenderParseError("", errors.New("x at offset 0")); !strings.Contains(got, "x") {
		t.Errorf("empty src should pass message through: %q", got)
	}
}

// TestRenderParseErrorAliasOffset verifies that a parse error produced during
// alias parsing (which carries "at offset N") is rendered with a caret pointing
// at the problematic column. This mirrors what session.go does when it wraps a
// RenderParseError call around a LoadSource parse failure on an alias line.
func TestRenderParseErrorAliasOffset(t *testing.T) {
	// Simulate a source file containing a malformed alias directive.
	// The parser would report an error at the offset of the bad token.
	src := "alias Std.Float as\nalias Std.IO as IO"
	// Offset 18 is the newline after "as" (the alias name is missing).
	err := errors.New("expected identifier after 'as', found newline at offset 18")
	got := RenderParseError(src, err)
	// The "at offset N" tail must be replaced by the caret.
	if strings.Contains(got, "offset") {
		t.Errorf("offset tail should be stripped in favour of caret:\n%s", got)
	}
	// The rendered output must include the source line and a caret.
	if !strings.Contains(got, "alias Std.Float as") {
		t.Errorf("source line should appear in rendered error:\n%s", got)
	}
	if !strings.Contains(got, "^") {
		t.Errorf("caret must appear in rendered error:\n%s", got)
	}
	// The caret must be on the last line of the output.
	lines := strings.Split(got, "\n")
	last := lines[len(lines)-1]
	if !strings.Contains(last, "^") {
		t.Errorf("caret should be on the last line:\n%s", got)
	}
}
