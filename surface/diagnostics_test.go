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
