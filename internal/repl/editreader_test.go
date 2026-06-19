package repl

import (
	"bufio"
	"bytes"
	"strings"
	"testing"
)

// newTestEditor builds an editReader whose input is a fixed byte script and whose
// output is captured — so the edit loop can be exercised without a real terminal
// (raw mode is only entered by ReadLine, which we bypass by calling edit directly).
func newTestEditor(input string, hist ...string) (*editReader, *bytes.Buffer) {
	var out bytes.Buffer
	e := &editReader{
		out:  &out,
		r:    bufio.NewReader(strings.NewReader(input)),
		hist: hist,
	}
	return e, &out
}

// TestEditReaderBasic checks printable insertion, backspace, and cursor editing all
// resolve to the right submitted line.
func TestEditReaderBasic(t *testing.T) {
	cases := []struct {
		name, input, want string
	}{
		{"plain", "abc\r", "abc"},
		{"backspace", "abX\x7fc\r", "abc"},              // type abX, backspace, c
		{"home-insert", "bc\x01a\r", "abc"},             // Ctrl-A then insert at start
		{"left-insert", "ac\x02b\r", "abc"},             // Ctrl-B (left) then insert b before c
		{"kill-to-start", "xyz\x15abc\r", "abc"},        // Ctrl-U wipes the line
		{"ctrl-c-abandons", "junk\x03\r", ""},           // Ctrl-C drops the line, next Enter submits empty
		{"delete-forward", "aXbc\x01\x06\x04\r", "abc"}, // home, right, Ctrl-D deletes the X
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			e, _ := newTestEditor(c.input)
			got, ok, err := e.edit("> ")
			if err != nil {
				t.Fatalf("edit error: %v", err)
			}
			if !ok {
				t.Fatalf("edit returned ok=false for %q", c.input)
			}
			if got != c.want {
				t.Errorf("edit(%q) = %q, want %q", c.input, got, c.want)
			}
		})
	}
}

// TestEditReaderEOF: Ctrl-D on an empty line is a clean end of input.
func TestEditReaderEOF(t *testing.T) {
	e, _ := newTestEditor("\x04")
	_, ok, err := e.edit("> ")
	if err != nil {
		t.Fatalf("edit error: %v", err)
	}
	if ok {
		t.Errorf("Ctrl-D on empty line should signal EOF (ok=false)")
	}
}

// TestEditReaderHistoryUp recalls a prior entry with the Up arrow (ESC [ A) and
// submits it unchanged.
func TestEditReaderHistoryUp(t *testing.T) {
	// Two history entries; Up once recalls the newest ("second"), Up again the older
	// ("first"), then Enter.
	e, _ := newTestEditor("\x1b[A\x1b[A\r", "first", "second")
	got, ok, err := e.edit("> ")
	if err != nil || !ok {
		t.Fatalf("edit err=%v ok=%v", err, ok)
	}
	if got != "first" {
		t.Errorf("two Up arrows should recall the oldest entry %q, got %q", "first", got)
	}
}

// TestEditReaderHistoryEditAppend recalls a history entry and appends to it.
func TestEditReaderHistoryEditAppend(t *testing.T) {
	e, _ := newTestEditor("\x10!\r", "hello") // Ctrl-P recalls "hello", append '!'
	got, ok, err := e.edit("> ")
	if err != nil || !ok {
		t.Fatalf("edit err=%v ok=%v", err, ok)
	}
	if got != "hello!" {
		t.Errorf("Ctrl-P recall + append = %q, want %q", got, "hello!")
	}
}

// TestEditReaderReverseSearch: Ctrl-R, type a substring, Enter accepts the match.
func TestEditReaderReverseSearch(t *testing.T) {
	// History has "double 21" and "gcd 12 18"; Ctrl-R then "gcd" matches the latter;
	// Enter accepts and submits it.
	e, _ := newTestEditor("\x12gcd\r", "double 21", "gcd 12 18")
	got, ok, err := e.edit("> ")
	if err != nil || !ok {
		t.Fatalf("edit err=%v ok=%v", err, ok)
	}
	if got != "gcd 12 18" {
		t.Errorf("reverse-search for %q accepted %q, want %q", "gcd", got, "gcd 12 18")
	}
}

// TestEditReaderReverseSearchCancel: Ctrl-R then Ctrl-G restores the original line.
func TestEditReaderReverseSearchCancel(t *testing.T) {
	// Type "keep", Ctrl-R, type "x" (no match), Ctrl-G to cancel, Enter submits "keep".
	e, _ := newTestEditor("keep\x12x\x07\r", "other")
	got, ok, err := e.edit("> ")
	if err != nil || !ok {
		t.Fatalf("edit err=%v ok=%v", err, ok)
	}
	if got != "keep" {
		t.Errorf("cancelled reverse-search should restore %q, got %q", "keep", got)
	}
}

// TestAddHistoryDedup: consecutive duplicates and blanks are not recorded.
func TestAddHistoryDedup(t *testing.T) {
	e := &editReader{} // no histPath ⇒ in-memory only
	e.AddHistory("a")
	e.AddHistory("a") // dup, skipped
	e.AddHistory("   ")
	e.AddHistory("b")
	if len(e.hist) != 2 || e.hist[0] != "a" || e.hist[1] != "b" {
		t.Errorf("history dedup/blank-skip failed: %#v", e.hist)
	}
}
