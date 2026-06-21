package infra

import (
	"fmt"
	"strings"
)

// hcl is a tiny deterministic HCL writer (blocks + attributes, two-space indent)
// that emits CANONICAL HCL — consecutive attributes get their `=` aligned and empty
// blocks render inline — so the output passes `tofu fmt -check` / `terraform fmt
// -check` and is stable for golden tests.
type hcl struct {
	b      strings.Builder
	indent int
	pend   [][2]string // buffered consecutive attributes awaiting `=` alignment
}

func (h *hcl) pad() { h.b.WriteString(strings.Repeat("  ", h.indent)) }

// flush writes any buffered attributes with their `=` aligned to the widest key.
func (h *hcl) flush() {
	if len(h.pend) == 0 {
		return
	}
	w := 0
	for _, kv := range h.pend {
		if len(kv[0]) > w {
			w = len(kv[0])
		}
	}
	for _, kv := range h.pend {
		h.pad()
		fmt.Fprintf(&h.b, "%-*s = %s\n", w, kv[0], kv[1])
	}
	h.pend = nil
}

// line writes one indented raw line (flushing pending attributes first).
func (h *hcl) line(format string, args ...any) {
	h.flush()
	h.pad()
	fmt.Fprintf(&h.b, format, args...)
	h.b.WriteByte('\n')
}

// open writes `<header> {` and indents.
func (h *hcl) open(format string, args ...any) {
	h.flush()
	h.pad()
	fmt.Fprintf(&h.b, format, args...)
	h.b.WriteString(" {\n")
	h.indent++
}

// emptyBlock writes an inline empty block `<header> {}` (HCL-canonical form).
func (h *hcl) emptyBlock(format string, args ...any) {
	h.flush()
	h.pad()
	fmt.Fprintf(&h.b, format, args...)
	h.b.WriteString(" {}\n")
}

// attr buffers a `key = <value>` attribute; value is a verbatim HCL expression
// (use str for a quoted string literal). Buffered attributes are `=`-aligned at the
// next flush (any open/close/line/blank).
func (h *hcl) attr(key, value string) { h.pend = append(h.pend, [2]string{key, value}) }

// close dedents and writes the closing brace.
func (h *hcl) close() {
	h.flush()
	h.indent--
	h.pad()
	h.b.WriteString("}\n")
}

// blank writes a separating blank line.
func (h *hcl) blank() {
	h.flush()
	h.b.WriteByte('\n')
}

func (h *hcl) String() string {
	h.flush()
	return h.b.String()
}

// str quotes a string as an HCL literal.
func str(s string) string { return fmt.Sprintf("%q", s) }
