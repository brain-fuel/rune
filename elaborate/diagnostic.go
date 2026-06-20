package elaborate

import (
	"sort"
	"strings"
)

// A Diagnostic is a human-facing error: a one-line Summary that says plainly what
// went wrong, a Body that explains WHY in the reader's terms (Winston: name the
// parts, tell the small story), and Hints that point at the fix or ask the question
// that leads there (Socrates: the right question over the bare verdict; Elm/Rust:
// concrete, actionable, kind). It is an ordinary `error`, so it threads through the
// existing pipeline untouched — the REPL and file loader print Error() as-is.
//
// The renderer mirrors the shape Elm and Rust converged on: the verdict first, then
// the explanation set off and indented, then help. Lines wrap at a terminal-friendly
// width so paragraphs read as prose, not as one long machine string.
type Diagnostic struct {
	Summary string   // the headline: what went wrong, in one plain sentence
	Body    []string // why it went wrong — each entry is a paragraph
	Hints   []string // how to fix it / the question that leads there
}

const diagWidth = 74

func (d *Diagnostic) Error() string {
	var sb strings.Builder
	sb.WriteString(d.Summary)
	for _, para := range d.Body {
		if strings.TrimSpace(para) == "" {
			continue
		}
		sb.WriteString("\n\n")
		sb.WriteString(indentWrap(para, "  ", diagWidth))
	}
	for _, h := range d.Hints {
		if strings.TrimSpace(h) == "" {
			continue
		}
		sb.WriteString("\n\n")
		sb.WriteString(indentWrap("help: "+h, "  ", diagWidth))
	}
	return sb.String()
}

// indentWrap word-wraps s to width columns (counting the indent) and prefixes every
// line with indent. A line is never split inside a word; a single over-long word
// (e.g. a pretty-printed type) is left intact rather than mangled.
func indentWrap(s, indent string, width int) string {
	avail := width - len(indent)
	if avail < 20 {
		avail = 20
	}
	var out []string
	for _, raw := range strings.Split(s, "\n") {
		words := strings.Fields(raw)
		if len(words) == 0 {
			out = append(out, strings.TrimRight(indent, " "))
			continue
		}
		line := words[0]
		for _, w := range words[1:] {
			if len(line)+1+len(w) > avail {
				out = append(out, indent+line)
				line = w
			} else {
				line += " " + w
			}
		}
		out = append(out, indent+line)
	}
	return strings.Join(out, "\n")
}

// suggest returns the names from candidates closest to target by edit distance, best
// first, for a "did you mean?" hint. It keeps only genuinely close matches (distance
// within a third of the name's length, and at most 2 absolute) so a wild typo offers
// nothing rather than a misleading guess. At most three are returned.
func suggest(target string, candidates []string) []string {
	type scored struct {
		name string
		dist int
	}
	var hits []scored
	seen := map[string]bool{}
	for _, c := range candidates {
		if c == "" || c == target || seen[c] {
			continue
		}
		seen[c] = true
		d := editDistance(target, c)
		limit := len(target) / 3
		if limit > 2 {
			limit = 2
		}
		if limit < 1 {
			limit = 1
		}
		if d <= limit {
			hits = append(hits, scored{c, d})
		}
	}
	sort.Slice(hits, func(i, j int) bool {
		if hits[i].dist != hits[j].dist {
			return hits[i].dist < hits[j].dist
		}
		return hits[i].name < hits[j].name
	})
	var out []string
	for i, h := range hits {
		if i >= 3 {
			break
		}
		out = append(out, h.name)
	}
	return out
}

// unboundError builds the diagnostic for a name that resolves to nothing — the most
// common error a newcomer hits. It gathers every name actually in scope (local
// binders first, then top-level definitions) and offers the closest as a "did you
// mean?". With no near match it falls back to the Socratic nudge that explains the
// one rule a beginner trips on: rune resolves names top to bottom, so a definition
// must come before its use.
func (e *Elaborator) unboundError(c *Ctx, name string) error {
	d := &Diagnostic{
		Summary: "I can't find `" + name + "` in scope.",
		Body: []string{
			"You used `" + name + "` here, but nothing with that name is in scope: " +
				"it is neither a local binder (a `fn`/`let`/`case` variable) nor a " +
				"top-level definition I have seen.",
		},
	}
	if hits := suggest(name, e.inScopeNames(c)); len(hits) > 0 {
		d.Hints = append(d.Hints, "Did you mean "+orList(hits)+"?")
	} else {
		d.Hints = append(d.Hints,
			"Is `"+name+"` defined yet? rune reads top to bottom, so a name must be "+
				"introduced (as a definition above, or a binder around this expression) "+
				"before the line that uses it. Check the spelling and the order.")
	}
	return d
}

// inScopeNames lists every name a reference could legally resolve to right here:
// the local binders (innermost first) followed by the top-level definitions. It is
// the candidate pool for "did you mean?".
func (e *Elaborator) inScopeNames(c *Ctx) []string {
	names := make([]string, 0, len(c.names)+len(e.Refs))
	if c != nil {
		names = append(names, c.names...)
	}
	for n := range e.Refs {
		names = append(names, n)
	}
	return names
}

// editDistance is the Levenshtein distance between a and b, case-insensitive (a
// capitalisation slip is a near miss, not a far one).
func editDistance(a, b string) int {
	a, b = strings.ToLower(a), strings.ToLower(b)
	ra, rb := []rune(a), []rune(b)
	prev := make([]int, len(rb)+1)
	for j := range prev {
		prev[j] = j
	}
	for i := 1; i <= len(ra); i++ {
		cur := make([]int, len(rb)+1)
		cur[0] = i
		for j := 1; j <= len(rb); j++ {
			cost := 1
			if ra[i-1] == rb[j-1] {
				cost = 0
			}
			cur[j] = min3(prev[j]+1, cur[j-1]+1, prev[j-1]+cost)
		}
		prev = cur
	}
	return prev[len(rb)]
}

func min3(a, b, c int) int {
	if b < a {
		a = b
	}
	if c < a {
		a = c
	}
	return a
}

// orList renders names as a human "a, b, or c" phrase, each backticked.
func orList(names []string) string {
	for i := range names {
		names[i] = "`" + names[i] + "`"
	}
	switch len(names) {
	case 0:
		return ""
	case 1:
		return names[0]
	case 2:
		return names[0] + " or " + names[1]
	default:
		return strings.Join(names[:len(names)-1], ", ") + ", or " + names[len(names)-1]
	}
}
