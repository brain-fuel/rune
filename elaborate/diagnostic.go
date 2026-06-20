package elaborate

import (
	"sort"
	"strings"

	"goforge.dev/rune/v3/core"
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

// typeMismatchError builds the diagnostic for a checked expression whose inferred
// type does not match what the context expects. It names both types plainly, and
// when the clash is a universe-level one — the mismatch that reads as nonsense
// ("expected U, got U1") to anyone who has not met the hierarchy — it explains what
// a universe level IS and which way to move. The raw unifier message is kept as a
// final precise note for the case the prose does not cover.
func (e *Elaborator) typeMismatchError(c *Ctx, want, got core.Val, cause error) error {
	wantS := e.pretty(c, want)
	gotS := e.pretty(c, got)
	d := &Diagnostic{Summary: "The types don't match here."}

	if wantS == gotS {
		// Same printed form, different cores (levels, metas, or quantities) — say so,
		// so the reader is not left comparing two identical-looking types.
		d.Body = []string{
			"This expression should have type `" + wantS + "`, and at a glance it does " +
				"— but the two differ in a detail the printer hides (a universe level, a " +
				"usage quantity, or an unsolved hole).",
		}
	} else {
		d.Body = []string{
			"I expected this expression to have type `" + wantS + "`, but it actually " +
				"has type `" + gotS + "`.",
		}
	}

	if isUniverseMismatch(cause) {
		d.Body = append(d.Body,
			"These are both universes (the types whose members are themselves types), "+
				"but at different LEVELS. `U` is `U0`, the universe of ordinary types; "+
				"`U1` is the next one up, large enough to contain `U0` itself. The levels "+
				"stop a type from containing itself, so they are not interchangeable.")
		d.Hints = append(d.Hints,
			"A value one level too big needs a bigger annotation (write `U1` where you "+
				"wrote `U`); a value one level too small means the surrounding type is "+
				"asking for more than it should. Move the annotation to match the level "+
				"the expression actually lives at.")
	} else {
		d.Hints = append(d.Hints,
			"One of the two is wrong: either this expression should be built differently "+
				"to produce a `"+wantS+"`, or the type it is checked against should be "+
				"`"+gotS+"`. Decide which you meant and change that one.")
	}

	if cause != nil {
		d.Hints = append(d.Hints, "(precise reason: "+cause.Error()+")")
	}
	return d
}

// reflMismatchError builds the diagnostic for a `refl` whose two sides are not
// definitionally equal — the daily error of a proof author. It shows what each side
// reduces to (the NbE normal form, so the reader sees exactly where they diverge)
// and asks the deciding question: are they really equal (rewrite, don't refl) or
// genuinely different (the statement is false)?
func (e *Elaborator) reflMismatchError(c *Ctx, eq core.VEq) error {
	lhs := e.pretty(c, eq.L)
	rhs := e.pretty(c, eq.R)
	d := &Diagnostic{Summary: "This `refl` does not prove the equation."}
	if lhs == rhs {
		d.Body = []string{
			"`refl` proves only that a thing equals itself. The two sides print the same " +
				"(`" + lhs + "`) yet are not definitionally equal — they differ in a detail " +
				"the printer hides (a universe level, an implicit argument, or a stuck term " +
				"that has not reduced).",
		}
	} else {
		d.Body = []string{
			"`refl` proves only that a thing equals itself, so both sides of the equation " +
				"must reduce to the SAME normal form — and these do not: the left-hand side " +
				"reduces to `" + lhs + "`, while the right-hand side reduces to `" + rhs + "`.",
		}
	}
	d.Hints = []string{
		"Either the two sides really are equal and rune cannot see it yet — then rewrite " +
			"one into the other with a lemma (`subst`, `cong`, or the relevant eliminator) " +
			"rather than `refl` — or they are genuinely different and the statement does not " +
			"hold. Which of the two is it?",
	}
	return d
}

// reflNonEqError builds the diagnostic for a `refl` checked against a type that is
// not an equality at all.
func (e *Elaborator) reflNonEqError(c *Ctx, want core.Val) error {
	return &Diagnostic{
		Summary: "`refl` was used where the expected type is not an equation.",
		Body: []string{
			"This position expects a value of type `" + e.pretty(c, want) + "`, but `refl` " +
				"only ever builds a proof of an equality `Eq T a a`. There is nothing for it " +
				"to prove here.",
		},
		Hints: []string{
			"If you meant to prove an equation, check the goal — it may have reduced to " +
				"something other than an `Eq`. Otherwise you want a different constructor or " +
				"a value of `" + e.pretty(c, want) + "`, not `refl`.",
		},
	}
}

// isUniverseMismatch reports whether a unifier error is a universe-level clash, so
// the diagnostic can switch to the level-aware explanation.
func isUniverseMismatch(err error) bool {
	return err != nil && strings.Contains(err.Error(), "universe level")
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
