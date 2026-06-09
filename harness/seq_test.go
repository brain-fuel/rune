package harness

import (
	"strings"
	"testing"

	"pgregory.net/rapid"

	"goforge.dev/rune/core"
	"goforge.dev/rune/surface"
)

// closedText draws a closed expression and returns its canonical printed form — a
// self-contained sentence of the grammar, safe to embed (parenthesized) as a seq
// binding value or result.
func closedText(t *rapid.T) string {
	e := genExp(t, nil, 3)
	r := &surface.Resolver{}
	c, err := r.ResolveExp(e)
	if err != nil {
		t.Fatalf("resolve failed on generated term: %v", err)
	}
	return surface.Pretty(c)
}

var separators = []string{"\n", ";", "\n\n", " ; ", ";\n", "\n  "}

func sep(t *rapid.T) string {
	return rapid.SampledFrom(separators).Draw(t, "sep")
}

// TestSeqDesugarsToNestedLet is the §6 sequencing law: a seq block and its nested
// let … in … desugaring resolve to identical core. Each draw builds the SAME binds
// and result two ways — as a seq (with randomly chosen newline / ';' separators and
// optional leading/trailing separators, §5.3) and as the hand-written let chain —
// then asserts equal content hashes. It exercises the seq parser, the separator
// rules, typed and untyped SeqBinds, and the desugaring itself.
func TestSeqDesugarsToNestedLet(t *testing.T) {
	rapid.Check(t, func(t *rapid.T) {
		k := rapid.IntRange(0, 3).Draw(t, "binds")

		type bind struct {
			name, ty, val string
		}
		binds := make([]bind, k)
		for i := range binds {
			b := bind{name: "v" + itoa(i), val: closedText(t)}
			if rapid.Bool().Draw(t, "typed") {
				b.ty = closedText(t)
			}
			binds[i] = b
		}
		result := closedText(t)

		// seq form: optional leading separator, items separated, optional trailing.
		var seqB strings.Builder
		seqB.WriteString("seq")
		seqB.WriteString(sep(t))
		for _, b := range binds {
			seqB.WriteString("let ")
			seqB.WriteString(b.name)
			if b.ty != "" {
				seqB.WriteString(" : (")
				seqB.WriteString(b.ty)
				seqB.WriteString(")")
			}
			seqB.WriteString(" = (")
			seqB.WriteString(b.val)
			seqB.WriteString(")")
			seqB.WriteString(sep(t))
		}
		seqB.WriteString("(")
		seqB.WriteString(result)
		seqB.WriteString(")")
		seqB.WriteString(sep(t))
		seqB.WriteString("end")

		// nested let … in … form, innermost-first.
		letForm := "(" + result + ")"
		for i := len(binds) - 1; i >= 0; i-- {
			b := binds[i]
			ty := ""
			if b.ty != "" {
				ty = " : (" + b.ty + ")"
			}
			letForm = "let " + b.name + ty + " = (" + b.val + ") in " + letForm
		}

		cSeq := resolveText(t, seqB.String())
		cLet := resolveText(t, letForm)
		if core.HashTerm(cSeq) != core.HashTerm(cLet) {
			t.Fatalf("seq and its let desugaring differ\n  seq: %s\n  let: %s\n  seqHash: %s\n  letHash: %s",
				seqB.String(), letForm, core.HashTerm(cSeq), core.HashTerm(cLet))
		}
	})
}

func resolveText(t *rapid.T, src string) core.Tm {
	e, err := surface.ParseExpr(src)
	if err != nil {
		t.Fatalf("parse failed for %q: %v", src, err)
	}
	r := &surface.Resolver{}
	c, err := r.ResolveExp(e)
	if err != nil {
		t.Fatalf("resolve failed for %q: %v", src, err)
	}
	return c
}
