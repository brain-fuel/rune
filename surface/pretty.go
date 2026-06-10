package surface

import (
	"strconv"
	"strings"

	"goforge.dev/rune/core"
)

// Precedence levels for the pretty-printer. Higher binds tighter.
const (
	precLow   = 0 // lambda, let, and the bodies of arrows
	precArrow = 1 // dependent and non-dependent function types
	precApp   = 2 // application
	precAtom  = 3 // variables, U, references, parenthesized forms
)

// Pretty turns a core term back into named surface syntax. This is the inverse
// direction of the three-representation split: the core is locally nameless, and
// exactly the layer you read gets names back.
//
// Bound variables are named from the Scope.Name hints carried for display, freshened
// on shadowing so the printed names never capture. Because names are reconstructed,
// parse . resolve . pretty . resolve is the identity on core up to the de Bruijn
// form — which is what the round-trip property asserts.
func Pretty(t core.Tm) string {
	return PrettyWith(t, nil)
}

// PrettyWith is Pretty with a map from definition hashes to names, used to render
// Ref nodes as the names the user wrote. An unknown reference prints as #<hash>.
func PrettyWith(t core.Tm, refNames map[core.Hash]string) string {
	p := &printer{refNames: refNames}
	var sb strings.Builder
	p.print(&sb, t, nil, precLow)
	return sb.String()
}

type printer struct {
	refNames map[core.Hash]string
}

// print writes t at the given surrounding precedence. names is the in-scope binder
// list, innermost-first, so a Var index selects directly into it.
func (p *printer) print(sb *strings.Builder, t core.Tm, names []string, prec int) {
	switch x := t.(type) {
	case core.Var:
		if x.Idx >= 0 && x.Idx < len(names) {
			sb.WriteString(names[x.Idx])
		} else {
			sb.WriteString("?" + strconv.Itoa(x.Idx))
		}
	case core.Ref:
		if n, ok := p.refNames[x.Hash]; ok {
			sb.WriteString(n)
		} else {
			sb.WriteString("#" + x.Hash.Short())
		}
	case core.Univ:
		sb.WriteString("U")
		if x.Lvl > 0 {
			sb.WriteString(strconv.Itoa(x.Lvl))
		}
	case core.Meta:
		sb.WriteString("?" + strconv.Itoa(x.ID))
	case core.Prop:
		sb.WriteString("Prop")
	case core.Eq:
		p.wrap(sb, prec, precApp, func() {
			sb.WriteString("Eq ")
			p.print(sb, x.Ty, names, precAtom)
			sb.WriteByte(' ')
			p.print(sb, x.L, names, precAtom)
			sb.WriteByte(' ')
			p.print(sb, x.R, names, precAtom)
		})
	case core.Refl:
		p.wrap(sb, prec, precApp, func() {
			sb.WriteString("refl ")
			p.print(sb, x.Tm, names, precAtom)
		})
	case core.Subst:
		p.wrap(sb, prec, precApp, func() {
			sb.WriteString("subst ")
			for i, sub := range []core.Tm{x.A, x.X, x.Y, x.Prf, x.P, x.Px} {
				if i > 0 {
					sb.WriteByte(' ')
				}
				p.print(sb, sub, names, precAtom)
			}
		})
	case core.Cast:
		p.wrap(sb, prec, precApp, func() {
			sb.WriteString("cast ")
			p.print(sb, x.A, names, precAtom)
			sb.WriteByte(' ')
			p.print(sb, x.B, names, precAtom)
			sb.WriteByte(' ')
			p.print(sb, x.P, names, precAtom)
			sb.WriteByte(' ')
			p.print(sb, x.X, names, precAtom)
		})
	case core.App:
		p.wrap(sb, prec, precApp, func() {
			p.print(sb, x.Fn, names, precApp)
			sb.WriteByte(' ')
			if x.Icit == core.Impl {
				sb.WriteByte('{')
				p.print(sb, x.Arg, names, precLow)
				sb.WriteByte('}')
			} else {
				p.print(sb, x.Arg, names, precAtom)
			}
		})
	case core.Lam:
		// fn (x : U) (y : U) … is BODY end. The core lambda is un-annotated, so the
		// printer emits the sole base type U for every binder domain (GRAMMAR.md §8);
		// re-resolution discards it. The block is self-delimiting, hence an atom — it
		// needs no surrounding parentheses at any precedence.
		body := core.Tm(x)
		cur := names
		sb.WriteString("fn")
		for {
			lam, ok := body.(core.Lam)
			if !ok {
				break
			}
			n := fresh(lam.Body.Name, cur)
			open, close := " (", " : U)"
			if lam.Icit == core.Impl {
				open, close = " {", " : U}"
			}
			sb.WriteString(open)
			sb.WriteString(qtyPrefix(lam.Qty))
			sb.WriteString(n)
			sb.WriteString(close)
			cur = prepend(n, cur)
			body = lam.Body.Body
		}
		sb.WriteString(" is ")
		p.print(sb, body, cur, precLow)
		sb.WriteString(" end")
	case core.Pi:
		if x.Icit == core.Impl {
			// An implicit Pi always shows its binder: {x : A} -> B.
			p.wrap(sb, prec, precArrow, func() {
				n := fresh(x.Cod.Name, names)
				sb.WriteByte('{')
				sb.WriteString(qtyPrefix(x.Qty))
				sb.WriteString(n)
				sb.WriteString(" : ")
				p.print(sb, x.Dom, names, precLow)
				sb.WriteString("} -> ")
				p.print(sb, x.Cod.Body, prepend(n, names), precArrow)
			})
		} else if occursVar(x.Cod.Body, 0) || x.Qty != core.QMany {
			p.wrap(sb, prec, precArrow, func() {
				n := fresh(x.Cod.Name, names)
				sb.WriteByte('(')
				sb.WriteString(qtyPrefix(x.Qty))
				sb.WriteString(n)
				sb.WriteString(" : ")
				p.print(sb, x.Dom, names, precLow)
				sb.WriteString(") -> ")
				p.print(sb, x.Cod.Body, prepend(n, names), precArrow)
			})
		} else {
			p.wrap(sb, prec, precArrow, func() {
				// A domain whose printed tail is "(e : T)" — an annotation, or an
				// application whose last argument is one — collides with dependent-Pi
				// binder syntax once the "->" follows. Parenthesize it so the arrow
				// cannot bind into it.
				if arrowDomainAmbiguous(x.Dom) {
					sb.WriteByte('(')
					p.print(sb, x.Dom, names, precLow)
					sb.WriteByte(')')
				} else {
					p.print(sb, x.Dom, names, precApp)
				}
				sb.WriteString(" -> ")
				p.print(sb, x.Cod.Body, prepend("_", names), precArrow)
			})
		}
	case core.Let:
		p.wrap(sb, prec, precLow, func() {
			n := fresh(x.Body.Name, names)
			sb.WriteString("let ")
			sb.WriteString(n)
			if x.Ty != nil {
				sb.WriteString(" : ")
				p.print(sb, x.Ty, names, precLow)
			}
			sb.WriteString(" = ")
			p.print(sb, x.Val, names, precLow)
			sb.WriteString(" in ")
			p.print(sb, x.Body.Body, prepend(n, names), precLow)
		})
	case core.Ann:
		// The annotation colon binds tighter than lambda/let/arrow, so the term must
		// print at application precedence or a lambda body would re-absorb the ": Ty".
		sb.WriteByte('(')
		p.print(sb, x.Term, names, precApp)
		sb.WriteString(" : ")
		p.print(sb, x.Ty, names, precLow)
		sb.WriteByte(')')
	default:
		sb.WriteString("?")
	}
}

// DebugCore renders a core term in explicit locally-nameless form: bound variables as
// their de Bruijn index (#n), references as @hash, binders unnamed (λ, Π). It is the
// REPL's :core debug view and deliberately is not a sentence of the surface grammar.
func DebugCore(t core.Tm) string {
	var sb strings.Builder
	debugCore(&sb, t)
	return sb.String()
}

func debugCore(sb *strings.Builder, t core.Tm) {
	switch x := t.(type) {
	case core.Var:
		sb.WriteString("#")
		sb.WriteString(strconv.Itoa(x.Idx))
	case core.Ref:
		sb.WriteString("@")
		sb.WriteString(x.Hash.Short())
	case core.Univ:
		sb.WriteString("U")
		if x.Lvl > 0 {
			sb.WriteString(strconv.Itoa(x.Lvl))
		}
	case core.App:
		sb.WriteByte('(')
		debugCore(sb, x.Fn)
		sb.WriteByte(' ')
		debugCore(sb, x.Arg)
		sb.WriteByte(')')
	case core.Lam:
		sb.WriteString("(λ. ")
		debugCore(sb, x.Body.Body)
		sb.WriteByte(')')
	case core.Pi:
		sb.WriteString("(Π ")
		debugCore(sb, x.Dom)
		sb.WriteString(". ")
		debugCore(sb, x.Cod.Body)
		sb.WriteByte(')')
	case core.Let:
		sb.WriteString("(let")
		if x.Ty != nil {
			sb.WriteString(" : ")
			debugCore(sb, x.Ty)
		}
		sb.WriteString(" = ")
		debugCore(sb, x.Val)
		sb.WriteString(" in ")
		debugCore(sb, x.Body.Body)
		sb.WriteByte(')')
	case core.Ann:
		sb.WriteByte('(')
		debugCore(sb, x.Term)
		sb.WriteString(" : ")
		debugCore(sb, x.Ty)
		sb.WriteByte(')')
	default:
		sb.WriteString("?")
	}
}

func (p *printer) wrap(sb *strings.Builder, outer, self int, body func()) {
	if outer > self {
		sb.WriteByte('(')
		body()
		sb.WriteByte(')')
		return
	}
	body()
}

func prepend(name string, names []string) []string {
	out := make([]string, 0, len(names)+1)
	out = append(out, name)
	return append(out, names...)
}

// fresh picks a display name from a hint that does not shadow any name in scope, so
// the printed term never captures. An empty or "_" hint becomes "x".
func fresh(hint string, names []string) string {
	base := hint
	if base == "" || base == "_" {
		base = "x"
	}
	if !contains(names, base) {
		return base
	}
	for i := 0; ; i++ {
		cand := base + strconv.Itoa(i)
		if !contains(names, cand) {
			return cand
		}
	}
}

func contains(xs []string, s string) bool {
	for _, x := range xs {
		if x == s {
			return true
		}
	}
	return false
}

// arrowDomainAmbiguous reports whether t, printed in a non-dependent arrow's domain
// position, would end in a "(e : T)" group that the parser reads as a dependent-Pi
// binder once "->" follows. That happens when t is an annotation, or an application
// whose final argument is one (the final argument prints as "(e : T)").
func arrowDomainAmbiguous(t core.Tm) bool {
	switch x := t.(type) {
	case core.Ann:
		return true
	case core.App:
		_, ok := x.Arg.(core.Ann)
		return ok
	default:
		return false
	}
}

// occursVar reports whether the de Bruijn variable at the given depth occurs in t.
// The pretty-printer uses depth 0 on a Pi codomain to decide between the dependent
// "(x : A) -> B" form and the non-dependent "A -> B" form.
func occursVar(t core.Tm, depth int) bool {
	switch x := t.(type) {
	case core.Var:
		return x.Idx == depth
	case core.App:
		return occursVar(x.Fn, depth) || occursVar(x.Arg, depth)
	case core.Lam:
		return occursVar(x.Body.Body, depth+1)
	case core.Pi:
		return occursVar(x.Dom, depth) || occursVar(x.Cod.Body, depth+1)
	case core.Let:
		return (x.Ty != nil && occursVar(x.Ty, depth)) ||
			occursVar(x.Val, depth) || occursVar(x.Body.Body, depth+1)
	case core.Ann:
		return occursVar(x.Term, depth) || occursVar(x.Ty, depth)
	default:
		return false
	}
}

// qtyPrefix renders a binder's quantity annotation ("0 ", "1 ", or nothing).
func qtyPrefix(q core.Qty) string {
	switch q {
	case core.QZero:
		return "0 "
	case core.QOne:
		return "1 "
	default:
		return ""
	}
}
