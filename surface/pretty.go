package surface

import (
	"strconv"
	"strings"

	"goforge.dev/rune/v3/core"
)

// Precedence levels for the pretty-printer. Higher binds tighter. They mirror
// GRAMMAR §3: arrow, then the additive and multiplicative operator levels, then
// application. (The '=' sugar is input-only and has no printing level; core Eq
// prints prefix, at application precedence.)
const (
	precLow   = 0 // lambda, let, and the bodies of arrows
	precArrow = 1 // dependent and non-dependent function types
	precAdd   = 2 // infix + -
	precMul   = 3 // infix * / %
	precApp   = 4 // application
	precAtom  = 5 // variables, U, references, parenthesized forms
)

// opPrec returns the printing precedence of an infix operator name, or -1 when
// the name is not in the closed operator table of GRAMMAR §3.
func opPrec(name string) int {
	switch name {
	case "+", "-":
		return precAdd
	case "*", "/", "//", "%":
		return precMul
	}
	return -1
}

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
	// natZero/natSucc are the constructor hashes of a registered `builtin nat`
	// binding; when set (foldNat), saturated succ-chains over zero print as
	// numerals (GRAMMAR §8). Digits re-parse to the same core under the same
	// binding, so the round-trip contract is preserved.
	natZero, natSucc core.Hash
	foldNat          bool
	// dec, when On, folds the REPL prelude's fraction/decimal results to their
	// positional notation: a `frac a b` prints `a/b`, and a `rdec` prints a
	// radix-point number with the repetend bracketed (1/3 -> 0.{3}). Display only.
	dec DecConfig
}

// DecConfig carries the REPL prelude's fraction/decimal constructor hashes so the
// printer can fold them to positional notation. On is false (and folding is off)
// unless every hash resolved (e.g. a bare `--no-prelude` session).
type DecConfig struct {
	Frac, RDec, Wcons, Wnil, True core.Hash
	Int, Ok, Err                  core.Hash
	On                            bool
}

// PrettyNat is PrettyWith with a registered `builtin nat` binding: saturated
// applications succ (… (succ zero)) print as numerals.
func PrettyNat(t core.Tm, refNames map[core.Hash]string, zero, succ core.Hash) string {
	return PrettyNatDec(t, refNames, zero, succ, DecConfig{})
}

// PrettyNatDec is PrettyNat that also folds the prelude's fraction/decimal results
// to positional notation (frac a b -> a/b; rdec -> a radix-point number).
func PrettyNatDec(t core.Tm, refNames map[core.Hash]string, zero, succ core.Hash, dc DecConfig) string {
	p := &printer{refNames: refNames, natZero: zero, natSucc: succ, foldNat: true, dec: dc}
	var sb strings.Builder
	p.print(&sb, t, nil, precLow)
	return sb.String()
}

// spineExpl peels a curried explicit application into (head, args-left-to-right).
func spineExpl(t core.Tm) (core.Tm, []core.Tm) {
	var args []core.Tm
	for {
		a, ok := t.(core.App)
		if !ok || a.Icit != core.Expl {
			return t, args
		}
		args = append([]core.Tm{a.Arg}, args...)
		t = a.Fn
	}
}

// wholeVal reads a Whole as an int: a folded succ-chain or a compressed NatLit.
func (p *printer) wholeVal(t core.Tm) (int, bool) {
	if n, ok := p.numeralOf(t); ok {
		return n, true
	}
	if nl, ok := t.(core.NatLit); ok && nl.N.IsInt64() {
		return int(nl.N.Int64()), true
	}
	return 0, false
}

// wlistVals reads a prelude WList (wcons/wnil over Whole) into its digit values.
func (p *printer) wlistVals(t core.Tm) ([]int, bool) {
	var ds []int
	for {
		if ref, ok := t.(core.Ref); ok && ref.Hash == p.dec.Wnil {
			return ds, true
		}
		head, args := spineExpl(t)
		ref, ok := head.(core.Ref)
		if !ok || ref.Hash != p.dec.Wcons || len(args) != 2 {
			return nil, false
		}
		d, ok := p.wholeVal(args[0])
		if !ok {
			return nil, false
		}
		ds = append(ds, d)
		t = args[1]
	}
}

func digitsStr(ds []int) string {
	var sb strings.Builder
	for _, d := range ds {
		sb.WriteByte(byte('0' + d))
	}
	return sb.String()
}

// decimalStr folds a saturated `frac`/`rdec` application to positional notation,
// returning ok=false for anything else (so it prints normally).
func (p *printer) decimalStr(t core.Tm) (string, bool) {
	if !p.dec.On {
		return "", false
	}
	head, args := spineExpl(t)
	ref, ok := head.(core.Ref)
	if !ok {
		return "", false
	}
	// A Bool flag arg is negative iff it is the `true` constructor.
	isNeg := func(t core.Tm) bool {
		r, ok := t.(core.Ref)
		return ok && r.Hash == p.dec.True
	}
	switch {
	case ref.Hash == p.dec.Int && len(args) == 2:
		// int sign mag : a signed integer; magnitude 0 is unsigned.
		a, ok := p.wholeVal(args[1])
		if !ok {
			return "", false
		}
		if a == 0 {
			return "0", true
		}
		if isNeg(args[0]) {
			return "-" + strconv.Itoa(a), true
		}
		return strconv.Itoa(a), true
	case ref.Hash == p.dec.Ok && len(args) == 3:
		// ok A E v : a successful Result — show the payload (the A E type args are
		// the first two explicit arguments). Fall through if the payload is not a
		// foldable numeric value.
		if inner, ok := p.decimalStr(args[2]); ok {
			return "ok " + inner, true
		}
		return "", false
	case ref.Hash == p.dec.Err && len(args) == 3:
		return "err", true
	case ref.Hash == p.dec.Frac && len(args) == 3:
		neg := isNeg(args[0])
		a, ok1 := p.wholeVal(args[1])
		b, ok2 := p.wholeVal(args[2])
		if !ok1 || !ok2 {
			return "", false
		}
		if a == 0 {
			return "0", true // zero is unsigned
		}
		sign := ""
		if neg {
			sign = "-"
		}
		if b == 1 {
			return sign + strconv.Itoa(a), true // a whole-valued fraction, e.g. -5
		}
		return sign + strconv.Itoa(a) + "/" + strconv.Itoa(b), true
	case ref.Hash == p.dec.RDec && len(args) == 5:
		neg := isNeg(args[0])
		ip, ok1 := p.wholeVal(args[1])
		ds, ok2 := p.wlistVals(args[2])
		rep, ok3 := p.wholeVal(args[3])
		if !ok1 || !ok2 || !ok3 {
			return "", false
		}
		terminating := isNeg(args[4]) // the term flag is the `true` constructor
		sign := ""
		if neg && !(ip == 0 && len(ds) == 0) {
			sign = "-" // zero is unsigned
		}
		s := sign + strconv.Itoa(ip)
		if len(ds) == 0 {
			return s, true // a whole number, e.g. 2 or -5
		}
		if terminating || rep >= len(ds) {
			return s + "." + digitsStr(ds), true
		}
		return s + "." + digitsStr(ds[:rep]) + "{" + digitsStr(ds[rep:]) + "}", true
	}
	return "", false
}

// numeralOf recognizes a saturated succ-chain terminating in zero and returns
// its value. Chains over a non-zero tail (succ n) are not numerals.
func (p *printer) numeralOf(t core.Tm) (int, bool) {
	if !p.foldNat {
		return 0, false
	}
	n := 0
	for {
		switch x := t.(type) {
		case core.Ref:
			if x.Hash == p.natZero {
				return n, true
			}
			return 0, false
		case core.App:
			ref, ok := x.Fn.(core.Ref)
			if !ok || x.Icit != core.Expl || ref.Hash != p.natSucc {
				return 0, false
			}
			n++
			t = x.Arg
		default:
			return 0, false
		}
	}
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
		if v, ok := p.numeralOf(x); ok {
			sb.WriteString(strconv.Itoa(v))
			return
		}
		if n, ok := p.refNames[x.Hash]; ok {
			if opPrec(n) >= 0 {
				// An operator outside its infix position prints first-class: (+).
				sb.WriteString("(" + n + ")")
			} else {
				sb.WriteString(n)
			}
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
		// A saturated succ-chain over zero prints as a numeral (an atom).
		if v, ok := p.numeralOf(x); ok {
			sb.WriteString(strconv.Itoa(v))
			return
		}
		// A prelude fraction/decimal result folds to positional notation.
		if s, ok := p.decimalStr(x); ok {
			sb.WriteString(s)
			return
		}
		// A saturated binary application of an operator-named definition prints
		// infix at its table level; left operand at the level (left-associative),
		// right operand one tighter. Re-parsing reads it back to the same core.
		if name, l, r, lvl, ok := p.infixApp(x); ok {
			p.wrap(sb, prec, lvl, func() {
				p.print(sb, l, names, lvl)
				sb.WriteString(" " + name + " ")
				p.print(sb, r, names, lvl+1)
			})
			return
		}
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
	case core.Sig:
		p.wrap(sb, prec, precApp, func() {
			n := fresh(x.Cod.Name, names)
			sb.WriteString("Sig ")
			p.print(sb, x.Dom, names, precAtom)
			sb.WriteString(" (fn (")
			sb.WriteString(n)
			sb.WriteString(" : U) is ")
			p.print(sb, x.Cod.Body, prepend(n, names), precLow)
			sb.WriteString(" end)")
		})
	case core.Pair:
		p.wrap(sb, prec, precApp, func() {
			n := fresh(x.Cod.Name, names)
			sb.WriteString("Pair ")
			p.print(sb, x.Dom, names, precAtom)
			sb.WriteString(" (fn (")
			sb.WriteString(n)
			sb.WriteString(" : U) is ")
			p.print(sb, x.Cod.Body, prepend(n, names), precLow)
			sb.WriteString(" end) ")
			p.print(sb, x.A, names, precAtom)
			sb.WriteByte(' ')
			p.print(sb, x.B, names, precAtom)
		})
	case core.Fst:
		p.wrap(sb, prec, precApp, func() {
			sb.WriteString("Fst ")
			p.print(sb, x.P, names, precAtom)
		})
	case core.Snd:
		p.wrap(sb, prec, precApp, func() {
			sb.WriteString("Snd ")
			p.print(sb, x.P, names, precAtom)
		})
	case core.NatLit:
		// A compressed numeral folds back to its decimal digit on output
		// (C7 / R-NUM, Decision 3) — inputs and outputs are both ordinary digits.
		sb.WriteString(x.N.String())
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

// DebugCoreNamed renders a core term as the same explicit structural tree as
// DebugCore, but HUMAN-READABLE: references print as their definition name (not a
// @hash), binders carry a freshened name (not an unnamed λ/Π and bare de Bruijn
// indices), and every node kind is shown — including NatLit (the number) and the
// equality/Sigma formers DebugCore renders as `?`. It is the REPL's :ast view; like
// :core it is a debug tree, not a sentence of the surface grammar. refNames maps a
// definition's content hash to its display name (Session.RefNames()).
func DebugCoreNamed(t core.Tm, refNames map[core.Hash]string) string {
	var sb strings.Builder
	debugCoreNamed(&sb, t, refNames, nil)
	return sb.String()
}

func debugCoreNamed(sb *strings.Builder, t core.Tm, refs map[core.Hash]string, scope []string) {
	rec := func(u core.Tm) { debugCoreNamed(sb, u, refs, scope) }
	switch x := t.(type) {
	case core.Var:
		if x.Idx >= 0 && x.Idx < len(scope) {
			sb.WriteString(scope[x.Idx])
		} else {
			sb.WriteString("#")
			sb.WriteString(strconv.Itoa(x.Idx))
		}
	case core.Ref:
		if n, ok := refs[x.Hash]; ok {
			sb.WriteString(n)
		} else {
			sb.WriteString("@")
			sb.WriteString(x.Hash.Short())
		}
	case core.Meta:
		sb.WriteString("?m")
		sb.WriteString(strconv.Itoa(x.ID))
	case core.Univ:
		sb.WriteString("U")
		if x.Lvl > 0 {
			sb.WriteString(strconv.Itoa(x.Lvl))
		}
	case core.Prop:
		sb.WriteString("Prop")
	case core.NatLit:
		sb.WriteString(x.N.String())
	case core.App:
		sb.WriteByte('(')
		rec(x.Fn)
		sb.WriteByte(' ')
		if x.Icit == core.Impl {
			sb.WriteByte('{')
			rec(x.Arg)
			sb.WriteByte('}')
		} else {
			rec(x.Arg)
		}
		sb.WriteByte(')')
	case core.Lam:
		name := fresh(x.Body.Name, scope)
		if x.Icit == core.Impl {
			sb.WriteString("(λ {")
			sb.WriteString(name)
			sb.WriteString("}. ")
		} else {
			sb.WriteString("(λ ")
			sb.WriteString(name)
			sb.WriteString(". ")
		}
		debugCoreNamed(sb, x.Body.Body, refs, prepend(name, scope))
		sb.WriteByte(')')
	case core.Pi:
		sb.WriteString("(Π ")
		lb, rb := "(", ")"
		if x.Icit == core.Impl {
			lb, rb = "{", "}"
		}
		sb.WriteString(lb)
		// the domain is in the OUTER scope; reserve the binder name for the codomain.
		name := fresh(x.Cod.Name, scope)
		sb.WriteString(name)
		sb.WriteString(" : ")
		rec(x.Dom)
		sb.WriteString(rb)
		sb.WriteString(". ")
		debugCoreNamed(sb, x.Cod.Body, refs, prepend(name, scope))
		sb.WriteByte(')')
	case core.Sig:
		sb.WriteString("(Σ (")
		name := fresh(x.Cod.Name, scope)
		sb.WriteString(name)
		sb.WriteString(" : ")
		rec(x.Dom)
		sb.WriteString("). ")
		debugCoreNamed(sb, x.Cod.Body, refs, prepend(name, scope))
		sb.WriteByte(')')
	case core.Pair:
		sb.WriteString("(pair ")
		rec(x.A)
		sb.WriteByte(' ')
		rec(x.B)
		sb.WriteByte(')')
	case core.Fst:
		sb.WriteString("(fst ")
		rec(x.P)
		sb.WriteByte(')')
	case core.Snd:
		sb.WriteString("(snd ")
		rec(x.P)
		sb.WriteByte(')')
	case core.Let:
		sb.WriteString("(let ")
		hint := fresh(x.Body.Name, scope)
		sb.WriteString(hint)
		if x.Ty != nil {
			sb.WriteString(" : ")
			rec(x.Ty)
		}
		sb.WriteString(" = ")
		rec(x.Val)
		sb.WriteString(" in ")
		debugCoreNamed(sb, x.Body.Body, refs, prepend(hint, scope))
		sb.WriteByte(')')
	case core.Ann:
		sb.WriteByte('(')
		rec(x.Term)
		sb.WriteString(" : ")
		rec(x.Ty)
		sb.WriteByte(')')
	case core.Eq:
		sb.WriteString("(Eq ")
		rec(x.Ty)
		sb.WriteByte(' ')
		rec(x.L)
		sb.WriteByte(' ')
		rec(x.R)
		sb.WriteByte(')')
	case core.Refl:
		sb.WriteString("(refl ")
		rec(x.Tm)
		sb.WriteByte(')')
	case core.Cast:
		sb.WriteString("(cast ")
		rec(x.A)
		sb.WriteByte(' ')
		rec(x.B)
		sb.WriteByte(' ')
		rec(x.X)
		sb.WriteByte(')')
	case core.Subst:
		sb.WriteString("(subst ")
		rec(x.A)
		sb.WriteByte(' ')
		rec(x.X)
		sb.WriteByte(' ')
		rec(x.Y)
		sb.WriteByte(' ')
		rec(x.P)
		sb.WriteByte(' ')
		rec(x.Px)
		sb.WriteByte(')')
	default:
		sb.WriteString("?")
	}
}

// infixApp recognizes `Ref⁺ l r` — a saturated, explicit, binary application of
// a definition whose display name is an infix operator — and returns the pieces
// for infix printing.
func (p *printer) infixApp(a core.App) (name string, l, r core.Tm, lvl int, ok bool) {
	if a.Icit != core.Expl {
		return
	}
	inner, isApp := a.Fn.(core.App)
	if !isApp || inner.Icit != core.Expl {
		return
	}
	ref, isRef := inner.Fn.(core.Ref)
	if !isRef {
		return
	}
	n, named := p.refNames[ref.Hash]
	if !named {
		return
	}
	if lv := opPrec(n); lv >= 0 {
		return n, inner.Arg, a.Arg, lv, true
	}
	return
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
	case core.Eq:
		return occursVar(x.Ty, depth) || occursVar(x.L, depth) || occursVar(x.R, depth)
	case core.Refl:
		return occursVar(x.Tm, depth)
	case core.Cast:
		return occursVar(x.A, depth) || occursVar(x.B, depth) ||
			occursVar(x.P, depth) || occursVar(x.X, depth)
	case core.Subst:
		return occursVar(x.A, depth) || occursVar(x.X, depth) || occursVar(x.Y, depth) ||
			occursVar(x.Prf, depth) || occursVar(x.P, depth) || occursVar(x.Px, depth)
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
