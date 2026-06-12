package surface

import (
	"errors"
	"fmt"
	"strconv"

	"goforge.dev/rune/core"
)

// ErrIncomplete marks a parse that ran out of input mid-form (an unterminated is/seq
// block, or a binder/let awaiting more tokens). The REPL distinguishes it from a hard
// syntax error to decide between a continuation prompt and an error message.
var ErrIncomplete = errors.New("incomplete input")

type parser struct {
	toks []token
	pos  int
	// noEq suppresses the infix '=' (equality-proposition sugar) at the spine of a
	// let/seq type annotation, where '=' belongs to the binding (GRAMMAR §5.4). It
	// is inherited through arrows and operators and reset inside bracketed groups.
	noEq bool
	// natZero/natSucc are the constructor names a `builtin nat` declaration
	// registered; numerals expand against them at parse time (GRAMMAR §5.5).
	// Empty means no binding: a numeral is then a parse error.
	natZero, natSucc string
}

// numMax caps numeral expansion: a literal is nothing but its unary succ-chain
// (a compressed core numeral is parked), so an absurd literal must fail loudly
// rather than materialize a million-node term.
const numMax = 1 << 16

// skipNL advances past tNewline tokens. Newlines are insignificant everywhere the
// ordinary peek/next path runs; only seq item collection reads tokens raw (§2, §5.3).
func (p *parser) skipNL() {
	for p.pos < len(p.toks) && p.toks[p.pos].kind == tNewline {
		p.pos++
	}
}

func (p *parser) peek() token { p.skipNL(); return p.toks[p.pos] }

func (p *parser) next() token { p.skipNL(); t := p.toks[p.pos]; p.pos++; return t }

// peekAt returns the n-th significant token ahead of the cursor (peekAt(0) == peek),
// skipping newlines. One token of lookahead past a ')' is all §5.1 needs.
func (p *parser) peekAt(n int) token {
	i := p.pos
	for {
		for i < len(p.toks) && p.toks[i].kind == tNewline {
			i++
		}
		if i >= len(p.toks) {
			return p.toks[len(p.toks)-1]
		}
		if n == 0 {
			return p.toks[i]
		}
		n--
		i++
	}
}

func (p *parser) expect(k tokKind) (token, error) {
	t := p.peek()
	if t.kind != k {
		if t.kind == tEOF {
			return t, fmt.Errorf("%w: expected %s", ErrIncomplete, k)
		}
		return t, fmt.Errorf("expected %s, found %s at offset %d", k, t.kind, t.pos)
	}
	return p.next(), nil
}

// ParseExpr parses a single surface expression from src. Used by the REPL, the
// property harness, and for ad-hoc terms.
func ParseExpr(src string) (Exp, error) {
	toks, err := lex(src)
	if err != nil {
		return nil, err
	}
	p := &parser{toks: toks}
	e, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	if p.peek().kind != tEOF {
		return nil, fmt.Errorf("unexpected %s at offset %d", p.peek().kind, p.peek().pos)
	}
	return e, nil
}

// ParseFile parses a flat list of top-level definitions (GRAMMAR.md §3). Each
// definition self-delimits with its own `end`, so no layout rule is needed;
// newlines between definitions are insignificant.
// ParseProgram parses a file of top-level items: definitions and datatype
// declarations, in order.
func ParseProgram(src string) ([]Item, error) {
	toks, err := lex(src)
	if err != nil {
		return nil, err
	}
	p := &parser{toks: appendEOF(toks)}
	var items []Item
	for p.peek().kind != tEOF {
		if p.peek().kind == tData {
			d, err := p.parseData()
			if err != nil {
				return nil, err
			}
			items = append(items, d)
			continue
		}
		if p.peek().kind == tBuiltin {
			b, err := p.parseBuiltin()
			if err != nil {
				return nil, err
			}
			items = append(items, b)
			continue
		}
		d, err := p.parseDef()
		if err != nil {
			return nil, err
		}
		items = append(items, d)
	}
	return items, nil
}

// parseBuiltin parses `builtin nat Ident Ident Ident` (GRAMMAR §3) and registers
// the binding with the parser, so numerals in the rest of the file expand
// against it (§5.5).
func (p *parser) parseBuiltin() (BuiltinNat, error) {
	p.next() // 'builtin'
	kind, err := p.expect(tIdent)
	if err != nil {
		return BuiltinNat{}, err
	}
	if kind.text != "nat" {
		return BuiltinNat{}, fmt.Errorf("unknown builtin kind %q at offset %d (only \"nat\" exists)", kind.text, kind.pos)
	}
	var names [3]string
	for i := range names {
		id, err := p.expect(tIdent)
		if err != nil {
			return BuiltinNat{}, err
		}
		names[i] = id.text
	}
	b := BuiltinNat{TyName: names[0], Zero: names[1], Succ: names[2]}
	p.natZero, p.natSucc = b.Zero, b.Succ
	return b, nil
}

// ParseExprNat is ParseExpr with a registered `builtin nat` binding, so numerals
// in the expression expand. The REPL uses it once a session has the binding.
func ParseExprNat(src, zero, succ string) (Exp, error) {
	toks, err := lex(src)
	if err != nil {
		return nil, err
	}
	p := &parser{toks: toks, natZero: zero, natSucc: succ}
	e, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	if p.peek().kind != tEOF {
		return nil, fmt.Errorf("unexpected %s at offset %d", p.peek().kind, p.peek().pos)
	}
	return e, nil
}

// parseData parses `data Ident ":" Expr "is" Ctor ("|" Ctor)* "end"` with
// Ctor ::= Ident ":" Expr. A leading "|" before the first constructor is
// permitted.
func (p *parser) parseData() (DataDef, error) {
	p.next() // 'data'
	id, err := p.expect(tIdent)
	if err != nil {
		return DataDef{}, err
	}
	if _, err := p.expect(tColon); err != nil {
		return DataDef{}, err
	}
	ty, err := p.parseExpr()
	if err != nil {
		return DataDef{}, err
	}
	if _, err := p.expect(tIs); err != nil {
		return DataDef{}, err
	}
	d := DataDef{Name: id.text, Ty: ty}
	if p.peek().kind == tBar {
		p.next()
	}
	for {
		cid, err := p.expect(tIdent)
		if err != nil {
			return DataDef{}, err
		}
		if _, err := p.expect(tColon); err != nil {
			return DataDef{}, err
		}
		cty, err := p.parseExpr()
		if err != nil {
			return DataDef{}, err
		}
		d.Ctors = append(d.Ctors, Ctor{Name: cid.text, Ty: cty})
		switch p.peek().kind {
		case tBar:
			p.next()
		case tEnd:
			p.next()
			return d, nil
		default:
			return DataDef{}, fmt.Errorf("expected '|' or 'end' in data declaration, found %s at offset %d",
				p.peek().kind, p.peek().pos)
		}
	}
}

func ParseFile(src string) ([]Def, error) {
	toks, err := lex(src)
	if err != nil {
		return nil, err
	}
	p := &parser{toks: toks}
	var defs []Def
	for p.peek().kind != tEOF {
		d, err := p.parseDef()
		if err != nil {
			return nil, err
		}
		defs = append(defs, d)
	}
	return defs, nil
}

// parseDef parses `DefName ":" Expr "is" Expr "end"`. The type annotation is
// mandatory in v0.2.0. A definition name is an identifier or an operator (§3).
func (p *parser) parseDef() (Def, error) {
	id := p.peek()
	if id.kind != tIdent && id.kind != tOp {
		if id.kind == tEOF {
			return Def{}, fmt.Errorf("%w: expected a definition name", ErrIncomplete)
		}
		return Def{}, fmt.Errorf("expected a definition name (identifier or operator), found %s at offset %d", id.kind, id.pos)
	}
	p.next()
	if _, err := p.expect(tColon); err != nil {
		return Def{}, err
	}
	ty, err := p.parseExpr()
	if err != nil {
		return Def{}, err
	}
	if _, err := p.expect(tIs); err != nil {
		return Def{}, err
	}
	body, err := p.parseExpr()
	if err != nil {
		return Def{}, err
	}
	if _, err := p.expect(tEnd); err != nil {
		return Def{}, err
	}
	return Def{Name: id.text, Ty: ty, Body: body}, nil
}

func (p *parser) parseExpr() (Exp, error) {
	if p.peek().kind == tLet {
		return p.parseLet()
	}
	return p.parseArrow()
}

// parseLet parses an inline `let Ident [":" Expr] "=" Expr "in" Expr`. The `in` is
// mandatory; the no-`in` binding form belongs to seq (§5.2) and is parsed there.
func (p *parser) parseLet() (Exp, error) {
	p.next() // 'let'
	id, err := p.expect(tIdent)
	if err != nil {
		return nil, err
	}
	let := ELet{Name: id.text}
	if p.peek().kind == tColon {
		p.next()
		// The annotation's spine-level '=' belongs to the binding (§5.4).
		saved := p.noEq
		p.noEq = true
		ty, err := p.parseExpr()
		p.noEq = saved
		if err != nil {
			return nil, err
		}
		let.Ty = ty
	}
	if _, err := p.expect(tEquals); err != nil {
		return nil, err
	}
	val, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	let.Val = val
	if _, err := p.expect(tIn); err != nil {
		return nil, err
	}
	body, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	let.Body = body
	return let, nil
}

// parseArrow parses an application followed by an optional trailing `-> Expr`, the
// non-dependent function type. Arrow is right-associative and looser than application.
// Bare `e : T` ascription does not exist at top level — ascription is only the
// parenthesized atom `(e : T)` (§5.1).
func (p *parser) parseArrow() (Exp, error) {
	if p.peek().kind == tLBrace {
		// {x : A} -> B : an implicit dependent function type.
		param, _, qty, dom, err := p.parseBinder()
		if err != nil {
			return nil, err
		}
		if _, err := p.expect(tArrow); err != nil {
			return nil, err
		}
		cod, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return EPi{Param: param, Icit: core.Impl, Qty: qty, Dom: dom, Cod: cod}, nil
	}
	lhs, err := p.parseEq()
	if err != nil {
		return nil, err
	}
	if p.peek().kind == tArrow {
		p.next()
		cod, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return EPi{Param: "_", Dom: lhs, Cod: cod}, nil
	}
	return lhs, nil
}

// parseEq parses `Add ["=" Add]` — the equality-proposition sugar, `l = r` ~~>
// `Eq _ l r` with a hole for elaboration to solve (§5.4). Non-associative.
// Suppressed (p.noEq) at the spine of a let/seq type annotation, where '='
// belongs to the binding.
func (p *parser) parseEq() (Exp, error) {
	lhs, err := p.parseAdd()
	if err != nil {
		return nil, err
	}
	if p.peek().kind != tEquals || p.noEq {
		return lhs, nil
	}
	p.next()
	rhs, err := p.parseAdd()
	if err != nil {
		return nil, err
	}
	if p.peek().kind == tEquals {
		return nil, fmt.Errorf("an equality cannot be chained; parenthesize one side (offset %d)", p.peek().pos)
	}
	return EApp{Fn: EApp{Fn: EApp{Fn: EEq{}, Arg: EHole{}}, Arg: lhs}, Arg: rhs}, nil
}

// parseAdd parses `Mul (("+"|"-") Mul)*`, left-associative. An infix operator is
// sugar for applying the like-named definition: `x + y` is `(+) x y` (§5.4).
func (p *parser) parseAdd() (Exp, error) {
	lhs, err := p.parseMul()
	if err != nil {
		return nil, err
	}
	for p.peek().kind == tOp && addLevel(p.peek().text) {
		op := p.next().text
		rhs, err := p.parseMul()
		if err != nil {
			return nil, err
		}
		lhs = EApp{Fn: EApp{Fn: EVar{Name: op}, Arg: lhs}, Arg: rhs}
	}
	return lhs, nil
}

// parseMul parses `App (("*"|"/"|"%") App)*`, left-associative.
func (p *parser) parseMul() (Exp, error) {
	lhs, err := p.parseApp()
	if err != nil {
		return nil, err
	}
	for p.peek().kind == tOp && !addLevel(p.peek().text) {
		op := p.next().text
		rhs, err := p.parseApp()
		if err != nil {
			return nil, err
		}
		lhs = EApp{Fn: EApp{Fn: EVar{Name: op}, Arg: lhs}, Arg: rhs}
	}
	return lhs, nil
}

// addLevel reports whether an operator sits at the loose additive level; the
// closed operator table of GRAMMAR §3 has exactly two levels.
func addLevel(op string) bool {
	return op == "+" || op == "-"
}

func (p *parser) parseApp() (Exp, error) {
	fn, err := p.parseAtom()
	if err != nil {
		return nil, err
	}
	for {
		if p.peek().kind == tLBrace {
			// f {e}: an explicitly-supplied implicit argument. The braces reset
			// the let-annotation '=' carve-out (§5.4).
			p.next()
			saved := p.noEq
			p.noEq = false
			arg, err := p.parseExpr()
			p.noEq = saved
			if err != nil {
				return nil, err
			}
			if _, err := p.expect(tRBrace); err != nil {
				return nil, err
			}
			fn = EApp{Fn: fn, Arg: arg, Icit: core.Impl}
			continue
		}
		if !p.atomStarts() {
			return fn, nil
		}
		arg, err := p.parseAtom()
		if err != nil {
			return nil, err
		}
		fn = EApp{Fn: fn, Arg: arg}
	}
}

func (p *parser) atomStarts() bool {
	switch p.peek().kind {
	case tIdent, tU, tLParen, tFn, tSeq, tHole, tProp, tEq, tRefl, tCast, tSubst, tNum, tCase:
		return true
	default:
		return false
	}
}

func (p *parser) parseAtom() (Exp, error) {
	t := p.peek()
	switch t.kind {
	case tU:
		p.next()
		lvl := 0
		if len(t.text) > 1 {
			lvl = int(t.text[1] - '0')
		}
		return EUniv{Lvl: lvl}, nil
	case tHole:
		p.next()
		return EHole{}, nil
	case tProp:
		p.next()
		return EProp{}, nil
	case tEq:
		p.next()
		return EEq{}, nil
	case tRefl:
		p.next()
		return ERefl{}, nil
	case tCast:
		p.next()
		return ECast{}, nil
	case tSubst:
		p.next()
		return ESubst{}, nil
	case tIdent:
		p.next()
		return EVar{Name: t.text}, nil
	case tNum:
		p.next()
		return p.expandNum(t)
	case tFn:
		return p.parseLam()
	case tCase:
		return p.parseCase()
	case tSeq:
		return p.parseSeq()
	case tLParen:
		return p.parseParen()
	case tEOF:
		return nil, fmt.Errorf("%w: expected an expression", ErrIncomplete)
	default:
		return nil, fmt.Errorf("expected an expression, found %s at offset %d", t.kind, t.pos)
	}
}

// parseLam parses `fn Binder+ "is" Expr "end"` and desugars the curried binders into
// nested single-parameter ELam (§6).
func (p *parser) parseLam() (Exp, error) {
	p.next() // 'fn'
	type binder struct {
		name string
		icit core.Icit
		qty  core.Qty
		dom  Exp
	}
	var bs []binder
	for p.peek().kind == tLParen || p.peek().kind == tLBrace {
		name, icit, qty, dom, err := p.parseBinder()
		if err != nil {
			return nil, err
		}
		bs = append(bs, binder{name, icit, qty, dom})
	}
	if len(bs) == 0 {
		return nil, fmt.Errorf("expected a (name : type) or {name : type} binder after 'fn' at offset %d", p.peek().pos)
	}
	if _, err := p.expect(tIs); err != nil {
		return nil, err
	}
	body, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	if _, err := p.expect(tEnd); err != nil {
		return nil, err
	}
	for i := len(bs) - 1; i >= 0; i-- {
		body = ELam{Param: bs[i].name, Icit: bs[i].icit, Qty: bs[i].qty, Dom: bs[i].dom, Body: body}
	}
	return body, nil
}

// parseBinder parses a lambda binder `"(" Ident ":" Expr ")"` (explicit) or
// `"{" Ident ":" Expr "}"` (implicit). Unlike the head of an Arrow/App, a binder
// position is unambiguous: the bracket always opens `name : type`.
func (p *parser) parseBinder() (string, core.Icit, core.Qty, Exp, error) {
	icit := core.Expl
	qty := core.QMany
	closeKind := tRParen
	if p.peek().kind == tLBrace {
		p.next()
		icit = core.Impl
		closeKind = tRBrace
	} else if _, err := p.expect(tLParen); err != nil {
		return "", icit, qty, nil, err
	}
	// A bracketed group resets the let-annotation '=' carve-out (§5.4).
	saved := p.noEq
	p.noEq = false
	defer func() { p.noEq = saved }()
	if isQtyTok(p.peek()) {
		qty = qtyOf(p.next().text)
	}
	id, err := p.expect(tIdent)
	if err != nil {
		return "", icit, qty, nil, err
	}
	if _, err := p.expect(tColon); err != nil {
		return "", icit, qty, nil, err
	}
	dom, err := p.parseExpr()
	if err != nil {
		return "", icit, qty, nil, err
	}
	if _, err := p.expect(closeKind); err != nil {
		return "", icit, qty, nil, err
	}
	return id.text, icit, qty, dom, nil
}

// qtyOf maps a quantity token to its core annotation.
func qtyOf(text string) core.Qty {
	if text == "0" {
		return core.QZero
	}
	return core.QOne
}

// expandNum desugars a numeral against the registered `builtin nat` binding:
// n becomes the n-fold application of Succ to Zero (GRAMMAR §5.5, §6). The
// expansion is pure parser sugar — downstream the constructors are ordinary
// names resolving to ordinary content-hash references.
func (p *parser) expandNum(t token) (Exp, error) {
	if p.natZero == "" {
		return nil, fmt.Errorf("numeral %s at offset %d has no meaning: no `builtin nat` declared", t.text, t.pos)
	}
	n, err := strconv.Atoi(t.text)
	if err != nil || n > numMax {
		return nil, fmt.Errorf("numeral %s at offset %d is too large to expand (unary literals cap at %d)", t.text, t.pos, numMax)
	}
	e := Exp(EVar{Name: p.natZero})
	for range n {
		e = EApp{Fn: EVar{Name: p.natSucc}, Arg: e}
	}
	return e, nil
}

// isQtyTok reports whether a token is a usage annotation in binder position:
// the numerals "0" and "1" (GRAMMAR §2 — position disambiguates them from
// numeric literals).
func isQtyTok(t token) bool {
	return t.kind == tNum && (t.text == "0" || t.text == "1")
}

// parseParen disambiguates the three things a '(' can open at the head of an
// Arrow/App, with one token of lookahead past the ')' (§5.1): a dependent-Pi binder
// `(x : A) ->`, a parenthesized ascription `(e : T)`, and plain grouping `(e)`.
func (p *parser) parseParen() (Exp, error) {
	p.next() // '('
	if p.peek().kind == tOp && p.peekAt(1).kind == tRParen {
		// "(" Op ")": an operator used prefix/first-class, e.g. (+) x y.
		op := p.next().text
		p.next() // ')'
		return EVar{Name: op}, nil
	}
	// A bracketed group resets the let-annotation '=' carve-out (§5.4).
	saved := p.noEq
	p.noEq = false
	defer func() { p.noEq = saved }()
	if isQtyTok(p.peek()) && p.peekAt(1).kind == tIdent && p.peekAt(2).kind == tColon {
		// (0 x : A) -> B / (1 x : A) -> B: a quantity-annotated dependent Pi.
		qty := qtyOf(p.next().text)
		param := p.next().text
		p.next() // ':'
		dom, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		if _, err := p.expect(tRParen); err != nil {
			return nil, err
		}
		if _, err := p.expect(tArrow); err != nil {
			return nil, err
		}
		// The codomain is outside the brackets: it inherits the carve-out (§5.4).
		p.noEq = saved
		cod, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return EPi{Param: param, Qty: qty, Dom: dom, Cod: cod}, nil
	}
	if p.peek().kind == tIdent && p.peekAt(1).kind == tColon {
		param := p.next().text
		p.next() // ':'
		dom, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		if _, err := p.expect(tRParen); err != nil {
			return nil, err
		}
		if p.peek().kind == tArrow {
			p.next()
			// The codomain is outside the brackets: it inherits the carve-out (§5.4).
			p.noEq = saved
			cod, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			return EPi{Param: param, Dom: dom, Cod: cod}, nil
		}
		return EAnn{Term: EVar{Name: param}, Ty: dom}, nil
	}
	inner, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	if p.peek().kind == tColon {
		p.next()
		ty, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		if _, err := p.expect(tRParen); err != nil {
			return nil, err
		}
		return EAnn{Term: inner, Ty: ty}, nil
	}
	if _, err := p.expect(tRParen); err != nil {
		return nil, err
	}
	return inner, nil
}

// parseCase parses `"case" Expr "of" ("|" Pattern ["with" Ident+] "->" Expr)+ "end"`
// (GRAMMAR §5.6). Patterns are flat: a constructor name followed by variable
// binders ('_' allowed). The block self-delimits, so it is an atom; like any
// bracketed group it resets the let-annotation '=' carve-out (§5.4).
func (p *parser) parseCase() (Exp, error) {
	p.next() // 'case'
	saved := p.noEq
	p.noEq = false
	defer func() { p.noEq = saved }()

	scrut, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	if _, err := p.expect(tOf); err != nil {
		return nil, err
	}
	e := ECase{Scrut: scrut}
	for {
		if _, err := p.expect(tBar); err != nil {
			return nil, err
		}
		id, err := p.expect(tIdent)
		if err != nil {
			return nil, err
		}
		cl := CaseClause{Ctor: id.text}
		for p.peek().kind == tIdent || p.peek().kind == tHole {
			cl.Binders = append(cl.Binders, p.next().text)
		}
		if p.peek().kind == tWith {
			p.next()
			for p.peek().kind == tIdent || p.peek().kind == tHole {
				cl.IHs = append(cl.IHs, p.next().text)
			}
			if len(cl.IHs) == 0 {
				return nil, fmt.Errorf("'with' needs at least one induction-hypothesis name at offset %d", p.peek().pos)
			}
		}
		if _, err := p.expect(tArrow); err != nil {
			return nil, err
		}
		body, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		cl.Body = body
		e.Clauses = append(e.Clauses, cl)
		switch p.peek().kind {
		case tBar:
			continue
		case tEnd:
			p.next()
			return e, nil
		case tEOF:
			return nil, fmt.Errorf("%w: unterminated case (missing 'end')", ErrIncomplete)
		default:
			return nil, fmt.Errorf("expected '|' or 'end' in case, found %s at offset %d",
				p.peek().kind, p.peek().pos)
		}
	}
}

// parseSeq parses `"seq" … "end"` and desugars it to nested let (§5.3, §6). The body
// is collected raw — newlines and ';' are significant separators here — up to the
// `end` that matches this `seq`, tracking the nesting of inner fn/seq blocks.
func (p *parser) parseSeq() (Exp, error) {
	p.next() // 'seq'
	start := p.pos
	depth := 0
	for i := p.pos; ; i++ {
		if i >= len(p.toks) || p.toks[i].kind == tEOF {
			return nil, fmt.Errorf("%w: unterminated seq (missing 'end')", ErrIncomplete)
		}
		switch p.toks[i].kind {
		case tFn, tSeq, tCase:
			depth++
		case tEnd:
			if depth == 0 {
				body := p.toks[start:i]
				p.pos = i + 1
				return desugarSeq(body)
			}
			depth--
		}
	}
}

// desugarSeq splits a seq body into items on top-level separators (newline or ';'),
// then lowers `let x₁ = e₁ … let xₙ = eₙ R` to `let x₁ = e₁ in (… in R)` (§6). All
// but the last item must be a binding; the last is the result expression.
func desugarSeq(toks []token) (Exp, error) {
	var groups [][]token
	var cur []token
	depth := 0
	flush := func() {
		if len(cur) > 0 {
			groups = append(groups, cur)
			cur = nil
		}
	}
	for _, t := range toks {
		switch t.kind {
		case tFn, tSeq, tCase:
			depth++
			cur = append(cur, t)
		case tEnd:
			depth--
			cur = append(cur, t)
		case tNewline, tSemi:
			if depth == 0 {
				flush()
			} else {
				cur = append(cur, t)
			}
		default:
			cur = append(cur, t)
		}
	}
	flush()
	if len(groups) == 0 {
		return nil, fmt.Errorf("a seq must produce a value: empty seq")
	}
	result, err := parseGroupExpr(groups[len(groups)-1])
	if err != nil {
		return nil, fmt.Errorf("a seq must end with a result expression, not a binding: %v", err)
	}
	body := result
	for i := len(groups) - 2; i >= 0; i-- {
		bind, err := parseSeqBind(groups[i])
		if err != nil {
			return nil, err
		}
		bind.Body = body
		body = bind
	}
	return body, nil
}

func appendEOF(toks []token) []token {
	out := make([]token, 0, len(toks)+1)
	out = append(out, toks...)
	return append(out, token{tEOF, "", 0})
}

// parseGroupExpr parses one seq item group as a complete expression (the Result).
func parseGroupExpr(toks []token) (Exp, error) {
	sub := &parser{toks: appendEOF(toks)}
	e, err := sub.parseExpr()
	if err != nil {
		return nil, err
	}
	if sub.peek().kind != tEOF {
		return nil, fmt.Errorf("unexpected %s at offset %d in seq item", sub.peek().kind, sub.peek().pos)
	}
	return e, nil
}

// parseSeqBind parses one non-final seq item: `"let" Ident [":" Expr] "=" Expr` with
// NO `in` (§5.3). The bound body is filled in by desugarSeq. Encountering `in` here is
// an error: seq bindings scope over the rest of the block, they do not take `in`.
func parseSeqBind(toks []token) (ELet, error) {
	sub := &parser{toks: appendEOF(toks)}
	if sub.peek().kind != tLet {
		return ELet{}, fmt.Errorf("a non-final seq item must be a binding 'let x = e', found %s at offset %d", sub.peek().kind, sub.peek().pos)
	}
	sub.next() // 'let'
	id, err := sub.expect(tIdent)
	if err != nil {
		return ELet{}, err
	}
	let := ELet{Name: id.text}
	if sub.peek().kind == tColon {
		sub.next()
		// The annotation's spine-level '=' belongs to the binding (§5.4).
		sub.noEq = true
		ty, err := sub.parseExpr()
		sub.noEq = false
		if err != nil {
			return ELet{}, err
		}
		let.Ty = ty
	}
	if _, err := sub.expect(tEquals); err != nil {
		return ELet{}, err
	}
	val, err := sub.parseExpr()
	if err != nil {
		return ELet{}, err
	}
	let.Val = val
	if sub.peek().kind == tIn {
		return ELet{}, fmt.Errorf("a seq binding does not take 'in' (offset %d); it scopes over the rest of the seq", sub.peek().pos)
	}
	if sub.peek().kind != tEOF {
		return ELet{}, fmt.Errorf("unexpected %s at offset %d after a seq binding", sub.peek().kind, sub.peek().pos)
	}
	return let, nil
}
