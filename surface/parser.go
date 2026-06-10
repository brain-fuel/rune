package surface

import (
	"errors"
	"fmt"

	"goforge.dev/rune/core"
)

// ErrIncomplete marks a parse that ran out of input mid-form (an unterminated is/seq
// block, or a binder/let awaiting more tokens). The REPL distinguishes it from a hard
// syntax error to decide between a continuation prompt and an error message.
var ErrIncomplete = errors.New("incomplete input")

type parser struct {
	toks []token
	pos  int
}

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

// parseDef parses `Ident ":" Expr "is" Expr "end"`. The type annotation is mandatory
// in v0.2.0.
func (p *parser) parseDef() (Def, error) {
	id, err := p.expect(tIdent)
	if err != nil {
		return Def{}, err
	}
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
		ty, err := p.parseExpr()
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
		param, _, dom, err := p.parseBinder()
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
		return EPi{Param: param, Icit: core.Impl, Dom: dom, Cod: cod}, nil
	}
	lhs, err := p.parseApp()
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

func (p *parser) parseApp() (Exp, error) {
	fn, err := p.parseAtom()
	if err != nil {
		return nil, err
	}
	for {
		if p.peek().kind == tLBrace {
			// f {e}: an explicitly-supplied implicit argument.
			p.next()
			arg, err := p.parseExpr()
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
	case tIdent, tU, tLParen, tFn, tSeq, tHole:
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
		return EUniv{}, nil
	case tHole:
		p.next()
		return EHole{}, nil
	case tIdent:
		p.next()
		return EVar{Name: t.text}, nil
	case tFn:
		return p.parseLam()
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
		dom  Exp
	}
	var bs []binder
	for p.peek().kind == tLParen || p.peek().kind == tLBrace {
		name, icit, dom, err := p.parseBinder()
		if err != nil {
			return nil, err
		}
		bs = append(bs, binder{name, icit, dom})
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
		body = ELam{Param: bs[i].name, Icit: bs[i].icit, Dom: bs[i].dom, Body: body}
	}
	return body, nil
}

// parseBinder parses a lambda binder `"(" Ident ":" Expr ")"` (explicit) or
// `"{" Ident ":" Expr "}"` (implicit). Unlike the head of an Arrow/App, a binder
// position is unambiguous: the bracket always opens `name : type`.
func (p *parser) parseBinder() (string, core.Icit, Exp, error) {
	icit := core.Expl
	closeKind := tRParen
	if p.peek().kind == tLBrace {
		p.next()
		icit = core.Impl
		closeKind = tRBrace
	} else if _, err := p.expect(tLParen); err != nil {
		return "", icit, nil, err
	}
	id, err := p.expect(tIdent)
	if err != nil {
		return "", icit, nil, err
	}
	if _, err := p.expect(tColon); err != nil {
		return "", icit, nil, err
	}
	dom, err := p.parseExpr()
	if err != nil {
		return "", icit, nil, err
	}
	if _, err := p.expect(closeKind); err != nil {
		return "", icit, nil, err
	}
	return id.text, icit, dom, nil
}

// parseParen disambiguates the three things a '(' can open at the head of an
// Arrow/App, with one token of lookahead past the ')' (§5.1): a dependent-Pi binder
// `(x : A) ->`, a parenthesized ascription `(e : T)`, and plain grouping `(e)`.
func (p *parser) parseParen() (Exp, error) {
	p.next() // '('
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
		case tFn, tSeq:
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
		case tFn, tSeq:
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
		ty, err := sub.parseExpr()
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
