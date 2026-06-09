package surface

import "fmt"

type parser struct {
	toks []token
	pos  int
}

func (p *parser) peek() token { return p.toks[p.pos] }
func (p *parser) peekAt(n int) token {
	i := p.pos + n
	if i >= len(p.toks) {
		i = len(p.toks) - 1
	}
	return p.toks[i]
}
func (p *parser) next() token { t := p.toks[p.pos]; p.pos++; return t }

func (p *parser) expect(k tokKind) (token, error) {
	t := p.peek()
	if t.kind != k {
		return t, fmt.Errorf("expected %s, found %s at offset %d", k, t.kind, t.pos)
	}
	return p.next(), nil
}

// ParseExpr parses a single surface expression from src. Used by the property
// harness and for ad-hoc terms.
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

// ParseFile parses a flat list of top-level definitions. Definitions begin at
// column 0; indented lines continue the current definition.
func ParseFile(src string) ([]Def, error) {
	toks, err := lex(src)
	if err != nil {
		return nil, err
	}
	var defs []Def
	i := 0
	for toks[i].kind != tEOF {
		if toks[i].col != 0 {
			return nil, fmt.Errorf("expected a definition at column 0, found %s at offset %d", toks[i].kind, toks[i].pos)
		}
		j := i + 1
		for toks[j].kind != tEOF && toks[j].col != 0 {
			j++
		}
		span := append(append([]token{}, toks[i:j]...), token{tEOF, "", toks[j].pos, 0})
		p := &parser{toks: span}
		d, err := p.parseDef()
		if err != nil {
			return nil, err
		}
		if p.peek().kind != tEOF {
			return nil, fmt.Errorf("trailing tokens in definition %q: %s at offset %d", d.Name, p.peek().kind, p.peek().pos)
		}
		defs = append(defs, d)
		i = j
	}
	return defs, nil
}

func (p *parser) parseDef() (Def, error) {
	id, err := p.expect(tIdent)
	if err != nil {
		return Def{}, err
	}
	d := Def{Name: id.text}
	if p.peek().kind == tColon {
		p.next()
		ty, err := p.parseExpr()
		if err != nil {
			return Def{}, err
		}
		d.Ty = ty
	}
	if _, err := p.expect(tEquals); err != nil {
		return Def{}, err
	}
	body, err := p.parseExpr()
	if err != nil {
		return Def{}, err
	}
	d.Body = body
	return d, nil
}

func (p *parser) parseExpr() (Exp, error) {
	switch p.peek().kind {
	case tLambda:
		return p.parseLam()
	case tLet:
		return p.parseLet()
	default:
		return p.parseArrowOrAnn()
	}
}

func (p *parser) parseLam() (Exp, error) {
	p.next() // '\'
	var params []string
	for p.peek().kind == tIdent {
		params = append(params, p.next().text)
	}
	if len(params) == 0 {
		return nil, fmt.Errorf("expected a parameter after '\\' at offset %d", p.peek().pos)
	}
	if _, err := p.expect(tArrow); err != nil {
		return nil, err
	}
	body, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	for i := len(params) - 1; i >= 0; i-- {
		body = ELam{Param: params[i], Body: body}
	}
	return body, nil
}

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

// parseArrowOrAnn parses an application, then an optional trailing '-> Cod'
// (non-dependent Pi) or ': Ty' (annotation). Both are right-associative and lower
// precedence than application.
func (p *parser) parseArrowOrAnn() (Exp, error) {
	lhs, err := p.parseApp()
	if err != nil {
		return nil, err
	}
	switch p.peek().kind {
	case tArrow:
		p.next()
		cod, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return EPi{Param: "_", Dom: lhs, Cod: cod}, nil
	case tColon:
		p.next()
		ty, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		return EAnn{Term: lhs, Ty: ty}, nil
	default:
		return lhs, nil
	}
}

func (p *parser) parseApp() (Exp, error) {
	fn, err := p.parseAtom()
	if err != nil {
		return nil, err
	}
	for p.atomStarts() {
		arg, err := p.parseAtom()
		if err != nil {
			return nil, err
		}
		fn = EApp{Fn: fn, Arg: arg}
	}
	return fn, nil
}

func (p *parser) atomStarts() bool {
	switch p.peek().kind {
	case tIdent, tU, tLParen:
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
	case tIdent:
		p.next()
		return EVar{Name: t.text}, nil
	case tLParen:
		return p.parseParen()
	default:
		return nil, fmt.Errorf("expected an expression, found %s at offset %d", t.kind, t.pos)
	}
}

// parseParen disambiguates the three things a '(' can open: a dependent-Pi binder
// group "(x : A) ->", a parenthesized annotation "(e : T)", and plain grouping
// "(e)". The commitment to Pi requires the trailing '->'.
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
