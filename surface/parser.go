package surface

import (
	"errors"
	"fmt"
	"math/big"
	"strings"

	"goforge.dev/rune/v3/core"
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
		if p.peek().kind == tModule {
			mod, err := p.parseModule()
			if err != nil {
				return nil, err
			}
			for _, d := range mod {
				items = append(items, d)
			}
			continue
		}
		// `protocol` is a CONTEXTUAL keyword: a block only in the form
		// `protocol Name is …` (an identifier follows). Otherwise `protocol` is an
		// ordinary identifier (e.g. a def named `protocol`, as in ch71), so it falls
		// through to parseItem.
		if p.peek().kind == tIdent && p.peek().text == "protocol" && p.peekAt(1).kind == tIdent {
			proto, err := p.parseProtocol()
			if err != nil {
				return nil, err
			}
			items = append(items, proto...)
			continue
		}
		it, err := p.parseItem()
		if err != nil {
			return nil, err
		}
		items = append(items, it)
	}
	return items, nil
}

// parseBuiltin parses a builtin binding (GRAMMAR §3):
//
//	builtin nat Nat zero succ
//	builtin natMul mul
//
// and returns the corresponding item. Numerals are NOT expanded at parse time
// (numLit defers that); the binding governs lowering downstream (§5.5). The old
// `builtin bin` binary-numeral binding is retired (C7 / R-NUM, Decision 5):
// NatLit subsumes it.
func (p *parser) parseBuiltin() (Item, error) {
	p.next() // 'builtin'
	kind, err := p.expect(tIdent)
	if err != nil {
		return nil, err
	}
	switch kind.text {
	case "nat":
		names, err := p.builtinNames(3)
		if err != nil {
			return nil, err
		}
		return BuiltinNat{TyName: names[0], Zero: names[1], Succ: names[2]}, nil
	case "natAdd", "natMul", "natMonus", "natDiv", "natMod":
		names, err := p.builtinNames(1)
		if err != nil {
			return nil, err
		}
		return BuiltinNatOp{Kind: kind.text, DefName: names[0]}, nil
	case "int", "rat":
		// builtin int Z intOf  /  builtin rat Rat ratOf — a typed numeral
		// injection (numeric-tower rung C4): a numeral checked at the codomain
		// type lowers to inj (NatLit n).
		names, err := p.builtinNames(2)
		if err != nil {
			return nil, err
		}
		return BuiltinNumInj{Kind: kind.text, TyName: names[0], InjName: names[1]}, nil
	default:
		return nil, fmt.Errorf("unknown builtin kind %q at offset %d (only \"nat\", \"int\", \"rat\", \"natAdd\", \"natMul\", \"natMonus\", \"natDiv\", and \"natMod\" exist)", kind.text, kind.pos)
	}
}

// builtinNames reads n names after a builtin kind keyword. Each name is an
// identifier OR an operator — `builtin natMul *` accelerates the operator-named
// definition `*`, exactly as `parseDef` admits an operator definition name (§3).
func (p *parser) builtinNames(n int) ([]string, error) {
	names := make([]string, n)
	for i := range names {
		id := p.peek()
		if id.kind != tIdent && id.kind != tOp {
			if id.kind == tEOF {
				return nil, fmt.Errorf("%w: expected a builtin name", ErrIncomplete)
			}
			return nil, fmt.Errorf("expected a builtin name (identifier or operator), found %s at offset %d", id.kind, id.pos)
		}
		p.next()
		names[i] = id.text
	}
	return names, nil
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
		case tEOF:
			return DataDef{}, fmt.Errorf("%w: unterminated data declaration (missing 'end')", ErrIncomplete)
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

// parseForeign parses `foreign Name : Type end` (R-FFI / B4): a bodiless typed
// axiom whose type is its contract. No body — its meaning is supplied by the
// host at link time, and it is tracked as an assumption.
func (p *parser) parseForeign() (Def, error) {
	if _, err := p.expect(tForeign); err != nil {
		return Def{}, err
	}
	id := p.peek()
	if id.kind != tIdent && id.kind != tOp {
		return Def{}, fmt.Errorf("expected a foreign name, found %s at offset %d", id.kind, id.pos)
	}
	p.next()
	if _, err := p.expect(tColon); err != nil {
		return Def{}, err
	}
	ty, err := p.parseExpr()
	if err != nil {
		return Def{}, err
	}
	if _, err := p.expect(tEnd); err != nil {
		return Def{}, err
	}
	return Def{Name: id.text, Ty: ty, IsForeign: true}, nil
}

// parseModule parses `module Name is <Def>* end` (C6): a namespace block. Each
// inner definition's name is prefixed with `Name.`, and it is otherwise an
// ordinary top-level def — references write the qualified name `Name.member`
// (one identifier token, since the lexer absorbs `.member` segments). Modules
// nest by name concatenation; only definitions (not datatypes) live inside for
// now. A module is just sugar over qualified-named defs — no new core.
func (p *parser) parseModule() ([]Def, error) {
	if _, err := p.expect(tModule); err != nil {
		return nil, err
	}
	name := p.peek()
	if name.kind != tIdent {
		return nil, fmt.Errorf("expected a module name, found %s at offset %d", name.kind, name.pos)
	}
	p.next()
	if _, err := p.expect(tIs); err != nil {
		return nil, err
	}
	var defs []Def
	for p.peek().kind != tEnd {
		if p.peek().kind == tEOF {
			return nil, fmt.Errorf("%w: module %q is missing its 'end'", ErrIncomplete, name.text)
		}
		d, err := p.parseDef()
		if err != nil {
			return nil, err
		}
		d.Name = name.text + "." + d.Name
		defs = append(defs, d)
	}
	p.next() // consume 'end'
	return defs, nil
}

// parseItem parses ONE top-level item that is not a block (foreign axiom, datatype,
// builtin binding, or plain definition) — the shared body of ParseProgram and the
// block parsers.
func (p *parser) parseItem() (Item, error) {
	switch p.peek().kind {
	case tForeign:
		return p.parseForeign()
	case tData:
		return p.parseData()
	case tBuiltin:
		return p.parseBuiltin()
	default:
		return p.parseDef()
	}
}

// protocolRequired are the members a `protocol … end` block MUST define: the CvRDT
// operations the simulator/projection consume (init/merge/value + at least one
// opN), and the three join-semilattice law proofs that make the merge converge.
// Their PRESENCE is checked here; their well-typedness is checked by elaboration of
// the desugared defs.
var protocolRequired = []string{"init", "merge", "value", "mergeComm", "mergeIdem", "mergeAssoc"}

// parseProtocol parses `protocol Name is <item>* end` (E4 / wavelet): a VERIFIED
// CvRDT block. It is a checked grouping, not a namespace — the members pass through
// as ordinary top-level defs (bare names, so `rune simulate` and the serveG
// projection consume them unchanged), but the block REJECTS a protocol that omits a
// required member, so a CvRDT cannot ship without proving its merge is a
// join-semilattice (convergence is structural, not convention). One protocol per
// file (the members are bare). Zero new core.
func (p *parser) parseProtocol() ([]Item, error) {
	kw := p.next() // the contextual `protocol` keyword (an identifier)
	if kw.kind != tIdent || kw.text != "protocol" {
		return nil, fmt.Errorf("expected 'protocol', found %s at offset %d", kw.kind, kw.pos)
	}
	name := p.peek()
	if name.kind != tIdent {
		return nil, fmt.Errorf("expected a protocol name, found %s at offset %d", name.kind, name.pos)
	}
	p.next()
	if _, err := p.expect(tIs); err != nil {
		return nil, err
	}
	var items []Item
	defined := map[string]bool{}
	hasOp := false
	for p.peek().kind != tEnd {
		if p.peek().kind == tEOF {
			return nil, fmt.Errorf("%w: protocol %q is missing its 'end'", ErrIncomplete, name.text)
		}
		it, err := p.parseItem()
		if err != nil {
			return nil, err
		}
		switch d := it.(type) {
		case Def:
			defined[d.Name] = true
			if strings.HasPrefix(d.Name, "op") {
				hasOp = true
			}
		case DataDef:
			defined[d.Name] = true
		}
		items = append(items, it)
	}
	p.next() // consume 'end'

	var missing []string
	for _, m := range protocolRequired {
		if !defined[m] {
			missing = append(missing, m)
		}
	}
	if !hasOp {
		missing = append(missing, "op0")
	}
	if len(missing) > 0 {
		return nil, fmt.Errorf(
			"protocol %q is missing its CvRDT contract: %s. A protocol must define "+
				"init/merge/value + at least one local update (op0…) AND prove its merge "+
				"is a join-semilattice (mergeComm, mergeIdem, mergeAssoc) — the laws that "+
				"make replicas converge",
			name.text, strings.Join(missing, ", "))
	}
	return items, nil
}

// parseDef parses `DefName ":" Expr "is" Expr "end"`. The type annotation is
// mandatory in v0.2.0. A definition name is an identifier or an operator (§3).
func (p *parser) parseDef() (Def, error) {
	isInstance := false
	if p.peek().kind == tInstance {
		p.next()
		isInstance = true
	}
	isPartial := false
	if p.peek().kind == tPartial {
		p.next()
		isPartial = true
	}
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
	return Def{Name: id.text, Ty: ty, Body: body, IsInstance: isInstance, IsPartial: isPartial}, nil
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
	lhs, err := p.parsePipe()
	if err != nil {
		return nil, err
	}
	if p.peek().kind != tEquals || p.noEq {
		return lhs, nil
	}
	p.next()
	rhs, err := p.parsePipe()
	if err != nil {
		return nil, err
	}
	if p.peek().kind == tEquals {
		return nil, fmt.Errorf("an equality cannot be chained; parenthesize one side (offset %d)", p.peek().pos)
	}
	return EApp{Fn: EApp{Fn: EApp{Fn: EEq{}, Arg: EHole{}}, Arg: lhs}, Arg: rhs}, nil
}

// parsePipe parses `Add ("|>" Add)*`, left-associative, the LOOSEST binary level.
// `x |> f` is pure reverse application — sugar for `f x`, NOT a named definition —
// so it composes with any function and needs no library binding: `3/4 |> to_radix`
// is `to_radix (3/4)`, and `x |> f |> g` is `g (f x)` (§5.4).
func (p *parser) parsePipe() (Exp, error) {
	lhs, err := p.parseAppend()
	if err != nil {
		return nil, err
	}
	for p.peek().kind == tOp && pipeLevel(p.peek().text) {
		p.next() // '|>'
		rhs, err := p.parseAppend()
		if err != nil {
			return nil, err
		}
		lhs = EApp{Fn: rhs, Arg: lhs}
	}
	return lhs, nil
}

// parseAppend parses `Add ("++" Add)*`, RIGHT-associative — the Semigroup/Monoid
// concat level, looser than `+`/`*` and tighter than `|>` (Haskell's `++` is infixr 5).
// `x ++ y` is sugar for `(++) x y` (§5.4); right recursion gives right-associativity, so
// `a ++ b ++ c` is `a ++ (b ++ c)` (no O(n²) left re-association for a cons-like append).
func (p *parser) parseAppend() (Exp, error) {
	lhs, err := p.parseAdd()
	if err != nil {
		return nil, err
	}
	if p.peek().kind == tOp && appendLevel(p.peek().text) {
		op := p.next().text
		rhs, err := p.parseAppend()
		if err != nil {
			return nil, err
		}
		return EApp{Fn: EApp{Fn: EVar{Name: op}, Arg: lhs}, Arg: rhs}, nil
	}
	return lhs, nil
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

// parseMul parses `Unary (("*"|"/"|"%"|"//") Unary)*`, left-associative. The
// pipe `|>` is excluded (it is the looser parsePipe level), so it is not grabbed
// here as a multiplicative operator.
func (p *parser) parseMul() (Exp, error) {
	lhs, err := p.parseUnary()
	if err != nil {
		return nil, err
	}
	for p.peek().kind == tOp && !addLevel(p.peek().text) && !pipeLevel(p.peek().text) && !appendLevel(p.peek().text) {
		op := p.next().text
		rhs, err := p.parseUnary()
		if err != nil {
			return nil, err
		}
		lhs = EApp{Fn: EApp{Fn: EVar{Name: op}, Arg: lhs}, Arg: rhs}
	}
	return lhs, nil
}

// parseUnary parses a PREFIX minus before an application: `-e` desugars to the
// application `negate e`. `Whole` has no negatives, so a type-directed `0 - e`
// would FLOOR (-1 monus to 0 — the `-1/3 = 0/3` bug); `negate` is a real sign flip
// on a signed type (the prelude's signed Frac), so `-1/3` is `(negate 1)/3`, a
// genuine -1/3. A bare `-` after a complete operand is binary subtraction (handled
// by parseAdd), so `a - b` is unaffected. `negate` must be in scope (the prelude
// provides it; a bare session must define it to use prefix minus).
func (p *parser) parseUnary() (Exp, error) {
	if p.peek().kind == tOp && p.peek().text == "-" {
		p.next()
		operand, err := p.parseUnary()
		if err != nil {
			return nil, err
		}
		return EApp{Fn: EVar{Name: "negate"}, Arg: operand}, nil
	}
	return p.parseApp()
}

// addLevel reports whether an operator sits at the loose additive level; the
// closed operator table of GRAMMAR §3 has three levels (pipe < add < mul).
func addLevel(op string) bool {
	return op == "+" || op == "-"
}

// appendLevel reports whether an operator sits at the concat level (Semigroup/Monoid
// `++`), looser than additive and tighter than the pipe (GRAMMAR §3).
func appendLevel(op string) bool {
	return op == "++"
}

// pipeLevel reports whether an operator is the pipe `|>` (the loosest level).
func pipeLevel(op string) bool {
	return op == "|>"
}

// projTail consumes trailing postfix Σ projections (.1 / .2), binding tighter
// than application: m.2.1 is ((m.2).1), and f x.1 is f (x.1).
func (p *parser) projTail(e Exp) Exp {
	for p.peek().kind == tProj {
		t := p.next()
		if t.text == "1" {
			e = EFst{P: e}
		} else {
			e = ESnd{P: e}
		}
	}
	return e
}

func (p *parser) parseApp() (Exp, error) {
	fn, err := p.parseAtom()
	if err != nil {
		return nil, err
	}
	fn = p.projTail(fn)
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
		fn = EApp{Fn: fn, Arg: p.projTail(arg)}
	}
}

func (p *parser) atomStarts() bool {
	switch p.peek().kind {
	case tIdent, tU, tLParen, tFn, tSeq, tHole, tProp, tEq, tRefl, tCast, tSubst, tNum, tDec, tStr, tCase, tCalc:
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
	case tSig:
		p.next()
		return ESig{}, nil
	case tPair:
		p.next()
		return EPair{}, nil
	case tFst:
		p.next()
		a, err := p.parseAtom()
		if err != nil {
			return nil, err
		}
		return EFst{P: p.projTail(a)}, nil
	case tSnd:
		p.next()
		a, err := p.parseAtom()
		if err != nil {
			return nil, err
		}
		return ESnd{P: p.projTail(a)}, nil
	case tIdent:
		p.next()
		return EVar{Name: t.text}, nil
	case tNum:
		p.next()
		return p.numLit(t)
	case tDec:
		p.next()
		return p.decLit(t)
	case tStr:
		p.next()
		return p.strLit(t)
	case tFn:
		return p.parseLam()
	case tCase:
		return p.parseCase()
	case tCalc:
		return p.parseCalc()
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

// numLit reads a numeral token into an unexpanded ENum. Expansion is deferred
// to lowering (resolver or elaborator), where the meaning — the unary
// `builtin nat`, lowered to a compressed NatLit — is fixed by the expected type
// (see numeral.go). Numerals are arbitrary-precision (one big.Int): there is no
// size cap, so a digit run of any length parses.
func (p *parser) numLit(t token) (Exp, error) {
	n, ok := new(big.Int).SetString(t.text, 10)
	if !ok {
		return nil, fmt.Errorf("numeral %s at offset %d is not a valid base-10 integer", t.text, t.pos)
	}
	return ENum{Val: n, Pos: t.pos}, nil
}

// decLit reads a decimal-literal token `I "." N? ("{" R "}")?` and desugars it, at
// parse time, to the exact fraction `num / den` — the SAME Exp a written `num / den`
// produces, so it reuses the prelude's `/` (the Div typeclass, which yields a reduced
// Frac) with no new AST node or elaborator change. The value of `I.N{R}` (p = |N|,
// q = |R|) is the standard repeating-decimal fraction:
//   terminating (q = 0): num = I·10ᵖ + N,                   den = 10ᵖ
//   repeating  (q > 0):  num = (I·10ᵖ + N)·(10^q − 1) + R,  den = 10ᵖ·(10^q − 1)
// e.g. 1.3 → 13/10, 1.{3} → 12/9 (= 4/3), 0.1{6} → 15/90 (= 1/6). Lowest-terms
// reduction is the prelude `/`'s job (`divWF` → `reduce`).
func (p *parser) decLit(t token) (Exp, error) {
	dot := strings.IndexByte(t.text, '.')
	intStr := t.text[:dot]
	rest := t.text[dot+1:]
	var nStr, rStr string
	if b := strings.IndexByte(rest, '{'); b >= 0 {
		nStr = rest[:b]
		rStr = rest[b+1 : len(rest)-1] // strip the braces
	} else {
		nStr = rest
	}
	bigOf := func(s string) *big.Int {
		if s == "" {
			return big.NewInt(0)
		}
		n, _ := new(big.Int).SetString(s, 10) // all-digits by construction (the lexer)
		return n
	}
	pow10 := func(k int) *big.Int {
		return new(big.Int).Exp(big.NewInt(10), big.NewInt(int64(k)), nil)
	}
	intPart, fracPart, repPart := bigOf(intStr), bigOf(nStr), bigOf(rStr)
	tenP := pow10(len(nStr))
	// scaled = I·10ᵖ + N  (the whole-plus-fraction numerator before the repetend)
	scaled := new(big.Int).Add(new(big.Int).Mul(intPart, tenP), fracPart)
	var num, den *big.Int
	if len(rStr) == 0 {
		num, den = scaled, tenP
	} else {
		tenQ1 := new(big.Int).Sub(pow10(len(rStr)), big.NewInt(1)) // 10^q − 1
		num = new(big.Int).Add(new(big.Int).Mul(scaled, tenQ1), repPart)
		den = new(big.Int).Mul(tenP, tenQ1)
	}
	slash := EVar{Name: "/"}
	return EApp{Fn: EApp{Fn: slash, Arg: ENum{Val: num, Pos: t.pos}}, Arg: ENum{Val: den, Pos: t.pos}}, nil
}

// strLit desugars a string literal `"…"` to a packed `Bytes` value, the surface
// sugar for B4's host-backed binary string ("Haskell's binary thing only better":
// packed, not a [Char] cons-list). The decoded content's UTF-8 bytes c0c1…c_{k-1}
// (c0 first) pack into ONE big integer with the FIRST byte least-significant and a
// 0x01 SENTINEL on top: n = 1·256ᵏ + Σ cᵢ·256ⁱ. That sentinel makes the byte count
// recoverable (and leading-NUL bytes survive), and the low-byte-first layout makes
// `uncons` O(1): head = n mod 256, tail = n div 256, empty ⟺ n = 1. The literal
// emits `bytes n` (the `Bytes` constructor must be in scope, like `/` for decimals);
// `bytes` wraps the numeral, so the whole content reduces in the kernel — strings
// COMPUTE in the REPL, no host needed.
func (p *parser) strLit(t token) (Exp, error) {
	acc := big.NewInt(1) // the sentinel (most-significant byte)
	bs := []byte(t.text)
	c256 := big.NewInt(256)
	for i := len(bs) - 1; i >= 0; i-- {
		acc.Mul(acc, c256)
		acc.Add(acc, big.NewInt(int64(bs[i])))
	}
	return EApp{Fn: EVar{Name: "bytes"}, Arg: ENum{Val: acc, Pos: t.pos}}, nil
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

// parseCalc parses `"calc" Expr ("=" Expr "by" Expr)+ "end"` (GRAMMAR §5.7) and
// desugars it at parse time: the first step's proof is ascribed to its equation,
// and each later step chains through the subst former —
//
//	subst _ prev next prf (fn (w : _) is first = w end) acc
//
// so the block proves `first = last` with every `by` pinned to exactly its own
// equation. Spine-level '=' inside the block belongs to the calc (the §5.4
// carve-out mechanism); parenthesize an equality proposition inside a step.
func (p *parser) parseCalc() (Exp, error) {
	p.next() // 'calc'
	saved := p.noEq
	p.noEq = true
	defer func() { p.noEq = saved }()

	first, err := p.parseExpr()
	if err != nil {
		return nil, err
	}
	type step struct{ rhs, prf Exp }
	var steps []step
	for {
		if _, err := p.expect(tEquals); err != nil {
			return nil, err
		}
		rhs, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		if _, err := p.expect(tBy); err != nil {
			return nil, err
		}
		prf, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		steps = append(steps, step{rhs: rhs, prf: prf})
		switch p.peek().kind {
		case tEquals:
			continue
		case tEnd:
			p.next()
		case tEOF:
			return nil, fmt.Errorf("%w: unterminated calc (missing 'end')", ErrIncomplete)
		default:
			return nil, fmt.Errorf("expected '=' or 'end' in calc, found %s at offset %d",
				p.peek().kind, p.peek().pos)
		}
		break
	}

	eq := func(l, r Exp) Exp {
		return EApp{Fn: EApp{Fn: EApp{Fn: EEq{}, Arg: EHole{}}, Arg: l}, Arg: r}
	}
	// The motive duplicates `first` under a new binder; freshen it against
	// every identifier appearing there so nothing is captured.
	w := freshAgainst("w", first)
	acc := Exp(EAnn{Term: steps[0].prf, Ty: eq(first, steps[0].rhs)})
	cur := steps[0].rhs
	for _, st := range steps[1:] {
		motive := ELam{Param: w, Dom: EHole{}, Body: eq(first, EVar{Name: w})}
		acc = EApp{Fn: EApp{Fn: EApp{Fn: EApp{Fn: EApp{Fn: EApp{Fn: ESubst{},
			Arg: EHole{}}, Arg: cur}, Arg: st.rhs}, Arg: st.prf}, Arg: motive}, Arg: acc}
		cur = st.rhs
	}
	return acc, nil
}

// freshAgainst picks a name based on hint that no identifier of e uses.
func freshAgainst(hint string, e Exp) string {
	used := map[string]bool{}
	var walk func(Exp)
	walk = func(e Exp) {
		switch x := e.(type) {
		case EVar:
			used[x.Name] = true
		case ELam:
			used[x.Param] = true
			walk(x.Dom)
			walk(x.Body)
		case EApp:
			walk(x.Fn)
			walk(x.Arg)
		case EPi:
			used[x.Param] = true
			walk(x.Dom)
			walk(x.Cod)
		case ELet:
			used[x.Name] = true
			if x.Ty != nil {
				walk(x.Ty)
			}
			walk(x.Val)
			walk(x.Body)
		case EAnn:
			walk(x.Term)
			walk(x.Ty)
		case ECase:
			walk(x.Scrut)
			for _, cl := range x.Clauses {
				used[cl.Ctor] = true
				for _, n := range cl.Binders {
					used[n] = true
				}
				for _, n := range cl.IHs {
					used[n] = true
				}
				walk(cl.Body)
			}
		}
	}
	walk(e)
	if !used[hint] {
		return hint
	}
	for i := 0; ; i++ {
		cand := fmt.Sprintf("%s%d", hint, i)
		if !used[cand] {
			return cand
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
		case tFn, tSeq, tCase, tCalc:
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
		case tFn, tSeq, tCase, tCalc:
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
