package surface

import (
	"fmt"
	"unicode"
)

type tokKind int

const (
	tEOF tokKind = iota
	tIdent
	tLParen
	tRParen
	tArrow   // ->
	tColon   // :
	tEquals  // =
	tSemi    // ;
	tNewline // a line break; a seq item separator, insignificant elsewhere
	tFn
	tIs
	tEnd
	tSeq
	tLet
	tIn
	tU
	tLBrace  // {  (implicit binder/argument; '{-' opens a comment instead)
	tRBrace  // }
	tHole    // _  (a hole: a metavariable for elaboration to solve)
	tData    // data (a datatype declaration; Phase 4)
	tBar     // |  (constructor separator)
	tProp    // Prop (the universe of propositions; Phase 3)
	tEq      // Eq   (the observational equality type former)
	tRefl    // refl (the reflexivity proof)
	tCast    // cast (transport along a type equality)
	tSubst   // subst (Leibniz transport along an equality; Phase 4)
	tSig     // Sig  (the dependent-pair type former; C1/R-SUM)
	tPair    // pair (the dependent-pair intro)
	tFst     // fst  (first projection)
	tSnd     // snd  (second projection)
	tProj    // .1 / .2 (postfix Σ projection; text is "1" or "2")
	tInstance // instance (a typeclass instance declaration; C2)
	tModule   // module (a namespace block; C6)
	tPartial  // partial (a general-recursive definition; C4)
	tForeign  // foreign (a typed FFI axiom; R-FFI / B4)
	tNum     // a numeral: digit run; "0"/"1" in binder position is a usage annotation
	tDec     // a decimal literal: digits "." digits, optionally "{" digits "}" (a repetend); e.g. 1.3, 1.{3}, 0.1{6}
	tStr     // a string literal: "..." with \n \t \r \\ \" escapes; the text is the DECODED content (packed-String sugar, GRAMMAR §2)
	tBytes   // a byte-string literal: b"..." with \xNN \n \t \r \\ \" \0 escapes; the text is the DECODED raw bytes (Phase-0 Bin sugar)
	tOp      // an infix operator: + - * / % (a symbolic identifier; GRAMMAR §5.4)
	tBuiltin // builtin (a builtin-binding declaration: builtin nat Nat zero succ)
	tCase    // case (a case expression; GRAMMAR §5.6)
	tOf      // of   (separates the scrutinee from the clauses)
	tWith    // with (binds the induction hypotheses in a clause)
	tCalc    // calc (an equational-reasoning block; GRAMMAR §5.7)
	tBy      // by   (justifies one calc step)
	tMutual  // mutual  (opens a mutually-recursive `data`/`partial` group block)
)

type token struct {
	kind tokKind
	text string
	pos  int
}

func (k tokKind) String() string {
	switch k {
	case tEOF:
		return "end of input"
	case tIdent:
		return "identifier"
	case tLParen:
		return "'('"
	case tRParen:
		return "')'"
	case tArrow:
		return "'->'"
	case tColon:
		return "':'"
	case tEquals:
		return "'='"
	case tSemi:
		return "';'"
	case tNewline:
		return "newline"
	case tFn:
		return "'fn'"
	case tIs:
		return "'is'"
	case tEnd:
		return "'end'"
	case tSeq:
		return "'seq'"
	case tLet:
		return "'let'"
	case tIn:
		return "'in'"
	case tU:
		return "'U'"
	case tLBrace:
		return "'{'"
	case tRBrace:
		return "'}'"
	case tHole:
		return "'_'"
	case tData:
		return "'data'"
	case tMutual:
		return "'mutual'"
	case tBar:
		return "'|'"
	case tProp:
		return "'Prop'"
	case tEq:
		return "'Eq'"
	case tRefl:
		return "'refl'"
	case tCast:
		return "'cast'"
	case tSubst:
		return "'subst'"
	case tNum:
		return "numeral"
	case tDec:
		return "decimal literal"
	case tStr:
		return "string literal"
	case tBytes:
		return "byte-string literal"
	case tOp:
		return "operator"
	case tBuiltin:
		return "'builtin'"
	case tCase:
		return "'case'"
	case tOf:
		return "'of'"
	case tWith:
		return "'with'"
	case tCalc:
		return "'calc'"
	case tBy:
		return "'by'"
	default:
		return "token"
	}
}

func isIdentStart(r rune) bool {
	return unicode.IsLetter(r) || r == '_'
}

func isIdentCont(r rune) bool {
	return unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_' || r == '\''
}

// isDigitBrace reports whether rs[at] begins a well-formed decimal repetend `{`
// digits `}` (at least one digit). Used by the numeral lexer to absorb a repeating
// part (`1.{3}`, `0.1{6}`) while leaving a non-digit brace block (`{x}`) untouched.
func isDigitBrace(rs []rune, at int) bool {
	if at >= len(rs) || rs[at] != '{' {
		return false
	}
	j := at + 1
	for j < len(rs) && unicode.IsDigit(rs[j]) {
		j++
	}
	return j > at+1 && j < len(rs) && rs[j] == '}'
}

// lex turns source into a token slice for the v0.2.0 surface grammar (see
// ref_docs/GRAMMAR.md §2). Whitespace separates tokens; a line break becomes a
// tNewline token, which the parser skips everywhere except while collecting the
// items of a seq block. Line comments (-- …) and nestable block comments ({- … -})
// are discarded. The lexer takes the longest match, so -> is one token, -- opens a
// line comment, and {- opens a block comment.
func lex(src string) ([]token, error) {
	var toks []token
	rs := []rune(src)
	i := 0
	for i < len(rs) {
		r := rs[i]
		switch {
		case r == '\n':
			toks = append(toks, token{tNewline, "\n", i})
			i++
		case r == ' ', r == '\t', r == '\r':
			i++
		case r == '-' && i+1 < len(rs) && rs[i+1] == '-':
			for i < len(rs) && rs[i] != '\n' {
				i++
			}
		case r == '{' && i+1 < len(rs) && rs[i+1] == '-':
			end, err := skipBlockComment(rs, i)
			if err != nil {
				return nil, err
			}
			i = end
		case r == '{':
			toks = append(toks, token{tLBrace, "{", i})
			i++
		case r == '}':
			toks = append(toks, token{tRBrace, "}", i})
			i++
		case r == '(':
			toks = append(toks, token{tLParen, "(", i})
			i++
		case r == ')':
			toks = append(toks, token{tRParen, ")", i})
			i++
		case r == ':':
			toks = append(toks, token{tColon, ":", i})
			i++
		case r == '=':
			toks = append(toks, token{tEquals, "=", i})
			i++
		case r == ';':
			toks = append(toks, token{tSemi, ";", i})
			i++
		case r == '|' && i+1 < len(rs) && rs[i+1] == '>':
			// Longest match: '|>' is the pipe (reverse-application) operator, one
			// token, lexed before the bare '|' so it is not split (GRAMMAR §2/§3).
			toks = append(toks, token{tOp, "|>", i})
			i += 2
		case r == '|':
			toks = append(toks, token{tBar, "|", i})
			i++
		case r == '-' && i+1 < len(rs) && rs[i+1] == '>':
			toks = append(toks, token{tArrow, "->", i})
			i += 2
		case r == '.' && i+1 < len(rs) && (rs[i+1] == '1' || rs[i+1] == '2'):
			// Postfix Σ projection .1 / .2 (one token; binds tighter than app).
			toks = append(toks, token{tProj, string(rs[i+1]), i})
			i += 2
		case r == '/' && i+1 < len(rs) && rs[i+1] == '/':
			// Longest match: '//' is the flooring quotient, one token
			// (ref_docs/rune-numeric-tower.md; GRAMMAR §2).
			toks = append(toks, token{tOp, "//", i})
			i += 2
		case r == '+' && i+1 < len(rs) && rs[i+1] == '+':
			// Longest match: '++' is the Semigroup/Monoid append operator, one token,
			// lexed before the bare '+' so it is not split into two (GRAMMAR §2/§3).
			toks = append(toks, token{tOp, "++", i})
			i += 2
		case r == '+', r == '-', r == '*', r == '/', r == '%':
			// A bare '-' reaches here only after the longest-match '--' and '->'
			// cases above have declined it; a bare '/' only after '//'.
			toks = append(toks, token{tOp, string(r), i})
			i++
		case r == 'b' && i+1 < len(rs) && rs[i+1] == '"':
			// A byte-string literal b"…" (Phase-0 Bin sugar). Lexed BEFORE the
			// identifier case so the `b` prefix is not swallowed as an identifier.
			tok, end, err := scanBytesLit(rs, i)
			if err != nil {
				return nil, err
			}
			toks = append(toks, tok)
			i = end
		case r == '"':
			stoks, end, err := scanStringToks(rs, i)
			if err != nil {
				return nil, err
			}
			toks = append(toks, stoks...)
			i = end
		case unicode.IsDigit(r):
			start := i
			for i < len(rs) && unicode.IsDigit(rs[i]) {
				i++
			}
			// Optional decimal tail: `. digits? ({ digits })?` — a decimal literal
			// (GRAMMAR §2). The `.` is consumed as part of the number ONLY when it is
			// followed by a digit or a well-formed `{digits}` repetend, so `1.3`/`1.{3}`
			// lex as one tDec while `x.1`/`(e).2` stay postfix projections and `1.` /
			// `1 {x}` are unaffected. A repetend `{…}` is absorbed only when all-digits.
			if i < len(rs) && rs[i] == '.' && i+1 < len(rs) &&
				(unicode.IsDigit(rs[i+1]) || (rs[i+1] == '{' && isDigitBrace(rs, i+1))) {
				i++ // the dot
				for i < len(rs) && unicode.IsDigit(rs[i]) {
					i++
				}
				if i < len(rs) && rs[i] == '{' && isDigitBrace(rs, i) {
					i++ // '{'
					for i < len(rs) && unicode.IsDigit(rs[i]) {
						i++
					}
					i++ // '}'
				}
				toks = append(toks, token{tDec, string(rs[start:i]), start})
			} else {
				toks = append(toks, token{tNum, string(rs[start:i]), start})
			}
		case isIdentStart(r):
			start := i
			for i < len(rs) && isIdentCont(rs[i]) {
				i++
			}
			// Absorb qualified-name segments `.seg` (a dot followed by an
			// identifier, NOT a digit — `.1`/`.2` stay postfix projections). So
			// `Mod.fn` is one identifier resolved via the (prefixed) ref table.
			for i+1 < len(rs) && rs[i] == '.' && isIdentStart(rs[i+1]) {
				i++ // the dot
				for i < len(rs) && isIdentCont(rs[i]) {
					i++
				}
			}
			toks = append(toks, keyword(string(rs[start:i]), start))
		default:
			return nil, fmt.Errorf("unexpected character %q at offset %d", string(r), i)
		}
	}
	toks = append(toks, token{tEOF, "", len(rs)})
	return toks, nil
}

// keyword classifies an identifier word: the reserved words of §2 become their own
// token kinds, everything else is an identifier.
func keyword(word string, pos int) token {
	switch word {
	case "fn":
		return token{tFn, word, pos}
	case "is":
		return token{tIs, word, pos}
	case "end":
		return token{tEnd, word, pos}
	case "seq":
		return token{tSeq, word, pos}
	case "let":
		return token{tLet, word, pos}
	case "in":
		return token{tIn, word, pos}
	case "U":
		return token{tU, word, pos}
	case "U1", "U2", "U3", "U4", "U5", "U6", "U7", "U8", "U9":
		return token{tU, word, pos}
	case "_":
		return token{tHole, word, pos}
	case "data":
		return token{tData, word, pos}
	case "builtin":
		return token{tBuiltin, word, pos}
	case "case":
		return token{tCase, word, pos}
	case "of":
		return token{tOf, word, pos}
	case "with":
		return token{tWith, word, pos}
	case "calc":
		return token{tCalc, word, pos}
	case "by":
		return token{tBy, word, pos}
	case "mutual":
		return token{tMutual, word, pos}
	case "Prop":
		return token{tProp, word, pos}
	case "Eq":
		return token{tEq, word, pos}
	case "refl":
		return token{tRefl, word, pos}
	case "cast":
		return token{tCast, word, pos}
	case "subst":
		return token{tSubst, word, pos}
	case "Sig":
		return token{tSig, word, pos}
	case "Pair":
		return token{tPair, word, pos}
	case "Fst":
		return token{tFst, word, pos}
	case "Snd":
		return token{tSnd, word, pos}
	case "instance":
		return token{tInstance, word, pos}
	case "module":
		return token{tModule, word, pos}
	case "partial":
		return token{tPartial, word, pos}
	case "foreign":
		return token{tForeign, word, pos}
	default:
		return token{tIdent, word, pos}
	}
}

// scanStringToks reads a double-quoted string literal starting at rs[at] (the opening
// '"') and returns the tokens it expands to plus the offset just past the closing '"'.
// A PLAIN string (no unescaped '{') yields a single `tStr` with the DECODED content
// (escapes \n \t \r \\ \" \0 \{ \}), the surface sugar for a packed `Bytes` (GRAMMAR §2).
// An INTERPOLATED string `"a {e} b"` expands to the token stream for `("a " ++ show (e)
// ++ " b")`: each `{…}` is an embedded expression (re-lexed; matched braces nest), each
// literal run a chunk, joined by the Semigroup `++` over `show` (§5.4). `\{`/`\}` are
// literal braces. A newline or `"` inside an interpolation, or an unterminated quote, is
// an error. (Nested string literals inside an interpolation are not supported.)
func scanStringToks(rs []rune, at int) ([]token, int, error) {
	var segs []strSeg
	var chunk []rune
	chunkStart := at + 1
	hasExpr := false
	i := at + 1 // past the opening quote
	for i < len(rs) {
		c := rs[i]
		switch {
		case c == '"':
			segs = append(segs, strSeg{false, string(chunk), chunkStart})
			toks, err := interpTokens(at, hasExpr, segs)
			if err != nil {
				return nil, i, err
			}
			return toks, i + 1, nil
		case c == '\n':
			return nil, i, fmt.Errorf("unterminated string literal at offset %d (newline before closing quote)", at)
		case c == '\\':
			if i+1 >= len(rs) {
				return nil, i, fmt.Errorf("unterminated escape in string literal at offset %d", i)
			}
			switch rs[i+1] {
			case 'n':
				chunk = append(chunk, '\n')
			case 't':
				chunk = append(chunk, '\t')
			case 'r':
				chunk = append(chunk, '\r')
			case '\\':
				chunk = append(chunk, '\\')
			case '"':
				chunk = append(chunk, '"')
			case '0':
				chunk = append(chunk, 0)
			case '{':
				chunk = append(chunk, '{')
			case '}':
				chunk = append(chunk, '}')
			default:
				return nil, i, fmt.Errorf("unknown escape %q in string literal at offset %d", string(rs[i+1]), i)
			}
			i += 2
		case c == '{':
			// an interpolation: scan the expression source to the matching '}'.
			segs = append(segs, strSeg{false, string(chunk), chunkStart})
			chunk = nil
			hasExpr = true
			depth := 1
			j := i + 1
			exprStart := j
			for j < len(rs) && depth > 0 {
				switch rs[j] {
				case '{':
					depth++
				case '}':
					depth--
				case '"', '\n':
					return nil, j, fmt.Errorf("unterminated interpolation `{…}` in string literal at offset %d", i)
				}
				if depth == 0 {
					break
				}
				j++
			}
			if depth != 0 {
				return nil, i, fmt.Errorf("unterminated interpolation `{…}` in string literal at offset %d", i)
			}
			segs = append(segs, strSeg{true, string(rs[exprStart:j]), exprStart})
			i = j + 1 // past '}'
			chunkStart = i
		default:
			chunk = append(chunk, c)
			i++
		}
	}
	return nil, at, fmt.Errorf("unterminated string literal at offset %d (end of input)", at)
}

// scanBytesLit reads a byte-string literal b"…" starting at rs[at] (the 'b'; the
// opening '"' is at at+1) and returns a single tBytes token whose text is the
// DECODED raw bytes, plus the offset just past the closing '"'. Escapes: \xNN (two
// hex digits — an arbitrary byte), \n \t \r \\ \" \0. No interpolation. A bare
// source rune contributes its UTF-8 bytes (so multibyte text is real bytes). A
// newline or EOF before the closing quote, or a malformed \x, is an error.
func scanBytesLit(rs []rune, at int) (token, int, error) {
	var out []byte
	i := at + 2 // past b"
	for i < len(rs) {
		c := rs[i]
		switch {
		case c == '"':
			return token{tBytes, string(out), at}, i + 1, nil
		case c == '\n':
			return token{}, i, fmt.Errorf("unterminated byte-string literal at offset %d (newline before closing quote)", at)
		case c == '\\':
			if i+1 >= len(rs) {
				return token{}, i, fmt.Errorf("unterminated escape in byte-string literal at offset %d", i)
			}
			switch rs[i+1] {
			case 'n':
				out = append(out, '\n')
			case 't':
				out = append(out, '\t')
			case 'r':
				out = append(out, '\r')
			case '\\':
				out = append(out, '\\')
			case '"':
				out = append(out, '"')
			case '0':
				out = append(out, 0)
			case 'x':
				if i+3 >= len(rs) {
					return token{}, i, fmt.Errorf("unterminated \\x escape in byte-string literal at offset %d", i)
				}
				hi, ok1 := hexVal(rs[i+2])
				lo, ok2 := hexVal(rs[i+3])
				if !ok1 || !ok2 {
					return token{}, i, fmt.Errorf("malformed \\xNN escape in byte-string literal at offset %d", i)
				}
				out = append(out, byte(hi*16+lo))
				i += 4
				continue
			default:
				return token{}, i, fmt.Errorf("unknown escape %q in byte-string literal at offset %d", string(rs[i+1]), i)
			}
			i += 2
		default:
			out = append(out, []byte(string(c))...)
			i++
		}
	}
	return token{}, at, fmt.Errorf("unterminated byte-string literal at offset %d (end of input)", at)
}

// hexVal decodes a single hex digit (for \xNN byte-string escapes).
func hexVal(r rune) (int, bool) {
	switch {
	case r >= '0' && r <= '9':
		return int(r - '0'), true
	case r >= 'a' && r <= 'f':
		return int(r-'a') + 10, true
	case r >= 'A' && r <= 'F':
		return int(r-'A') + 10, true
	}
	return 0, false
}

// strSeg is a scanned string segment: a literal chunk (decoded) or an embedded
// expression source, with its source offset.
type strSeg struct {
	isExpr bool
	text   string
	pos    int
}

// interpTokens turns the scanned segments into a token stream. A plain string (no
// interpolation) is one `tStr`. Otherwise it is `( chunk ++ show ( e ) ++ … )`: literal
// chunks (non-empty) and `show (expr)` parts joined by `++`, parenthesized so the whole
// interpolation is one operand wherever it appears.
func interpTokens(at int, hasExpr bool, segs []strSeg) ([]token, error) {
	if !hasExpr {
		// plain string: the single literal chunk, decoded.
		return []token{{tStr, segs[0].text, at}}, nil
	}
	var parts [][]token
	for _, s := range segs {
		if s.isExpr {
			sub, err := lex(s.text)
			if err != nil {
				return nil, err
			}
			et := []token{{tIdent, "show", at}, {tLParen, "(", at}}
			for _, t := range sub {
				if t.kind == tEOF {
					continue
				}
				et = append(et, token{t.kind, t.text, s.pos + t.pos})
			}
			et = append(et, token{tRParen, ")", at})
			parts = append(parts, et)
		} else if len(s.text) > 0 {
			parts = append(parts, []token{{tStr, s.text, s.pos}})
		}
	}
	if len(parts) == 0 {
		return []token{{tStr, "", at}}, nil
	}
	out := []token{{tLParen, "(", at}}
	for i, p := range parts {
		if i > 0 {
			out = append(out, token{tOp, "++", at})
		}
		out = append(out, p...)
	}
	out = append(out, token{tRParen, ")", at})
	return out, nil
}

// skipBlockComment consumes a nestable {- … -} comment starting at i (positioned on
// the '{') and returns the offset just past the matching -}.
func skipBlockComment(rs []rune, i int) (int, error) {
	depth := 0
	for i < len(rs) {
		switch {
		case rs[i] == '{' && i+1 < len(rs) && rs[i+1] == '-':
			depth++
			i += 2
		case rs[i] == '-' && i+1 < len(rs) && rs[i+1] == '}':
			depth--
			i += 2
			if depth == 0 {
				return i, nil
			}
		default:
			i++
		}
	}
	return i, fmt.Errorf("unterminated block comment")
}
