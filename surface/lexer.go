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
	tOp      // an infix operator: + - * / % (a symbolic identifier; GRAMMAR §5.4)
	tBuiltin // builtin (a builtin-binding declaration: builtin nat Nat zero succ)
	tCase    // case (a case expression; GRAMMAR §5.6)
	tOf      // of   (separates the scrutinee from the clauses)
	tWith    // with (binds the induction hypotheses in a clause)
	tCalc    // calc (an equational-reasoning block; GRAMMAR §5.7)
	tBy      // by   (justifies one calc step)
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
		case r == '"':
			s, end, err := scanString(rs, i)
			if err != nil {
				return nil, err
			}
			toks = append(toks, token{tStr, s, i})
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

// scanString reads a double-quoted string literal starting at rs[at] (the opening
// '"') and returns the DECODED content (escapes resolved) and the offset just past
// the closing '"'. Supported escapes: \n \t \r \\ \" \0. The content is sugar for a
// packed `Bytes` value (the parser desugars it over the UTF-8 bytes); a newline or
// EOF before the closing quote is an error.
func scanString(rs []rune, at int) (string, int, error) {
	var b []rune
	i := at + 1 // past the opening quote
	for i < len(rs) {
		c := rs[i]
		switch {
		case c == '"':
			return string(b), i + 1, nil
		case c == '\n':
			return "", i, fmt.Errorf("unterminated string literal at offset %d (newline before closing quote)", at)
		case c == '\\':
			if i+1 >= len(rs) {
				return "", i, fmt.Errorf("unterminated escape in string literal at offset %d", i)
			}
			switch rs[i+1] {
			case 'n':
				b = append(b, '\n')
			case 't':
				b = append(b, '\t')
			case 'r':
				b = append(b, '\r')
			case '\\':
				b = append(b, '\\')
			case '"':
				b = append(b, '"')
			case '0':
				b = append(b, 0)
			default:
				return "", i, fmt.Errorf("unknown escape %q in string literal at offset %d", string(rs[i+1]), i)
			}
			i += 2
		default:
			b = append(b, c)
			i++
		}
	}
	return "", at, fmt.Errorf("unterminated string literal at offset %d (end of input)", at)
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
