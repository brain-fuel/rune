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
	tLBrace // {  (implicit binder/argument; '{-' opens a comment instead)
	tRBrace // }
	tHole   // _  (a hole: a metavariable for elaboration to solve)
	tData   // data (a datatype declaration; Phase 4)
	tBar    // |  (constructor separator)
	tProp   // Prop (the universe of propositions; Phase 3)
	tEq     // Eq   (the observational equality type former)
	tRefl   // refl (the reflexivity proof)
	tCast   // cast (transport along a type equality)
	tSubst  // subst (Leibniz transport along an equality; Phase 4)
	tQty    // 0 or 1: a usage annotation on a binder (Phase 5)
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
	case tQty:
		return "quantity"
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
		case r == '|':
			toks = append(toks, token{tBar, "|", i})
			i++
		case r == '-' && i+1 < len(rs) && rs[i+1] == '>':
			toks = append(toks, token{tArrow, "->", i})
			i += 2
		case r == '0' || r == '1':
			toks = append(toks, token{tQty, string(r), i})
			i++
		case isIdentStart(r):
			start := i
			for i < len(rs) && isIdentCont(rs[i]) {
				i++
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
	default:
		return token{tIdent, word, pos}
	}
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
