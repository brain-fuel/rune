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
	tLambda // \
	tArrow  // ->
	tColon  // :
	tEquals // =
	tLet
	tIn
	tU
)

type token struct {
	kind tokKind
	text string
	pos  int
	col  int // column within its line; col 0 marks a top-level definition start
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
	case tLambda:
		return "'\\'"
	case tArrow:
		return "'->'"
	case tColon:
		return "':'"
	case tEquals:
		return "'='"
	case tLet:
		return "'let'"
	case tIn:
		return "'in'"
	case tU:
		return "'U'"
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

// lex turns source into a token slice. It is intentionally small: the surface
// language is exactly variables, lambda, application, Pi, let, U, and annotation.
//
// Each token records its column. A column-0 token marks the start of a top-level
// definition; continuation lines are indented. This layout rule is how a flat list
// of definitions is delimited without statement terminators.
func lex(src string) ([]token, error) {
	var toks []token
	rs := []rune(src)
	i := 0
	lineStart := 0
	for i < len(rs) {
		r := rs[i]
		col := i - lineStart
		switch {
		case r == '\n':
			i++
			lineStart = i
		case r == ' ', r == '\t', r == '\r':
			i++
		case r == '-' && i+1 < len(rs) && rs[i+1] == '-':
			for i < len(rs) && rs[i] != '\n' {
				i++
			}
		case r == '(':
			toks = append(toks, token{tLParen, "(", i, col})
			i++
		case r == ')':
			toks = append(toks, token{tRParen, ")", i, col})
			i++
		case r == '\\':
			toks = append(toks, token{tLambda, "\\", i, col})
			i++
		case r == ':':
			toks = append(toks, token{tColon, ":", i, col})
			i++
		case r == '=':
			toks = append(toks, token{tEquals, "=", i, col})
			i++
		case r == '-' && i+1 < len(rs) && rs[i+1] == '>':
			toks = append(toks, token{tArrow, "->", i, col})
			i += 2
		case isIdentStart(r):
			start := i
			for i < len(rs) && isIdentCont(rs[i]) {
				i++
			}
			word := string(rs[start:i])
			switch word {
			case "let":
				toks = append(toks, token{tLet, word, start, col})
			case "in":
				toks = append(toks, token{tIn, word, start, col})
			case "U":
				toks = append(toks, token{tU, word, start, col})
			default:
				toks = append(toks, token{tIdent, word, start, col})
			}
		default:
			return nil, fmt.Errorf("unexpected character %q at offset %d", string(r), i)
		}
	}
	toks = append(toks, token{tEOF, "", len(rs), 0})
	return toks, nil
}
