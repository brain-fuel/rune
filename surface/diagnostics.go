package surface

import (
	"regexp"
	"strconv"
	"strings"
)

// offsetRe pulls the byte offset out of a parser message. Every positional parse
// error ends with "… at offset N" (the parser threads token.pos through), so the
// renderer can recover the location without the parser returning a structured value
// at all of its ~30 error sites.
var offsetRe = regexp.MustCompile(`\s*\(?at offset (\d+)\)?`)

// RenderParseError turns a flat parser error into a located, caret-pointed message
// against the source it was parsed from — the Elm/Rust shape: say what went wrong,
// then SHOW where, with the offending line and a caret beneath the exact column.
// When the message carries no offset (or src is empty) it is returned unchanged, so
// this is always safe to wrap around any parse error.
func RenderParseError(src string, err error) string {
	if err == nil {
		return ""
	}
	msg := err.Error()
	loc := offsetRe.FindStringSubmatchIndex(msg)
	if loc == nil || src == "" {
		return msg
	}
	off, _ := strconv.Atoi(msg[loc[2]:loc[3]])
	// Drop the "at offset N" tail — the caret says it better.
	clean := strings.TrimSpace(msg[:loc[0]] + msg[loc[1]:])
	if clean == "" {
		clean = msg
	}
	if off < 0 {
		off = 0
	}
	if off > len(src) {
		off = len(src)
	}

	// Locate the line containing off.
	lineStart := strings.LastIndexByte(src[:off], '\n') + 1
	lineEnd := strings.IndexByte(src[off:], '\n')
	if lineEnd < 0 {
		lineEnd = len(src)
	} else {
		lineEnd += off
	}
	lineNo := strings.Count(src[:lineStart], "\n") + 1
	col := off - lineStart // 0-based column within the line

	line := strings.ReplaceAll(src[lineStart:lineEnd], "\t", " ")
	gutter := strconv.Itoa(lineNo)
	pad := strings.Repeat(" ", len(gutter))

	var sb strings.Builder
	sb.WriteString(humanizeParse(clean))
	sb.WriteString("\n\n")
	sb.WriteString("  " + gutter + " | " + line + "\n")
	sb.WriteString("  " + pad + " | " + strings.Repeat(" ", col) + "^")
	return sb.String()
}

// humanizeParse rewrites the parser's terse token vocabulary into the words a reader
// thinks in. It is intentionally light — the caret carries the location, so this
// only softens the phrasing — and leaves anything it does not recognise untouched.
func humanizeParse(msg string) string {
	repl := strings.NewReplacer(
		"unexpected end of input", "I reached the end of the input before this form was finished",
		"end-of-input", "the end of the input",
		"found EOF", "found the end of the input",
	)
	return repl.Replace(msg)
}
