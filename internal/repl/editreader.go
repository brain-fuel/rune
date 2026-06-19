package repl

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
)

// editReader is the interactive line editor: raw-mode terminal input with cursor
// movement, an in-session history ring (Up/Down + Ctrl-P/Ctrl-N), Ctrl-R reverse
// incremental search, and a persistent ~/.rune_history. Raw mode is entered only
// for the duration of a single ReadLine — results are printed in cooked mode — so a
// `\n` from the program still behaves normally between prompts.
//
// The key handling and reverse-search UI follow the conventions of the MIT-licensed
// github.com/chzyer/readline (used as a reference only; not imported or vendored).
type editReader struct {
	in       *os.File
	out      io.Writer
	r        *bufio.Reader
	hist     []string
	histPath string
}

func newEditReader(in, out *os.File) (lineReader, error) {
	e := &editReader{in: in, out: out, r: bufio.NewReader(in), histPath: historyPath()}
	e.loadHistory()
	return e, nil
}

// historyPath is ~/.rune_history, or "" (in-memory only) when there is no home dir.
func historyPath() string {
	home, err := os.UserHomeDir()
	if err != nil || home == "" {
		return ""
	}
	return filepath.Join(home, ".rune_history")
}

func (e *editReader) loadHistory() {
	if e.histPath == "" {
		return
	}
	data, err := os.ReadFile(e.histPath)
	if err != nil {
		return
	}
	for _, ln := range strings.Split(string(data), "\n") {
		if ln != "" {
			e.hist = append(e.hist, ln)
		}
	}
}

// AddHistory records a submitted form, de-duping consecutive repeats and skipping
// blanks. Multi-line forms are stored with newlines escaped so one entry is one line
// in the history file (and recalls as a single editable line).
func (e *editReader) AddHistory(line string) {
	line = strings.TrimRight(line, "\n")
	if strings.TrimSpace(line) == "" {
		return
	}
	stored := strings.ReplaceAll(line, "\n", " ")
	if n := len(e.hist); n > 0 && e.hist[n-1] == stored {
		return
	}
	e.hist = append(e.hist, stored)
	if e.histPath != "" {
		if f, err := os.OpenFile(e.histPath, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o600); err == nil {
			fmt.Fprintln(f, stored)
			f.Close()
		}
	}
}

func (e *editReader) Close() error { return nil }

func (e *editReader) write(s string) { io.WriteString(e.out, s) }
func (e *editReader) newline()       { e.write("\r\n") }

// render repaints the single edit line: carriage-return to column 0, clear to end of
// line, write prompt + buffer, then move the cursor back to its logical position.
func (e *editReader) render(prompt string, buf []rune, pos int) {
	s := "\r\x1b[K" + prompt + string(buf)
	if back := len(buf) - pos; back > 0 {
		s += fmt.Sprintf("\x1b[%dD", back)
	}
	e.write(s)
}

// ReadLine enters raw mode, runs the edit loop, restores the terminal, and returns
// the submitted line. ok=false is a clean EOF (Ctrl-D on an empty line).
func (e *editReader) ReadLine(prompt string) (string, bool, error) {
	st, err := enterRaw(e.in.Fd())
	if err != nil {
		return "", false, err
	}
	defer restoreRaw(e.in.Fd(), st)
	return e.edit(prompt)
}

func (e *editReader) edit(prompt string) (string, bool, error) {
	var buf []rune
	pos := 0
	histIdx := len(e.hist) // index into hist; == len means the live (un-saved) line
	saved := ""            // the live line, stashed while browsing history
	e.render(prompt, buf, pos)

	for {
		r, _, err := e.r.ReadRune()
		if err != nil {
			e.newline()
			if err == io.EOF {
				return "", false, nil
			}
			return "", false, err
		}
		switch r {
		case '\r', '\n':
			e.newline()
			return string(buf), true, nil
		case 3: // Ctrl-C: abandon the current line, stay in the REPL
			e.write("^C")
			e.newline()
			return "", true, nil
		case 4: // Ctrl-D: EOF on empty, else delete-forward
			if len(buf) == 0 {
				e.newline()
				return "", false, nil
			}
			if pos < len(buf) {
				buf = append(buf[:pos], buf[pos+1:]...)
			}
		case 127, 8: // Backspace
			if pos > 0 {
				buf = append(buf[:pos-1], buf[pos:]...)
				pos--
			}
		case 1: // Ctrl-A: home
			pos = 0
		case 5: // Ctrl-E: end
			pos = len(buf)
		case 2: // Ctrl-B: left
			if pos > 0 {
				pos--
			}
		case 6: // Ctrl-F: right
			if pos < len(buf) {
				pos++
			}
		case 21: // Ctrl-U: kill to start
			buf = append([]rune{}, buf[pos:]...)
			pos = 0
		case 11: // Ctrl-K: kill to end
			buf = buf[:pos]
		case 23: // Ctrl-W: kill previous word
			i := pos
			for i > 0 && buf[i-1] == ' ' {
				i--
			}
			for i > 0 && buf[i-1] != ' ' {
				i--
			}
			buf = append(buf[:i], buf[pos:]...)
			pos = i
		case 12: // Ctrl-L: clear screen
			e.write("\x1b[2J\x1b[H")
		case 16: // Ctrl-P: previous history
			buf, pos, histIdx, saved = e.histUp(buf, pos, histIdx, saved)
		case 14: // Ctrl-N: next history
			buf, pos, histIdx, saved = e.histDown(buf, histIdx, saved)
		case 18: // Ctrl-R: reverse incremental search
			nb, np, submit, eof, serr := e.reverseSearch(buf)
			if serr != nil {
				e.newline()
				return "", false, serr
			}
			if eof {
				e.newline()
				return "", false, nil
			}
			buf, pos = nb, np
			histIdx = len(e.hist)
			if submit {
				e.newline()
				return string(buf), true, nil
			}
		case 27: // ESC: an escape sequence (arrows, Home/End, Delete)
			buf, pos, histIdx, saved = e.escape(buf, pos, histIdx, saved)
		default:
			if r >= 32 {
				buf = append(buf, 0)
				copy(buf[pos+1:], buf[pos:])
				buf[pos] = r
				pos++
			}
		}
		e.render(prompt, buf, pos)
	}
}

// escape consumes an ESC-introduced sequence and applies the editing action.
func (e *editReader) escape(buf []rune, pos, histIdx int, saved string) ([]rune, int, int, string) {
	r2, _, err := e.r.ReadRune()
	if err != nil || (r2 != '[' && r2 != 'O') {
		return buf, pos, histIdx, saved
	}
	r3, _, err := e.r.ReadRune()
	if err != nil {
		return buf, pos, histIdx, saved
	}
	switch r3 {
	case 'A': // up
		return e.histUp(buf, pos, histIdx, saved)
	case 'B': // down
		return e.histDown(buf, histIdx, saved)
	case 'C': // right
		if pos < len(buf) {
			pos++
		}
	case 'D': // left
		if pos > 0 {
			pos--
		}
	case 'H': // home
		pos = 0
	case 'F': // end
		pos = len(buf)
	case '1', '3', '4', '7', '8': // extended: <n>~
		r4, _, _ := e.r.ReadRune()
		if r4 == '~' {
			switch r3 {
			case '3': // delete-forward
				if pos < len(buf) {
					buf = append(buf[:pos], buf[pos+1:]...)
				}
			case '1', '7':
				pos = 0
			case '4', '8':
				pos = len(buf)
			}
		}
	}
	return buf, pos, histIdx, saved
}

// histUp recalls the previous history entry, stashing the live line on first step.
func (e *editReader) histUp(buf []rune, pos, idx int, saved string) ([]rune, int, int, string) {
	if idx == len(e.hist) {
		saved = string(buf)
	}
	if idx > 0 {
		idx--
		nb := []rune(e.hist[idx])
		return nb, len(nb), idx, saved
	}
	return buf, pos, idx, saved
}

// histDown moves toward (and back to) the live line.
func (e *editReader) histDown(buf []rune, idx int, saved string) ([]rune, int, int, string) {
	if idx < len(e.hist) {
		idx++
		var nb []rune
		if idx == len(e.hist) {
			nb = []rune(saved)
		} else {
			nb = []rune(e.hist[idx])
		}
		return nb, len(nb), idx, saved
	}
	return buf, len(buf), idx, saved
}

// reverseSearch runs the Ctrl-R incremental search. It returns the chosen buffer and
// cursor, whether to submit immediately (Enter), and whether EOF was hit. Ctrl-G/ESC
// cancels (restores the original line); a fresh query searches from the newest entry,
// and a repeated Ctrl-R steps to the next older match.
func (e *editReader) reverseSearch(cur []rune) (buf []rune, pos int, submit, eof bool, err error) {
	query := []rune{}
	matchIdx := -1
	match := string(cur)
	find := func(from int) {
		q := string(query)
		for i := from; i >= 0; i-- {
			if strings.Contains(e.hist[i], q) {
				matchIdx, match = i, e.hist[i]
				return
			}
		}
		matchIdx = -1
	}
	draw := func() {
		e.write("\r\x1b[K" + fmt.Sprintf("(reverse-i-search)`%s': %s", string(query), match))
	}
	draw()
	for {
		r, _, rerr := e.r.ReadRune()
		if rerr != nil {
			return cur, len(cur), false, rerr == io.EOF, nil
		}
		switch {
		case r == '\r' || r == '\n':
			m := []rune(match)
			return m, len(m), true, false, nil
		case r == 18: // Ctrl-R: next older match
			from := len(e.hist) - 1
			if matchIdx >= 0 {
				from = matchIdx - 1
			}
			find(from)
			draw()
		case r == 7 || r == 27: // Ctrl-G or ESC: cancel
			return cur, len(cur), false, false, nil
		case r == 3: // Ctrl-C: cancel search, keep the original line
			return cur, len(cur), false, false, nil
		case r == 127 || r == 8: // Backspace
			if len(query) > 0 {
				query = query[:len(query)-1]
				find(len(e.hist) - 1)
			}
			draw()
		case r >= 32:
			query = append(query, r)
			find(len(e.hist) - 1)
			draw()
		default:
			// Any other control key: accept the current match and leave search.
			m := []rune(match)
			return m, len(m), false, false, nil
		}
	}
}
