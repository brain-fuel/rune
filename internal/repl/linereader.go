package repl

import (
	"bufio"
	"fmt"
	"io"
	"os"
)

// lineReader abstracts reading one line of input with a given prompt. It is the
// seam between the REPL loop and HOW a line arrives: a plain bufio.Scanner (the
// non-TTY fallback, used by tests and piped input) or an interactive terminal
// editor with history and reverse search (a real TTY). ReadLine owns prompt
// printing so the loop never writes the prompt itself — that keeps the fallback's
// byte output identical to the legacy path and avoids a double prompt on a TTY.
type lineReader interface {
	// ReadLine prints prompt and returns the next submitted line (no trailing
	// newline). ok=false is a clean end of input (EOF, or Ctrl-D on an empty line);
	// err is reserved for a genuine I/O failure on the stream.
	ReadLine(prompt string) (line string, ok bool, err error)
	// AddHistory records a submitted form for later recall (no-op on the fallback).
	AddHistory(line string)
	// Close restores the terminal and flushes history. Safe to call more than once.
	Close() error
}

// newLineReader picks the interactive editor when BOTH streams are real terminals
// (and the editor initializes), else the bufio fallback. Tests and pipes pass
// non-*os.File streams, so they deterministically get scanReader and behave exactly
// as before.
func newLineReader(in io.Reader, out io.Writer) lineReader {
	if inF, ok := in.(*os.File); ok {
		if outF, ok := out.(*os.File); ok {
			if isTerminal(inF.Fd()) && isTerminal(outF.Fd()) {
				if ed, err := newEditReader(inF, outF); err == nil {
					return ed
				}
			}
		}
	}
	return newScanReader(in, out)
}

// scanReader is the line-buffered fallback: it reproduces the pre-editor behavior
// (print the prompt to out, read one line via bufio.Scanner) exactly.
type scanReader struct {
	sc  *bufio.Scanner
	out io.Writer
}

func newScanReader(in io.Reader, out io.Writer) *scanReader {
	sc := bufio.NewScanner(in)
	sc.Buffer(make([]byte, 0, 64*1024), 1024*1024)
	return &scanReader{sc: sc, out: out}
}

func (r *scanReader) ReadLine(prompt string) (string, bool, error) {
	fmt.Fprint(r.out, prompt)
	if !r.sc.Scan() {
		return "", false, r.sc.Err()
	}
	return r.sc.Text(), true, nil
}

func (r *scanReader) AddHistory(string) {}
func (r *scanReader) Close() error      { return nil }

// --- interactive editor seam ---------------------------------------------------
// isTerminal and newEditReader are the entry points the interactive editor plugs
// into. Until the editor lands (checkpoint 3) they decline, so newLineReader always
// returns the bufio fallback. Replaced by the platform-tagged editor files.

func isTerminal(uintptr) bool { return false }

func newEditReader(_, _ *os.File) (lineReader, error) {
	return nil, errNoEditor
}

var errNoEditor = fmt.Errorf("interactive editor not available")
