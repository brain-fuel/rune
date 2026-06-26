package ledger

import (
	"fmt"
	"os/exec"
	"strings"

	"goforge.dev/rune/v3/internal/session"
)

// lineOf returns the 1-based line number of byte offset off in src.
// If off exceeds len(src) it is clamped to len(src).
func lineOf(src string, off int) int {
	if off > len(src) {
		off = len(src)
	}
	return 1 + strings.Count(src[:off], "\n")
}

// GitBlame attributes a single line in file to its last committer via porcelain
// git blame output. On any failure (not a git repo, untracked file, git not in
// PATH) it returns a zero Provenance and the error; the caller treats that as
// "unattributed" - never fatal.
func GitBlame(file string, line int) (Provenance, error) {
	cmd := exec.Command("git", "blame", "-L",
		fmt.Sprintf("%d,%d", line, line), "--porcelain", "--", file)
	out, err := cmd.Output()
	if err != nil {
		return Provenance{}, err
	}
	var p Provenance
	lines := strings.Split(string(out), "\n")
	// The first porcelain line is: <40-hex-sha> <orig-line> <curr-line> [<count>]
	// The commit sha is the first whitespace-delimited token.
	if len(lines) > 0 {
		fields := strings.Fields(lines[0])
		if len(fields) > 0 && len(fields[0]) >= 7 {
			p.Commit = fields[0]
		}
	}
	// Subsequent lines include "author <Name>" and "author-time <unix-ts>".
	// "author " (with a trailing space) does NOT match "author-mail"/"author-time"/
	// "author-tz" since those have a '-' after "author".
	for _, ln := range lines {
		if strings.HasPrefix(ln, "author ") {
			p.Author = strings.TrimPrefix(ln, "author ")
		} else if strings.HasPrefix(ln, "author-time ") {
			p.Date = strings.TrimPrefix(ln, "author-time ")
		}
	}
	return p, nil
}

// BuildWithSource is Build plus git-blame provenance. It calls Build, then for
// each entry uses the byte offset of the definition's name token (from the
// session) to compute the 1-based source line, and if file is non-empty it
// calls GitBlame to fill Provenance. Blame failures are silently ignored;
// the entry's Provenance stays zero-valued (unattributed).
func BuildWithSource(s *session.Session, file, src string) []Entry {
	es := Build(s)
	defs := s.Defs()
	byName := make(map[string]session.Def, len(defs))
	for _, d := range defs {
		byName[d.Name] = d
	}
	for i := range es {
		d := byName[es[i].Name]
		es[i].File = file
		es[i].Line = lineOf(src, d.Pos)
		if file != "" {
			if p, err := GitBlame(file, es[i].Line); err == nil {
				es[i].Provenance = p
			}
		}
	}
	return es
}
