package explain

import "strings"

// annotateWideMin is the width at or above which --annotate uses the
// two-column layout; below it the narrow interleaved layout is used. Width
// is supplied by the frontend (the CLI --width flag, default 80): the
// explainer never probes a TTY, so output stays deterministic.
const annotateWideMin = 100

// RenderAnnotate renders the annotated view of a Step tree: code beside its
// English. Wide (width >= annotateWideMin): two columns aligned by line,
// code left, English right, long code hard-wrapped at the column. Narrow:
// a `--` comment line with the English above each code fragment.
func RenderAnnotate(root Step, width int) string {
	rows := annotateRows(root)
	if width >= annotateWideMin {
		return renderWide(rows, width)
	}
	return renderNarrow(rows)
}

// annotateRow is one pre-order step: its indentation, source fragment (may
// be empty for structural steps), and bracketed English.
type annotateRow struct {
	ind  string
	code string
	eng  string
}

func annotateRows(root Step) []annotateRow {
	var rows []annotateRow
	var walk func(st Step, depth int)
	walk = func(st Step, depth int) {
		ind := ""
		if depth > 1 {
			ind = strings.Repeat(" ", (depth-1)*2)
		}
		rows = append(rows, annotateRow{ind: ind, code: st.Code, eng: "[" + st.Text + "]"})
		for _, k := range st.Kids {
			walk(k, depth+1)
		}
	}
	walk(root, 0)
	return rows
}

func renderNarrow(rows []annotateRow) string {
	var sb strings.Builder
	for _, r := range rows {
		sb.WriteString(r.ind + "-- " + r.eng + "\n")
		if r.code != "" {
			sb.WriteString(r.ind + r.code + "\n")
		}
	}
	return sb.String()
}

func renderWide(rows []annotateRow, width int) string {
	colW := 0
	for _, r := range rows {
		if n := len(r.ind) + len(r.code); n > colW {
			colW = n
		}
	}
	if capW := (width - 2) / 2; colW > capW {
		colW = capW
	}
	var sb strings.Builder
	for _, r := range rows {
		chunks := hardWrap(r.ind+r.code, colW)
		first := chunks[0]
		sb.WriteString(first + strings.Repeat(" ", colW-len(first)) + "  " + r.ind + r.eng + "\n")
		for _, ch := range chunks[1:] {
			sb.WriteString(ch + "\n")
		}
	}
	return sb.String()
}

// hardWrap splits s into chunks of at most w bytes (the explainer's output
// is ASCII); an empty s yields one empty chunk.
func hardWrap(s string, w int) []string {
	if s == "" || w <= 0 {
		return []string{""}
	}
	var out []string
	for len(s) > w {
		out = append(out, s[:w])
		s = s[w:]
	}
	return append(out, s)
}
