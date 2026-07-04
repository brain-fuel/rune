package explain

import "strings"

// RenderText flattens a Step tree to the bracketed step list: the root and
// its direct Kids print flush left (the spec's four-line double view), and
// each level below that indents two more spaces. Output ends with a newline.
func RenderText(root Step) string {
	var sb strings.Builder
	var walk func(st Step, depth int)
	walk = func(st Step, depth int) {
		ind := 0
		if depth > 1 {
			ind = (depth - 1) * 2
		}
		sb.WriteString(strings.Repeat(" ", ind))
		sb.WriteString("[" + st.Text + "]\n")
		for _, k := range st.Kids {
			walk(k, depth+1)
		}
	}
	walk(root, 0)
	return sb.String()
}
