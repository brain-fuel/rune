package explain

// primTemplate is the English rendering of one host-op prim.
//
// verb is a fmt template with one %s per consumed argument, in application
// order. args is the number of TRAILING explicit arguments the template
// consumes (leading explicit arguments are type parameters, e.g. foldLines'
// state type, and are not shown). bound, when non-empty and args == 0, is
// the fmt template used when a bindIO chain names the result (its one %s is
// the binder name); every other bound step appends " as `x`". Template text
// must never contain an em or en dash (ASCII hyphen only); the coverage test
// in templates_test.go enforces this.
type primTemplate struct {
	verb  string
	args  int
	bound string
}

// primTemplates maps a prim's LAST-DOT-SEGMENT name (codegen's primName rule)
// to its English template. Every name in codegen.IOPrimNames() must have an
// entry (Task 4's coverage gate). pureIO is listed too: it is a builtin, not
// an ioprim, but it renders as an ordinary payload step. bindIO is structural
// (the chain flattener) and never appears in output.
var primTemplates = map[string]primTemplate{
	"pureIO":     {verb: "Give Back %s", args: 1},
	"getFloat":   {verb: "Get Float from Command Line", args: 0, bound: "Get Float `%s` from Command Line"},
	"printFloat": {verb: "Print %s to Command Line", args: 1},
	"printNat":   {verb: "Print %s to Command Line", args: 1},
	"getNat":     {verb: "Get Number from Command Line", args: 0, bound: "Get Number `%s` from Command Line"},
	"parseFloat": {verb: "Parse %s as a Float", args: 1},
	"fromNat":    {verb: "Convert %s to Float", args: 1},
	"fmul":       {verb: "Multiply %s by %s", args: 2},
}
