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
	// Builtin IO group (store/io.go) - not an ioprim, but a step shape.
	"pureIO": {verb: "Give Back %s", args: 1},

	// D6 console / OS basics.
	"printNat":     {verb: "Print %s to Command Line", args: 1},
	"getNat":       {verb: "Get Number from Command Line", args: 0, bound: "Get Number `%s` from Command Line"},
	"timeNanos":    {verb: "Read the OS Clock in Nanoseconds", args: 0},
	"readLineCode": {verb: "Read a Line from Command Line", args: 0, bound: "Read a Line `%s` from Command Line"},

	// D6 stream / file-processing vocabulary.
	"foldLines":    {verb: "Process File %s Line by Line with Step %s from Start Value %s", args: 3},
	"foldDir":      {verb: "Process Directory %s (files ending %s) with Step %s from Start Value %s", args: 4},
	"splitOn":      {verb: "Split on Separator %s: %s", args: 2},
	"byteLen":      {verb: "Measure the Byte Length of %s", args: 1},
	"jsonStrField": {verb: "Extract JSON Field %s from %s", args: 2},
	"sqlQuote":     {verb: "SQL-Quote %s", args: 1},
	"openWrite":    {verb: "Open %s for Writing", args: 1},
	"writeChunk":   {verb: "Write to Handle %s the Line %s", args: 2},
	"closeWrite":   {verb: "Close Handle %s", args: 1},
	"sortFile":     {verb: "Sort the Lines of File %s into %s", args: 2},
	"dbApply":      {verb: "Run on Database %s the SQL Script %s", args: 2},

	// D6 env / file / argv / process.
	"getEnvCode":    {verb: "Get Environment Variable %s", args: 1},
	"readFileCode":  {verb: "Read File %s", args: 1},
	"writeFileCode": {verb: "Write to File %s the Content %s", args: 2},
	"printStrCode":  {verb: "Print Text %s to Command Line", args: 1},
	"argCountCode":  {verb: "Count the Command-Line Arguments", args: 0},
	"argAtCode":     {verb: "Get Command-Line Argument %s", args: 1},
	"exitWith":      {verb: "Exit the Program with Status %s", args: 1},

	// scribe rasterizer accel.
	"rasterFill": {verb: "Rasterize a %s by %s Alpha Mask from Stream %s", args: 3},

	// D3 machine floats (f64).
	"fromNat":    {verb: "Convert %s to Float", args: 1},
	"fadd":       {verb: "Add %s and %s", args: 2},
	"fsub":       {verb: "Subtract %s minus %s", args: 2},
	"fmul":       {verb: "Multiply %s by %s", args: 2},
	"fdiv":       {verb: "Divide %s by %s", args: 2},
	"fabsP":      {verb: "Take the Absolute Value of %s", args: 1},
	"floatToNat": {verb: "Truncate %s to a Whole Number", args: 1},
	"fleqN":      {verb: "Test whether %s is at most %s", args: 2},
	"fsqrt":      {verb: "Take the Square Root of %s", args: 1},
	"fpow":       {verb: "Raise %s to the Power %s", args: 2},
	"parseFloat": {verb: "Parse %s as a Float", args: 1},
	"getFloat":   {verb: "Get Float from Command Line", args: 0, bound: "Get Float `%s` from Command Line"},
	"printFloat": {verb: "Print %s to Command Line", args: 1},

	// D3 BLAS kernels.
	"dot2":    {verb: "Compute the Dot Product of %s, %s and %s, %s", args: 4},
	"dotList": {verb: "Compute the Dot Product of %s and %s", args: 2},
	"gemmSum": {verb: "Multiply Matrices %s and %s and Sum the Entries", args: 2},

	// D4 NumPy-capability suite.
	"npDot":    {verb: "Compute the Dot Product of %s and %s (NumPy)", args: 2},
	"npMean":   {verb: "Compute the Mean of %s (NumPy)", args: 1},
	"npMatSum": {verb: "Multiply Matrices %s and %s and Sum the Entries (NumPy)", args: 2},
	"npVar":    {verb: "Compute the Variance of %s (NumPy)", args: 1},
	"npMax":    {verb: "Find the Maximum of %s (NumPy)", args: 1},
	"npNorm":   {verb: "Compute the Norm of %s (NumPy)", args: 1},

	// E4 wavelet data plane (kv / object / queue).
	"kvPutCode":   {verb: "Store under Key %s the Value %s in the Key-Value Store", args: 2},
	"kvGetCode":   {verb: "Fetch the Value under Key %s from the Key-Value Store", args: 1},
	"kvDelCode":   {verb: "Delete Key %s from the Key-Value Store", args: 1},
	"objPutCode":  {verb: "Store under Object Key %s the Content %s in the Object Store", args: 2},
	"objGetCode":  {verb: "Fetch Object %s from the Object Store", args: 1},
	"objDelCode":  {verb: "Delete Object %s from the Object Store", args: 1},
	"enqueueCode": {verb: "Add %s to the Queue", args: 1},
	"dequeueCode": {verb: "Take the Next Item from the Queue", args: 0},

	// Phase-0 real-byte strings (Bin).
	"binEmpty": {verb: "Make an Empty Byte String", args: 0},
	"binCons":  {verb: "Prepend Byte %s to %s", args: 2},
	"binLen":   {verb: "Measure the Length of %s", args: 1},
	"binAt":    {verb: "Get from %s the Byte at Position %s", args: 2},
	"printBin": {verb: "Print %s to Command Line", args: 1},

	// Phase-1 sockets.
	"sockConnect": {verb: "Connect to Host %s on Port %s", args: 2},
	"sockWrite":   {verb: "Send over Connection %s the Data %s", args: 2},
	"sockRead":    {verb: "Receive from Connection %s up to %s Bytes", args: 2},
	"sockClose":   {verb: "Close Connection %s", args: 1},
	"sockListen":  {verb: "Listen for Connections on Port %s", args: 1},
	"sockAccept":  {verb: "Accept a Connection from Listener %s", args: 1},

	// Phase-1 filesystem over Bin.
	"fsWrite":  {verb: "Write to File %s the Bytes %s", args: 2},
	"fsRead":   {verb: "Read the Bytes of File %s", args: 1},
	"fsExists": {verb: "Check whether File %s Exists", args: 1},
	"fsRemove": {verb: "Remove File %s", args: 1},
	"fsMkdir":  {verb: "Make Directory %s", args: 1},

	// Singletons: process, crypto, TLS.
	"procRun": {verb: "Run Program %s with Argument %s", args: 2},
	"sha256":  {verb: "Compute the SHA-256 Digest of %s", args: 1},
	"tlsGet":  {verb: "Fetch over HTTPS from Host %s Port %s Path %s", args: 3},
}
