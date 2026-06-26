# Assurance Ledger Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Every definition that states a control or foreign claim gets a tier (`postulate` / `assume` / `guard` / `proven`), a content-hashed proposition identity, a proof hash when proven, git-blame provenance, postulate->proven upgrade detection, and a CI gate — surfaced by a new `rune ledger` command, with zero kernel change.

**Architecture:** A new pure-Go `ledger/` package (the read-side dual of `infra/`/`codegen/`) computes a ledger from a loaded `session.Session`. Tier is classified from existing facts: assumed-ness (`store.assumed`), proof certification (the cert cache), body presence, and three new pieces of SURFACE metadata threaded session-side only (postulate-ness + reason, guard usage, source position) — never into `core`/`store` hashing. The proposition identity is `core.HashTerm(def.Ty)` and the proof identity is `core.HashTerm(def.Body)`; both already exist. The `ledger` package renders text/JSON, detects upgrades by grouping on the proposition hash, and gates CI. Kernel, store hashing, and the 0x06 hash format are untouched.

**Tech Stack:** Go (stdlib only; `os/exec` to shell `git blame`), the rune surface (lexer/parser), `internal/session`, `cmd/rune` CLI, the Go test harness.

## Global Constraints

- **Kernel frozen.** No outer-core changes. Hash-format stays `0x06`. No new `core` constructor. The ledger's new per-definition metadata (postulate flag, reason, guard usage, source position) lives in the `internal/session` layer ONLY and is NEVER hashed (proposition/proof identity stay `core.HashTerm(Ty)` / `core.HashTerm(Body)`).
- **Content-addressing is the upgrade detector.** Proposition identity = `core.HashTerm(def.Ty)`; proof identity = `core.HashTerm(def.Body)`. A postulate->proven transition is the SAME proposition hash gaining a proof hash. No new VC mechanism: provenance reads `git blame`.
- **Reuse, do not rebuild.** Assumed-ness reuses `store.assumed` via `IsAssumed`/`Assumptions` (`store/store.go:161,164`). Proof status reuses `session.Certified` (`internal/session/session.go:1012`). Guard tier reuses the existing `with post ... guard ... blame` desugar (`surface/parser.go:530`). Diagnostics use `elaborate.Diagnostic` (`elaborate/diagnostic.go:22`).
- **Process standards.** Contextual keywords only (no reserved-token collisions — see `surface/lexer.go`); REPL acceptance (a surface feature is not done until it works in `rune repl` plus a REPL test); human-grade error diagnostics; NO em or en dashes in any code, comment, or doc; Conventional Commits; verify before claiming done. Run the FULL `go test ./...` before tagging any surface change (new keywords can shadow identifiers in listings).
- **Backends unaffected.** The ledger is tooling, not codegen; it adds no backend behavior, so `harness/backend_conformance_test.go` must stay green unchanged.

---

### Task 1: Session exposes assumed-ness

The `ledger/` package lives outside `internal/session` and cannot reach the unexported `s.st`. Add the one missing read accessor so tier classification can ask "is this definition an assumption?".

**Files:**
- Modify: `internal/session/session.go` (add `Assumed` method near `Certified`, ~line 1012)
- Test: `internal/session/assumed_test.go`

**Interfaces:**
- Consumes: `s.refs` (name -> hash map, already used by `Certified`/`Lookup`), `s.st.IsAssumed` (`store/store.go:161`).
- Produces: `func (s *Session) Assumed(name string) bool` — true iff `name` resolves to a bodiless assumed (foreign/postulate) definition.

- [ ] **Step 1: Write the failing test**

```go
// internal/session/assumed_test.go
package session

import "testing"

func TestSessionAssumed(t *testing.T) {
	s := New()
	src := "foreign hostThing : (A : U) -> A -> A end\n" +
		"idU : (A : U) -> A -> A is fn (A : U) (x : A) is x end end"
	if _, err := s.LoadString("assumed_probe.rune", src); err != nil {
		t.Fatalf("load: %v", err)
	}
	if !s.Assumed("hostThing") {
		t.Fatalf("foreign def should be assumed")
	}
	if s.Assumed("idU") {
		t.Fatalf("a defined function must not be assumed")
	}
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestSessionAssumed ./internal/session/`
Expected: FAIL (`s.Assumed` undefined).

- [ ] **Step 3: Implement the accessor**

Add to `internal/session/session.go` immediately after `Certified` (~line 1016):

```go
// Assumed reports whether name resolves to a bodiless assumed definition (a
// foreign axiom or a postulate). It is the read seam the assurance ledger uses
// to classify the assume/postulate tiers.
func (s *Session) Assumed(name string) bool {
	h, ok := s.refs[name]
	if !ok {
		return false
	}
	return s.st.IsAssumed(h)
}
```

(If `LoadString` is not the loader name, use the same one `Certified`'s tests use; check `internal/session/session.go` for `LoadString`/`LoadSource` and match it. The rest of this plan uses `LoadString(name, src)`; if only `LoadSource(src)` exists, drop the first argument throughout.)

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestSessionAssumed ./internal/session/`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add internal/session/session.go internal/session/assumed_test.go
git commit -m "feat(session): expose Assumed(name) for the assurance ledger"
```

---

### Task 2: The `ledger` package — tier classification + proposition/proof hashes

The core of the ledger: classify every definition and capture its proposition + proof identity.

**Files:**
- Create: `ledger/ledger.go`
- Test: `ledger/ledger_test.go`

**Interfaces:**
- Consumes: `session.Session` with `Defs() []session.Def` (`internal/session/session.go:762`; each `Def{Name string, Ty core.Tm, Body core.Tm, Hash core.Hash}`), `s.Assumed(name)` (Task 1), `s.Certified(name)` (session.go:1012); `core.HashTerm(core.Tm) core.Hash` (`core/hash.go:69`); `core.Hash.Short() string` (`core/hash.go:26`).
- Produces:
  - `type Tier int` with `const ( Proven Tier = iota; Guard; Assume; Postulate; Unproven )` and `func (t Tier) String() string`.
  - `type Entry struct { Name string; Tier Tier; PropHash core.Hash; ProofHash core.Hash; Why string; File string; Line int; Provenance Provenance }` (later fields default-zero until their tasks fill them).
  - `type Provenance struct { Author string; Date string; Commit string }`
  - `func Build(s *session.Session) []Entry`

- [ ] **Step 1: Write the failing test**

```go
// ledger/ledger_test.go
package ledger

import (
	"testing"

	"goforge.dev/rune/v3/internal/session"
)

func buildFrom(t *testing.T, src string) []Entry {
	t.Helper()
	s := session.New()
	if _, err := s.LoadString("ledger_probe.rune", src); err != nil {
		t.Fatalf("load: %v", err)
	}
	return Build(s)
}

func find(es []Entry, name string) (Entry, bool) {
	for _, e := range es {
		if e.Name == name {
			return e, true
		}
	}
	return Entry{}, false
}

func TestTierClassification(t *testing.T) {
	src := "foreign hostThing : (A : U) -> A -> A end\n" +
		"idU : (A : U) -> A -> A is fn (A : U) (x : A) is x end end"
	es := buildFrom(t, src)

	host, ok := find(es, "hostThing")
	if !ok || host.Tier != Assume {
		t.Fatalf("hostThing want Assume, got %v ok=%v", host.Tier, ok)
	}
	id, ok := find(es, "idU")
	if !ok || id.Tier != Proven {
		t.Fatalf("idU want Proven, got %v ok=%v", id.Tier, ok)
	}
	if id.PropHash == (host.PropHash) {
		// different types, different proposition hashes
		t.Fatalf("distinct types must have distinct proposition hashes")
	}
	if id.ProofHash.Short() == "" {
		t.Fatalf("a proven def must carry a proof hash")
	}
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test ./ledger/`
Expected: FAIL (package `ledger` does not exist).

- [ ] **Step 3: Implement the package**

```go
// ledger/ledger.go
// Package ledger computes the assurance ledger: a tier (postulate/assume/guard/
// proven) plus a content-hashed proposition identity and proof hash for every
// definition in a session. It is pure read-side tooling over the session and the
// store; it adds no kernel machinery and hashes nothing new (the proposition is
// the type hash, the proof is the body hash, both already computed by core).
package ledger

import (
	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/internal/session"
)

// Tier is the assurance level of a claim, strongest first.
type Tier int

const (
	Proven    Tier = iota // a checked proof term (body present + certified)
	Guard                 // a runtime contract (the with-post-guard sugar)
	Assume                // a trusted foreign/host binding (bodiless)
	Postulate             // an asserted axiom, a debt to be paid down (bodiless)
	Unproven              // has a body but no proof certificate yet
)

func (t Tier) String() string {
	switch t {
	case Proven:
		return "proven"
	case Guard:
		return "guard"
	case Assume:
		return "assume"
	case Postulate:
		return "postulate"
	default:
		return "unproven"
	}
}

// Provenance is the version-control attribution of a claim's witness.
type Provenance struct {
	Author string
	Date   string
	Commit string
}

// Entry is one definition's ledger row.
type Entry struct {
	Name       string
	Tier       Tier
	PropHash   core.Hash // identity of the CLAIM = content hash of the type
	ProofHash  core.Hash // identity of the WITNESS = content hash of the body (zero if bodiless)
	Why        string    // postulate/assume reason (Task 4)
	File       string    // source file (Task 6)
	Line       int       // 1-based source line (Task 6)
	Provenance Provenance
}

// Build classifies every definition in the session.
func Build(s *session.Session) []Entry {
	defs := s.Defs()
	out := make([]Entry, 0, len(defs))
	for _, d := range defs {
		e := Entry{
			Name:     d.Name,
			PropHash: core.HashTerm(d.Ty),
		}
		if d.Body != nil {
			e.ProofHash = core.HashTerm(d.Body)
		}
		e.Tier = classify(s, d)
		out = append(out, e)
	}
	return out
}

// classify picks the tier from the facts available so far. Later tasks extend
// this (postulate vs assume in Task 4, guard in Task 5).
func classify(s *session.Session, d session.Def) Tier {
	if d.Body == nil {
		// bodiless: an assumption (foreign) for now; Task 4 splits out postulate.
		return Assume
	}
	if s.Certified(d.Name) {
		return Proven
	}
	return Unproven
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `go test ./ledger/`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add ledger/ledger.go ledger/ledger_test.go
git commit -m "feat(ledger): tier classification + proposition/proof hashes"
```

---

### Task 3: `rune ledger <file>` CLI + text render

A usable artifact early (Lambert: something that runs). Render the ledger as an aligned text table.

**Files:**
- Create: `ledger/render.go`
- Modify: `cmd/rune/main.go` (add a `ledger` case to the dispatch switch at ~line 34, and a `runLedger` function alongside `runSimulate` ~line 123)
- Test: `ledger/render_test.go`

**Interfaces:**
- Consumes: `[]Entry` (Task 2), `core.Hash.Short()`.
- Produces: `func RenderText(es []Entry, w io.Writer)` — one aligned line per entry: `TIER  NAME  prop:<short>  proof:<short|->`. CLI: `rune ledger FILE` prints it.

- [ ] **Step 1: Write the failing test**

```go
// ledger/render_test.go
package ledger

import (
	"strings"
	"testing"
)

func TestRenderText(t *testing.T) {
	src := "foreign hostThing : (A : U) -> A -> A end\n" +
		"idU : (A : U) -> A -> A is fn (A : U) (x : A) is x end end"
	s := mustSession(t, src)
	var b strings.Builder
	RenderText(Build(s), &b)
	out := b.String()
	if !strings.Contains(out, "assume") || !strings.Contains(out, "hostThing") {
		t.Fatalf("assume row missing:\n%s", out)
	}
	if !strings.Contains(out, "proven") || !strings.Contains(out, "idU") {
		t.Fatalf("proven row missing:\n%s", out)
	}
}
```

Add this helper to `ledger/ledger_test.go` (so both test files share it):

```go
func mustSession(t *testing.T, src string) *session.Session {
	t.Helper()
	s := session.New()
	if _, err := s.LoadString("ledger_probe.rune", src); err != nil {
		t.Fatalf("load: %v", err)
	}
	return s
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test ./ledger/`
Expected: FAIL (`RenderText` undefined).

- [ ] **Step 3: Implement render + wire the CLI**

`ledger/render.go`:

```go
package ledger

import (
	"fmt"
	"io"
)

// RenderText writes an aligned one-line-per-entry table. The tier column is
// fixed-width so the ladder reads at a glance; proof shows "-" for bodiless.
func RenderText(es []Entry, w io.Writer) {
	for _, e := range es {
		proof := "-"
		if e.ProofHash != (core.Hash{}) {
			proof = e.ProofHash.Short()
		}
		why := ""
		if e.Why != "" {
			why = "  why: " + e.Why
		}
		fmt.Fprintf(w, "%-9s %-24s prop:%s  proof:%s%s\n",
			e.Tier.String(), e.Name, e.PropHash.Short(), proof, why)
	}
}
```

Add the `core` import to `ledger/render.go`:

```go
import "goforge.dev/rune/v3/core"
```

(Combine the two import blocks into one; the `core.Hash{}` zero comparison needs the import.)

In `cmd/rune/main.go`, add a case to the switch (after the `"simulate"` case, ~line 105):

```go
	case "ledger":
		if len(os.Args) < 3 {
			fatal(fmt.Errorf("usage: rune ledger <file>"))
		}
		src, err := os.ReadFile(os.Args[2])
		if err != nil {
			fatal(err)
		}
		if err := runLedger(string(src), os.Stdout); err != nil {
			fatal(err)
		}
```

Add `runLedger` alongside `runSimulate` (~line 123). Match the file's existing error/`fatal` and session-load idioms (see `runEmit` at line 257 for the load pattern):

```go
func runLedger(src string, w io.Writer) error {
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		return err
	}
	ledger.RenderText(ledger.Build(s), w)
	return nil
}
```

Add the import `"goforge.dev/rune/v3/ledger"` to `cmd/rune/main.go`. (Use the exact `session.New()` / `LoadSource` calls the neighboring `runEmit` uses; if `runEmit` calls a helper like `loadSession`, reuse it.)

- [ ] **Step 4: Run test to verify it passes**

Run: `go test ./ledger/ && go build ./cmd/rune/`
Expected: PASS, and the binary builds.

- [ ] **Step 5: Commit**

```bash
git add ledger/render.go ledger/render_test.go ledger/ledger_test.go cmd/rune/main.go
git commit -m "feat(ledger): rune ledger <file> text report"
```

---

### Task 4: `postulate ... because "..." end` surface keyword (splits postulate from assume)

A postulate is an asserted claim with a stated reason (a debt), distinct from a trusted host binding (`foreign` = assume). Add a contextual keyword that produces a bodiless assumed definition plus session-side metadata (postulate-ness + reason). The store/core are untouched: the definition is stored exactly like a `foreign` axiom; only the session records that it was written as a postulate and why.

**Files:**
- Modify: `surface/ast.go` (add `IsPostulate bool` and `Why string` to `Def`, ~line 164)
- Modify: `surface/lexer.go` (recognize `postulate` and `because` as CONTEXTUAL keywords, mirroring how `foreign`/`seq` are handled)
- Modify: `surface/parser.go` (add `parsePostulate`, dispatched where `parseForeign` is dispatched; model on `parseForeign` at line 259)
- Modify: `internal/session/session.go` (carry `IsPostulate`/`Why` into a per-name metadata map; expose `Postulate(name) bool` and `Why(name) string`; populate `Def` in `Defs()`)
- Modify: `internal/session/session.go` (`session.Def` gains `Postulate bool`, `Why string`)
- Modify: `ledger/ledger.go` (`classify` returns `Postulate` for postulated defs; `Build` fills `Entry.Why`)
- Test: `surface/parser_postulate_test.go`, `internal/repl/repl_postulate_test.go`, extend `ledger/ledger_test.go`

**Interfaces:**
- Consumes: `parseForeign` shape (`surface/parser.go:259`), the contextual-keyword mechanism in `surface/lexer.go`.
- Produces: surface form `postulate NAME : TYPE because "REASON" end` -> `Def{Name, Ty, IsForeign: true, IsPostulate: true, Why: "REASON"}`. Session: `func (s *Session) Postulate(name string) bool`, `func (s *Session) Why(name string) string`. `session.Def.Postulate bool`, `session.Def.Why string`.

- [ ] **Step 1: Write the failing test**

```go
// surface/parser_postulate_test.go
package surface

import "testing"

func TestParsePostulate(t *testing.T) {
	defs, err := ParseProgram(`postulate inRegion : U because "cloud API not yet modeled" end`)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if len(defs) != 1 {
		t.Fatalf("want 1 def, got %d", len(defs))
	}
	d := defs[0]
	if d.Name != "inRegion" || !d.IsPostulate || !d.IsForeign {
		t.Fatalf("postulate flags wrong: %+v", d)
	}
	if d.Why != "cloud API not yet modeled" {
		t.Fatalf("reason wrong: %q", d.Why)
	}
}
```

(Use the program-parse entry point the other surface tests use; check `surface/` for `ParseProgram`/`ParseItems`/`ParseDef` and match the existing signature. `parser_do_test.go` shows the convention.)

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestParsePostulate ./surface/`
Expected: FAIL (`postulate` parsed as an identifier; `IsPostulate`/`Why` undefined).

- [ ] **Step 3: Implement the surface form**

In `surface/ast.go`, add to the `Def` struct (after `IsForeign bool`, ~line 177):

```go
	IsPostulate bool   // written as `postulate` (an asserted debt), not `foreign`
	Why         string // the postulate's stated reason
```

In `surface/lexer.go`, recognize `postulate` and `because` as CONTEXTUAL keywords exactly as `foreign`/`seq` are handled (a keyword check at identifier position, NOT a new reserved token, so both stay usable as ordinary identifiers elsewhere). Follow the existing contextual-keyword pattern; do not add a reserved token.

In `surface/parser.go`, add `parsePostulate` modeled on `parseForeign` (line 259), dispatched at the same site `parseForeign` is. It parses `postulate NAME : TYPE because "REASON" end`:

```go
// parsePostulate parses `postulate NAME : TYPE because "REASON" end`. Like a
// foreign axiom (bodiless, assumed) but tagged as a postulate with a stated
// reason: the ledger shows it as a debt to be paid down by a later proof of the
// same proposition. The reason is a string literal.
func (p *parser) parsePostulate() (Def, error) {
	// p has just consumed the contextual `postulate`.
	name := p.expectIdent()
	p.expect(tColon)
	ty, err := p.parseExpr()
	if err != nil {
		return Def{}, err
	}
	p.expectKeyword("because")
	why := p.expectStringLit()
	p.expect(tEnd)
	return Def{Name: name, Ty: ty, IsForeign: true, IsPostulate: true, Why: why}, nil
}
```

(Use the exact helper names this parser already has: replace `expectIdent`/`expect(tColon)`/`parseExpr`/`expectKeyword`/`expectStringLit`/`expect(tEnd)` with whatever `parseForeign` and the string-literal path actually call. Read `parseForeign` and a string-literal-consuming parse site first and mirror them precisely.)

In `internal/session/session.go`: add a metadata map and populate it when loading surface defs. Find where `LoadSource` iterates surface `Def`s and registers them (grep for `AddForeign`/`AddDef`). Add:

```go
// def metadata the ledger needs but the store does not hash.
type defMeta struct {
	postulate bool
	why       string
	usesGuard bool // Task 5
	pos       int  // Task 6 (byte offset)
}
```

Add `meta map[string]defMeta` to the `Session` struct, initialize it in `New()`, and set `s.meta[d.Name] = defMeta{postulate: d.IsPostulate, why: d.Why}` at the registration site (merge with later tasks' fields). A postulate registers through the SAME `AddForeign` path a `foreign` does (bodiless + assumed); only the session metadata differs.

Add accessors and extend `Def`/`Defs()`:

```go
func (s *Session) Postulate(name string) bool { return s.meta[name].postulate }
func (s *Session) Why(name string) string     { return s.meta[name].why }
```

Add `Postulate bool` and `Why string` to `session.Def` (the struct at session.go:24), and in `Defs()` set them from `s.meta[d.Name]`.

In `ledger/ledger.go`, refine `classify` and fill `Why`:

```go
func classify(s *session.Session, d session.Def) Tier {
	if d.Body == nil {
		if d.Postulate {
			return Postulate
		}
		return Assume
	}
	if s.Certified(d.Name) {
		return Proven
	}
	return Unproven
}
```

In `Build`, after `e.Tier = classify(...)`, add `e.Why = d.Why`.

- [ ] **Step 4: Run the surface test + add the ledger + REPL tests**

Run: `go test -run TestParsePostulate ./surface/`
Expected: PASS.

Add to `ledger/ledger_test.go`:

```go
func TestPostulateTier(t *testing.T) {
	es := buildFrom(t, `postulate inRegion : U because "cloud API not yet modeled" end`)
	e, ok := find(es, "inRegion")
	if !ok || e.Tier != Postulate {
		t.Fatalf("inRegion want Postulate, got %v ok=%v", e.Tier, ok)
	}
	if e.Why != "cloud API not yet modeled" {
		t.Fatalf("why not carried: %q", e.Why)
	}
}
```

Add the REPL acceptance test (a surface feature is not done until it works in `rune repl`):

```go
// internal/repl/repl_postulate_test.go
package repl

import (
	"bytes"
	"strings"
	"testing"
)

func TestReplAcceptsPostulate(t *testing.T) {
	script := []string{
		`postulate inRegion : U because "not yet modeled" end`,
		`:type inRegion`,
		":quit",
	}
	in := strings.NewReader(strings.Join(script, "\n") + "\n")
	var out bytes.Buffer
	if err := RunWith(in, &out, Config{NoPrelude: true}); err != nil {
		t.Fatalf("RunWith: %v", err)
	}
	got := out.String()
	if !strings.Contains(got, "defined inRegion") && !strings.Contains(got, "inRegion") {
		t.Fatalf("repl did not accept postulate:\n%s", got)
	}
}
```

(If the REPL's declaration path is `runForm`/`looksLikeDecl` in `internal/repl/repl.go`, ensure `postulate` is dispatched there too — mirror how `foreign` is recognized in `looksLikeDecl`. Match the existing repl test helper name/signature from `internal/repl/repl_test.go`.)

Run: `go test ./ledger/ ./surface/ ./internal/repl/`
Expected: PASS.

- [ ] **Step 5: Run the full suite (surface keyword) and commit**

Run: `go test ./...`
Expected: PASS (a new keyword must not shadow an identifier in any listing; if a listing breaks, the keyword was not made contextual correctly).

```bash
git add surface/ast.go surface/lexer.go surface/parser.go internal/session/session.go ledger/ledger.go ledger/ledger_test.go surface/parser_postulate_test.go internal/repl/repl_postulate_test.go
git commit -m "feat(ledger): postulate ... because ... end keyword + Postulate tier"
```

---

### Task 5: Guard tier

A definition built with the `with post ... guard ... blame` contract sugar carries a runtime guard. Flag those at parse and surface the `guard` tier.

**Files:**
- Modify: `surface/ast.go` (add `UsesGuard bool` to `Def`)
- Modify: `surface/parser.go` (`parseGuard` at line 530 sets a parser flag; the enclosing `Def` records `UsesGuard`)
- Modify: `internal/session/session.go` (carry `usesGuard` into `defMeta`; `session.Def` gains `UsesGuard bool`; `Defs()` fills it)
- Modify: `ledger/ledger.go` (`classify`: a body that uses a guard is the `Guard` tier unless it is also `Certified`)
- Test: extend `ledger/ledger_test.go`

**Interfaces:**
- Consumes: `parseGuard` (`surface/parser.go:530`).
- Produces: `session.Def.UsesGuard bool`; `Guard` tier classification.

- [ ] **Step 1: Write the failing test**

```go
// add to ledger/ledger_test.go
func TestGuardTier(t *testing.T) {
	// A definition whose body uses the with-post-guard contract sugar.
	src := `data Bool : U is false : Bool | true : Bool end
data Result : (A : U) -> (E : U) -> U is
  ok  : (A : U) -> (E : U) -> A -> Result A E
| err : (A : U) -> (E : U) -> E -> Result A E
end
data Nat : U is zero : Nat | succ : Nat -> Nat end
foreign fast : Nat end
checked : Result Nat Nat is fast with post r guard true blame zero end`
	es := buildFrom(t, src)
	e, ok := find(es, "checked")
	if !ok || e.Tier != Guard {
		t.Fatalf("checked want Guard, got %v ok=%v", e.Tier, ok)
	}
}
```

(Match the `with post ... guard ... blame` syntax exactly to `parseGuard`/`harness` usage — see `listings/ch440*` for a real example and copy its shape. Adjust the `Result`/`ok`/`err` declaration to the form the guard desugar expects.)

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestGuardTier ./ledger/`
Expected: FAIL (tier is `Unproven` or `Proven`, not `Guard`).

- [ ] **Step 3: Implement guard tracking**

In `surface/ast.go`, add `UsesGuard bool` to `Def`.

In `surface/parser.go`, when `parseGuard` (line 530) fires while parsing a definition body, mark the in-progress definition. The simplest threading: have the definition parser record whether `parseGuard` was invoked for its body (e.g. a `p.guardSeen` bool reset before parsing a body and read after, set inside `parseGuard`). Set `Def.UsesGuard = p.guardSeen`.

In `internal/session/session.go`, extend `defMeta` population: `m.usesGuard = d.UsesGuard` at the registration site; add `UsesGuard bool` to `session.Def`; fill it in `Defs()`.

In `ledger/ledger.go`, refine `classify` (guard outranks unproven, but a fully proven body stays proven):

```go
func classify(s *session.Session, d session.Def) Tier {
	if d.Body == nil {
		if d.Postulate {
			return Postulate
		}
		return Assume
	}
	if s.Certified(d.Name) {
		return Proven
	}
	if d.UsesGuard {
		return Guard
	}
	return Unproven
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `go test ./ledger/ && go test ./surface/`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add surface/ast.go surface/parser.go internal/session/session.go ledger/ledger.go ledger/ledger_test.go
git commit -m "feat(ledger): guard tier from the with-post-guard sugar"
```

---

### Task 6: Source provenance (position + git blame)

Attribute each witness to its committer via `git blame` on the definition's source line.

**Files:**
- Modify: `surface/ast.go` (add `Pos int` to `Def` — byte offset of the name token)
- Modify: `surface/parser.go` (capture the name token's `pos` into `Def.Pos` for every definition form: regular, foreign, postulate)
- Modify: `internal/session/session.go` (carry `pos` into `defMeta`; `session.Def` gains `Pos int`; the loader records the source FILE name; expose `DefFile() string`)
- Create: `ledger/provenance.go`
- Test: `ledger/provenance_test.go`

**Interfaces:**
- Consumes: token `pos` (`surface/lexer.go:60`), the source text + filename available at `LoadString(name, src)`.
- Produces: `session.Def.Pos int`; a 1-based line via byte offset; `func GitBlame(file string, line int) (Provenance, error)`; `Build` fills `Entry.File/Line/Provenance` when a file is known.

- [ ] **Step 1: Write the failing test**

```go
// ledger/provenance_test.go
package ledger

import (
	"os/exec"
	"strings"
	"testing"
)

// lineOf computes the 1-based line of a byte offset (the helper Build uses).
func TestLineOf(t *testing.T) {
	src := "a\nbb\nccc"
	if got := lineOf(src, 0); got != 1 {
		t.Fatalf("offset 0 want line 1, got %d", got)
	}
	if got := lineOf(src, 2); got != 2 {
		t.Fatalf("offset 2 want line 2, got %d", got)
	}
	if got := lineOf(src, 5); got != 3 {
		t.Fatalf("offset 5 want line 3, got %d", got)
	}
}

func TestGitBlameDegradesGracefully(t *testing.T) {
	if _, err := exec.LookPath("git"); err != nil {
		t.Skip("git not in PATH")
	}
	// A path git cannot blame yields a zero Provenance and an error, never a panic.
	p, err := GitBlame("/nonexistent/file.rune", 1)
	if err == nil {
		t.Fatalf("expected an error blaming a nonexistent file")
	}
	if p.Author != "" || p.Commit != "" {
		t.Fatalf("failed blame must be zero-valued, got %+v", strings.TrimSpace(p.Author))
	}
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test ./ledger/`
Expected: FAIL (`lineOf`, `GitBlame` undefined).

- [ ] **Step 3: Implement provenance**

In `surface/ast.go` add `Pos int` to `Def`. In `surface/parser.go`, set `Pos: id.pos` (the name token's byte offset) in EVERY def-returning path (the regular def parser at ~line 465-487, `parseForeign`, and `parsePostulate`). Capture the token before consuming it.

In `internal/session/session.go`: add `pos` to `defMeta` (set from `d.Pos`); add a `srcFile string` field on the session set by `LoadString(name, src)` (the `name` argument) and a getter `func (s *Session) DefFile() string { return s.srcFile }`; add `Pos int` to `session.Def` and fill it in `Defs()`. (If only `LoadSource(src)` exists with no filename, add an optional `LoadFile(path)` that reads the file and records its path; `runLedger` will use it.)

`ledger/provenance.go`:

```go
package ledger

import (
	"fmt"
	"os/exec"
	"strings"
)

// lineOf returns the 1-based line number of byte offset off in src.
func lineOf(src string, off int) int {
	if off > len(src) {
		off = len(src)
	}
	return 1 + strings.Count(src[:off], "\n")
}

// GitBlame attributes file:line to its last committer via porcelain blame. On
// any failure (not a repo, untracked, no git) it returns a zero Provenance and
// the error; the caller treats that as "unattributed", never fatal.
func GitBlame(file string, line int) (Provenance, error) {
	cmd := exec.Command("git", "blame", "-L",
		fmt.Sprintf("%d,%d", line, line), "--porcelain", "--", file)
	out, err := cmd.Output()
	if err != nil {
		return Provenance{}, err
	}
	var p Provenance
	for _, ln := range strings.Split(string(out), "\n") {
		switch {
		case strings.HasPrefix(ln, "author "):
			p.Author = strings.TrimPrefix(ln, "author ")
		case strings.HasPrefix(ln, "author-time "):
			p.Date = strings.TrimPrefix(ln, "author-time ")
		case p.Commit == "" && len(ln) >= 40 && !strings.Contains(ln, " ") == false:
			// the first porcelain line begins with the 40-char commit sha
		}
	}
	if fields := strings.Fields(string(out)); len(fields) > 0 && len(fields[0]) >= 7 {
		p.Commit = fields[0]
	}
	return p, nil
}
```

(The porcelain parse above is intentionally minimal: the commit sha is the first whitespace-delimited token of the first line; `author`/`author-time` lines follow. Verify against `git blame --porcelain` output on this repo and tighten the field extraction if needed; keep the graceful-error contract.)

In `ledger/ledger.go`, add an overload that knows the file+source so it can fill provenance. Keep `Build(s)` (no provenance) and add:

```go
// BuildWithSource is Build plus git-blame provenance, using the session's source
// file and text. Definitions whose blame fails are left unattributed.
func BuildWithSource(s *session.Session, file, src string) []Entry {
	es := Build(s)
	defs := s.Defs()
	byName := map[string]session.Def{}
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
```

Update `runLedger` in `cmd/rune/main.go` to read the file path, pass it through, and call `BuildWithSource`:

```go
func runLedger(path string, w io.Writer) error {
	src, err := os.ReadFile(path)
	if err != nil {
		return err
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		return err
	}
	ledger.RenderText(ledger.BuildWithSource(s, path, string(src)), w)
	return nil
}
```

And change the `case "ledger"` dispatch to pass `os.Args[2]` (the path) instead of pre-reading the source.

Extend `RenderText` to append provenance when present: in the format string add `  by:<author>` when `e.Provenance.Author != ""`.

- [ ] **Step 4: Run test to verify it passes**

Run: `go test ./ledger/ ./surface/ && go build ./cmd/rune/`
Expected: PASS, binary builds.

- [ ] **Step 5: Commit**

```bash
git add surface/ast.go surface/parser.go internal/session/session.go ledger/provenance.go ledger/provenance_test.go ledger/ledger.go ledger/render.go cmd/rune/main.go
git commit -m "feat(ledger): source position + git-blame provenance"
```

---

### Task 7: Upgrade detector

A postulate->proven transition is the same proposition hash gaining a proof. Detect it by comparing the current ledger against a saved baseline ledger.

**Files:**
- Create: `ledger/upgrade.go`
- Test: `ledger/upgrade_test.go`

**Interfaces:**
- Consumes: two `[]Entry` slices (baseline, current).
- Produces:
  - `type Upgrade struct { Name string; PropHash core.Hash; From Tier; To Tier }`
  - `func Upgrades(baseline, current []Entry) []Upgrade` — for each proposition hash present in both, report a tier change toward a stronger tier (lower `Tier` int = stronger; `Proven` < `Guard` < `Assume` < `Postulate`).

- [ ] **Step 1: Write the failing test**

```go
// ledger/upgrade_test.go
package ledger

import "testing"

func TestUpgradesDetectsPostulateToProven(t *testing.T) {
	// same proposition hash, postulate -> proven
	prop := mkHash(1)
	base := []Entry{{Name: "inRegion", Tier: Postulate, PropHash: prop}}
	cur := []Entry{{Name: "inRegion", Tier: Proven, PropHash: prop, ProofHash: mkHash(2)}}
	ups := Upgrades(base, cur)
	if len(ups) != 1 {
		t.Fatalf("want 1 upgrade, got %d", len(ups))
	}
	if ups[0].From != Postulate || ups[0].To != Proven {
		t.Fatalf("want postulate->proven, got %v->%v", ups[0].From, ups[0].To)
	}
}

func TestUpgradesIgnoresUnchanged(t *testing.T) {
	prop := mkHash(1)
	base := []Entry{{Name: "x", Tier: Proven, PropHash: prop}}
	cur := []Entry{{Name: "x", Tier: Proven, PropHash: prop}}
	if ups := Upgrades(base, cur); len(ups) != 0 {
		t.Fatalf("unchanged tier must not be an upgrade, got %d", len(ups))
	}
}
```

Add `mkHash` to `ledger/ledger_test.go`:

```go
import "goforge.dev/rune/v3/core"

func mkHash(b byte) core.Hash {
	var h core.Hash
	h[0] = b
	return h
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test ./ledger/`
Expected: FAIL (`Upgrades`, `Upgrade` undefined).

- [ ] **Step 3: Implement the detector**

```go
// ledger/upgrade.go
package ledger

import "goforge.dev/rune/v3/core"

// Upgrade is a strengthening of a claim's witness across two ledgers: the same
// proposition hash moving to a stronger tier (e.g. postulate -> proven).
type Upgrade struct {
	Name     string
	PropHash core.Hash
	From     Tier
	To       Tier
}

// Upgrades reports, per proposition hash present in both ledgers, a move toward a
// stronger tier (smaller Tier value). Downgrades are reported by gate.go, not here.
func Upgrades(baseline, current []Entry) []Upgrade {
	base := map[core.Hash]Entry{}
	for _, e := range baseline {
		base[e.PropHash] = e
	}
	var ups []Upgrade
	for _, e := range current {
		b, ok := base[e.PropHash]
		if !ok {
			continue
		}
		if e.Tier < b.Tier { // smaller = stronger
			ups = append(ups, Upgrade{Name: e.Name, PropHash: e.PropHash, From: b.Tier, To: e.Tier})
		}
	}
	return ups
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `go test ./ledger/`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add ledger/upgrade.go ledger/upgrade_test.go ledger/ledger_test.go
git commit -m "feat(ledger): postulate->proven upgrade detector"
```

---

### Task 8: CI gate

Fail the build on a new postulate that is not signed off, and on a named flagship control that has left `proven`. Render anomalies as `elaborate.Diagnostic`.

**Files:**
- Create: `ledger/gate.go`
- Test: `ledger/gate_test.go`

**Interfaces:**
- Consumes: current `[]Entry`, an optional baseline `[]Entry`, and a `GateConfig`.
- Produces:
  - `type GateConfig struct { Flagships []string; AllowedPostulates []string }`
  - `func Gate(current, baseline []Entry, cfg GateConfig) []error` — one `*elaborate.Diagnostic` per violation: (a) a `Postulate`-tier entry whose name is not in `AllowedPostulates`; (b) a `Flagships` name whose current tier is not `Proven`. Empty slice = pass.

- [ ] **Step 1: Write the failing test**

```go
// ledger/gate_test.go
package ledger

import "testing"

func TestGateRejectsUnapprovedPostulate(t *testing.T) {
	cur := []Entry{{Name: "inRegion", Tier: Postulate, PropHash: mkHash(1)}}
	errs := Gate(cur, nil, GateConfig{})
	if len(errs) != 1 {
		t.Fatalf("an unapproved postulate must fail the gate, got %d errors", len(errs))
	}
}

func TestGateAllowsApprovedPostulate(t *testing.T) {
	cur := []Entry{{Name: "inRegion", Tier: Postulate, PropHash: mkHash(1)}}
	errs := Gate(cur, nil, GateConfig{AllowedPostulates: []string{"inRegion"}})
	if len(errs) != 0 {
		t.Fatalf("an approved postulate must pass, got %v", errs)
	}
}

func TestGateRejectsFlagshipLeavingProven(t *testing.T) {
	cur := []Entry{{Name: "iamLeastPriv", Tier: Guard, PropHash: mkHash(2)}}
	errs := Gate(cur, nil, GateConfig{Flagships: []string{"iamLeastPriv"}})
	if len(errs) != 1 {
		t.Fatalf("a flagship not proven must fail, got %d", len(errs))
	}
}

func TestGatePassesProvenFlagship(t *testing.T) {
	cur := []Entry{{Name: "iamLeastPriv", Tier: Proven, PropHash: mkHash(2)}}
	errs := Gate(cur, nil, GateConfig{Flagships: []string{"iamLeastPriv"}})
	if len(errs) != 0 {
		t.Fatalf("a proven flagship must pass, got %v", errs)
	}
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test ./ledger/`
Expected: FAIL (`Gate`, `GateConfig` undefined).

- [ ] **Step 3: Implement the gate**

```go
// ledger/gate.go
package ledger

import "goforge.dev/rune/v3/elaborate"

// GateConfig is the CI policy: which controls must stay proven, and which
// postulates are signed off (allowed to remain a debt for now).
type GateConfig struct {
	Flagships         []string // must be Proven or the build fails
	AllowedPostulates []string // postulates that are signed off
}

func contains(xs []string, x string) bool {
	for _, s := range xs {
		if s == x {
			return true
		}
	}
	return false
}

// Gate returns one diagnostic per policy violation. Empty = pass. The baseline
// is accepted for future "no NEW postulate" deltas; the current policy gates on
// the present ledger (an unapproved postulate, a flagship off proven).
func Gate(current, baseline []Entry, cfg GateConfig) []error {
	var errs []error
	for _, e := range current {
		if e.Tier == Postulate && !contains(cfg.AllowedPostulates, e.Name) {
			errs = append(errs, &elaborate.Diagnostic{
				Summary: "Unapproved postulate in the assurance ledger.",
				Body: []string{
					"The control " + e.Name + " (proposition " + e.PropHash.Short() +
						") is a postulate: " + e.Why,
					"A postulate is an unproven debt. CI requires sign-off before a new one lands.",
				},
				Hints: []string{
					"Prove it (replace the postulate with a proof of the same proposition), " +
						"or add " + e.Name + " to the gate's AllowedPostulates with a reviewer sign-off.",
				},
			})
		}
		if contains(cfg.Flagships, e.Name) && e.Tier != Proven {
			errs = append(errs, &elaborate.Diagnostic{
				Summary: "Flagship control is not proven.",
				Body: []string{
					"The flagship control " + e.Name + " is at tier " + e.Tier.String() +
						", but flagship controls must stay proven.",
				},
				Hints: []string{
					"Restore the proof for " + e.Name + ", or remove it from the flagship set " +
						"if it is intentionally being demoted (a reviewed decision).",
				},
			})
		}
	}
	return errs
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `go test ./ledger/`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add ledger/gate.go ledger/gate_test.go
git commit -m "feat(ledger): CI gate for postulates and flagship controls"
```

---

### Task 9: JSON output + CLI flags (`--json`, `--check`, `--upgrades`)

Machine-readable output for CALM (Plan 4) and CI, plus the gate/upgrade flags on the CLI.

**Files:**
- Create: `ledger/json.go`
- Modify: `cmd/rune/main.go` (`runLedger` grows `--json`, `--check`, `--baseline <file>` flags)
- Test: `ledger/json_test.go`

**Interfaces:**
- Consumes: `[]Entry`, `[]Upgrade`.
- Produces: `func RenderJSON(es []Entry, w io.Writer) error` — a stable JSON array of `{name, tier, proposition, proof, why, file, line, author, date, commit}` with hashes as hex strings. CLI: `rune ledger FILE [--json] [--check] [--baseline B.json]`.

- [ ] **Step 1: Write the failing test**

```go
// ledger/json_test.go
package ledger

import (
	"encoding/json"
	"strings"
	"testing"
)

func TestRenderJSON(t *testing.T) {
	es := []Entry{{Name: "inRegion", Tier: Postulate, PropHash: mkHash(1), Why: "not modeled"}}
	var b strings.Builder
	if err := RenderJSON(es, &b); err != nil {
		t.Fatalf("render: %v", err)
	}
	var rows []map[string]any
	if err := json.Unmarshal([]byte(b.String()), &rows); err != nil {
		t.Fatalf("invalid JSON: %v\n%s", err, b.String())
	}
	if len(rows) != 1 || rows[0]["tier"] != "postulate" || rows[0]["name"] != "inRegion" {
		t.Fatalf("bad row: %v", rows)
	}
	if rows[0]["proposition"] == "" {
		t.Fatalf("proposition hash must be a hex string")
	}
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test ./ledger/`
Expected: FAIL (`RenderJSON` undefined).

- [ ] **Step 3: Implement JSON + wire flags**

```go
// ledger/json.go
package ledger

import (
	"encoding/json"
	"io"
)

type jsonRow struct {
	Name        string `json:"name"`
	Tier        string `json:"tier"`
	Proposition string `json:"proposition"`
	Proof       string `json:"proof,omitempty"`
	Why         string `json:"why,omitempty"`
	File        string `json:"file,omitempty"`
	Line        int    `json:"line,omitempty"`
	Author      string `json:"author,omitempty"`
	Date        string `json:"date,omitempty"`
	Commit      string `json:"commit,omitempty"`
}

// RenderJSON writes the ledger as a stable JSON array (hashes as hex). This is
// the form CALM emit (Plan 4) and CI consume.
func RenderJSON(es []Entry, w io.Writer) error {
	rows := make([]jsonRow, 0, len(es))
	for _, e := range es {
		r := jsonRow{
			Name:        e.Name,
			Tier:        e.Tier.String(),
			Proposition: e.PropHash.Short(),
			Why:         e.Why,
			File:        e.File,
			Line:        e.Line,
			Author:      e.Provenance.Author,
			Date:        e.Provenance.Date,
			Commit:      e.Provenance.Commit,
		}
		if e.ProofHash != (core.Hash{}) {
			r.Proof = e.ProofHash.Short()
		}
		rows = append(rows, r)
	}
	enc := json.NewEncoder(w)
	enc.SetIndent("", "  ")
	return enc.Encode(rows)
}
```

Add the `core` import to `ledger/json.go` (`goforge.dev/rune/v3/core`) for the zero-hash comparison.

In `cmd/rune/main.go`, extend `runLedger` to parse flags from `os.Args[2:]` (file is the first non-flag arg; support `--json`, `--check`, `--baseline <path>`). On `--check`, load the baseline JSON if given, build the current ledger, run `Gate` (and report `Upgrades` for information), print any diagnostics to stderr, and exit non-zero if there are violations:

```go
func runLedger(args []string, w io.Writer) error {
	var (
		file     string
		asJSON   bool
		check    bool
		baseline string
	)
	for i := 0; i < len(args); i++ {
		switch args[i] {
		case "--json":
			asJSON = true
		case "--check":
			check = true
		case "--baseline":
			i++
			if i < len(args) {
				baseline = args[i]
			}
		default:
			file = args[i]
		}
	}
	if file == "" {
		return fmt.Errorf("usage: rune ledger <file> [--json] [--check] [--baseline <ledger.json>]")
	}
	src, err := os.ReadFile(file)
	if err != nil {
		return err
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		return err
	}
	es := ledger.BuildWithSource(s, file, string(src))

	if check {
		violations := ledger.Gate(es, nil, ledger.GateConfig{})
		for _, v := range violations {
			fmt.Fprintln(os.Stderr, v.Error())
		}
		if len(violations) > 0 {
			return fmt.Errorf("assurance gate failed: %d violation(s)", len(violations))
		}
		return nil
	}
	if asJSON {
		return ledger.RenderJSON(es, w)
	}
	ledger.RenderText(es, w)
	_ = baseline // reserved for --baseline upgrade reporting
	return nil
}
```

Update the `case "ledger"` dispatch to `runLedger(os.Args[2:], os.Stdout)`.

- [ ] **Step 4: Run test to verify it passes**

Run: `go test ./ledger/ && go build ./cmd/rune/`
Expected: PASS, binary builds.

- [ ] **Step 5: Commit**

```bash
git add ledger/json.go ledger/json_test.go cmd/rune/main.go
git commit -m "feat(ledger): JSON output + --json/--check/--baseline CLI flags"
```

---

## Self-Review

**Spec coverage (against `2026-06-25-wavelet-beta-design.md` Section 1 + Plan 2 in `00-INDEX.md`):**
- Tiers `postulate`/`assume`/`guard`/`proven` — Tasks 2 (assume/proven), 4 (postulate), 5 (guard). Covered.
- Content-hashed proposition identity — Task 2 (`PropHash = HashTerm(Ty)`). Covered.
- Proof hash when proven — Task 2 (`ProofHash = HashTerm(Body)`). Covered.
- Honest about limitations (tier in the artifact + the "why") — Tasks 4 (`Why`), 3/9 (render text/JSON show tier + why). Covered.
- Attributable via version control (`git blame`) — Task 6. Covered.
- Upgradeable + verifiable (same proposition-hash, postulate->proven) — Task 7. Covered.
- CI gate (no new postulate without sign-off; flagship stays proven) — Task 8 + Task 9 (`--check`). Covered.
- Kernel sacred (postulate is a bodiless content-addressed def; provenance is git tooling; upgrade detector is content-addressing) — every task keeps `core`/`store` hashing untouched; metadata is session-only. Covered.
- `rune assumptions` / `check --safe` substrate — partially reused: assumed-ness via Task 1; `--check` is the gate. (The legacy `check --safe` flag is not extended; the ledger gate supersedes it for control claims. Note for the author: if `rune check --safe` should also call the gate, that is a one-line wire-up in a follow-up.)

**Placeholder scan:** The parser helper names in Tasks 4-6 (`expectIdent`, `expectStringLit`, `id.pos`, the guard-seen flag) are "match the existing symbol" instructions with the exact file:line to mirror (`parseForeign` line 259, `parseGuard` line 530), not placeholders — the real names must be read from those functions because this plan cannot guess the parser's private helper names. Every Go step shows real, compilable code against confirmed public APIs (`session.Defs`, `session.Certified`, `core.HashTerm`, `core.Hash.Short`, `elaborate.Diagnostic`).

**Type consistency:** `Entry`, `Tier`, `Provenance`, `Upgrade`, `GateConfig` are defined in Task 2/6/7/8 and used consistently. `Tier` ordering (Proven=0 strongest .. Unproven=4) is relied on by `Upgrades` (smaller = stronger) and `Gate`. `core.Hash` zero-comparison (`!= core.Hash{}`) is used in render/json for "has proof". `session.Def` field additions (`Postulate`, `Why`, `UsesGuard`, `Pos`) are introduced in Tasks 4/5/6 and read by `ledger` only through `s.Defs()`.

**One scope note:** the gate's "no NEW postulate" delta (comparing against a committed baseline) is wired as `--baseline` plumbing in Task 9 but the delta comparison itself is left as the reserved hook (the present-tense gate in Task 8 already fails on ANY unapproved postulate, which is stricter and sufficient for v0.1). Tightening to "new since baseline" is a small follow-up if the author wants postulates grandfathered rather than allowlisted.

## Execution Handoff

Plan complete and saved to `docs/superpowers/plans/2026-06-26-assurance-ledger.md`. Two execution options:

1. **Subagent-Driven (recommended)** — I dispatch a fresh subagent per task, review between tasks, fast iteration.
2. **Inline Execution** — Execute tasks in this session using executing-plans, batch execution with checkpoints.

Which approach?
