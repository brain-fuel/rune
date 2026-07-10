# Ord Plan C — Proof-Gated Lowering Registry Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the compiler a proof-gated lowering registry so a proof-shaped def (e.g. the O(n) recursive `leb`) is redirected at codegen to its already-native speed-shaped twin (`leW`, riding the `natMonus` accel), making comparison host-speed on all 9 backends with the proofs left untouched.

**Architecture:** A new surface directive `lower SLOW to FAST by PROOF`. The session registers `(slow-hash → fast-hash)` in a table ONLY after kernel-checking that `PROOF` literally proves `(x…) -> Eq _ (SLOW x…)(FAST x…)`. The single shared `Erase` choke point (codegen/ir.go) redirects a top-level reference through the table before naming, so all 9 backends inherit the rewrite with zero per-backend code. Tree-shaking runs after Erase, so the fast twin is naturally retained and the now-unreferenced slow def is dropped. Nothing in core/store/elaborate changes; the prelude proofs stay stated over `leb`.

**Tech Stack:** Go (surface parser/AST, internal/session, codegen/ir.go), the rune prelude (internal/prelude/prelude.rune), the listings corpus + harness gates.

## Global Constraints

- KERNEL FROZEN: no edits to core/, store/, or elaborate/ that change type theory, hashing, or conversion. `core.Eq`/`core.Pi`/`core.App`/`core.Var`/`core.Ref` are READ (pattern-matched) here, never extended. No hash-format bump (no new core constructor).
- Mutate the shadow, not the source (Rule 4): the redirect lives in codegen (Erase) + session registry, never in the immutable core/store. Proofs stay stated over `leb`; `ordWhole` keeps `le = leb`.
- Scope capped by demonstrated need (Rule 1): the ONLY registry entry this plan adds is `lower leb to leW by lebEquiv`. Build no second directive form, no CLI surface, no measurement harness beyond the gates named here.
- NO em-dashes or en-dashes anywhere (code, comments, docs, commit messages). Use hyphens or rewrite.
- Conventional Commits; every commit message ends with a trailer line `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>` (faithful to the actual authoring model).
- `lower` is a CONTEXTUAL keyword (never a reserved lexer token) so it cannot shadow a user identifier named `lower`. See the surface-keyword-collision discipline.
- Full `go test -timeout 30m ./...` green before the tag. A rune feature is not done until it works in `rune repl` (REPL acceptance is a mandatory task).
- Commit with explicit pathspecs only; work in a dedicated git worktree off `main` @ v3.379.0.

---

### Task 1: Surface directive `lower SLOW to FAST by PROOF`

**Files:**
- Modify: `surface/ast.go` (add `LowerDef` Item + `isItem`)
- Modify: `surface/parser.go` (contextual `lower` parse in `parseItem`; qualification in the resolve pass)
- Modify: `surface/pretty.go` (pretty-print `LowerDef` so `parse ∘ pretty = id` holds)
- Test: `surface/parser_test.go`

**Interfaces:**
- Produces: `surface.LowerDef{Slow, Fast, Proof string}` (three resolved names), implementing `surface.Item`. Consumed by session Task 2.

- [ ] **Step 1: Write the failing test**

Add to `surface/parser_test.go`:

```go
func TestParseLowerDirective(t *testing.T) {
	src := "lower leb to leW by lebEquiv\n"
	prog, err := ParseProgram(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if len(prog) != 1 {
		t.Fatalf("want 1 item, got %d", len(prog))
	}
	ld, ok := prog[0].(LowerDef)
	if !ok {
		t.Fatalf("want LowerDef, got %T", prog[0])
	}
	if ld.Slow != "leb" || ld.Fast != "leW" || ld.Proof != "lebEquiv" {
		t.Fatalf("got %+v", ld)
	}
}

// `lower` must remain usable as an ordinary identifier (contextual keyword).
func TestLowerNotReserved(t *testing.T) {
	src := "lower : Whole -> Whole is fn (x : Whole) is x end end\n"
	prog, err := ParseProgram(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	d, ok := prog[0].(Def)
	if !ok || d.Name != "lower" {
		t.Fatalf("want Def named lower, got %T %+v", prog[0], prog[0])
	}
}

// Round-trip: pretty then re-parse yields the same LowerDef.
func TestLowerPrettyRoundTrip(t *testing.T) {
	orig := LowerDef{Slow: "leb", Fast: "leW", Proof: "lebEquiv"}
	out := PrettyItem(orig) // if no PrettyItem exists, pretty-print the one-item program
	prog, err := ParseProgram(out + "\n")
	if err != nil {
		t.Fatalf("reparse %q: %v", out, err)
	}
	if got, ok := prog[0].(LowerDef); !ok || got != orig {
		t.Fatalf("round-trip got %T %+v from %q", prog[0], prog[0], out)
	}
}
```

If the surface package exposes program pretty-printing under a different name than `PrettyItem`, use the existing entry point (grep `func Pretty` in surface/pretty.go) and adapt the assertion; the requirement is that a pretty-printed `LowerDef` re-parses to an equal `LowerDef`.

- [ ] **Step 2: Run to verify it fails**

Run: `go test ./surface/ -run 'TestParseLower|TestLowerNot|TestLowerPretty' -v`
Expected: FAIL (LowerDef undefined).

- [ ] **Step 3: Add the AST node**

In `surface/ast.go`, next to `BuiltinNumInj`:

```go
// LowerDef is a proof-gated lowering directive (v4 Ord Plan C):
//
//	lower leb to leW by lebEquiv
//
// It tags the SLOW def's content hash to be redirected to the FAST def's hash at
// codegen (the shared Erase choke point), so emitted code calls the O(1) native
// twin while the proofs stay stated over the slow def. Registration is accepted
// ONLY if PROOF kernel-checks at `(x...) -> Eq _ (SLOW x...)(FAST x...)`, so the
// redirect cannot miscompile. `lower` is a CONTEXTUAL keyword (still a valid
// identifier). Session state only; nothing enters the store.
type LowerDef struct {
	Slow  string
	Fast  string
	Proof string
}
```

And with the other `isItem` methods:

```go
func (LowerDef) isItem() {}
```

- [ ] **Step 4: Parse it contextually**

In `surface/parser.go`, in `parseItem`, the default (identifier-led) branch currently falls through to `parseDef`. Add a contextual check BEFORE that. The disambiguation: a `LowerDef` is `lower IDENT to IDENT by IDENT`; a def named `lower` is `lower :` or `lower <param>` or `lower is`. So peek two tokens: leading ident text `lower` AND the token after the next ident is the ident `to`.

Locate the `default:` arm of `parseItem` (the one that calls the def parser). Replace it with:

```go
	default:
		if p.peek().kind == tIdent && p.peek().text == "lower" && p.looksLikeLower() {
			return p.parseLower()
		}
		// ... existing def-parsing fallthrough unchanged ...
```

Add the helper + parser (place near `parseBuiltin`):

```go
// looksLikeLower reports whether the upcoming tokens are `lower IDENT to ...`
// (the LowerDef shape) rather than a definition named `lower`. It peeks without
// consuming: positions 0='lower', 1=slow ident, 2 must be the ident `to`.
func (p *parser) looksLikeLower() bool {
	return p.peekAt(1).kind == tIdent &&
		p.peekAt(2).kind == tIdent && p.peekAt(2).text == "to"
}

// parseLower parses `lower SLOW to FAST by PROOF` (v4 Ord Plan C). `to` and `by`
// are contextual idents, matched by text (like `builtin`'s kind word).
func (p *parser) parseLower() (Item, error) {
	p.next() // 'lower' (an ident, matched contextually)
	slow, err := p.expect(tIdent)
	if err != nil {
		return nil, err
	}
	if kw, err := p.expect(tIdent); err != nil {
		return nil, err
	} else if kw.text != "to" {
		return nil, fmt.Errorf("lower %s: expected `to`, got %q at offset %d", slow.text, kw.text, kw.pos)
	}
	fast, err := p.expect(tIdent)
	if err != nil {
		return nil, err
	}
	if kw, err := p.expect(tIdent); err != nil {
		return nil, err
	} else if kw.text != "by" {
		return nil, fmt.Errorf("lower %s to %s: expected `by`, got %q at offset %d", slow.text, fast.text, kw.text, kw.pos)
	}
	proof, err := p.expect(tIdent)
	if err != nil {
		return nil, err
	}
	return LowerDef{Slow: slow.text, Fast: fast.text, Proof: proof.text}, nil
}
```

If `p.peekAt(n)` does not exist, use the parser's existing multi-token lookahead helper (grep `func (p *parser) peek` in surface/parser.go); if only single `peek` exists, add a minimal `peekAt(n int) token` that indexes the token buffer without consuming, mirroring `peek`.

- [ ] **Step 5: Qualify names in the resolve pass + pretty-print**

In `surface/parser.go`, the module-qualification switch (the one with `case BuiltinNatOp:` calling `qualLocal`) add:

```go
		case LowerDef:
			d.Slow = qualLocal(d.Slow)
			d.Fast = qualLocal(d.Fast)
			d.Proof = qualLocal(d.Proof)
			out = append(out, d)
```

In `surface/pretty.go`, wherever items are rendered (grep the switch that handles `BuiltinNatOp`), add:

```go
	case LowerDef:
		return "lower " + it.Slow + " to " + it.Fast + " by " + it.Proof
```

Match the surrounding function's signature/return style exactly (some pretty functions write to a builder rather than returning a string; adapt accordingly).

- [ ] **Step 6: Run to verify pass**

Run: `go test ./surface/ -run 'TestParseLower|TestLowerNot|TestLowerPretty' -v`
Expected: PASS.

- [ ] **Step 7: Full surface package + fmt idempotence**

Run: `go test ./surface/...`
Expected: PASS (the existing `parse ∘ pretty = id` harness must stay green with the new item).

- [ ] **Step 8: Commit**

```bash
git add surface/ast.go surface/parser.go surface/pretty.go surface/parser_test.go
git commit -m "feat(surface): lower directive (contextual keyword) for the lowering registry"
```

---

### Task 2: Session lowering registry + proof gate

**Files:**
- Modify: `internal/session/session.go` (add `lower` table field; `AddLowering`; LoadSource dispatch; Reset/resetBuiltins clearing; a `Lowering()` accessor for codegen)
- Test: `internal/session/lowering_test.go` (create)

**Interfaces:**
- Consumes: `surface.LowerDef` (Task 1).
- Produces: `s.lower map[core.Hash]core.Hash` and `func (s *Session) Lowering() map[core.Hash]core.Hash` (returns the table; nil-safe). Consumed by codegen Task 3.

- [ ] **Step 1: Write the failing test**

Create `internal/session/lowering_test.go`. It loads a minimal self-contained program (a builtin-nat, a slow recursive `sleb`, a fast `sleW` via a monus, and a proof), registers the lowering, and checks acceptance + three rejections. Use the SAME `builtin nat` + `builtin natMonus` scaffold the existing accel tests use (grep `nataccel_test.go` for the exact preamble and reuse it verbatim).

```go
package session

import (
	"strings"
	"testing"
)

// A minimal program with a slow recursive le and a fast monus-based twin, plus a
// proof of their pointwise equality. Mirrors the prelude leb/leW/lebEquiv shape
// at listing scale. `PRELUDE_NAT` is the builtin-nat + natMonus preamble copied
// from nataccel_test.go (Whole, zero, succ, subW/monus, isZero).
const lowerProg = PRELUDE_NAT + `
sleb : Whole -> Whole -> Bool is
  fn (m : Whole) is
    case m of
    | zero -> fn (n : Whole) is true end
    | succ k with ih -> fn (n : Whole) is
        case n of | zero -> false | succ j -> ih j end end
    end end end
sleW : Whole -> Whole -> Bool is fn (a : Whole)(b : Whole) is isZero (monus a b) end end
`

// slebEquiv proves (a b) -> Eq Bool (sleb a b)(sleW a b). Provide its full body
// in the test fixture (induction on a, then b), or if that is heavy, POSTULATE it
// via `foreign slebEquiv : (a : Whole) -> (b : Whole) -> Eq Bool (sleb a b)(sleW a b)`
// -- the gate checks the TYPE, so a foreign axiom of the right type exercises the
// acceptance path, and a foreign axiom of a WRONG type exercises rejection.
func TestAddLoweringAcceptsWellTypedProof(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(lowerProg +
		"foreign slebEquiv : (a : Whole) -> (b : Whole) -> Eq Bool (sleb a b) (sleW a b)\n" +
		"lower sleb to sleW by slebEquiv\n"); err != nil {
		t.Fatalf("expected acceptance, got %v", err)
	}
	if s.Lowering() == nil || len(s.Lowering()) != 1 {
		t.Fatalf("registry not populated: %v", s.Lowering())
	}
}

func TestAddLoweringRejectsWrongProof(t *testing.T) {
	cases := map[string]string{
		// codomain is not an Eq between sleb and sleW (constant refl of true).
		"not-an-eq-of-the-pair": "foreign bad : (a : Whole) -> (b : Whole) -> Eq Bool (sleW a b) (sleW a b)\nlower sleb to sleW by bad\n",
		// wrong arity (one binder short).
		"arity-mismatch": "foreign bad : (a : Whole) -> Eq Bool (sleb a a) (sleW a a)\nlower sleb to sleW by bad\n",
		// codomain not an Eq at all.
		"not-eq-former": "foreign bad : (a : Whole) -> (b : Whole) -> Bool\nlower sleb to sleW by bad\n",
	}
	for name, tail := range cases {
		t.Run(name, func(t *testing.T) {
			s := New()
			_, err := s.LoadSource(lowerProg + tail)
			if err == nil {
				t.Fatalf("expected rejection for %s, got acceptance", name)
			}
			if !strings.Contains(err.Error(), "lower") {
				t.Fatalf("error should mention the lower directive, got %v", err)
			}
		})
	}
}
```

If `New()`/`PRELUDE_NAT` are named differently in this package, match the existing test constructor (grep `func New(` and the nataccel preamble constant).

- [ ] **Step 2: Run to verify it fails**

Run: `go test ./internal/session/ -run TestAddLowering -v`
Expected: FAIL (AddLowering / Lowering undefined).

- [ ] **Step 3: Add the table field + accessor + lifecycle**

In `internal/session/session.go`: add the field to the `Session` struct near `natAccel`:

```go
	lower map[core.Hash]core.Hash // slow-hash -> fast-hash (proof-gated redirect, Plan C)
```

In `resetBuiltins` (grep its body) add:

```go
	s.lower = map[core.Hash]core.Hash{}
```

(and ensure `New`/`Reset` route through `resetBuiltins`, as they do for `natAccel`). Add the accessor near `RefNames`:

```go
// Lowering returns the proof-gated lowering table (slow-hash -> fast-hash) used
// by codegen to redirect a reference to its verified native twin. Never nil once
// the session is initialized.
func (s *Session) Lowering() map[core.Hash]core.Hash { return s.lower }
```

- [ ] **Step 4: Dispatch in LoadSource**

In `LoadSource`, next to `case surface.BuiltinNumInj:` add:

```go
		case surface.LowerDef:
			if err := s.AddLowering(d); err != nil {
				return nil, err
			}
```

- [ ] **Step 5: Implement the proof-gated AddLowering**

Add near `AddBuiltinNatOp`. The gate is SYNTACTIC on the proof's stored type (de Bruijn core, so structural match is exactly right): peel the Pi telescope, require the codomain be `core.Eq{L, R}` where `L` is the ref-spine `slow $n-1 ... $0` and `R` is `fast $n-1 ... $0`.

```go
// AddLowering validates and registers a proof-gated lowering directive (v4 Ord
// Plan C): `lower slow to fast by proof` redirects the slow def's hash to the
// fast def's hash at codegen, so emitted code calls the O(1) native twin while
// the proofs stay stated over the slow def.
//
// Soundness gate. The redirect is sound iff slow and fast are extensionally
// equal. Rather than trust the registrant (or sample, as the arithmetic accels
// do), this CHECKS the supplied proof: its stored type must be, for some arity n,
//
//	(x1 : A1) -> ... -> (xn : An) -> Eq B (slow x1 ... xn) (fast x1 ... xn)
//
// verified structurally on the de Bruijn core (alpha-invariant, so `slow x...`
// must appear as the ref-spine `Ref(slowHash) applied to Var(n-1)...Var(0)`).
// A wrong proof (a different equality, wrong arity, or a non-Eq codomain) is
// REJECTED here, so no unsound redirect can enter codegen. Session state only.
func (s *Session) AddLowering(d surface.LowerDef) error {
	slowH, ok := s.refs[d.Slow]
	if !ok {
		return fmt.Errorf("lower %s to %s: unknown name %q", d.Slow, d.Fast, d.Slow)
	}
	fastH, ok := s.refs[d.Fast]
	if !ok {
		return fmt.Errorf("lower %s to %s: unknown name %q", d.Slow, d.Fast, d.Fast)
	}
	proofH, ok := s.refs[d.Proof]
	if !ok {
		return fmt.Errorf("lower %s to %s by %s: unknown proof %q", d.Slow, d.Fast, d.Proof, d.Proof)
	}
	pd, ok := s.byHash[proofH]
	if !ok {
		return fmt.Errorf("lower %s to %s by %s: proof has no stored type", d.Slow, d.Fast, d.Proof, d.Proof)
	}
	// Peel the proof type's Pi telescope: collect arity n, land on the codomain.
	n := 0
	ty := pd.Ty
	for {
		pi, isPi := ty.(core.Pi)
		if !isPi {
			break
		}
		n++
		ty = pi.Cod.Body
	}
	eq, isEq := ty.(core.Eq)
	if !isEq {
		return fmt.Errorf("lower %s to %s by %s: proof codomain is %T, not an `Eq` between %s and %s", d.Slow, d.Fast, d.Proof, ty, d.Slow, d.Fast)
	}
	// The proof must equate the ref-spines `slow $n-1 ... $0` and `fast $...`.
	if !isRefSpine(eq.L, slowH, n) {
		return fmt.Errorf("lower %s to %s by %s: proof LHS is not `%s` applied to all %d arguments", d.Slow, d.Fast, d.Proof, d.Slow, n)
	}
	if !isRefSpine(eq.R, fastH, n) {
		return fmt.Errorf("lower %s to %s by %s: proof RHS is not `%s` applied to all %d arguments", d.Slow, d.Fast, d.Proof, d.Fast, n)
	}
	s.lower[slowH] = fastH
	return nil
}

// isRefSpine reports whether t is `Ref(h)` applied to exactly the n telescope
// variables in order: under n binders the first bound variable has de Bruijn
// index n-1 and the last has index 0, so the spine is
// `App(...App(Ref(h), Var(n-1))..., Var(0))`.
func isRefSpine(t core.Tm, h core.Hash, n int) bool {
	for i := 0; i < n; i++ {
		app, ok := t.(core.App)
		if !ok {
			return false
		}
		v, ok := app.Arg.(core.Var)
		if !ok || v.Idx != i { // outermost App peeled first: its Arg is Var{Idx:0} (i=0)
			return false
		}
		t = app.Fn
	}
	ref, ok := t.(core.Ref)
	return ok && ref.Hash == h
}
```

VERIFY the exact field names against core/term.go before writing: `core.Eq` fields (the test above assumes `.L`/`.R`; if the struct names them `Lhs`/`Rhs` or the type arg differently, adapt), `core.App{Fn, Arg}`, `core.Var{Idx}` (the index field is `Idx`), `core.Pi{Cod}` where `Cod` is a `Scope` carrying `.Body` (confirm; the `telescope` helper in elaborate/data.go reads `pi.Cod.Body`, so mirror that). `core.Ref{Hash}` (confirm field). The spine-walk peels outermost `App` first: the LAST argument applied is `Var(0)`, matched on the first loop iteration -- CONFIRM the index arithmetic by tracing `sleb a b` under 2 binders: it is `App(App(Ref sleb, Var 1), Var 0)`; peeling the outer App gives `Var 0` (i=0), then `Var 1` (i=1). So the check `v.Index == i` is correct.

- [ ] **Step 6: Run to verify pass**

Run: `go test ./internal/session/ -run TestAddLowering -v`
Expected: PASS (acceptance + all three rejections).

- [ ] **Step 7: Commit**

```bash
git add internal/session/session.go internal/session/lowering_test.go
git commit -m "feat(session): proof-gated AddLowering registry (peels the proof telescope, matches ref-spines)"
```

---

### Task 3: Codegen Erase redirect (the single choke point)

**Files:**
- Modify: `codegen/ir.go` (thread `lower map[core.Hash]core.Hash` into `Erase`; redirect at the `core.Ref` case)
- Modify: `internal/session/session.go` (`emitDefs` passes `s.lower` to Erase)
- Modify: existing direct callers of `Erase` (tests) to pass `nil`
- Test: `codegen/lowering_test.go` (create) and an emit-level session test

**Interfaces:**
- Consumes: `s.Lowering()` (Task 2).
- Produces: emitted programs whose references to a lowered slow def call the fast def instead; the slow def is then tree-shaken out.

- [ ] **Step 1: Write the failing test**

The cleanest observable: erase a term that references the slow hash and assert the resulting `IGlobal` carries the FAST name. Do it at the session/emit level so the whole pipeline (redirect + shake) is exercised. Create `internal/session/lowering_emit_test.go`:

```go
package session

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// After `lower sleb to sleW`, a main that calls sleb must EMIT a call to sleW,
// and the sleb definition must be tree-shaken away.
func TestLoweringRedirectsEmit(t *testing.T) {
	s := New()
	src := lowerProg +
		"foreign slebEquiv : (a : Whole) -> (b : Whole) -> Eq Bool (sleb a b) (sleW a b)\n" +
		"lower sleb to sleW by slebEquiv\n" +
		"main : Bool is sleb (succ zero) (succ (succ zero)) end\n"
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load: %v", err)
	}
	p, err := s.EmitProgram("main")
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	js, err := codegen.JS{}.Emit(p)
	if err != nil {
		t.Fatalf("js emit: %v", err)
	}
	out := string(js)
	if !strings.Contains(out, "sleW") {
		t.Fatalf("emitted program does not call sleW (redirect did not fire):\n%s", out)
	}
	// The slow def must be shaken out: no top-level `sleb` definition remains.
	for _, d := range p.Defs {
		if d.Name == "sleb" {
			t.Fatalf("sleb was not tree-shaken after redirect")
		}
	}
}
```

Confirm `p.Defs` field/name for the emitted `codegen.Program` (grep `type Program` in codegen), and the `IGlobal`/def name field used for the shake assertion.

- [ ] **Step 2: Run to verify it fails**

Run: `go test ./internal/session/ -run TestLoweringRedirectsEmit -v`
Expected: FAIL (redirect not implemented; `sleb` still called / still defined).

- [ ] **Step 3: Thread `lower` into Erase and redirect**

In `codegen/ir.go`, change the `Erase` signature to accept the table and redirect at the ref case:

```go
// Erase lowers a checked, meta-free core term to the erased IR. `lower` is the
// proof-gated lowering table (slow-hash -> fast-hash, Plan C): a top-level
// reference whose hash appears as a key is redirected to its verified native
// twin before naming, so all backends inherit the rewrite. A nil table disables
// redirection.
func Erase(t core.Tm, names map[core.Hash]string, typeRefs map[core.Hash]bool, lower map[core.Hash]core.Hash) Ir {
	switch tm := t.(type) {
	case core.Ref:
		h := tm.Hash
		if lower != nil {
			if to, ok := lower[h]; ok {
				h = to
			}
		}
		if typeRefs[h] {
			return IUnit{}
		}
		n, ok := names[h]
		if !ok {
			n = "$" + h.Short()
		}
		return IGlobal{Name: n}
	// ... all other cases unchanged, but every recursive Erase(...) call gains `, lower` ...
	}
}
```

Update EVERY recursive `Erase(x, names, typeRefs)` call inside `ir.go` to `Erase(x, names, typeRefs, lower)` (the lambda body, both `IApp` args, both `ILet` args, the annotation-strip case, and any case-arm/pair/field recursions). Grep `Erase(` within ir.go to catch all of them.

IMPORTANT ordering note to preserve behavior: the ORIGINAL code checked `typeRefs[tm.Hash]` then `names[tm.Hash]` on the un-redirected hash. After redirect, both must key off the REDIRECTED `h` (a fast twin is a normal value def, never a type former, so `typeRefs[h]` is false and `names[h]` is its emitted name). This is what the snippet does.

- [ ] **Step 4: Pass the table from emitDefs, fix other callers**

In `internal/session/session.go` `emitDefs`, find every `codegen.Erase(body, emitNames, typeRefs)` call (grep `Erase(` in the file) and append `, s.lower`.

Then fix the remaining direct callers so the build compiles: grep the whole tree `codegen.Erase(` and `\bErase(` in `codegen/*_test.go`; pass `nil` as the new final argument to each (they do not exercise lowering).

Run: `go build ./...`
Expected: compiles (all `Erase` call sites updated).

- [ ] **Step 5: Add a focused codegen unit test for the redirect primitive**

Create `codegen/lowering_test.go` (unit-level, no session):

```go
package codegen

import (
	"testing"

	"goforge.dev/rune/v3/core"
)

func TestEraseRedirectsLoweredRef(t *testing.T) {
	// Two distinct hashes standing in for slow/fast.
	slow := core.HashOf( /* any distinct string token per the core.Hash constructor */ )
	fast := core.HashOf( /* a different token */ )
	names := map[core.Hash]string{slow: "sleb", fast: "sleW"}
	typeRefs := map[core.Hash]bool{}
	lower := map[core.Hash]core.Hash{slow: fast}

	got := Erase(core.Ref{Hash: slow}, names, typeRefs, lower)
	g, ok := got.(IGlobal)
	if !ok || g.Name != "sleW" {
		t.Fatalf("want IGlobal sleW, got %#v", got)
	}
	// Nil table = no redirect.
	got2 := Erase(core.Ref{Hash: slow}, names, typeRefs, nil)
	if g2, ok := got2.(IGlobal); !ok || g2.Name != "sleb" {
		t.Fatalf("nil table should not redirect, got %#v", got2)
	}
}
```

Use the actual `core.Hash` construction API (grep how tests build a `core.Hash`, e.g. `core.HashBytes`/`core.HashOf`/a literal); the two hashes only need to be distinct. Match `core.Ref`'s field name.

- [ ] **Step 6: Run to verify pass**

Run: `go test ./codegen/ -run TestEraseRedirect -v && go test ./internal/session/ -run TestLoweringRedirectsEmit -v`
Expected: PASS (redirect fires at the primitive level and end-to-end; slow def shaken out).

- [ ] **Step 7: Commit**

```bash
git add codegen/ir.go codegen/lowering_test.go internal/session/session.go internal/session/lowering_emit_test.go codegen/*_test.go
git commit -m "feat(codegen): redirect lowered refs at the shared Erase choke point (all backends inherit)"
```

---

### Task 4: Wire the prelude `lower leb to leW by lebEquiv`

**Files:**
- Modify: `internal/prelude/prelude.rune` (one directive line after `lebEquiv`)
- Test: rely on the existing prelude-load + listings gates; add one session pin

**Interfaces:**
- Consumes: `leb`, `leW`, `lebEquiv` (already in the prelude), the Task 1-3 machinery.
- Produces: the live redirect for the whole tower (ordWhole/ordInt/ordFrac comparison compiles to `leW`).

- [ ] **Step 1: Add the directive**

In `internal/prelude/prelude.rune`, immediately AFTER the `lebEquiv` definition (ends around line 265, find the `end` closing `lebEquiv`), add:

```
-- LOWERING (v4 Ord Plan C): compiled/emitted comparison runs the O(1) native
-- twin `leW` (rides the natMonus accel on all 9 backends) instead of the O(n)
-- recursive `leb`. The proofs stay stated over `leb`; `lebEquiv` above is the
-- kernel-checked witness that the redirect is sound. See docs Decision 6.
lower leb to leW by lebEquiv
```

- [ ] **Step 2: Verify the prelude still loads**

Run: `go test ./internal/session/ -run TestPrelude -v` (or the test that loads the shared prelude; grep `internal/prelude` usage in tests).
Expected: PASS (the directive registers; `lebEquiv` has exactly the required type, so the gate accepts).

- [ ] **Step 3: Add a prelude-level redirect pin**

Add to `internal/session/lowering_test.go`:

```go
// The shipped prelude registers leb -> leW.
func TestPreludeLowersLeb(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(""); err != nil { // loads the always-on prelude per the CLI path; adapt to the real prelude-load entry
		t.Fatalf("prelude load: %v", err)
	}
	if len(s.Lowering()) == 0 {
		t.Fatalf("prelude did not register any lowering")
	}
}
```

Adapt the prelude-load invocation to however tests obtain a prelude-loaded session (grep for where `internal/prelude` source is fed to a `Session`; there is an always-on loader per the script-ergonomics work).

- [ ] **Step 4: Run the listings gate (nothing regresses)**

Run: `go test ./harness/ -run TestListings -v`
Expected: PASS (every chapter still elaborates, checks, runs; the redirect only changes emitted reference targets, and `lebEquiv` guarantees semantic equality).

- [ ] **Step 5: Commit**

```bash
git add internal/prelude/prelude.rune internal/session/lowering_test.go
git commit -m "feat(prelude): lower leb to leW by lebEquiv (tower comparison now compiles native)"
```

---

### Task 5: 9-backend comparison conformance gate + REPL acceptance

**Files:**
- Modify: `harness/backend_conformance_test.go` (add a comparison corpus program) OR create `harness/comparison_conformance_test.go` following the existing conformance harness shape
- Test: the conformance corpus + REPL pins
- Modify (if a listing is the corpus vehicle): `listings/ch576_lowering.rune` is created in Task 6; this task may reference it, or use an inline program

**Interfaces:**
- Consumes: the full pipeline (prelude redirect + all 9 backends).
- Produces: the acceptance gate proving byte-identical comparison results across js/py/go/rust/beam/jvm/c/ll/wasm.

- [ ] **Step 1: Write the conformance program**

A self-contained rune program exercising the three views on the tower at boundary + interior + large + negative values. Put it where the existing conformance corpus lives (grep `TestBackendConformance` in harness/backend_conformance_test.go for how programs are registered; follow that exact pattern). The program (uses the prelude tower):

```
-- comparison corpus: le/compare/eqb on Whole, Int, Frac.
main : ... is
  seq
    -- Whole: boundary (0 <= 0), interior (3 <= 5), reverse (5 <= 3),
    -- large bignum (10^18 <= 10^18 + 1)
    ... print (le Whole ordWhole 0 0)
    ... print (le Whole ordWhole 3 5)
    ... print (le Whole ordWhole 5 3)
    ... print (compareOf Whole ordWhole 3 5)   -- lt
    -- Int: negative comparisons
    ... print (le Int ordInt (zneg 2) (zpos 1))
    -- Frac: non-canonical representatives
    ... print (le Frac ordFrac (frac 1 2) (frac 2 4))  -- equal reps
  end
end
```

Fill in the exact constructor/print spellings by reading how the existing arithmetic conformance corpus renders Bool/Ordering results to bytes (the divergence-lock gates print via the D6 ioprims). The REQUIREMENT: the output is byte-identical across all 9 backends. Keep the corpus small but cover: Whole boundary/interior/reverse/large-bignum, Int negative, Frac non-canonical, at least one `compare` (Ordering) and one `eqb` (DecEq) call.

- [ ] **Step 2: Run the conformance gate**

Run: `go test ./harness/ -run 'Conformance' -v` (the target that runs the multi-backend byte-identity check; native C/LLVM and JVM/WASM may be behind their own sub-tests as in the existing suite).
Expected: PASS (all available backends byte-identical). If a backend is environment-gated (JVM/native), match the existing skip conventions; do NOT weaken them.

- [ ] **Step 3: REPL acceptance pins**

Add REPL-level pins (grep an existing REPL test, e.g. TestREPL* in internal/repl or internal/session, for the harness). Assert:

```
le Whole ordWhole 3 5          --> true
le Whole ordWhole 5 3          --> false
compareOf Whole ordWhole 3 5   --> lt
le Int ordInt (zneg 2) (zpos 1) --> true
eqb Whole decWhole 4 4         --> true   (use the real DecEq instance/accessor names from the prelude)
```

and a large-bignum comparison that returns promptly (no succ-chain materialization) -- e.g. `le Whole ordWhole 1000000000000 1000000000001 --> true`. Read the prelude for the exact instance names (`ordWhole`/`ordInt`/`ordFrac`, the DecEq instance, `compareOf`/`leOf`/`eqb`) before writing.

- [ ] **Step 4: Run the REPL pins**

Run: `go test ./internal/repl/... ./internal/session/... -run 'REPL' -v`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add harness/ internal/repl/ internal/session/
git commit -m "test: 9-backend comparison conformance + REPL host-speed acceptance for the lowering"
```

---

### Task 6: Chapter ch576 + bookkeeping

**Files:**
- Create: `listings/ch576_lowering.rune`
- Modify: `harness/listings_test.go` (register ch576 if the listing registry is explicit; grep how ch575 is listed)
- Modify: `internal/session/tower_hash_test.go` (NO new class former, so `TestTowerClassHashesDistinct` is UNCHANGED; add only a one-line comment noting Plan C adds no record former, if the file has a running ledger of covered decisions)

**Interfaces:**
- Consumes: everything above.
- Produces: the self-contained pedagogical listing demonstrating the lowering registry end-to-end.

- [ ] **Step 1: Write ch576**

A self-contained chapter (its own `builtin nat` + `builtin natMonus`, a slow `myLeb`, a fast `myLeW`, a proof `myLebEquiv` by induction, `lower myLeb to myLeW by myLebEquiv`, and a `main` that uses `myLeb`). Narrate: the proof-shaped/speed-shaped pairing, the proof gate, the single-choke-point redirect, proofs untouched. Mirror the byte-identical-mirror discipline of ch574/ch575 (self-contained, book narration). Model the proof `myLebEquiv` on the prelude's `lebEquiv` (read prelude:265 for the induction structure).

- [ ] **Step 2: Verify it elaborates, checks, runs**

Run: `go test ./harness/ -run 'TestListings' -v`
Expected: PASS (ch576 included; `rune run` of its main produces the expected Bool).

- [ ] **Step 3: Confirm the hash-audit is intentionally unchanged**

Run: `go test ./internal/session/ -run TestTowerClassHashesDistinct -v`
Expected: PASS unchanged (Plan C adds NO record former, so no new name enters the audit; this is correct, not an omission).

- [ ] **Step 4: Full suite**

Run: `go test -timeout 30m ./...`
Expected: PASS (the tag gate).

- [ ] **Step 5: Commit**

```bash
git add listings/ch576_lowering.rune harness/listings_test.go internal/session/tower_hash_test.go
git commit -m "docs(listings): ch576 lowering registry chapter + Plan C close"
```

---

## Self-Review (plan author)

- **Spec coverage:** Decision 6 (proof-gated lowering registry) -> Tasks 1-4; conformance + REPL acceptance (Decision 6 gates + testing summary) -> Task 5; chapter (Decision 7 spirit, one new listing) -> Task 6; "no new record former, hash audit unchanged" (Decision 6 / non-goals) -> Task 6 Step 3. The `lower … by …` directive + AddLowering proof gate + Erase redirect + 9-backend conformance + redirect unit test + rejection test are all present.
- **Kernel-frozen check:** no task edits core/store/elaborate semantics; Task 2/3 only READ `core.Eq`/`core.Pi`/`core.App`/`core.Var`/`core.Ref` and `telescope`-style peeling. No hash-format bump.
- **Placeholder scan:** the two spots requiring on-the-spot field-name confirmation (`core.Eq`/`core.App`/`core.Var`/`core.Ref`/`core.Pi.Cod.Body` field names in Task 2/3; the conformance/REPL instance names in Task 5) are flagged with an explicit "VERIFY against core/term.go / the prelude" instruction and a trace to confirm the de Bruijn index arithmetic. The proof-gate test uses a `foreign` axiom to exercise both acceptance and rejection without a heavy induction fixture.
- **Type consistency:** `LowerDef{Slow,Fast,Proof}` (Task 1) -> `AddLowering` reads those fields (Task 2) -> `s.lower map[core.Hash]core.Hash` + `Lowering()` (Task 2) -> `Erase(..., lower)` (Task 3). Consistent throughout.
