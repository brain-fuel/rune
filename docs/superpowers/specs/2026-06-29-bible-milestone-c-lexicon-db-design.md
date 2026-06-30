# Bible Port -- Milestone C: `build_db` (lexicon table) (Design)

**Author:** Claude (with Matt)
**Date:** 2026-06-29
**Status:** Approved design; ready for implementation plan.
**Source request:** `~/matt/goforge.dev/bible_feature_request.md` (FR5; Milestone C "the DB").
**Rune baseline:** local checkout `~/matt/goforge.dev/rune`, v3.348.0 (Milestone B shipped).
**Builds on:** [Milestone B](2026-06-29-bible-milestone-b-shared-root-design.md) -- reuses `foldDir`/`jsonStrField`/`byteLen` and the write-stream trio `openWrite`/`writeChunk`/`closeWrite`/`sortFile`, the `__s2h`/`__h2s` codec, the `'latin1'` byte-exact read.

---

## 1. Purpose

Port `tools/build_db.py` -- restricted to its **`lexicon` table** -- to the Rune->Go binary, so a `rune run` produces a SQLite database whose `lexicon` table is **query-equivalent** to Python's `data/tokens.sqlite`. This is Milestone C ("the DB"): the smallest complete database builder. It introduces the SQL-emit + load path (a new `dbApply` host op shelling to the `sqlite3` CLI, and a `sqlQuote` value-escaper) that every later table reuses, and it establishes the **query-equivalence** acceptance model that replaces Milestone B's byte-identity (a `.sqlite` file is not byte-deterministic).

## 2. What `build_db` does, restricted to the lexicon table (the Python being ported)

`tools/build_db.py` builds an 8-table SQLite DB; this milestone ports ONLY the `lexicon` table:

1. `CREATE TABLE lexicon (strong TEXT PRIMARY KEY, lemma TEXT, translit TEXT, lang TEXT, pos TEXT, root TEXT)`.
2. For every `lexicon/grc/*.json` + `lexicon/hbo/*.json` entry (23,681 files), `INSERT OR IGNORE` one row pulling the six top-level scalar fields `strong`/`lemma`/`translit`/`lang`/`pos`/`root`. `strong = entry.get("strong") or None` (empty string -> NULL); the other five are `entry.get(field)` (absent -> NULL).
3. `CREATE INDEX idx_lexicon_lemma ON lexicon(lemma)`.

NULL-strong entries (LXX-only `lemma-<slug>.json`) are still inserted (SQLite treats each NULL PK as distinct). The entry's nested `glosses`/`senses`/`domains` feed OTHER tables and are OUT of scope here.

## 3. Findings that shape the design

Verified against the real data and the Milestone-B build:
- **Byte-identity is gone.** A `.sqlite` file is not byte-deterministic (AUTOINCREMENT rowids, page/b-tree layout, INSERT-OR-IGNORE collision order). The acceptance is **query-equivalence**: the loaded `lexicon` table's sorted dump + `COUNT(*)` equal Python's. The deterministic `.sql` TEXT retains a byte-identical gate (the rune-side determinism check).
- **Milestone C is the first non-ASCII WRITE consumer.** Greek/Hebrew lemmas flow into the SQL text. The Milestone-B parked finding -- the JS write vocabulary (`writeFileCode`/`writeChunk`/`sortFile` output) emits Node's default utf8, not raw bytes -- now has a consumer and MUST be fixed (write `Buffer.from(str, 'latin1')`) so the JS conformance gate matches the Go target on non-ASCII. Fix the whole write vocabulary at once (the parked note's instruction).
- **The six lexicon fields are all top-level strings**, so `jsonStrField` (Milestone B) extracts each with no new parsing.
- **No PK collisions in the lexicon load.** grc strongs (`G####`) and hbo strongs (`H####`) are disjoint and unique within each language; NULL strongs never collide. So `INSERT OR IGNORE` order does not affect table CONTENTS -- query-equivalence holds regardless of emit order.
- **`sqlite3` CLI is the loader.** No Go SQLite library enters the dependency graph (Standing engineering convention: no new dependency without recording why). `sqlite3` is a test-environment tool, gracefully skipped where absent (see section 8).

## 4. Architecture

A streaming pipeline mirroring Milestone B, plus a load step. State stays O(1) -- no large in-language `List`. **Two new host ops** (Go target + JS conformance gate; D6 foreign-op mechanism; no core/store/elaborate change, no hash bump) plus a write-vocabulary fix:

| Op | Rune type | Host behavior |
|----|-----------|---------------|
| `sqlQuote` | `Bytes -> Bytes` | SQL-escape a string value: double every `'` to `''`, wrap the whole in single quotes. Pure. Produces text that SQLite parses back to the original bytes. |
| `dbApply` | `Path -> Path -> IO Unit` | run the `sqlite3` CLI to load a `.sql` script into a database file (`sqlite3 <dbPath> ".read <sqlPath>"`). The whole build becomes one `rune run`. Returns unit. |

**Write-vocabulary fix (no new op):** `writeFileCode`, `writeChunk`, `sortFile` JS bodies write `Buffer.from(s, 'latin1')` instead of the default-utf8 string, so JS write output is raw bytes matching Go. (Un-parks the Milestone-B finding.)

**NULL handling is in-Rune**, not a host op: a field is an `Option Bytes` from `jsonStrField`; `OptionElim` maps `none -> codeOf "NULL"` and `some v -> sqlQuote v`. `strong`'s empty-string-as-NULL: `jsonStrField` returns `none` for an absent field and `some ""` for an empty string; the builder treats a zero-length `some` as NULL too (matching Python's `or None`) via a `byteLen` guard.

**Reused from Milestone B:** `foldDir`, `jsonStrField`, `byteLen`, the write-stream trio, `sortFile`, the codec, the `'latin1'` read.

## 5. Data flow (4 phases + load)

1. **`foldDir lexiconDir ".json" step (openWrite stage1)`.** The step extracts the six fields via `jsonStrField`, renders each to a SQL term (`NULL` or `sqlQuote`d), assembles one line `INSERT OR IGNORE INTO lexicon(strong,lemma,translit,lang,pos,root) VALUES (s,l,t,g,p,r);`, and `writeChunk`s it. State = the write handle. `closeWrite` after. -> ~23,681 unsorted INSERT lines in stage1.
2. **`sortFile stage1 stage1.sorted`.** Bytewise line sort -> a deterministic INSERT order (for the `.sql` text gate; contents-irrelevant per section 3).
3. **Assemble the script** using only existing ops (no concat op): `openWrite out.sql`; `writeChunk` the `CREATE TABLE lexicon (...)` DDL line; `foldLines stage1.sorted` re-`writeChunk`ing each sorted INSERT line into `out.sql`; `writeChunk` the `CREATE INDEX idx_lexicon_lemma ON lexicon(lemma);` line; `closeWrite`. (DDL must lead and the index trail, so the assembly stream is built post-sort -- the sort runs on the INSERT-only `stage1`.)
4. **`dbApply out.db out.sql`.** The `sqlite3` CLI loads the script into a fresh `out.db`.

The DDL + index lines are constant templates. The only varying text is the per-entry `VALUES (...)`, built from the six `sqlQuote`/`NULL` terms.

## 6. Determinism and escaping (the `.sql` text gate)

- **Constant DDL/index templates** -- `CREATE TABLE lexicon (strong TEXT PRIMARY KEY, lemma TEXT, translit TEXT, lang TEXT, pos TEXT, root TEXT);` and `CREATE INDEX idx_lexicon_lemma ON lexicon(lemma);`, byte-for-byte.
- **`sqlQuote`** wraps in `'...'` and doubles internal `'`. ASCII fields (strong/lang/pos) and Unicode fields (lemma/translit/root) pass through as raw bytes (with the JS write fix, Go and JS emit identical bytes).
- **`NULL`** is the bare literal (no quotes) for absent/empty fields.
- **Bytewise `sortFile`** orders the INSERT lines deterministically; each line ends `\n`, file ends with a trailing newline.
- The `.sql` TEXT is byte-identical across the Go and JS backends -- the always-on determinism gate.

## 7. Error handling

- Missing lexicon dir -> `foldDir` yields nothing -> a DDL+index-only `.sql` -> an empty `lexicon` table (Python builds the same empty table).
- A file `jsonStrField` cannot read is skipped (foldDir's per-file skip; matches Python's `try/except: continue`).
- `dbApply` to a fresh path; the build targets a fresh `out.db` (no stale-DB idempotency concern, like Python's `unlink`).
- `sqlite3` absent -> the load-and-query gates skip (section 8); the `.sql` text gate still runs.

## 8. Testing

- **Per-op unit gates** (Go + JS, always-on): `sqlQuote` (escape a value with an embedded `'`, and a plain value, checked via byteLen/round-trip in a listing); `dbApply` (write a tiny `.sql`, apply it, query back -- guarded on `sqlite3` presence via `exec.LookPath`, skipped otherwise). The JS write-fix is covered by a non-ASCII round-trip regression (write a Greek string, read it back byte-exact, js+go).
- **Always-on builder `.sql`-text gate:** a small **synthetic lexicon fixture** (a handful of entries: a grc entry, an hbo entry, a NULL-strong / LXX-only lemma entry, an entry with an embedded `'` in a field to exercise escaping) -> emit `out.sql` -> assert byte-identical to a committed expected `.sql`. Fast; no `sqlite3` needed; js + go.
- **sqlite3-guarded fixture query gate:** the same fixture -> `dbApply` into a temp `.db` -> `SELECT count(*)` and a sorted `SELECT *` match the expected rows. Skipped if `sqlite3` is absent.
- **Byte-equivalent acceptance gate (the real one):** env-gated (`BIBLE_REPO=~/matt/bible`, and `sqlite3` present). Build the lexicon `.db` from the real 23,681-file lexicon, then `sqlite3 rune.db "SELECT * FROM lexicon ORDER BY strong IS NULL, strong, lemma, translit, lang, pos, root"` must equal the same query against Python's `data/tokens.sqlite`; `SELECT COUNT(*)` must match. Query-equivalence, on the Go target. ~5min (dominated by the 23,681 file reads through the codec, as in Milestone B). Skipped without the env var, like the live-service tests.
- **Listings gate:** the builder + any op-demo listings live in `listings/` so `TestListingsElaborateAndCheck` keeps them elaborating + checking.

## 9. Acceptance criteria

1. The two new host ops (`sqlQuote`, `dbApply`) elaborate, type-check, and erase on Go + JS; the JS write-vocabulary fix lands; no core/store/elaborate edit; no hash-format bump; Go + JS only.
2. Each op passes its per-op unit gate on Go + JS (`dbApply`'s query assertion guarded on `sqlite3`).
3. The synthetic-fixture `.sql`-text gate is byte-identical js + go (always-on); the sqlite3-guarded fixture query gate passes where `sqlite3` is present.
4. With `BIBLE_REPO` set and `sqlite3` present, the real-lexicon build is **query-equivalent** to Python's `tokens.sqlite` lexicon table (sorted dump + `COUNT(*)` identical) on the Go target.
5. `go test ./harness/ -run TestListingsElaborateAndCheck` stays green.

## 10. Explicitly out of scope (named for honesty)

The other seven tables (verses/tokens/glosses/senses/domains/mt_lxx/relations) + the `relations_default` view + the ten non-lexicon indexes; the nested glosses/senses/domains extraction; batched-transaction tuning and the 11.6M-row relations load; real SQLite Go bindings (`modernc.org/sqlite`/`mattn`); a type-safe query DSL (FR5 P2); `dbQuery`/`dbPrepare`/`dbBind` streaming-read ops; py/rust/beam/jvm/native backends for the new ops (Go + JS only, consumer-driven). Moving the builder `.rune` source into the bible repo (it lands as a rune listing).

## 11. Risks

- **`sqlite3` CLI availability.** The load + query gates depend on the `sqlite3` binary. Mitigation: the `.sql`-text gate is always-on and `sqlite3`-free (catches determinism + escaping regressions); the load/query gates `exec.LookPath`-guard and skip cleanly. Document `sqlite3` as a test-env tool.
- **JS write-encoding fix scope.** Changing `writeFileCode`'s JS body touches an op shipped since v3.20.0 (ch215). Mitigation: a non-ASCII round-trip regression on js + go; the change makes JS match Go (the byte-exact target), and the Milestone-B byte-identical shared-root gate (ASCII-only) is unaffected.
- **`sqlQuote` correctness on Unicode.** Escaping must touch only the ASCII `'` byte and pass all other bytes (incl. multi-byte Greek/Hebrew) through untouched. Mitigation: operate at the byte level (the packed code), doubling only byte 0x27; the query-equivalence gate over the full lexicon catches any divergence.
- **`jsonStrField` empty-vs-absent for `strong`.** Python's `strong or None` folds empty-string to NULL; the builder must treat a zero-length `some` as NULL (a `byteLen` guard), or a `strong=""` entry would emit `''` where Python emits NULL. Covered by the fixture's NULL-strong entry and the real-lexicon gate.
