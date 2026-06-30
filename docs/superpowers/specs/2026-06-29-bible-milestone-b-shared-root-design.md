# Bible Port — Milestone B: `build_shared_root` (Design)

**Author:** Claude (with Matt)
**Date:** 2026-06-29
**Status:** Approved design; ready for implementation plan.
**Source request:** `~/matt/goforge.dev/bible_feature_request.md` (Milestone B; FR2 write-side, FR4 minimal, FR7).
**Rune baseline:** local checkout `~/matt/goforge.dev/rune`, v3.347.0 (Milestone A streaming shipped).
**Builds on:** [Milestone A](2026-06-29-bible-milestone-a-streaming-design.md) -- reuses `foldLines`/`splitOn`/`byteLen`, the `__s2h`/`__h2s` codec, and the `'latin1'` byte-exact read.

---

## 1. Purpose

Port `tools/relations/build_shared_root.py` to the Rune->Go binary so it reproduces
`relations/derived/shared-root.jsonl` (80,904 lines) **byte-identical** to Python. This is
Milestone B ("one builder"): the smallest complete builder -- no DB, no download, no XML --
that validates the data model and the JSONL **write path** end to end. It introduces the
write-side and sort primitives every later builder/miner reuses.

## 2. What `build_shared_root` does (the Python being ported)

1. Load all lexicon entries from `lexicon/grc/*.json` + `lexicon/hbo/*.json` (23,681 files, one
   JSON object each; fields incl. `strong`, `lemma`, `root`).
2. Group entry **keys** by `root` value, skipping entries with falsy `root`.
3. For each root group, emit one `shared-root` Edge per unordered key pair, canonically
   oriented (`src <= dst`), excluding self-loops.
4. Write JSONL sorted by `(src, rel, dst, source)`, UTF-8 / LF, one JSON object per line.

The lexkey (`key_for`) is `entry['strong']` if present, else `"lemma-" + sha1(lemma)[:12]`.

## 3. Findings that shaped (and shrank) the design

Verified against the real data:
- **0 of 9,584 root-bearing entries lack a `strong`.** So shared-root endpoints are **always**
  strong keys -- **no sha1 / lemma-slug needed** (FR3's sha1 gap does not bite Milestone B). The
  Python `valid_keys` filter (`if key not in lexicon_keys(): continue`) is provably a **no-op**
  here: every root-group key is its own present lexicon entry. Skipped entirely.
- **The sort key reduces to `(src, dst)`** -- `rel="shared-root"` and `source="strongs-root"` are
  constant across every shared-root edge.
- **No JSON-encode primitive** -- each edge line is a fixed template with only `src`/`dst` varying:
  ```
  {"src": "<S>", "dst": "<D>", "rel": "shared-root", "directed": false, "provenance": {"source": "strongs-root", "method": "derived"}, "rank": 65535, "note": null}
  ```
  Built by in-language string interpolation (shipped in ch438/439).

## 4. Architecture

A streaming, 4-phase pipeline. **No large in-language `List` is ever materialized** -- state
stays O(group size), dodging the eager-eliminator blow-up (see [[rune-strict-case-arms]]). The
loops live in the host (foldDir/foldLines) or in external sort.

**Four new host ops** (Go target + JS conformance gate; D6 foreign-op mechanism; no kernel
change, no hash bump):

| Op | Rune type | Host behavior |
|----|-----------|---------------|
| `foldDir` | `(S:U) -> Path -> Bytes -> (S -> Bytes -> IO S) -> S -> IO S` | recursively enumerate files under a dir whose name ends with the given suffix (e.g. `.json`); apply the erased step to each file's CONTENTS (read `'latin1'` byte-exact), threading state. Higher-order, like `foldLines`. Enumeration order is irrelevant (output is sorted in phase 4). |
| `sortFile` | `Path -> Path -> IO Unit` | read the input file's lines, **bytewise** stable line sort, write to the output path (LF-joined, trailing newline). The FR7 external sort -- scales to the 11.6M-edge miners later. |
| `openWrite` / `writeChunk` / `closeWrite` | `Path -> IO Handle` ; `Handle -> Bytes -> IO Handle` ; `Handle -> IO Unit` | the FR2 write path: open a file for writing, append a chunk (the Rune side appends `\n` itself), close. `Handle` is a foreign type (erases like `Pid`). |
| `jsonStrField` | `Bytes -> Bytes -> Option Bytes` | extract a TOP-LEVEL JSON string field by name from an object; `None` if the field is absent or `null`. Minimal FR4 (not a full parser); the byte-identical gate is the correctness net. |

**Reused from Milestone A:** `foldLines`, `splitOn`, `byteLen`, the `__s2h`/`__h2s` codec, the
`'latin1'` byte-exact read.

## 5. Data flow (4 phases, O(group) state)

1. **`foldDir lexiconDir ".json" step h0`** where `h0 = openWrite intermediate1`. The step reads
   each file's contents, pulls `strong` + `root` via `jsonStrField`; if `root` is non-null,
   `writeChunk`s a `root\tkey` line (`key = strong`). State = the write handle. `closeWrite`
   after. -> ~9,584 unsorted `root<TAB>key` lines in intermediate1.
2. **`sortFile intermediate1 sorted1`.** Equal roots become contiguous; within a root the keys
   are ascending (line is `root\tkey`, bytewise).
3. **`foldLines sorted1 step2 s0`** with `s0 = (emptyRoot, nilKeys, openWrite intermediate2)`.
   Per line: `splitOn` TAB -> `(root, key)`. If `root == currentRoot`: push `key`. Else: FLUSH
   the current group -- emit the edge-line template for every ascending pair `(keys[i], keys[j])`,
   `i < j` (so `src <= dst` is free; canonical orientation requires no comparison), `writeChunk`
   each; then reset `currentRoot=root`, `currentKeys=[key]`. After `foldLines` returns, FLUSH the
   final group. `closeWrite`. -> grouped, not-yet-globally-sorted edge lines in intermediate2.
4. **`sortFile intermediate2 outputFinal`.** Bytewise line sort = `(src, dst)` order (template is
   identical up to `src`, then identical between `src` and `dst`). -> byte-identical
   `shared-root.jsonl`.

## 6. Determinism (byte-identical requirements)

- **Line template** hardcodes `: ` / `, ` separators (Python `json.dumps` defaults), lowercase
  `false` / `null`, key order (`src,dst,rel,directed,provenance,rank,note`), nested `provenance`,
  `rank: 65535`, `note: null`. Only `src`/`dst` interpolate.
- Each line is followed by `\n`; the file ends with a trailing newline.
- **Bytewise sort = Python's tuple sort:** strong keys are 4-digit zero-padded (`G0004`), `G`<`H`,
  so lexicographic byte order is the intended `(src,dst)` order.
- **No dedup.** Python `combinations` over a multiset keeps duplicate edges if a `(root,key)`
  repeats; the no-dedup scan replicates that. (Whether any duplicate exists in the real lexicon is
  unknown and irrelevant -- the gate enforces parity either way.)
- **Skip-on-unparseable** matches Python's `try/except: continue`: a file `jsonStrField` cannot
  pull a `root` from is skipped.

## 7. Error handling

- Missing lexicon dir -> `foldDir` yields nothing -> empty output.
- `sortFile` on an empty input -> empty output.
- Intermediates are written under an OS temp dir; the final output to the target path.
- Open-fail on the OUTPUT path is a hard error (not silently swallowed); open-fail on an INPUT
  during `foldDir` skips that file (matches Python's per-file `try/except`).

## 8. Testing

- **Per-op unit gates** (Go + JS, always-on): `foldDir` (count files in a small fixture dir);
  `jsonStrField` (extract `strong`/`root` + null/absent handling from sample entries); `sortFile`
  (shuffled lines -> sorted); the write-stream trio (write lines, read back, verify).
- **Always-on builder gate:** a small **synthetic lexicon fixture** committed to the rune repo (a
  handful of entries across two shared roots, incl. a `root:null` skip and a singleton root that
  yields no edge) -> assert the exact edge lines and ordering. Fast; every CI run.
- **Byte-identical acceptance gate (the real one):** run the full builder on the real 23,681-file
  lexicon -> diff against the committed `relations/derived/shared-root.jsonl` (80,904 lines); must
  be byte-identical, on the Go target. The lexicon lives in `~/matt/bible`, not the rune repo, so
  this gate is **env-gated** (`BIBLE_REPO=~/matt/bible`; skipped otherwise, like the live-service
  tests). The synthetic fixture is the always-green proxy.

## 9. Acceptance criteria

1. The four new host ops elaborate, type-check, and erase on Go + JS; no core/store/elaborate edit;
   no hash bump.
2. Each op passes its per-op unit gate on Go + JS.
3. The synthetic-fixture builder gate produces the exact expected edge lines (always-on).
4. With `BIBLE_REPO` set, the full builder reproduces `shared-root.jsonl` **byte-identical**
   (80,904 lines) on the Go target.
5. `go test ./harness/ -run TestListingsElaborateAndCheck` stays green (every listing still checks).

## 10. Explicitly out of scope (named for honesty)

The other relation builders/miners (domain-sibling, cross-language, synonym, antonym; WordNet/
Roget/BDB miners); FR4 full JSON/XML parsing (only the minimal `jsonStrField` lands); FR5 SQLite;
FR6 `log1p`/`log2` (shared-root's rank is the constant 65535); the sha1 lexkey slug (no consumer
in shared-root); dedup-to-max and group-by-rel (those are `build_relations`, not this builder);
py/rust/beam/jvm/native backends for the new ops (Go + JS only -- consumer-driven). Moving the
builder `.rune` source into the bible repo (it lands as a rune example/listing now).

## 11. Risks

- **`jsonStrField` robustness (primary).** A naive top-level-field extractor could mis-match a
  nested `"root"`/`"strong"`. Mitigation: extract only the first top-level occurrence; the
  byte-identical gate over 80,904 lines catches any divergence immediately. Fallback: tighten the
  extractor (anchor on the object's top level) if the gate fails.
- **Two higher-order host ops** (`foldDir` like `foldLines`). De-risked: `foldLines` already
  proved the erased-closure callback convention (`primSpawn` precedent) on both backends.
- **23,681 file reads** in the Go binary (the full gate) are slow but one-time; the gate is
  env-gated and the synthetic fixture is the fast proxy.
- **Cross-repo coupling:** the full gate depends on `~/matt/bible`. Env-gated, so the rune repo
  stays self-contained.
