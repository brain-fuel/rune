// driver.mjs <module.wat> <glue.js> <scenario>
//
// Assembles the WAT with wabt (the same way a build pipeline or a browser devtool
// would turn source text into WASM bytes -- there is no wat2wasm binary dependency
// here, only the npm package), loads it through the generated glue exactly as a
// browser would (WebAssembly.instantiate against stub WASI imports, no filesystem, no
// process), and runs the named scenario, printing one line per assertion for the Go
// gate to compare.
//
// Scenarios:
//   smoke              initGC -> bump -> bump -> value via the plain Rune-named
//                       export ABI. Prints the counter value (expect 2), then
//                       rt_live() as a baseline delta check for the Go side.
//   smoke-host          the same chain, but the final accessor is called through a
//                       HostName override (glue's EXPORT_MAP keyed by the
//                       host-facing name, not the Rune name) -- the end-to-end
//                       check for Task 2's deferred HostName path. Requires the
//                       caller to build the module with an export whose HostName
//                       is "getValue".
//   converge             TWO INDEPENDENT module instances (A, B -- as isolated as
//   converge-reversed    two browser tabs sharing nothing but the bytes they
//                       choose to exchange). A does two local writes, B one; each
//                       serializes its own state via gcToBin, crosses the
//                       "channel" as plain bytes, and the peer decodes via
//                       gcFromBin and merges into its own state. Both instances
//                       print value(merged) (expect 3), then a live-delta
//                       (rt_live() minus a baseline captured after a throwaway
//                       warm-up round -- the steady-harness run-2 discipline,
//                       since each def_<name> accessor's cache is a one-time
//                       constant allocation on its first call). converge-reversed
//                       swaps which instance processes the remote message first;
//                       the ch72 mergeComm proof says both orders converge to the
//                       same value, observed here at the browser ABI.
import { readFile } from "node:fs/promises";
import { pathToFileURL } from "node:url";

const [watPath, gluePath, scenario] = process.argv.slice(2);
if (!watPath || !gluePath || !scenario) {
  console.error("usage: driver.mjs <module.wat> <glue.js> <scenario>");
  process.exit(2);
}

const wabt = await (await import("wabt")).default();
const wat = await readFile(watPath, "utf8");
const mod = wabt.parseWat("m.wat", wat);
const { buffer } = mod.toBinary({});

// import() needs a file:// URL for an absolute filesystem path under ESM.
const glue = await import(pathToFileURL(gluePath));

if (scenario === "smoke") {
  const h = await glue.load(buffer);
  // init already ran inside glue.load; drive value(bump(bump(initGC))) via the ABI.
  const s0 = glue.call(h, "initGC");
  const s1 = glue.call(h, "bump", s0);
  const s2 = glue.call(h, "bump", s1);
  const v = glue.call(h, "value", s2);
  console.log(glue.readNat(h, v)); // expect 2
  console.log(h.exports.rt_live()); // baseline delta check for the Go side
} else if (scenario === "smoke-host") {
  const h = await glue.load(buffer);
  // Same chain, but the final accessor is looked up by its HostName override
  // ("getValue"), not the bare Rune name -- exercises EXPORT_MAP's HostName branch.
  const s0 = glue.call(h, "initGC");
  const s1 = glue.call(h, "bump", s0);
  const s2 = glue.call(h, "bump", s1);
  const v = glue.call(h, "getValue", s2);
  console.log(glue.readNat(h, v)); // expect 2
  console.log(h.exports.rt_live());
} else if (scenario === "converge" || scenario === "converge-reversed") {
  // Two INDEPENDENT instances -- two separate glue.load calls against the SAME
  // module bytes, exactly like two browser tabs each running their own copy.
  const A = await glue.load(buffer);
  const B = await glue.load(buffer);

  // encodeNatLE mirrors ch565's proven encodeNat (a little-endian bit list,
  // length-prefixed: [bitCount, bit0, bit1, ...], bit0 the least-significant bit)
  // -- used only to hand-home B's own count into the counter's SECOND slot below.
  // Small-n only (this demo's counts are tiny, per ch565's own header note).
  function encodeNatLE(n) {
    const bits = [];
    let v = n;
    while (v > 0) {
      bits.push(v & 1);
      v >>>= 1;
    }
    return [bits.length, ...bits];
  }

  // Warm-up round: seed every def_<name> accessor's memoized cache on BOTH
  // instances, using the SAME NONZERO argument shapes the real scenario below
  // uses (a bumped, non-trivial GC state), not just the trivial all-zero
  // initGC. gcToBin/gcFromBin's own bodies recurse through the bit-codec's Pos/
  // NList machinery (ch565's posToBits/bnBits/append chain), which memoizes its
  // OWN internal sub-accessors the first time it runs a NONZERO encode/decode; a
  // trivial all-zero warm-up call never touches that code path (fromNat 0 short-
  // circuits before any of it), so its one-time cost would otherwise land inside
  // the balance window below instead of the baseline. Each accessor's cache is a
  // one-time constant contribution to $live that has nothing to do with this
  // scenario's own ARC balance; the baseline is captured AFTER this throwaway
  // round -- the steady-harness run-2 discipline applied to a single-shot
  // scenario.
  function warmup(h) {
    glue.release(h, glue.call(h, "value", glue.call(h, "bump", glue.call(h, "initGC"))));
    const w = glue.call(h, "bump", glue.call(h, "bump", glue.call(h, "initGC")));
    glue.retain(h, w);
    const wbin = glue.call(h, "gcToBin", w);
    const wbytes = glue.readBin(h, wbin);
    glue.release(h, wbin);
    const wdecoded = glue.call(h, "gcFromBin", glue.mkBin(h, wbytes));
    glue.release(h, glue.call(h, "merge", w, wdecoded));
  }
  warmup(A);
  warmup(B);
  const liveBaselineA = A.exports.rt_live();
  const liveBaselineB = B.exports.rt_live();

  // A: two local writes via the shared `bump` export -- lands in the counter's
  // FIRST slot, exactly ch72's replicaA = gc 2 0.
  const sa = glue.call(A, "bump", glue.call(A, "bump", glue.call(A, "initGC")));

  // B: one local write via the SAME shared `bump` export (also slot one, count
  // 1) -- then re-homed into the counter's SECOND slot for gossip, mirroring
  // ch72's replicaB = gc 0 1. This is not a workaround: `bump` (like every
  // ABI-level primitive here) has no replica-id parameter, so slot assignment is
  // a protocol-level convention, and the wire codec (gcToBin/gcFromBin, the very
  // thing this scenario proves) is exactly the tool that carries a peer's state
  // across that boundary -- used here to carry B's own count into its own slot.
  const bLocal = glue.call(B, "bump", glue.call(B, "initGC"));
  const bCountPtr = glue.call(B, "value", bLocal); // consumes bLocal
  const bCount = glue.readNat(B, bCountPtr);
  glue.release(B, bCountPtr);
  const homedFrame = new Uint8Array([...encodeNatLE(0), ...encodeNatLE(bCount)]);
  const sb = glue.call(B, "gcFromBin", glue.mkBin(B, homedFrame));

  // Serialize each peer's state on its OWN host and copy the bytes out (readBin
  // copies into a fresh Uint8Array, decoupled from that instance's linear memory
  // -- the two instances share NOTHING but these plain bytes, the "channel").
  // Retain before the gcToBin call: `call` consumes its argument, but sa/sb are
  // each still needed below for the merge.
  glue.retain(A, sa);
  const binA = glue.call(A, "gcToBin", sa);
  const bytesA = glue.readBin(A, binA);
  glue.release(A, binA);

  glue.retain(B, sb);
  const binB = glue.call(B, "gcToBin", sb);
  const bytesB = glue.readBin(B, binB);
  glue.release(B, binB);

  // decodeAndMerge: mkBin the received bytes on the peer's OWN instance,
  // gcFromBin to decode, then merge into the peer's own state -- `merge` consumes
  // both its arguments (own and the freshly decoded remote), so neither needs a
  // separate release.
  function decodeAndMerge(h, own, bytes) {
    const remote = glue.call(h, "gcFromBin", glue.mkBin(h, bytes));
    return glue.call(h, "merge", own, remote);
  }

  // converge processes B first (with A's bytes), then A (with B's bytes);
  // converge-reversed swaps the order. The two instances share no state, so this
  // order is symbolic of the async two-tab case -- both orders converge to the
  // same value, the commutativity ch72's mergeComm proves.
  let mergedA, mergedB;
  if (scenario === "converge") {
    mergedB = decodeAndMerge(B, sb, bytesA);
    mergedA = decodeAndMerge(A, sa, bytesB);
  } else {
    mergedA = decodeAndMerge(A, sa, bytesB);
    mergedB = decodeAndMerge(B, sb, bytesA);
  }

  const va = glue.call(A, "value", mergedA);
  const vb = glue.call(B, "value", mergedB);
  console.log(glue.readNat(A, va)); // expect 3
  console.log(glue.readNat(B, vb)); // expect 3
  glue.release(A, va);
  glue.release(B, vb);

  // Balance: every pointer retained/allocated by this scenario beyond the
  // warm-up round has now been released; the delta from the post-warm-up
  // baseline must be exactly zero on both instances.
  console.log(A.exports.rt_live() - liveBaselineA);
  console.log(B.exports.rt_live() - liveBaselineB);
} else {
  console.error(`unknown scenario ${scenario}`);
  process.exit(2);
}
