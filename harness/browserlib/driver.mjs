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
//   smoke       initGC -> bump -> bump -> value via the plain Rune-named export ABI.
//               prints the counter value (expect 2), then rt_live() as a baseline
//               delta check for the Go side.
//   smoke-host  the same chain, but the final accessor is called through a HostName
//               override (glue's EXPORT_MAP keyed by the host-facing name, not the
//               Rune name) -- the end-to-end check for Task 2's deferred HostName
//               path. Requires the caller to build the module with an export whose
//               HostName is "getValue".
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
const h = await glue.load(buffer);

if (scenario === "smoke") {
  // init already ran inside glue.load; drive value(bump(bump(initGC))) via the ABI.
  const s0 = glue.call(h, "initGC");
  const s1 = glue.call(h, "bump", s0);
  const s2 = glue.call(h, "bump", s1);
  const v = glue.call(h, "value", s2);
  console.log(glue.readNat(h, v)); // expect 2
  console.log(h.exports.rt_live()); // baseline delta check for the Go side
} else if (scenario === "smoke-host") {
  // Same chain, but the final accessor is looked up by its HostName override
  // ("getValue"), not the bare Rune name -- exercises EXPORT_MAP's HostName branch.
  const s0 = glue.call(h, "initGC");
  const s1 = glue.call(h, "bump", s0);
  const s2 = glue.call(h, "bump", s1);
  const v = glue.call(h, "getValue", s2);
  console.log(glue.readNat(h, v)); // expect 2
  console.log(h.exports.rt_live());
} else {
  console.error(`unknown scenario ${scenario}`);
  process.exit(2);
}
