// smoke.mjs: the built artifacts load in node exactly as a browser would.
// Usage: node smoke.mjs   (from examples/twotab/, after node build.mjs)
import { readFileSync } from "node:fs";
import { fileURLToPath, pathToFileURL } from "node:url";
import path from "node:path";

const here = path.dirname(fileURLToPath(import.meta.url));
const glue = await import(pathToFileURL(path.join(here, "counter.glue.js")));
const h = await glue.load(readFileSync(path.join(here, "counter.wasm")));
const s0 = glue.call(h, "initGC");
const v = glue.call(h, "value", s0);
const n = glue.readNat(h, v);
if (n !== 0) {
  console.error(`value(initGC) = ${n}, want 0`);
  process.exit(1);
}
console.log("smoke ok");
