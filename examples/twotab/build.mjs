// build.mjs: rune build (library, wasm) then wabt-assemble WAT -> wasm.
// Usage: node build.mjs   (from examples/twotab/; rune binary from PATH, or
// `go run ./cmd/rune` against the repo root when RUNE_BIN is unset)
//
// `rune build` (cmd/rune/build.go) writes artifacts named after --module (via
// codegen/wasm_library.go's wasmLibraryBaseName), NOT after the source file's
// stem -- so --module counter is required to land counter.wat/counter.glue.js
// rather than the library backend's default module.wat/module.glue.js.
//
// wabt resolution: node's ESM loader resolves a bare specifier by walking up
// the directory tree from the IMPORTING FILE's own location (harness/browserlib/
// driver.mjs relies on exactly this to find harness/node_modules/wabt). This
// script lives under examples/twotab/, which is not under harness/, so a bare
// `import("wabt")` would fail; resolving the absolute path to
// harness/node_modules/wabt/index.js (bin/setup.sh section 7 installs it
// there, not vendored) is the simplest fix that needs no cwd assumption and
// no extra node_modules copy.
import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import path from "node:path";

const here = path.dirname(fileURLToPath(import.meta.url));
const repo = path.resolve(here, "../..");
const exportsList = ["initGC", "bump", "value", "merge", "gcToBin", "gcFromBin"];
const args = [
  "build", path.join(here, "counter.rune"),
  "--kind", "library",
  "--target", "wasm",
  "--module", "counter",
  "--out", here,
  ...exportsList.flatMap((e) => ["--export", e]),
];

const rune = process.env.RUNE_BIN || "go";
if (rune === "go") {
  execFileSync("go", ["run", "./cmd/rune", ...args], { cwd: repo, stdio: "inherit" });
} else {
  execFileSync(rune, args, { stdio: "inherit" });
}

const wabt = await (await import(path.join(repo, "harness", "node_modules", "wabt", "index.js"))).default();
const wat = readFileSync(path.join(here, "counter.wat"), "utf8");
const mod = wabt.parseWat("counter.wat", wat);
writeFileSync(path.join(here, "counter.wasm"), Buffer.from(mod.toBinary({}).buffer));
console.log("built counter.wasm + counter.glue.js");
