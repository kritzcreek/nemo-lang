import * as fs from "node:fs/promises";
import { argv } from "node:process";

let instantiated;

let imports = {
  env: {
    log: (arg) => console.log(arg),
    print_char: (cp) => process.stdout.write(String.fromCodePoint(cp)),
    random: () => Math.random(),
  },
};

async function main() {
  if (argv.length < 3) {
    console.error("Usage: node run-wasm.mjs <file.wasm>");
    process.exit(1);
  }
  const file = argv[2];
  const bytes = new Uint8Array(await fs.readFile(file));
  instantiated = await WebAssembly.instantiate(bytes, imports);
  console.log(instantiated.instance.exports.main());
}

main();
