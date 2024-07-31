import * as fs from "node:fs/promises";
import { argv } from "node:process";

let instantiated;

let imports = {
  env: {
    log: (arg) => console.log(arg),
    print: (s) => {
      let len = instantiated.instance.exports.byte_size(s);
      let bytes = new Uint8Array(len);
      for (let i = 0; i < len; i++) {
        bytes[i] = instantiated.instance.exports.byte(s, i);
      }
      console.log(new TextDecoder("utf-8").decode(bytes));
    },
    random_float: () => Math.random(),
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
