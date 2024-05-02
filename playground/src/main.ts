import "./reset.css";
import "./style.css";
import { setupEditor } from "./editor.ts";
import { setupWasmImports } from "./wasm_imports.ts";
import init from "../wasm-lib/wasm_lib.js";

async function main() {
  await init();
  const imports = setupWasmImports();
  setupEditor(imports);
}

main();
