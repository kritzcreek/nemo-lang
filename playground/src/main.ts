import "./reset.css";
import "./style.css";
import { setupEditor } from "./editor.ts";
import init from "../wasm-lib/wasm_lib.js";

async function main() {
  await init();
  setupEditor();
}

main();
