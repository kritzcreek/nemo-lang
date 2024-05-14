import "./reset.css";
import "./style.css";
import { setupEditor } from "./editor.ts";
import init from "../wasm-lib/wasm_lib.js";

async function main() {
  await init();
  let initialExample;
  if (window.location.hash.startsWith("#")) {
    initialExample = window.location.hash.slice(1);
  }
  setupEditor(initialExample ?? "");
}

main();
