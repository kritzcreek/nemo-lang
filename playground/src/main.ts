import "./style.css";
import { setupEditor } from "./editor.ts";
import init from "../wasm-lib/wasm_lib.js";

await init();

setupEditor();
