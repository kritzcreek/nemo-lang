// import { EditorView } from "codemirror";
// import * as compiler from "../wasm-lib/wasm_lib.js";
// import { EditorState, Extension, Text } from "@codemirror/state";

// function mainProgram() {
//   return localStorage.getItem("mainProgram") ?? "";
// }

// function canvasProgram() {
//   return localStorage.getItem("canvasProgram") ?? "";
// }

// const canvas = document.getElementById("canvas")! as HTMLCanvasElement;
// /**
//  * @type CanvasRenderingContext2D
//  */
// const ctx = canvas.getContext("2d")!;
// ctx.lineWidth = 3.0;

// function clear_canvas() {
//   ctx.clearRect(0, 0, canvas.width, canvas.height);
// }

// function begin_path() {
//   ctx.beginPath();
// }

// function move_to(x: number, y: number) {
//   ctx.moveTo(x, y);
// }

// function line_to(x: number, y: number) {
//   ctx.lineTo(x, y);
// }

// function close_path() {
//   ctx.closePath();
// }

// function stroke() {
//   ctx.stroke();
// }

// function set_stroke_color(r: number, g: number, b: number) {
//   ctx.strokeStyle = `rgb(${r}, ${g}, ${b})`;
// }

// let tick = (_: number) => {};
// let autoRecompile = true;
// let useCanvas = true;

// function stopRender() {
//   tick = (_: number) => {};
// }

// document.getElementById("stopBtn")!.onclick = function (e) {
//   e.preventDefault();
//   e.stopPropagation();

//   console.log("Stopping");
//   stopRender();
// };

// document.getElementById("startBtn")!.onclick = function (e) {
//   e.preventDefault();
//   e.stopPropagation();

//   console.log("Starting");
//   if (useCanvas) {
//     restartRender();
//   } else {
//     runStaticWasm();
//   }
// };

// document.getElementById("toggleAuto")!.onclick = function (e) {
//   e.preventDefault();
//   e.stopPropagation();

//   autoRecompile = !autoRecompile;
//   localStorage.setItem("autoRecompile", JSON.stringify(autoRecompile));
//   displayToggleAuto();
// };

// function displayToggleAuto() {
//   document.getElementById("toggleAuto")!.innerText = autoRecompile
//     ? "Recompile: On"
//     : "Recompile: Off";
// }

// document.getElementById("toggleCanvas")!.onclick = function (e) {
//   e.preventDefault();
//   e.stopPropagation();

//   useCanvas = !useCanvas;
//   localStorage.setItem("useCanvas", JSON.stringify(useCanvas));
//   if (useCanvas) {
//     setEditorContent(canvasProgram());
//   } else {
//     clear_canvas();
//     stopRender();
//     setEditorContent(mainProgram());
//   }
//   displayUseCanvas();
// };

// function displayUseCanvas() {
//   document.getElementById("toggleCanvas")!.innerText = useCanvas
//     ? "Use canvas: On"
//     : "Use canvas: Off";
// }

// function runCompiler(code: string): Uint8Array | null {
//   const result = compiler.compile(code);
//   if (result.errors.length == 0) {
//     return result.wasm;
//   } else {
//     return null;
//   }
// }

// async function instantiateWasm(
//   compiledWasm: Uint8Array,
//   imports: WebAssembly.Imports,
// ) {
//   try {
//     const inst = await WebAssembly.instantiate(compiledWasm, imports);
//     return inst.instance;
//   } catch (err) {
//     console.error("Failed to instantiate wasm with: " + err);
//   }
// }

// let previousTimeStamp: number | undefined;
// function render(timeStamp: number) {
//   const elapsed = timeStamp - (previousTimeStamp ?? timeStamp);
//   previousTimeStamp = timeStamp;
//   if (tick) {
//     tick(elapsed);
//     requestAnimationFrame(render);
//   }
// }

// async function restartRender() {
//   previousTimeStamp = undefined;
//   const compiled = runCompiler(getEditorContent());
//   if (compiled != null) {
//     const imports = {
//       env: {
//         clear_canvas,
//         begin_path,
//         move_to,
//         line_to,
//         arc: (x: number, y: number, a: number, b: number, c: number) =>
//           ctx.arc(x, y, a, b, c),
//         close_path,
//         set_stroke_color,
//         stroke,
//       },
//     };
//     instantiateWasm(compiled, imports)
//       .then((inst) => {
//         clear_canvas();
//         // @ts-ignore
//         tick = (elapsed) => inst.exports.tick(elapsed);
//       })
//       .then(() => {
//         requestAnimationFrame(render);
//       });
//   }
// }

// async function runStaticWasm() {
//   const compiled = runCompiler(getEditorContent());
//   if (compiled != null) {
//     instantiateWasm(compiled, {
//       env: {
//         log: (x: number) => {
//           console.log(x);
//           return x;
//         },
//       },
//     })
//       .then((inst) => {
//         // @ts-ignore
//         const result = inst.exports.main();
//         console.log(result);
//       })
//       .catch((err) => {
//         console.error("Failed to run wasm: " + err.toString());
//       });
//   }
// }

// function loadSettings() {
//   autoRecompile = JSON.parse(localStorage.getItem("autoRecompile") ?? "true");
//   useCanvas = JSON.parse(localStorage.getItem("useCanvas") ?? "true");
// }

// function setEditorContent(editor: EditorView, content: string) {
//   editor.setState(EditorState.create({ doc: content }));
// }

// async function watchEditor() {
//   editor.session.on("change", () => {
//     if (useCanvas) {
//       localStorage.setItem("canvasProgram", getEditorContent());
//       if (autoRecompile) {
//         restartRender();
//       }
//     } else {
//       localStorage.setItem("mainProgram", getEditorContent());
//       if (autoRecompile) {
//         runStaticWasm();
//       }
//     }
//   });
// }

// export function setupPlayground(editor: EditorView): Extension {
//   loadSettings();
//   setEditorContent(editor, useCanvas ? canvasProgram() : mainProgram());
//   displayToggleAuto();
//   displayUseCanvas();

//   if (useCanvas) {
//     restartRender();
//   } else {
//     runStaticWasm();
//   }

//   EditorView.updateListener.of((update) => {});

//   watchEditor();
// }
