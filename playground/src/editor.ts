import "./style.css";

import { html, render } from "lit-html";
import { examples } from "./examples";
import { EditorView, minimalSetup } from "codemirror";
import { closeBrackets } from "@codemirror/autocomplete";
import {
  ViewPlugin,
  DecorationSet,
  Decoration,
  ViewUpdate,
  lineNumbers,
  showPanel,
  Panel,
  keymap,
} from "@codemirror/view";
import { linter, Diagnostic } from "@codemirror/lint";
import {
  EditorState,
  Extension,
  RangeSetBuilder,
  StateField,
  Transaction,
} from "@codemirror/state";
import * as compiler from "../wasm-lib/wasm_lib.js";
import { clearConsoleBuffer, getConsoleBuffer, getWasmImports } from "./wasm_imports.js";
import { indentWithTab } from "@codemirror/commands";

const base_theme = EditorView.baseTheme({
  ".cm-literal": { color: "#3548cf" },
  ".cm-keyword": { color: "#5317ac" },
  ".cm-property": { color: "#00538b" },
  ".cm-type": { color: "#721045" },
  ".cm-local": { color: "black" },
  ".cm-global": { fontStyle: "italic" },
  ".cm-func": { color: "#00538b" },
  ".cm-comment": { color: "#999999" },
});

const literal_decoration = Decoration.mark({
  class: "cm-literal",
});
const keyword_decoration = Decoration.mark({
  class: "cm-keyword",
});
const property_decoration = Decoration.mark({
  class: "cm-property",
});
const type_decoration = Decoration.mark({
  class: "cm-type",
});
const func_decoration = Decoration.mark({
  class: "cm-func",
});
const local_decoration = Decoration.mark({
  class: "cm-local",
});
const global_decoration = Decoration.mark({
  class: "cm-global",
});
const comment_decoration = Decoration.mark({
  class: "cm-comment",
});

function highlight_view(view: EditorView) {
  let builder = new RangeSetBuilder<Decoration>();
  let result = view.state.field(compile_result);
  for (const hl of result.highlights) {
    let decoration: Decoration | undefined;
    switch (hl.kind) {
      case "Keyword":
        decoration = keyword_decoration;
        break;
      case "Literal":
        decoration = literal_decoration;
        break;
      case "Function":
        decoration = func_decoration;
        break;
      case "Property":
        decoration = property_decoration;
        break;
      case "Type":
        decoration = type_decoration;
        break;
      case "Local":
        decoration = local_decoration;
        break;
      case "Global":
        decoration = global_decoration;
        break;
      case "Comment":
        decoration = comment_decoration;
        break;
    }
    decoration &&
      builder.add(hl.start, hl.end, decoration);
  }
  return builder.finish();
}

const highlight_plugin = ViewPlugin.fromClass(
  class {
    decorations: DecorationSet;

    constructor(view: EditorView) {
      this.decorations = highlight_view(view);
    }

    update(update: ViewUpdate) {
      if (update.docChanged) this.decorations = highlight_view(update.view);
    }
  },
  {
    decorations: (v) => v.decorations,
  }
);

function nemo_highlighter(): Extension {
  return [base_theme, highlight_plugin];
}

const nemoLinter = linter((view) => {
  let diagnostics: Diagnostic[] = [];
  let result = view.state.field(compile_result);
  for (const error of result.diagnostics) {
    diagnostics.push({
      from: error.start,
      to: error.end,
      severity: "error",
      message: error.message,
    });
  }
  return diagnostics;
}, { delay: 0});

type CompileState = {
  diagnostics: compiler.Diagnostic[];
  highlights: compiler.Highlight[];
  instance: WebAssembly.Instance | undefined;
};

function compile(input: string, imports: WebAssembly.Imports): CompileState {
  const result = compiler.compile(input);
  const diagnostics = result.errors;
  const highlights = result.highlights;
  let instance;
  if (diagnostics.length === 0) {
    try {
      document.getElementById("output-wast")!.textContent = result.wast;
      // Using synchronous module instantiation
      // (not recommended, but makes working with the Codemirror API much simpler)
      const mod = new WebAssembly.Module(result.wasm);
      instance = new WebAssembly.Instance(mod, imports);
    } catch (e) {
      console.error(e);
    }
  }
  return { diagnostics, highlights, instance };
}

const compile_result = StateField.define<CompileState>({
  create: function (state: EditorState): CompileState {
    return compile(state.doc.toString(), getWasmImports());
  },
  update: function (
    prev: CompileState,
    transaction: Transaction,
  ): CompileState {
    return transaction.docChanged
      ? compile(transaction.newDoc.toString(), getWasmImports())
      : prev;
  },
});

function runConsoleApplication(instance: WebAssembly.Instance) {
  const main = instance.exports.main as () => void;
  clearConsoleBuffer();
  showOutput("console");
  const result = main();
  const output = getConsoleBuffer();
  const output_console = html`
    <h3>Return value</h3>
    <pre>${result}</pre>
    <h3>Console output</h3>
    <pre>${output}</pre>
    `;
  render(output_console, document.getElementById("output-console")! as HTMLElement)
}

function pick_example(view: EditorView, name: string) {
  view.dispatch({
    changes: { from: 0, to: view.state.doc.length, insert: examples[name] },
  });
}

function actions_panel(view: EditorView): Panel {
  function render_buttons(
    element: HTMLElement,
    instance: WebAssembly.Instance | undefined,
  ) {
    let can_run = instance != null && instance.exports.main != null;
    let can_render = instance != null && instance.exports.tick != null;
    let example_picker = html`
      <select id="example-picker" @change=${(e : Event) => pick_example(view, (e.target as HTMLInputElement).value)}>
        <option value="">Pick an example...</option>
        ${Object.keys(examples).map((name) => html`<option value=${name}>${name}</option>`)}
      </select>
    `;
    let button_bar = html`
      <h3 class="header">Nemo playground</h3>
      <div id="button-bar">
        <button
          id="runBtn"
          ?disabled=${!can_run}
          @click=${() => runConsoleApplication(instance!)}
        >
          Run
        </button>
        <button
          id="renderButn"
          ?disabled=${!can_render}
          @click=${() => start_render(view)}
        >
          Render
        </button>
      </div>
      ${example_picker}
    `;
    render(button_bar, element);
  }
  const dom = document.createElement("div");
  dom.id = "actions-panel";
  render_buttons(dom, view.state.field(compile_result).instance);
  return {
    dom,
    update(view_update) {
      if (view_update.docChanged) {
        const result = view_update.state.field(compile_result);
        render_buttons(dom, result.instance);
      }
    },
    top: true,
  };
}

function start_render(editorView: EditorView) {
  showOutput("canvas");
  clearConsoleBuffer();
  let previousTimeStamp: number | undefined;
  function render_canvas(timeStamp: number) {
    const elapsed = timeStamp - (previousTimeStamp ?? timeStamp);
    previousTimeStamp = timeStamp;
    let tick = editorView.state.field(compile_result).instance?.exports
      .tick as any;
    if (tick) {
      tick(elapsed);
      const output = getConsoleBuffer();
      const output_console = html`
        <h3>Console output</h3>
        <pre>${output}</pre>
        `;
      render(output_console, document.getElementById("output-console")! as HTMLElement)
      requestAnimationFrame(render_canvas);
    }
  }
  requestAnimationFrame(render_canvas);
}

type Output = { id: string, pane: HTMLElement, button: HTMLButtonElement };
let outputs: Output[] = [];

function showOutput(id: string) {
  for (const o of outputs) {
    o.button.classList.remove("active");
    o.pane.classList.add('hidden');
  }
  const o = outputs.find(o => o.pane.id === `output-${id}`);
  if (o) {
    o.button.classList.add("active")
    o.pane.classList.remove("hidden");
  }
}

function setupOutputs() {
  const mk_output = (id: string) => {
    return {
      id,
      pane: document.getElementById(`output-${id}`)!,
      button: document.getElementById(`output-${id}-btn`)! as HTMLButtonElement,
    };
  };
  outputs = ["console", "canvas", "wast"].map(mk_output);

  for (const o of outputs) {
    o.button.onclick = () => {
      showOutput(o.id);
    };
  }
}

export function setupEditor() {
  setupOutputs();
  new EditorView({
    doc: examples.bouncy_shapes,
    extensions: [
      minimalSetup,
      lineNumbers(),
      keymap.of([indentWithTab]),
      compile_result,
      showPanel.of(actions_panel),
      nemo_highlighter(),
      closeBrackets(),
      nemoLinter,
    ],
    parent: document.getElementById("editor-container")!,
  });
}
