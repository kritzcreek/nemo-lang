import "./style.css";

import { html, render } from "lit-html";
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
  panels,
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

// Hacky
let imports: WebAssembly.Imports = undefined as any;

function highlight_view(view: EditorView) {
  let builder = new RangeSetBuilder<Decoration>();
  // let { root } = run_parse(input, 10000);
  // if (!root) {
  //   return builder.finish();
  // }
  // let highlights = highlight(root);
  // for (const { range, token_type } of highlights) {
  //   let decoration: Decoration;
  //   switch (token_type) {
  //     case Highlight.Literal:
  //       decoration = literal_decoration;
  //       break;
  //     case Highlight.Keyword:
  //       decoration = keyword_decoration;
  //       break;
  //     case Highlight.Column:
  //       decoration = column_decoration;
  //       break;
  //     case Highlight.Table:
  //       decoration = table_decoration;
  //       break;
  //     case Highlight.Func:
  //       decoration = func_decoration;
  //       break;
  //     case Highlight.Recover:
  //       decoration = recover_decoration;
  //       break;
  //   }
  //   decoration &&
  //     builder.add(range.offset, range.offset + range.length, decoration);
  // }
  return builder.finish();
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
});

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
  },
);

function highlight_nemo(): Extension {
  return [highlight_plugin];
}

type CompileState = {
  diagnostics: compiler.Diagnostic[];
  instance: WebAssembly.Instance | undefined;
};

function compile(input: string, imports: WebAssembly.Imports): CompileState {
  const result = compiler.compile(input);
  const diagnostics = result.errors;
  let instance;
  if (diagnostics.length === 0) {
    try {
      // Using synchronous module instantiation
      // (not recommended, but makes working with the Codemirror API much simpler)
      const mod = new WebAssembly.Module(result.wasm);
      instance = new WebAssembly.Instance(mod, imports);
    } catch (e) {
      console.error(e);
    }
  }
  return { diagnostics, instance };
}

const compile_result = StateField.define<CompileState>({
  create: function (state: EditorState): CompileState {
    return compile(state.doc.toString(), imports);
  },
  update: function (
    prev: CompileState,
    transaction: Transaction,
  ): CompileState {
    return transaction.docChanged
      ? compile(transaction.newDoc.toString(), imports)
      : prev;
  },
});

function actions_panel(_view: EditorView): Panel {
  function render_buttons(
    element: HTMLElement,
    instance: WebAssembly.Instance | undefined,
  ) {
    let active = instance != null;
    let button_bar = html`<button
      id="runBtn"
      ?disabled=${!active}
      @click=${() => console.log((instance?.exports as any).main())}
    >
      Run
    </button>`;
    render(button_bar, element);
  }
  const dom = document.createElement("div");
  render_buttons(dom, undefined);
  return {
    dom,
    update(view_update) {
      if (view_update.docChanged) {
        const result = view_update.state.field(compile_result);
        render_buttons(dom, result.instance);
      }
    },
  };
}

function canvas_panel(view: EditorView): Panel {
  function render_canvas(element: HTMLElement) {
    const canvas = html`<canvas id="canvas" width="500" height="500"></canvas>`;
    render(canvas, element);
  }

  let dom = document.createElement("div");
  render_canvas(dom);
  return {
    dom,
    async update(view_update) {},
    top: true,
  };
}

const actions = showPanel.of(actions_panel);
const canvas = showPanel.of(canvas_panel);

const initial_code =
  JSON.parse(localStorage.getItem("code") ?? '""') ||
  "fn main() : i32 = {\n  20 * 2 + 2\n}";

export function setupEditor(imports: WebAssembly.Imports) {
  imports = imports;
  new EditorView({
    doc: initial_code,
    extensions: [
      minimalSetup,
      panels({
        topContainer: document.getElementById("canvas-container") ?? undefined,
      }),
      lineNumbers(),
      compile_result,
      canvas,
      actions,
      highlight_nemo(),
      closeBrackets(),
      nemoLinter,
    ],
    parent: document.getElementById("editor-container")!,
  });
}
