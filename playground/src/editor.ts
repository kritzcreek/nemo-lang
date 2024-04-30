import "./style.css";

import { EditorView, minimalSetup } from "codemirror";
import { autocompletion, closeBrackets } from "@codemirror/autocomplete";
import {
  ViewPlugin,
  DecorationSet,
  Decoration,
  ViewUpdate,
  lineNumbers,
} from "@codemirror/view";
import { linter, Diagnostic } from "@codemirror/lint";
import { Extension, RangeSetBuilder } from "@codemirror/state";
import * as compiler from "../wasm-lib/wasm_lib.js";

const base_theme = EditorView.baseTheme({
  ".cm-literal": { color: "#3548cf" },
  ".cm-keyword": { color: "#5317ac" },
  ".cm-column": { color: "#00538b" },
  ".cm-table": { color: "#721045" },
  ".cm-func": { fontStyle: "italic" },
  ".cm-recover": {
    background: "rgba(255, 220, 220, 0.5)",
    color: "rgba(0, 0, 0, 0.5)",
  },
});

const literal_decoration = Decoration.mark({
  class: "cm-literal",
});
const keyword_decoration = Decoration.mark({
  class: "cm-keyword",
});
const column_decoration = Decoration.mark({
  class: "cm-column",
});
const table_decoration = Decoration.mark({
  class: "cm-table",
});
const func_decoration = Decoration.mark({
  class: "cm-func",
});
const recover_decoration = Decoration.mark({
  class: "cm-recover",
});

function highlight_view(view: EditorView) {
  let builder = new RangeSetBuilder<Decoration>();
  let input = view.state.doc.toString();
  let result = compiler.compile(input);
  for (const error of result.errors) {
    console.log(error.message);
  }
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
  let input = view.state.doc.toString();
  let result = compiler.compile(input);
  for (const error of result.errors) {
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
  return [base_theme, highlight_plugin];
}

const initial_code =
  JSON.parse(localStorage.getItem("code") ?? '""') ||
  "fn main() : i32 = {\n  20 * 2 + 2\n}";

export function setupEditor() {
  new EditorView({
    doc: initial_code,
    extensions: [
      minimalSetup,
      lineNumbers(),
      autocompletion({ override: [] }),
      highlight_nemo(),
      closeBrackets(),
      nemoLinter,
    ],
    parent: document.getElementById("editor")!,
  });
}
