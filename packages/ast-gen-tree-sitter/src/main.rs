use serde::{Deserialize, Serialize};
use serde_json;
use std::fmt::Write;
use std::{
    collections::{BTreeMap, HashMap},
    path::Path,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum SymbolType {
    External,
    End,
    EndOfNonTerminalExtra,
    Terminal,
    NonTerminal,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Associativity {
    Left,
    Right,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Alias {
    pub value: String,
    pub is_named: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub enum Precedence {
    #[default]
    None,
    Integer(i32),
    Name(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol {
    pub kind: SymbolType,
    pub index: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ChildType {
    Normal(Symbol),
    Aliased(Alias),
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct FieldInfo {
    pub quantity: ChildQuantity,
    pub types: Vec<ChildType>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct VariableInfo {
    pub fields: HashMap<String, FieldInfo>,
    pub children: FieldInfo,
    pub children_without_fields: FieldInfo,
    pub has_multi_step_production: bool,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Default, PartialOrd, Ord)]
pub struct NodeInfoJSON {
    #[serde(rename = "type")]
    kind: String,
    named: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    fields: Option<BTreeMap<String, FieldInfoJSON>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    children: Option<FieldInfoJSON>,
    #[serde(skip_serializing_if = "Option::is_none")]
    subtypes: Option<Vec<NodeTypeJSON>>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeTypeJSON {
    #[serde(rename = "type")]
    kind: String,
    named: bool,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct FieldInfoJSON {
    multiple: bool,
    required: bool,
    types: Vec<NodeTypeJSON>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ChildQuantity {
    exists: bool,
    required: bool,
    multiple: bool,
}

impl Default for FieldInfoJSON {
    fn default() -> Self {
        Self {
            multiple: false,
            required: true,
            types: Vec::new(),
        }
    }
}

impl Default for ChildQuantity {
    fn default() -> Self {
        Self::one()
    }
}

impl ChildQuantity {
    #[must_use]
    const fn one() -> Self {
        Self {
            exists: true,
            required: true,
            multiple: false,
        }
    }
}

fn read_node_infos(path: &Path) -> Vec<NodeInfoJSON> {
    let text = std::fs::read_to_string(path).unwrap();
    serde_json::from_str(&text).unwrap()
}

fn to_camel_case(s: &str) -> String {
    let mut result = String::new();
    let mut capitalize = true;
    for c in s.chars() {
        if c == '_' {
            capitalize = true;
        } else if capitalize {
            result.push(c.to_ascii_uppercase());
            capitalize = false;
        } else {
            result.push(c);
        }
    }
    result
}

fn gen_rust_type(node_info: &NodeInfoJSON) -> String {
    if let Some(subtypes) = &node_info.subtypes {
        gen_enum_node(node_info, subtypes)
    } else {
        gen_struct_node(node_info)
    }
}

fn gen_enum_node(node_info: &NodeInfoJSON, subtypes: &Vec<NodeTypeJSON>) -> String {
    let ty_name = to_camel_case(&node_info.kind);
    let mut source_buf = String::new();
    writeln!(
        source_buf,
        "#[derive(Clone, Debug)]
    enum {}Node<'a> {{",
        ty_name
    )
    .unwrap();
    for subtype in subtypes {
        let subtype_name = to_camel_case(&subtype.kind);
        writeln!(source_buf, "  {}({}Node<'a>),", subtype_name, subtype_name).unwrap();
    }
    writeln!(source_buf, "}}").unwrap();

    writeln!(
        source_buf,
        "
  impl<'a> {}Node<'a> {{
    pub fn can_cast(node: &Node<'a>) -> bool {{",
        ty_name
    )
    .unwrap();

    writeln!(
        source_buf,
        "{}Node::can_cast(node)",
        to_camel_case(&subtypes[0].kind)
    )
    .unwrap();
    for subtype in subtypes.iter().skip(1) {
        write!(
            source_buf,
            " || {}Node::can_cast(node)",
            to_camel_case(&subtype.kind)
        )
        .unwrap();
    }

    writeln!(
        source_buf,
        "
    }}

    pub fn cast(node: &Node<'a>) -> Option<Self> {{"
    )
    .unwrap();

    write!(
        source_buf,
        "
    if let Some(casted_node) = {name}Node::cast(node) {{
        Some(Self::{name}(casted_node))
    }} else ",
        name = to_camel_case(&subtypes[0].kind),
    )
    .unwrap();

    for subtype in subtypes.iter().skip(1) {
        write!(
            source_buf,
            "if let Some(casted_node) = {name}Node::cast(node) {{
        Some(Self::{name}(casted_node))
    }} else ",
            name = to_camel_case(&subtype.kind)
        )
        .unwrap();
    }
    writeln!(
        source_buf,
        "{{ None }}
    }}
}}
"
    )
    .unwrap();

    source_buf
}

fn gen_struct_node(node_info: &NodeInfoJSON) -> String {
    let ty_name = to_camel_case(&node_info.kind);
    let mut source_buf = String::new();

    writeln!(
        source_buf,
        "#[derive(Clone, Copy, Debug)]
struct {}Node<'a>(Node<'a>);",
        ty_name
    )
    .unwrap();

    writeln!(
        source_buf,
        "
  impl<'a> {}Node<'a> {{
    pub fn can_cast(node: &Node<'a>) -> bool {{
        node.kind() == \"{}\"
    }}

    pub fn cast(node: &Node<'a>) -> Option<Self> {{
        if Self::can_cast(node) {{
            Some(Self(*node))
        }} else {{
            None
        }}
    }}
}}
",
        ty_name, node_info.kind
    )
    .unwrap();
    source_buf
}

fn main() {
    let input = Path::new("./packages/tree-sitter-nemo/src/node-types.json");
    let output = Path::new("./packages/nemo-frontend/src/ast.rs");
    let node_infos = read_node_infos(input);
    let mut source_buf = String::new();
    writeln!(source_buf, "// THIS FILE IS GENERATED").unwrap();
    write!(source_buf, "use tree_sitter::Node;\n\n").unwrap();

    for node_info in node_infos {
        if node_info.named {
            writeln!(source_buf, "{}", gen_rust_type(&node_info)).unwrap()
        }
    }

    std::fs::write(output, source_buf).unwrap()
}
