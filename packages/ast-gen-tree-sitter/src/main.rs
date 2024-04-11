use serde::{Deserialize, Serialize};
use serde_json;
use std::fmt::Write;
use std::{collections::BTreeMap, path::Path};

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
    pub enum {}Node<'a> {{",
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
    pub fn can_cast(node: Node<'a>) -> bool {{",
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

    pub fn cast(node: Node<'a>) -> Option<Self> {{"
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
pub struct {}Node<'a>(pub Node<'a>);",
        ty_name
    )
    .unwrap();

    writeln!(
        source_buf,
        "
  impl<'a> {}Node<'a> {{
    pub fn can_cast(node: Node<'a>) -> bool {{
        node.kind() == \"{}\"
    }}

    pub fn cast(node: Node<'a>) -> Option<Self> {{
        if Self::can_cast(node) {{
            Some(Self(node))
        }} else {{
            None
        }}
    }}
}}
",
        ty_name, node_info.kind
    )
    .unwrap();

    writeln!(
        source_buf,
        "
  impl<'a> {ty_name}Node<'a> {{"
    )
    .unwrap();
    // Generate field accessors
    if let Some(fields) = &node_info.fields {
        for (field_name, field_info) in fields.iter() {
            if field_info.types.len() > 1 {
                // Best thing I can do here for now is to return an accessor for Node<'a>
                if field_info.multiple {
                    writeln!(
                        source_buf,
                        "
            pub fn {field_name}s(&self) -> Vec<Node<'a>> {{
                let mut cursor = self.0.walk();
                self.0.children_by_field_name(\"{field_name}\", &mut cursor).collect()
            }}
            "
                    )
                    .unwrap();
                } else {
                    let func_name = match field_name.as_ref() {
                        "type" => "type_",
                        "struct" => "struct_",
                        "enum" => "enum_",
                        "if" => "if_",
                        "else" => "else_",
                        "continue" => "continue_",
                        "break" => "break_",
                        f => f,
                    };

                    writeln!(
                        source_buf,
                        "
            pub fn {func_name}(&self) -> Option<Node<'a>> {{
                self.0.child_by_field_name(\"{field_name}\")
            }}
            "
                    )
                    .unwrap();
                }
                continue;
            }
            let field_type = &field_info.types[0];

            if !field_type.named {
                eprintln!(
                    "Unnamed field types not supported yet: {} in {}",
                    field_type.kind, node_info.kind
                );
                continue;
            }
            let field_type_name = to_camel_case(&field_type.kind);

            if field_info.multiple {
                writeln!(
                    source_buf,
                    "
            pub fn {field_name}s(&self) -> Vec<{field_type_name}Node<'a>> {{
                let mut cursor = self.0.walk();
                self.0
                  .children_by_field_name(\"{field_name}\", &mut cursor)
                  .filter_map({field_type_name}Node::cast).collect()
            }}
            "
                )
                .unwrap();
                continue;
            }

            // TODO: Do we care about required?
            let func_name = match field_name.as_ref() {
                "type" => "type_",
                "struct" => "struct_",
                "enum" => "enum_",
                "if" => "if_",
                "else" => "else_",
                "continue" => "continue_",
                "break" => "break_",
                f => f,
            };

            writeln!(
                source_buf,
                "
            pub fn {func_name}(&self) -> Option<{field_type_name}Node<'a>> {{
                self.0.child_by_field_name(\"{field_name}\").and_then({field_type_name}Node::cast)
            }}
            "
            )
            .unwrap();
        }
    }

    if let Some(children) = &node_info.children {
        let child_types: Vec<_> = children.types.iter().filter(|t| t.named).collect();
        if child_types.is_empty() {
            // Do nothing
        } else if child_types.len() > 1 {
            eprintln!(
                "Multiple children not supported yet: {}, {children:?}",
                node_info.kind
            );
        } else if children.multiple {
            let type_name = children.types[0].kind.trim_start_matches("_");
            let field_type_name = to_camel_case(&type_name);
            writeln!(
                source_buf,
                "
            pub fn {type_name}s(&self) -> Vec<{field_type_name}Node<'a>> {{
                let mut cursor = self.0.walk();
                self.0
                  .children(&mut cursor)
                  .filter_map({field_type_name}Node::cast).collect()
            }}
            "
            )
            .unwrap();
        } else {
            let type_name = children.types[0].kind.trim_start_matches("_");
            let field_type_name = to_camel_case(&type_name);
            writeln!(
                source_buf,
                "
            pub fn {type_name}(&self) -> Option<{field_type_name}Node<'a>> {{
                let mut cursor = self.0.walk();
                let child = self.0
                  .children(&mut cursor)
                  .find_map({field_type_name}Node::cast);
                child
            }}
            "
            )
            .unwrap();
        }
    }

    writeln!(source_buf, "}}").unwrap();

    source_buf
}

fn main() {
    let input = Path::new("./packages/tree-sitter-nemo/src/node-types.json");
    let output = Path::new("./packages/nemo-frontend/src/ast.rs");
    let node_infos = read_node_infos(input);
    let mut source_buf = String::new();
    writeln!(source_buf, "// THIS FILE IS GENERATED").unwrap();
    writeln!(source_buf, "use tree_sitter::Node;\n").unwrap();

    for node_info in node_infos {
        if node_info.named {
            writeln!(source_buf, "{}", gen_rust_type(&node_info)).unwrap()
        }
    }

    std::fs::write(output, source_buf).unwrap()
}
