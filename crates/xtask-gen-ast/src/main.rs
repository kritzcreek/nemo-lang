mod ast_src;

use crate::ast_src::{AstEnumSrc, AstNodeSrc, AstSrc, Cardinality, Field};
use anyhow::Result;
use quote::{format_ident, quote};
use std::{
    env,
    fs::File,
    io::Write,
    path::{Path, PathBuf},
    str::FromStr,
};
use ungrammar::{Grammar, Rule};

pub fn project_root() -> PathBuf {
    Path::new(
        &env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned()),
    )
    .ancestors()
    .nth(2)
    .unwrap()
    .to_path_buf()
}

/// Copied from the Rust compiler: https://github.com/rust-lang/rust/pull/62948/
/// Replaces `\r\n` with `\n` in-place in `src`.
///
/// Returns error if there's a lone `\r` in the string
fn normalize_newlines(src: &mut String) {
    if !src.as_bytes().contains(&b'\r') {
        return;
    }

    // We replace `\r\n` with `\n` in-place, which doesn't break utf-8 encoding.
    // While we *can* call `as_mut_vec` and do surgery on the live string
    // directly, let's rather steal the contents of `src`. This makes the code
    // safe even if a panic occurs.

    let mut buf = std::mem::take(src).into_bytes();
    let mut gap_len = 0;
    let mut tail = buf.as_mut_slice();
    loop {
        let idx = match find_crlf(&tail[gap_len..]) {
            None => tail.len(),
            Some(idx) => idx + gap_len,
        };
        tail.copy_within(gap_len..idx, 0);
        tail = &mut tail[idx - gap_len..];
        if tail.len() == gap_len {
            break;
        }
        gap_len += 1;
    }

    // Account for removed `\r`.
    // After `set_len`, `buf` is guaranteed to contain utf-8 again.
    let new_len = buf.len() - gap_len;
    unsafe {
        buf.set_len(new_len);
        *src = String::from_utf8_unchecked(buf);
    }

    fn find_crlf(src: &[u8]) -> Option<usize> {
        let mut search_idx = 0;
        while let Some(idx) = find_cr(&src[search_idx..]) {
            if src[search_idx..].get(idx + 1) != Some(&b'\n') {
                search_idx += idx + 1;
                continue;
            }
            return Some(search_idx + idx);
        }
        None
    }

    fn find_cr(src: &[u8]) -> Option<usize> {
        src.iter().position(|&b| b == b'\r')
    }
}

pub fn run() -> Result<()> {
    generate_syntax()
}

fn generate_nodes(grammar: &AstSrc) -> Result<String> {
    let (node_defs, node_boilerplate_impls): (Vec<_>, Vec<_>) = grammar
        .nodes
        .iter()
        .map(|node| {
            let name = format_ident!("{}", node.name);
            let kind = format_ident!("{}", node.name);

            // TODO: We're not using these yet, but eventually we can
            // use these to increase ergonomics around nodes that hold
            // names
            let traits = node.traits.iter().map(|trait_name| {
                let trait_name = format_ident!("{}", trait_name);
                quote!(impl ast::#trait_name for #name {})
            });

            let methods = node.fields.iter().map(|field| {
                let method_name = field.method_name();
                let ty = field.ty();

                if field.is_many() {
                    quote! {
                        pub fn #method_name(&self) -> AstChildren<#ty> {
                            support::children(&self.syntax)
                        }
                    }
                } else if let Some(token_kind) = field.token_kind() {
                    quote! {
                        pub fn #method_name(&self) -> Option<#ty> {
                            support::token(&self.syntax, #token_kind)
                        }
                    }
                } else {
                    quote! {
                        pub fn #method_name(&self) -> Option<#ty> {
                            support::child(&self.syntax)
                        }
                    }
                }
            });
            (
                quote! {
                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub struct #name {
                        pub(crate) syntax: SyntaxNode,
                    }

                    #(#traits)*

                    impl #name {
                        #(#methods)*
                    }
                },
                quote! {
                    impl AstNode for #name {
                        fn can_cast(kind: SyntaxKind) -> bool {
                            kind == #kind
                        }
                        fn cast(syntax: SyntaxNode) -> Option<Self> {
                            if Self::can_cast(syntax.kind()) { Some(Self { syntax }) } else { None }
                        }
                        fn syntax(&self) -> &SyntaxNode { &self.syntax }
                    }
                },
            )
        })
        .unzip();

    let (enum_defs, enum_boilerplate_impls): (Vec<_>, Vec<_>) = grammar
        .enums
        .iter()
        .map(|en| {
            let variants: Vec<_> = en
                .variants
                .iter()
                .map(|var| format_ident!("{}", var))
                .collect();
            let name = format_ident!("{}", en.name);
            let kinds: Vec<_> = variants
                .iter()
                .map(|name| format_ident!("{}", name.to_string()))
                .collect();
            let traits = en.traits.iter().map(|trait_name| {
                let trait_name = format_ident!("{}", trait_name);
                quote!(impl ast::#trait_name for #name {})
            });

            let ast_node = if en.name == "Stmt" {
                quote! {}
            } else {
                quote! {
                    impl AstNode for #name {
                        fn can_cast(kind: SyntaxKind) -> bool {
                            match kind {
                                #(#kinds)|* => true,
                                _ => false,
                            }
                        }
                        fn cast(syntax: SyntaxNode) -> Option<Self> {
                            let res = match syntax.kind() {
                                #(
                                #kinds => #name::#variants(#variants { syntax }),
                                )*
                                _ => return None,
                            };
                            Some(res)
                        }
                        fn syntax(&self) -> &SyntaxNode {
                            match self {
                                #(
                                #name::#variants(it) => &it.syntax,
                                )*
                            }
                        }
                    }
                }
            };

            (
                quote! {
                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub enum #name {
                        #(#variants(#variants),)*
                    }

                    #(#traits)*
                },
                quote! {
                    #(
                        impl From<#variants> for #name {
                            fn from(node: #variants) -> #name {
                                #name::#variants(node)
                            }
                        }
                    )*
                    #ast_node
                },
            )
        })
        .unzip();

    let enum_names = grammar.enums.iter().map(|it| &it.name);
    let node_names = grammar.nodes.iter().map(|it| &it.name);

    let display_impls = enum_names
        .chain(node_names.clone())
        .map(|it| format_ident!("{}", it))
        .map(|name| {
            quote! {
                impl std::fmt::Display for #name {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        std::fmt::Display::fmt(self.syntax(), f)
                    }
                }
            }
        });

    let ast = quote! {
        use super::{
            SyntaxNode, SyntaxToken, SyntaxKind::{self, *},
            ast::{AstNode, AstChildren, support},
        };
        use crate::T;

        #(#node_defs)*
        #(#enum_defs)*
        #(#node_boilerplate_impls)*
        #(#enum_boilerplate_impls)*
        #(#display_impls)*
    };

    let ast = ast.to_string().replace("T ! [", "T![");

    Ok(ast)
}

fn generate_syntax() -> Result<()> {
    let mut grammar_str = include_str!("../../frontend/src/syntax/nodes.ungram").to_string();
    normalize_newlines(&mut grammar_str);
    let grammar = Grammar::from_str(&grammar_str).unwrap();
    println!("{:#?}", grammar);

    let ast = lower(&grammar);

    println!("{:#?}", ast);

    let src = generate_nodes(&ast)?;

    let nodes_path = project_root()
        .join("crates")
        .join("frontend")
        .join("src")
        .join("syntax")
        .join("nodes.rs");

    let mut file: File = File::create(nodes_path)?;
    write!(file, "{}", src)?;
    Ok(())
}

fn lower(grammar: &Grammar) -> AstSrc {
    let mut res = AstSrc::default();
    let nodes = grammar.iter().collect::<Vec<_>>();
    for &node in &nodes {
        let name = grammar[node].name.clone();
        let rule = &grammar[node].rule;
        match lower_enum(grammar, rule) {
            Some(variants) => {
                let enum_src = AstEnumSrc {
                    name,
                    traits: Vec::new(),
                    variants,
                };
                res.enums.push(enum_src);
            }
            None => {
                let mut fields = Vec::new();
                lower_rule(&mut fields, grammar, None, rule);
                res.nodes.push(AstNodeSrc {
                    name,
                    traits: Vec::new(),
                    fields,
                });
            }
        }
    }
    res
}

fn lower_enum(grammar: &Grammar, rule: &Rule) -> Option<Vec<String>> {
    let alternatives = match rule {
        Rule::Alt(it) => it,
        _ => return None,
    };
    let mut variants = Vec::new();
    for alternative in alternatives {
        match alternative {
            Rule::Node(it) => variants.push(grammar[*it].name.clone()),
            // TODO: What's going on here?
            Rule::Token(it) if grammar[*it].name == ";" => (),
            _ => return None,
        }
    }
    Some(variants)
}

fn lower_rule(acc: &mut Vec<Field>, grammar: &Grammar, label: Option<&String>, rule: &Rule) {
    if lower_comma_list(acc, grammar, label, rule) {
        return;
    }

    match rule {
        Rule::Node(node) => {
            let ty = grammar[*node].name.clone();
            let name = label.cloned().unwrap_or_else(|| to_lower_snake_case(&ty));
            let field = Field::Node {
                name,
                ty,
                cardinality: Cardinality::Optional,
            };
            acc.push(field);
        }
        Rule::Token(token) => {
            assert!(label.is_none());
            let mut name = grammar[*token].name.clone();
            if name != "int_number" && name != "string" {
                if "[]{}()".contains(&name) {
                    name = format!("'{}'", name);
                }
                let field = Field::Token(name);
                acc.push(field);
            }
        }
        Rule::Rep(inner) => {
            if let Rule::Node(node) = &**inner {
                let ty = grammar[*node].name.clone();
                let name = label
                    .cloned()
                    .unwrap_or_else(|| pluralize(&to_lower_snake_case(&ty)));
                let field = Field::Node {
                    name,
                    ty,
                    cardinality: Cardinality::Many,
                };
                acc.push(field);
                return;
            }
            todo!("{:?}", rule)
        }
        Rule::Labeled { label: l, rule } => {
            assert!(label.is_none());
            let manually_implemented = matches!(
                l.as_str(),
                "lhs"
                    | "rhs"
                    | "then_branch"
                    | "else_branch"
                    | "start"
                    | "end"
                    | "op"
                    | "index"
                    | "base"
                    | "value"
                    | "trait"
                    | "self_ty"
            );
            if manually_implemented {
                return;
            }
            lower_rule(acc, grammar, Some(l), rule);
        }
        Rule::Seq(rules) | Rule::Alt(rules) => {
            for rule in rules {
                lower_rule(acc, grammar, label, rule)
            }
        }
        Rule::Opt(rule) => lower_rule(acc, grammar, label, rule),
    }
}

// Matches rules of the shape:
// (T (',' T)*)
// or
// (T (';' T)*)
fn lower_comma_list(
    acc: &mut Vec<Field>,
    grammar: &Grammar,
    label: Option<&String>,
    rule: &Rule,
) -> bool {
    let rule = match rule {
        Rule::Seq(it) => it,
        _ => return false,
    };
    let (node, repeat) = match rule.as_slice() {
        [Rule::Node(node), Rule::Rep(repeat)] => (node, repeat),
        _ => return false,
    };
    let repeat = match &**repeat {
        Rule::Seq(it) => it,
        _ => return false,
    };
    match repeat.as_slice() {
        [Rule::Token(comma), Rule::Node(n)]
            if n == node && (grammar[*comma].name == "," || grammar[*comma].name == ";") => {}
        _ => return false,
    }
    let ty = grammar[*node].name.clone();
    let name = label
        .cloned()
        .unwrap_or_else(|| pluralize(&to_lower_snake_case(&ty)));
    let field = Field::Node {
        name,
        ty,
        cardinality: Cardinality::Many,
    };
    acc.push(field);
    true
}

fn to_lower_snake_case(s: &str) -> String {
    let mut buf = String::with_capacity(s.len());
    let mut prev = false;
    for c in s.chars() {
        if c.is_ascii_uppercase() && prev {
            buf.push('_')
        }
        prev = true;

        buf.push(c.to_ascii_lowercase());
    }
    buf
}

fn pluralize(s: &str) -> String {
    format!("{}s", s)
}

impl Field {
    fn is_many(&self) -> bool {
        matches!(
            self,
            Field::Node {
                cardinality: Cardinality::Many,
                ..
            }
        )
    }
    fn token_kind(&self) -> Option<proc_macro2::TokenStream> {
        match self {
            Field::Token(token) => {
                let token: proc_macro2::TokenStream = token.parse().unwrap();
                Some(quote! { T![#token] })
            }
            _ => None,
        }
    }
    fn ty(&self) -> proc_macro2::Ident {
        match self {
            Field::Token(_) => format_ident!("SyntaxToken"),
            Field::Node { ty, .. } => format_ident!("{}", ty),
        }
    }

    fn method_name(&self) -> proc_macro2::Ident {
        match self {
            Field::Token(name) => {
                let name = match name.as_str() {
                    ";" => "semicolon",
                    "->" => "arrow",
                    "=>" => "fat_arrow",
                    "'{'" => "l_brace",
                    "'}'" => "r_brace",
                    "'('" => "l_paren",
                    "')'" => "r_paren",
                    "'['" => "l_brack",
                    "']'" => "r_brack",
                    "|" => "pipe",
                    "<" => "l_angle",
                    ">" => "r_angle",
                    "=" => "eq",
                    "==" => "double_eq",
                    "!" => "bang",
                    "_" => "underscore",
                    "." => "dot",
                    ".." => "dotdot",
                    ":" => "colon",
                    "::" => "coloncolon",
                    "#" => "hash",
                    "?" => "question_mark",
                    "," => "comma",
                    "<:" => "sub",
                    _ => name,
                };
                format_ident!("{}_token", name)
            }
            Field::Node { name, .. } => {
                if name == "type" {
                    format_ident!("ty")
                } else {
                    format_ident!("{}", name)
                }
            }
        }
    }
}

fn main() {
    run().unwrap()
}
