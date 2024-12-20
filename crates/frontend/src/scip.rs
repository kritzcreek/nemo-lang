use camino::Utf8PathBuf;
use hop_scip::scip::{
    self, Document, Index, Metadata, PositionEncoding, SymbolInformation, SymbolRole, TextEncoding,
    ToolInfo,
};
use hop_scip::symbol::{Descriptor, GlobalSymbol, Package, Scheme, Symbol};
use line_index::LineIndex;
use rowan::TextRange;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fs;

use crate::ir::{Ctx, Name};
use crate::types::{Interface, Occurrence};
use crate::{run_frontend, ModuleResult};

pub fn write_index(sources: &[(Utf8PathBuf, String)]) -> Result<(), String> {
    let index = index_files(sources)?;
    fs::write("index.scip", hop_scip::encode_index(index)).map_err(|e| e.to_string())?;
    Ok(())
}

fn cmp_range(r1: &[i32], r2: &[i32]) -> Ordering {
    r1[0].cmp(&r2[0]).then(r1[1].cmp(&r2[1]))
}

fn mk_range(line_index: &LineIndex, range: &TextRange) -> Vec<i32> {
    let start = line_index.line_col(range.start());
    let end = line_index.line_col(range.end());
    if start == end {
        vec![start.line as i32, start.col as i32, end.col as i32]
    } else {
        vec![
            start.line as i32,
            start.col as i32,
            end.line as i32,
            end.col as i32,
        ]
    }
}

fn mk_sym(descriptors: Vec<Descriptor>) -> Symbol {
    Symbol::Global(GlobalSymbol {
        scheme: Scheme::new("scip-nemo"),
        package: Package::new(None, None, None),
        descriptors,
    })
}

fn module_definitions<'a>(
    accumulator: &mut HashMap<Name, (Symbol<'a>, SymbolInformation)>,
    descriptors: Vec<Descriptor<'a>>,
    ctx: &Ctx,
    iface: &Interface,
) {
    for def in iface.functions.values() {
        let mut descriptors = descriptors.clone();
        descriptors.push(Descriptor::Method {
            name: ctx.display_name(def.name).into(),
            disambiguator: None,
        });
        let sym = mk_sym(descriptors.clone());
        let symbol = sym.to_string();
        if accumulator
            .insert(
                def.name,
                (
                    sym,
                    SymbolInformation {
                        symbol,
                        documentation: vec![format!("`{}`", def.display(ctx))],
                        ..Default::default()
                    },
                ),
            )
            .is_some()
        {
            panic!(
                "Double inserted a definition {}",
                ctx.display_name(def.name)
            )
        }
    }
    for def in iface.variants.values() {
        let mut descriptors = descriptors.clone();
        let s = ctx.display_name(def.name);
        descriptors.push(Descriptor::Type(s.into()));
        let sym = mk_sym(descriptors.clone());
        let symbol = sym.to_string();
        if accumulator
            .insert(
                def.name,
                (
                    sym,
                    SymbolInformation {
                        symbol,
                        documentation: vec![format!("`{}`", def.display(ctx))],
                        ..Default::default()
                    },
                ),
            )
            .is_some()
        {
            panic!(
                "Double inserted a definition {}",
                ctx.display_name(def.name)
            )
        }
    }
    for def in iface.structs.values() {
        let mut descriptors = descriptors.clone();
        let s = ctx.display_name(def.name);
        if let Some(variant) = def.variant {
            descriptors.push(Descriptor::Type(ctx.display_name(variant).into()));
        }
        descriptors.push(Descriptor::Type(s.into()));
        let sym = mk_sym(descriptors.clone());
        let symbol = sym.to_string();
        if accumulator
            .insert(
                def.name,
                (
                    sym,
                    SymbolInformation {
                        symbol,
                        documentation: vec![format!("`{}`", def.display(ctx))],
                        ..Default::default()
                    },
                ),
            )
            .is_some()
        {
            panic!(
                "Double inserted a definition {}",
                ctx.display_name(def.name)
            )
        }
        for (name, ty) in def.fields.values() {
            let mut descriptors = descriptors.clone();
            let field_str = ctx.display_name(*name);
            descriptors.push(Descriptor::Term(field_str.into()));
            let sym = mk_sym(descriptors.clone());
            let symbol = sym.to_string();
            if accumulator
                .insert(
                    *name,
                    (
                        sym,
                        SymbolInformation {
                            symbol,
                            documentation: vec![format!("`{}`", ty.display(ctx))],
                            ..Default::default()
                        },
                    ),
                )
                .is_some()
            {
                panic!(
                    "Double inserted a definition {}",
                    ctx.display_name(def.name)
                )
            }
        }
    }
}

fn index_occurrence_map(
    definitions: &mut HashMap<Name, (Symbol, SymbolInformation)>,
    ctx: &Ctx,
    module: &ModuleResult,
) -> Document {
    let line_index = LineIndex::new(module.parse_result.source);
    let mut local_supply = 0;
    let mut symbols = vec![];
    let mut occurrences = vec![];
    let mut all_occs: Vec<_> = module.occurrences.iter().collect();
    all_occs.sort_by(|(ptr1, _), (ptr2, _)| ptr1.text_range().ordering(ptr2.text_range()));
    for (ptr, occ) in &all_occs {
        let range = ptr.0;
        if let Occurrence::Def(d) = occ {
            let (sym, symbol_information) = definitions.entry(*d).or_insert_with(|| {
                local_supply += 1;
                let sym = Symbol::Local {
                    local_id: local_supply.to_string().into(),
                };
                let symbol = sym.to_string();
                (
                    sym,
                    SymbolInformation {
                        symbol,
                        ..Default::default()
                    },
                )
            });
            if !sym.is_local() {
                symbols.push(symbol_information.clone());
            }
            occurrences.push(scip::Occurrence {
                range: mk_range(&line_index, &range),
                symbol: sym.to_string(),
                symbol_roles: SymbolRole::Definition.into(),
                ..Default::default()
            });
        }
    }
    for (ptr, occ) in &all_occs {
        let range = ptr.0;
        if let Occurrence::Ref(r) = occ {
            let (sym, _) = definitions.get(r).expect("Reference with no definition");
            occurrences.push(scip::Occurrence {
                range: mk_range(&line_index, &range),
                symbol: sym.to_string(),
                ..Default::default()
            });
        }
    }
    occurrences.sort_by(|o1, o2| cmp_range(&o1.range, &o2.range));
    Document {
        language: "nemo".to_string(),
        relative_path: ctx.get_module_path(module.parse_result.id).to_string(),
        occurrences,
        position_encoding: PositionEncoding::Utf8CodeUnitOffsetFromLineStart.into(),
        text: "".to_string(),
        symbols,
    }
}

fn index_files(sources: &[(Utf8PathBuf, String)]) -> Result<Index, String> {
    let frontend_result = run_frontend(sources);
    let ctx = &frontend_result.ctx;
    let mut definitions: HashMap<Name, (Symbol, SymbolInformation)> = HashMap::new();
    let mut documents: Vec<Document> = vec![];
    for module in &frontend_result.modules {
        let descriptors = vec![Descriptor::Namespace(
            module.parse_result.name.as_str().into(),
        )];
        let iface = ctx.get_interface(module.parse_result.id);
        module_definitions(&mut definitions, descriptors, ctx, iface);
    }
    for module in &frontend_result.modules {
        documents.push(index_occurrence_map(&mut definitions, ctx, module))
    }
    Ok(Index {
        metadata: Some(Metadata {
            tool_info: Some(ToolInfo {
                name: "scip-nemo".to_string(),
                version: env!("CARGO_PKG_VERSION").to_string(),
                arguments: vec![],
            }),
            text_document_encoding: TextEncoding::Utf8.into(),
            version: 1,
            project_root: "".to_string(),
        }),
        external_symbols: vec![],
        documents,
    })
}
