use rowan::TextRange;

use crate::ir::{FuncTy, Name, Symbol, Ty};
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: Name,
    pub span: TextRange,
    pub variant: Option<Name>,
    pub ty_params: Vec<Name>,
}

#[derive(Debug, Clone)]
pub struct VariantDef {
    pub name: Name,
    pub span: TextRange,
    pub ty_params: Vec<Name>,
    // TODO: The ordering of these alternatives needs to be deterministic
    pub alternatives: HashMap<Symbol, Name>,
}

impl VariantDef {
    pub fn lookup_alternative(&self, name: Symbol) -> Option<Name> {
        self.alternatives.get(&name).copied()
    }
}

#[derive(Debug, Clone)]
pub enum TypeDef {
    Struct(Rc<StructDef>),
    Variant(Rc<VariantDef>),
}
impl TypeDef {
    pub fn name(&self) -> Name {
        match self {
            TypeDef::Struct(x) => x.name,
            TypeDef::Variant(x) => x.name,
        }
    }

    pub fn ty_params(&self) -> &[Name] {
        match self {
            TypeDef::Struct(x) => &x.ty_params,
            TypeDef::Variant(x) => &x.ty_params,
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructFields {
    // TODO: The ordering of these fields needs to be deterministic
    pub fields: HashMap<Symbol, (Name, Ty)>,
}

impl StructFields {
    pub fn to_ir(&self) -> Vec<(Name, Ty)> {
        self.fields.values().cloned().collect()
    }

    pub fn names(&self) -> Vec<Name> {
        self.fields.values().map(|(n, _)| *n).collect()
    }
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub name: Name,
    pub ty_params: Vec<Name>,
    pub ty: FuncTy,
}

/// Contains all information needed to type check another module that uses
/// this module
#[derive(Debug, Default)]
pub struct Interface {
    pub structs: HashMap<Symbol, (StructDef, Visibility)>,
    pub variants: HashMap<Symbol, (VariantDef, Visibility)>,
    pub functions: HashMap<Symbol, (FuncDef, Visibility)>,
}
