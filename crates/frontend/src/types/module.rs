use rowan::TextRange;

use crate::ir::{Ctx, FuncTy, Name, Symbol, Ty};
use core::fmt;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: Name,
    pub span: TextRange,
    pub variant: Option<Name>,
    pub ty_params: Vec<Name>,
    pub fields: HashMap<String, (Name, Ty)>,
}

impl StructDef {
    pub fn display<'a>(&'a self, ctx: &'a Ctx) -> StructDefDisplay<'a> {
        StructDefDisplay { def: self, ctx }
    }
}

pub struct StructDefDisplay<'a> {
    def: &'a StructDef,
    ctx: &'a Ctx,
}

impl fmt::Display for StructDefDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "struct {}", self.ctx.display_name(self.def.name))?;
        if !self.def.ty_params.is_empty() {
            write!(f, "[")?;
            for (i, ty_param) in self.def.ty_params.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", self.ctx.display_name(*ty_param))?;
            }
            write!(f, "]")?;
        }
        if let Some(variant) = self.def.variant {
            write!(f, " <: {}", self.ctx.display_name(variant))?;
        }
        write!(f, " {{")?;
        for (i, (name, ty)) in self.def.fields.values().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(
                f,
                "{}: {}",
                self.ctx.display_name(*name),
                ty.display(self.ctx)
            )?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct VariantDef {
    pub name: Name,
    pub span: TextRange,
    pub ty_params: Vec<Name>,
    // TODO: The ordering of these alternatives needs to be deterministic
    pub alternatives: HashMap<String, Name>,
}

impl VariantDef {
    pub fn lookup_alternative(&self, name: &str) -> Option<Name> {
        self.alternatives.get(name).copied()
    }

    pub fn display<'a>(&'a self, ctx: &'a Ctx) -> VariantDefDisplay<'a> {
        VariantDefDisplay { def: self, ctx }
    }
}

pub struct VariantDefDisplay<'a> {
    def: &'a VariantDef,
    ctx: &'a Ctx,
}

impl fmt::Display for VariantDefDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "variant {}", self.ctx.display_name(self.def.name))?;
        if !self.def.ty_params.is_empty() {
            write!(f, "[")?;
            for (i, ty_param) in self.def.ty_params.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", self.ctx.display_name(*ty_param))?;
            }
            write!(f, "]")?;
        }
        write!(f, " {{")?;
        for (i, (_, name)) in self.def.alternatives.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", self.ctx.display_name(*name))?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum TypeDef<'a> {
    Struct(&'a StructDef),
    Variant(&'a VariantDef),
}
impl TypeDef<'_> {
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

impl FuncDef {
    pub fn display<'a>(&'a self, ctx: &'a Ctx) -> FuncDefDisplay<'a> {
        FuncDefDisplay { def: self, ctx }
    }
}

pub struct FuncDefDisplay<'a> {
    def: &'a FuncDef,
    ctx: &'a Ctx,
}

impl fmt::Display for FuncDefDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {}", self.ctx.display_name(self.def.name))?;
        if !self.def.ty_params.is_empty() {
            write!(f, "[")?;
            for (i, ty_param) in self.def.ty_params.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", self.ctx.display_name(*ty_param))?;
            }
            write!(f, "]")?;
        }
        write!(f, "(")?;
        for (i, ty) in self.def.ty.arguments.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", ty.display(self.ctx))?;
        }
        write!(f, ")")?;
        write!(f, " -> {}", self.def.ty.result.display(self.ctx))?;
        Ok(())
    }
}

/// Contains all information needed to type check another module that uses
/// this module
#[derive(Debug, Default, Clone)]
pub struct Interface {
    pub structs: HashMap<Name, StructDef>,
    // NOTE: Does not contain mappings for variant alternatives
    pub struct_names: HashMap<String, Name>,
    pub variants: HashMap<Name, VariantDef>,
    pub variant_names: HashMap<String, Name>,
    pub functions: HashMap<String, FuncDef>,
}

impl Interface {
    pub fn display<'a>(&'a self, ctx: &'a Ctx) -> InterfaceDisplay<'a> {
        InterfaceDisplay {
            interface: self,
            ctx,
        }
    }

    pub fn lookup_type(&self, name: &str) -> Option<TypeDef> {
        self.variant_names
            .get(name)
            .map(|n| TypeDef::Variant(self.variants.get(n).unwrap()))
            .or_else(|| {
                self.struct_names
                    .get(name)
                    .map(|n| TypeDef::Struct(self.structs.get(n).unwrap()))
            })
    }

    pub fn lookup_type_name(&self, name: Name) -> Option<TypeDef> {
        self.structs
            .get(&name)
            .map(TypeDef::Struct)
            .or_else(|| self.variants.get(&name).map(TypeDef::Variant))
    }

    pub fn lookup_func(&self, name: &str) -> Option<&FuncDef> {
        self.functions.get(name)
    }
}

pub struct InterfaceDisplay<'a> {
    interface: &'a Interface,
    ctx: &'a Ctx,
}

impl fmt::Display for InterfaceDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Structs:")?;
        for def in self.interface.structs.values() {
            writeln!(f, "  {}", def.display(self.ctx))?;
        }
        writeln!(f, "Variants:")?;
        for def in self.interface.variants.values() {
            writeln!(f, "  {}", def.display(self.ctx))?;
        }
        writeln!(f, "Functions:")?;
        for def in self.interface.functions.values() {
            writeln!(f, "  {}", def.display(self.ctx))?;
        }
        Ok(())
    }
}
