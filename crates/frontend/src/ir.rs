mod names;

use core::fmt;
use derive_ir::IrBuilder;
pub use names::{Id, Name, NameMap, NameSupply};
use std::{collections::BTreeMap, fmt::Debug};
use text_size::TextRange;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Ty {
    I32,
    F32,
    Unit,
    Bool,
    Array(Box<Ty>),
    Cons { name: Name, ty_args: Substitution },
    Var(Name),
    Func(Box<FuncTy>),

    // Typechecking internal used for error recovery
    Error,
}

impl Ty {
    pub fn display<'a>(&'a self, name_map: &'a NameMap) -> TyDisplay<'a> {
        TyDisplay { ty: self, name_map }
    }
}

pub struct TyDisplay<'a> {
    ty: &'a Ty,
    name_map: &'a NameMap,
}

impl fmt::Display for TyDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            Ty::I32 => write!(f, "i32"),
            Ty::F32 => write!(f, "f32"),
            Ty::Bool => write!(f, "bool"),
            Ty::Unit => write!(f, "unit"),
            Ty::Array(t) => write!(f, "[{}]", t.display(self.name_map)),
            Ty::Cons {
                name: t,
                ty_args: args,
            } => {
                write!(f, "{}", self.name_map.get(t).unwrap().it)?;
                if !args.is_empty() {
                    write!(f, "[")?;
                    for (idx, arg) in args.tys().into_iter().enumerate() {
                        if idx != 0 {
                            write!(f, ", ")?
                        }
                        write!(f, "{}", arg.display(self.name_map))?;
                    }
                    write!(f, "]")?;
                };
                Ok(())
            }
            Ty::Var(v) => write!(f, "{}", self.name_map.get(v).unwrap().it),
            Ty::Func(func_ty) => func_ty.display(self.name_map).fmt(f),
            Ty::Error => write!(f, "ERROR"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct FuncTy {
    pub arguments: Vec<Ty>,
    pub result: Ty,
}

impl FuncTy {
    pub fn display<'a>(&'a self, name_map: &'a NameMap) -> FuncTyDisplay<'a> {
        FuncTyDisplay {
            func_ty: self,
            name_map,
        }
    }
}

pub struct FuncTyDisplay<'a> {
    func_ty: &'a FuncTy,
    name_map: &'a NameMap,
}

impl fmt::Display for FuncTyDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fn ({}) -> {}",
            self.func_ty
                .arguments
                .iter()
                .map(|a| format!("{}", a.display(self.name_map)))
                .collect::<Vec<String>>()
                .join(", "),
            self.func_ty.result.display(self.name_map)
        )
    }
}

#[derive(Debug, Default, PartialEq, Eq, Clone, Hash)]
pub struct Substitution(BTreeMap<Name, Ty>);
impl Substitution {
    pub fn empty() -> Self {
        Substitution(BTreeMap::new())
    }

    pub fn new(names: &[Name], tys: &[Ty]) -> Self {
        let mut mappings = BTreeMap::new();
        for (name, ty) in names.iter().zip(tys.iter()) {
            mappings.insert(*name, ty.clone());
        }
        Substitution(mappings)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn lookup(&self, var: Name) -> Option<&Ty> {
        self.0.get(&var)
    }

    pub fn apply(&self, ty: Ty) -> Ty {
        if self.is_empty() {
            return ty;
        }
        match ty {
            Ty::Var(n) => self.0.get(&n).cloned().unwrap_or(ty),
            Ty::I32 | Ty::F32 | Ty::Unit | Ty::Bool | Ty::Error => ty,
            Ty::Array(t) => Ty::Array(Box::new(self.apply(*t))),
            Ty::Func(f) => Ty::Func(Box::new(self.apply_func(*f))),
            Ty::Cons { name, ty_args } => Ty::Cons {
                name,
                ty_args: self.apply_subst(ty_args),
            },
        }
    }

    pub fn apply_func(&self, ty: FuncTy) -> FuncTy {
        if self.is_empty() {
            return ty;
        }
        FuncTy {
            arguments: ty.arguments.into_iter().map(|t| self.apply(t)).collect(),
            result: self.apply(ty.result),
        }
    }

    pub fn apply_subst(&self, subst: Substitution) -> Substitution {
        if self.is_empty() {
            return subst;
        }
        let mut subst = subst;
        for (_, v) in subst.0.iter_mut() {
            let ty = std::mem::replace(v, Ty::Error);
            *v = self.apply(ty);
        }
        subst
    }

    pub fn names(&self) -> Vec<Name> {
        let mut keys: Vec<Name> = self.0.keys().copied().collect();
        keys.sort();
        keys
    }

    pub fn tys(&self) -> Vec<&Ty> {
        let mut keys: Vec<(&Name, &Ty)> = self.0.iter().collect();
        keys.sort_by_key(|(n, _)| **n);
        keys.into_iter().map(|(_, t)| t).collect()
    }

    pub fn tys_owned(&self) -> Vec<Ty> {
        let mut keys: Vec<(&Name, &Ty)> = self.0.iter().collect();
        keys.sort_by_key(|(n, _)| **n);
        keys.into_iter().map(|(_, t)| t.clone()).collect()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Op {
    pub it: OpData,
    pub at: TextRange,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum OpData {
    I32Add,
    I32Sub,
    I32Mul,
    I32Div,
    I32Lt,
    I32Gt,
    I32Le,
    I32Ge,
    I32Eq,
    I32Ne,

    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,
    F32Eq,
    F32Ne,

    BoolEq,
    BoolNe,
    BoolAnd,
    BoolOr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Lit {
    pub it: LitData,
    pub at: TextRange,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LitData {
    I32(i32),
    F32(f32),
    Bool(bool),
    Unit,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Pattern {
    pub it: Box<PatternData>,
    pub at: TextRange,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Clone, IrBuilder)]
pub enum PatternData {
    PatVar {
        var: Name,
    },
    PatVariant {
        variant: Name,
        alternative: Name,
        binder: Name,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct MatchBranch {
    pub at: TextRange,
    pub pattern: Pattern,
    pub body: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub it: Box<ExprData>,
    pub at: TextRange,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Callee {
    Func { name: Name, type_args: Substitution },
    FuncRef(Expr),
    Builtin(&'static str),
}

#[derive(Debug, PartialEq, Clone, IrBuilder)]
pub enum ExprData {
    Lit {
        lit: Lit,
    },
    Var {
        name: Name,
    },
    Call {
        func: Callee,
        arguments: Vec<Expr>,
    },
    Binary {
        op: Op,
        left: Expr,
        right: Expr,
    },
    Array {
        elems: Vec<Expr>,
    },
    ArrayIdx {
        array: Expr,
        index: Expr,
    },
    If {
        condition: Expr,
        then_branch: Expr,
        else_branch: Expr,
    },
    Block {
        declarations: Vec<Declaration>,
        // All IR blocks end in an expression, lowering inserts a dummy unit expression
        expr: Expr,
    },
    Struct {
        name: Name,
        fields: Vec<(Name, Expr)>,
    },
    StructIdx {
        expr: Expr,
        index: Name,
    },
    Match {
        scrutinee: Expr,
        branches: Vec<MatchBranch>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Declaration {
    pub it: DeclarationData,
    pub at: TextRange,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Clone, IrBuilder)]
pub enum DeclarationData {
    Let { binder: Name, expr: Expr },
    Set { set_target: SetTarget, expr: Expr },
    Expr { expr: Expr },
    While { condition: Expr, body: Expr },
}

#[derive(Debug, PartialEq, Clone)]
pub struct SetTarget {
    pub it: SetTargetData,
    pub at: TextRange,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Clone, IrBuilder)]
pub enum SetTargetData {
    SetArray { target: Expr, index: Expr },
    SetStruct { target: Expr, index: Name },
    SetVar { name: Name },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Import {
    pub span: TextRange,
    pub internal: Name,
    pub func_ty: FuncTy,
    pub external: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeDef {
    Struct(Struct),
    Variant(Variant),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variant {
    pub span: TextRange,
    pub name: Name,
    pub alternatives: Vec<Name>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Struct {
    pub span: TextRange,
    pub name: Name,
    pub variant: Option<Name>,
    pub fields: Vec<(Name, Ty)>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Global {
    pub span: TextRange,
    pub binder: Name,
    pub init: Expr,
}

#[derive(Debug, PartialEq, Clone, IrBuilder)]
pub struct Func {
    pub name: Name,
    pub ty_params: Vec<Name>,
    pub params: Vec<(Name, Ty)>,
    pub return_ty: Ty,
    pub body: Expr,
}

impl Func {
    pub fn func_ty(&self) -> FuncTy {
        FuncTy {
            arguments: self.params.iter().map(|(_, t)| t.clone()).collect(),
            result: self.return_ty.clone(),
        }
    }
    pub fn is_monomorphic(&self) -> bool {
        self.ty_params.is_empty()
    }

    pub fn is_polymorphic(&self) -> bool {
        !self.is_monomorphic()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub imports: Vec<Import>,
    pub types: Vec<TypeDef>,
    pub globals: Vec<Global>,
    pub funcs: Vec<Func>,
    pub start_fn: Name,
}
