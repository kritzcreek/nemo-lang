pub use crate::names::{Id, Name, NameMap, NameSupply};
use core::fmt;
use std::{collections::BTreeMap, fmt::Debug};
use text_size::TextRange;

pub(crate) trait Spanned {
    fn at(&self) -> &TextRange;
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Ty {
    I32,
    F32,
    Unit,
    Bool,
    Array(Box<Ty>),
    Struct {
        name: Name,
        ty_args: Option<Substitution>,
    },
    Var(Name),
    Func(Box<FuncTy>),

    // Typechecking internal used for error recovery
    Any,
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
            Ty::Struct {
                name: t,
                ty_args: args,
            } => {
                write!(f, "{}", self.name_map.get(t).unwrap().it)?;
                if let Some(subst) = args {
                    write!(f, "[")?;
                    for (idx, arg) in subst.tys().into_iter().enumerate() {
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
            Ty::Any => write!(f, "ANY"),
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Substitution(BTreeMap<Name, Ty>);
impl Substitution {
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
        match ty {
            Ty::Var(n) => self.0.get(&n).cloned().unwrap_or(ty),
            Ty::I32 | Ty::F32 | Ty::Unit | Ty::Bool | Ty::Any => ty,
            Ty::Array(t) => Ty::Array(Box::new(self.apply(*t))),
            Ty::Func(f) => Ty::Func(Box::new(self.apply_func(*f))),
            Ty::Struct { name, ty_args } => Ty::Struct {
                name,
                ty_args: ty_args.map(|s| self.apply_subst(s)),
            },
        }
    }

    pub fn apply_func(&self, ty: FuncTy) -> FuncTy {
        FuncTy {
            arguments: ty.arguments.into_iter().map(|t| self.apply(t)).collect(),
            result: self.apply(ty.result),
        }
    }

    pub fn apply_subst(&self, subst: Substitution) -> Substitution {
        let mut subst = subst;
        for (_, v) in subst.0.iter_mut() {
            let ty = std::mem::replace(v, Ty::Any);
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
}

// Our backend ast is very similar to our syntax ast.
//
// The main differences at this point in time are:
// 1. All names are replaced with unique numeric identifiers the `Name` type in this module
// 2. All syntactic type annotations are omitted, as we only care about the infered/semantic ones
// 3. All type-based resolution is done during lowering -> + becomes f32.+ or i32.+ and all struct indexes are
//    unique based on their type
// 4. The only desugarings we do at this point in time:
//    a) Transform set_targets to expr + index pairs, which which makes it easier to generate
//       code for these eventually
//    b) Transform all blocks into a list of declarations and a trailing expression, inserting
//       a dummy Unit expression if the last declaration is not a Declaration::Expr
//
// We also try to keep as many Spans around as possible, mostly to help us when debugging our compiler

#[derive(Debug, PartialEq, Clone)]
pub struct Op {
    pub it: OpData,
    pub at: TextRange,
}

impl Spanned for Op {
    fn at(&self) -> &TextRange {
        &self.at
    }
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

impl Spanned for Lit {
    fn at(&self) -> &TextRange {
        &self.at
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LitData {
    I32(i32),
    F32(f32),
    Bool(bool),
    Unit,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Intrinsic {
    pub it: IntrinsicData,
    pub at: TextRange,
}

impl Spanned for Intrinsic {
    fn at(&self) -> &TextRange {
        &self.at
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum IntrinsicData {
    ArrayLen,
    ArrayNew,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Pattern {
    pub it: Box<PatternData>,
    pub at: TextRange,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PatternData {
    Var(Name),
    Variant {
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

impl Spanned for Expr {
    fn at(&self) -> &TextRange {
        &self.at
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Callee {
    Func { name: Name, type_args: Vec<Ty> },
    FuncRef(Expr),
    Builtin(&'static str),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprData {
    Lit(Lit),
    Var(Name),
    Call {
        func: Callee,
        arguments: Vec<Expr>,
    },
    Binary {
        op: Op,
        left: Expr,
        right: Expr,
    },
    Array(Vec<Expr>),
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
    Intrinsic {
        intrinsic: Intrinsic,
        arguments: Vec<Expr>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Declaration {
    pub it: DeclarationData,
    pub at: TextRange,
    pub ty: Ty,
}

impl Spanned for Declaration {
    fn at(&self) -> &TextRange {
        &self.at
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum DeclarationData {
    Let { binder: Name, expr: Expr },
    Set { set_target: SetTarget, expr: Expr },
    Expr(Expr),
    While { condition: Expr, body: Expr },
}

#[derive(Debug, PartialEq, Clone)]
pub struct SetTarget {
    pub it: SetTargetData,
    pub at: TextRange,
    pub ty: Ty,
}

impl Spanned for SetTarget {
    fn at(&self) -> &TextRange {
        &self.at
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SetTargetData {
    Array { target: Expr, index: Expr },
    Struct { target: Expr, index: Name },
    Var { name: Name },
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

#[derive(Debug, PartialEq, Clone)]
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
