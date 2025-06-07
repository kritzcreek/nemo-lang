pub mod format;
mod names;

use core::fmt;
use derive_ir::IrBuilder;
pub use names::{CompactId, Ctx, Id, ModuleId, ModuleIdGen, Name, NameSupply, NameTag, Symbol};
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt::Debug,
};
use text_size::TextRange;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Ty {
    I32,
    U32,
    F32,
    Unit,
    Bool,
    Bytes,
    Array(Box<Ty>),
    Cons { name: Name, ty_args: Substitution },
    Var(Name),
    Func(Box<FuncTy>),
    // The type of return expressions
    Diverge,

    // Typechecking internal used for error recovery
    Error,
}

impl Ty {
    pub fn display<'a>(&'a self, ctx: &'a Ctx) -> TyDisplay<'a> {
        TyDisplay { ty: self, ctx }
    }

    fn vars_inner(&self, acc: &mut HashSet<Name>) {
        match self {
            Ty::I32
            | Ty::U32
            | Ty::F32
            | Ty::Unit
            | Ty::Bool
            | Ty::Bytes
            | Ty::Error
            | Ty::Diverge => {}
            Ty::Array(t) => t.vars_inner(acc),
            Ty::Cons { name: _, ty_args } => {
                for ty in ty_args.0.values() {
                    ty.vars_inner(acc)
                }
            }
            Ty::Var(v) => {
                acc.insert(*v);
            }
            Ty::Func(func_ty) => {
                for t in &func_ty.arguments {
                    t.vars_inner(acc)
                }
                func_ty.result.vars_inner(acc)
            }
        }
    }

    pub fn vars(&self) -> HashSet<Name> {
        let mut result = HashSet::new();
        self.vars_inner(&mut result);
        result
    }
}

pub struct TyDisplay<'a> {
    ty: &'a Ty,
    ctx: &'a Ctx,
}

impl fmt::Display for TyDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            Ty::I32 => write!(f, "I32"),
            Ty::U32 => write!(f, "U32"),
            Ty::F32 => write!(f, "F32"),
            Ty::Bool => write!(f, "Bool"),
            Ty::Unit => write!(f, "Unit"),
            Ty::Bytes => write!(f, "Bytes"),
            Ty::Diverge => write!(f, "!"),
            Ty::Array(t) => write!(f, "[{}]", t.display(self.ctx)),
            Ty::Cons {
                name: t,
                ty_args: args,
            } => {
                self.ctx.fmt_name(f, *t)?;
                if !args.is_empty() {
                    write!(f, "[")?;
                    for (idx, arg) in args.tys().into_iter().enumerate() {
                        if idx != 0 {
                            write!(f, ", ")?
                        }
                        write!(f, "{}", arg.display(self.ctx))?;
                    }
                    write!(f, "]")?;
                };
                Ok(())
            }
            Ty::Var(v) => self.ctx.fmt_name(f, *v),
            Ty::Func(func_ty) => func_ty.display(self.ctx).fmt(f),
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
    pub fn display<'a>(&'a self, ctx: &'a Ctx) -> FuncTyDisplay<'a> {
        FuncTyDisplay { func_ty: self, ctx }
    }
}

pub struct FuncTyDisplay<'a> {
    func_ty: &'a FuncTy,
    ctx: &'a Ctx,
}

impl fmt::Display for FuncTyDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fn ({}) -> {}",
            self.func_ty
                .arguments
                .iter()
                .map(|a| format!("{}", a.display(self.ctx)))
                .collect::<Vec<String>>()
                .join(", "),
            self.func_ty.result.display(self.ctx)
        )
    }
}

#[derive(Debug, Default, PartialEq, Eq, Clone, Hash)]
pub struct Substitution(pub(crate) BTreeMap<Name, Ty>);
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

    pub fn insert(&mut self, var: Name, ty: Ty) -> Option<Ty> {
        self.0.insert(var, ty)
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
            Ty::I32
            | Ty::U32
            | Ty::F32
            | Ty::Unit
            | Ty::Bool
            | Ty::Bytes
            | Ty::Error
            | Ty::Diverge => ty,
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
pub struct UnOp {
    pub it: UnOpData,
    pub at: TextRange,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum UnOpData {
    I32Neg,
    F32Neg,
    I32Not,
    U32Not,
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
    I32Shl,
    I32Shr,
    I32Rem,
    I32Lt,
    I32Gt,
    I32Le,
    I32Ge,
    I32Eq,
    I32Ne,
    I32And,
    I32Or,

    U32Add,
    U32Sub,
    U32Mul,
    U32Div,
    U32Shl,
    U32Shr,
    U32Rem,
    U32Lt,
    U32Gt,
    U32Le,
    U32Ge,
    U32Eq,
    U32Ne,
    U32And,
    U32Or,

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
    U32(u32),
    F32(f32),
    Bool(bool),
    Bytes(String),
    Unit,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Pattern {
    pub it: Box<PatternData>,
    pub at: TextRange,
    pub ty: Ty,
}
impl Pattern {
    fn bound_vars(&self) -> Vec<Name> {
        match *self.it {
            PatternData::PatVar { var } => vec![var],
            PatternData::PatVariant { binder, .. } => vec![binder],
        }
    }
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

impl MatchBranch {
    fn free_vars(&self) -> HashMap<Name, FreeVarInfo<'_>> {
        let mut fvs = self.body.free_vars();
        for bound in self.pattern.bound_vars() {
            fvs.remove(&bound);
        }
        fvs
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub it: Box<ExprData>,
    pub at: TextRange,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FreeVarInfo<'a> {
    pub ty: &'a Ty,
    pub is_assigned: Option<TextRange>,
}

impl Expr {
    pub fn free_vars(&self) -> HashMap<Name, FreeVarInfo<'_>> {
        fn free_vars_inner<'a>(current: &mut HashMap<Name, FreeVarInfo<'a>>, expr: &'a Expr) {
            match &*expr.it {
                ExprData::Lit { .. } => {}
                ExprData::Var { name } => {
                    if let NameTag::Local = name.tag {
                        // We don't want to override an existing entry for a variable that gets re-assigned
                        current.entry(*name).or_insert_with(|| FreeVarInfo {
                            ty: &expr.ty,
                            is_assigned: None,
                        });
                    }
                }
                ExprData::Call { func, arguments } => {
                    match func {
                        Callee::FuncRef(e) => free_vars_inner(current, e),
                        Callee::Func { .. } | Callee::Builtin(_) => {}
                    }
                    for arg in arguments {
                        free_vars_inner(current, arg)
                    }
                }
                ExprData::Unary { op: _, expr } => {
                    free_vars_inner(current, expr);
                }
                ExprData::Binary { op: _, left, right } => {
                    free_vars_inner(current, left);
                    free_vars_inner(current, right);
                }
                ExprData::Array { elems } => {
                    for elem in elems {
                        free_vars_inner(current, elem)
                    }
                }
                ExprData::ArrayIdx { array, index } => {
                    free_vars_inner(current, array);
                    free_vars_inner(current, index);
                }
                ExprData::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    free_vars_inner(current, condition);
                    free_vars_inner(current, then_branch);
                    free_vars_inner(current, else_branch);
                }
                ExprData::Block { declarations, expr } => {
                    let mut fvs = HashMap::new();
                    free_vars_inner(&mut fvs, expr);
                    for decl in declarations.iter().rev() {
                        match &decl.it {
                            DeclarationData::Let { binder, expr } => {
                                fvs.remove(binder);
                                free_vars_inner(&mut fvs, expr)
                            }
                            DeclarationData::Set { set_target, expr } => {
                                match &set_target.it {
                                    SetTargetData::SetArray { target, index } => {
                                        free_vars_inner(&mut fvs, target);
                                        free_vars_inner(&mut fvs, index)
                                    }
                                    SetTargetData::SetStruct { target, index: _ } => {
                                        free_vars_inner(&mut fvs, target)
                                    }
                                    SetTargetData::SetVar { name } => {
                                        fvs.entry(*name)
                                            .and_modify(|fvi| fvi.is_assigned = Some(set_target.at))
                                            .or_insert(FreeVarInfo {
                                                ty: &set_target.ty,
                                                is_assigned: Some(set_target.at),
                                            });
                                    }
                                }
                                free_vars_inner(&mut fvs, expr)
                            }
                            DeclarationData::Expr { expr } => free_vars_inner(&mut fvs, expr),
                            DeclarationData::While { condition, body } => {
                                free_vars_inner(&mut fvs, condition);
                                free_vars_inner(&mut fvs, body)
                            }
                        }
                    }
                    current.extend(fvs)
                }
                ExprData::Struct { name: _, fields } => {
                    for (_, expr) in fields {
                        free_vars_inner(current, expr)
                    }
                }
                ExprData::StructIdx { expr, index: _ } => free_vars_inner(current, expr),
                ExprData::Match {
                    scrutinee,
                    branches,
                } => {
                    free_vars_inner(current, scrutinee);
                    for branch in branches {
                        let fvs = branch.free_vars();
                        current.extend(fvs)
                    }
                }
                ExprData::Lambda { captures, .. } => {
                    for (capture, ty) in captures {
                        current.insert(
                            *capture,
                            FreeVarInfo {
                                ty,
                                is_assigned: None,
                            },
                        );
                    }
                }
                ExprData::Return { expr } => free_vars_inner(current, expr),
            }
        }
        let mut results = HashMap::new();
        free_vars_inner(&mut results, self);
        results
    }
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
    Unary {
        op: UnOp,
        expr: Expr,
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
    Lambda {
        captures: Vec<(Name, Ty)>,
        params: Vec<(Name, Ty)>,
        return_ty: Ty,
        body: Expr,
    },
    Return {
        expr: Expr,
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

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Program {
    pub imports: Vec<Import>,
    pub types: Vec<TypeDef>,
    pub globals: Vec<Global>,
    pub funcs: Vec<Func>,
}

impl Program {
    pub fn merge(&mut self, other: Program) {
        self.imports.extend(other.imports);
        self.types.extend(other.types);
        self.globals.extend(other.globals);
        self.funcs.extend(other.funcs);
    }
}
