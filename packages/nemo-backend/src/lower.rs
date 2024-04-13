use std::collections::HashMap;

use crate::ir::{
    Callee, Declaration, DeclarationData, Expr, ExprData, Func, FuncTy, Global, Import, Intrinsic,
    IntrinsicData, Lit, LitData, Name, Op, OpData, Program, SetTarget, SetTargetData, Struct, Ty,
};
use nemo_frontend::{
    builtins,
    syntax::{self, FuncId, Id, Span, ToplevelData},
    types,
};

// Tracks local names
struct Scope(Vec<HashMap<String, Name>>);

impl Scope {
    fn new() -> Scope {
        Scope(vec![HashMap::new()])
    }

    fn insert(&mut self, s: String, name: Name) {
        self.0.last_mut().unwrap().insert(s, name);
    }

    fn get(&self, s: &str) -> Option<Name> {
        self.0.iter().rev().find_map(|block| block.get(s)).copied()
    }

    fn enter_block(&mut self) {
        self.0.push(HashMap::new())
    }

    fn leave_block(&mut self) {
        self.0.pop().unwrap();
    }
}

struct TypeInfo {
    name: Name,
    fields: HashMap<String, Name>,
}

struct Lower {
    // Name supply
    local: u32,
    global: u32,
    func: u32,
    typ: u32,
    field: u32,

    types: HashMap<String, TypeInfo>,
    funcs: HashMap<String, Name>,

    scope: Scope,
    // For debugging purposes (lets us map lowered identifiers back to their original source)
    name_map: HashMap<Name, Id>,
}

impl Lower {
    fn new() -> Lower {
        Lower {
            local: 0,
            global: 0,
            func: 0,
            typ: 0,
            field: 0,

            types: HashMap::new(),
            funcs: HashMap::new(),

            scope: Scope::new(),
            name_map: HashMap::new(),
        }
    }

    fn local_idx(&mut self, local: Id) -> Name {
        self.local += 1;
        let name = Name::Local(self.local);
        self.scope.insert(local.it.clone(), name);
        self.name_map.insert(name, local);
        name
    }

    fn global_idx(&mut self, global: Id) -> Name {
        self.global += 1;
        let name = Name::Global(self.global);
        self.name_map.insert(name, global);
        name
    }

    fn func_idx(&mut self, func: Id) -> Name {
        self.func += 1;
        let name = Name::Func(self.func);
        self.name_map.insert(
            name,
            Id {
                it: func.it,
                at: func.at,
            },
        );
        name
    }

    fn type_idx(&mut self, typ: Id) -> Name {
        self.typ += 1;
        let name = Name::Type(self.typ);
        self.name_map.insert(name, typ);
        name
    }

    fn field_idx(&mut self, field: Id) -> Name {
        self.field += 1;
        let name = Name::Field(self.field);
        self.name_map.insert(name, field);
        name
    }

    fn declare_func(&mut self, func: FuncId) {
        let name = self.func_idx(func.clone().to_id());
        self.funcs.insert(func.it, name);
    }

    fn declare_struct(&mut self, ty: Id, fields: Vec<Id>) {
        let ty_name = self.type_idx(ty.clone());
        let mut field_info = HashMap::with_capacity(fields.len());
        for field in fields {
            let field_idx = self.field_idx(field.clone());
            field_info.insert(field.it, field_idx);
        }
        self.types.insert(
            ty.it,
            TypeInfo {
                name: ty_name,
                fields: field_info,
            },
        );
    }

    fn lookup_ty(&self, ty: &str) -> &TypeInfo {
        self.types.get(ty).unwrap()
    }

    fn func_exists(&self, func: &str) -> bool {
        self.funcs.contains_key(func)
    }

    fn lookup_func(&self, func: &str) -> Name {
        *self.funcs.get(func).unwrap()
    }

    fn lookup_field(&self, struct_name: &str, field: &str) -> Name {
        *self.lookup_ty(struct_name).fields.get(field).unwrap()
    }

    fn lookup_field_ty(&self, ty: &types::Ty, field: &str) -> Name {
        if let types::Ty::Struct(struct_name) = ty {
            self.lookup_field(struct_name, field)
        } else {
            unreachable!("Tried to look up a non-existing struct type {ty}")
        }
    }

    fn lookup_var_or_func(&self, var: &str) -> Name {
        match self.scope.get(var) {
            Some(n) => n,
            None => self.lookup_func(var),
        }
    }

    fn lookup_var(&self, var: &str) -> Name {
        self.scope.get(var).unwrap()
    }

    fn gen_unit(&self) -> Expr {
        Expr {
            it: Box::new(ExprData::Lit(Lit {
                it: LitData::Unit,
                at: Span::SYN,
                ty: Ty::Unit,
            })),
            at: Span::SYN,
            ty: Ty::Unit,
        }
    }

    fn lower_type(&self, ty: &syntax::Type) -> Ty {
        self.lower_ty(&ty.ty)
    }

    fn lower_ty(&self, ty: &types::Ty) -> Ty {
        match ty {
            types::Ty::I32 => Ty::I32,
            types::Ty::F32 => Ty::F32,
            types::Ty::Unit => Ty::Unit,
            types::Ty::Bool => Ty::Bool,
            types::Ty::Array(t) => Ty::Array(Box::new(self.lower_ty(t))),
            types::Ty::Struct(s) => Ty::Struct(self.lookup_ty(s).name),
            types::Ty::Func(ref func_ty) => Ty::Func(Box::new(self.lower_func_ty(func_ty))),
        }
    }

    fn lower_func_ty(&self, func_ty: &types::FuncTy) -> FuncTy {
        FuncTy {
            arguments: func_ty.arguments.iter().map(|a| self.lower_ty(a)).collect(),
            result: self.lower_ty(&func_ty.result),
        }
    }

    fn lower_import(&self, toplevel: syntax::Toplevel) -> Import {
        if let syntax::ToplevelData::Import {
            internal,
            func_ty,
            external,
        } = toplevel.it
        {
            Import {
                span: toplevel.at,
                internal: self.lookup_func(&internal.it),
                func_ty: self.lower_func_ty(&func_ty.ty),
                external: external.it,
            }
        } else {
            unreachable!("Passed a non-import to lower_import")
        }
    }

    fn lower_struct(&self, toplevel: syntax::Toplevel) -> Struct {
        if let syntax::ToplevelData::Struct { name, fields } = toplevel.it {
            let ty_info = self.lookup_ty(&name.it);
            Struct {
                span: toplevel.at,
                name: ty_info.name,
                fields: fields
                    .into_iter()
                    .map(|(name, ty)| {
                        (*ty_info.fields.get(&name.it).unwrap(), self.lower_type(&ty))
                    })
                    .collect(),
            }
        } else {
            unreachable!("Passed a non-struct to lower_struct")
        }
    }

    fn lower_global(&mut self, toplevel: syntax::Toplevel) -> Global {
        if let syntax::ToplevelData::Global {
            binder,
            annotation: _,
            init,
        } = toplevel.it
        {
            Global {
                span: toplevel.at,
                binder: self.lookup_var(&binder.it),
                init: self.lower_expr(init),
            }
        } else {
            unreachable!("Passed a non-global to lower_global")
        }
    }

    fn lower_func(&mut self, toplevel: syntax::Toplevel) -> Func {
        if let syntax::ToplevelData::Func {
            name,
            params,
            return_ty,
            body,
        } = toplevel.it
        {
            self.scope.enter_block();
            let params: Vec<_> = params
                .into_iter()
                .map(|(name, ty)| (self.local_idx(name), self.lower_type(&ty)))
                .collect();
            let func = Func {
                name: self.lookup_func(&name.it),
                params,
                return_ty: return_ty.map(|t| self.lower_type(&t)).unwrap_or(Ty::Unit),
                body: self.lower_expr(body),
            };
            self.scope.leave_block();
            func
        } else {
            unreachable!("Passed a non-func to lower_func")
        }
    }

    fn lower_lit(&self, lit: syntax::Lit) -> Lit {
        Lit {
            at: lit.at,
            ty: self.lower_ty(&lit.ty),
            it: match lit.it {
                syntax::LitData::I32(i) => LitData::I32(i),
                syntax::LitData::F32(f) => LitData::F32(f),
                syntax::LitData::Bool(b) => LitData::Bool(b),
            },
        }
    }

    fn lower_intrinsic(&self, intrinsic: syntax::Intrinsic) -> crate::ir::Intrinsic {
        let it = match intrinsic.it {
            syntax::IntrinsicData::ArrayLen => IntrinsicData::ArrayLen,
            syntax::IntrinsicData::ArrayNew => IntrinsicData::ArrayNew,
        };
        Intrinsic {
            it,
            at: intrinsic.at,
        }
    }

    fn lower_expr(&mut self, expr: syntax::Expr) -> Expr {
        let expr_data = match *expr.it {
            syntax::ExprData::Lit(l) => ExprData::Lit(self.lower_lit(l)),
            syntax::ExprData::Var(v) => ExprData::Var(self.lookup_var_or_func(&v.it)),
            syntax::ExprData::Call { func, arguments } => {
                let func = match *func.it {
                    syntax::ExprData::Var(ref v) => {
                        if self.scope.get(&v.it).is_some() {
                            let fn_expr = self.lower_expr(func);
                            Callee::FuncRef(fn_expr)
                        } else if self.func_exists(&v.it) {
                            Callee::Func(self.lookup_func(&v.it))
                        } else if let Some(fun) = builtins::lookup_builtin(&v.it) {
                            Callee::Builtin(fun.name)
                        } else {
                            unreachable!("Can't resolve function: {}", v)
                        }
                    }
                    _ => {
                        let fn_expr = self.lower_expr(func);
                        Callee::FuncRef(fn_expr)
                    }
                };
                ExprData::Call {
                    func,
                    arguments: arguments.into_iter().map(|e| self.lower_expr(e)).collect(),
                }
            }
            syntax::ExprData::Binary { op, left, right } => {
                let op = self.lower_op(op, &left.ty, &right.ty);

                ExprData::Binary {
                    op,
                    left: self.lower_expr(left),
                    right: self.lower_expr(right),
                }
            }
            syntax::ExprData::Array(elements) => {
                ExprData::Array(elements.into_iter().map(|e| self.lower_expr(e)).collect())
            }
            syntax::ExprData::ArrayIdx { array, index } => ExprData::ArrayIdx {
                array: self.lower_expr(array),
                index: self.lower_expr(index),
            },
            syntax::ExprData::If {
                condition,
                then_branch,
                else_branch,
            } => ExprData::If {
                condition: self.lower_expr(condition),
                then_branch: self.lower_expr(then_branch),
                else_branch: self.lower_expr(else_branch),
            },
            syntax::ExprData::Block { mut declarations } => {
                self.scope.enter_block();
                let last = declarations.pop();

                let mut declarations: Vec<Declaration> = declarations
                    .into_iter()
                    .map(|d| self.lower_decl(d))
                    .collect();

                let expr = match last {
                    Some(syntax::Declaration {
                        it: syntax::DeclarationData::Expr(e),
                        ..
                    }) => self.lower_expr(e),
                    Some(decl) => {
                        declarations.push(self.lower_decl(decl));
                        self.gen_unit()
                    }
                    None => self.gen_unit(),
                };

                self.scope.leave_block();

                ExprData::Block { declarations, expr }
            }
            syntax::ExprData::Struct { name, fields } => {
                let fields = fields
                    .into_iter()
                    .map(|(field_name, expr)| {
                        (
                            self.lookup_field(&name.it, &field_name.it),
                            self.lower_expr(expr),
                        )
                    })
                    .collect();
                let ty_info = self.lookup_ty(&name.it);
                ExprData::Struct {
                    name: ty_info.name,
                    fields,
                }
            }
            syntax::ExprData::StructIdx { expr, index } => {
                let index = self.lookup_field_ty(&expr.ty, &index.it);
                ExprData::StructIdx {
                    expr: self.lower_expr(expr),
                    index,
                }
            }
            syntax::ExprData::Intrinsic {
                intrinsic,
                arguments,
            } => ExprData::Intrinsic {
                intrinsic: self.lower_intrinsic(intrinsic),
                arguments: arguments.into_iter().map(|e| self.lower_expr(e)).collect(),
            },
        };
        Expr {
            it: Box::new(expr_data),
            at: expr.at,
            ty: self.lower_ty(&expr.ty),
        }
    }

    fn set_target_to_expr(&mut self, set_target: syntax::SetTarget) -> Expr {
        let it = match *set_target.it {
            syntax::SetTargetData::Array { target, index } => {
                let array = self.set_target_to_expr(target);
                ExprData::ArrayIdx {
                    array,
                    index: self.lower_expr(index),
                }
            }
            syntax::SetTargetData::Struct { target, index } => {
                let index = self.lookup_field_ty(&target.ty, &index.it);
                let expr = self.set_target_to_expr(target);
                ExprData::StructIdx { expr, index }
            }
            syntax::SetTargetData::Var { name } => ExprData::Var(self.lookup_var(&name.it)),
        };
        Expr {
            it: Box::new(it),
            at: set_target.at,
            ty: self.lower_ty(&set_target.ty),
        }
    }

    fn lower_set_target(&mut self, set_target: syntax::SetTarget) -> SetTarget {
        let set_target_data = match *set_target.it {
            syntax::SetTargetData::Array { target, index } => SetTargetData::Array {
                target: self.set_target_to_expr(target),
                index: self.lower_expr(index),
            },
            syntax::SetTargetData::Struct { target, index } => {
                let index = self.lookup_field_ty(&target.ty, &index.it);
                SetTargetData::Struct {
                    target: self.set_target_to_expr(target),
                    index,
                }
            }
            syntax::SetTargetData::Var { name } => SetTargetData::Var {
                name: self.lookup_var(&name.it),
            },
        };
        SetTarget {
            it: set_target_data,
            at: set_target.at,
            ty: self.lower_ty(&set_target.ty),
        }
    }

    fn lower_decl(&mut self, decl: syntax::Declaration) -> Declaration {
        let declaration_data = match decl.it {
            syntax::DeclarationData::Let {
                binder,
                annotation: _,
                expr,
            } => {
                let expr = self.lower_expr(expr);
                let binder = self.local_idx(binder);
                DeclarationData::Let { binder, expr }
            }
            syntax::DeclarationData::Set { set_target, expr } => DeclarationData::Set {
                set_target: self.lower_set_target(set_target),
                expr: self.lower_expr(expr),
            },
            syntax::DeclarationData::Expr(e) => DeclarationData::Expr(self.lower_expr(e)),
            syntax::DeclarationData::While { condition, body } => DeclarationData::While {
                condition: self.lower_expr(condition),
                body: self.lower_expr(body),
            },
        };
        Declaration {
            it: declaration_data,
            at: decl.at,
            ty: self.lower_ty(&decl.ty),
        }
    }

    fn lower_program(mut self, program: syntax::Program) -> (Program, HashMap<Name, Id>) {
        for ele in program.toplevels.iter() {
            match ele.it {
                ToplevelData::Import { ref internal, .. } => {
                    self.declare_func(internal.clone());
                }
                ToplevelData::Struct {
                    ref name,
                    ref fields,
                } => {
                    self.declare_struct(
                        name.clone(),
                        fields.iter().map(|(name, _)| name.clone()).collect(),
                    );
                }
                ToplevelData::Global { ref binder, .. } => {
                    let global_name = self.global_idx(binder.clone());
                    self.scope.insert(binder.it.clone(), global_name);
                }
                ToplevelData::Func { ref name, .. } => {
                    self.declare_func(name.clone());
                }
            }
        }

        let start_fn = self.func_idx(Id {
            it: "$start".to_string(),
            at: Span::SYN,
        });

        let mut prog = Program {
            imports: vec![],
            structs: vec![],
            globals: vec![],
            funcs: vec![],
            start_fn,
        };

        for toplevel in program.toplevels {
            match toplevel.it {
                ToplevelData::Import { .. } => prog.imports.push(self.lower_import(toplevel)),
                ToplevelData::Struct { .. } => prog.structs.push(self.lower_struct(toplevel)),
                ToplevelData::Global { .. } => prog.globals.push(self.lower_global(toplevel)),
                ToplevelData::Func { .. } => prog.funcs.push(self.lower_func(toplevel)),
            }
        }
        (prog, self.name_map)
    }

    fn lower_op(&self, op: syntax::Op, ty_left: &types::Ty, ty_right: &types::Ty) -> crate::ir::Op {
        let op_data = match (op.it, ty_left, ty_right) {
            (syntax::OpData::Add, types::Ty::I32, types::Ty::I32) => OpData::I32Add,
            (syntax::OpData::Sub, types::Ty::I32, types::Ty::I32) => OpData::I32Sub,
            (syntax::OpData::Mul, types::Ty::I32, types::Ty::I32) => OpData::I32Mul,
            (syntax::OpData::Div, types::Ty::I32, types::Ty::I32) => OpData::I32Div,
            (syntax::OpData::Lt, types::Ty::I32, types::Ty::I32) => OpData::I32Lt,
            (syntax::OpData::Le, types::Ty::I32, types::Ty::I32) => OpData::I32Le,
            (syntax::OpData::Gt, types::Ty::I32, types::Ty::I32) => OpData::I32Gt,
            (syntax::OpData::Ge, types::Ty::I32, types::Ty::I32) => OpData::I32Ge,
            (syntax::OpData::Eq, types::Ty::I32, types::Ty::I32) => OpData::I32Eq,
            (syntax::OpData::Ne, types::Ty::I32, types::Ty::I32) => OpData::I32Ne,

            (syntax::OpData::Add, types::Ty::F32, types::Ty::F32) => OpData::F32Add,
            (syntax::OpData::Sub, types::Ty::F32, types::Ty::F32) => OpData::F32Sub,
            (syntax::OpData::Mul, types::Ty::F32, types::Ty::F32) => OpData::F32Mul,
            (syntax::OpData::Div, types::Ty::F32, types::Ty::F32) => OpData::F32Div,
            (syntax::OpData::Lt, types::Ty::F32, types::Ty::F32) => OpData::F32Lt,
            (syntax::OpData::Le, types::Ty::F32, types::Ty::F32) => OpData::F32Le,
            (syntax::OpData::Gt, types::Ty::F32, types::Ty::F32) => OpData::F32Gt,
            (syntax::OpData::Ge, types::Ty::F32, types::Ty::F32) => OpData::F32Ge,
            (syntax::OpData::Eq, types::Ty::F32, types::Ty::F32) => OpData::F32Eq,
            (syntax::OpData::Ne, types::Ty::F32, types::Ty::F32) => OpData::F32Ne,

            (syntax::OpData::Eq, types::Ty::Bool, types::Ty::Bool) => OpData::BoolEq,
            (syntax::OpData::Ne, types::Ty::Bool, types::Ty::Bool) => OpData::BoolNe,
            (syntax::OpData::And, types::Ty::Bool, types::Ty::Bool) => OpData::BoolAnd,
            (syntax::OpData::Or, types::Ty::Bool, types::Ty::Bool) => OpData::BoolOr,
            (_, _, _) => unreachable!("Can't lower {:?} for types {ty_left} and {ty_right}", op.it),
        };
        Op {
            it: op_data,
            at: op.at,
        }
    }
}

pub fn lower(program: syntax::Program) -> (Program, HashMap<Name, Id>) {
    Lower::new().lower_program(program)
}
