use std::collections::HashMap;

use crate::ir::{Expr, Func, FuncTy, Global, Import, Name, Program, Struct, Ty};
use nemo_frontend::{
    syntax::{self, FuncId, Id, ToplevelData},
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

pub struct Lower {
    // Name supply
    local: u32,
    global: u32,
    func: u32,
    typ: u32,
    field: u32,

    types: HashMap<String, TypeInfo>,
    funcs: HashMap<String, Name>,

    scope: Scope,
    // For debugging purposes (let's us map renamed identifiers back to their original source)
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

    fn func_idx(&mut self, func: FuncId) -> Name {
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
        let name = self.func_idx(func.clone());
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

    fn lookup_func(&self, func: &str) -> Name {
        self.funcs.get(func).unwrap().clone()
    }

    fn lookup_var(&self, var: &str) -> Name {
        self.scope.get(var).unwrap()
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
        }
    }

    fn rename_func_ty(&self, func_ty: &types::FuncTy) -> FuncTy {
        FuncTy {
            arguments: func_ty.arguments.iter().map(|a| self.lower_ty(a)).collect(),
            result: self.lower_ty(&func_ty.result),
        }
    }

    fn rename_import(&self, toplevel: syntax::Toplevel) -> Import {
        if let syntax::ToplevelData::Import {
            internal,
            func_ty,
            external,
        } = toplevel.it
        {
            Import {
                span: toplevel.at,
                internal: self.lookup_func(&internal.it),
                func_ty: self.rename_func_ty(&func_ty.ty),
                external: external.it,
            }
        } else {
            unreachable!("Passed a non-import to rename_import")
        }
    }

    fn rename_struct(&self, toplevel: syntax::Toplevel) -> Struct {
        if let syntax::ToplevelData::Struct { name, fields } = toplevel.it {
            let ty_info = self.lookup_ty(&name.it);
            Struct {
                span: toplevel.at,
                name: ty_info.name,
                fields: fields
                    .into_iter()
                    .map(|(name, ty)| {
                        (
                            ty_info.fields.get(&name.it).unwrap().clone(),
                            self.lower_type(&ty),
                        )
                    })
                    .collect(),
            }
        } else {
            unreachable!("Passed a non-struct to rename_struct")
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

    fn lower_expr(&mut self, expr: syntax::Expr) -> Expr {
        let expr_data = match *expr.it {
            syntax::ExprData::Lit(l) => todo!(),
            syntax::ExprData::Var(v) => todo!(),
            syntax::ExprData::Call { func, arguments } => todo!(),
            syntax::ExprData::Binary { op, left, right } => todo!(),
            syntax::ExprData::Array(_) => todo!(),
            syntax::ExprData::ArrayIdx { array, index } => todo!(),
            syntax::ExprData::If { condition, then_branch, else_branch } => todo!(),
            syntax::ExprData::Block { declarations } => todo!(),
            syntax::ExprData::Struct { name, fields } => todo!(),
            syntax::ExprData::StructIdx { expr, index } => todo!(),
            syntax::ExprData::Intrinsic { intrinsic, arguments } => todo!(),
        };
        Expr { it: expr_data, at: expr.at, ty: expr.ty }
    }

    pub fn rename_program(mut self, program: syntax::Program) -> (Program, HashMap<Name, Id>) {
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

        let mut prog = Program {
            imports: vec![],
            structs: vec![],
            globals: vec![],
            funcs: vec![],
        };

        for toplevel in program.toplevels {
            match toplevel.it {
                ToplevelData::Import { .. } => prog.imports.push(self.rename_import(toplevel)),
                ToplevelData::Struct { .. } => prog.structs.push(self.rename_struct(toplevel)),
                ToplevelData::Global { .. } => prog.globals.push(self.lower_global(toplevel)),
                ToplevelData::Func { .. } => prog.funcs.push(self.lower_func(toplevel)),
            }
        }

        (prog, self.name_map)
    }
}
