use std::collections::HashMap;

use crate::ir::{Declaration, FuncTy, Import, Name, Program, Ty, Type, Expr};
use nemo_frontend::syntax::{self, Spanned, Toplevel};

struct Scope(Vec<HashMap<String, Name>>);

impl Scope {
    fn new() -> Scope {
        Scope(vec![HashMap::new()])
    }

    fn insert(&mut self, s: String, name: Name) {
        self.0.last_mut().unwrap().insert(s, name);
    }

    fn get(&self, s: String) -> Option<Name> {
        self.0.iter().rev().find_map(|block| block.get(&s)).copied()
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

pub struct Renamer {
    // Name supply
    local: u32,
    global: u32,
    func: u32,
    typ: u32,
    field: u32,

    types: HashMap<String, TypeInfo>,
    funcs: HashMap<String, Name>,

    scope: Scope,
    name_map: HashMap<Name, Spanned<String>>,
}

impl Renamer {
    fn new() -> Renamer {
        Renamer {
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

    fn local_idx(&mut self, local: Spanned<String>) -> Name {
        self.local += 1;
        let name = Name::Local(self.local);
        self.name_map.insert(name, local);
        name
    }

    fn global_idx(&mut self, global: Spanned<String>) -> Name {
        self.global += 1;
        let name = Name::Global(self.global);
        self.name_map.insert(name, global);
        name
    }

    fn func_idx(&mut self, func: Spanned<String>) -> Name {
        self.func += 1;
        let name = Name::Func(self.func);
        self.name_map.insert(name, func);
        name
    }

    fn type_idx(&mut self, typ: Spanned<String>) -> Name {
        self.typ += 1;
        let name = Name::Type(self.typ);
        self.name_map.insert(name, typ);
        name
    }

    fn field_idx(&mut self, field: Spanned<String>) -> Name {
        self.field += 1;
        let name = Name::Field(self.field);
        self.name_map.insert(name, field);
        name
    }

    fn declare_func(&mut self, func: Spanned<String>) {
        let name = self.func_idx(func.clone());
        self.funcs.insert(func.it, name);
    }

    fn declare_type(&mut self, ty: Spanned<String>, fields: Vec<Spanned<String>>) {
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

    fn rename_ty(&self, ty: syntax::Ty) -> Ty {
        match ty {
            syntax::Ty::I32 => Ty::I32,
            syntax::Ty::F32 => Ty::F32,
            syntax::Ty::Unit => Ty::Unit,
            syntax::Ty::Bool => Ty::Bool,
            syntax::Ty::Array(t) => Ty::Array(Box::new(self.rename_ty(*t))),
            syntax::Ty::Struct(s) => Ty::Struct(self.lookup_ty(&s).name),
        }
    }

    fn rename_func_ty(&self, func_ty: syntax::FuncTy) -> FuncTy {
        FuncTy {
            arguments: func_ty
                .arguments
                .into_iter()
                .map(|a| self.rename_ty(a))
                .collect(),
            result: self.rename_ty(func_ty.result),
        }
    }

    fn rename_expr(&mut self, expr: syntax::Expr) -> Expr {
        todo!()
    }

    pub fn rename_program(
        mut self,
        program: syntax::Program,
    ) -> (Program, HashMap<Name, Spanned<String>>) {
        for ele in program.toplevels.iter() {
            match ele {
                Toplevel::TopImport { ref internal, .. } => {
                    self.declare_func(internal.clone());
                }
                Toplevel::TopStruct {
                    ref name,
                    ref fields,
                } => {
                    self.declare_type(
                        name.clone(),
                        fields.iter().map(|f| f.name.clone()).collect(),
                    );
                }
                Toplevel::TopLet { ref binder, .. } => {
                    let spanned_binder = Spanned::new(binder.at.clone(), binder.it.clone());
                    let global_name = self.global_idx(spanned_binder);
                    self.scope.insert(binder.it.clone(), global_name);
                }
                Toplevel::TopFunc { ref name, .. } => {
                    self.declare_func(name.clone());
                }
            }
        }

        let mut prog = Program {
            imports: vec![],
            types: vec![],
            globals: vec![],
            funcs: vec![],
        };

        for ele in program.toplevels {
            match ele {
                Toplevel::TopImport {
                    internal,
                    func_ty,
                    external,
                } => {
                    let ty = self.rename_func_ty(func_ty.it);
                    let internal = self.lookup_func(&internal.it);
                    prog.imports.push(Import {
                        internal,
                        ty,
                        external: external.it,
                    })
                }
                Toplevel::TopStruct { name, fields } => {
                    let ty_info = self.lookup_ty(&name.it);
                    let fields = fields
                        .into_iter()
                        .map(|f| {
                            let field_name = ty_info.fields.get(&f.name.it).unwrap();
                            let ty = self.rename_ty(f.ty.it);
                            (field_name.clone(), ty)
                        })
                        .collect();
                    prog.types.push(Type {
                        name: ty_info.name,
                        fields,
                    })
                }
                Toplevel::TopLet { binder, expr } => {}
                Toplevel::TopFunc {
                    name,
                    params,
                    return_ty,
                    body,
                } => {}
            }
        }

        (prog, self.name_map)
    }
}
