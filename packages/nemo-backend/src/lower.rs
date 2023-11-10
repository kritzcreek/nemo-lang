use std::collections::HashMap;

use crate::ir::{Name, Ty, FuncTy, Program};
use nemo_frontend::{syntax::{self, Toplevel, Id, ToplevelData, FuncId}, types};

// Tracks local names
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
        self.name_map.insert(name, Id { it: func.it, at: func.at });
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

    fn lower_ty(&self, ty: types::Ty) -> Ty {
        match ty {
            types::Ty::I32 => Ty::I32,
            types::Ty::F32 => Ty::F32,
            types::Ty::Unit => Ty::Unit,
            types::Ty::Bool => Ty::Bool,
            types::Ty::Array(t) => Ty::Array(Box::new(self.lower_ty(*t))),
            types::Ty::Struct(s) => Ty::Struct(self.lookup_ty(&s).name),
        }
    }

    fn rename_func_ty(&self, func_ty: types::FuncTy) -> FuncTy {
        FuncTy {
            arguments: func_ty
                .arguments
                .into_iter()
                .map(|a| self.lower_ty(a))
                .collect(),
            result: self.lower_ty(func_ty.result),
        }
    }

    pub fn rename_program(
        mut self,
        program: syntax::Program,
    ) -> (Program, HashMap<Name, Id>) {
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
        (prog, self.name_map)
    }
}
