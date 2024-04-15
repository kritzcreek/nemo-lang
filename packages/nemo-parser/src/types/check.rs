use std::collections::HashMap;

use super::errors::TyError;
use super::names::{Name, NameSupply};
use super::{FuncTy, Ty};
use crate::syntax::token_ptr::SyntaxTokenPtr;
use crate::syntax::{nodes::*, SyntaxNodePtr, SyntaxToken};

#[derive(Debug)]
struct StructDef {
    fields: HashMap<String, (Ty, Name)>,
}

// NOTE: We could consider passing an explicit Ctx around,
// but we'd need to make it immutable and persistent
#[derive(Debug)]
struct Ctx {
    values: Vec<HashMap<String, (Ty, Name)>>,
    types: HashMap<String, (Option<StructDef>, Name)>,
}

impl Ctx {
    fn new() -> Ctx {
        Ctx {
            values: vec![],
            types: HashMap::new(),
        }
    }

    fn add_var(&mut self, v: String, ty: Ty, name: Name) {
        self.values.last_mut().unwrap().insert(v, (ty, name));
    }

    fn lookup_var(&self, v: &str) -> Option<&(Ty, Name)> {
        self.values.iter().rev().find_map(|scope| scope.get(v))
    }

    fn declare_type(&mut self, v: &str, name: Name) {
        self.types.insert(v.to_string(), (None, name));
    }

    fn add_struct_fields(&mut self, v: &str, def: StructDef) {
        let declared = &mut self
            .types
            .get_mut(v)
            .expect("expected type to be forward-declared")
            .0;
        *declared = Some(def)
    }

    fn lookup_type_declared(&self, v: &str) -> Option<&(Option<StructDef>, Name)> {
        self.types.get(v)
    }

    fn lookup_type(&self, v: &str) -> Option<(&StructDef, Name)> {
        self.types.get(v).map(|(def, name)| {
            (
                def.as_ref().expect("can't lookup forward-declared type"),
                *name,
            )
        })
    }

    fn enter_block(&mut self) {
        self.values.push(HashMap::new())
    }

    fn leave_block(&mut self) {
        self.values.pop().expect("Tried to pop from an empty Ctx");
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Occurence<N> {
    Def(N),
    Ref(N),
}

pub struct Typechecker {
    pub typed_nodes: HashMap<SyntaxNodePtr, Ty>,
    pub names: HashMap<SyntaxTokenPtr, Occurence<Name>>,
    pub errors: Vec<TyError>,

    pub name_supply: NameSupply,
    context: Ctx,
}

impl Typechecker {
    pub fn new() -> Typechecker {
        Typechecker {
            typed_nodes: HashMap::new(),
            names: HashMap::new(),
            errors: Vec::new(),

            name_supply: NameSupply::new(),
            context: Ctx::new(),
        }
    }

    fn record_def(&mut self, token: &SyntaxToken, name: Name) {
        let previous = self
            .names
            .insert(SyntaxTokenPtr::new(&token), Occurence::Def(name));
        assert!(previous.is_none())
    }

    fn record_ref(&mut self, token: &SyntaxToken, name: Name) {
        let previous = self
            .names
            .insert(SyntaxTokenPtr::new(&token), Occurence::Ref(name));
        assert!(previous.is_none())
    }

    pub fn infer_program(&mut self, root: Root) {
        self.context.enter_block();

        self.check_type_definitions(&root);
        self.check_imports(&root);
        self.check_function_headers(&root);

        self.check_globals(&root);
        // self.check_function_bodies(&root);

        self.context.leave_block();
    }

    fn check_imports(&mut self, root: &Root) {
        for top_level in root.top_levels() {
            match top_level {
                TopLevel::TopImport(i) => {
                    let Some(internal_name_tkn) = i.imp_internal().and_then(|x| x.ident_token())
                    else {
                        continue;
                    };
                    let func_ty = i.ty().map(|t| self.check_ty(&t)).unwrap_or(Ty::Any);
                    let name = self.name_supply.func_idx(&internal_name_tkn);
                    self.record_def(&internal_name_tkn, name);

                    self.context
                        .add_var(internal_name_tkn.text().to_string(), func_ty, name)
                }
                _ => {}
            }
        }
    }

    fn check_type_definitions(&mut self, root: &Root) {
        let top_levels: Vec<TopLevel> = root.top_levels().collect();
        // Because types can be mutually recursive we need two passes:
        // - 1. Forward declare all types
        for top_level in top_levels.iter() {
            match top_level {
                TopLevel::TopStruct(s) => {
                    if let Some(tkn) = s.upper_ident_token() {
                        let name = self.name_supply.type_idx(&tkn);
                        self.record_def(&tkn, name);

                        self.context.declare_type(tkn.text(), name)
                    }
                }
                _ => {}
            }
        }

        // - 2. Actually check their definitions
        for top_level in top_levels.iter() {
            match top_level {
                TopLevel::TopStruct(s) => {
                    if let Some(struct_name_tkn) = s.upper_ident_token() {
                        let mut def = StructDef {
                            fields: HashMap::new(),
                        };
                        for field in s.struct_fields() {
                            let Some(field_name) = field.ident_token() else {
                                // TODO error
                                continue;
                            };
                            let ty = match field.ty() {
                                Some(field_ty) => self.check_ty(&field_ty),
                                None => {
                                    // TODO: report error
                                    Ty::Any
                                }
                            };
                            let name = self.name_supply.field_idx(&field_name);
                            self.record_def(&field_name, name);

                            def.fields.insert(field_name.text().to_string(), (ty, name));
                        }
                        self.context.add_struct_fields(struct_name_tkn.text(), def)
                    }
                }
                _ => {}
            }
        }
    }

    fn check_function_headers(&mut self, root: &Root) {
        for top_level in root.top_levels() {
            match top_level {
                TopLevel::TopFn(top_fn) => {
                    let Some(fn_name_tkn) = top_fn.ident_token() else {
                        continue;
                    };
                    let mut arguments = vec![];
                    for param in top_fn.params() {
                        let ty = param.ty().map(|t| self.check_ty(&t)).unwrap_or(Ty::Any);
                        arguments.push(ty);
                    }
                    let name = self.name_supply.func_idx(&fn_name_tkn);
                    self.record_def(&fn_name_tkn, name);
                    let result = top_fn.ty().map(|t| self.check_ty(&t)).unwrap_or(Ty::Unit);
                    self.context.add_var(
                        fn_name_tkn.text().to_string(),
                        Ty::Func(Box::new(FuncTy { arguments, result })),
                        name,
                    )
                }
                _ => {}
            }
        }
    }

    fn check_ty(&mut self, ty: &Type) -> Ty {
        match ty {
            Type::TyInt(_) => Ty::I32,
            Type::TyFloat(_) => Ty::F32,
            Type::TyBool(_) => Ty::Bool,
            Type::TyUnit(_) => Ty::Unit,
            Type::TyArray(t) => match t.elem().map(|e| self.check_ty(&e)) {
                Some(elem_ty) => Ty::Array(Box::new(elem_ty)),
                None => {
                    // TODO report error
                    Ty::Array(Box::new(Ty::Any))
                }
            },
            Type::TyCons(t) => {
                let ty_name = t.upper_ident_token().unwrap();
                let name = match self.context.lookup_type_declared(ty_name.text()) {
                    Some((_, name)) => *name,
                    None => return Ty::Any,
                };
                self.record_ref(&ty_name, name);
                Ty::Struct(name)
            }
            Type::TyFn(t) => {
                let mut arguments = vec![];
                match t.ty_arg_list() {
                    Some(arg_list) => {
                        for arg in arg_list.types() {
                            arguments.push(self.check_ty(&arg))
                        }
                    }
                    None => {
                        // TODO error
                    }
                }
                let result = t.result().map_or_else(
                    || {
                        // TODO error
                        Ty::Any
                    },
                    |t| self.check_ty(&t),
                );
                let func_ty = FuncTy { arguments, result };
                Ty::Func(Box::new(func_ty))
            }
        }
    }

    fn check_globals(&mut self, root: &Root) {
        for top_level in root.top_levels() {
            match top_level {
                TopLevel::TopLet(top_let) => {
                    let binder_tkn = top_let.ident_token();
                    let ty = match (top_let.ty().map(|t| self.check_ty(&t)), top_let.expr()) {
                        (None, None) => continue,
                        (Some(ty), None) => ty,
                        (None, Some(e)) => {
                            self.infer_expr(&e);
                            // No name, so can't add to context
                            continue;
                        }
                        (Some(t), Some(e)) => {
                            self.check_expr(&e, &t);
                            t
                        }
                    };
                    // TODO add to context
                }
                _ => {}
            }
        }
    }

    fn infer_expr(&mut self, expr: &Expr) -> Ty {
        match expr {
            Expr::EArray(arr) => {
                let mut elems = arr.exprs();
                if let Some(first_elem) = elems.next() {
                    let elem_ty = self.infer_expr(&first_elem);
                    for elem in elems {
                        self.check_expr(&elem, &elem_ty);
                    }
                    Ty::Array(Box::new(elem_ty))
                } else {
                    // TODO error
                    Ty::Array(Box::new(Ty::Any))
                }
            }
            _ => {
                // TODO error
                Ty::Any
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr, expected: &Ty) {
        match (expr, expected) {
            (Expr::EArray(expr), Ty::Array(elem_ty)) => {
                for elem in expr.exprs() {
                    self.check_expr(&elem, &**elem_ty);
                }
            }
            (Expr::EIf(expr), ty) => {
                if let Some(condition) = expr.condition() {
                    self.check_expr(&condition, &Ty::Bool)
                }
                // TODO after custom impl for branches

                // if let Some(then_branch) = expr.then_branch() {
                //     self.check_expr(&then_branch, ty)
                // }
                // if let Some(else_branch) = expr.else_branch() {
                //     self.check_expr(&else_branch, ty)
                // }
            }
            _ => {
                let ty = self.infer_expr(expr);
                // TODO match types (special logic for Any)
                if ty != *expected {
                    // TODO error
                }
            }
        }
    }
}
