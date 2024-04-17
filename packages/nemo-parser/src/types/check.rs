use super::errors::{
    TyError,
    TyErrorData::{self, *},
};
use super::names::{Name, NameSupply};
use super::{FuncTy, Ty};
use crate::syntax::ast::AstNode;
use crate::syntax::token_ptr::SyntaxTokenPtr;
use crate::syntax::{nodes::*, SyntaxNode, SyntaxNodePtr, SyntaxToken};
use crate::T;
use crate::{
    builtins::{self, lookup_builtin},
    lexer::{is_whitespace, SyntaxKind},
};
use nemo_backend::ir::{self, OpData};
use rowan::TextRange;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
struct StructDef {
    fields: HashMap<String, (Ty, Name)>,
}

// NOTE: We could consider passing an explicit Ctx around,
// but we'd need to make it immutable and persistent
#[derive(Debug)]
struct Ctx {
    values: Vec<HashMap<String, (Ty, Name)>>,
    types_names: HashMap<String, Name>,
    types_defs: HashMap<Name, Rc<StructDef>>,
}

impl Ctx {
    fn new() -> Ctx {
        Ctx {
            values: vec![],
            types_names: HashMap::new(),
            types_defs: HashMap::new(),
        }
    }

    fn add_var(&mut self, v: String, ty: Ty, name: Name) {
        self.values.last_mut().unwrap().insert(v, (ty, name));
    }

    fn lookup_var(&self, v: &str) -> Option<&(Ty, Name)> {
        self.values.iter().rev().find_map(|scope| scope.get(v))
    }

    fn declare_type(&mut self, v: &str, name: Name) {
        self.types_names.insert(v.to_string(), name);
    }

    fn declare_struct_fields(&mut self, v: &str, def: StructDef) {
        let name = self
            .types_names
            .get(v)
            .expect("expected type to be forward-declared");
        self.types_defs.insert(*name, Rc::new(def));
    }

    fn lookup_type_name(&self, v: &str) -> Option<Name> {
        self.types_names.get(v).copied()
    }

    fn lookup_type_def(&self, name: Name) -> Option<Rc<StructDef>> {
        self.types_defs.get(&name).cloned()
    }

    fn lookup_type(&self, v: &str) -> Option<(Rc<StructDef>, Name)> {
        let n = self.lookup_type_name(v)?;
        let def = self
            .lookup_type_def(n)
            .expect("type declared but not defined");
        Some((def, n))
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

    fn record_typed(&mut self, node: &SyntaxNode, ty: &Ty) {
        let previous = self
            .typed_nodes
            .insert(SyntaxNodePtr::new(node), ty.clone());
        assert!(previous.is_none())
    }

    fn report_error_token(&mut self, elem: &SyntaxToken, error: TyErrorData) {
        self.errors.push(TyError {
            at: elem.text_range(),
            it: error,
        })
    }

    fn report_error<N: AstNode>(&mut self, elem: &N, error: TyErrorData) {
        let at = elem.syntax().text_range();
        let mut start = at.start();
        let mut end = at.end();
        let mut children = elem.syntax().descendants_with_tokens();
        while let Some(elem) = children.next() {
            if elem.as_token().is_some() && !is_whitespace(elem.kind()) {
                start = elem.text_range().start();
                break;
            }
        }

        while let Some(elem) = children.next() {
            if elem.as_token().is_some() && !is_whitespace(elem.kind()) {
                end = elem.text_range().end();
            }
        }

        self.errors.push(TyError {
            at: TextRange::new(start, end),
            it: error,
        })
    }

    pub fn infer_program(&mut self, root: Root) {
        self.context.enter_block();

        self.check_type_definitions(&root);
        self.check_imports(&root);
        self.check_function_headers(&root);

        self.check_globals(&root);
        self.check_function_bodies(&root);

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
                                continue;
                            };
                            let ty = match field.ty() {
                                Some(field_ty) => self.check_ty(&field_ty),
                                None => Ty::Any,
                            };
                            let name = self.name_supply.field_idx(&field_name);
                            self.record_def(&field_name, name);

                            def.fields.insert(field_name.text().to_string(), (ty, name));
                        }
                        self.context
                            .declare_struct_fields(struct_name_tkn.text(), def)
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
                None => Ty::Array(Box::new(Ty::Any)),
            },
            Type::TyCons(t) => {
                let ty_name = t.upper_ident_token().unwrap();
                let Some(name) = self.context.lookup_type_name(ty_name.text()) else {
                    self.report_error_token(&ty_name, UnknownType(ty_name.text().to_string()));
                    return Ty::Any;
                };
                self.record_ref(&ty_name, name);
                Ty::Struct(name)
            }
            Type::TyFn(t) => {
                let mut arguments = vec![];
                if let Some(arg_list) = t.ty_arg_list() {
                    for arg in arg_list.types() {
                        arguments.push(self.check_ty(&arg))
                    }
                }
                let result = t.result().map_or_else(|| Ty::Any, |t| self.check_ty(&t));
                let func_ty = FuncTy { arguments, result };
                Ty::Func(Box::new(func_ty))
            }
        }
    }

    fn check_globals(&mut self, root: &Root) {
        for top_level in root.top_levels() {
            match top_level {
                TopLevel::TopLet(top_let) => {
                    let ty = match (top_let.ty().map(|t| self.check_ty(&t)), top_let.expr()) {
                        (None, None) => continue,
                        (Some(ty), None) => ty,
                        (None, Some(e)) => self.infer_expr(&e),
                        (Some(ty), Some(e)) => {
                            self.check_expr(&e, &ty);
                            ty
                        }
                    };
                    if let Some(binder_tkn) = top_let.ident_token() {
                        let name = self.name_supply.global_idx(&binder_tkn);
                        self.record_def(&binder_tkn, name);
                        self.context
                            .add_var(binder_tkn.text().to_string(), ty, name)
                    };
                }
                _ => {}
            }
        }
    }

    fn check_function_bodies(&mut self, root: &Root) {
        for top_level in root.top_levels() {
            match top_level {
                TopLevel::TopFn(top_fn) => {
                    let Some(func_name) = top_fn.ident_token() else {
                        continue;
                    };
                    let Some((Ty::Func(func_ty), _)) = self.context.lookup_var(func_name.text())
                    else {
                        panic!("didn't pre-declare function, {}", func_name.text())
                    };

                    // Use Rc for Tys in the context?
                    let func_ty = func_ty.clone();

                    self.context.enter_block();
                    for (param, ty) in top_fn.params().zip(func_ty.arguments.into_iter()) {
                        let Some(ident_tkn) = param.ident_token() else {
                            continue;
                        };
                        let name = self.name_supply.local_idx(&ident_tkn);
                        self.record_def(&ident_tkn, name);
                        self.context.add_var(ident_tkn.text().to_string(), ty, name);
                    }

                    if let Some(body) = top_fn.body() {
                        self.check_expr(&body.into(), &func_ty.result)
                    }

                    self.context.leave_block();
                }
                _ => {}
            }
        }
    }

    fn infer_expr(&mut self, expr: &Expr) -> Ty {
        match self.infer_expr_inner(expr) {
            Some(ty) => {
                self.record_typed(expr.syntax(), &ty);
                ty
            }
            None => Ty::Any,
        }
    }

    fn infer_expr_inner(&mut self, expr: &Expr) -> Option<Ty> {
        let ty = match expr {
            Expr::EArray(arr) => {
                let mut elems = arr.exprs();
                if let Some(first_elem) = elems.next() {
                    let elem_ty = self.infer_expr(&first_elem);
                    for elem in elems {
                        self.check_expr(&elem, &elem_ty);
                    }
                    Ty::Array(Box::new(elem_ty))
                } else {
                    self.report_error(arr, CantInferEmptyArray);
                    Ty::Array(Box::new(Ty::Any))
                }
            }
            Expr::ELit(l) => match l.literal().unwrap() {
                Literal::LitBool(_) => Ty::Bool,
                Literal::LitFloat(l) => {
                    let float_tkn = l.float_lit_token().unwrap();
                    match float_tkn.text().parse::<f32>() {
                        Err(_) => {
                            self.report_error_token(&float_tkn, InvalidLiteral);
                        }
                        _ => {}
                    }
                    Ty::F32
                }
                Literal::LitInt(l) => {
                    let int_tkn = l.int_lit_token().unwrap();
                    match int_tkn.text().parse::<i32>() {
                        Err(_) => {
                            self.report_error_token(&int_tkn, InvalidLiteral);
                        }
                        _ => {}
                    }
                    Ty::I32
                }
            },
            Expr::EVar(v) => {
                let var_tkn = v.ident_token().unwrap();
                match self.context.lookup_var(var_tkn.text()) {
                    None => match lookup_builtin(var_tkn.text()) {
                        Some(builtin) => Ty::Func(Box::new(builtin.ty.clone())),
                        None => {
                            self.report_error_token(
                                &var_tkn,
                                UnknownVar(var_tkn.text().to_string()),
                            );
                            return None;
                        }
                    },
                    Some((ty, name)) => {
                        let return_ty = ty.clone();
                        self.record_ref(&var_tkn, *name);
                        return_ty
                    }
                }
            }
            Expr::EStruct(struct_expr) => {
                let struct_name_tkn = struct_expr.upper_ident_token().unwrap();
                match self.context.lookup_type(struct_name_tkn.text()) {
                    None => {
                        self.report_error_token(
                            &struct_name_tkn,
                            UnknownType(struct_name_tkn.text().to_string()),
                        );
                        return None;
                    }
                    Some((def, name)) => {
                        // TODO compute missing fields
                        for field in struct_expr.e_struct_fields() {
                            let Some(field_tkn) = field.ident_token() else {
                                continue;
                            };
                            let Some(field_expr) = field.expr() else {
                                continue;
                            };

                            let Some((ty, name)) = def.fields.get(field_tkn.text()) else {
                                self.report_error_token(
                                    &field_tkn,
                                    UnknownField {
                                        struct_name: struct_name_tkn.text().to_string(),
                                        field_name: field_tkn.text().to_string(),
                                    },
                                );
                                continue;
                            };
                            self.record_ref(&field_tkn, *name);
                            self.check_expr(&field_expr, ty)
                        }
                        Ty::Struct(name)
                    }
                }
            }
            Expr::ECall(call_expr) => {
                let func_expr = call_expr.expr()?;
                match self.infer_expr(&func_expr) {
                    Ty::Func(func_ty) => {
                        if let Some(arg_list) = call_expr.e_arg_list() {
                            let arg_exprs: Vec<Expr> = arg_list.exprs().collect();
                            let arg_tys = func_ty.arguments;
                            if arg_exprs.len() != arg_tys.len() {
                                self.report_error(
                                    &arg_list,
                                    ArgCountMismatch(arg_tys.len(), arg_exprs.len()),
                                )
                            }
                            for (param, expected_ty) in arg_exprs.iter().zip(arg_tys.iter()) {
                                self.check_expr(param, expected_ty)
                            }
                        }
                        func_ty.result
                    }
                    ty => {
                        if ty != Ty::Any {
                            self.report_error(&func_expr, NotAFunction(ty));
                        }
                        return None;
                    }
                }
            }
            Expr::EIntrinsic(intrinsic_expr) => {
                let intrinsic_tkn = intrinsic_expr.at_ident_token().unwrap();
                let args: Vec<Expr> = intrinsic_expr.e_arg_list().unwrap().exprs().collect();
                match (intrinsic_tkn.text(), args.len()) {
                    ("@array_len", 1) => {
                        let ty = self.infer_expr(&args[0]);
                        if !matches!(ty, Ty::Array(_)) {
                            self.report_error(
                                &args[0],
                                Message(format!("expected an array type, but got {:?}", ty)),
                            )
                        }
                        Ty::I32
                    }
                    ("@array_new", 2) => {
                        let elem_ty = self.infer_expr(&args[0]);
                        self.check_expr(&args[1], &Ty::I32);
                        Ty::Array(Box::new(elem_ty))
                    }
                    (f, arg_count) => {
                        self.report_error_token(
                            &intrinsic_tkn,
                            UnknownIntrinsic(f.to_string(), arg_count),
                        );
                        Ty::Any
                    }
                }
            }
            Expr::EParen(e) => self.infer_expr(&e.expr()?),
            Expr::EIf(if_expr) => {
                if let Some(condition) = if_expr.condition() {
                    self.check_expr(&condition, &Ty::Bool)
                }
                if let Some(then_branch) = if_expr.then_branch() {
                    let ty = self.infer_expr(&then_branch);
                    if let Some(else_branch) = if_expr.else_branch() {
                        self.check_expr(&else_branch, &ty)
                    }
                    ty
                } else {
                    Ty::Any
                }
            }
            Expr::EArrayIdx(idx_expr) => {
                let arr_expr = idx_expr.expr().unwrap();
                let elem_ty = match self.infer_expr(&arr_expr) {
                    Ty::Array(elem_ty) => *elem_ty,
                    ty => {
                        if ty != Ty::Any {
                            self.report_error(&arr_expr, NonArrayIdx(ty))
                        }
                        return None;
                    }
                };
                if let Some(index) = idx_expr.index() {
                    self.check_expr(&index, &Ty::I32);
                }
                elem_ty
            }
            Expr::EStructIdx(idx_expr) => {
                let struct_expr = idx_expr.expr().unwrap();
                let (def, name) = match self.infer_expr(&struct_expr) {
                    Ty::Struct(name) => (
                        self.context
                            .lookup_type_def(name)
                            .expect("inferred a type that wasn't defined"),
                        name,
                    ),
                    ty => {
                        if ty != Ty::Any {
                            self.report_error(&struct_expr, NonStructIdx(ty))
                        }
                        return None;
                    }
                };
                let field_name_tkn = idx_expr.ident_token()?;
                match def.fields.get(field_name_tkn.text()) {
                    None => {
                        // TODO this isn't ideal, maybe we should require a
                        // NameSupply when rendering the errors
                        let struct_name = self.name_supply.lookup(name).unwrap().it.clone();
                        self.report_error_token(
                            &field_name_tkn,
                            UnknownField {
                                struct_name,
                                field_name: field_name_tkn.text().to_string(),
                            },
                        );
                        return None;
                    }
                    Some((t, n)) => {
                        self.record_ref(&field_name_tkn, *n);
                        t.clone()
                    }
                }
            }
            Expr::EBinary(bin_expr) => {
                let lhs_ty = self.infer_expr(&bin_expr.lhs()?);
                // TODO could maybe check the rhs based on operator and lhs?
                let rhs_ty = self.infer_expr(&bin_expr.rhs()?);
                let op_tkn = bin_expr.op()?;
                match check_op(&op_tkn, &lhs_ty, &rhs_ty) {
                    None => {
                        self.report_error_token(
                            &op_tkn,
                            Message(format!(
                                "Invalid operator {} for lhs of type {lhs_ty} and rhs of type {rhs_ty}", op_tkn.text()
                            )),
                        );
                        return None;
                    }
                    Some((_, ty)) => ty,
                }
            }
            Expr::EBlock(block_expr) => {
                self.context.enter_block();
                let mut ty = Ty::Unit;
                for decl in block_expr.declarations() {
                    ty = self.infer_decl(&decl);
                }
                self.context.leave_block();
                ty
            }
        };

        Some(ty)
    }

    fn check_expr(&mut self, expr: &Expr, expected: &Ty) {
        match (expr, expected) {
            (Expr::EArray(expr), Ty::Array(elem_ty)) => {
                for elem in expr.exprs() {
                    self.check_expr(&elem, &**elem_ty);
                }
            }
            (Expr::EArrayIdx(idx_expr), elem_ty) => {
                let arr_expr = idx_expr.expr().unwrap();
                let elem_ty = self.check_expr(&arr_expr, &Ty::Array(Box::new(elem_ty.clone())));
                if let Some(index) = idx_expr.index() {
                    self.check_expr(&index, &Ty::I32);
                }
                elem_ty
            }
            (Expr::EIf(expr), ty) => {
                if let Some(condition) = expr.condition() {
                    self.check_expr(&condition, &Ty::Bool)
                }
                if let Some(then_branch) = expr.then_branch() {
                    self.check_expr(&then_branch, ty)
                }
                if let Some(else_branch) = expr.else_branch() {
                    self.check_expr(&else_branch, ty)
                }
            }
            (Expr::EParen(expr), _) => {
                if let Some(expr) = expr.expr() {
                    self.check_expr(&expr, expected)
                }
            }
            _ => {
                let ty = self.infer_expr(expr);
                // TODO match types (special logic for Any)
                if *expected != Ty::Any && ty != Ty::Any && ty != *expected {
                    self.errors.push(TyError {
                        at: expr.syntax().text_range(),
                        it: TypeMismatch {
                            expected: expected.clone(),
                            actual: ty,
                        },
                    })
                }
                return;
            }
        }
        self.record_typed(expr.syntax(), expected)
    }

    fn infer_decl(&mut self, decl: &Declaration) -> Ty {
        match decl {
            Declaration::DLet(let_decl) => {
                let ty = if let Some(expr) = let_decl.expr() {
                    if let Some(ty) = let_decl.ty().map(|ta| self.check_ty(&ta)) {
                        self.check_expr(&expr, &ty);
                        ty
                    } else {
                        self.infer_expr(&expr)
                    }
                } else {
                    Ty::Any
                };

                if let Some(binder_tkn) = let_decl.ident_token() {
                    let name = self.name_supply.local_idx(&binder_tkn);
                    self.record_def(&binder_tkn, name);
                    self.context
                        .add_var(binder_tkn.text().to_string(), ty, name)
                }
                Ty::Unit
            }
            Declaration::DSet(set_decl) => {
                let set_ty = if let Some(set_target) = set_decl.set_target() {
                    self.infer_set_target(&set_target)
                } else {
                    Ty::Any
                };
                if let Some(expr) = set_decl.expr() {
                    self.check_expr(&expr, &set_ty);
                }
                Ty::Unit
            }
            Declaration::DWhile(while_decl) => {
                let condition = while_decl.expr();
                if let Some(condition) = condition {
                    self.check_expr(&condition, &Ty::Bool)
                }
                let body = while_decl.e_block();
                if let Some(body) = body {
                    self.check_expr(&body.into(), &Ty::Unit)
                }
                Ty::Unit
            }
            Declaration::DExpr(expr_decl) => self.infer_expr(&expr_decl.expr().unwrap()),
        }
    }

    fn infer_set_target(&mut self, set_target: &SetTarget) -> Ty {
        let Some(ident_tkn) = set_target.ident_token() else {
            return Ty::Any;
        };
        let Some((ty, name)) = self.context.lookup_var(ident_tkn.text()) else {
            self.report_error_token(&ident_tkn, UnknownVar(ident_tkn.text().to_string()));
            return Ty::Any;
        };
        let mut ty = ty.clone();
        self.record_ref(&ident_tkn, *name);

        for indirection in set_target.set_indirections() {
            match (indirection, &ty) {
                (SetIndirection::SetStruct(set_struct), Ty::Struct(name)) => {
                    let def = self
                        .context
                        .lookup_type_def(*name)
                        .expect("inferred unknown struct type");
                    let Some(ident_tkn) = set_struct.ident_token() else {
                        return Ty::Any;
                    };
                    if let Some((field, name)) = def.fields.get(ident_tkn.text()) {
                        self.record_ref(&ident_tkn, *name);
                        ty = field.clone()
                    } else {
                        let struct_name = self.name_supply.lookup(*name).unwrap().it.clone();
                        self.report_error_token(
                            &ident_tkn,
                            UnknownField {
                                struct_name,
                                field_name: ident_tkn.text().to_string(),
                            },
                        );
                        return Ty::Any;
                    }
                }
                (SetIndirection::SetStruct(set_struct), ty) => {
                    self.report_error(&set_struct, NonStructIdx(ty.clone()));
                    return Ty::Any;
                }
                (SetIndirection::SetArray(set_array), Ty::Array(elem_ty)) => {
                    if let Some(index) = set_array.expr() {
                        self.check_expr(&index, &Ty::I32)
                    }
                    ty = (**elem_ty).clone();
                }
                (SetIndirection::SetArray(set_array), ty) => {
                    self.report_error(&set_array, NonArrayIdx(ty.clone()));
                    return Ty::Any;
                }
            }
        }
        ty
    }
}

fn check_op(op: &SyntaxToken, ty_left: &Ty, ty_right: &Ty) -> Option<(ir::OpData, Ty)> {
    let op_data = match (op.kind(), ty_left, ty_right) {
        (T![+], Ty::I32, Ty::I32) => (OpData::I32Add, Ty::I32),
        (T![-], Ty::I32, Ty::I32) => (OpData::I32Sub, Ty::I32),
        (T![*], Ty::I32, Ty::I32) => (OpData::I32Mul, Ty::I32),
        (T![/], Ty::I32, Ty::I32) => (OpData::I32Div, Ty::I32),
        (T![<], Ty::I32, Ty::I32) => (OpData::I32Lt, Ty::Bool),
        (T![<=], Ty::I32, Ty::I32) => (OpData::I32Le, Ty::Bool),
        (T![>], Ty::I32, Ty::I32) => (OpData::I32Gt, Ty::Bool),
        (T![>=], Ty::I32, Ty::I32) => (OpData::I32Ge, Ty::Bool),
        (T![==], Ty::I32, Ty::I32) => (OpData::I32Eq, Ty::Bool),
        (T![!=], Ty::I32, Ty::I32) => (OpData::I32Ne, Ty::Bool),

        (T![+], Ty::F32, Ty::F32) => (OpData::F32Add, Ty::F32),
        (T![-], Ty::F32, Ty::F32) => (OpData::F32Sub, Ty::F32),
        (T![*], Ty::F32, Ty::F32) => (OpData::F32Mul, Ty::F32),
        (T![/], Ty::F32, Ty::F32) => (OpData::F32Div, Ty::F32),
        (T![<], Ty::F32, Ty::F32) => (OpData::F32Lt, Ty::Bool),
        (T![<=], Ty::F32, Ty::F32) => (OpData::F32Le, Ty::Bool),
        (T![>], Ty::F32, Ty::F32) => (OpData::F32Gt, Ty::Bool),
        (T![>=], Ty::F32, Ty::F32) => (OpData::F32Ge, Ty::Bool),
        (T![==], Ty::F32, Ty::F32) => (OpData::F32Eq, Ty::Bool),
        (T![!=], Ty::F32, Ty::F32) => (OpData::F32Ne, Ty::Bool),

        (T![==], Ty::Bool, Ty::Bool) => (OpData::BoolEq, Ty::Bool),
        (T![!=], Ty::Bool, Ty::Bool) => (OpData::BoolNe, Ty::Bool),
        (T![&&], Ty::Bool, Ty::Bool) => (OpData::BoolAnd, Ty::Bool),
        (T![||], Ty::Bool, Ty::Bool) => (OpData::BoolOr, Ty::Bool),
        (_, _, _) => return None,
    };
    Some(op_data)
}
