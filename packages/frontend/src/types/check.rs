use super::ir::{
    array_len, array_new, expr_decl, unit_lit, var, ArrayBuilder, ArrayIdxBuilder, BinaryBuilder,
    BlockBuilder, CallBuilder, FuncBuilder, IfBuilder, IntrinsicBuilder, LetBuilder,
    SetArrayBuilder, SetBuilder, StructBuilder, StructIdxBuilder, WhileBuilder,
};
use super::names::{Name, NameSupply};
use super::{
    errors::{
        TyError,
        TyErrorData::{self, *},
    },
    ir::lit,
};
use super::{FuncTy, Ty};
use crate::syntax::ast::AstNode;
use crate::syntax::token_ptr::SyntaxTokenPtr;
use crate::syntax::{nodes::*, SyntaxNode, SyntaxNodePtr, SyntaxToken};
use crate::types::ir::SetStructBuilder;
use crate::T;
use crate::{
    builtins::lookup_builtin,
    lexer::{is_whitespace, SyntaxKind},
};
use backend::ir::{self, LitData, OpData};
use rowan::TextRange;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
struct StructDef {
    name: Name,
    variant: Option<Name>,
    fields: HashMap<String, (Ty, Name)>,
}

#[derive(Debug, Clone)]
struct VariantDef {
    name: Name,
    alternatives: HashMap<String, Name>,
}

#[derive(Debug, Clone)]
enum TypeDef {
    Struct(Rc<StructDef>),
    Variant(Rc<VariantDef>),
}

// NOTE: We could consider passing an explicit Ctx around,
// but we'd need to make it immutable and persistent
#[derive(Debug)]
struct Ctx {
    values: Vec<HashMap<String, (Ty, Name)>>,
    types_names: HashMap<String, Name>,
    struct_defs: HashMap<Name, Rc<StructDef>>,
    variant_defs: HashMap<Name, Rc<VariantDef>>,
}

impl Ctx {
    fn new() -> Ctx {
        Ctx {
            values: vec![],
            types_names: HashMap::new(),
            struct_defs: HashMap::new(),
            variant_defs: HashMap::new(),
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

    fn declare_alternatives(&mut self, name: Name, def: VariantDef) {
        self.variant_defs.insert(name, Rc::new(def));
    }

    fn declare_struct_fields(&mut self, name: Name, def: StructDef) {
        self.struct_defs.insert(name, Rc::new(def));
    }

    fn lookup_type_name(&self, v: &str) -> Option<Name> {
        self.types_names.get(v).copied()
    }

    fn lookup_type_def(&self, name: Name) -> Option<TypeDef> {
        if let Some(struct_def) = self.struct_defs.get(&name) {
            Some(TypeDef::Struct(struct_def.clone()))
        } else if let Some(variant_def) = self.variant_defs.get(&name) {
            Some(TypeDef::Variant(variant_def.clone()))
        } else {
            None
        }
    }

    fn lookup_type(&self, v: &str) -> Option<TypeDef> {
        let n = self.lookup_type_name(v)?;
        let def = self
            .lookup_type_def(n)
            .expect("type declared but not defined");
        Some(def)
    }

    fn lookup_struct(&self, ty: &str) -> Option<Rc<StructDef>> {
        let def = self.lookup_type(ty)?;
        match def {
            TypeDef::Struct(s) => Some(s),
            _ => None,
        }
    }

    fn lookup_variant(&self, ty: &str) -> Option<Rc<VariantDef>> {
        let def = self.lookup_type(ty)?;
        match def {
            TypeDef::Variant(s) => Some(s),
            _ => None,
        }
    }

    fn lookup_alternative(&self, ty: &str, alt: &str) -> Option<Rc<StructDef>> {
        let def = self.lookup_type(ty)?;
        match def {
            TypeDef::Variant(var_def) => {
                let struct_name = var_def.alternatives.get(alt)?;
                Some(
                    self.struct_defs
                        .get(&struct_name)
                        .expect("variant def references unknown struct")
                        .clone(),
                )
            }
            _ => None,
        }
    }

    fn enter_block(&mut self) {
        self.values.push(HashMap::new())
    }

    fn leave_block(&mut self) {
        self.values.pop().expect("Tried to pop from an empty Ctx");
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Occurrence<N> {
    Def(N),
    Ref(N),
}

impl<N> Occurrence<N> {
    pub fn name(&self) -> &N {
        match self {
            Occurrence::Def(n) => n,
            Occurrence::Ref(n) => n,
        }
    }
}

pub struct Typechecker {
    pub typed_nodes: HashMap<SyntaxNodePtr, Ty>,
    pub names: HashMap<SyntaxTokenPtr, Occurrence<Name>>,
    pub errors: Vec<TyError>,

    pub name_supply: NameSupply,
    context: Ctx,
}

impl Default for Typechecker {
    fn default() -> Self {
        Self::new()
    }
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
            .insert(SyntaxTokenPtr::new(token), Occurrence::Def(name));
        assert!(previous.is_none())
    }

    fn record_ref(&mut self, token: &SyntaxToken, name: Name) {
        let previous = self
            .names
            .insert(SyntaxTokenPtr::new(token), Occurrence::Ref(name));
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
        for elem in children.by_ref() {
            if elem.as_token().is_some() && !is_whitespace(elem.kind()) {
                start = elem.text_range().start();
                break;
            }
        }

        for elem in children {
            if elem.as_token().is_some() && !is_whitespace(elem.kind()) {
                end = elem.text_range().end();
            }
        }

        self.errors.push(TyError {
            at: TextRange::new(start, end),
            it: error,
        })
    }

    pub fn infer_program(&mut self, root: Root) -> Option<ir::Program> {
        self.context.enter_block();

        let types = self.check_type_definitions(&root);
        let imports = self.check_imports(&root);
        self.check_function_headers(&root);

        let globals = self.check_globals(&root);
        let functions = self.check_function_bodies(&root);

        self.context.leave_block();

        Some(ir::Program {
            imports: imports?,
            structs: types?,
            globals,
            funcs: functions?,
            start_fn: self.name_supply.start_idx(),
        })
    }

    fn check_imports(&mut self, root: &Root) -> Option<Vec<ir::Import>> {
        let mut imports = vec![];
        for top_level in root.top_levels() {
            if let TopLevel::TopImport(i) = top_level {
                let Some(internal_name_tkn) = i.imp_internal().and_then(|x| x.ident_token()) else {
                    continue;
                };
                let name = self.name_supply.func_idx(&internal_name_tkn);
                self.record_def(&internal_name_tkn, name);

                let ty = if let Some(ty_node) = i.ty() {
                    match self.check_ty(&ty_node) {
                        ty @ (Ty::Func(_) | Ty::Any) => ty,
                        ty => {
                            self.report_error(&ty_node, NonFunctionImport { name, ty });
                            Ty::Any
                        }
                    }
                } else {
                    Ty::Any
                };
                imports.push(self.build_import_ir(&i, name, &ty));
                self.context
                    .add_var(internal_name_tkn.text().to_string(), ty, name);
            }
        }
        imports.into_iter().collect()
    }

    fn build_import_ir(
        &mut self,
        import: &TopImport,
        internal: Name,
        ty: &Ty,
    ) -> Option<ir::Import> {
        let Ty::Func(func_ty) = ty else {
            return None;
        };
        let external_name_tkn = import.imp_external()?.ident_token()?;
        Some(ir::Import {
            span: import.syntax().text_range(),
            internal,
            func_ty: *func_ty.clone(),
            external: external_name_tkn.text().to_string(),
        })
    }

    fn check_type_definitions(&mut self, root: &Root) -> Option<Vec<ir::Struct>> {
        // Because types can be mutually recursive we need two passes:
        // - 1. Forward declare all types
        let mut alt_struct_defs = vec![];
        for top_level in root.top_levels() {
            match top_level {
                TopLevel::TopStruct(s) => {
                    if let Some(tkn) = s.upper_ident_token() {
                        let name = self.name_supply.type_idx(&tkn);
                        self.record_def(&tkn, name);

                        self.context.declare_type(tkn.text(), name)
                    }
                }
                TopLevel::TopVariant(v) => {
                    if let Some(tkn) = v.upper_ident_token() {
                        let name = self.name_supply.type_idx(&tkn);
                        self.record_def(&tkn, name);
                        self.context.declare_type(tkn.text(), name);
                        let mut def = VariantDef {
                            name,
                            alternatives: HashMap::new(),
                        };
                        for alt in v.top_structs() {
                            if let Some(tkn) = alt.upper_ident_token() {
                                let alt_name = self.name_supply.type_idx(&tkn);
                                self.record_def(&tkn, alt_name);
                                self.context.declare_type(tkn.text(), alt_name);
                                def.alternatives.insert(tkn.text().to_string(), alt_name);
                            }
                            alt_struct_defs.push((alt, Some(name)))
                        }
                        self.context.declare_alternatives(name, def)
                    }
                }
                _ => {}
            }
        }

        // - 2. Actually check their definitions
        let mut structs = Some(vec![]);
        let top_structs = root.top_levels().filter_map(|tl| {
            if let TopLevel::TopStruct(s) = tl {
                Some((s, None))
            } else {
                None
            }
        });
        for (s, variant) in top_structs.chain(alt_struct_defs.into_iter()) {
            if let Some(struct_name_tkn) = s.upper_ident_token() {
                let name = self
                    .context
                    .lookup_type_name(struct_name_tkn.text())
                    .expect("non-forward declared struct");
                let mut def = StructDef {
                    name,
                    variant,
                    fields: HashMap::new(),
                };
                for field in s.struct_fields() {
                    let Some(field_name) = field.ident_token() else {
                        structs = None;
                        continue;
                    };
                    let ty = match field.ty() {
                        Some(field_ty) => self.check_ty(&field_ty),
                        None => {
                            structs = None;
                            Ty::Any
                        }
                    };
                    let name = self.name_supply.field_idx(&field_name);
                    self.record_def(&field_name, name);

                    def.fields.insert(field_name.text().to_string(), (ty, name));
                }

                if let Some(structs) = &mut structs {
                    structs.push(ir::Struct {
                        span: s.syntax().text_range(),
                        name,
                        fields: def
                            .fields
                            .iter()
                            .map(|(_, (ty, name))| (*name, ty.clone()))
                            .collect(),
                    })
                }
                self.context.declare_struct_fields(name, def)
            }
        }
        structs
    }

    fn check_function_headers(&mut self, root: &Root) {
        for top_level in root.top_levels() {
            if let TopLevel::TopFn(top_fn) = top_level {
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
                if let Some(ty) = t.qualifier().map(|q| q.upper_ident_token().unwrap()) {
                    let Some(TypeDef::Variant(ty_def)) = self.context.lookup_type(ty.text()) else {
                        self.report_error_token(&ty, UnknownType(ty.text().to_string()));
                        return Ty::Any;
                    };
                    self.record_ref(&ty, ty_def.name);

                    let Some(alt) = t.upper_ident_token() else {
                        return Ty::Any;
                    };
                    let Some(name) = ty_def.alternatives.get(alt.text()) else {
                        self.report_error_token(
                            &alt,
                            UnknownType(format!("{}::{}", ty.text(), alt.text())),
                        );
                        return Ty::Any;
                    };
                    self.record_ref(&alt, *name);
                    Ty::Struct(*name)
                } else {
                    let ty_name = t.upper_ident_token().unwrap();
                    let Some(name) = self.context.lookup_type_name(ty_name.text()) else {
                        self.report_error_token(&ty_name, UnknownType(ty_name.text().to_string()));
                        return Ty::Any;
                    };
                    self.record_ref(&ty_name, name);
                    Ty::Struct(name)
                }
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

    fn check_globals(&mut self, root: &Root) -> Vec<ir::Global> {
        let mut globals = vec![];
        for top_level in root.top_levels() {
            if let TopLevel::TopGlobal(top_global) = top_level {
                let (ty, ir) = match (
                    top_global.ty().map(|t| self.check_ty(&t)),
                    top_global.expr(),
                ) {
                    (None, None) => {
                        continue;
                    }
                    (Some(ty), None) => (ty, None),
                    (None, Some(e)) => self.infer_expr(&e),
                    (Some(ty), Some(e)) => {
                        let ir = self.check_expr(&e, &ty);
                        (ty, ir)
                    }
                };
                if let Some(binder_tkn) = top_global.ident_token() {
                    let name = self.name_supply.global_idx(&binder_tkn);
                    self.record_def(&binder_tkn, name);
                    if let Some(ir) = ir {
                        globals.push(ir::Global {
                            span: top_global.syntax().text_range(),
                            binder: name,
                            init: ir,
                        })
                    }
                    self.context
                        .add_var(binder_tkn.text().to_string(), ty, name)
                };
            }
        }
        globals
    }

    fn check_function_bodies(&mut self, root: &Root) -> Option<Vec<ir::Func>> {
        let mut funcs = Some(vec![]);
        for top_level in root.top_levels() {
            if let TopLevel::TopFn(top_fn) = top_level {
                let mut builder = FuncBuilder::new();
                let Some(func_name) = top_fn.ident_token() else {
                    funcs = None;
                    continue;
                };
                let Some((Ty::Func(func_ty), name)) = self.context.lookup_var(func_name.text())
                else {
                    panic!("didn't pre-declare function, {}", func_name.text())
                };

                builder.name(*name);

                // Use Rc for Tys in the context?
                let func_ty = func_ty.clone();

                self.context.enter_block();
                for (param, ty) in top_fn.params().zip(func_ty.arguments.into_iter()) {
                    let Some(ident_tkn) = param.ident_token() else {
                        funcs = None;
                        continue;
                    };
                    let name = self.name_supply.local_idx(&ident_tkn);
                    builder.param(name, Some(ty.clone()));
                    self.record_def(&ident_tkn, name);
                    self.context.add_var(ident_tkn.text().to_string(), ty, name);
                }

                builder.return_ty(Some(func_ty.result.clone()));

                if let Some(body) = top_fn.body() {
                    // println!("Checking body {}", func_name.text());
                    builder.body(self.check_expr(&body.into(), &func_ty.result));
                }

                self.context.leave_block();

                if let Some(fs) = &mut funcs {
                    if let Some(func) = builder.build() {
                        fs.push(func)
                    } else {
                        funcs = None
                    }
                }
            }
        }
        funcs
    }

    fn infer_literal(&mut self, lit: &Literal) -> (Ty, Option<ir::Lit>) {
        let (ty, it) = match lit {
            Literal::LitBool(b) => (Ty::Bool, Some(LitData::Bool(b.true_token().is_some()))),
            Literal::LitFloat(l) => {
                let float_tkn = l.float_lit_token().unwrap();
                if let Ok(float) = float_tkn.text().parse::<f32>() {
                    (Ty::F32, Some(ir::LitData::F32(float)))
                } else {
                    self.report_error_token(&float_tkn, InvalidLiteral);
                    (Ty::F32, None)
                }
            }
            Literal::LitInt(l) => {
                let int_tkn = l.int_lit_token().unwrap();
                if let Ok(int) = int_tkn.text().parse::<i32>() {
                    (Ty::I32, Some(ir::LitData::I32(int)))
                } else {
                    self.report_error_token(&int_tkn, InvalidLiteral);
                    (Ty::I32, None)
                }
            }
        };
        (
            ty.clone(),
            it.map(|it| ir::Lit {
                at: lit.syntax().text_range(),
                ty,
                it,
            }),
        )
    }

    fn infer_callee(&mut self, expr: &Expr) -> (Ty, Option<ir::Callee>) {
        if let Expr::EVar(v) = expr {
            let var_tkn = v.ident_token().unwrap();
            if self.context.lookup_var(var_tkn.text()).is_none() {
                if let Some(builtin) = lookup_builtin(var_tkn.text()) {
                    return (
                        Ty::Func(Box::new(builtin.ty.clone())),
                        Some(ir::Callee::Builtin(builtin.name)),
                    );
                }
            }
        }
        let (ty, ir) = self.infer_expr(expr);
        (ty, ir.map(ir::Callee::FuncRef))
    }

    fn infer_expr(&mut self, expr: &Expr) -> (Ty, Option<ir::Expr>) {
        match self.infer_expr_inner(expr) {
            Some((ty, ir)) => {
                self.record_typed(expr.syntax(), &ty);
                (ty, ir)
            }
            None => (Ty::Any, None),
        }
    }

    fn infer_expr_inner(&mut self, expr: &Expr) -> Option<(Ty, Option<ir::Expr>)> {
        let (ty, ir): (Ty, Option<ir::ExprData>) = match expr {
            Expr::EArray(arr) => {
                let mut builder = ArrayBuilder::new();
                let mut elems = arr.exprs();
                if let Some(first_elem) = elems.next() {
                    let (elem_ty, elem_ir) = self.infer_expr(&first_elem);
                    builder.elem(elem_ir);
                    for elem in elems {
                        builder.elem(self.check_expr(&elem, &elem_ty));
                    }
                    (Ty::Array(Box::new(elem_ty)), builder.build())
                } else {
                    self.report_error(arr, CantInferEmptyArray);
                    (Ty::Array(Box::new(Ty::Any)), None)
                }
            }
            Expr::ELit(l) => {
                let (ty, ir) = self.infer_literal(&l.literal().unwrap());
                (ty, lit(ir))
            }
            Expr::EVar(v) => {
                let var_tkn = v.ident_token().unwrap();
                match self.context.lookup_var(var_tkn.text()) {
                    None => {
                        self.report_error_token(&var_tkn, UnknownVar(var_tkn.text().to_string()));
                        return None;
                    }
                    Some((ty, name)) => {
                        let return_ty = ty.clone();
                        let ir = var(*name);
                        self.record_ref(&var_tkn, *name);
                        (return_ty, ir)
                    }
                }
            }
            Expr::EStruct(struct_expr) => {
                let (struct_def, struct_name_tkn) = if let Some(ty) = struct_expr
                    .qualifier()
                    .map(|q| q.upper_ident_token().unwrap())
                {
                    let alt = struct_expr.upper_ident_token()?;
                    // TODO record reference for variant
                    // self.record_ref(&ty, NAME_GOES_HERE)
                    (self.context.lookup_alternative(ty.text(), alt.text()), alt)
                } else {
                    let tkn = struct_expr.upper_ident_token()?;
                    (self.context.lookup_struct(tkn.text()), tkn)
                };
                match struct_def {
                    None => {
                        self.report_error_token(
                            &struct_name_tkn,
                            UnknownType(struct_name_tkn.text().to_string()),
                        );
                        return None;
                    }
                    Some(def) => {
                        let name = def.name;
                        self.record_ref(&struct_name_tkn, name);
                        let mut builder = StructBuilder::new();
                        builder.name(Some(name));
                        let mut seen = vec![];
                        for field in struct_expr.e_struct_fields() {
                            let Some(field_tkn) = field.ident_token() else {
                                continue;
                            };
                            let Some((ty, field_name)) = def.fields.get(field_tkn.text()) else {
                                self.report_error_token(
                                    &field_tkn,
                                    UnknownField {
                                        struct_name: name,
                                        field_name: field_tkn.text().to_string(),
                                    },
                                );
                                continue;
                            };
                            self.record_ref(&field_tkn, *field_name);
                            seen.push(*field_name);
                            if let Some(field_expr) = field.expr() {
                                builder.field(*field_name, self.check_expr(&field_expr, ty));
                            }
                        }
                        // TODO report all missing fields at once
                        for (_, field_name) in def.fields.values() {
                            if !seen.contains(field_name) {
                                self.report_error_token(
                                    &struct_name_tkn,
                                    MissingField {
                                        struct_name: name,
                                        field_name: *field_name,
                                    },
                                );
                            }
                        }
                        (Ty::Struct(def.variant.unwrap_or(name)), builder.build())
                    }
                }
            }
            Expr::ECall(call_expr) => {
                let func_expr = call_expr.expr()?;
                match self.infer_callee(&func_expr) {
                    (Ty::Func(func_ty), callee) => {
                        let mut builder = CallBuilder::new();
                        builder.func(callee);
                        if let Some(arg_list) = call_expr.e_arg_list() {
                            let arg_exprs: Vec<Expr> = arg_list.exprs().collect();
                            let arg_tys = func_ty.arguments;
                            if arg_exprs.len() != arg_tys.len() {
                                self.report_error(
                                    &arg_list,
                                    ArgCountMismatch(arg_tys.len(), arg_exprs.len()),
                                );
                            }
                            for (param, expected_ty) in arg_exprs.iter().zip(arg_tys.iter()) {
                                builder.argument(self.check_expr(param, expected_ty));
                            }
                        }
                        (func_ty.result, builder.build())
                    }
                    (ty, _) => {
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
                        let (ty, ir) = self.infer_expr(&args[0]);
                        if !matches!(ty, Ty::Array(_)) {
                            self.report_error(
                                &args[0],
                                Message(format!("expected an array type, but got {:?}", ty)),
                            );
                            (Ty::I32, None)
                        } else {
                            let mut builder = IntrinsicBuilder::new();
                            builder.intrinsic(array_len(intrinsic_tkn.text_range()));
                            builder.argument(ir);
                            (Ty::I32, builder.build())
                        }
                    }
                    ("@array_new", 2) => {
                        let mut builder = IntrinsicBuilder::new();
                        builder.intrinsic(array_new(intrinsic_tkn.text_range()));
                        let (elem_ty, ir) = self.infer_expr(&args[0]);
                        builder.argument(ir);
                        builder.argument(self.check_expr(&args[1], &Ty::I32));
                        (Ty::Array(Box::new(elem_ty)), builder.build())
                    }
                    (f, arg_count) => {
                        self.report_error_token(
                            &intrinsic_tkn,
                            UnknownIntrinsic(f.to_string(), arg_count),
                        );
                        (Ty::Any, None)
                    }
                }
            }
            Expr::EParen(e) => {
                let (ty, ir) = self.infer_expr(&e.expr()?);
                (ty, ir.map(|x| *x.it))
            }
            Expr::EIf(if_expr) => {
                let mut builder = IfBuilder::new();
                if let Some(condition) = if_expr.condition() {
                    builder.condition(self.check_expr(&condition, &Ty::Bool));
                }
                if let Some(then_branch) = if_expr.then_branch() {
                    let (ty, ir) = self.infer_expr(&then_branch);
                    builder.then_branch(ir);
                    if let Some(else_branch) = if_expr.else_branch() {
                        builder.else_branch(self.check_expr(&else_branch, &ty));
                    }
                    (ty, builder.build())
                } else {
                    (Ty::Any, None)
                }
            }
            Expr::EMatch(match_expr) => {
                let Some(scrutinee) = match_expr.scrutinee() else {
                    return None;
                };

                let (ty_scrutinee, ir_scrutinee) = self.infer_expr(&scrutinee);
                let mut ty = None;
                for branch in match_expr.e_match_branchs() {
                    let (Some(pattern), Some(body)) = (branch.pattern(), branch.body()) else {
                        continue;
                    };
                    self.context.enter_block();
                    // TODO handle return value
                    self.check_pattern(&pattern, &ty_scrutinee);
                    let body_ir = if let Some(expected) = &ty {
                        self.check_expr(&body.into(), expected)
                    } else {
                        let (ty_body, ir) = self.infer_expr(&body.into());
                        if ty_body != Ty::Any {
                            ty = Some(ty_body);
                        }
                        ir
                    };
                    self.context.leave_block();
                }
                (ty.unwrap_or(Ty::Any), None)
            }
            Expr::EArrayIdx(idx_expr) => {
                let mut builder = ArrayIdxBuilder::new();
                let arr_expr = idx_expr.expr().unwrap();
                if let Some(index) = idx_expr.index() {
                    builder.index(self.check_expr(&index, &Ty::I32));
                }
                let elem_ty = match self.infer_expr(&arr_expr) {
                    (Ty::Array(elem_ty), ir) => {
                        builder.array(ir);
                        *elem_ty
                    }
                    (ty, _) => {
                        if ty != Ty::Any {
                            self.report_error(&arr_expr, NonArrayIdx(ty))
                        }
                        return None;
                    }
                };
                (elem_ty, builder.build())
            }
            Expr::EStructIdx(idx_expr) => {
                let mut builder = StructIdxBuilder::new();
                let struct_expr = idx_expr.expr().unwrap();
                let (ty_receiver, receiver_ir) = self.infer_expr(&struct_expr);
                builder.expr(receiver_ir);

                let field_name_tkn = idx_expr.ident_token()?;
                let (field_name, ty) = self.check_struct_idx(&ty_receiver, &field_name_tkn)?;
                builder.index(field_name);
                (ty, builder.build())
            }
            Expr::EBinary(bin_expr) => {
                let mut builder = BinaryBuilder::new();
                let (lhs_ty, lhs_ir) = self.infer_expr(&bin_expr.lhs()?);
                builder.left(lhs_ir);
                // TODO could maybe check the rhs based on operator and lhs?
                let (rhs_ty, rhs_ir) = self.infer_expr(&bin_expr.rhs()?);
                builder.right(rhs_ir);
                let op_tkn = bin_expr.op()?;
                if lhs_ty == Ty::Any || rhs_ty == Ty::Any {
                    return None;
                }
                match check_op(&op_tkn, &lhs_ty, &rhs_ty) {
                    None => {
                        self.report_error_token(
                            &op_tkn,
                            Message(format!(
                                "Invalid operator {} for lhs of type {} and rhs of type {}",
                                op_tkn.text(),
                                lhs_ty.display(&self.name_supply.name_map),
                                rhs_ty.display(&self.name_supply.name_map)
                            )),
                        );
                        return None;
                    }
                    Some((op_data, ty)) => {
                        let op = ir::Op {
                            it: op_data,
                            at: op_tkn.text_range(),
                        };
                        builder.op(op);
                        (ty, builder.build())
                    }
                }
            }
            Expr::EBlock(block_expr) => {
                let mut builder = BlockBuilder::new();
                let declarations: Vec<Declaration> = block_expr.declarations().collect();
                if let Some((last, declarations)) = declarations.split_last() {
                    self.context.enter_block();
                    for decl in declarations {
                        let (_, ir) = self.infer_decl(decl);
                        builder.declaration(ir);
                    }
                    let ty = match last {
                        Declaration::DExpr(expr) => {
                            let (new_ty, ir) = self.infer_expr(&expr.expr().unwrap());
                            builder.expr(ir);
                            new_ty
                        }
                        decl => {
                            let (_, ir) = self.infer_decl(decl);
                            builder.declaration(ir);
                            builder.expr(unit_lit(decl.syntax().text_range()));
                            Ty::Unit
                        }
                    };
                    self.context.leave_block();
                    (ty, builder.build())
                } else {
                    builder.expr(unit_lit(block_expr.syntax().text_range()));
                    (Ty::Unit, builder.build())
                }
            }
        };

        let ir_expr = ir.map(|expr_data| ir::Expr {
            at: expr.syntax().text_range(),
            // Bail if Ty is any?
            ty: ty.clone(),
            it: Box::new(expr_data),
        });

        Some((ty, ir_expr))
    }

    // TODO: Could add a check case for blocks, passing down the check to the trailing Expr
    fn check_expr(&mut self, expr: &Expr, expected: &Ty) -> Option<ir::Expr> {
        let ir = match (expr, expected) {
            (Expr::EArray(expr), Ty::Array(elem_ty)) => {
                let mut builder = ArrayBuilder::new();
                for elem in expr.exprs() {
                    builder.elem(self.check_expr(&elem, elem_ty));
                }
                builder.build()
            }
            (Expr::EArrayIdx(idx_expr), elem_ty) => {
                let mut builder = ArrayIdxBuilder::new();
                let arr_expr = idx_expr.expr().unwrap();
                builder.array(self.check_expr(&arr_expr, &Ty::Array(Box::new(elem_ty.clone()))));
                if let Some(index) = idx_expr.index() {
                    builder.index(self.check_expr(&index, &Ty::I32));
                }
                builder.build()
            }
            (Expr::EIf(expr), ty) => {
                let mut builder = IfBuilder::new();
                if let Some(condition) = expr.condition() {
                    builder.condition(self.check_expr(&condition, &Ty::Bool));
                }
                if let Some(then_branch) = expr.condition() {
                    builder.then_branch(self.check_expr(&then_branch, ty));
                }
                if let Some(else_branch) = expr.condition() {
                    builder.else_branch(self.check_expr(&else_branch, ty));
                }
                builder.build()
            }
            (Expr::EParen(expr), _) => expr
                .expr()
                .and_then(|expr| self.check_expr(&expr, expected))
                .map(|ir| *ir.it),
            _ => {
                let (ty, ir) = self.infer_expr(expr);
                if *expected != Ty::Any && ty != Ty::Any && ty != *expected {
                    self.errors.push(TyError {
                        at: expr.syntax().text_range(),
                        it: TypeMismatch {
                            expected: expected.clone(),
                            actual: ty,
                        },
                    })
                }
                return ir;
            }
        };
        self.record_typed(expr.syntax(), expected);
        ir.map(|it| ir::Expr {
            it: Box::new(it),
            at: expr.syntax().text_range(),
            ty: expected.clone(),
        })
    }

    fn check_struct_idx(
        &mut self,
        receiver: &Ty,
        field_name_tkn: &SyntaxToken,
    ) -> Option<(Name, Ty)> {
        let def = match receiver {
            Ty::Struct(name) => {
                let type_def = self
                    .context
                    .lookup_type_def(*name)
                    .expect("inferred a type that wasn't defined");
                match type_def {
                    TypeDef::Struct(s) => s,
                    TypeDef::Variant(_) => {
                        self.report_error_token(field_name_tkn, NonStructIdx(receiver.clone()));
                        return None;
                    }
                }
            }
            ty => {
                if *ty != Ty::Any {
                    self.report_error_token(field_name_tkn, NonStructIdx(ty.clone()))
                }
                return None;
            }
        };
        match def.fields.get(field_name_tkn.text()) {
            None => {
                self.report_error_token(
                    field_name_tkn,
                    UnknownField {
                        struct_name: def.name,
                        field_name: field_name_tkn.text().to_string(),
                    },
                );
                None
            }
            Some((t, n)) => {
                self.record_ref(field_name_tkn, *n);
                Some((*n, t.clone()))
            }
        }
    }

    fn infer_decl(&mut self, decl: &Declaration) -> (Ty, Option<ir::Declaration>) {
        let (ty, ir) = match decl {
            Declaration::DLet(let_decl) => {
                let mut builder = LetBuilder::new();
                let (ty, ir) = if let Some(expr) = let_decl.expr() {
                    if let Some(ty) = let_decl.ty().map(|ta| self.check_ty(&ta)) {
                        let ir = self.check_expr(&expr, &ty);
                        (ty, ir)
                    } else {
                        self.infer_expr(&expr)
                    }
                } else {
                    (Ty::Any, None)
                };
                builder.expr(ir);

                if let Some(binder_tkn) = let_decl.ident_token() {
                    let name = self.name_supply.local_idx(&binder_tkn);
                    builder.binder(name);
                    self.record_def(&binder_tkn, name);
                    // TODO can't record this as typed because the ident token is not a syntax node
                    // self.record_typed(, &ty)
                    self.context
                        .add_var(binder_tkn.text().to_string(), ty, name)
                }
                (Ty::Unit, builder.build())
            }
            Declaration::DSet(set_decl) => {
                let mut builder = SetBuilder::new();
                let set_ty = if let Some(set_target) = set_decl.set_target() {
                    let (ty, ir) = self.infer_set_target(&set_target);
                    builder.set_target(ir);
                    ty
                } else {
                    Ty::Any
                };
                if let Some(expr) = set_decl.expr() {
                    builder.expr(self.check_expr(&expr, &set_ty));
                }
                (Ty::Unit, builder.build())
            }
            Declaration::DWhile(while_decl) => {
                let mut builder = WhileBuilder::new();
                let condition = while_decl.expr();
                if let Some(condition) = condition {
                    builder.condition(self.check_expr(&condition, &Ty::Bool));
                }
                let body = while_decl.e_block();
                if let Some(body) = body {
                    builder.body(self.check_expr(&body.into(), &Ty::Unit));
                }
                (Ty::Unit, builder.build())
            }
            Declaration::DExpr(decl) => {
                let (ty, ir) = self.infer_expr(&decl.expr().unwrap());
                (ty, expr_decl(ir))
            }
        };
        (
            ty.clone(),
            ir.map(|ir| ir::Declaration {
                ty,
                it: ir,
                at: decl.syntax().text_range(),
            }),
        )
    }

    fn infer_set_target(&mut self, set_target: &SetTarget) -> (Ty, Option<ir::SetTarget>) {
        let Some(expr) = set_target.set_target_expr() else {
            return (Ty::Any, None);
        };
        let (ty, ir_data) = match expr {
            SetTargetExpr::EVar(var) => {
                let ident_tkn = var.ident_token().unwrap();
                if let Some((ty, name)) = self.context.lookup_var(ident_tkn.text()) {
                    let ir = ir::SetTargetData::Var { name: *name };
                    let ty = ty.clone();
                    self.record_ref(&ident_tkn, *name);
                    (ty, Some(ir))
                } else {
                    self.report_error_token(&ident_tkn, UnknownVar(ident_tkn.text().to_string()));
                    (Ty::Any, None)
                }
            }
            SetTargetExpr::EArrayIdx(arr_idx) => {
                let mut builder = SetArrayBuilder::new();
                let arr_ty = arr_idx.expr().map(|target| {
                    let (ty, ir) = self.infer_expr(&target);
                    builder.target(ir);
                    ty
                });

                if let Some(index) = arr_idx.index() {
                    builder.index(self.check_expr(&index, &Ty::I32));
                }

                match arr_ty {
                    None | Some(Ty::Any) => (Ty::Any, None),
                    Some(Ty::Array(elem_ty)) => ((*elem_ty).clone(), builder.build()),
                    Some(t) => {
                        self.report_error(&arr_idx, NonArrayIdx(t));
                        (Ty::Any, None)
                    }
                }
            }
            SetTargetExpr::EStructIdx(struct_idx) => {
                let mut builder = SetStructBuilder::new();
                let Some(target_expr) = struct_idx.expr() else {
                    return (Ty::Any, None);
                };
                let (target_ty, ir) = self.infer_expr(&target_expr);
                builder.target(ir);
                let Some(field_name_tkn) = struct_idx.ident_token() else {
                    return (Ty::Any, None);
                };

                let Some((name, field_ty)) = self.check_struct_idx(&target_ty, &field_name_tkn)
                else {
                    return (Ty::Any, None);
                };
                builder.index(name);
                (field_ty, builder.build())
            }
        };

        let ir = ir_data.map(|ir_data| ir::SetTarget {
            at: set_target.syntax().text_range(),
            ty: ty.clone(),
            it: ir_data,
        });

        (ty, ir)
    }

    fn check_pattern(&mut self, pattern: &Pattern, expected: &Ty) -> Option<()> {
        match pattern {
            Pattern::PatVariant(pat) => {
                let ty = pat.qualifier()?.upper_ident_token()?;
                let ctor = pat.upper_ident_token()?;
                let var = pat.ident_token()?;
                let Some(def) = self.context.lookup_variant(ty.text()) else {
                    self.report_error_token(&ty, UnknownType(ty.text().to_string()));
                    return None;
                };
                self.record_ref(&ty, def.name);
                match expected {
                    Ty::Struct(s) if def.name == *s => {
                        if let Some(struct_name) = def.alternatives.get(ctor.text()) {
                            self.record_ref(&ctor, *struct_name);
                            let name = self.name_supply.local_idx(&var);
                            self.context.add_var(
                                var.text().to_string(),
                                Ty::Struct(*struct_name),
                                name,
                            );
                            self.record_def(&var, name);
                            Some(())
                        } else {
                            self.report_error_token(
                                &ctor,
                                UnknownAlternative {
                                    variant_name: def.name,
                                    alternative: ctor.text().to_string(),
                                },
                            );
                            None
                        }
                    }
                    Ty::Any => None,
                    _ => {
                        self.report_error(
                            pattern,
                            PatternTypeMismatch {
                                expected: expected.clone(),
                            },
                        );
                        None
                    }
                }
            }
            Pattern::PatVar(v) => {
                let ident_tkn = v.ident_token().unwrap();
                let name = self.name_supply.local_idx(&ident_tkn);
                self.context
                    .add_var(ident_tkn.text().to_string(), expected.clone(), name);
                self.record_def(&ident_tkn, name);
                Some(())
            }
        }
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
