use super::errors::TyErrors;
use super::errors::{TyError, TyErrorData::*};
use super::names::{Name, NameSupply};
use super::{FuncTy, Ty};
use crate::builtins::lookup_builtin;
use crate::ir::{self, ExprBuilder, LitBuilder, PatVarBuilder, Substitution, VarBuilder};
use crate::lexer::SyntaxKind;
use crate::syntax::ast::AstNode;
use crate::syntax::nodes::*;
use crate::syntax::token_ptr::SyntaxTokenPtr;
use crate::syntax::{SyntaxNode, SyntaxNodePtr, SyntaxToken};
use crate::T;
use rowan::TextRange;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
struct StructDef {
    name: Name,
    span: TextRange,
    variant: Option<Name>,
    ty_params: Vec<(String, Name)>,
}

#[derive(Debug, Clone)]
struct VariantDef {
    name: Name,
    span: TextRange,
    ty_params: Vec<(String, Name)>,
    // TODO: The ordering of these alternatives needs to be deterministic
    alternatives: HashMap<String, Name>,
}

impl VariantDef {
    pub fn lookup_alternative(&self, name: &str) -> Option<Name> {
        self.alternatives.get(name).copied()
    }
}

#[derive(Debug, Clone)]
enum TypeDef {
    Struct(Rc<StructDef>),
    Variant(Rc<VariantDef>),
}
impl TypeDef {
    fn name(&self) -> Name {
        match self {
            TypeDef::Struct(x) => x.name,
            TypeDef::Variant(x) => x.name,
        }
    }

    fn ty_params(&self) -> &[(String, Name)] {
        match self {
            TypeDef::Struct(x) => &x.ty_params,
            TypeDef::Variant(x) => &x.ty_params,
        }
    }
}

#[derive(Debug, Clone)]
struct StructFields {
    // TODO: The ordering of these fields needs to be deterministic
    fields: HashMap<String, (Name, Ty)>,
}

impl StructFields {
    fn to_ir(&self) -> Vec<(Name, Ty)> {
        self.fields.values().cloned().collect()
    }

    fn names(&self) -> Vec<Name> {
        self.fields.values().map(|(n, _)| *n).collect()
    }
}

#[derive(Debug, Clone)]
struct FuncDef {
    name: Name,
    ty_params: Vec<(String, Name)>,
    ty: FuncTy,
}

// NOTE: We could consider passing an explicit Ctx around,
// but we'd need to make it immutable and persistent
#[derive(Debug)]
struct Ctx {
    values: Vec<HashMap<String, (Ty, Name)>>,
    functions: HashMap<String, Rc<FuncDef>>,
    type_vars: HashMap<String, Name>,
    types_names: HashMap<String, Name>,
    type_defs: HashMap<Name, TypeDef>,
    field_defs: HashMap<Name, StructFields>,
}

impl Ctx {
    fn new() -> Ctx {
        Ctx {
            values: vec![],
            functions: HashMap::new(),
            type_vars: HashMap::new(),
            type_defs: HashMap::new(),
            types_names: HashMap::new(),
            field_defs: HashMap::new(),
        }
    }

    fn add_var(&mut self, v: String, ty: Ty, name: Name) {
        self.values.last_mut().unwrap().insert(v, (ty, name));
    }

    fn lookup_var(&self, v: &str) -> Option<(Ty, Name)> {
        if let Some((ty, name)) = self.values.iter().rev().find_map(|scope| scope.get(v)) {
            Some((ty.clone(), *name))
        } else {
            None
        }
    }

    fn lookup_var_or_func(&self, v: &str) -> Option<(Ty, Name)> {
        self.lookup_var(v).or_else(|| {
            let def = self.lookup_func(v)?;
            if !def.ty_params.is_empty() {
                eprintln!(
                    "No func refs for polymorphic functions yet. Allow instantiate in the future?"
                );
                return None;
            }
            Some((Ty::Func(Box::new(def.ty.clone())), def.name))
        })
    }

    fn add_type_var(&mut self, v: String, name: Name) {
        self.type_vars.insert(v, name);
    }

    fn lookup_type_var(&self, v: &str) -> Option<Name> {
        self.type_vars.get(v).copied()
    }

    fn clear_type_vars(&mut self) {
        self.type_vars.clear()
    }

    fn add_func(&mut self, v: String, name: Name, ty_params: Vec<(String, Name)>, ty: FuncTy) {
        self.functions.insert(
            v,
            Rc::new(FuncDef {
                name,
                ty_params,
                ty,
            }),
        );
    }

    fn lookup_func(&self, name: &str) -> Option<Rc<FuncDef>> {
        self.functions.get(name).cloned()
    }

    fn declare_type_def(&mut self, v: &str, name: Name, def: TypeDef) {
        let mut is_sub_struct = false;
        if let TypeDef::Struct(struct_def) = &def {
            is_sub_struct = struct_def.variant.is_some()
        }
        // We don't record String -> Name mapping for variant structs
        // as those are looked up via their Variant name
        if !is_sub_struct {
            self.types_names.insert(v.to_string(), name);
        }
        self.type_defs.insert(name, def);
    }

    fn lookup_type_name(&self, v: &str) -> Option<Name> {
        self.types_names.get(v).copied()
    }

    fn lookup_type_def(&self, name: Name) -> Option<TypeDef> {
        self.type_defs.get(&name).cloned()
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

    fn lookup_struct_name(&self, name: Name) -> Rc<StructDef> {
        let Some(TypeDef::Struct(d)) = self.lookup_type_def(name) else {
            panic!("inferred unknown struct name")
        };
        d.clone()
    }

    fn lookup_variant(&self, ty: &str) -> Option<Rc<VariantDef>> {
        let def = self.lookup_type(ty)?;
        match def {
            TypeDef::Variant(s) => Some(s),
            _ => None,
        }
    }

    fn enter_block(&mut self) {
        self.values.push(HashMap::new())
    }

    fn leave_block(&mut self) {
        self.values.pop().expect("Tried to pop from an empty Ctx");
    }

    fn get_fields(&self, name: Name) -> &HashMap<String, (Name, Ty)> {
        &self.field_defs.get(&name).unwrap().fields
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

pub type OccurenceMap = HashMap<SyntaxTokenPtr, Occurrence<Name>>;

pub struct Typechecker {
    pub typed_nodes: HashMap<SyntaxNodePtr, Ty>,
    pub names: OccurenceMap,

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

            name_supply: NameSupply::new(),
            context: Ctx::new(),
        }
    }

    fn record_def(&mut self, token: &SyntaxToken, name: Name) {
        let previous_def = self
            .names
            .insert(SyntaxTokenPtr::new(token), Occurrence::Def(name));
        assert!(previous_def.is_none())
    }

    fn record_ref(&mut self, token: &SyntaxToken, name: Name) {
        let previous_ref = self
            .names
            .insert(SyntaxTokenPtr::new(token), Occurrence::Ref(name));
        assert!(previous_ref.is_none())
    }

    fn record_typed(&mut self, node: &SyntaxNode, ty: &Ty) {
        let previous_typed = self
            .typed_nodes
            .insert(SyntaxNodePtr::new(node), ty.clone());
        assert!(previous_typed.is_none())
    }

    pub fn infer_program(&mut self, root: &Root) -> (Option<ir::Program>, Vec<TyError>) {
        let mut errors: TyErrors = TyErrors::new();
        let ir = self.infer_program_inner(&mut errors, root);
        (ir, errors.errors)
    }

    pub fn infer_program_inner(
        &mut self,
        errors: &mut TyErrors,
        root: &Root,
    ) -> Option<ir::Program> {
        self.context.enter_block();

        let types = self.check_type_definitions(errors, root);
        let imports = self.check_imports(errors, root);
        self.check_function_headers(errors, root);

        let globals = self.check_globals(errors, root);
        let functions = self.check_function_bodies(errors, root);

        self.context.leave_block();

        Some(ir::Program {
            imports: imports?,
            types: types?,
            globals,
            funcs: functions?,
            start_fn: self.name_supply.start_idx(),
        })
    }

    fn check_imports(&mut self, errors: &mut TyErrors, root: &Root) -> Option<Vec<ir::Import>> {
        let mut imports = vec![];
        for top_level in root.top_levels() {
            if let TopLevel::TopImport(i) = top_level {
                let Some(internal_name_tkn) = i.imp_internal().and_then(|x| x.ident_token()) else {
                    continue;
                };
                let name = self.name_supply.func_idx(&internal_name_tkn);
                self.record_def(&internal_name_tkn, name);

                let ty = if let Some(ty_node) = i.ty() {
                    match self.check_ty(errors, &ty_node) {
                        Ty::Func(t) => *t,
                        ty => {
                            if ty != Ty::Error {
                                errors.report(&ty_node, NonFunctionImport { name, ty });
                            }
                            FuncTy {
                                arguments: vec![],
                                result: Ty::Error,
                            }
                        }
                    }
                } else {
                    FuncTy {
                        arguments: vec![],
                        result: Ty::Error,
                    }
                };
                imports.push(self.build_import_ir(&i, name, &ty));
                self.context
                    .add_func(internal_name_tkn.text().to_string(), name, vec![], ty);
            }
        }
        imports.into_iter().collect()
    }

    fn build_import_ir(
        &mut self,
        import: &TopImport,
        internal: Name,
        ty: &FuncTy,
    ) -> Option<ir::Import> {
        let external_name_tkn = import.imp_external()?.ident_token()?;
        Some(ir::Import {
            span: import.syntax().text_range(),
            internal,
            func_ty: (*ty).clone(),
            external: external_name_tkn.text().to_string(),
        })
    }

    fn check_ty_param(&mut self, ty_param: &ParamTy) -> (String, Name) {
        let tkn = ty_param.ident_token().unwrap();
        let name = self.name_supply.type_var(&tkn);
        self.record_def(&tkn, name);
        (tkn.text().to_string(), name)
    }

    fn check_type_definitions(
        &mut self,
        errors: &mut TyErrors,
        root: &Root,
    ) -> Option<Vec<ir::TypeDef>> {
        // Because types can be mutually recursive we need two passes:
        // - 1. Forward declare all types and their "shapes"
        let mut struct_defs = vec![];
        for top_level in root.top_levels() {
            match top_level {
                TopLevel::TopStruct(s) => {
                    let Some(tkn) = s.upper_ident_token() else {
                        continue;
                    };
                    let name = self.name_supply.type_idx(&tkn);
                    self.record_def(&tkn, name);

                    let ty_params = s.type_params().map(|p| self.check_ty_param(&p)).collect();
                    self.context.declare_type_def(
                        tkn.text(),
                        name,
                        TypeDef::Struct(Rc::new(StructDef {
                            name,
                            span: s.syntax().text_range(),
                            ty_params,
                            variant: None,
                        })),
                    );
                    struct_defs.push((name, s))
                }
                TopLevel::TopVariant(v) => {
                    let Some(tkn) = v.upper_ident_token() else {
                        continue;
                    };
                    let variant_name = self.name_supply.type_idx(&tkn);
                    self.record_def(&tkn, variant_name);

                    let ty_params: Vec<(String, Name)> =
                        v.type_params().map(|p| self.check_ty_param(&p)).collect();

                    let mut alternatives = HashMap::new();
                    for s in v.top_structs() {
                        let Some(tkn) = s.upper_ident_token() else {
                            continue;
                        };
                        let alt_name = self.name_supply.type_idx(&tkn);
                        self.record_def(&tkn, alt_name);

                        if s.type_params().next().is_some() {
                            errors.report(&tkn, TypeParamInVariantStruct);
                        }

                        self.context.declare_type_def(
                            tkn.text(),
                            alt_name,
                            TypeDef::Struct(Rc::new(StructDef {
                                name: alt_name,
                                span: s.syntax().text_range(),
                                ty_params: ty_params.clone(),
                                variant: Some(variant_name),
                            })),
                        );
                        alternatives.insert(tkn.text().to_string(), alt_name);
                        struct_defs.push((alt_name, s))
                    }

                    self.context.declare_type_def(
                        tkn.text(),
                        variant_name,
                        TypeDef::Variant(Rc::new(VariantDef {
                            name: variant_name,
                            span: v.syntax().text_range(),
                            ty_params,
                            alternatives,
                        })),
                    );
                }
                _ => {}
            }
        }

        // - 2. Check types of struct fields
        for (name, s) in struct_defs {
            let Some(TypeDef::Struct(def)) = self.context.lookup_type_def(name) else {
                panic!("Impossible! Failed to look up a struct def");
            };

            let mut fields = StructFields {
                fields: HashMap::new(),
            };

            self.context.clear_type_vars();
            for (s, n) in &def.ty_params {
                self.context.add_type_var(s.clone(), *n)
            }
            for field in s.struct_fields() {
                let Some(field_name) = field.ident_token() else {
                    continue;
                };
                let ty = match field.ty() {
                    Some(field_ty) => self.check_ty(errors, &field_ty),
                    None => Ty::Error,
                };
                let name = self.name_supply.field_idx(&field_name);
                self.record_def(&field_name, name);

                fields
                    .fields
                    .insert(field_name.text().to_string(), (name, ty));
            }
            self.context.clear_type_vars();
            self.context.field_defs.insert(name, fields);
        }
        let mut type_defs = vec![];
        for (name, def) in self.context.type_defs.iter() {
            match def {
                TypeDef::Variant(v) => type_defs.push(ir::TypeDef::Variant(ir::Variant {
                    name: *name,
                    span: v.span,
                    alternatives: v.alternatives.values().copied().collect(),
                })),
                TypeDef::Struct(s) => type_defs.push(ir::TypeDef::Struct(ir::Struct {
                    name: *name,
                    span: s.span,
                    variant: s.variant,
                    fields: self.context.field_defs.get(name).unwrap().to_ir(),
                })),
            }
        }
        Some(type_defs)
    }

    fn check_function_headers(&mut self, errors: &mut TyErrors, root: &Root) {
        for top_level in root.top_levels() {
            if let TopLevel::TopFn(top_fn) = top_level {
                let Some(fn_name_tkn) = top_fn.ident_token() else {
                    continue;
                };

                let mut ty_args = vec![];
                for ty_arg in top_fn.param_tys() {
                    let Some(tkn) = ty_arg.ident_token() else {
                        continue;
                    };
                    let name = self.name_supply.type_var(&tkn);
                    self.record_def(&tkn, name);
                    self.context.add_type_var(tkn.to_string(), name);
                    ty_args.push((tkn.to_string(), name))
                }

                let mut arguments = vec![];
                for param in top_fn.params() {
                    let ty = param
                        .ty()
                        .map(|t| self.check_ty(errors, &t))
                        .unwrap_or(Ty::Error);
                    arguments.push(ty);
                }
                let name = self.name_supply.func_idx(&fn_name_tkn);
                self.record_def(&fn_name_tkn, name);
                let result = top_fn
                    .ty()
                    .map(|t| self.check_ty(errors, &t))
                    .unwrap_or(Ty::Unit);
                self.context.add_func(
                    fn_name_tkn.text().to_string(),
                    name,
                    ty_args,
                    FuncTy { arguments, result },
                );
                self.context.clear_type_vars()
            }
        }
    }

    fn check_ty(&mut self, errors: &mut TyErrors, ty: &Type) -> Ty {
        match ty {
            Type::TyInt(_) => Ty::I32,
            Type::TyFloat(_) => Ty::F32,
            Type::TyBool(_) => Ty::Bool,
            Type::TyUnit(_) => Ty::Unit,
            Type::TyArray(t) => match t.elem().map(|e| self.check_ty(errors, &e)) {
                Some(elem_ty) => Ty::Array(Box::new(elem_ty)),
                None => Ty::Array(Box::new(Ty::Error)),
            },
            Type::TyCons(t) => {
                let ty_args: Vec<Ty> = t.type_args().map(|t| self.check_ty(errors, &t)).collect();
                if let Some(ty) = t.qualifier().map(|q| q.upper_ident_token().unwrap()) {
                    let Some(TypeDef::Variant(def)) = self.context.lookup_type(ty.text()) else {
                        errors.report(&ty, UnknownType(ty.text().to_string()));
                        return Ty::Error;
                    };
                    self.record_ref(&ty, def.name);

                    let Some(alt) = t.upper_ident_token() else {
                        return Ty::Error;
                    };
                    let Some(name) = def.alternatives.get(alt.text()) else {
                        errors.report(&alt, UnknownType(format!("{}::{}", ty.text(), alt.text())));
                        return Ty::Error;
                    };
                    self.record_ref(&alt, *name);

                    let ty_param_names: Vec<Name> = def.ty_params.iter().map(|(_, n)| *n).collect();
                    let subst = Substitution::new(&ty_param_names, &ty_args);

                    Ty::Cons {
                        name: *name,
                        ty_args: subst,
                    }
                } else {
                    let ty_name = t.upper_ident_token().unwrap();
                    let Some(def) = self.context.lookup_type(ty_name.text()) else {
                        errors.report(&ty_name, UnknownType(ty_name.text().to_string()));
                        return Ty::Error;
                    };
                    self.record_ref(&ty_name, def.name());

                    let ty_param_names: Vec<Name> =
                        def.ty_params().iter().map(|(_, n)| *n).collect();
                    let subst = Substitution::new(&ty_param_names, &ty_args);
                    Ty::Cons {
                        name: def.name(),
                        ty_args: subst,
                    }
                }
            }
            Type::TyVar(v) => {
                let tkn = v.ident_token().unwrap();
                if let Some(name) = self.context.lookup_type_var(tkn.text()) {
                    self.record_ref(&tkn, name);
                    Ty::Var(name)
                } else {
                    errors.report(&tkn, UnknownType(tkn.to_string()));
                    Ty::Error
                }
            }
            Type::TyFn(t) => {
                let mut arguments = vec![];
                if let Some(arg_list) = t.ty_arg_list() {
                    for arg in arg_list.types() {
                        arguments.push(self.check_ty(errors, &arg))
                    }
                }
                let result = t
                    .result()
                    .map_or_else(|| Ty::Error, |t| self.check_ty(errors, &t));
                let func_ty = FuncTy { arguments, result };
                Ty::Func(Box::new(func_ty))
            }
        }
    }

    fn check_globals(&mut self, errors: &mut TyErrors, root: &Root) -> Vec<ir::Global> {
        let mut globals = vec![];
        for top_level in root.top_levels() {
            if let TopLevel::TopGlobal(top_global) = top_level {
                let (ty, ir) = match (
                    top_global.ty().map(|t| self.check_ty(errors, &t)),
                    top_global.expr(),
                ) {
                    (None, None) => {
                        continue;
                    }
                    (Some(ty), None) => (ty, None),
                    (None, Some(e)) => self.infer_expr(errors, &e),
                    (Some(ty), Some(e)) => {
                        let ir = self.check_expr(errors, &e, &ty);
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

    fn check_function_bodies(
        &mut self,
        errors: &mut TyErrors,
        root: &Root,
    ) -> Option<Vec<ir::Func>> {
        let mut funcs = Some(vec![]);
        for top_level in root.top_levels() {
            if let TopLevel::TopFn(top_fn) = top_level {
                let mut builder = ir::FuncBuilder::default();
                let Some(func_name) = top_fn.ident_token() else {
                    funcs = None;
                    continue;
                };
                let Some(def) = self.context.lookup_func(func_name.text()) else {
                    panic!("didn't pre-declare function, {}", func_name.text())
                };

                builder.name(Some(def.name));

                let func_ty = def.ty.clone();
                self.context.enter_block();
                for (v, name) in def.ty_params.iter() {
                    builder.ty_params(Some(*name));
                    // TODO ideally we shouldn't need to clone here
                    self.context.add_type_var(v.clone(), *name);
                }

                for (param, ty) in top_fn.params().zip(func_ty.arguments.into_iter()) {
                    let Some(ident_tkn) = param.ident_token() else {
                        funcs = None;
                        continue;
                    };
                    let name = self.name_supply.local_idx(&ident_tkn);
                    builder.params(Some((name, ty.clone())));
                    self.record_def(&ident_tkn, name);
                    self.context.add_var(ident_tkn.text().to_string(), ty, name);
                }

                builder.return_ty(Some(func_ty.result.clone()));

                if let Some(body) = top_fn.body() {
                    // println!("Checking body {}", func_name.text());
                    builder.body(self.check_expr(errors, &body.into(), &func_ty.result));
                }

                self.context.clear_type_vars();
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

    fn infer_literal(&mut self, errors: &mut TyErrors, lit: &Literal) -> (Ty, Option<ir::Lit>) {
        let (ty, it) = match lit {
            Literal::LitBool(b) => (Ty::Bool, Some(ir::LitData::Bool(b.true_token().is_some()))),
            Literal::LitFloat(l) => {
                let float_tkn = l.float_lit_token().unwrap();
                if let Ok(float) = float_tkn.text().parse::<f32>() {
                    (Ty::F32, Some(ir::LitData::F32(float)))
                } else {
                    errors.report(&float_tkn, InvalidLiteral);
                    (Ty::F32, None)
                }
            }
            Literal::LitInt(l) => {
                let int = if let Some(tkn) = l.int_lit_token() {
                    i32::from_str_radix(tkn.text(), 10)
                } else if let Some(tkn) = l.binary_lit_token() {
                    i32::from_str_radix(tkn.text().strip_prefix("0b").unwrap(), 2)
                } else if let Some(tkn) = l.hex_lit_token() {
                    i32::from_str_radix(tkn.text().strip_prefix("0x").unwrap(), 16)
                } else {
                    panic!("No token for int literal");
                };
                if let Ok(int) = int {
                    (Ty::I32, Some(ir::LitData::I32(int)))
                } else {
                    errors.report(l, InvalidLiteral);
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

    fn infer_callee(
        &mut self,
        errors: &mut TyErrors,
        expr: &Expr,
        ty_args: &[Ty],
    ) -> (Ty, Option<ir::Callee>) {
        if let Expr::EVar(v) = expr {
            let var_tkn = v.ident_token().unwrap();
            if self.context.lookup_var(var_tkn.text()).is_some() {
                if !ty_args.is_empty() {
                    errors.report(&var_tkn, CantInstantiateFunctionRef);
                    return (Ty::Error, None);
                }
                let (ty, ir) = self.infer_expr(errors, expr);
                (ty, ir.map(ir::Callee::FuncRef))
            } else if let Some(def) = self.context.lookup_func(var_tkn.text()) {
                self.record_ref(&var_tkn, def.name);
                let params: Vec<Name> = def.ty_params.iter().map(|(_, name)| *name).collect();
                if params.len() != ty_args.len() {
                    errors.report(expr, TyArgCountMismatch(params.len(), ty_args.len()));
                    return (Ty::Error, None);
                }
                let subst = Substitution::new(&params, ty_args);
                let ty = subst.apply_func(def.ty.clone());
                (
                    Ty::Func(Box::new(ty)),
                    Some(ir::Callee::Func {
                        name: def.name,
                        type_args: subst,
                    }),
                )
            } else if let Some(builtin) = lookup_builtin(var_tkn.text()) {
                if builtin.ty_params.len() != ty_args.len() {
                    errors.report(
                        expr,
                        TyArgCountMismatch(builtin.ty_params.len(), ty_args.len()),
                    );
                    return (Ty::Error, None);
                }
                let subst = Substitution::new(&builtin.ty_params, ty_args);
                (
                    Ty::Func(Box::new(subst.apply_func(builtin.ty.clone()))),
                    Some(ir::Callee::Builtin(builtin.name)),
                )
            } else {
                errors.report(expr, UnknownVar(var_tkn.text().to_string()));
                return (Ty::Error, None);
            }
        } else {
            let (ty, ir) = self.infer_expr(errors, expr);
            (ty, ir.map(ir::Callee::FuncRef))
        }
    }

    fn infer_expr(&mut self, errors: &mut TyErrors, expr: &Expr) -> (Ty, Option<ir::Expr>) {
        match self.infer_expr_inner(errors, expr) {
            Some((ty, ir)) => {
                self.record_typed(expr.syntax(), &ty);
                (ty, ir)
            }
            None => (Ty::Error, None),
        }
    }

    fn infer_expr_inner(
        &mut self,
        errors: &mut TyErrors,
        expr: &Expr,
    ) -> Option<(Ty, Option<ir::Expr>)> {
        let (ty, ir): (Ty, Option<ir::ExprData>) = match expr {
            Expr::EArray(arr) => {
                let mut builder = ir::ArrayBuilder::default();
                let mut elems = arr.exprs();
                if let Some(first_elem) = elems.next() {
                    let (elem_ty, elem_ir) = self.infer_expr(errors, &first_elem);
                    builder.elems(elem_ir);
                    for elem in elems {
                        builder.elems(self.check_expr(errors, &elem, &elem_ty));
                    }
                    (Ty::Array(Box::new(elem_ty)), builder.build())
                } else {
                    errors.report(arr, CantInferEmptyArray);
                    (Ty::Array(Box::new(Ty::Error)), None)
                }
            }
            Expr::ELit(l) => {
                let (ty, ir) = self.infer_literal(errors, &l.literal().unwrap());
                let mut builder = LitBuilder::default();
                builder.lit(ir);
                (ty, builder.build())
            }
            Expr::EVar(v) => {
                let var_tkn = v.ident_token().unwrap();
                match self.context.lookup_var_or_func(var_tkn.text()) {
                    None => {
                        errors.report(&var_tkn, UnknownVar(var_tkn.text().to_string()));
                        return None;
                    }
                    Some((ty, name)) => {
                        let mut builder = VarBuilder::default();
                        builder.name(Some(name));
                        let ir = builder.build();
                        self.record_ref(&var_tkn, name);
                        (ty, ir)
                    }
                }
            }
            Expr::EStruct(struct_expr) => {
                let (struct_def, struct_name_tkn) = if let Some(ty) = struct_expr
                    .qualifier()
                    .map(|q| q.upper_ident_token().unwrap())
                {
                    let alt = struct_expr.upper_ident_token()?;
                    let struct_def = self.context.lookup_variant(ty.text()).and_then(|def| {
                        self.record_ref(&ty, def.name);
                        def.lookup_alternative(alt.text())
                            .map(|alt_name| self.context.lookup_struct_name(alt_name))
                    });
                    (struct_def, alt)
                } else {
                    let tkn = struct_expr.upper_ident_token()?;
                    (self.context.lookup_struct(tkn.text()), tkn)
                };
                match struct_def {
                    None => {
                        errors.report(
                            &struct_name_tkn,
                            UnknownType(struct_name_tkn.text().to_string()),
                        );
                        return None;
                    }
                    Some(def) => {
                        let name = def.name;
                        self.record_ref(&struct_name_tkn, name);
                        let mut builder = ir::StructBuilder::default();
                        builder.name(Some(name));

                        let subst = if let Some(ty_arg_list) = struct_expr.e_ty_arg_list() {
                            let ty_arg_names: Vec<Name> =
                                def.ty_params.iter().map(|(_, name)| *name).collect();
                            let tys: Vec<Ty> = ty_arg_list
                                .types()
                                .map(|t| self.check_ty(errors, &t))
                                .collect();
                            if ty_arg_names.len() != tys.len() {
                                errors.report(
                                    &ty_arg_list,
                                    TyArgCountMismatch(ty_arg_names.len(), tys.len()),
                                )
                            }
                            Substitution::new(&ty_arg_names, &tys)
                        } else {
                            Substitution::default()
                        };

                        let mut seen = vec![];
                        for field in struct_expr.e_struct_fields() {
                            let Some(field_tkn) = field.ident_token() else {
                                continue;
                            };
                            let Some((field_name, ty)) =
                                self.context.get_fields(name).get(field_tkn.text()).cloned()
                            else {
                                errors.report(
                                    &field_tkn,
                                    UnknownField {
                                        struct_name: name,
                                        field_name: field_tkn.text().to_string(),
                                    },
                                );
                                continue;
                            };
                            self.record_ref(&field_tkn, field_name);
                            seen.push(field_name);
                            if let Some(field_expr) = field.expr() {
                                builder.fields(
                                    self.check_expr(errors, &field_expr, &subst.apply(ty.clone()))
                                        .map(|e| (field_name, e)),
                                );
                            }
                        }
                        // TODO report all missing fields at once
                        for field_name in self.context.field_defs.get(&name).unwrap().names() {
                            if !seen.contains(&field_name) {
                                errors.report(
                                    &struct_name_tkn,
                                    MissingField {
                                        struct_name: name,
                                        field_name,
                                    },
                                );
                            }
                        }
                        (
                            Ty::Cons {
                                name: def.variant.unwrap_or(name),
                                ty_args: subst,
                            },
                            builder.build(),
                        )
                    }
                }
            }
            Expr::ECall(call_expr) => {
                let func_expr = call_expr.expr()?;
                let ty_arg_list: Vec<Ty> = call_expr
                    .e_ty_arg_list()
                    .map(|tas| tas.types().map(|t| self.check_ty(errors, &t)).collect())
                    .unwrap_or_default();
                match self.infer_callee(errors, &func_expr, &ty_arg_list) {
                    (Ty::Func(func_ty), callee) => {
                        let mut builder = ir::CallBuilder::default();
                        builder.func(callee);
                        if let Some(arg_list) = call_expr.e_arg_list() {
                            let arg_exprs: Vec<Expr> = arg_list.exprs().collect();
                            let arg_tys = func_ty.arguments;
                            if arg_exprs.len() != arg_tys.len() {
                                errors.report(
                                    &arg_list,
                                    ArgCountMismatch(arg_tys.len(), arg_exprs.len()),
                                );
                            }
                            for (param, expected_ty) in arg_exprs.iter().zip(arg_tys.iter()) {
                                builder.arguments(self.check_expr(errors, param, expected_ty));
                            }
                        }
                        (func_ty.result, builder.build())
                    }
                    (ty, _) => {
                        if ty != Ty::Error {
                            errors.report(&func_expr, NotAFunction(ty));
                        }
                        return None;
                    }
                }
            }
            Expr::EParen(e) => {
                let (ty, ir) = self.infer_expr(errors, &e.expr()?);
                (ty, ir.map(|x| *x.it))
            }
            Expr::EIf(if_expr) => {
                let mut builder = ir::IfBuilder::default();
                if let Some(condition) = if_expr.condition() {
                    builder.condition(self.check_expr(errors, &condition, &Ty::Bool));
                }
                if let Some(then_branch) = if_expr.then_branch() {
                    let (ty, ir) = self.infer_expr(errors, &then_branch);
                    builder.then_branch(ir);
                    if let Some(else_branch) = if_expr.else_branch() {
                        builder.else_branch(self.check_expr(errors, &else_branch, &ty));
                    }
                    (ty, builder.build())
                } else {
                    (Ty::Error, None)
                }
            }
            Expr::EMatch(match_expr) => self.check_match(errors, match_expr, None),
            Expr::EArrayIdx(idx_expr) => {
                let mut builder = ir::ArrayIdxBuilder::default();
                let arr_expr = idx_expr.expr().unwrap();
                if let Some(index) = idx_expr.index() {
                    builder.index(self.check_expr(errors, &index, &Ty::I32));
                }
                let elem_ty = match self.infer_expr(errors, &arr_expr) {
                    (Ty::Array(elem_ty), ir) => {
                        builder.array(ir);
                        *elem_ty
                    }
                    (ty, _) => {
                        if ty != Ty::Error {
                            errors.report(&arr_expr, NonArrayIdx(ty))
                        }
                        return None;
                    }
                };
                (elem_ty, builder.build())
            }
            Expr::EStructIdx(idx_expr) => {
                let mut builder = ir::StructIdxBuilder::default();
                let struct_expr = idx_expr.expr().unwrap();
                let (ty_receiver, receiver_ir) = self.infer_expr(errors, &struct_expr);
                builder.expr(receiver_ir);

                let field_name_tkn = idx_expr.ident_token()?;
                let (field_name, ty) =
                    self.check_struct_idx(errors, &ty_receiver, &field_name_tkn)?;
                builder.index(Some(field_name));
                (ty, builder.build())
            }
            Expr::EBinary(bin_expr) => {
                let mut builder = ir::BinaryBuilder::default();
                let (lhs_ty, lhs_ir) = self.infer_expr(errors, &bin_expr.lhs()?);
                builder.left(lhs_ir);
                // TODO could maybe check the rhs based on operator and lhs?
                let (rhs_ty, rhs_ir) = self.infer_expr(errors, &bin_expr.rhs()?);
                builder.right(rhs_ir);
                let op_tkn = bin_expr.op()?;
                if lhs_ty == Ty::Error || rhs_ty == Ty::Error {
                    return None;
                }
                match check_op(&op_tkn, &lhs_ty, &rhs_ty) {
                    None => {
                        errors.report(
                            &op_tkn,
                            Message(format!(
                                "Invalid operator {} for lhs of type {} and rhs of type {}",
                                op_tkn.text(),
                                lhs_ty.display(self.name_supply.name_map()),
                                rhs_ty.display(self.name_supply.name_map())
                            )),
                        );
                        return None;
                    }
                    Some((op_data, ty)) => {
                        builder.op(Some(ir::Op {
                            it: op_data,
                            at: op_tkn.text_range(),
                        }));
                        (ty, builder.build())
                    }
                }
            }
            Expr::EBlock(block_expr) => {
                self.context.enter_block();
                let (last_expr, mut builder) = self.infer_block(errors, block_expr);
                let ty = if let Some(last_expr) = last_expr {
                    let (ty, ir) = self.infer_expr(errors, &last_expr);
                    builder.expr(ir);
                    ty
                } else {
                    builder.expr(unit_lit(block_expr.syntax().text_range()));
                    Ty::Unit
                };
                self.context.leave_block();
                (ty, builder.build())
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

    fn check_match(
        &mut self,
        errors: &mut TyErrors,
        match_expr: &EMatch,
        expected: Option<Ty>,
    ) -> (Ty, Option<ir::ExprData>) {
        let mut builder = ir::MatchBuilder::default();
        let Some(scrutinee) = match_expr.scrutinee() else {
            return (Ty::Error, None);
        };

        let (ty_scrutinee, ir_scrutinee) = self.infer_expr(errors, &scrutinee);
        builder.scrutinee(ir_scrutinee);
        let mut ty = expected;
        for branch in match_expr.e_match_branchs() {
            let (Some(pattern), Some(body)) = (branch.pattern(), branch.body()) else {
                continue;
            };
            self.context.enter_block();
            let pattern_ir = self.check_pattern(errors, &pattern, &ty_scrutinee);
            let body_ir = if let Some(expected) = &ty {
                self.check_expr(errors, &body.into(), expected)
            } else {
                let (ty_body, ir) = self.infer_expr(errors, &body.into());
                if ty_body != Ty::Error {
                    ty = Some(ty_body);
                }
                ir
            };

            if let (Some(pattern_ir), Some(body_ir)) = (pattern_ir, body_ir) {
                builder.branches(Some(ir::MatchBranch {
                    at: branch.syntax().text_range(),
                    pattern: pattern_ir,
                    body: body_ir,
                }));
            }
            self.context.leave_block();
        }
        (ty.unwrap_or(Ty::Error), builder.build())
    }

    // Infers all declarations in the given block, and returns the trailing expression iff it exists
    fn infer_block(
        &mut self,
        errors: &mut TyErrors,
        block_expr: &EBlock,
    ) -> (Option<Expr>, ir::BlockBuilder) {
        let mut builder = ir::BlockBuilder::default();
        let declarations: Vec<Declaration> = block_expr.declarations().collect();
        let last_expr = if let Some((last, declarations)) = declarations.split_last() {
            for decl in declarations {
                let (_, ir) = self.infer_decl(errors, decl);
                builder.declarations(ir);
            }
            match last {
                Declaration::DExpr(expr) => expr.expr(),
                decl => {
                    let (_, ir) = self.infer_decl(errors, decl);
                    builder.declarations(ir);
                    None
                }
            }
        } else {
            None
        };
        (last_expr, builder)
    }

    fn check_expr(
        &mut self,
        errors: &mut TyErrors,
        expr: &Expr,
        expected: &Ty,
    ) -> Option<ir::Expr> {
        let ir = match (expr, expected) {
            (Expr::EArray(expr), Ty::Array(elem_ty)) => {
                let mut builder = ir::ArrayBuilder::default();
                for elem in expr.exprs() {
                    builder.elems(self.check_expr(errors, &elem, elem_ty));
                }
                builder.build()
            }
            (Expr::EArrayIdx(idx_expr), elem_ty) => {
                let mut builder = ir::ArrayIdxBuilder::default();
                let arr_expr = idx_expr.expr().unwrap();
                builder.array(self.check_expr(
                    errors,
                    &arr_expr,
                    &Ty::Array(Box::new(elem_ty.clone())),
                ));
                if let Some(index) = idx_expr.index() {
                    builder.index(self.check_expr(errors, &index, &Ty::I32));
                }
                builder.build()
            }
            (Expr::EIf(expr), ty) => {
                let mut builder = ir::IfBuilder::default();
                if let Some(condition) = expr.condition() {
                    builder.condition(self.check_expr(errors, &condition, &Ty::Bool));
                }
                if let Some(then_branch) = expr.then_branch() {
                    builder.then_branch(self.check_expr(errors, &then_branch, ty));
                }
                if let Some(else_branch) = expr.else_branch() {
                    builder.else_branch(self.check_expr(errors, &else_branch, ty));
                }
                builder.build()
            }
            (Expr::EParen(expr), _) => expr
                .expr()
                .and_then(|expr| self.check_expr(errors, &expr, expected))
                .map(|ir| *ir.it),
            (Expr::EBlock(block_expr), _) => {
                self.context.enter_block();
                let (last_expr, mut builder) = self.infer_block(errors, block_expr);
                if let Some(last_expr) = last_expr {
                    builder.expr(self.check_expr(errors, &last_expr, expected));
                } else {
                    builder.expr(unit_lit(block_expr.syntax().text_range()));
                    if !matches!(expected, Ty::Unit | Ty::Error) {
                        errors.report(
                            block_expr,
                            TypeMismatch {
                                expected: expected.clone(),
                                actual: Ty::Unit,
                            },
                        )
                    }
                };
                self.context.leave_block();
                builder.build()
            }
            (Expr::EMatch(match_expr), _) => {
                self.check_match(errors, match_expr, Some(expected.clone()))
                    .1
            }
            _ => {
                let (ty, ir) = self.infer_expr(errors, expr);
                if *expected != Ty::Error && ty != Ty::Error && ty.ne(expected) {
                    println!("{ty:?} {expected:?}");
                    errors.report(
                        expr,
                        TypeMismatch {
                            expected: expected.clone(),
                            actual: ty,
                        },
                    );
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
        errors: &mut TyErrors,
        receiver: &Ty,
        field_name_tkn: &SyntaxToken,
    ) -> Option<(Name, Ty)> {
        let (def, ty_args, field) = match receiver {
            Ty::Cons { name, ty_args } => {
                let type_def = self
                    .context
                    .lookup_type_def(*name)
                    .expect("inferred a type that wasn't defined");
                match type_def {
                    TypeDef::Struct(s) => (
                        s,
                        ty_args,
                        self.context
                            .get_fields(*name)
                            .get(field_name_tkn.text())
                            .cloned(),
                    ),
                    TypeDef::Variant(_) => {
                        errors.report(field_name_tkn, NonStructIdx(receiver.clone()));
                        return None;
                    }
                }
            }
            ty => {
                if *ty != Ty::Error {
                    errors.report(field_name_tkn, NonStructIdx(ty.clone()))
                }
                return None;
            }
        };
        match field {
            None => {
                errors.report(
                    field_name_tkn,
                    UnknownField {
                        struct_name: def.name,
                        field_name: field_name_tkn.text().to_string(),
                    },
                );
                None
            }
            Some((n, t)) => {
                self.record_ref(field_name_tkn, n);
                Some((n, ty_args.apply(t)))
            }
        }
    }

    fn infer_decl(
        &mut self,
        errors: &mut TyErrors,
        decl: &Declaration,
    ) -> (Ty, Option<ir::Declaration>) {
        let (ty, ir) = match decl {
            Declaration::DLet(let_decl) => {
                let mut builder = ir::LetBuilder::default();
                let (ty, ir) = if let Some(expr) = let_decl.expr() {
                    if let Some(ty) = let_decl.ty().map(|ta| self.check_ty(errors, &ta)) {
                        let ir = self.check_expr(errors, &expr, &ty);
                        (ty, ir)
                    } else {
                        self.infer_expr(errors, &expr)
                    }
                } else {
                    (Ty::Error, None)
                };
                builder.expr(ir);

                if let Some(binder_tkn) = let_decl.ident_token() {
                    let name = self.name_supply.local_idx(&binder_tkn);
                    builder.binder(Some(name));
                    self.record_def(&binder_tkn, name);
                    // TODO can't record this as typed because the ident token is not a syntax node
                    // self.record_typed(, &ty)
                    self.context
                        .add_var(binder_tkn.text().to_string(), ty, name)
                }
                (Ty::Unit, builder.build())
            }
            Declaration::DSet(set_decl) => {
                let mut builder = ir::SetBuilder::default();
                let set_ty = if let Some(set_target) = set_decl.set_target() {
                    let (ty, ir) = self.infer_set_target(errors, &set_target);
                    builder.set_target(ir);
                    ty
                } else {
                    Ty::Error
                };
                if let Some(expr) = set_decl.expr() {
                    builder.expr(self.check_expr(errors, &expr, &set_ty));
                }
                (Ty::Unit, builder.build())
            }
            Declaration::DWhile(while_decl) => {
                let mut builder = ir::WhileBuilder::default();
                let condition = while_decl.expr();
                if let Some(condition) = condition {
                    builder.condition(self.check_expr(errors, &condition, &Ty::Bool));
                }
                let body = while_decl.e_block();
                if let Some(body) = body {
                    builder.body(self.check_expr(errors, &body.into(), &Ty::Unit));
                }
                (Ty::Unit, builder.build())
            }
            Declaration::DExpr(decl) => {
                let mut builder = ExprBuilder::default();
                let (ty, ir) = self.infer_expr(errors, &decl.expr().unwrap());
                builder.expr(ir);
                (ty, builder.build())
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

    fn infer_set_target(
        &mut self,
        errors: &mut TyErrors,
        set_target: &SetTarget,
    ) -> (Ty, Option<ir::SetTarget>) {
        let Some(expr) = set_target.set_target_expr() else {
            return (Ty::Error, None);
        };
        let (ty, ir_data) = match expr {
            SetTargetExpr::EVar(var) => {
                let ident_tkn = var.ident_token().unwrap();
                if let Some((ty, name)) = self.context.lookup_var_or_func(ident_tkn.text()) {
                    let ir = ir::SetTargetData::SetVar { name };
                    self.record_ref(&ident_tkn, name);
                    (ty, Some(ir))
                } else {
                    errors.report(&ident_tkn, UnknownVar(ident_tkn.text().to_string()));
                    (Ty::Error, None)
                }
            }
            SetTargetExpr::EArrayIdx(arr_idx) => {
                let mut builder = ir::SetArrayBuilder::default();
                let arr_ty = arr_idx.expr().map(|target| {
                    let (ty, ir) = self.infer_expr(errors, &target);
                    builder.target(ir);
                    ty
                });

                if let Some(index) = arr_idx.index() {
                    builder.index(self.check_expr(errors, &index, &Ty::I32));
                }

                match arr_ty {
                    None | Some(Ty::Error) => (Ty::Error, None),
                    Some(Ty::Array(elem_ty)) => ((*elem_ty).clone(), builder.build()),
                    Some(t) => {
                        errors.report(&arr_idx, NonArrayIdx(t));
                        (Ty::Error, None)
                    }
                }
            }
            SetTargetExpr::EStructIdx(struct_idx) => {
                let mut builder = ir::SetStructBuilder::default();
                let Some(target_expr) = struct_idx.expr() else {
                    return (Ty::Error, None);
                };
                let (target_ty, ir) = self.infer_expr(errors, &target_expr);
                builder.target(ir);
                let Some(field_name_tkn) = struct_idx.ident_token() else {
                    return (Ty::Error, None);
                };

                let Some((name, field_ty)) =
                    self.check_struct_idx(errors, &target_ty, &field_name_tkn)
                else {
                    return (Ty::Error, None);
                };
                builder.index(Some(name));
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

    fn check_pattern(
        &mut self,
        errors: &mut TyErrors,
        pattern: &Pattern,
        expected: &Ty,
    ) -> Option<ir::Pattern> {
        let ir = match pattern {
            Pattern::PatVariant(pat) => {
                let ty = pat.qualifier()?.upper_ident_token()?;
                let ctor = pat.upper_ident_token()?;
                let var = pat.ident_token()?;
                let mut builder = ir::PatVariantBuilder::default();
                let Some(def) = self.context.lookup_variant(ty.text()) else {
                    errors.report(&ty, UnknownType(ty.text().to_string()));
                    return None;
                };
                self.record_ref(&ty, def.name);
                builder.variant(Some(def.name));
                match expected {
                    Ty::Cons { name: s, ty_args } if def.name == *s => {
                        if let Some(struct_name) = def.alternatives.get(ctor.text()) {
                            self.record_ref(&ctor, *struct_name);
                            builder.alternative(Some(*struct_name));
                            let name = self.name_supply.local_idx(&var);
                            self.context.add_var(
                                var.text().to_string(),
                                // TODO Think about this clone
                                Ty::Cons {
                                    name: *struct_name,
                                    ty_args: ty_args.clone(),
                                },
                                name,
                            );
                            builder.binder(Some(name));
                            self.record_def(&var, name);
                            builder.build()
                        } else {
                            errors.report(
                                &ctor,
                                UnknownAlternative {
                                    variant_name: def.name,
                                    alternative: ctor.text().to_string(),
                                },
                            );
                            None
                        }
                    }
                    Ty::Error => None,
                    _ => {
                        errors.report(
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
                let mut builder = PatVarBuilder::default();
                builder.var(Some(name));
                builder.build()
            }
        };
        Some(ir::Pattern {
            it: Box::new(ir?),
            ty: expected.clone(),
            at: pattern.syntax().text_range(),
        })
    }
}

fn check_op(op: &SyntaxToken, ty_left: &Ty, ty_right: &Ty) -> Option<(ir::OpData, Ty)> {
    let op_data = match (op.kind(), ty_left, ty_right) {
        (T![+], Ty::I32, Ty::I32) => (ir::OpData::I32Add, Ty::I32),
        (T![-], Ty::I32, Ty::I32) => (ir::OpData::I32Sub, Ty::I32),
        (T![*], Ty::I32, Ty::I32) => (ir::OpData::I32Mul, Ty::I32),
        (T![/], Ty::I32, Ty::I32) => (ir::OpData::I32Div, Ty::I32),
        (T![<], Ty::I32, Ty::I32) => (ir::OpData::I32Lt, Ty::Bool),
        (T![<=], Ty::I32, Ty::I32) => (ir::OpData::I32Le, Ty::Bool),
        (T![>], Ty::I32, Ty::I32) => (ir::OpData::I32Gt, Ty::Bool),
        (T![>=], Ty::I32, Ty::I32) => (ir::OpData::I32Ge, Ty::Bool),
        (T![==], Ty::I32, Ty::I32) => (ir::OpData::I32Eq, Ty::Bool),
        (T![!=], Ty::I32, Ty::I32) => (ir::OpData::I32Ne, Ty::Bool),

        (T![+], Ty::F32, Ty::F32) => (ir::OpData::F32Add, Ty::F32),
        (T![-], Ty::F32, Ty::F32) => (ir::OpData::F32Sub, Ty::F32),
        (T![*], Ty::F32, Ty::F32) => (ir::OpData::F32Mul, Ty::F32),
        (T![/], Ty::F32, Ty::F32) => (ir::OpData::F32Div, Ty::F32),
        (T![<], Ty::F32, Ty::F32) => (ir::OpData::F32Lt, Ty::Bool),
        (T![<=], Ty::F32, Ty::F32) => (ir::OpData::F32Le, Ty::Bool),
        (T![>], Ty::F32, Ty::F32) => (ir::OpData::F32Gt, Ty::Bool),
        (T![>=], Ty::F32, Ty::F32) => (ir::OpData::F32Ge, Ty::Bool),
        (T![==], Ty::F32, Ty::F32) => (ir::OpData::F32Eq, Ty::Bool),
        (T![!=], Ty::F32, Ty::F32) => (ir::OpData::F32Ne, Ty::Bool),

        (T![==], Ty::Bool, Ty::Bool) => (ir::OpData::BoolEq, Ty::Bool),
        (T![!=], Ty::Bool, Ty::Bool) => (ir::OpData::BoolNe, Ty::Bool),
        (T![&&], Ty::Bool, Ty::Bool) => (ir::OpData::BoolAnd, Ty::Bool),
        (T![||], Ty::Bool, Ty::Bool) => (ir::OpData::BoolOr, Ty::Bool),
        (_, _, _) => return None,
    };
    Some(op_data)
}

fn unit_lit(range: TextRange) -> Option<ir::Expr> {
    Some(ir::Expr {
        at: range,
        ty: Ty::Unit,
        it: Box::new(ir::ExprData::Lit {
            lit: ir::Lit {
                at: range,
                ty: Ty::Unit,
                it: ir::LitData::Unit,
            },
        }),
    })
}
