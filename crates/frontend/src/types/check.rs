use super::names::NameSupply;
use super::{
    error::{
        TyError,
        TyErrorData::{self, *},
        TyErrors,
    },
    Interface,
};
use super::{FuncDef, StructDef, TypeDef, VariantDef};
use crate::ir::{
    self, ExprBuilder, FuncTy, LambdaBuilder, LitBuilder, Name, PatVarBuilder, ReturnBuilder,
    Substitution, Symbol, Ty, VarBuilder,
};
use crate::parser::SyntaxKind;
use crate::syntax::token_ptr::SyntaxTokenPtr;
use crate::syntax::*;
use crate::T;
use crate::{
    builtins::lookup_builtin,
    ir::{ModuleId, NameTag},
};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::mem;
use std::rc::Rc;
use text_size::TextRange;

#[derive(Debug)]
struct Scope {
    values: Vec<HashMap<Symbol, (Ty, Name)>>,
    type_vars: HashMap<Symbol, Name>,
    return_type: Option<Rc<Ty>>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            values: vec![],
            type_vars: HashMap::new(),
            return_type: None,
        }
    }

    fn add_var(&mut self, v: Symbol, ty: Ty, name: Name) {
        self.values.last_mut().unwrap().insert(v, (ty, name));
    }

    fn lookup_var(&self, v: Symbol) -> Option<(Ty, Name)> {
        if let Some((ty, name)) = self.values.iter().rev().find_map(|scope| scope.get(&v)) {
            Some((ty.clone(), *name))
        } else {
            None
        }
    }

    fn add_type_var(&mut self, v: Symbol, name: Name) {
        self.type_vars.insert(v, name);
    }

    fn lookup_type_var(&self, v: Symbol) -> Option<Name> {
        self.type_vars.get(&v).copied()
    }

    fn clear_type_vars(&mut self) {
        self.type_vars.clear()
    }

    fn return_type(&self) -> Option<Rc<Ty>> {
        self.return_type.clone()
    }

    fn set_return_type(&mut self, ty: Ty) -> Option<Rc<Ty>> {
        mem::replace(&mut self.return_type, Some(Rc::new(ty)))
    }

    fn restore_return_type(&mut self, ty: Option<Rc<Ty>>) {
        self.return_type = ty
    }

    fn enter_block(&mut self) {
        self.values.push(HashMap::new())
    }

    fn leave_block(&mut self) {
        self.values.pop().expect("Tried to pop from an empty Ctx");
    }
}

// TyCtx contains top-level function, type, and import definitions.
// Because it is immutable once we've walked the top-level structure of a
// module we keep it in the Typechecker struct for easier access.
// The mutable part of the type checking "context" is called Scope in this module
// and passed as an explicit mutable reference throughout the checking process
#[derive(Debug)]
struct TyCtx<'ctx> {
    functions: HashMap<Symbol, FuncDef>,
    uses: HashMap<Symbol, &'ctx Interface>,

    structs: HashMap<Name, StructDef>,
    variants: HashMap<Name, VariantDef>,
    // NOTE: Does not contain mappings for variant alternatives
    struct_names: HashMap<Symbol, Name>,
    variant_names: HashMap<Symbol, Name>,
}

impl TyCtx<'_> {
    fn new(uses: HashMap<Symbol, &Interface>) -> TyCtx {
        TyCtx {
            functions: HashMap::new(),
            uses,
            structs: HashMap::new(),
            variants: HashMap::new(),
            struct_names: HashMap::new(),
            variant_names: HashMap::new(),
        }
    }

    fn lookup_var_or_func(&self, scope: &Scope, v: Symbol) -> Option<(Ty, Name)> {
        scope.lookup_var(v).or_else(|| {
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

    fn lookup_func(&self, name: Symbol) -> Option<&FuncDef> {
        self.functions.get(&name)
    }

    fn add_func(&mut self, v: Symbol, name: Name, ty_params: Vec<Name>, ty: FuncTy) {
        self.functions.insert(
            v,
            FuncDef {
                name,
                ty_params,
                ty,
            },
        );
    }

    fn declare_struct_def(&mut self, v: Symbol, name: Name, def: StructDef) {
        let is_sub_struct = def.variant.is_some();
        // We don't record String -> Name mapping for variant structs
        // as those are looked up via their Variant name
        if !is_sub_struct {
            self.struct_names.insert(v, name);
        }
        self.structs.insert(name, def);
    }

    fn declare_variant_def(&mut self, v: Symbol, name: Name, def: VariantDef) {
        self.variant_names.insert(v, name);
        self.variants.insert(name, def);
    }

    fn lookup_type_name(&self, v: Symbol) -> Option<Name> {
        self.variant_names
            .get(&v)
            .or_else(|| self.struct_names.get(&v))
            .copied()
    }

    fn lookup_type_def(&self, name: Name) -> Option<TypeDef> {
        self.variants.get(&name).map(TypeDef::Variant).or_else(|| {
            self.structs.get(&name).map(TypeDef::Struct).or_else(|| {
                self.uses
                    .values()
                    .find_map(|iface| iface.lookup_type_name(name))
            })
        })
    }

    fn lookup_type(&self, v: Symbol) -> Option<TypeDef> {
        let n = self.lookup_type_name(v)?;
        let def = self
            .lookup_type_def(n)
            .expect("type declared but not defined");
        Some(def)
    }

    fn lookup_struct_name(&self, name: Name) -> StructDef {
        let Some(TypeDef::Struct(d)) = self.lookup_type_def(name) else {
            panic!("inferred unknown struct name")
        };
        d.clone()
    }

    fn set_fields(&mut self, name: Name, fields: HashMap<String, (Name, Ty)>) {
        // TODO: expect?
        if let Some(def) = self.structs.get_mut(&name) {
            def.fields = fields;
        }
    }

    fn lookup_qual_type_name(&self, q: Symbol, n: Name) -> Option<TypeDef> {
        self.uses
            .get(&q)
            .and_then(|interface| interface.lookup_type_name(n))
    }

    fn lookup_qual_type(&self, q: Symbol, v: &str) -> Option<TypeDef> {
        self.uses
            .get(&q)
            .and_then(|interface| interface.lookup_type(v))
    }

    fn lookup_qual_func(&self, q: Symbol, v: &str) -> Option<&FuncDef> {
        self.uses
            .get(&q)
            .and_then(|interface| interface.lookup_func(v))
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

pub type OccurrenceMap = HashMap<SyntaxTokenPtr, Occurrence<Name>>;

pub struct Typechecker<'ctx> {
    pub occurrences: RefCell<OccurrenceMap>, // Write only
    pub name_supply: NameSupply,
    context: TyCtx<'ctx>,
}

impl Typechecker<'_> {
    pub fn new<'ctx>(
        module: ModuleId,
        deps: &'ctx [(&'ctx str, &'ctx Interface)],
    ) -> Typechecker<'ctx> {
        let name_supply = NameSupply::new(module);
        let mut uses = HashMap::new();
        for (name, interface) in deps {
            uses.insert(name_supply.get_or_intern(name), *interface);
        }
        let context = TyCtx::new(uses);
        Typechecker {
            occurrences: RefCell::new(HashMap::new()),
            name_supply,
            context,
        }
    }

    fn record_def(&self, token: &SyntaxToken, name: Name) {
        let previous_def = self
            .occurrences
            .borrow_mut()
            .insert(SyntaxTokenPtr::new(token), Occurrence::Def(name));
        assert!(previous_def.is_none())
    }

    fn record_ref(&self, token: &SyntaxToken, name: Name) {
        let previous_ref = self
            .occurrences
            .borrow_mut()
            .insert(SyntaxTokenPtr::new(token), Occurrence::Ref(name));
        assert!(previous_ref.is_none())
    }

    pub fn infer_module(
        &mut self,
        module: &Module,
    ) -> (Option<ir::Program>, Interface, Vec<TyError>) {
        let mut errors: TyErrors = TyErrors::new();
        let ir = self.infer_module_inner(&mut errors, module);
        let interface = self.mk_interface(&mut errors, module);
        (ir, interface, errors.errors)
    }

    fn mk_interface(&self, errors: &mut TyErrors, module: &Module) -> Interface {
        let mut interface = Interface::default();
        let Some(header) = module.mod_header() else {
            return interface;
        };
        for export in header.mod_exports() {
            match export {
                ModExport::ModExportVal(n) => {
                    let tkn = n.ident_token().unwrap();
                    let str_name = tkn.text().to_string();
                    let name = self.sym(&str_name);
                    match self.context.lookup_func(name) {
                        Some(f) => {
                            self.record_ref(&tkn, f.name);
                            interface.functions.insert(str_name, (*f).clone());
                        }
                        None => errors.report(
                            &n,
                            UnknownFunction(n.ident_token().unwrap().text().to_string()),
                        ),
                    }
                }
                ModExport::ModExportTy(n) => {
                    let tkn = n.upper_ident_token().unwrap();
                    let str_name = tkn.text().to_string();
                    let name = self.sym(&str_name);
                    match self.context.lookup_type(name) {
                        Some(TypeDef::Struct(def)) => {
                            self.record_ref(&tkn, def.name);
                            interface.struct_names.insert(str_name, def.name);
                            interface.structs.insert(def.name, def.clone());
                        }
                        Some(TypeDef::Variant(def)) => {
                            self.record_ref(&tkn, def.name);
                            interface.variant_names.insert(str_name, def.name);
                            interface.variants.insert(def.name, def.clone());
                            for alt_name in def.alternatives.values() {
                                interface
                                    .structs
                                    .insert(*alt_name, self.context.lookup_struct_name(*alt_name));
                            }
                        }
                        None => errors.report(
                            &n,
                            UnknownType(n.upper_ident_token().unwrap().text().to_string()),
                        ),
                    }
                }
                ModExport::ModExportAll(_) => todo!("Can't export all yet"),
            }
        }
        interface
    }

    fn infer_module_inner(
        &mut self,
        errors: &mut TyErrors,
        module: &Module,
    ) -> Option<ir::Program> {
        // TODO: There should be no need for a top-level scope here.
        // Globals should get their own field on TyCtx
        let mut scope = Scope::new();
        scope.enter_block();

        let types = self.check_type_definitions(errors, module);
        let imports = self.check_imports(errors, module);
        self.check_function_headers(errors, module);

        let globals = self.check_globals(errors, &mut scope, module);
        let functions = self.check_function_bodies(errors, &mut scope, module);

        scope.leave_block();

        Some(ir::Program {
            imports: imports?,
            types: types?,
            globals,
            funcs: functions?,
        })
    }

    fn sym(&self, s: &str) -> Symbol {
        self.name_supply.get_or_intern(s)
    }

    fn check_imports(&mut self, errors: &mut TyErrors, module: &Module) -> Option<Vec<ir::Import>> {
        // Imports should check in the context of an empty scope
        let mut scope = Scope::new();
        let mut imports = vec![];
        for top_level in module.top_levels() {
            if let TopLevel::TopImport(i) = top_level {
                let Some(internal_name_tkn) = i.imp_internal().and_then(|x| x.ident_token()) else {
                    continue;
                };
                let (name, sym) = self.name_supply.func_idx(&internal_name_tkn);
                self.record_def(&internal_name_tkn, name);

                let ty = if let Some(ty_node) = i.ty() {
                    match self.check_ty(errors, &mut scope, &ty_node) {
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
                self.context.add_func(sym, name, vec![], ty);
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

    fn check_ty_param(&self, ty_param: &ParamTy) -> Name {
        let tkn = ty_param.ident_token().unwrap();
        let (name, _) = self.name_supply.type_var(&tkn);
        self.record_def(&tkn, name);
        name
    }

    fn check_type_definitions(
        &mut self,
        errors: &mut TyErrors,
        module: &Module,
    ) -> Option<Vec<ir::TypeDef>> {
        // Because types can be mutually recursive we need two passes:
        // - 1. Forward declare all types and their "shapes"
        let mut struct_defs = vec![];
        for top_level in module.top_levels() {
            match top_level {
                TopLevel::TopStruct(s) => {
                    let Some(tkn) = s.upper_ident_token() else {
                        continue;
                    };
                    let (name, sym) = self.name_supply.type_idx(&tkn);
                    self.record_def(&tkn, name);

                    let ty_params = s.type_params().map(|p| self.check_ty_param(&p)).collect();
                    self.context.declare_struct_def(
                        sym,
                        name,
                        StructDef {
                            name,
                            span: s.syntax().text_range(),
                            ty_params,
                            variant: None,
                            fields: HashMap::new(),
                        },
                    );
                    struct_defs.push((name, s))
                }
                TopLevel::TopVariant(v) => {
                    let Some(tkn) = v.upper_ident_token() else {
                        continue;
                    };
                    let (variant_name, variant_sym) = self.name_supply.type_idx(&tkn);
                    self.record_def(&tkn, variant_name);

                    let ty_params: Vec<Name> =
                        v.type_params().map(|p| self.check_ty_param(&p)).collect();

                    let mut alternatives = HashMap::new();
                    for s in v.top_structs() {
                        let Some(tkn) = s.upper_ident_token() else {
                            continue;
                        };
                        let (alt_name, alt_sym) = self.name_supply.type_idx(&tkn);
                        self.record_def(&tkn, alt_name);

                        if s.type_params().next().is_some() {
                            errors.report(&tkn, TypeParamInVariantStruct);
                        }

                        self.context.declare_struct_def(
                            alt_sym,
                            alt_name,
                            StructDef {
                                name: alt_name,
                                span: s.syntax().text_range(),
                                ty_params: ty_params.clone(),
                                variant: Some(variant_name),
                                fields: HashMap::new(),
                            },
                        );
                        alternatives.insert(tkn.text().to_string(), alt_name);
                        struct_defs.push((alt_name, s))
                    }

                    self.context.declare_variant_def(
                        variant_sym,
                        variant_name,
                        VariantDef {
                            name: variant_name,
                            span: v.syntax().text_range(),
                            ty_params,
                            alternatives,
                        },
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

            let mut fields = HashMap::new();
            let mut scope = Scope::new();
            for n in &def.ty_params {
                let s = self.name_supply.resolve(*n);
                scope.add_type_var(s, *n)
            }
            for field in s.struct_fields() {
                let Some(field_name) = field.ident_token() else {
                    continue;
                };
                let ty = match field.ty() {
                    Some(field_ty) => self.check_ty(errors, &mut scope, &field_ty),
                    None => Ty::Error,
                };
                let (name, _) = self.name_supply.field_idx(&field_name);
                self.record_def(&field_name, name);

                fields.insert(field_name.text().to_string(), (name, ty));
            }
            self.context.set_fields(name, fields);
        }
        let mut type_defs = vec![];
        for (name, def) in &self.context.structs {
            type_defs.push(ir::TypeDef::Struct(ir::Struct {
                name: *name,
                span: def.span,
                variant: def.variant,
                fields: def.fields.values().cloned().collect(),
            }))
        }
        for (name, def) in &self.context.variants {
            type_defs.push(ir::TypeDef::Variant(ir::Variant {
                name: *name,
                span: def.span,
                alternatives: def.alternatives.values().copied().collect(),
            }))
        }
        Some(type_defs)
    }

    fn check_function_headers(&mut self, errors: &mut TyErrors, module: &Module) {
        let mut scope = Scope::new();
        for top_level in module.top_levels() {
            if let TopLevel::TopFn(top_fn) = top_level {
                let Some(fn_name_tkn) = top_fn.ident_token() else {
                    continue;
                };

                let mut ty_args = vec![];
                for ty_arg in top_fn.param_tys() {
                    let Some(tkn) = ty_arg.ident_token() else {
                        continue;
                    };
                    let (name, sym) = self.name_supply.type_var(&tkn);
                    self.record_def(&tkn, name);
                    scope.add_type_var(sym, name);
                    ty_args.push(name)
                }

                let mut arguments = vec![];
                for param in top_fn.params() {
                    let ty = param
                        .ty()
                        .map(|t| self.check_ty(errors, &mut scope, &t))
                        .unwrap_or(Ty::Error);
                    arguments.push(ty);
                }
                let (name, sym) = self.name_supply.func_idx(&fn_name_tkn);
                self.record_def(&fn_name_tkn, name);
                let result = top_fn
                    .ty()
                    .map(|t| self.check_ty(errors, &mut scope, &t))
                    .unwrap_or(Ty::Unit);
                self.context
                    .add_func(sym, name, ty_args, FuncTy { arguments, result });
            }
        }
    }

    fn lookup_type(
        &self,
        errors: &mut TyErrors,
        mod_qualifier: Option<ModQualifier>,
        qualifier: Option<Qualifier>,
        ty_tkn: &SyntaxToken,
    ) -> Option<TypeDef> {
        let mod_qualifier_tkn = mod_qualifier.map(|q| q.ident_token().unwrap());
        if let Some(variant_tkn) = qualifier.map(|q| q.upper_ident_token().unwrap()) {
            let opt_def = if let Some(ref mod_qual_tkn) = mod_qualifier_tkn {
                self.context
                    .lookup_qual_type(self.sym(mod_qual_tkn.text()), variant_tkn.text())
            } else {
                self.context.lookup_type(self.sym(variant_tkn.text()))
            };
            let Some(TypeDef::Variant(def)) = opt_def else {
                // TODO: Non-variant type error?
                errors.report(&variant_tkn, UnknownType(variant_tkn.text().to_string()));
                return None;
            };
            self.record_ref(&variant_tkn, def.name);

            let Some(name) = def.alternatives.get(ty_tkn.text()) else {
                errors.report(
                    ty_tkn,
                    UnknownType(format!("{}::{}", variant_tkn.text(), ty_tkn.text())),
                );
                return None;
            };
            self.record_ref(ty_tkn, *name);
            let opt_struct_def = if let Some(mod_qual_tkn) = mod_qualifier_tkn {
                self.context
                    .lookup_qual_type_name(self.sym(mod_qual_tkn.text()), *name)
            } else {
                self.context.lookup_type_def(*name)
            };

            assert!(
                matches!(opt_struct_def, Some(TypeDef::Struct(_))),
                "Non-struct typedef"
            );
            opt_struct_def
        } else {
            let ty_name = ty_tkn.text();
            let opt_def = if let Some(mod_qual) = mod_qualifier_tkn {
                self.context
                    .lookup_qual_type(self.sym(mod_qual.text()), ty_name)
            } else {
                self.context.lookup_type(self.sym(ty_name))
            };
            let Some(def) = opt_def else {
                errors.report(ty_tkn, UnknownType(ty_name.to_string()));
                return None;
            };
            self.record_ref(ty_tkn, def.name());
            Some(def)
        }
    }

    fn check_ty(&self, errors: &mut TyErrors, scope: &mut Scope, ty: &Type) -> Ty {
        match ty {
            Type::TyInt(_) => Ty::I32,
            Type::TyFloat(_) => Ty::F32,
            Type::TyBool(_) => Ty::Bool,
            Type::TyUnit(_) => Ty::Unit,
            Type::TyBytes(_) => Ty::Bytes,
            Type::TyArray(t) => match t.elem().map(|e| self.check_ty(errors, scope, &e)) {
                Some(elem_ty) => Ty::Array(Box::new(elem_ty)),
                None => Ty::Array(Box::new(Ty::Error)),
            },
            Type::TyCons(t) => {
                let Some(ty_tkn) = t.upper_ident_token() else {
                    return Ty::Error;
                };
                let Some(ty_def) = self.lookup_type(errors, t.mod_qualifier(), None, &ty_tkn)
                else {
                    return Ty::Error;
                };
                let ty_args: Vec<Ty> = t
                    .type_args()
                    .map(|t| self.check_ty(errors, scope, &t))
                    .collect();
                let ty_param_names = ty_def.ty_params();
                if ty_param_names.len() != ty_args.len() {
                    errors.report(t, TyArgCountMismatch(ty_param_names.len(), ty_args.len()));
                    return Ty::Error;
                }
                Ty::Cons {
                    name: ty_def.name(),
                    ty_args: Substitution::new(ty_param_names, &ty_args),
                }
            }
            Type::TyVar(v) => {
                let tkn = v.ident_token().unwrap();
                if let Some(name) = scope.lookup_type_var(self.sym(tkn.text())) {
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
                        arguments.push(self.check_ty(errors, scope, &arg))
                    }
                }
                let result = t
                    .result()
                    .map_or_else(|| Ty::Error, |t| self.check_ty(errors, scope, &t));
                let func_ty = FuncTy { arguments, result };
                Ty::Func(Box::new(func_ty))
            }
        }
    }

    fn check_globals(
        &mut self,
        errors: &mut TyErrors,
        scope: &mut Scope,
        module: &Module,
    ) -> Vec<ir::Global> {
        let mut globals = vec![];
        for top_level in module.top_levels() {
            if let TopLevel::TopGlobal(top_global) = top_level {
                let (ty, ir) = match (
                    top_global.ty().map(|t| self.check_ty(errors, scope, &t)),
                    top_global.expr(),
                ) {
                    (None, None) => {
                        continue;
                    }
                    (Some(ty), None) => (ty, None),
                    (None, Some(e)) => self.infer_expr(errors, scope, &e),
                    (Some(ty), Some(e)) => {
                        let ir = self.check_expr(errors, scope, &e, &ty);
                        (ty, ir)
                    }
                };
                if let Some(binder_tkn) = top_global.ident_token() {
                    let (name, sym) = self.name_supply.global_idx(&binder_tkn);
                    self.record_def(&binder_tkn, name);
                    if let Some(ir) = ir {
                        globals.push(ir::Global {
                            span: top_global.syntax().text_range(),
                            binder: name,
                            init: ir,
                        })
                    }
                    scope.add_var(sym, ty, name)
                };
            }
        }
        globals
    }

    fn check_function_bodies(
        &self,
        errors: &mut TyErrors,
        scope: &mut Scope,
        module: &Module,
    ) -> Option<Vec<ir::Func>> {
        // TODO: Do we really need the Option wrapper here? As long as we report errors on all paths
        // we should be fine to emit funcs anyway.
        let mut funcs = Some(vec![]);
        for top_level in module.top_levels() {
            if let TopLevel::TopFn(top_fn) = top_level {
                let mut builder = ir::FuncBuilder::default();
                let Some(func_name) = top_fn.ident_token() else {
                    funcs = None;
                    continue;
                };
                let Some(def) = self.context.lookup_func(self.sym(func_name.text())) else {
                    panic!("didn't pre-declare function, {}", func_name.text())
                };

                builder.name(Some(def.name));

                let func_ty = def.ty.clone();
                scope.enter_block();
                for name in &def.ty_params {
                    builder.ty_params(Some(*name));
                    let v = self.name_supply.resolve(*name);
                    scope.add_type_var(v, *name);
                }

                for (param, ty) in top_fn.params().zip(func_ty.arguments.into_iter()) {
                    let Some(ident_tkn) = param.ident_token() else {
                        funcs = None;
                        continue;
                    };
                    let (name, sym) = self.name_supply.local_idx(&ident_tkn);
                    builder.params(Some((name, ty.clone())));
                    self.record_def(&ident_tkn, name);
                    scope.add_var(sym, ty, name);
                }

                builder.return_ty(Some(func_ty.result.clone()));
                // It's fine to drop any previous return type here
                let _ = scope.set_return_type(func_ty.result.clone());

                if let Some(body) = top_fn.body() {
                    // println!("Checking body {}", func_name.text());
                    builder.body(self.check_expr(errors, scope, &body.into(), &func_ty.result));
                }

                scope.clear_type_vars();
                scope.leave_block();

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

    fn infer_literal(&self, errors: &mut TyErrors, lit: &Literal) -> (Ty, Option<ir::Lit>) {
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
                    tkn.text().parse()
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
            Literal::LitBytes(s) => {
                let tkn = s.bytes_lit_token().unwrap();
                let without_quotes = tkn
                    .text()
                    .strip_prefix('"')
                    .and_then(|t| t.strip_suffix('"'))
                    .unwrap();
                (
                    Ty::Bytes,
                    Some(ir::LitData::Bytes(without_quotes.to_string())),
                )
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

    fn infer_expr(
        &self,
        errors: &mut TyErrors,
        scope: &mut Scope,
        expr: &Expr,
    ) -> (Ty, Option<ir::Expr>) {
        self.infer_expr_inner(errors, scope, expr)
            .unwrap_or((Ty::Error, None))
    }

    fn infer_expr_inner(
        &self,
        errors: &mut TyErrors,
        scope: &mut Scope,
        expr: &Expr,
    ) -> Option<(Ty, Option<ir::Expr>)> {
        let (ty, ir): (Ty, Option<ir::ExprData>) = match expr {
            Expr::EArray(arr) => {
                let mut builder = ir::ArrayBuilder::default();
                let mut elems = arr.exprs();
                if let Some(first_elem) = elems.next() {
                    let (elem_ty, elem_ir) = self.infer_expr(errors, scope, &first_elem);
                    builder.elems(elem_ir);
                    for elem in elems {
                        builder.elems(self.check_expr(errors, scope, &elem, &elem_ty));
                    }
                    (Ty::Array(Box::new(elem_ty)), builder.build())
                } else {
                    errors.report(arr, CantInferEmptyArray);
                    (Ty::Error, None)
                }
            }
            Expr::ELit(l) => {
                let (ty, ir) = self.infer_literal(errors, &l.literal().unwrap());
                let mut builder = LitBuilder::default();
                builder.lit(ir);
                (ty, builder.build())
            }
            Expr::EVar(v) => {
                let mod_qual_tkn = v.mod_qualifier().map(|q| q.ident_token().unwrap());
                let var_tkn = v.ident_token().unwrap();
                if let Some(mod_qual_tkn) = mod_qual_tkn {
                    match self
                        .context
                        .lookup_qual_func(self.sym(mod_qual_tkn.text()), var_tkn.text())
                    {
                        None => {
                            errors.report(&var_tkn, UnknownVar(var_tkn.text().to_string()));
                            return None;
                        }
                        Some(func_def) => {
                            let mut builder = VarBuilder::default();
                            builder.name(Some(func_def.name));
                            let ir = builder.build();
                            self.record_ref(&var_tkn, func_def.name);
                            (Ty::Func(Box::new(func_def.ty.clone())), ir)
                        }
                    }
                } else {
                    match self
                        .context
                        .lookup_var_or_func(scope, self.sym(var_tkn.text()))
                    {
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
            }
            Expr::EStruct(struct_expr) => self.check_struct(errors, scope, struct_expr, None)?,
            Expr::ECall(call_expr) => self.check_call(errors, scope, call_expr, None)?,
            Expr::EParen(e) => {
                let (ty, ir) = self.infer_expr(errors, scope, &e.expr()?);
                (ty, ir.map(|x| *x.it))
            }
            Expr::EIf(if_expr) => {
                let mut builder = ir::IfBuilder::default();
                if let Some(condition) = if_expr.condition() {
                    builder.condition(self.check_expr(errors, scope, &condition, &Ty::Bool));
                }
                let mut ty = Ty::Error;
                if let Some(then_branch) = if_expr.then_branch() {
                    let (then_ty, ir) = self.infer_expr(errors, scope, &then_branch);
                    ty = then_ty;
                    builder.then_branch(ir);
                }
                if let Some(else_branch) = if_expr.else_branch() {
                    let ir = if matches!(ty, Ty::Error | Ty::Diverge) {
                        let (else_ty, ir) = self.infer_expr(errors, scope, &else_branch);
                        ty = else_ty;
                        ir
                    } else {
                        self.check_expr(errors, scope, &else_branch, &ty)
                    };
                    builder.else_branch(ir);
                }
                (ty, builder.build())
            }
            Expr::EMatch(match_expr) => self.check_match(errors, scope, match_expr, None),
            Expr::EArrayIdx(idx_expr) => {
                let mut builder = ir::ArrayIdxBuilder::default();
                let arr_expr = idx_expr.expr().unwrap();
                if let Some(index) = idx_expr.index() {
                    builder.index(self.check_expr(errors, scope, &index, &Ty::I32));
                }
                let elem_ty = match self.infer_expr(errors, scope, &arr_expr) {
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
                let (ty_receiver, receiver_ir) = self.infer_expr(errors, scope, &struct_expr);
                builder.expr(receiver_ir);

                let field_name_tkn = idx_expr.ident_token()?;
                let (field_name, ty) =
                    self.check_struct_idx(errors, &ty_receiver, &field_name_tkn)?;
                builder.index(Some(field_name));
                (ty, builder.build())
            }
            Expr::EBinary(bin_expr) => {
                let mut builder = ir::BinaryBuilder::default();
                let (lhs_ty, lhs_ir) = self.infer_expr(errors, scope, &bin_expr.lhs()?);
                builder.left(lhs_ir);
                // TODO could maybe check the rhs based on operator and lhs?
                let (rhs_ty, rhs_ir) = self.infer_expr(errors, scope, &bin_expr.rhs()?);
                builder.right(rhs_ir);
                let op_tkn = bin_expr.op()?;
                if lhs_ty == Ty::Error || rhs_ty == Ty::Error {
                    return None;
                }
                match check_op(&op_tkn, &lhs_ty, &rhs_ty) {
                    None => {
                        errors.report(
                            &op_tkn,
                            InvalidOperator(op_tkn.text().to_string(), lhs_ty, rhs_ty),
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
            Expr::ELambda(lambda) => {
                let mut ty_func = FuncTy {
                    arguments: vec![],
                    result: lambda
                        .return_ty()
                        .map(|t| self.check_ty(errors, scope, &t))
                        .unwrap_or(Ty::Unit),
                };
                let mut builder = LambdaBuilder::default();
                builder.return_ty(Some(ty_func.result.clone()));
                scope.enter_block();
                let prev_return_ty = scope.set_return_type(ty_func.result.clone());
                let mut params = HashSet::new();
                for param in lambda.params() {
                    let Some(name_tkn) = param.ident_token() else {
                        continue;
                    };
                    let (name, sym) = self.name_supply.local_idx(&name_tkn);
                    let ty = match param.ty() {
                        // TODO: Produce a type error. We can't infer lambdas
                        None => Ty::Error,
                        Some(t) => self.check_ty(errors, scope, &t),
                    };
                    builder.params(Some((name, ty.clone())));
                    ty_func.arguments.push(ty.clone());
                    params.insert(name);
                    scope.add_var(sym, ty, name);
                }

                if let Some(body) = lambda.body() {
                    let body_ir = self.check_expr(errors, scope, &body, &ty_func.result);
                    if let Some(body_ir) = body_ir {
                        for (n, fvi) in body_ir.free_vars() {
                            if !params.contains(&n) {
                                if let Some(assignment) = fvi.is_assigned {
                                    errors.report(&assignment, CantReassignCapturedVariable(n));
                                } else {
                                    builder.captures(Some((n, fvi.ty.clone())));
                                }
                            }
                        }
                        builder.body(Some(body_ir));
                    }
                }
                scope.leave_block();
                // Restore the previous return type
                scope.restore_return_type(prev_return_ty);
                (Ty::Func(Box::new(ty_func)), builder.build())
            }
            Expr::EBlock(block_expr) => {
                scope.enter_block();
                let (last_expr, mut builder) = self.infer_block(errors, scope, block_expr);
                let ty = if let Some(last_expr) = last_expr {
                    let (ty, ir) = self.infer_expr(errors, scope, &last_expr);
                    builder.expr(ir);
                    ty
                } else {
                    builder.expr(unit_lit(block_expr.syntax().text_range()));
                    Ty::Unit
                };
                scope.leave_block();
                (ty, builder.build())
            }
            Expr::EReturn(return_expr) => {
                let Some(return_ty) = scope.return_type() else {
                    errors.report(return_expr, CantReturnFromGlobal);
                    return None;
                };
                let mut builder = ReturnBuilder::default();
                if let Some(return_value) = return_expr.expr() {
                    builder.expr(self.check_expr(errors, scope, &return_value, return_ty.as_ref()));
                }
                (Ty::Diverge, builder.build())
            }
        };

        let ir_expr = ir.map(|expr_data| ir::Expr {
            at: expr.syntax().text_range(),
            ty: ty.clone(),
            it: Box::new(expr_data),
        });

        Some((ty, ir_expr))
    }

    fn lookup_func(&self, qual: Option<SyntaxToken>, name: &str) -> Option<&FuncDef> {
        if let Some(qual) = qual {
            self.context.lookup_qual_func(self.sym(qual.text()), name)
        } else {
            self.context.lookup_func(self.sym(name))
        }
    }

    fn check_call(
        &self,
        errors: &mut TyErrors,
        scope: &mut Scope,
        call_expr: &ECall,
        expected: Option<&Ty>,
    ) -> Option<(Ty, Option<ir::ExprData>)> {
        let func_expr = call_expr.expr()?;
        let ty_args: Vec<Ty> = call_expr
            .e_ty_arg_list()
            .map(|tas| {
                tas.types()
                    .map(|t| self.check_ty(errors, scope, &t))
                    .collect()
            })
            .unwrap_or_default();
        let callee = if let Expr::EVar(v) = &func_expr {
            let mod_qual_tkn = v.mod_qualifier().map(|q| q.ident_token().unwrap());
            let var_tkn = v.ident_token().unwrap();
            if mod_qual_tkn.is_none() && scope.lookup_var(self.sym(var_tkn.text())).is_some() {
                if !ty_args.is_empty() {
                    errors.report(&var_tkn, CantInstantiateFunctionRef);
                    return None;
                }
                let (ty, ir) = self.infer_expr(errors, scope, &func_expr);
                (ty, ir.map(ir::Callee::FuncRef))
            } else if let Some(def) = self.lookup_func(mod_qual_tkn, var_tkn.text()) {
                self.record_ref(&var_tkn, def.name);
                let ty_params: &[Name] = &def.ty_params;
                // NOTE(early-return-control-flow)
                if !ty_params.is_empty() && ty_args.is_empty() {
                    return self.infer_poly_call(errors, scope, def, &var_tkn, call_expr, expected);
                }
                if ty_params.len() != ty_args.len() {
                    errors.report(
                        &func_expr,
                        TyArgCountMismatch(ty_params.len(), ty_args.len()),
                    );
                    return None;
                }
                let subst = Substitution::new(ty_params, &ty_args);
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
                        &func_expr,
                        TyArgCountMismatch(builtin.ty_params.len(), ty_args.len()),
                    );
                    return None;
                }
                let subst = Substitution::new(&builtin.ty_params, &ty_args);
                (
                    Ty::Func(Box::new(subst.apply_func(builtin.ty.clone()))),
                    Some(ir::Callee::Builtin(builtin.name)),
                )
            } else {
                errors.report(&func_expr, UnknownVar(var_tkn.text().to_string()));
                return None;
            }
        } else {
            let (ty, ir) = self.infer_expr(errors, scope, &func_expr);
            (ty, ir.map(ir::Callee::FuncRef))
        };

        match callee {
            (Ty::Func(func_ty), callee) => {
                let mut builder = ir::CallBuilder::default();
                builder.func(callee);
                if let Some(arg_list) = call_expr.e_arg_list() {
                    let arg_exprs: Vec<Expr> = arg_list.exprs().collect();
                    let arg_tys = func_ty.arguments;
                    if arg_exprs.len() != arg_tys.len() {
                        errors.report(&arg_list, ArgCountMismatch(arg_tys.len(), arg_exprs.len()));
                    }
                    for (param, expected_ty) in arg_exprs.iter().zip(arg_tys.iter()) {
                        builder.arguments(self.check_expr(errors, scope, param, expected_ty));
                    }
                }
                Some((func_ty.result, builder.build()))
            }
            (ty, _) => {
                if ty != Ty::Error {
                    errors.report(&func_expr, NotAFunction(ty));
                }
                None
            }
        }
    }

    fn infer_poly_struct(
        &self,
        errors: &mut TyErrors,
        scope: &mut Scope,
        def: &StructDef,
        struct_tkn: &SyntaxToken,
        struct_expr: &EStruct,
    ) -> Option<(Ty, Option<ir::ExprData>)> {
        let fresh_subst = {
            let mut fresh_subst = Substitution::empty();
            for n in &def.ty_params {
                fresh_subst.insert(*n, Ty::Var(self.name_supply.gen_idx("poly")));
            }
            fresh_subst
        };
        let mut subst = Substitution::empty();
        let mut builder = ir::StructBuilder::default();
        builder.name(Some(def.name));

        let mut seen = vec![];
        for field in struct_expr.e_struct_fields() {
            let Some((name, expected_ty)) = self.lookup_struct_field(errors, def.name, &field)
            else {
                // NOTE: no need to report an error here, because `lookup_struct_field` already does
                continue;
            };
            seen.push(name);
            let Some(expr) = field.expr() else {
                continue;
            };
            let applied_ty = subst.apply(fresh_subst.apply(expected_ty));
            let ir = if applied_ty.vars().iter().any(|v| v.tag == NameTag::Gen) {
                let (ty, ir) = self.infer_expr(errors, scope, &expr);
                if ty != Ty::Error {
                    if let Err(err) = match_ty(&mut subst, applied_ty, &ty) {
                        errors.report(&expr, err);
                    }
                }
                ir
            } else {
                self.check_expr(errors, scope, &expr, &applied_ty)
            };
            builder.fields(ir.map(|ir| (name, ir)));
        }

        for (field_name, _) in self.context.lookup_struct_name(def.name).fields.values() {
            if !seen.contains(field_name) {
                errors.report(
                    struct_tkn,
                    MissingField {
                        struct_name: def.name,
                        field_name: *field_name,
                    },
                );
            }
        }

        let mut final_subst = Substitution::empty();
        for n in &def.ty_params {
            let solved = subst.apply(fresh_subst.apply(Ty::Var(*n)));
            if matches!(solved, Ty::Var(name) if name.tag == NameTag::Gen) {
                errors.report(struct_tkn, CantInferTypeParam(*n));
                return None;
            }
            final_subst.insert(*n, solved);
        }
        Some((
            Ty::Cons {
                name: def.name,
                ty_args: final_subst,
            },
            builder.build(),
        ))
    }

    // Calls to top-level functions get special handling as we attempt to infer
    // instantiations for their type parameters (if the user omits them at the call-site).
    fn infer_poly_call(
        &self,
        errors: &mut TyErrors,
        scope: &mut Scope,
        def: &FuncDef,
        func_tkn: &SyntaxToken,
        call_expr: &ECall,
        expected: Option<&Ty>,
    ) -> Option<(Ty, Option<ir::ExprData>)> {
        // Need to instantiate type parameters with fresh identifiers,
        // so recursive calls don't mess with us
        let fresh_subst = {
            let mut fresh_subst = Substitution::empty();
            for n in def.ty_params.iter() {
                fresh_subst.insert(*n, Ty::Var(self.name_supply.gen_idx("poly_call")));
            }
            fresh_subst
        };
        let func_ty = fresh_subst.apply_func(def.ty.clone());

        let mut subst = Substitution::empty();
        if let Some(expected) = expected {
            if let Err(err) = match_ty(&mut subst, func_ty.result.clone(), expected) {
                errors.report(call_expr, err);
                return None;
            }
        }
        let args = call_expr.e_arg_list()?.exprs().collect::<Vec<_>>();
        if args.len() != func_ty.arguments.len() {
            errors.report(
                func_tkn,
                ArgCountMismatch(func_ty.arguments.len(), args.len()),
            );
            return None;
        }
        let mut builder = ir::CallBuilder::default();
        for (arg, expected_ty) in args.iter().zip(func_ty.arguments.iter()) {
            let applied_ty = subst.apply(expected_ty.clone());
            let ir = if applied_ty.vars().iter().any(|v| v.tag == NameTag::Gen) {
                let (ty, ir) = self.infer_expr(errors, scope, arg);
                if ty != Ty::Error {
                    if let Err(err) = match_ty(&mut subst, applied_ty, &ty) {
                        errors.report(arg, err);
                    }
                }
                ir
            } else {
                self.check_expr(errors, scope, arg, &applied_ty)
            };
            builder.arguments(ir);
        }
        let mut final_subst = Substitution::empty();
        for n in def.ty_params.iter() {
            let solved = subst.apply(fresh_subst.apply(Ty::Var(*n)));
            if matches!(solved, Ty::Var(name) if name.tag == NameTag::Gen) {
                errors.report(func_tkn, CantInferTypeParam(*n));
                return None;
            }
            final_subst.insert(*n, solved);
        }
        builder.func(Some(ir::Callee::Func {
            name: def.name,
            type_args: final_subst,
        }));
        Some((
            expected
                .cloned()
                .unwrap_or_else(|| subst.apply(func_ty.result)),
            builder.build(),
        ))
    }

    fn lookup_struct_field(
        &self,
        errors: &mut TyErrors,
        struct_name: Name,
        field: &EStructField,
    ) -> Option<(Name, Ty)> {
        let field_tkn = field.ident_token()?;
        let Some((field_name, ty)) = self
            .context
            .lookup_struct_name(struct_name)
            .fields
            .get(field_tkn.text())
            .cloned()
        else {
            errors.report(
                &field_tkn,
                UnknownField {
                    struct_name,
                    field_name: field_tkn.text().to_string(),
                },
            );
            return None;
        };
        self.record_ref(&field_tkn, field_name);
        Some((field_name, ty))
    }

    fn check_struct(
        &self,
        errors: &mut TyErrors,
        scope: &mut Scope,
        struct_expr: &EStruct,
        expected: Option<&Ty>,
    ) -> Option<(Ty, Option<ir::ExprData>)> {
        let struct_name_tkn = struct_expr.upper_ident_token()?;
        let TypeDef::Struct(def) = self.lookup_type(
            errors,
            struct_expr.mod_qualifier(),
            struct_expr.qualifier(),
            &struct_name_tkn,
        )?
        else {
            // TODO: Report struct expr of variant type?
            errors.report(
                &struct_name_tkn,
                UnknownType(struct_name_tkn.text().to_string()),
            );
            return None;
        };
        let mut builder = ir::StructBuilder::default();
        builder.name(Some(def.name));

        let ty_arg_list: Option<Vec<Type>> =
            struct_expr.e_ty_arg_list().map(|t| t.types().collect());
        let subst = if !def.ty_params.is_empty() && ty_arg_list.is_none() {
            match expected {
                None => {
                    return self.infer_poly_struct(
                        errors,
                        scope,
                        def,
                        &struct_name_tkn,
                        struct_expr,
                    )
                }
                Some(expected) => {
                    let Some(subst) = infer_struct_instantiation(def, expected) else {
                        errors.report(
                            struct_expr,
                            TypeMismatch {
                                expected: expected.clone(),
                                actual: Ty::Cons {
                                    name: def.name,
                                    ty_args: Substitution::empty(),
                                },
                            },
                        );
                        return None;
                    };
                    Some(subst.clone())
                }
            }
        } else {
            let ty_arg_list = ty_arg_list.unwrap_or_default();
            if def.ty_params.len() != ty_arg_list.len() {
                errors.report(
                    struct_expr,
                    TyArgCountMismatch(def.ty_params.len(), ty_arg_list.len()),
                );
                return None;
            }
            let ty_arg_names: &[Name] = &def.ty_params;
            let tys: Vec<Ty> = ty_arg_list
                .into_iter()
                .map(|t| self.check_ty(errors, scope, &t))
                .collect();
            Some(Substitution::new(ty_arg_names, &tys))
        };

        let subst = subst.unwrap_or_default();
        // TODO: Can we reduce duplication between this and infer_poly_struct somewhat?
        let mut seen = vec![];
        for field in struct_expr.e_struct_fields() {
            let Some((field_name, ty)) = self.lookup_struct_field(errors, def.name, &field) else {
                // NOTE: no need to report an error here, because `lookup_struct_field` already does
                continue;
            };
            seen.push(field_name);
            if let Some(field_expr) = field.expr() {
                builder.fields(
                    self.check_expr(errors, scope, &field_expr, &subst.apply(ty))
                        .map(|e| (field_name, e)),
                );
            }
        }
        // TODO report all missing fields at once
        for (field_name, _) in self.context.lookup_struct_name(def.name).fields.values() {
            if !seen.contains(field_name) {
                errors.report(
                    &struct_name_tkn,
                    MissingField {
                        struct_name: def.name,
                        field_name: *field_name,
                    },
                );
            }
        }
        Some((
            Ty::Cons {
                name: def.variant.unwrap_or(def.name),
                ty_args: subst,
            },
            builder.build(),
        ))
    }

    fn check_match(
        &self,
        errors: &mut TyErrors,
        scope: &mut Scope,
        match_expr: &EMatch,
        expected: Option<Ty>,
    ) -> (Ty, Option<ir::ExprData>) {
        let mut builder = ir::MatchBuilder::default();
        let Some(scrutinee) = match_expr.scrutinee() else {
            return (Ty::Error, None);
        };

        let (ty_scrutinee, ir_scrutinee) = self.infer_expr(errors, scope, &scrutinee);
        builder.scrutinee(ir_scrutinee);
        let mut ty = expected;
        for branch in match_expr.e_match_branchs() {
            let (Some(pattern), Some(body)) = (branch.pattern(), branch.body()) else {
                continue;
            };
            scope.enter_block();
            let pattern_ir = self.check_pattern(errors, scope, &pattern, &ty_scrutinee);
            let body_ir = match &ty {
                Some(expected) if expected != &Ty::Diverge => {
                    self.check_expr(errors, scope, &body.into(), expected)
                }
                _ => {
                    let (ty_body, ir) = self.infer_expr(errors, scope, &body.into());
                    if ty_body != Ty::Error {
                        ty = Some(ty_body);
                    }
                    ir
                }
            };

            if let (Some(pattern_ir), Some(body_ir)) = (pattern_ir, body_ir) {
                builder.branches(Some(ir::MatchBranch {
                    at: branch.syntax().text_range(),
                    pattern: pattern_ir,
                    body: body_ir,
                }));
            }
            scope.leave_block();
        }
        (ty.unwrap_or(Ty::Error), builder.build())
    }

    // Infers all declarations in the given block, and returns the trailing expression iff it exists
    fn infer_block(
        &self,
        errors: &mut TyErrors,
        scope: &mut Scope,
        block_expr: &EBlock,
    ) -> (Option<Expr>, ir::BlockBuilder) {
        let mut builder = ir::BlockBuilder::default();
        let declarations: Vec<Declaration> = block_expr.declarations().collect();
        let last_expr = if let Some((last, declarations)) = declarations.split_last() {
            for decl in declarations {
                let (_, ir) = self.infer_decl(errors, scope, decl);
                // TODO: if ty_decl is Ty::Diverge here, all following declarations are dead code
                builder.declarations(ir);
            }
            match last {
                Declaration::DExpr(expr) => expr.expr(),
                decl => {
                    let (_, ir) = self.infer_decl(errors, scope, decl);
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
        &self,
        errors: &mut TyErrors,
        scope: &mut Scope,
        expr: &Expr,
        expected: &Ty,
    ) -> Option<ir::Expr> {
        let ir = match (expr, expected) {
            (Expr::EArray(expr), Ty::Array(elem_ty)) => {
                let mut builder = ir::ArrayBuilder::default();
                for elem in expr.exprs() {
                    builder.elems(self.check_expr(errors, scope, &elem, elem_ty));
                }
                builder.build()
            }
            (Expr::EArrayIdx(idx_expr), elem_ty) => {
                let mut builder = ir::ArrayIdxBuilder::default();
                let arr_expr = idx_expr.expr().unwrap();
                builder.array(self.check_expr(
                    errors,
                    scope,
                    &arr_expr,
                    &Ty::Array(Box::new(elem_ty.clone())),
                ));
                if let Some(index) = idx_expr.index() {
                    builder.index(self.check_expr(errors, scope, &index, &Ty::I32));
                }
                builder.build()
            }
            (Expr::EStruct(expr), ty) => {
                let (ty, ir) = self.check_struct(errors, scope, expr, Some(ty))?;
                // A bit weird to duplicate this here
                if *expected != Ty::Error
                    && !matches!(ty, Ty::Error | Ty::Diverge)
                    && ty.ne(expected)
                {
                    errors.report(
                        expr,
                        TypeMismatch {
                            expected: expected.clone(),
                            actual: ty,
                        },
                    );
                }
                ir
            }
            (Expr::EIf(expr), ty) => {
                let mut builder = ir::IfBuilder::default();
                if let Some(condition) = expr.condition() {
                    builder.condition(self.check_expr(errors, scope, &condition, &Ty::Bool));
                }
                if let Some(then_branch) = expr.then_branch() {
                    builder.then_branch(self.check_expr(errors, scope, &then_branch, ty));
                }
                if let Some(else_branch) = expr.else_branch() {
                    builder.else_branch(self.check_expr(errors, scope, &else_branch, ty));
                }
                builder.build()
            }
            (Expr::EParen(expr), _) => expr
                .expr()
                .and_then(|expr| self.check_expr(errors, scope, &expr, expected))
                .map(|ir| *ir.it),
            (Expr::EBlock(block_expr), _) => {
                scope.enter_block();
                let (last_expr, mut builder) = self.infer_block(errors, scope, block_expr);
                if let Some(last_expr) = last_expr {
                    builder.expr(self.check_expr(errors, scope, &last_expr, expected));
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
                scope.leave_block();
                builder.build()
            }
            (Expr::EMatch(match_expr), _) => {
                self.check_match(errors, scope, match_expr, Some(expected.clone()))
                    .1
            }
            (Expr::ECall(expr), _) => {
                let (_, ir) = self.check_call(errors, scope, expr, Some(expected))?;
                ir
            }
            (Expr::ELambda(expr), Ty::Func(func_ty)) => {
                self.check_lambda(errors, scope, expr, func_ty.as_ref())
            }
            _ => {
                let (ty, ir) = self.infer_expr(errors, scope, expr);
                if let Some(ty_err) = expect_ty(expected, &ty) {
                    errors.report(expr, ty_err);
                }
                return ir;
            }
        };
        ir.map(|it| ir::Expr {
            it: Box::new(it),
            at: expr.syntax().text_range(),
            ty: expected.clone(),
        })
    }

    fn check_struct_idx(
        &self,
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
                            .lookup_struct_name(*name)
                            .fields
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
        &self,
        errors: &mut TyErrors,
        scope: &mut Scope,
        decl: &Declaration,
    ) -> (Ty, Option<ir::Declaration>) {
        let (ty, ir) = match decl {
            Declaration::DLet(let_decl) => {
                let mut builder = ir::LetBuilder::default();
                let (ty, ir) = if let Some(expr) = let_decl.expr() {
                    if let Some(ty) = let_decl.ty().map(|ta| self.check_ty(errors, scope, &ta)) {
                        let ir = self.check_expr(errors, scope, &expr, &ty);
                        (ty, ir)
                    } else {
                        self.infer_expr(errors, scope, &expr)
                    }
                } else {
                    (Ty::Error, None)
                };
                builder.expr(ir);

                if let Some(binder_tkn) = let_decl.ident_token() {
                    let (name, sym) = self.name_supply.local_idx(&binder_tkn);
                    builder.binder(Some(name));
                    self.record_def(&binder_tkn, name);
                    scope.add_var(sym, ty, name)
                }
                (Ty::Unit, builder.build())
            }
            Declaration::DSet(set_decl) => {
                let mut builder = ir::SetBuilder::default();
                let set_ty = if let Some(set_target) = set_decl.set_target() {
                    let (ty, ir) = self.infer_set_target(errors, scope, &set_target);
                    builder.set_target(ir);
                    ty
                } else {
                    Ty::Error
                };
                if let Some(expr) = set_decl.expr() {
                    builder.expr(self.check_expr(errors, scope, &expr, &set_ty));
                }
                (Ty::Unit, builder.build())
            }
            Declaration::DWhile(while_decl) => {
                let mut builder = ir::WhileBuilder::default();
                let condition = while_decl.expr();
                if let Some(condition) = condition {
                    builder.condition(self.check_expr(errors, scope, &condition, &Ty::Bool));
                }
                let body = while_decl.e_block();
                if let Some(body) = body {
                    builder.body(self.check_expr(errors, scope, &body.into(), &Ty::Unit));
                }
                (Ty::Unit, builder.build())
            }
            Declaration::DExpr(decl) => {
                let mut builder = ExprBuilder::default();
                let (ty, ir) = self.infer_expr(errors, scope, &decl.expr().unwrap());
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
        &self,
        errors: &mut TyErrors,
        scope: &mut Scope,
        set_target: &SetTarget,
    ) -> (Ty, Option<ir::SetTarget>) {
        let Some(expr) = set_target.set_target_expr() else {
            return (Ty::Error, None);
        };
        let (ty, ir_data) = match expr {
            SetTargetExpr::EVar(var) => {
                let ident_tkn = var.ident_token().unwrap();
                if let Some((ty, name)) = self
                    .context
                    .lookup_var_or_func(scope, self.sym(ident_tkn.text()))
                {
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
                    let (ty, ir) = self.infer_expr(errors, scope, &target);
                    builder.target(ir);
                    ty
                });

                if let Some(index) = arr_idx.index() {
                    builder.index(self.check_expr(errors, scope, &index, &Ty::I32));
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
                let (target_ty, ir) = self.infer_expr(errors, scope, &target_expr);
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
        &self,
        errors: &mut TyErrors,
        scope: &mut Scope,
        pattern: &Pattern,
        expected: &Ty,
    ) -> Option<ir::Pattern> {
        let ir = match pattern {
            Pattern::PatVariant(pat) => {
                let ty_tkn = pat.upper_ident_token()?;
                let TypeDef::Struct(def) =
                    self.lookup_type(errors, pat.mod_qualifier(), pat.qualifier(), &ty_tkn)?
                else {
                    errors.report(&ty_tkn, UnknownType(ty_tkn.text().to_string()));
                    return None;
                };
                match expected {
                    Ty::Cons { name: s, ty_args } if def.name == *s || def.variant == Some(*s) => {
                        let var = pat.ident_token()?;
                        let mut builder = ir::PatVariantBuilder::default();
                        builder.variant(def.variant);
                        builder.alternative(Some(def.name));
                        let (name, sym) = self.name_supply.local_idx(&var);
                        scope.add_var(
                            sym,
                            // TODO Think about this clone
                            Ty::Cons {
                                name: def.name,
                                ty_args: ty_args.clone(),
                            },
                            name,
                        );
                        builder.binder(Some(name));
                        self.record_def(&var, name);
                        builder.build()
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
                let ident_tkn = v.ident_token()?;
                let (name, sym) = self.name_supply.local_idx(&ident_tkn);
                scope.add_var(sym, expected.clone(), name);
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

    fn check_lambda(
        &self,
        errors: &mut TyErrors,
        scope: &mut Scope,
        expr: &ELambda,
        expected: &FuncTy,
    ) -> Option<ir::ExprData> {
        if let Some(return_ty) = expr.return_ty() {
            let t = self.check_ty(errors, scope, &return_ty);
            if let Some(ty_err) = expect_ty(&expected.result, &t) {
                errors.report(&return_ty, ty_err);
            }
        }
        let mut builder = LambdaBuilder::default();
        builder.return_ty(Some(expected.result.clone()));
        scope.enter_block();
        let prev_return_ty = scope.set_return_type(expected.result.clone());
        // TODO: Check for duplicate parameter names
        let params: Vec<Param> = expr.params().collect();
        if params.len() != expected.arguments.len() {
            errors.report(
                // TODO: Make a node for the arg list, so we can report it instead of the whole lambda
                expr,
                ArgCountMismatch(expected.arguments.len(), params.len()),
            );
        }
        let mut param_names = HashSet::new();
        for (param, param_ty) in params.iter().zip(
            expected
                .arguments
                .iter()
                .chain(std::iter::repeat(&Ty::Error)),
        ) {
            let Some(name_tkn) = param.ident_token() else {
                continue;
            };
            let (name, sym) = self.name_supply.local_idx(&name_tkn);
            let ty = if let Some(t) = param.ty() {
                let ty = self.check_ty(errors, scope, &t);
                if let Some(ty_err) = expect_ty(param_ty, &ty) {
                    errors.report(&t, ty_err);
                }
                ty
            } else {
                param_ty.clone()
            };
            builder.params(Some((name, ty.clone())));
            param_names.insert(name);
            scope.add_var(sym, ty.clone(), name);
        }

        if let Some(body) = expr.body() {
            let body_ir = self.check_expr(errors, scope, &body, &expected.result);
            if let Some(body_ir) = body_ir {
                for (n, fvi) in body_ir.free_vars() {
                    if !param_names.contains(&n) {
                        if let Some(assignment) = fvi.is_assigned {
                            errors.report(&assignment, CantReassignCapturedVariable(n));
                        } else {
                            builder.captures(Some((n, fvi.ty.clone())));
                        }
                    }
                }
                builder.body(Some(body_ir));
            }
        }
        scope.leave_block();
        // Restore the previous return type
        scope.restore_return_type(prev_return_ty);
        builder.build()
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

fn infer_struct_instantiation<'a>(def: &StructDef, expected: &'a Ty) -> Option<&'a Substitution> {
    // Infer instantiation
    if let Ty::Cons { name, ty_args } = expected {
        if *name == def.name || def.variant.is_some_and(|v| v == *name) {
            return Some(ty_args);
        }
    }
    None
}

fn expect_ty(expected: &Ty, ty: &Ty) -> Option<TyErrorData> {
    if *expected != Ty::Error && !matches!(ty, Ty::Error | Ty::Diverge) && expected != ty {
        Some(TypeMismatch {
            expected: expected.clone(),
            actual: ty.clone(),
        })
    } else {
        None
    }
}

// `match_ty` is non-commutative unification. Only variables on the left are allowed to be solved, and we limit
// the set of variables that may be solved to `Name::Gen(_)`. This is used to implement inference for type
// parameters to polymorphic functions or struct literals.
fn match_ty(subst: &mut Substitution, definition: Ty, inferred: &Ty) -> Result<(), TyErrorData> {
    // Cow<Ty>?
    let definition = subst.apply(definition);
    match (definition, inferred) {
        (
            Ty::Var(
                n @ Name {
                    tag: NameTag::Gen, ..
                },
            ),
            t,
        ) => {
            // We don't solve for ERROR or DIVERGE, because we're still hoping to
            // solve for a real type.
            if !matches!(t, Ty::Error | Ty::Diverge) && subst.insert(n, t.clone()).is_some() {
                // This is impossible because we applied the substitution before
                panic!("Impossible! match_ty solved the same variable twice.")
            }
            Ok(())
        }
        (Ty::Array(t1), Ty::Array(t2)) => match_ty(subst, *t1, t2),
        (Ty::Func(t1), Ty::Func(t2)) => {
            for (a1, a2) in t1.arguments.into_iter().zip(t2.arguments.iter()) {
                // TODO: do we want the early return here?
                match_ty(subst, a1, a2)?
            }
            match_ty(subst, t1.result, &t2.result)
        }
        (
            Ty::Cons {
                name: n1,
                ty_args: ts1,
            },
            Ty::Cons {
                name: n2,
                ty_args: ts2,
            },
        ) if n1 == *n2 => {
            let ts1_len = ts1.0.len();
            for (k, v) in ts1.0 {
                match_ty(
                    subst,
                    v,
                    ts2.lookup(k)
                        .ok_or_else(|| ArgCountMismatch(ts1_len, ts2.0.len()))?,
                )?
            }
            Ok(())
        }
        (t1, t2) => {
            // TODO: How to deal with `ERROR` here? We don't want to produce follow-up errors
            if t1 == *t2 {
                Ok(())
            } else {
                Err(TypeMismatch {
                    expected: t1.clone(),
                    actual: t2.clone(),
                })
            }
        }
    }
}
