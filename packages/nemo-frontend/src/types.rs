use core::fmt;
use std::collections::HashMap;

use crate::builtins;
use crate::syntax::{
    Declaration, DeclarationData, Expr, ExprData, FuncId, FuncType, FuncTypeData, Id, Intrinsic,
    IntrinsicData, Lit, LitData, Op, OpData, Program, SetTarget, SetTargetData, Span, Spanned,
    Toplevel, ToplevelData, Type, TypeData,
};
use tree_sitter::Node;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Ty {
    I32,
    F32,
    Unit,
    Bool,
    Array(Box<Ty>),
    Struct(String),
}

impl Ty {
    pub fn from_syntax(ty: &Type) -> Ty {
        Ty::from_syntax_data(&ty.it)
    }
    pub fn from_syntax_data(ty: &TypeData) -> Ty {
        match ty {
            TypeData::I32 => Ty::I32,
            TypeData::F32 => Ty::F32,
            TypeData::Unit => Ty::Unit,
            TypeData::Bool => Ty::Bool,
            TypeData::Array(ref t) => Ty::Array(Box::new(Ty::from_syntax(t))),
            TypeData::Struct(ref s) => Ty::Struct(s.clone()),
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::I32 => write!(f, "i32"),
            Ty::F32 => write!(f, "f32"),
            Ty::Bool => write!(f, "bool"),
            Ty::Unit => write!(f, "unit"),
            Ty::Array(t) => write!(f, "[{}]", t),
            Ty::Struct(t) => write!(f, "{}", t),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncTy {
    pub arguments: Vec<Ty>,
    pub result: Ty,
}

impl FuncTy {
    fn from_syntax_data(ty: &FuncTypeData) -> FuncTy {
        let arguments = ty.arguments.iter().map(Ty::from_syntax).collect();
        FuncTy {
            arguments,
            result: Ty::from_syntax(&ty.result),
        }
    }

    pub fn from_syntax(ty: &FuncType) -> FuncTy {
        FuncTy::from_syntax_data(&ty.it)
    }
}

#[derive(Debug)]
pub struct TyError {
    at: Span,
    it: TyErrorData,
}

impl Spanned for TyError {
    fn at(&self) -> &Span {
        &self.at
    }
}

#[derive(Debug)]
pub enum TyErrorData {
    MissingNode(String),
    InvalidLiteral,
    InvalidOperator,
    Message(String),
    UnknownVar(String),
    UnknownFunction(String),
    UnknownType(String),
    UnknownIntrinsic(String, usize),
    NonArrayIdx(Ty),
    NonStructIdx(Ty),
    ArgCountMismatch(usize, usize),
    FieldTypeMismatch {
        struct_name: String,
        field_name: String,
        expected: Ty,
        actual: Ty,
    },
    UnknownField {
        struct_name: String,
        field_name: String,
    },
    MissingField {
        struct_name: String,
        field_name: String,
    },
    TypeMismatch {
        expected: Ty,
        actual: Ty,
    },
}

impl fmt::Display for TyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.at, self.it)
    }
}

impl fmt::Display for TyErrorData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyErrorData::MissingNode(s) => write!(f, "Missing node: '{s}'"),
            TyErrorData::InvalidLiteral => write!(f, "Invalid literal"),
            TyErrorData::InvalidOperator => write!(f, "Invalid operator"),
            TyErrorData::Message(m) => write!(f, "{m}"),
            TyErrorData::UnknownVar(v) => write!(f, "Unknown variable '{v}'"),
            TyErrorData::UnknownFunction(fun) => write!(f, "Unknown function '{fun}'"),
            TyErrorData::UnknownType(t) => write!(f, "Unknown type '{t}'"),
            TyErrorData::UnknownIntrinsic(i, arg_count) =>
              write!(f, "Unknown intrinsic '{i}' with '{arg_count}' arguments"),
            TyErrorData::ArgCountMismatch(expected, actual) =>
                write!(f, "Expected {expected} arguments, but got {actual} instead"),
            TyErrorData::NonArrayIdx(t) => write!(
                f,
                "Tried to access a value of type '{t}', as if it was an array"
            ),
            TyErrorData::NonStructIdx(t) => write!(
                f,
                "Tried to access a value of type '{t}', as if it was a struct"
            ),
            TyErrorData::FieldTypeMismatch { struct_name, field_name, expected, actual } =>
              write!(f, "Expected a value of type '{expected}' for {struct_name}.{field_name}, but got '{actual}' instead"),
            TyErrorData::UnknownField { struct_name, field_name } =>
              write!(f, "Unknown field '{field_name}' for struct '{struct_name}'"),
            TyErrorData::MissingField { struct_name, field_name } =>
              write!(f, "Missing field {struct_name}.{field_name}"),
            TyErrorData::TypeMismatch { expected, actual } =>
                write!(f, "Expected type: '{expected}', but got '{actual}'"),
            }
    }
}

trait TyNode {
    fn child_by_field(&self, field: &str) -> TyResult<Node<'_>>;
}

impl TyNode for Node<'_> {
    fn child_by_field(&self, field: &str) -> TyResult<Node<'_>> {
        self.child_by_field_name(field).ok_or(TyError {
            at: self.into(),
            it: TyErrorData::MissingNode(field.to_string()),
        })
    }
}

fn is_toplevel_kind(n: &Node<'_>) -> bool {
    matches!(
        n.kind(),
        "top_import" | "top_let" | "top_struct" | "top_func"
    )
}

fn is_expr_kind(n: &Node<'_>) -> bool {
    matches!(
        n.kind(),
        "int_lit"
            | "float_lit"
            | "bool_lit"
            | "var_e"
            | "parenthesized_e"
            | "call_e"
            | "binary_e"
            | "block_e"
            | "array_e"
            | "array_idx_e"
            | "struct_e"
            | "struct_idx_e"
            | "if_e"
            | "intrinsic_e"
    )
}

pub type TyResult<T> = Result<T, TyError>;

struct Ctx {
    values: Vec<HashMap<String, Ty>>,
}

impl Ctx {
    fn new() -> Ctx {
        Ctx { values: vec![] }
    }

    fn add(&mut self, v: String, ty: Ty) {
        let _ = self.values.last_mut().unwrap().insert(v, ty);
    }

    fn lookup(&self, v: &str, span: &Span) -> TyResult<&Ty> {
        match self.values.iter().rev().find_map(|scope| scope.get(v)) {
            Some(t) => Ok(t),
            None => Err(TyError {
                at: span.clone(),
                it: TyErrorData::UnknownVar(v.to_string()),
            }),
        }
    }

    fn enter_block(&mut self) {
        self.values.push(HashMap::new())
    }

    fn leave_block(&mut self) {
        self.values.pop().expect("Tried to pop from an empty Ctx");
    }
}

pub struct Typechecker<'a> {
    source: &'a [u8],
    structs: HashMap<String, Vec<(Id, Type)>>,
    functions: HashMap<String, FuncTy>,
}

impl<'a> Typechecker<'a> {
    fn new(source: &'a str) -> Typechecker<'a> {
        Typechecker {
            source: source.as_bytes(),
            structs: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn lookup_function(&self, name: &str, span: &Span) -> TyResult<&FuncTy> {
        match self.functions.get(name) {
            Some(ty) => Ok(ty),
            None => match builtins::lookup_builtin(name) {
                Some(f) => Ok(&f.ty),
                None => Err(TyError {
                    at: span.clone(),
                    it: TyErrorData::UnknownFunction(name.to_string()),
                }),
            },
        }
    }

    fn lookup_struct(&self, name: &str, span: &Span) -> TyResult<&[(Id, Type)]> {
        match self.structs.get(name) {
            Some(fields) => Ok(fields),
            None => Err(TyError {
                at: span.clone(),
                it: TyErrorData::UnknownType(name.to_string()),
            }),
        }
    }

    fn text(&self, node: &Node<'_>) -> &str {
        node.utf8_text(self.source).unwrap()
    }

    fn id(&self, node: &Node<'_>) -> Id {
        Id {
            at: node.into(),
            it: self.text(node).to_string(),
        }
    }

    fn func_id(&self, node: &Node<'_>, ty: FuncTy) -> FuncId {
        FuncId {
            at: node.into(),
            it: self.text(node).to_string(),
            ty,
        }
    }

    fn check_ty(&self, ty: &Type) -> TyResult<Ty> {
        match *ty.it {
            TypeData::Array(ref t) => {
                let inner = self.check_ty(t)?;
                Ok(Ty::Array(Box::new(inner)))
            }
            TypeData::Struct(ref n) => {
                if self.structs.contains_key(n) {
                    Ok(Ty::Struct(n.clone()))
                } else {
                    Err(TyError {
                        at: ty.at.clone(),
                        it: TyErrorData::UnknownType(n.to_string()),
                    })
                }
            }
            TypeData::I32 => Ok(Ty::I32),
            TypeData::F32 => Ok(Ty::F32),
            TypeData::Unit => Ok(Ty::Unit),
            TypeData::Bool => Ok(Ty::Bool),
        }
    }

    fn expect_ty(&self, ty1: &Ty, ty2: &Ty, span: &Span) -> TyResult<()> {
        match (ty1, ty2) {
            (Ty::I32, Ty::I32) => Ok(()),
            (Ty::F32, Ty::F32) => Ok(()),
            (Ty::Unit, Ty::Unit) => Ok(()),
            (Ty::Bool, Ty::Bool) => Ok(()),
            (Ty::Array(t1), Ty::Array(t2)) => self.expect_ty(t1, t2, span),
            (Ty::Struct(s1), Ty::Struct(s2)) if s1 == s2 => Ok(()),
            _ => Err(TyError {
                at: span.clone(),
                it: TyErrorData::TypeMismatch {
                    expected: ty1.clone(),
                    actual: ty2.clone(),
                },
            }),
        }
    }

    fn expect_ty_num(&self, ty: &Ty, span: &Span) -> TyResult<()> {
        match ty {
            Ty::I32 | Ty::F32 => Ok(()),
            _ => Err(TyError {
                at: span.clone(),
                it: TyErrorData::Message(format!("Expected a numeric type, but got {}", ty)),
            }),
        }
    }

    fn expect_ty_struct(
        &'a self,
        ty: &'a Ty,
        span: &'a Span,
    ) -> TyResult<(&'a str, &'a [(Id, Type)])> {
        match ty {
            Ty::Struct(s) => Ok((s, self.lookup_struct(s, span)?)),
            _ => Err(TyError {
                at: span.clone(),
                it: TyErrorData::Message(format!("Expected a struct type, but got {}", ty)),
            }),
        }
    }

    fn expect_ty_array(&self, ty: &'a Ty, span: &Span) -> TyResult<&'a Ty> {
        match ty {
            Ty::Array(t) => Ok(t),
            _ => Err(TyError {
                at: span.clone(),
                it: TyErrorData::Message(format!("Expected an array type, but got {}", ty)),
            }),
        }
    }

    fn declare_struct(&mut self, node: &Node<'_>) -> TyResult<()> {
        assert!(node.kind() == "top_struct");
        let name = node.child_by_field("name")?;
        let mut cursor = node.walk();
        let field_nodes = node
            .children(&mut cursor)
            .filter(|n| n.kind() == "struct_field_top")
            .map(|n| {
                let name = n.child_by_field("name")?;
                let ty_node = n.child_by_field("type")?;
                let ty = self.convert_ty(ty_node)?;
                Ok((self.id(&name), ty))
            })
            .collect::<TyResult<Vec<_>>>()?;
        self.structs
            .insert(self.text(&name).to_string(), field_nodes);
        Ok(())
    }

    fn check_import(&mut self, node: &Node<'_>) -> TyResult<Toplevel> {
        assert!(node.kind() == "top_import");
        let internal_node = node.child_by_field("internal")?;
        let func_ty_node = node.child_by_field("type")?;
        let func_ty = self.convert_func_ty(func_ty_node)?;
        let external_node = node.child_by_field("external")?;

        Ok(Toplevel {
            at: node.into(),
            it: ToplevelData::Import {
                internal: self.func_id(&internal_node, FuncTy::from_syntax(&func_ty)),
                func_ty,
                external: self.id(&external_node),
            },
        })
    }

    fn check_struct(&mut self, node: &Node<'_>) -> TyResult<Toplevel> {
        assert!(node.kind() == "top_struct");
        let name_node = node.child_by_field("name")?;
        let struct_fields = self
            .structs
            .get(self.text(&name_node))
            .expect("Checked a struct that wasn't declared upfront");
        for (_, ty) in struct_fields {
            self.check_ty(ty)?;
        }
        Ok(Toplevel {
            at: node.into(),
            it: ToplevelData::Struct {
                name: self.id(&name_node),
                fields: struct_fields.clone(),
            },
        })
    }

    fn check_top_let(&self, ctx: &mut Ctx, node: &Node<'_>) -> TyResult<(Id, Expr)> {
        assert!(node.kind() == "top_let");
        let binder = node.child_by_field("binder")?;
        // let ty = node.child_by_field("ty")?;
        let expr_node = node.child_by_field("expr")?;
        let typed_expr = self.infer_expr(ctx, &expr_node)?;
        let binder = Id {
            at: binder.into(),
            it: self.text(&binder).to_string(),
        };

        Ok((binder, typed_expr))
    }

    fn check_func(&self, ctx: &mut Ctx, node: &Node<'_>) -> TyResult<Toplevel> {
        assert!(node.kind() == "top_func");

        let name = node.child_by_field("name")?;
        let func_params_node = node.child_by_field("params")?;
        let mut cursor = func_params_node.walk();
        let params = func_params_node
            .children(&mut cursor)
            .filter(|n| n.kind() == "func_param")
            .map(|n| {
                let name_node = n.child_by_field("name")?;
                let ty_node = n.child_by_field("type")?;
                let ty = self.convert_ty(ty_node)?;
                Ok((self.id(&name_node), ty))
            })
            .collect::<TyResult<Vec<_>>>()?;
        let return_ty = match node.child_by_field_name("result") {
            Some(n) => Some(self.convert_ty(n)?),
            None => None,
        };

        ctx.enter_block();
        for (name, ty) in params.iter() {
            ctx.add(name.it.clone(), ty.ty.clone())
        }

        let body_node = node.child_by_field("body")?;
        let body = self.infer_expr(ctx, &body_node)?;
        ctx.leave_block();

        let result = return_ty.as_ref().map(Ty::from_syntax).unwrap_or(Ty::Unit);
        self.expect_ty(
            &result,
            &body.ty,
            &return_ty
                .as_ref()
                .map(|t| t.at.clone())
                .unwrap_or(body_node.into()),
        )?;

        let func_ty = FuncTy {
            arguments: params.iter().map(|(_, ty)| Ty::from_syntax(ty)).collect(),
            result,
        };

        Ok(Toplevel {
            at: node.into(),
            it: ToplevelData::Func {
                name: self.func_id(&name, func_ty),
                params,
                return_ty,
                body,
            },
        })
    }

    fn check_struct_fields(
        &self,
        expected_fields: &Vec<(Id, Type)>,
        actual_fields: &Vec<(Id, Expr)>,
        struct_name: &str,
        span: Span,
    ) -> TyResult<()> {
        for (expected_name, expected_ty) in expected_fields {
            let actual = actual_fields
                .iter()
                .find(|(name, _)| name.it == expected_name.it);
            match actual {
                None => {
                    return Err(TyError {
                        at: span,
                        it: TyErrorData::MissingField {
                            struct_name: struct_name.to_string(),
                            field_name: expected_name.it.clone(),
                        },
                    })
                }
                Some((actual_name, actual_expr)) => {
                    if self
                        .expect_ty(&expected_ty.ty, &actual_expr.ty, &actual_name.at)
                        .is_err()
                    {
                        return Err(TyError {
                            at: actual_name.at.clone(),
                            it: TyErrorData::FieldTypeMismatch {
                                struct_name: struct_name.to_string(),
                                field_name: actual_name.it.to_string(),
                                expected: expected_ty.ty.clone(),
                                actual: actual_expr.ty.clone(),
                            },
                        });
                    }
                }
            }
        }
        for (actual_name, _) in actual_fields {
            let expected = expected_fields
                .iter()
                .find(|(name, _)| name.it == actual_name.it);
            if expected.is_none() {
                return Err(TyError {
                    at: actual_name.at.clone(),
                    it: TyErrorData::UnknownField {
                        struct_name: struct_name.to_string(),
                        field_name: actual_name.it.clone(),
                    },
                });
            }
        }
        Ok(())
    }

    fn declare_func(&mut self, node: &Node<'_>) -> TyResult<()> {
        assert!(node.kind() == "top_func");
        let name = node.child_by_field("name")?;
        let func_params_node = node.child_by_field("params")?;
        let mut cursor = func_params_node.walk();
        let arguments = func_params_node
            .children(&mut cursor)
            .filter(|n| n.kind() == "func_param")
            .map(|n| {
                n.child_by_field("type")
                    .and_then(|n| self.convert_ty(n))
                    .map(|t| Ty::from_syntax(&t))
            })
            .collect::<TyResult<Vec<Ty>>>()?;
        let result = match node.child_by_field_name("result") {
            Some(n) => Ty::from_syntax(&self.convert_ty(n)?),
            None => Ty::Unit,
        };
        let func_ty = FuncTy { arguments, result };
        self.functions.insert(self.text(&name).to_string(), func_ty);
        Ok(())
    }

    fn declare_import(&mut self, node: &Node<'_>) -> TyResult<()> {
        assert!(node.kind() == "top_import");
        let name = node.child_by_field("internal")?;
        let func_ty_node = node.child_by_field("type")?;
        let func_ty = self.convert_func_ty(func_ty_node)?;
        self.functions
            .insert(self.text(&name).to_string(), FuncTy::from_syntax(&func_ty));
        Ok(())
    }

    fn convert_func_ty(&self, node: Node<'_>) -> TyResult<FuncType> {
        assert!(node.kind() == "func_type");
        let mut cursor = node.walk();
        let arg_nodes = node.children_by_field_name("argument", &mut cursor);
        let arguments = arg_nodes
            .map(|n| self.convert_ty(n))
            .collect::<TyResult<Vec<_>>>()?;
        let result_node = node.child_by_field("result")?;
        let result = self.convert_ty(result_node)?;
        let func_type = FuncTypeData { arguments, result };
        Ok(FuncType {
            at: node.into(),
            ty: FuncTy::from_syntax_data(&func_type),
            it: func_type,
        })
    }

    fn convert_ty(&self, node: Node<'_>) -> TyResult<Type> {
        let ty_data = match node.kind() {
            "ty_i32" => TypeData::I32,
            "ty_f32" => TypeData::F32,
            "ty_bool" => TypeData::Bool,
            "ty_unit" => TypeData::Unit,
            "ty_array" => {
                let elem_ty = self.convert_ty(node.child_by_field("elem_ty")?)?;
                TypeData::Array(elem_ty)
            }
            "ty_struct" => TypeData::Struct(self.text(&node).to_string()),
            _ => unreachable!("Unknown type of type"),
        };
        Ok(Type {
            at: node.into(),
            ty: Ty::from_syntax_data(&ty_data),
            it: Box::new(ty_data),
        })
    }

    fn infer_prog(mut self, node: Node<'_>) -> TyResult<Program> {
        assert!(node.kind() == "source_file");
        let mut cursor = node.walk();
        for top_level_node in node
            .children(&mut cursor)
            .filter(is_toplevel_kind)
        {
            match top_level_node.kind() {
                "top_import" => self.declare_import(&top_level_node)?,
                "top_struct" => self.declare_struct(&top_level_node)?,
                "top_func" => self.declare_func(&top_level_node)?,
                "top_let" => {
                    // Globals can't be forward declared as we need to infer their
                    // assigned expression to get their types
                }
                k => unreachable!("Unknown top_level {}", k),
            }
        }

        let mut global_ctx = Ctx::new();
        global_ctx.enter_block();
        let mut toplevels = vec![];
        for top_level_node in node
            .children(&mut cursor)
            .filter(is_toplevel_kind)
        {
            let toplevel = match top_level_node.kind() {
                "top_import" => self.check_import(&top_level_node)?,
                "top_struct" => self.check_struct(&top_level_node)?,
                "top_let" => {
                    // TODO: check top lets before top funcs
                    let (binder, expr) = self.check_top_let(&mut global_ctx, &top_level_node)?;
                    // TODO: parse and check annotation
                    global_ctx.add(binder.it.clone(), expr.ty.clone());
                    let top_data = ToplevelData::Global {
                        binder,
                        annotation: None,
                        init: expr,
                    };
                    Toplevel {
                        it: top_data,
                        at: top_level_node.into(),
                    }
                }
                "top_func" => self.check_func(&mut global_ctx, &top_level_node)?,
                k => unreachable!("Unknown top_level {}", k),
            };
            toplevels.push(toplevel)
        }
        global_ctx.leave_block();
        assert!(global_ctx.values.is_empty());
        Ok(Program { toplevels })
    }

    fn infer_call_args(&self, ctx: &mut Ctx, node: Node<'_>) -> TyResult<Vec<Expr>> {
        assert!(node.kind() == "call_args");
        let mut cursor = node.walk();
        node.children(&mut cursor)
            .filter(is_expr_kind)
            .map(|n| self.infer_expr(ctx, &n))
            .collect()
    }

    fn infer_set_target(&self, ctx: &mut Ctx, node: &Node) -> TyResult<SetTarget> {
        let at: Span = node.into();
        match node.kind() {
            "set_var" => {
                let name_node = node.child_by_field("name")?;
                let name = self.text(&name_node);
                let ty = ctx.lookup(name, &name_node.into())?;
                Ok(SetTarget {
                    at,
                    ty: ty.clone(),
                    it: Box::new(SetTargetData::Var {
                        name: self.id(&name_node),
                    }),
                })
            }

            "set_array_idx" => {
                let target_node = node.child_by_field("target")?;
                let target = self.infer_set_target(ctx, &target_node)?;
                let elem_ty = self.expect_ty_array(&target.ty, &target.at)?;

                let index_node = node.child_by_field("index")?;
                let index = self.infer_expr(ctx, &index_node)?;
                self.expect_ty(&Ty::I32, &index.ty, &index.at)?;
                Ok(SetTarget {
                    at,
                    ty: elem_ty.clone(),
                    it: Box::new(SetTargetData::Array { target, index }),
                })
            }
            "set_struct_idx" => {
                let target_node = node.child_by_field("target")?;
                let target = self.infer_set_target(ctx, &target_node)?;
                let (struct_name, fields) = self.expect_ty_struct(&target.ty, &target.at)?;

                let index_node = node.child_by_field("index")?;
                let index = self.id(&index_node);

                let field_ty = match fields.iter().find(|(name, _)| name.it == index.it) {
                    Some((_, ty)) => Ty::from_syntax(ty),
                    None => {
                        return Err(TyError {
                            at: index_node.into(),
                            it: TyErrorData::UnknownField {
                                struct_name: struct_name.to_string(),
                                field_name: index.it.to_string(),
                            },
                        })
                    }
                };

                Ok(SetTarget {
                    at,
                    ty: field_ty,
                    it: Box::new(SetTargetData::Struct { target, index }),
                })
            }
            k => Err(TyError {
                at,
                it: TyErrorData::Message(format!("Unhandled set target type: {k}")),
            }),
        }
    }

    fn infer_decl(&self, ctx: &mut Ctx, node: &Node<'_>) -> TyResult<Declaration> {
        let at: Span = node.into();
        match node.kind() {
            "let_decl" => {
                let value_node = node.child_by_field("expr")?;
                let typed_expr = self.infer_expr(ctx, &value_node)?;

                let name_node = node.child_by_field("binder")?;
                ctx.add(self.text(&name_node).to_string(), typed_expr.ty.clone());

                Ok(Declaration {
                    ty: Ty::Unit,
                    at,
                    it: DeclarationData::Let {
                        binder: self.id(&name_node),
                        // TODO
                        annotation: None,
                        expr: typed_expr,
                    },
                })
            }
            "set_decl" => {
                let set_target_node = node.child_by_field("target")?;
                let set_target = self.infer_set_target(ctx, &set_target_node)?;

                let expr_node = node.child_by_field("expr")?;
                let expr = self.infer_expr(ctx, &expr_node)?;

                self.expect_ty(&set_target.ty, &expr.ty, &expr.at)?;
                Ok(Declaration {
                    ty: Ty::Unit,
                    at,
                    it: DeclarationData::Set { set_target, expr },
                })
            }
            "expr_decl" => {
                let expr_node = node.child_by_field("expr")?;
                let expr = self.infer_expr(ctx, &expr_node)?;
                Ok(Declaration {
                    ty: expr.ty.clone(),
                    at,
                    it: DeclarationData::Expr(expr),
                })
            }
            "while_decl" => {
                let condition_node = node.child_by_field("condition")?;
                let condition = self.infer_expr(ctx, &condition_node)?;
                self.expect_ty(&Ty::Bool, &condition.ty, &condition.at)?;

                let body_node = node.child_by_field("body")?;
                let body = self.infer_expr(ctx, &body_node)?;
                self.expect_ty(&Ty::Unit, &body.ty, &body.at)?;

                Ok(Declaration {
                    ty: Ty::Unit,
                    at,
                    it: DeclarationData::While { condition, body },
                })
            }

            k => Err(TyError {
                at,
                it: TyErrorData::Message(format!("Unhandled decl type: {k}")),
            }),
        }
    }

    fn infer_expr(&self, ctx: &mut Ctx, node: &Node<'_>) -> TyResult<Expr> {
        let at: Span = node.into();
        match node.kind() {
            "parenthesized_e" => self.infer_expr(ctx, &node.child_by_field("expr")?),
            "int_lit" => {
                let lit = LitData::I32(self.text(node).parse().map_err(|_| TyError {
                    at: node.into(),
                    it: TyErrorData::InvalidLiteral,
                })?);
                let ty = Ty::I32;
                Ok(Expr {
                    ty: ty.clone(),
                    at: at.clone(),
                    it: Box::new(ExprData::Lit(Lit { at, ty, it: lit })),
                })
            }
            "float_lit" => {
                let lit = LitData::F32(self.text(node).parse().map_err(|_| TyError {
                    at: node.into(),
                    it: TyErrorData::InvalidLiteral,
                })?);
                let ty = Ty::F32;
                Ok(Expr {
                    ty: ty.clone(),
                    at: at.clone(),
                    it: Box::new(ExprData::Lit(Lit { at, ty, it: lit })),
                })
            }
            "bool_lit" => {
                let lit = LitData::Bool(self.text(node).parse().map_err(|_| TyError {
                    at: node.into(),
                    it: TyErrorData::InvalidLiteral,
                })?);
                let ty = Ty::Bool;
                Ok(Expr {
                    ty: ty.clone(),
                    at: at.clone(),
                    it: Box::new(ExprData::Lit(Lit { at, ty, it: lit })),
                })
            }
            "var_e" => {
                let name_node = node.child(0).expect("Expected a name in a var_e node");
                let name = self.id(&name_node);
                let ty = ctx.lookup(&name.it, &name_node.into())?;
                Ok(Expr {
                    ty: ty.clone(),
                    at,
                    it: Box::new(ExprData::Var(name)),
                })
            }
            "binary_e" => {
                let left_node = node.child_by_field("left")?;
                let left = self.infer_expr(ctx, &left_node)?;

                let right_node = node.child_by_field("right")?;
                let right = self.infer_expr(ctx, &right_node)?;

                let op_node = node.child_by_field("op")?;
                let op_data: OpData = self
                    .text(&op_node)
                    .parse()
                    .expect("failed to parse operator");

                let ty = match op_data {
                    OpData::Add | OpData::Sub | OpData::Mul | OpData::Div => {
                        self.expect_ty_num(&left.ty, &left.at)?;
                        self.expect_ty(&left.ty, &right.ty, &right.at)?;
                        left.ty.clone()
                    }
                    OpData::Lt | OpData::Le | OpData::Gt | OpData::Ge => {
                        self.expect_ty_num(&left.ty, &left.at)?;
                        self.expect_ty(&left.ty, &right.ty, &right.at)?;
                        Ty::Bool
                    }
                    OpData::And | OpData::Or => {
                        self.expect_ty(&Ty::Bool, &left.ty, &left.at)?;
                        self.expect_ty(&Ty::Bool, &right.ty, &right.at)?;
                        Ty::Bool
                    }
                    OpData::Eq | OpData::Ne => {
                        self.expect_ty_num(&left.ty, &left.at)?;
                        self.expect_ty(&left.ty, &right.ty, &right.at)?;
                        Ty::Bool
                    }
                };

                Ok(Expr {
                    ty,
                    at,
                    it: Box::new(ExprData::Binary {
                        op: Op {
                            it: op_data,
                            at: op_node.into(),
                        },
                        left,
                        right,
                    }),
                })
            }
            "call_e" => {
                let call_args_node = node.child_by_field("arguments")?;
                let call_args = self.infer_call_args(ctx, call_args_node)?;

                let function_node = node.child_by_field("function")?;
                let func_ty =
                    self.lookup_function(self.text(&function_node), &function_node.into())?;

                if func_ty.arguments.len() != call_args.len() {
                    return Err(TyError {
                        it: TyErrorData::ArgCountMismatch(func_ty.arguments.len(), call_args.len()),
                        at: call_args_node.into(),
                    });
                }
                for (expected, actual) in func_ty.arguments.iter().zip(call_args.iter()) {
                    self.expect_ty(expected, &actual.ty, &actual.at)?
                }

                Ok(Expr {
                    ty: func_ty.result.clone(),
                    at,
                    it: Box::new(ExprData::Call {
                        func: self.func_id(&function_node, func_ty.clone()),
                        arguments: call_args,
                    }),
                })
            }
            "array_e" => {
                let mut cursor = node.walk();
                let elems = node
                    .children(&mut cursor)
                    .filter(is_expr_kind)
                    .map(|n| self.infer_expr(ctx, &n))
                    .collect::<TyResult<Vec<_>>>()?;
                let ty_elem = match elems.get(0) {
                    Some(head) => {
                        for elem in elems.iter() {
                            self.expect_ty(&head.ty, &elem.ty, &elem.at)?
                        }
                        head.ty.clone()
                    }
                    None => Ty::Unit,
                };
                Ok(Expr {
                    ty: Ty::Array(Box::new(ty_elem)),
                    at,
                    it: Box::new(ExprData::Array(elems)),
                })
            }
            "array_idx_e" => {
                let array_node = node.child_by_field("array")?;
                let array = self.infer_expr(ctx, &array_node)?;
                let index_node = node.child_by_field("index")?;
                let index = self.infer_expr(ctx, &index_node)?;
                let ty_elem = match array.ty {
                    Ty::Array(ref t) => *t.clone(),
                    t => {
                        return Err(TyError {
                            at,
                            it: TyErrorData::NonArrayIdx(t),
                        })
                    }
                };
                self.expect_ty(&Ty::I32, &index.ty, &index.at)?;
                Ok(Expr {
                    ty: ty_elem,
                    at,
                    it: Box::new(ExprData::ArrayIdx { array, index }),
                })
            }
            "if_e" => {
                let condition_node = node.child_by_field("condition")?;
                let condition = self.infer_expr(ctx, &condition_node)?;
                let then_node = node.child_by_field("then")?;
                let then_branch = self.infer_expr(ctx, &then_node)?;
                let else_node = node.child_by_field("else")?;
                let else_branch = self.infer_expr(ctx, &else_node)?;

                self.expect_ty(&Ty::Bool, &condition.ty, &condition.at)?;
                self.expect_ty(&then_branch.ty, &else_branch.ty, &then_branch.at)?;

                Ok(Expr {
                    ty: then_branch.ty.clone(),
                    at,
                    it: Box::new(ExprData::If {
                        condition,
                        then_branch,
                        else_branch,
                    }),
                })
            }
            "block_e" => {
                let mut cursor = node.walk();
                let mut declarations = vec![];
                for decl_node in node.children_by_field_name("block_decl", &mut cursor) {
                    let declaration = self.infer_decl(ctx, &decl_node)?;
                    declarations.push(declaration)
                }
                let ty = declarations
                    .last()
                    .map(|d| d.ty.clone())
                    .unwrap_or(Ty::Unit);
                Ok(Expr {
                    ty,
                    at,
                    it: Box::new(ExprData::Block { declarations }),
                })
            }
            "struct_e" => {
                let struct_name_node = node.child_by_field("struct")?;
                let struct_name = self.text(&struct_name_node);
                let expected_fields = match self.structs.get(struct_name) {
                    None => {
                        return Err(TyError {
                            at: struct_name_node.into(),
                            it: TyErrorData::UnknownType(struct_name.to_string()),
                        })
                    }
                    Some(s) => s,
                };
                let mut cursor = node.walk();
                let field_nodes = node
                    .children(&mut cursor)
                    .filter(|n| n.kind() == "struct_field_e")
                    .collect::<Vec<_>>();

                let mut actual_fields = vec![];
                for field_node in field_nodes {
                    let name_node = field_node.child_by_field("name")?;
                    let expr_node = field_node.child_by_field("expr")?;
                    let expr = self.infer_expr(ctx, &expr_node)?;
                    let field = (self.id(&name_node), expr);
                    actual_fields.push(field)
                }
                self.check_struct_fields(
                    expected_fields,
                    &actual_fields,
                    struct_name,
                    node.into(),
                )?;

                Ok(Expr {
                    ty: Ty::Struct(struct_name.to_string()),
                    at,
                    it: Box::new(ExprData::Struct {
                        name: self.id(&struct_name_node),
                        fields: actual_fields,
                    }),
                })
            }
            "struct_idx_e" => {
                let expr_node = node.child_by_field("expr")?;
                let expr = self.infer_expr(ctx, &expr_node)?;
                let index_node = node.child_by_field("index")?;
                let index = self.text(&index_node);

                let (fields, struct_name) = match expr.ty {
                    Ty::Struct(ref s) => match self.structs.get(s) {
                        None => unreachable!("Inferred an unknown struct type '{s}'"),
                        Some(fs) => (fs, s),
                    },
                    t => {
                        return Err(TyError {
                            at: expr.at,
                            it: TyErrorData::NonStructIdx(t),
                        })
                    }
                };

                let ty = match fields.iter().find(|(name, _)| name.it == index) {
                    Some((_, ty)) => ty.ty.clone(),
                    None => {
                        return Err(TyError {
                            at: index_node.into(),
                            it: TyErrorData::UnknownField {
                                struct_name: struct_name.to_string(),
                                field_name: index.to_string(),
                            },
                        })
                    }
                };

                Ok(Expr {
                    ty,
                    at,
                    it: Box::new(ExprData::StructIdx {
                        expr,
                        index: self.id(&index_node),
                    }),
                })
            }
            "intrinsic_e" => {
                let function_node = node.child_by_field("function")?;
                let function = self.text(&function_node);

                let call_args_node = node.child_by_field("arguments")?;
                let call_args = self.infer_call_args(ctx, call_args_node)?;

                match (function, call_args.len()) {
                    ("@array_len", 1) => {
                        self.expect_ty_array(&call_args[0].ty, &call_args[0].at)?;
                        Ok(Expr {
                            ty: Ty::I32,
                            at,
                            it: Box::new(ExprData::Intrinsic {
                                intrinsic: Intrinsic {
                                    at: function_node.into(),
                                    it: IntrinsicData::ArrayLen,
                                },
                                arguments: call_args,
                            }),
                        })
                    }
                    ("@array_new", 2) => {
                        self.expect_ty(&Ty::I32, &call_args[1].ty, &call_args[1].at)?;
                        Ok(Expr {
                            ty: Ty::Array(Box::new(call_args[0].ty.clone())),
                            at,
                            it: Box::new(ExprData::Intrinsic {
                                intrinsic: Intrinsic {
                                    at: function_node.into(),
                                    it: IntrinsicData::ArrayNew,
                                },
                                arguments: call_args,
                            }),
                        })
                    }
                    (f, arg_count) => Err(TyError {
                        it: TyErrorData::UnknownIntrinsic(f.to_string(), arg_count),
                        at,
                    }),
                }
            }
            k => unreachable!("Unknown expr type: {}", k),
        }
    }
}

pub fn typecheck(source: &str, program: Node<'_>) -> TyResult<Program> {
    let typechecker = Typechecker::new(source);
    typechecker.infer_prog(program)
}
