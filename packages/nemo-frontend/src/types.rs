use core::fmt;
use std::collections::HashMap;

use crate::syntax::{
    Declaration, Expr, FuncParam, FuncTy, Lit, Op, Program, SetTarget, Span, Spanned, StructField,
    StructFieldE, Toplevel, Ty, Typed, TypedDeclaration, TypedExpr,
};
use tree_sitter::Node;

#[derive(Debug)]
pub enum TyError {
    MissingNode(String),
    InvalidLiteral,
    InvalidOperator,
    Message(String),
    UnknownVar(String),
    UnknownFunction(String),
    UnknownType(String),
    NonArrayIdx(Ty),
    FieldTypeMismatch {
        struct_name: String,
        field_name: String,
        expected: Ty,
        actual: Ty,
    },
    ExtraField {
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
        match self {
            TyError::MissingNode(s) => write!(f, "Missing node: '{s}'"),
            TyError::InvalidLiteral => write!(f, "Invalid literal"),
            TyError::InvalidOperator => write!(f, "Invalid operator"),
            TyError::Message(m) => write!(f, "{m}"),
            TyError::UnknownVar(v) => write!(f, "Unknown variable '{v}'"),
            TyError::UnknownFunction(fun) => write!(f, "Unknown function '{fun}'"),
            TyError::UnknownType(t) => write!(f, "Unknown type '{t}'"),
            TyError::NonArrayIdx(t) => write!(
                f,
                "Tried to access a value of type '{t}', as if it was an array"
            ),
            TyError::FieldTypeMismatch { struct_name, field_name, expected, actual } =>
              write!(f, "Expected a value of type '{expected}' for {struct_name}.{field_name}, but got '{actual}' instead"),
            TyError::ExtraField { struct_name, field_name } =>
              write!(f, "Unknown field '{field_name}' for struct '{struct_name}'"),
            TyError::MissingField { struct_name, field_name } =>
              write!(f, "Missing field {struct_name}.{field_name}"),
            TyError::TypeMismatch { expected, actual } =>
                write!(f, "Expected type: '{expected}', but got '{actual}'"),
            }
    }
}

trait TyNode {
    fn child_by_field(&self, field: &str) -> TyResult<Node<'_>>;
}

impl TyNode for Node<'_> {
    fn child_by_field(&self, field: &str) -> TyResult<Node<'_>> {
        self.child_by_field_name(field).ok_or(Spanned::from(
            self.into(),
            TyError::MissingNode(field.to_string()),
        ))
    }
}

fn is_expr_kind(s: &str) -> bool {
    matches!(
        s,
        "int_lit"
            | "float_lit"
            | "bool_lit"
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

type TyResult<T> = Result<T, Spanned<TyError>>;

struct Ctx {
    values: Vec<HashMap<String, Ty>>,
}

impl Ctx {
    pub fn new() -> Ctx {
        Ctx { values: vec![] }
    }

    pub fn add(&mut self, v: String, ty: Ty) {
        let _ = self.values.last_mut().unwrap().insert(v, ty);
    }

    pub fn lookup(&self, v: &str, span: &Span) -> TyResult<&Ty> {
        match self.values.iter().rev().find_map(|scope| scope.get(v)) {
            Some(t) => Ok(t),
            None => Err(Spanned::from(
                span.clone(),
                TyError::UnknownVar(v.to_string()),
            )),
        }
    }

    pub fn enter_block(&mut self) {
        self.values.push(HashMap::new())
    }

    pub fn leave_block(&mut self) {
        self.values.pop().expect("Tried to pop from an empty Ctx");
    }
}

pub struct Typechecker<'a> {
    source: &'a [u8],
    structs: HashMap<String, Vec<StructField>>,
    functions: HashMap<String, (Span, FuncTy)>,
}

impl<'a> Typechecker<'a> {
    pub fn new(source: &'a str) -> Typechecker<'a> {
        Typechecker {
            source: source.as_bytes(),
            structs: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn text(&self, node: &Node<'_>) -> &str {
        node.utf8_text(self.source).unwrap()
    }

    fn spanned_text(&self, node: &Node<'_>) -> Spanned<String> {
        Spanned::from(node.into(), self.text(node).to_string())
    }

    fn check_ty(&self, ty: &Ty, span: &Span) -> TyResult<()> {
        match ty {
            Ty::Array(t) => self.check_ty(&t, span),
            Ty::Struct(n) => {
                if self.structs.contains_key(n) {
                    Ok(())
                } else {
                    Err(Spanned::from(
                        span.clone(),
                        TyError::UnknownType(n.to_string()),
                    ))
                }
            }
            _ => Ok(()),
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
            _ => Err(Spanned::from(
                span.clone(),
                TyError::TypeMismatch {
                    expected: ty1.clone(),
                    actual: ty2.clone(),
                },
            )),
        }
    }

    fn expect_ty_num(&self, ty: &Ty, span: &Span) -> TyResult<()> {
        match ty {
            Ty::I32 | Ty::F32 => Ok(()),
            _ => Err(Spanned::from(
                span.clone(),
                TyError::Message(format!("Expected a numeric type, but got {}", ty)),
            )),
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
                Ok(StructField {
                    name: self.spanned_text(&name),
                    ty,
                })
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

        Ok(Toplevel::TopImport {
            internal: self.spanned_text(&internal_node),
            func_ty,
            external: self.spanned_text(&external_node),
        })
    }

    fn check_struct(&mut self, node: &Node<'_>) -> TyResult<Toplevel> {
        assert!(node.kind() == "top_struct");
        let name_node = node.child_by_field("name")?;
        let struct_fields = self
            .structs
            .get(self.text(&name_node))
            .expect("Checked a struct that wasn't declared upfront");
        for ele in struct_fields {
            self.check_ty(&ele.ty.it, &ele.ty.at)?;
        }
        Ok(Toplevel::TopStruct {
            name: self.spanned_text(&name_node),
            fields: struct_fields.clone(),
        })
    }

    fn check_top_let(
        &self,
        ctx: &mut Ctx,
        node: &Node<'_>,
    ) -> TyResult<(Typed<String>, TypedExpr)> {
        assert!(node.kind() == "top_let");
        let binder = node.child_by_field("binder")?;
        // let ty = node.child_by_field("ty")?;
        let expr_node = node.child_by_field("expr")?;
        let typed_expr = self.infer_expr(ctx, &expr_node)?;
        let typed_binder = Typed {
            ty: typed_expr.ty.clone(),
            at: binder.into(),
            it: self.text(&binder).to_string(),
        };

        Ok((typed_binder, typed_expr))
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
                Ok(FuncParam {
                    name: self.spanned_text(&name_node),
                    ty,
                })
            })
            .collect::<TyResult<Vec<FuncParam>>>()?;
        let return_ty = match node.child_by_field_name("result") {
            Some(n) => Some(self.convert_ty(n)?),
            None => None,
        };

        ctx.enter_block();
        for param in params.iter() {
            ctx.add(param.name.it.clone(), param.ty.it.clone())
        }

        let body_node = node.child_by_field("body")?;
        let body = self.infer_expr(ctx, &body_node)?;
        ctx.leave_block();
        Ok(Toplevel::TopFunc {
            name: self.spanned_text(&name),
            params,
            return_ty,
            body,
        })
    }

    fn check_struct_fields(
        &self,
        expected_fields: &Vec<StructField>,
        actual_fields: &Vec<StructFieldE>,
        struct_name: &str,
        span: Span,
    ) -> TyResult<()> {
        for expected in expected_fields {
            let actual = actual_fields.iter().find(|f| f.name.it == expected.name.it);
            match actual {
                None => {
                    return Err(Spanned::from(
                        span.clone(),
                        TyError::MissingField {
                            struct_name: struct_name.to_string(),
                            field_name: expected.name.it.clone(),
                        },
                    ))
                }
                Some(actual) => {
                    if self
                        .expect_ty(&expected.ty.it, &actual.expr.ty, &actual.expr.at)
                        .is_err()
                    {
                        return Err(Spanned {
                            at: actual.expr.at.clone(),
                            it: TyError::FieldTypeMismatch {
                                struct_name: struct_name.to_string(),
                                field_name: actual.name.it.to_string(),
                                expected: expected.ty.it.clone(),
                                actual: actual.expr.ty.clone(),
                            },
                        });
                    }
                }
            }
        }
        for actual in actual_fields {
            let expected = expected_fields.iter().find(|f| f.name.it == actual.name.it);
            if expected.is_none() {
                return Err(Spanned {
                    at: actual.name.at.clone(),
                    it: TyError::ExtraField {
                        struct_name: struct_name.to_string(),
                        field_name: actual.name.it.clone(),
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
                    .map(|t| t.it)
            })
            .collect::<TyResult<Vec<Ty>>>()?;
        let result = match node.child_by_field_name("result") {
            Some(n) => self.convert_ty(n)?.it,
            None => Ty::Unit,
        };
        let func_ty = FuncTy { arguments, result };
        self.functions
            .insert(self.text(&name).to_string(), (name.into(), func_ty));
        Ok(())
    }

    fn declare_import(&mut self, node: &Node<'_>) -> TyResult<()> {
        assert!(node.kind() == "top_import");
        let name = node.child_by_field("internal")?;
        let func_ty_node = node.child_by_field("type")?;
        let func_ty = self.convert_func_ty(func_ty_node)?;
        self.functions
            .insert(self.text(&name).to_string(), (func_ty.at, func_ty.it));
        Ok(())
    }

    fn convert_func_ty(&self, node: Node<'_>) -> TyResult<Spanned<FuncTy>> {
        assert!(node.kind() == "func_type");
        let mut cursor = node.walk();
        let arg_nodes = node.children_by_field_name("argument", &mut cursor);
        let arguments = arg_nodes
            .map(|n| self.convert_ty(n).map(|t| t.it))
            .collect::<TyResult<Vec<_>>>()?;
        let result_node = node.child_by_field("result")?;
        let result = self.convert_ty(result_node)?;
        Ok(Spanned::from(
            node.into(),
            FuncTy {
                arguments,
                result: result.it,
            },
        ))
    }

    fn convert_ty(&self, node: Node<'_>) -> TyResult<Spanned<Ty>> {
        let ty = match node.kind() {
            "ty_i32" => Ty::I32,
            "ty_f32" => Ty::F32,
            "ty_bool" => Ty::Bool,
            "ty_unit" => Ty::Unit,
            "ty_array" => {
                let elem_ty = self.convert_ty(node.child_by_field("elem_ty")?)?;
                Ty::Array(Box::new(elem_ty.it))
            }
            "ty_struct" => Ty::Struct(self.text(&node).to_string()),
            _ => unreachable!("Unknown type of type"),
        };
        Ok(Spanned::from(node.into(), ty))
    }

    pub fn infer_prog(mut self, node: Node<'_>) -> TyResult<Program> {
        assert!(node.kind() == "source_file");
        let mut cursor = node.walk();
        for top_level_node in node.children(&mut cursor) {
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
        for top_level_node in node.children(&mut cursor) {
            let toplevel = match top_level_node.kind() {
                "top_import" => self.check_import(&top_level_node)?,
                "top_struct" => self.check_struct(&top_level_node)?,
                "top_let" => {
                    // TODO: check top lets before top funcs
                    let (binder, expr) = self.check_top_let(&mut global_ctx, &top_level_node)?;
                    global_ctx.add(binder.it.clone(), binder.ty.clone());
                    Toplevel::TopLet { binder, expr }
                }
                "top_func" => self.check_func(&mut global_ctx, &top_level_node)?,
                k => unreachable!("Unknown top_level {}", k),
            };
            toplevels.push(toplevel)
        }
        global_ctx.leave_block();
        assert!(global_ctx.values.is_empty());
        return Ok(Program { toplevels });
    }

    fn infer_call_args(&self, ctx: &mut Ctx, node: Node<'_>) -> TyResult<Vec<TypedExpr>> {
        assert!(node.kind() == "call_args");
        let mut cursor = node.walk();
        node.children(&mut cursor)
            .filter(|n| is_expr_kind(n.kind()))
            .map(|n| self.infer_expr(ctx, &n))
            .collect()
    }

    fn infer_set_target(&self, ctx: &mut Ctx, node: &Node) -> TyResult<Typed<SetTarget>> {
        let at: Span = node.into();
        match node.kind() {
            "set_var" => {
                let name_node = node.child_by_field("name")?;
                let name = self.text(&name_node);
                let ty = ctx.lookup(&name, &name_node.into())?;
                let it = SetTarget::Var {
                    name: Typed {
                        ty: ty.clone(),
                        at: at.clone(),
                        it: name.to_string(),
                    },
                };
                Ok(Typed {
                    ty: ty.clone(),
                    at,
                    it,
                })
            }

            "set_array_idx" => {
                let array_node = node.child_by_field("array")?;
                let array = self.text(&array_node);

                let (array_ty, elem_ty) = {
                    let array_ty = ctx.lookup(array, &array_node.into())?;
                    let elem_ty = if let Ty::Array(ref elem_ty) = array_ty {
                        *elem_ty.clone()
                    } else {
                        return Err(Spanned::from(
                            array_node.into(),
                            TyError::NonArrayIdx(array_ty.clone()),
                        ));
                    };
                    (array_ty.clone(), elem_ty)
                };

                let index_node = node.child_by_field("index")?;
                let index = self.infer_expr(ctx, &index_node)?;
                self.expect_ty(&Ty::I32, &index.ty, &index.at)?;
                let it = SetTarget::Array {
                    name: Typed {
                        ty: array_ty.clone(),
                        at: array_node.into(),
                        it: array.to_string(),
                    },
                    index,
                };
                Ok(Typed {
                    ty: elem_ty,
                    at,
                    it,
                })
            }

            //   "set_struct_idx" => {
            //     let struct_node = target_node.child_by_field("struct")?;
            //     let struct_ty = self.infer_expr(ctx, &struct_node)?.ty;

            //     if let Ty::Struct(fields) = struct_ty {
            //       let field_name = self.text(&target_node.child_by_field("field")?);
            //       fields.get(&field_name)
            //         .cloned()
            //         .ok_or(Spanned::from(struct_node.start_position(), TyError::MissingStructField(field_name)))
            //     } else {
            //       Err(Spanned::from(struct_node.start_position(), TyError::InvalidIndexTarget(struct_ty)))
            //     }
            //   },
            _ => unimplemented!(),
        }
    }

    fn infer_decl(&self, ctx: &mut Ctx, node: &Node<'_>) -> TyResult<TypedDeclaration> {
        let at: Span = node.into();
        match node.kind() {
            "let_decl" => {
                let value_node = node.child_by_field("expr")?;
                let typed_expr = self.infer_expr(ctx, &value_node)?;

                let name_node = node.child_by_field("binder")?;
                ctx.add(self.text(&name_node).to_string(), typed_expr.ty.clone());

                Ok(Typed {
                    ty: Ty::Unit,
                    at,
                    it: Declaration::Let {
                        binder: self.spanned_text(&name_node),
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
                Ok(Typed {
                    ty: Ty::Unit,
                    at,
                    it: Declaration::Set { set_target, expr },
                })
            }
            "expr_decl" => {
                let expr_node = node.child_by_field("expr")?;
                let expr = self.infer_expr(ctx, &expr_node)?;
                Ok(Typed {
                    ty: expr.ty.clone(),
                    at,
                    it: Declaration::Expr(expr),
                })
            }
            "while_decl" => {
                let condition_node = node.child_by_field("cond")?;
                let condition = self.infer_expr(ctx, &condition_node)?;
                self.expect_ty(&Ty::Bool, &condition.ty, &condition.at)?;

                let body_node = node.child_by_field("body")?;
                let body = self.infer_expr(ctx, &body_node)?;
                self.expect_ty(&Ty::Unit, &body.ty, &body.at)?;

                Ok(Typed {
                    ty: Ty::Unit,
                    at,
                    it: Declaration::While { condition, body },
                })
            }

            k => Err(Spanned::from(
                at,
                TyError::Message(format!("Unhandled decl type: {k}")),
            )),
        }
    }

    fn infer_expr(&self, ctx: &mut Ctx, node: &Node<'_>) -> TyResult<TypedExpr> {
        let at: Span = node.into();
        match node.kind() {
            "int_lit" => {
                let lit = Lit::I32(
                    self.text(node)
                        .parse()
                        .map_err(|_| Spanned::from(node.into(), TyError::InvalidLiteral))?,
                );
                Ok(Typed {
                    ty: Ty::I32,
                    at,
                    it: Expr::Lit(lit),
                })
            }
            "float_lit" => {
                let lit = Lit::F32(
                    self.text(node)
                        .parse()
                        .map_err(|_| Spanned::from(node.into(), TyError::InvalidLiteral))?,
                );
                Ok(Typed {
                    ty: Ty::F32,
                    at,
                    it: Expr::Lit(lit),
                })
            }
            "bool_lit" => {
                let lit = Lit::Bool(
                    self.text(node)
                        .parse()
                        .map_err(|_| Spanned::from(node.into(), TyError::InvalidLiteral))?,
                );
                Ok(Typed {
                    ty: Ty::Bool,
                    at,
                    it: Expr::Lit(lit),
                })
            }
            "var_e" => {
                let name_node = node.child(0).expect("Expected a name in a var_e node");
                let name = self.text(&name_node);
                let ty = ctx.lookup(name, &name_node.into())?;
                Ok(Typed {
                    ty: ty.clone(),
                    at,
                    it: Expr::Var(name.to_string()),
                })
            }
            "binary_e" => {
                let left_node = node.child_by_field("left")?;
                let left = self.infer_expr(ctx, &left_node)?;

                let right_node = node.child_by_field("right")?;
                let right = self.infer_expr(ctx, &right_node)?;

                let op_node = node.child_by_field("op")?;
                let op: Op = self
                    .text(&op_node)
                    .parse()
                    .expect("failed to parse operator");

                let ty = match op {
                    Op::Add | Op::Sub | Op::Mul | Op::Div => {
                        self.expect_ty_num(&left.ty, &left.at)?;
                        self.expect_ty(&left.ty, &right.ty, &right.at)?;
                        left.ty.clone()
                    }
                    Op::Lt | Op::Le | Op::Gt | Op::Ge => {
                        self.expect_ty_num(&left.ty, &left.at)?;
                        self.expect_ty(&left.ty, &right.ty, &right.at)?;
                        Ty::Bool
                    }
                    Op::And | Op::Or => {
                        self.expect_ty(&Ty::Bool, &left.ty, &left.at)?;
                        self.expect_ty(&Ty::Bool, &right.ty, &right.at)?;
                        Ty::Bool
                    }
                    Op::Eq | Op::Ne => {
                        self.expect_ty_num(&left.ty, &left.at)?;
                        self.expect_ty(&left.ty, &right.ty, &right.at)?;
                        Ty::Bool
                    }
                };

                Ok(Typed {
                    ty,
                    at,
                    it: Expr::Binary {
                        op: Spanned::from(op_node.into(), op),
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                })
            }
            "call_e" => {
                let call_args_node = node.child_by_field("arguments")?;
                let call_args = self.infer_call_args(ctx, call_args_node)?;

                let function_node = node.child_by_field("function")?;
                let func_ty = match self.functions.get(self.text(&function_node)) {
                    Some((_, t)) => (*t).clone(),
                    None => {
                        return Err(Spanned {
                            it: TyError::UnknownFunction(self.text(&function_node).to_string()),
                            at: function_node.into(),
                        })
                    }
                };

                Ok(Typed {
                    ty: func_ty.result.clone(),
                    at,
                    it: Expr::Call {
                        func: self.spanned_text(&function_node),
                        func_ty,
                        arguments: call_args,
                    },
                })
            }
            "array_e" => {
                let mut cursor = node.walk();
                let elems = node
                    .children(&mut cursor)
                    .filter(|n| is_expr_kind(n.kind()))
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
                Ok(Typed {
                    ty: Ty::Array(Box::new(ty_elem)),
                    at,
                    it: Expr::Array(elems),
                })
            }
            "array_idx_e" => {
                let array_node = node.child_by_field("array")?;
                let array = self.infer_expr(ctx, &array_node)?;
                let index_node = node.child_by_field("index")?;
                let index = self.infer_expr(ctx, &index_node)?;
                let ty_elem = match array.ty {
                    Ty::Array(ref t) => *t.clone(),
                    t => return Err(Spanned::from(at.clone(), TyError::NonArrayIdx(t))),
                };
                self.expect_ty(&Ty::I32, &index.ty, &index.at)?;
                Ok(Typed {
                    ty: ty_elem,
                    at,
                    it: Expr::ArrayIdx {
                        array: Box::new(array),
                        index: Box::new(index),
                    },
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

                Ok(Typed {
                    ty: then_branch.ty.clone(),
                    at,
                    it: Expr::If {
                        condition: Box::new(condition),
                        then_branch: Box::new(then_branch),
                        else_branch: Box::new(else_branch),
                    },
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
                Ok(Typed {
                    ty: ty.clone(),
                    at,
                    it: Expr::Block { declarations },
                })
            }
            "struct_e" => {
                let struct_name_node = node.child_by_field("struct")?;
                let struct_name = self.text(&struct_name_node);
                let expected_fields = match self.structs.get(struct_name) {
                    None => todo!(),
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
                    let field = StructFieldE {
                        name: self.spanned_text(&name_node),
                        expr,
                    };
                    actual_fields.push(field)
                }
                self.check_struct_fields(
                    expected_fields,
                    &actual_fields,
                    struct_name,
                    node.into(),
                )?;

                Ok(Typed {
                    ty: Ty::Struct(struct_name.to_string()),
                    at,
                    it: Expr::Struct {
                        fields: actual_fields,
                    },
                })
            }
            "struct_idx_e" | "intrinsic_e" => Err(Spanned::from(
                node.into(),
                TyError::Message(format!("Unhandled expr type: {}", node.kind())),
            )),
            k => unreachable!("Unknown expr type: {}", k),
        }
    }
}
