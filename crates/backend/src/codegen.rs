use std::collections::HashMap;
use std::fmt::Write;

use crate::wasm_builder::{BodyBuilder, Builder};
use frontend::ir::{
    Callee, Ctx, Declaration, DeclarationData, Expr, ExprData, Func, Global, Lit, LitData,
    ModuleId, Name, NameTag, Op, OpData, Pattern, PatternData, Program, SetTarget, SetTargetData,
    Substitution, Ty, TypeDef, UnOpData,
};
use text_size::TextRange;
use wasm_encoder::{BlockType, ConstExpr, HeapType, Instruction, RefType, ValType};

pub fn codegen(program: Program, ctx: Ctx) -> (Vec<u8>, Ctx) {
    let builder = Builder::new(ctx);
    let mut codegen = Codegen {
        builder,
        poly_funcs: HashMap::new(),
    };
    codegen.compile_program(program);
    codegen.finish()
}

struct PolyFunc {
    func: Func,
    // TODO: Should use ValType, but for now Ty gives us more predictable output
    instances: HashMap<Vec<Ty>, Name>,
}

struct CompiledPattern<'a> {
    check: Option<Vec<Instruction<'a>>>,
    deconstruct: Vec<Instruction<'a>>,
}

struct Codegen<'a> {
    builder: Builder<'a>,
    poly_funcs: HashMap<Name, PolyFunc>,
}

impl<'a> Codegen<'a> {
    fn const_expr(expr: &Expr) -> Option<ConstExpr> {
        let ExprData::Lit { lit } = &*expr.it else {
            return None;
        };
        match lit.it {
            LitData::Bool(false) => Some(ConstExpr::i32_const(0)),
            LitData::Bool(true) => Some(ConstExpr::i32_const(1)),
            LitData::I32(n) => Some(ConstExpr::i32_const(n)),
            LitData::F32(n) => Some(ConstExpr::f32_const(n.into())),
            _ => None,
        }
    }

    fn const_init_for_ty(&mut self, ty: &Ty) -> ConstExpr {
        match ty {
            Ty::I32 | Ty::U32 | Ty::Unit | Ty::Bool => ConstExpr::i32_const(0),
            Ty::I64 | Ty::U64 => ConstExpr::i64_const(0),
            Ty::F32 => ConstExpr::f32_const((0.0).into()),
            Ty::F64 => ConstExpr::f64_const((0.0).into()),
            Ty::Bytes => {
                let ty_idx = self.builder.bytes_ty();
                ConstExpr::ref_null(HeapType::Concrete(ty_idx))
            }
            Ty::Tuple(ts) => {
                let ty_idx = self.builder.tuple_type(ts);
                ConstExpr::ref_null(HeapType::Concrete(ty_idx))
            }
            Ty::Array(t) => {
                let ty_idx = self.builder.array_type_elem(t);
                ConstExpr::ref_null(HeapType::Concrete(ty_idx))
            }
            Ty::Cons { name: s, ty_args } => {
                let idx = self.builder.heap_type(*s, ty_args);
                ConstExpr::ref_null(HeapType::Concrete(idx))
            }
            Ty::Func(ty) => {
                let clos_ty = self.builder.closure_type(ty);
                ConstExpr::ref_null(HeapType::Concrete(clos_ty.closure_struct_ty))
            }
            Ty::Var(_) => {
                unreachable!("Globals can't be var-typed")
            }
            Ty::Diverge => {
                unreachable!("Globals can't be diverge-typed")
            }
            Ty::Error => {
                unreachable!("ERROR shouldn't make it into codegen")
            }
        }
    }

    fn compile_lit(&mut self, lit: Lit) -> Vec<Instruction<'a>> {
        match lit.it {
            LitData::I32(i) => vec![Instruction::I32Const(i)],
            LitData::I64(i) => vec![Instruction::I64Const(i)],
            LitData::U32(i) => vec![Instruction::I32Const(i as i32)],
            LitData::U64(i) => vec![Instruction::I64Const(i as i64)],
            LitData::F32(f) => vec![Instruction::F32Const(f.into())],
            LitData::F64(f) => vec![Instruction::F64Const(f.into())],
            LitData::Bool(t) => vec![Instruction::I32Const(if t { 1 } else { 0 })],
            LitData::Bytes(s) => {
                let bytes = s.as_bytes().to_vec();
                vec![
                    Instruction::I32Const(0),
                    Instruction::I32Const(bytes.len() as i32),
                    Instruction::ArrayNewData {
                        array_type_index: self.builder.bytes_ty(),
                        array_data_index: self.builder.data(bytes),
                    },
                ]
            }
            LitData::Unit => vec![Instruction::I32Const(0)],
        }
    }

    fn compile_op(op: &Op) -> Instruction<'a> {
        match op.it {
            OpData::I32Add => Instruction::I32Add,
            OpData::I32Sub => Instruction::I32Sub,
            OpData::I32Mul => Instruction::I32Mul,
            OpData::I32Div => Instruction::I32DivS,
            OpData::I32Shl => Instruction::I32Shl,
            OpData::I32Shr => Instruction::I32ShrS,
            OpData::I32Rem => Instruction::I32RemS,
            OpData::I32And => Instruction::I32And,
            OpData::I32Or => Instruction::I32Or,
            OpData::I32Lt => Instruction::I32LtS,
            OpData::I32Gt => Instruction::I32GtS,
            OpData::I32Le => Instruction::I32LeS,
            OpData::I32Ge => Instruction::I32GeS,
            OpData::I32Eq => Instruction::I32Eq,
            OpData::I32Ne => Instruction::I32Ne,

            OpData::I64Add => Instruction::I64Add,
            OpData::I64Sub => Instruction::I64Sub,
            OpData::I64Mul => Instruction::I64Mul,
            OpData::I64Div => Instruction::I64DivS,
            OpData::I64Shl => Instruction::I64Shl,
            OpData::I64Shr => Instruction::I64ShrS,
            OpData::I64Rem => Instruction::I64RemS,
            OpData::I64And => Instruction::I64And,
            OpData::I64Or => Instruction::I64Or,
            OpData::I64Lt => Instruction::I64LtS,
            OpData::I64Gt => Instruction::I64GtS,
            OpData::I64Le => Instruction::I64LeS,
            OpData::I64Ge => Instruction::I64GeS,
            OpData::I64Eq => Instruction::I64Eq,
            OpData::I64Ne => Instruction::I64Ne,

            OpData::U32Add => Instruction::I32Add,
            OpData::U32Sub => Instruction::I32Sub,
            OpData::U32Mul => Instruction::I32Mul,
            OpData::U32Div => Instruction::I32DivU,
            OpData::U32Shl => Instruction::I32Shl,
            OpData::U32Shr => Instruction::I32ShrU,
            OpData::U32Rem => Instruction::I32RemU,
            OpData::U32And => Instruction::I32And,
            OpData::U32Or => Instruction::I32Or,
            OpData::U32Lt => Instruction::I32LtU,
            OpData::U32Gt => Instruction::I32GtU,
            OpData::U32Le => Instruction::I32LeU,
            OpData::U32Ge => Instruction::I32GeU,
            OpData::U32Eq => Instruction::I32Eq,
            OpData::U32Ne => Instruction::I32Ne,

            OpData::U64Add => Instruction::I32Add,
            OpData::U64Sub => Instruction::I32Sub,
            OpData::U64Mul => Instruction::I32Mul,
            OpData::U64Div => Instruction::I32DivU,
            OpData::U64Shl => Instruction::I32Shl,
            OpData::U64Shr => Instruction::I32ShrU,
            OpData::U64Rem => Instruction::I32RemU,
            OpData::U64And => Instruction::I32And,
            OpData::U64Or => Instruction::I32Or,
            OpData::U64Lt => Instruction::I32LtU,
            OpData::U64Gt => Instruction::I32GtU,
            OpData::U64Le => Instruction::I32LeU,
            OpData::U64Ge => Instruction::I32GeU,
            OpData::U64Eq => Instruction::I32Eq,
            OpData::U64Ne => Instruction::I32Ne,

            OpData::F32Add => Instruction::F32Add,
            OpData::F32Sub => Instruction::F32Sub,
            OpData::F32Mul => Instruction::F32Mul,
            OpData::F32Div => Instruction::F32Div,
            OpData::F32Lt => Instruction::F32Lt,
            OpData::F32Gt => Instruction::F32Gt,
            OpData::F32Le => Instruction::F32Le,
            OpData::F32Ge => Instruction::F32Ge,
            OpData::F32Eq => Instruction::F32Eq,
            OpData::F32Ne => Instruction::F32Ne,

            OpData::F64Add => Instruction::F64Add,
            OpData::F64Sub => Instruction::F64Sub,
            OpData::F64Mul => Instruction::F64Mul,
            OpData::F64Div => Instruction::F64Div,
            OpData::F64Lt => Instruction::F64Lt,
            OpData::F64Gt => Instruction::F64Gt,
            OpData::F64Le => Instruction::F64Le,
            OpData::F64Ge => Instruction::F64Ge,
            OpData::F64Eq => Instruction::F64Eq,
            OpData::F64Ne => Instruction::F64Ne,

            OpData::BoolEq => Instruction::I32Eq,
            OpData::BoolNe => Instruction::I32Eq,
            OpData::BoolAnd => Instruction::I32And,
            OpData::BoolOr => Instruction::I32Or,
        }
    }

    fn compile_expr(&mut self, body: &mut BodyBuilder, expr: Expr) -> Vec<Instruction<'a>> {
        match *expr.it {
            ExprData::Lit { lit } => self.compile_lit(lit),
            ExprData::Var { name } => match name.tag {
                NameTag::Local => vec![Instruction::LocalGet(body.lookup_local(&name).unwrap())],
                NameTag::Global => vec![Instruction::GlobalGet(self.builder.lookup_global(&name))],
                NameTag::Func => {
                    let Ty::Func(ty) = &expr.ty else {
                        unreachable!("Non-function type for function reference")
                    };
                    let closure_ty = self.builder.closure_type(ty);
                    let wrapped_func_idx = self.builder.func_ref(name, closure_ty, ty);
                    vec![
                        Instruction::RefFunc(wrapped_func_idx),
                        Instruction::StructNew(closure_ty.closure_struct_ty),
                    ]
                }
                _ => unreachable!("Cannot compile a variable reference to {name:?}"),
            },
            ExprData::Call { func, arguments } => {
                let mut arg_instrs = vec![];
                for arg in arguments {
                    arg_instrs.extend(self.compile_expr(body, arg));
                }

                let mut instrs = vec![];
                match func {
                    Callee::Func { name, type_args } => {
                        instrs.extend(arg_instrs);
                        if name.module == ModuleId::PRIM {
                            instrs.extend(self.compile_builtin_call(name, type_args));
                        } else {
                            let name = if !type_args.is_empty() {
                                let type_args = self.builder.substitution().apply_subst(type_args);
                                self.instantiate_polyfunc(name, &type_args)
                            } else {
                                name
                            };
                            let func_idx = self.builder.lookup_func(&name);
                            instrs.push(Instruction::Call(func_idx));
                        }
                    }
                    Callee::FuncRef(callee) => {
                        let Ty::Func(ty) = callee.ty.clone() else {
                            unreachable!("Non-function type for callee")
                        };
                        let closure_info = self.builder.closure_type(ty.as_ref());
                        let clos_local = body.fresh_local(ValType::Ref(RefType {
                            nullable: false,
                            heap_type: HeapType::Concrete(closure_info.closure_struct_ty),
                        }));
                        instrs.extend(self.compile_expr(body, callee));
                        instrs.push(Instruction::LocalTee(clos_local));
                        instrs.extend(arg_instrs);
                        instrs.extend([
                            Instruction::LocalGet(clos_local),
                            Instruction::StructGet {
                                struct_type_index: closure_info.closure_struct_ty,
                                field_index: 0,
                            },
                        ]);
                        instrs.push(Instruction::CallRef(closure_info.closure_func_ty));
                    }
                }
                instrs
            }
            ExprData::Unary { op, expr } => {
                let mut instrs = vec![];
                match op.it {
                    UnOpData::I32Not | UnOpData::U32Not => {
                        instrs.extend(self.compile_expr(body, expr));
                        instrs.push(Instruction::I32Const(-1));
                        instrs.push(Instruction::I32Xor);
                    }
                    UnOpData::I64Not | UnOpData::U64Not => {
                        instrs.extend(self.compile_expr(body, expr));
                        instrs.push(Instruction::I64Const(-1));
                        instrs.push(Instruction::I64Xor);
                    }
                    UnOpData::I32Neg => {
                        instrs.push(Instruction::I32Const(0));
                        instrs.extend(self.compile_expr(body, expr));
                        instrs.push(Instruction::I32Sub);
                    }
                    UnOpData::I64Neg => {
                        instrs.push(Instruction::I64Const(0));
                        instrs.extend(self.compile_expr(body, expr));
                        instrs.push(Instruction::I64Sub);
                    }
                    UnOpData::F32Neg => {
                        instrs.extend(self.compile_expr(body, expr));
                        instrs.push(Instruction::F32Neg);
                    }
                    UnOpData::F64Neg => {
                        instrs.extend(self.compile_expr(body, expr));
                        instrs.push(Instruction::F64Neg);
                    }
                }
                instrs
            }
            ExprData::Binary { op, left, right } => {
                let mut instrs = self.compile_expr(body, left);
                // BoolOr and BoolAnd get compiled to ifs because their second
                // argument is evaluated lazily
                match op.it {
                    OpData::BoolOr => {
                        instrs.push(Instruction::If(BlockType::Result(ValType::I32)));
                        instrs.push(Instruction::I32Const(1));
                        instrs.push(Instruction::Else);
                        instrs.extend(self.compile_expr(body, right));
                        instrs.push(Instruction::End)
                    }
                    OpData::BoolAnd => {
                        instrs.push(Instruction::If(BlockType::Result(ValType::I32)));
                        instrs.extend(self.compile_expr(body, right));
                        instrs.push(Instruction::Else);
                        instrs.push(Instruction::I32Const(0));
                        instrs.push(Instruction::End)
                    }
                    _ => {
                        instrs.extend(self.compile_expr(body, right));
                        instrs.push(Self::compile_op(&op));
                    }
                }
                instrs
            }
            ExprData::Array { elems } => {
                let array_size = elems.len() as u32;
                let array_type_index = self.builder.array_type(&expr.ty);

                let mut instrs = vec![];
                for element in elems {
                    instrs.extend(self.compile_expr(body, element));
                }
                instrs.push(Instruction::ArrayNewFixed {
                    array_type_index,
                    array_size,
                });
                instrs
            }
            ExprData::ArrayIdx { array, index } => {
                let array_ty = self.builder.array_type(&array.ty);
                let mut instrs = self.compile_expr(body, array);
                instrs.extend(self.compile_expr(body, index));
                instrs.push(Instruction::ArrayGet(array_ty));
                instrs
            }
            ExprData::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let block_ty = BlockType::Result(self.builder.val_ty(&expr.ty));

                let mut instrs = self.compile_expr(body, condition);
                instrs.push(Instruction::If(block_ty));
                instrs.extend(self.compile_expr(body, then_branch));
                instrs.push(Instruction::Else);
                instrs.extend(self.compile_expr(body, else_branch));
                instrs.push(Instruction::End);
                instrs
            }
            ExprData::Block { declarations, expr } => {
                let mut instrs = vec![];
                for declaration in declarations {
                    instrs.extend(self.compile_decl(body, declaration))
                }
                instrs.extend(self.compile_expr(body, expr));
                instrs
            }
            ExprData::Match {
                scrutinee,
                branches,
            } => {
                let mut instrs = vec![];

                // TODO(no-intern-in-codegen): static
                let gen_name = self.builder.name_supply().local_idx(
                    ModuleId::CODEGEN,
                    self.builder
                        .ctx
                        .get_interner()
                        .get_or_intern("$match_scrutinee"),
                    scrutinee.at,
                );
                let scrutinee_ty = self.builder.val_ty(&scrutinee.ty);
                let scrutinee_local = body.new_local(gen_name, scrutinee_ty);
                instrs.extend(self.compile_expr(body, scrutinee));

                instrs.push(Instruction::LocalSet(scrutinee_local));

                let ret_ty = self.builder.val_ty(&expr.ty);

                // TODO: I hate this. builder support for blocks?
                let branch_count = branches.len() as u32;
                instrs.push(Instruction::Block(BlockType::Result(ret_ty)));
                instrs.push(Instruction::Block(BlockType::Empty));
                let mut checks = vec![];
                let mut bodies = vec![];
                for (depth, branch) in branches.into_iter().enumerate() {
                    let depth = depth as u32;
                    instrs.push(Instruction::Block(BlockType::Empty));
                    let compiled_pattern = self.compile_pattern(body, branch.pattern);
                    if let Some(check) = compiled_pattern.check {
                        checks.push(Instruction::LocalGet(scrutinee_local));
                        checks.extend(check);
                        checks.push(Instruction::BrIf(depth));
                    } else {
                        checks.push(Instruction::Br(depth))
                    }
                    bodies.push(Instruction::LocalGet(scrutinee_local));
                    bodies.extend(compiled_pattern.deconstruct);
                    bodies.extend(self.compile_expr(body, branch.body));
                    bodies.push(Instruction::Br(branch_count - depth));
                    bodies.push(Instruction::End)
                }
                instrs.extend(checks);
                instrs.push(Instruction::Br(branch_count));
                instrs.push(Instruction::End);
                instrs.extend(bodies);
                instrs.push(Instruction::Unreachable);
                instrs.push(Instruction::End);
                instrs
            }
            ExprData::Struct { name, mut fields } => {
                let mut instrs = vec![];
                let Ty::Cons { name: _, ty_args } = &expr.ty else {
                    panic!("Can't create a non-struct ty from a struct expr")
                };

                let type_idx = self.builder.heap_type(name, ty_args);
                let struct_info = self.builder.struct_type(name);
                // Sort fields according to field_order
                fields.sort_by_cached_key(|(name, _)| struct_info.field_idx(*name));
                if let Some(variant) = struct_info.definition.variant {
                    let tag = self.builder.variant_tag(variant, name);
                    instrs.push(Instruction::I32Const(tag))
                }

                for (_, expr) in fields {
                    instrs.extend(self.compile_expr(body, expr));
                }
                instrs.push(Instruction::StructNew(type_idx));
                instrs
            }
            ExprData::StructIdx { expr, index } => {
                let Ty::Cons { name, ty_args } = &expr.ty else {
                    panic!("Can't index a non-struct type")
                };
                let struct_type_index = self.builder.heap_type(*name, ty_args);
                let ty_info = self.builder.struct_type(*name);
                let field_index = ty_info.field_idx(index);

                let mut instrs = self.compile_expr(body, expr);
                instrs.push(Instruction::StructGet {
                    struct_type_index,
                    field_index,
                });
                instrs
            }
            ExprData::Tuple { exprs } => {
                let Ty::Tuple(ts) = &expr.ty else {
                    panic!("Can't create a non-tuple type from tuple")
                };
                let ty = self.builder.tuple_type(ts);
                let mut instrs = vec![];
                for e in exprs {
                    instrs.extend(self.compile_expr(body, e));
                }
                instrs.push(Instruction::StructNew(ty));
                instrs
            }
            ExprData::TupleIdx { expr, index } => {
                let Ty::Tuple(ts) = &expr.ty else {
                    panic!("Can't index a non-tuple type")
                };
                let ty_idx = self.builder.tuple_type(ts);
                let mut instrs = self.compile_expr(body, expr);
                instrs.push(Instruction::StructGet {
                    struct_type_index: ty_idx,
                    field_index: index,
                });
                instrs
            }
            ExprData::Return { expr } => {
                let mut instrs = self.compile_expr(body, expr);
                instrs.push(Instruction::Return);
                instrs
            }
            ExprData::Lambda {
                captures,
                params,
                return_ty: _,
                body: lambda_body,
            } => {
                let capture_tys = captures.iter().map(|(_, ty)| ty).collect::<Vec<_>>();
                let Ty::Func(func_ty) = &expr.ty else {
                    panic!("Found lambda expression with non-func ty: {:?}", expr.ty)
                };
                let closure_info = self.builder.closure_type(func_ty);
                let concrete_closure_ty = self
                    .builder
                    .closure_type_concrete(closure_info, capture_tys.as_ref());
                if captures.is_empty() {
                    // Actually not special, until I implement an optimization,
                    // that detects applications of closures with no env
                }
                // Hoist the lambda to a top-level function accepting an environment
                let (func_name, func_idx) = self
                    .builder
                    .declare_anon_func(expr.at, closure_info.closure_func_ty);
                let env_name = self.builder.name_supply().local_idx(
                    ModuleId::CODEGEN,
                    // TODO(no-intern-in-codegen): static
                    self.builder.ctx.get_interner().get_or_intern_static("env"),
                    TextRange::default(),
                );
                let mut func_params = vec![(
                    env_name,
                    ValType::Ref(RefType {
                        nullable: false,
                        heap_type: HeapType::Concrete(closure_info.closure_struct_ty),
                    }),
                )];
                func_params.extend(
                    params
                        .iter()
                        .map(|(name, ty)| (*name, self.builder.val_ty(ty))),
                );
                let mut body_builder = BodyBuilder::new(func_params);

                // Downcast to concret env type
                let env_local = body_builder.fresh_local(ValType::Ref(RefType {
                    nullable: false,
                    heap_type: HeapType::Concrete(concrete_closure_ty),
                }));
                let mut lambda_instrs = vec![];
                lambda_instrs.push(Instruction::LocalGet(0));
                lambda_instrs.push(Instruction::RefCastNonNull(HeapType::Concrete(
                    concrete_closure_ty,
                )));
                lambda_instrs.push(Instruction::LocalSet(env_local));

                // Splat captures into locals
                for (idx, (name, ty)) in captures.iter().enumerate() {
                    let local = body_builder.new_local(*name, self.builder.val_ty(ty));
                    lambda_instrs.push(Instruction::LocalGet(env_local));
                    lambda_instrs.push(Instruction::StructGet {
                        struct_type_index: concrete_closure_ty,
                        field_index: idx as u32 + 1,
                    });
                    lambda_instrs.push(Instruction::LocalSet(local));
                }
                // Generate function body
                lambda_instrs.extend(self.compile_expr(&mut body_builder, lambda_body));

                self.builder
                    .fill_func(func_name, body_builder.get_locals(), lambda_instrs);

                // Construct concrete closure struct
                let mut instrs = vec![Instruction::RefFunc(func_idx)];
                for (name, _) in captures {
                    let local = body.lookup_local(&name).expect("Capture not found");
                    instrs.push(Instruction::LocalGet(local));
                }
                instrs.push(Instruction::StructNew(concrete_closure_ty));
                instrs
            }
        }
    }

    // Expects the scrutinee on the stack
    fn compile_pattern(&mut self, body: &mut BodyBuilder, pattern: Pattern) -> CompiledPattern<'a> {
        match *pattern.it {
            PatternData::PatVar { var } => {
                let ty = self.builder.val_ty(&pattern.ty);
                let idx = body.new_local(var, ty);
                CompiledPattern {
                    check: None,
                    deconstruct: vec![Instruction::LocalSet(idx)],
                }
            }
            PatternData::PatVariant {
                variant,
                alternative,
                binder,
            } => {
                let Ty::Cons { name: _, ty_args } = &pattern.ty else {
                    panic!("Can't pattern match a non-struct type")
                };
                let subst = self.builder.substitution().apply_subst(ty_args.clone());

                let variant_type_idx = self.builder.heap_type(variant, &subst);
                let check = vec![
                    Instruction::StructGet {
                        struct_type_index: variant_type_idx,
                        // The tag field is always field 0
                        field_index: 0,
                    },
                    Instruction::I32Const(self.builder.variant_tag(variant, alternative)),
                    Instruction::I32Eq,
                ];
                let struct_type_idx = self.builder.heap_type(alternative, &subst);
                let val_ty = ValType::Ref(RefType {
                    nullable: false,
                    heap_type: HeapType::Concrete(struct_type_idx),
                });
                let deconstruct = vec![
                    Instruction::RefCastNonNull(HeapType::Concrete(struct_type_idx)),
                    Instruction::LocalSet(body.new_local(binder, val_ty)),
                ];
                CompiledPattern {
                    check: Some(check),
                    deconstruct,
                }
            }
        }
    }

    fn compile_decl(&mut self, body: &mut BodyBuilder, decl: Declaration) -> Vec<Instruction<'a>> {
        match decl.it {
            DeclarationData::Let { binder, expr } => {
                let val_ty = self.builder.val_ty(&expr.ty);
                let mut instrs = self.compile_expr(body, expr);
                let local_idx = body.new_local(binder, val_ty);
                instrs.push(Instruction::LocalSet(local_idx));
                instrs
            }
            DeclarationData::Set { set_target, expr } => {
                self.compile_set_target(body, set_target, expr)
            }
            DeclarationData::Expr { expr } => {
                let mut instrs = self.compile_expr(body, expr);
                instrs.push(Instruction::Drop);
                instrs
            }
            DeclarationData::While {
                condition,
                body: body_expr,
            } => {
                let mut instrs = vec![];
                let condition_instructions = self.compile_expr(body, condition);
                instrs.push(Instruction::Block(BlockType::Empty));

                instrs.extend(condition_instructions.clone());
                instrs.push(Instruction::I32Eqz);
                instrs.push(Instruction::BrIf(0));

                instrs.push(Instruction::Loop(BlockType::Empty));
                instrs.extend(self.compile_expr(body, body_expr));

                instrs.extend(condition_instructions);
                instrs.push(Instruction::I32Eqz);
                instrs.push(Instruction::BrIf(1));
                instrs.push(Instruction::Br(0));

                instrs.push(Instruction::End);
                instrs.push(Instruction::End);
                instrs
            }
        }
    }

    fn compile_set_target(
        &mut self,
        body: &mut BodyBuilder,
        set_target: SetTarget,
        expr: Expr,
    ) -> Vec<Instruction<'a>> {
        match set_target.it {
            SetTargetData::SetArray { target, index } => {
                let array_ty = self.builder.array_type(&target.ty);
                let mut instrs = self.compile_expr(body, target);
                instrs.extend(self.compile_expr(body, index));
                instrs.extend(self.compile_expr(body, expr));
                instrs.push(Instruction::ArraySet(array_ty));
                instrs
            }
            SetTargetData::SetStruct { target, index } => {
                let Ty::Cons { name, ty_args } = &target.ty else {
                    panic!("Can't index a non-struct type")
                };
                let struct_type_index = self.builder.heap_type(*name, ty_args);
                let ty_info = self.builder.struct_type(*name);
                let field_index = ty_info.field_idx(index);

                let mut instrs = self.compile_expr(body, target);
                instrs.extend(self.compile_expr(body, expr));
                instrs.push(Instruction::StructSet {
                    struct_type_index,
                    field_index,
                });
                instrs
            }
            SetTargetData::SetVar { name } => {
                let mut instrs = self.compile_expr(body, expr);
                match name.tag {
                    NameTag::Global => {
                        instrs.push(Instruction::GlobalSet(self.builder.lookup_global(&name)));
                    }
                    NameTag::Local => {
                        instrs.push(Instruction::LocalSet(body.lookup_local(&name).unwrap()));
                    }
                    NameTag::Func
                    | NameTag::Type
                    | NameTag::TypeVar
                    | NameTag::Field
                    | NameTag::Gen => unreachable!("can't set a non local/global variable"),
                };
                instrs
            }
        }
    }

    fn instantiate_polyfunc(&mut self, name: Name, ty_params: &Substitution) -> Name {
        let new_name = {
            let poly_func = self.poly_funcs.get_mut(&name).expect("Unknown polyfunc");
            let tys = ty_params.tys_owned();
            if let Some(existing) = poly_func.instances.get(&tys) {
                return *existing;
            }
            let func_name = self.builder.ctx.display_qualified_name(name);
            let mut it = format!("{func_name}#");
            for param in &tys {
                write!(&mut it, "_{}", param.display(&self.builder.ctx)).unwrap()
            }
            // TODO(no-intern-in-codegen): Monomorphization creates new functions that need names
            let new_name = self.builder.name_supply().func_idx(
                ModuleId::CODEGEN,
                self.builder.ctx.get_interner().get_or_intern(&func_name),
                TextRange::default(),
            );
            poly_func.instances.insert(tys, new_name);
            new_name
        };

        let poly_func = self.poly_funcs.get(&name).expect("Unknown polyfunc");
        let previous_subst = self.builder.set_substitution(ty_params.clone());
        self.builder
            .declare_func(new_name, poly_func.func.func_ty());

        let params = poly_func
            .func
            .params
            .iter()
            .map(|(name, ty)| (*name, self.builder.val_ty(ty)))
            .collect();
        let mut body_builder = BodyBuilder::new(params);
        let body = self.compile_expr(&mut body_builder, poly_func.func.clone().body);
        let locals = body_builder.get_locals();
        self.builder.fill_func(new_name, locals, body);
        self.builder.restore_substitution(previous_subst);

        new_name
    }

    fn initialize_globals(&mut self, globals: Vec<Global>) {
        let mut start_body = BodyBuilder::new(vec![]);
        let mut start_instrs = vec![];
        for global in globals {
            match Self::const_expr(&global.init) {
                Some(e) => {
                    self.builder
                        .declare_global(global.binder, &global.init.ty, e);
                }
                None => {
                    let pre_init = self.const_init_for_ty(&global.init.ty);
                    let index =
                        self.builder
                            .declare_global(global.binder, &global.init.ty, pre_init);

                    start_instrs.extend(self.compile_expr(&mut start_body, global.init));
                    start_instrs.push(Instruction::GlobalSet(index))
                }
            }
        }
        let start_fn = self.builder.name_supply().func_idx(
            ModuleId::CODEGEN,
            self.builder
                .ctx
                .get_interner()
                // TODO(no-intern-in-codegen): static
                .get_or_intern_static("start"),
            TextRange::default(),
        );
        let start_locals = start_body.get_locals();
        self.builder.declare_start(start_fn);
        self.builder.fill_func(start_fn, start_locals, start_instrs);
    }

    fn compile_builtin_call(
        &mut self,
        name: Name,
        ty_args: Substitution,
    ) -> Vec<Instruction<'static>> {
        assert!(name.module == ModuleId::PRIM);
        assert!(name.tag == NameTag::Func);

        match self.builder.resolve_name(name) {
            "f32_neg" => vec![Instruction::F32Neg],
            "f32_abs" => vec![Instruction::F32Abs],
            "f32_ceil" => vec![Instruction::F32Ceil],
            "f32_floor" => vec![Instruction::F32Floor],
            "f32_trunc" => vec![Instruction::F32Trunc],
            "f32_nearest" => vec![Instruction::F32Nearest],
            "f32_sqrt" => vec![Instruction::F32Sqrt],
            "f32_copysign" => vec![Instruction::F32Copysign],
            "f32_min" => vec![Instruction::F32Min],
            "f32_max" => vec![Instruction::F32Max],
            "f32_convert_i32" => vec![Instruction::F32ConvertI32S],
            "f32_convert_u32" => vec![Instruction::F32ConvertI32U],

            "f64_neg" => vec![Instruction::F64Neg],
            "f64_abs" => vec![Instruction::F64Abs],
            "f64_ceil" => vec![Instruction::F64Ceil],
            "f64_floor" => vec![Instruction::F64Floor],
            "f64_trunc" => vec![Instruction::F64Trunc],
            "f64_nearest" => vec![Instruction::F64Nearest],
            "f64_sqrt" => vec![Instruction::F64Sqrt],
            "f64_copysign" => vec![Instruction::F64Copysign],
            "f64_min" => vec![Instruction::F64Min],
            "f64_max" => vec![Instruction::F64Max],
            "f64_convert_i64" => vec![Instruction::F64ConvertI64S],
            "f64_convert_u64" => vec![Instruction::F64ConvertI64U],

            "i32_clz" | "u32_clz" => vec![Instruction::I32Clz],
            "i32_ctz" | "u32_ctz" => vec![Instruction::I32Ctz],
            "i32_popcnt" | "u32_popcnt" => vec![Instruction::I32Popcnt],
            "i32_rotl" | "u32_rotl" => vec![Instruction::I32Rotl],
            "i32_rotr" | "u32_rotr" => vec![Instruction::I32Rotr],
            "i32_and" | "u32_and" => vec![Instruction::I32And],
            "i32_or" | "u32_or" => vec![Instruction::I32Or],
            "i32_xor" | "u32_xor" => vec![Instruction::I32Xor],
            "i32_shl" | "u32_shl" => vec![Instruction::I32Shl],

            "i64_clz" | "u64_clz" => vec![Instruction::I64Clz],
            "i64_ctz" | "u64_ctz" => vec![Instruction::I64Ctz],
            "i64_popcnt" | "u64_popcnt" => vec![Instruction::I64Popcnt],
            "i64_rotl" | "u64_rotl" => vec![Instruction::I64Rotl],
            "i64_rotr" | "u64_rotr" => vec![Instruction::I64Rotr],
            "i64_and" | "u64_and" => vec![Instruction::I64And],
            "i64_or" | "u64_or" => vec![Instruction::I64Or],
            "i64_xor" | "u64_xor" => vec![Instruction::I64Xor],
            "i64_shl" | "u64_shl" => vec![Instruction::I64Shl],

            "i32_rem" => vec![Instruction::I32RemS],
            "u32_rem" => vec![Instruction::I32RemU],
            "i32_shr" => vec![Instruction::I32ShrS],
            "u32_shr" => vec![Instruction::I32ShrU],
            "i32_trunc_f32" => vec![Instruction::I32TruncF32S],
            "u32_trunc_f32" => vec![Instruction::I32TruncF32U],

            "i64_rem" => vec![Instruction::I64RemS],
            "u64_rem" => vec![Instruction::I64RemU],
            "i64_shr" => vec![Instruction::I64ShrS],
            "u64_shr" => vec![Instruction::I64ShrU],
            "i64_trunc_f64" => vec![Instruction::I64TruncF64S],
            "u64_trunc_f64" => vec![Instruction::I64TruncF64U],

            "i32_to_u32" | "u32_to_i32" => vec![],
            "i64_to_u64" | "u64_to_i64" => vec![],

            "i32_reinterpret_f32" => vec![Instruction::I32ReinterpretF32],
            "i64_reinterpret_f64" => vec![Instruction::I64ReinterpretF64],

            "array_len" => vec![Instruction::ArrayLen],
            "array_new" => {
                let ty_idx = self.builder.array_type_elem(ty_args.tys()[0]);
                vec![Instruction::ArrayNew(ty_idx)]
            }
            "array_copy" => {
                let ty_idx = self.builder.array_type_elem(ty_args.tys()[0]);
                vec![
                    Instruction::ArrayCopy {
                        array_type_index_dst: ty_idx,
                        array_type_index_src: ty_idx,
                    },
                    Instruction::I32Const(0),
                ]
            }
            "bytes_get" => vec![Instruction::ArrayGetU(self.builder.bytes_ty())],
            "bytes_set" => vec![
                Instruction::ArraySet(self.builder.bytes_ty()),
                Instruction::I32Const(0),
            ],
            "bytes_len" => vec![Instruction::ArrayLen],
            "bytes_new" => vec![Instruction::ArrayNew(self.builder.bytes_ty())],
            "bytes_copy" => {
                let bytes_ty = self.builder.bytes_ty();
                vec![
                    Instruction::ArrayCopy {
                        array_type_index_src: bytes_ty,
                        array_type_index_dst: bytes_ty,
                    },
                    Instruction::I32Const(0),
                ]
            }
            "panic" => {
                vec![Instruction::Unreachable]
            }
            name => panic!("Unknown prim: {name}"),
        }
    }

    pub fn compile_program(&mut self, program: Program) {
        for import in program.imports {
            self.builder.declare_import(import);
        }
        self.builder.finish_imports();

        for type_def in program.types {
            match type_def {
                TypeDef::Struct(s) => self.builder.declare_struct(s),
                TypeDef::Variant(v) => self.builder.declare_variant(v),
            }
        }

        for func in &program.funcs {
            if func.is_monomorphic() {
                self.builder.declare_func(func.name, func.func_ty());
            } else {
                self.poly_funcs.insert(
                    func.name,
                    PolyFunc {
                        func: func.clone(),
                        instances: HashMap::new(),
                    },
                );
            }
        }

        self.initialize_globals(program.globals);

        for func in program.funcs {
            if func.is_polymorphic() {
                continue;
            }
            let params = func
                .params
                .into_iter()
                .map(|(name, ty)| (name, self.builder.val_ty(&ty)))
                .collect();
            let mut body_builder = BodyBuilder::new(params);
            let body = self.compile_expr(&mut body_builder, func.body);
            let locals = body_builder.get_locals();
            self.builder.fill_func(func.name, locals, body);
            let module_name = self.builder.resolve_module_name(func.name);
            let external_name = if module_name.is_empty() {
                self.builder.resolve_name(func.name).to_owned()
            } else {
                format!("{module_name}::{}", self.builder.resolve_name(func.name))
            };
            self.builder.declare_export(func.name, external_name);
        }
    }

    pub fn finish(self) -> (Vec<u8>, Ctx) {
        self.builder.finish()
    }
}
