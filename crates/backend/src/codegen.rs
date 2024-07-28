use std::collections::HashMap;
use std::fmt::Write;

use crate::wasm_builder::{BodyBuilder, Builder};
use frontend::ir::{
    Callee, Declaration, DeclarationData, Expr, ExprData, Func, Id, Lit, LitData, Name, NameSupply,
    Op, OpData, Pattern, PatternData, Program, SetTarget, SetTargetData, Substitution, Ty, TypeDef,
};
use text_size::TextRange;
use wasm_encoder::{BlockType, ConstExpr, HeapType, Instruction, RefType, ValType};

pub fn codegen(program: Program, name_supply: NameSupply) -> (Vec<u8>, NameSupply) {
    let builder = Builder::new(name_supply);
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
            LitData::F32(n) => Some(ConstExpr::f32_const(n)),
            _ => None,
        }
    }

    fn const_init_for_ty(&mut self, ty: &Ty) -> ConstExpr {
        match ty {
            Ty::I32 | Ty::Unit | Ty::Bool => ConstExpr::i32_const(0),
            Ty::F32 => ConstExpr::f32_const(0.0),
            Ty::Bytes => {
                let ty_idx = self.builder.bytes_ty();
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
            LitData::F32(f) => vec![Instruction::F32Const(f)],
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

    fn compile_op(op: &Op) -> Vec<Instruction<'a>> {
        match op.it {
            OpData::I32Add => vec![Instruction::I32Add],
            OpData::I32Sub => vec![Instruction::I32Sub],
            OpData::I32Mul => vec![Instruction::I32Mul],
            OpData::I32Div => vec![Instruction::I32DivS],
            OpData::I32Lt => vec![Instruction::I32LtS],
            OpData::I32Gt => vec![Instruction::I32GtS],
            OpData::I32Le => vec![Instruction::I32LeS],
            OpData::I32Ge => vec![Instruction::I32GeS],
            OpData::I32Eq => vec![Instruction::I32Eq],
            OpData::I32Ne => vec![Instruction::I32Ne],
            OpData::F32Add => vec![Instruction::F32Add],
            OpData::F32Sub => vec![Instruction::F32Sub],
            OpData::F32Mul => vec![Instruction::F32Mul],
            OpData::F32Div => vec![Instruction::F32Div],
            OpData::F32Lt => vec![Instruction::F32Lt],
            OpData::F32Gt => vec![Instruction::F32Gt],
            OpData::F32Le => vec![Instruction::F32Le],
            OpData::F32Ge => vec![Instruction::F32Ge],
            OpData::F32Eq => vec![Instruction::F32Eq],
            OpData::F32Ne => vec![Instruction::F32Ne],
            OpData::BoolEq => vec![Instruction::I32Eq],
            OpData::BoolNe => vec![Instruction::I32Eq],
            OpData::BoolAnd => vec![Instruction::I32And],
            OpData::BoolOr => vec![Instruction::I32Or],
        }
    }

    fn compile_expr(&mut self, body: &mut BodyBuilder, expr: Expr) -> Vec<Instruction<'a>> {
        match *expr.it {
            ExprData::Lit { lit } => self.compile_lit(lit),
            ExprData::Var { name } => match name {
                Name::Local(_) => vec![Instruction::LocalGet(body.lookup_local(&name).unwrap())],
                Name::Global(_) => vec![Instruction::GlobalGet(self.builder.lookup_global(&name))],
                Name::Func(_) => {
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
                        let name = if !type_args.is_empty() {
                            let type_args = self.builder.substitution().apply_subst(type_args);
                            self.instantiate_polyfunc(name, &type_args)
                        } else {
                            name
                        };
                        let func_idx = self.builder.lookup_func(&name);
                        instrs.push(Instruction::Call(func_idx));
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
                    Callee::Builtin(builtin) => {
                        instrs.extend(arg_instrs);
                        if builtin == "array_new" {
                            let array_ty = self.builder.array_type(&expr.ty);
                            instrs.push(Instruction::ArrayNew(array_ty))
                        } else {
                            instrs.push(builtin_instruction(builtin))
                        }
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
                        instrs.extend(Self::compile_op(&op));
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

                let gen_name = self.builder.name_supply.local_idx(Id {
                    it: "$match_scrutinee".to_string(),
                    at: scrutinee.at,
                });
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
                    let (check, setup) =
                        self.compile_pattern(body, branch.pattern, scrutinee_local);
                    checks.extend(check);
                    checks.push(Instruction::BrIf(depth));

                    bodies.extend(setup);
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
                let env_name = self.builder.name_supply.local_idx(Id {
                    at: TextRange::empty(0.into()),
                    it: "env".to_string(),
                });
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

    fn compile_pattern(
        &mut self,
        body: &mut BodyBuilder,
        pattern: Pattern,
        scrutinee_idx: u32,
    ) -> (Vec<Instruction<'a>>, Vec<Instruction<'a>>) {
        match *pattern.it {
            PatternData::PatVar { var } => {
                let ty = self.builder.val_ty(&pattern.ty);
                let idx = body.new_local(var, ty);
                (
                    vec![Instruction::I32Const(1)],
                    vec![
                        Instruction::LocalGet(scrutinee_idx),
                        Instruction::LocalSet(idx),
                    ],
                )
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
                    Instruction::LocalGet(scrutinee_idx),
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
                let setup = vec![
                    Instruction::LocalGet(scrutinee_idx),
                    Instruction::RefCastNonNull(HeapType::Concrete(struct_type_idx)),
                    Instruction::LocalSet(body.new_local(binder, val_ty)),
                ];
                (check, setup)
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
                match name {
                    Name::Global(_) => {
                        instrs.push(Instruction::GlobalSet(self.builder.lookup_global(&name)));
                    }
                    Name::Local(_) => {
                        instrs.push(Instruction::LocalSet(body.lookup_local(&name).unwrap()));
                    }
                    Name::Func(_)
                    | Name::Type(_)
                    | Name::TypeVar(_)
                    | Name::Field(_)
                    | Name::Gen(_) => {
                        unreachable!("can't set a non local/global variable")
                    }
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
            let definition = self
                .builder
                .name_supply
                .lookup(name)
                .expect("Unknown polyfunc");
            let mut it = format!("{}#", definition.it);
            for param in &tys {
                write!(
                    &mut it,
                    "_{}",
                    param.display(&self.builder.name_supply.name_map)
                )
                .unwrap()
            }
            let new_name = self.builder.name_supply.func_idx(Id {
                it,
                at: definition.at,
            });
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

    pub fn compile_program(&mut self, program: Program) {
        for import in program.imports {
            self.builder.declare_import(import);
        }

        // Variants need to be declared before structs
        for type_def in program.types.iter() {
            if let TypeDef::Variant(v) = type_def {
                self.builder.declare_variant(v.clone())
            }
        }

        for type_def in program.types {
            if let TypeDef::Struct(s) = type_def {
                self.builder.declare_struct(s)
            }
        }

        for func in program.funcs.iter() {
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

        {
            let mut start_body = BodyBuilder::new(vec![]);
            let mut start_instrs = vec![];
            for global in program.globals {
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
            let start_locals = start_body.get_locals();
            self.builder.declare_start(program.start_fn);
            self.builder
                .fill_func(program.start_fn, start_locals, start_instrs);
        }

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
            self.builder.declare_export(
                func.name,
                self.builder.resolve_name(func.name).it.to_string(),
            );
        }
    }

    pub fn finish(self) -> (Vec<u8>, NameSupply) {
        self.builder.finish()
    }
}

fn builtin_instruction(builtin: &str) -> Instruction<'static> {
    match builtin {
        "f32_neg" => Instruction::F32Neg,
        "f32_abs" => Instruction::F32Abs,
        "f32_ceil" => Instruction::F32Ceil,
        "f32_floor" => Instruction::F32Floor,
        "f32_trunc" => Instruction::F32Trunc,
        "f32_nearest" => Instruction::F32Nearest,
        "f32_sqrt" => Instruction::F32Sqrt,
        "f32_copysign" => Instruction::F32Copysign,
        "f32_min" => Instruction::F32Min,
        "f32_max" => Instruction::F32Max,
        "f32_convert_i32_s" => Instruction::F32ConvertI32S,
        "f32_convert_i32_u" => Instruction::F32ConvertI32U,
        "i32_clz" => Instruction::I32Clz,
        "i32_ctz" => Instruction::I32Ctz,
        "i32_popcnt" => Instruction::I32Popcnt,
        "i32_rotl" => Instruction::I32Rotl,
        "i32_rotr" => Instruction::I32Rotr,
        "i32_rem_s" => Instruction::I32RemS,
        "i32_shl" => Instruction::I32Shl,
        "i32_shr_s" => Instruction::I32ShrS,
        "i32_trunc_f32_s" => Instruction::I32TruncF32S,
        "i32_reinterpret_f32" => Instruction::I32ReinterpretF32,
        "array_len" => Instruction::ArrayLen,
        "array_new" => unreachable!("array_new needs special handling"),
        b => unreachable!("Unknown builtin {b}"),
    }
}
