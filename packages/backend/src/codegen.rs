use std::collections::HashMap;
use std::fmt::Write;

use wasm_encoder::{BlockType, ConstExpr, HeapType, Instruction, RefType, ValType};

use crate::{
    ir::{
        Callee, Declaration, DeclarationData, Expr, ExprData, Func, IntrinsicData, Lit, LitData,
        Name, Op, OpData, Pattern, PatternData, Program, SetTarget, SetTargetData, Substitution,
        Ty, TypeDef,
    },
    names::{Id, NameSupply},
    wasm_builder::{BodyBuilder, Builder},
};

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
        match *expr.it {
            ExprData::Lit(Lit {
                it: LitData::Bool(false),
                ..
            }) => Some(ConstExpr::i32_const(0)),
            ExprData::Lit(Lit {
                it: LitData::Bool(true),
                ..
            }) => Some(ConstExpr::i32_const(1)),
            ExprData::Lit(Lit {
                it: LitData::I32(n),
                ..
            }) => Some(ConstExpr::i32_const(n)),
            ExprData::Lit(Lit {
                it: LitData::F32(n),
                ..
            }) => Some(ConstExpr::f32_const(n)),
            _ => None,
        }
    }

    fn const_init_for_ty(&mut self, ty: &Ty) -> ConstExpr {
        match ty {
            Ty::I32 | Ty::Unit | Ty::Bool => ConstExpr::i32_const(0),
            Ty::F32 => ConstExpr::f32_const(0.0),
            Ty::Array(t) => {
                let ty_idx = self.builder.array_type_elem(t);
                ConstExpr::ref_null(HeapType::Concrete(ty_idx))
            }
            Ty::Struct(s, ty_args) => {
                let s = self.builder.struct_type(*s);
                ConstExpr::ref_null(HeapType::Concrete(s.type_idx))
            }
            Ty::Func(ty) => {
                let ty_idx = self.builder.func_type(ty);
                ConstExpr::ref_null(HeapType::Concrete(ty_idx))
            }
            Ty::Var(_) => {
                unreachable!("Globals can't be var-typed")
            }
            Ty::Any => {
                unreachable!("ANY shouldn't make it into codegen")
            }
        }
    }

    fn compile_lit(lit: Lit) -> Vec<Instruction<'a>> {
        match lit.it {
            LitData::I32(i) => vec![Instruction::I32Const(i)],
            LitData::F32(f) => vec![Instruction::F32Const(f)],
            LitData::Bool(t) => vec![Instruction::I32Const(if t { 1 } else { 0 })],
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
            ExprData::Lit(l) => Self::compile_lit(l),
            ExprData::Var(v) => match v {
                Name::Local(_) => vec![Instruction::LocalGet(body.lookup_local(&v).unwrap())],
                Name::Global(_) => vec![Instruction::GlobalGet(self.builder.lookup_global(&v))],
                Name::Func(_) => vec![Instruction::RefFunc(self.builder.lookup_func(&v))],
                n => unreachable!("Cannot compile a variable reference to {n:?}"),
            },
            ExprData::Call { func, arguments } => {
                let mut instrs = vec![];
                for arg in arguments {
                    let arg_instrs = self.compile_expr(body, arg);
                    instrs.extend(arg_instrs);
                }

                match func {
                    Callee::Func { name, type_args } => {
                        let name = if !type_args.is_empty() {
                            let type_args: Vec<Ty> = type_args
                                .into_iter()
                                .map(|t| self.builder.substitution().apply(t))
                                .collect();
                            self.instantiate_polyfunc(name, &type_args)
                        } else {
                            name
                        };
                        let func_idx = self.builder.lookup_func(&name);
                        instrs.push(Instruction::Call(func_idx));
                    }
                    Callee::FuncRef(callee) => {
                        let Ty::Func(ty) = &callee.ty else {
                            unreachable!("Non-function type for callee")
                        };
                        let ty_idx = self.builder.func_type(ty);
                        instrs.extend(self.compile_expr(body, callee));
                        instrs.push(Instruction::CallRef(ty_idx))
                    }
                    Callee::Builtin(builtin) => instrs.push(builtin_instruction(builtin)),
                }
                instrs
            }
            ExprData::Binary { op, left, right } => {
                let mut instrs = self.compile_expr(body, left);
                instrs.extend(self.compile_expr(body, right));
                instrs.extend(Self::compile_op(&op));
                instrs
            }
            ExprData::Array(elements) => {
                let array_size = elements.len() as u32;
                let array_type_index = self.builder.array_type(&expr.ty);

                let mut instrs = vec![];
                for element in elements {
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
                let scrutinee_ty = self.builder.val_ty(&scrutinee.ty);
                instrs.extend(self.compile_expr(body, scrutinee));

                // TODO: Could use name_supply to gen a proper named local here?
                let scrutinee_local = body.fresh_local(scrutinee_ty);
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
                let struct_info = self.builder.struct_type(name);
                let type_idx = struct_info.type_idx;
                // Sort fields according to field_order
                fields.sort_by_cached_key(|(name, _)| {
                    struct_info.fields.iter().position(|f| name == f).unwrap()
                });
                if let Some(tag) = struct_info.variant_tag {
                    instrs.push(Instruction::I32Const(tag))
                }

                for (_, expr) in fields {
                    instrs.extend(self.compile_expr(body, expr));
                }
                instrs.push(Instruction::StructNew(type_idx));
                instrs
            }
            ExprData::StructIdx { expr, index } => {
                let (struct_type_index, field_index) = self.builder.lookup_field(&index);
                let mut instrs = self.compile_expr(body, expr);
                instrs.push(Instruction::StructGet {
                    struct_type_index,
                    field_index,
                });
                instrs
            }
            ExprData::Intrinsic {
                intrinsic,
                arguments,
            } => {
                let mut instrs = vec![];
                for argument in arguments {
                    instrs.extend(self.compile_expr(body, argument))
                }
                match intrinsic.it {
                    IntrinsicData::ArrayLen => instrs.push(Instruction::ArrayLen),
                    IntrinsicData::ArrayNew => {
                        let array_ty = self.builder.array_type(&expr.ty);
                        instrs.push(Instruction::ArrayNew(array_ty))
                    }
                }
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
            PatternData::Var(v) => {
                let ty = self.builder.val_ty(&pattern.ty);
                let idx = body.new_local(v, ty);
                (
                    vec![Instruction::I32Const(1)],
                    vec![
                        Instruction::LocalGet(scrutinee_idx),
                        Instruction::LocalSet(idx),
                    ],
                )
            }
            PatternData::Variant {
                variant,
                alternative,
                binder,
            } => {
                let variant_info = self.builder.variant_type(variant);
                let check = vec![
                    Instruction::LocalGet(scrutinee_idx),
                    Instruction::StructGet {
                        struct_type_index: variant_info.type_idx,
                        // The tag field is always field 0
                        field_index: 0,
                    },
                    Instruction::I32Const(self.builder.variant_tag(variant, alternative)),
                    Instruction::I32Eq,
                ];

                let struct_info = self.builder.struct_type(alternative);
                let val_ty = ValType::Ref(RefType {
                    nullable: false,
                    heap_type: HeapType::Concrete(struct_info.type_idx),
                });

                let setup = vec![
                    Instruction::LocalGet(scrutinee_idx),
                    Instruction::RefCastNonNull(HeapType::Concrete(struct_info.type_idx)),
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
            DeclarationData::Expr(e) => {
                let mut instrs = self.compile_expr(body, e);
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
            SetTargetData::Array { target, index } => {
                let array_ty = self.builder.array_type(&target.ty);
                let mut instrs = self.compile_expr(body, target);
                instrs.extend(self.compile_expr(body, index));
                instrs.extend(self.compile_expr(body, expr));
                instrs.push(Instruction::ArraySet(array_ty));
                instrs
            }
            SetTargetData::Struct { target, index } => {
                let (struct_type_index, field_index) = self.builder.lookup_field(&index);
                let mut instrs = self.compile_expr(body, target);
                instrs.extend(self.compile_expr(body, expr));
                instrs.push(Instruction::StructSet {
                    struct_type_index,
                    field_index,
                });
                instrs
            }
            SetTargetData::Var { name } => {
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

    fn instantiate_polyfunc(&mut self, name: Name, ty_params: &[Ty]) -> Name {
        let new_name = {
            let poly_func = self.poly_funcs.get_mut(&name).expect("Unknown polyfunc");
            if let Some(existing) = poly_func.instances.get(ty_params) {
                return *existing;
            }
            let definition = self
                .builder
                .name_supply
                .lookup(name)
                .expect("Unknown polyfunc");
            let mut it = format!("{}#", definition.it);
            for param in ty_params {
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
            poly_func.instances.insert(ty_params.to_vec(), new_name);
            new_name
        };

        let poly_func = self.poly_funcs.get(&name).expect("Unknown polyfunc");
        let subst = Substitution::new(&poly_func.func.ty_params, ty_params);
        let previous_subst = self.builder.set_substitution(subst);
        self.builder
            .declare_func(new_name, poly_func.func.func_ty());

        let params = poly_func
            .func
            .clone()
            .params
            .into_iter()
            .map(|(name, ty)| (name, self.builder.val_ty(&ty)))
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
        b => unreachable!("Unknown builtin {b}"),
    }
}
