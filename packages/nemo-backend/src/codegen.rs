use wasm_encoder::{ConstExpr, Function, HeapType, Instruction, BlockType};

use crate::{
    ir::{Expr, ExprData, Lit, LitData, Program, Ty, Op, OpData},
    wasm_builder::{BodyBuilder, Builder},
};

pub fn codegen(program: Program) -> Vec<u8> {
    let mut builder = Builder::new();

    builder.finish()
}

struct Codegen<'a> {
    builder: Builder<'a>,
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
                let ty_idx = self.builder.array_type(&t);
                ConstExpr::ref_null(HeapType::Concrete(ty_idx))
            }
            Ty::Struct(s) => {
                let (ty_idx, _) = self.builder.struct_type(&s);
                ConstExpr::ref_null(HeapType::Concrete(*ty_idx))
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

    fn compile_expr(&mut self, body: &mut BodyBuilder, expr: Expr) -> Vec<Instruction> {
        match *expr.it {
            ExprData::Lit(l) => Self::compile_lit(l),
            ExprData::Var(v) => match body.lookup_local(&v) {
                Some(ix) => vec![Instruction::LocalGet(ix)],
                None => vec![Instruction::GlobalGet(self.builder.lookup_global(&v))],
            },
            ExprData::Call { func, arguments } => {
                let mut instrs: Vec<Instruction<'_>> = arguments
                    .into_iter()
                    .flat_map(|arg| self.compile_expr(body, arg))
                    .collect();
                // TODO: Built-ins
                instrs.push(Instruction::Call(self.builder.lookup_func(&func)));
                instrs
            }
            ExprData::Binary { op, left, right } => {
                let mut instrs = vec![];
                instrs.extend(self.compile_expr(body, left));
                instrs.extend(self.compile_expr(body, right));
                instrs.extend(Self::compile_op(&op));
                instrs
            },
            ExprData::Array(_) => todo!(),
            ExprData::ArrayIdx { array, index } => todo!(),
            ExprData::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let block_ty = BlockType::Result(self.builder.val_ty(&expr.ty));
                
                let mut instrs = vec![];
                instrs.extend(self.compile_expr(body, condition));
                instrs.push(Instruction::If(block_ty));
                instrs.extend(self.compile_expr(body, then_branch));
                instrs.push(Instruction::Else);
                instrs.extend(self.compile_expr(body, else_branch));
                instrs.push(Instruction::End);
                instrs
            },
            ExprData::Block { declarations } => todo!(),
            ExprData::Struct { name, fields } => todo!(),
            ExprData::StructIdx { expr, index } => {
                let (struct_ty, field_index) = self.builder.lookup_field(&index);
                let mut instrs = self.compile_expr(body, expr);
                // struct.get is not implemented in wasm-encoder yet
                // instrs.push(Instruction::StructGet());
                instrs
            },
            ExprData::Intrinsic {
                intrinsic,
                arguments,
            } => todo!(),
        }
    }

    pub fn compile_program(&mut self, program: Program) {
        for import in program.imports {
            self.builder.declare_import(import);
        }

        for struct_ in program.structs {
            self.builder.declare_struct(struct_)
        }

        for func in program.funcs.iter() {
            self.builder.declare_func(func.name.clone(), func.func_ty());
        }

        let mut start_func = Function::new([]);

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

                    let instrs = self.compile_expr(&global.init);
                    for inst in instrs {
                        start_func.instruction(&inst);
                    }
                    start_func.instruction(&Instruction::GlobalSet(index));
                }
            }
        }
        // TODO: declare start func
        // self.builder.declare_func(, func_ty)

        for func in program.funcs {
            let params = func
                .params
                .into_iter()
                .map(|(name, ty)| (name, self.builder.val_ty(&ty)))
                .collect();
            let mut body_builder = BodyBuilder::new(func.name, params);
        }
    }
}
