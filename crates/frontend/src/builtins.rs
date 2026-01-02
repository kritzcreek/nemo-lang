use camino::Utf8Path;
use rowan::TextRange;

use crate::ir::{Ctx, FuncTy, ModuleId, NameSupply, Ty};
use crate::types::{FuncDef, Interface};
use std::collections::HashMap;

enum Arity {
    Unary,
    Binary,
}

pub fn define_prim(ctx: &Ctx) {
    let mut functions = HashMap::new();
    let names = NameSupply::new();

    let numeric_prims = [
        (Ty::F32, Arity::Unary, "f32_neg"),
        (Ty::F32, Arity::Unary, "f32_abs"),
        (Ty::F32, Arity::Unary, "f32_ceil"),
        (Ty::F32, Arity::Unary, "f32_floor"),
        (Ty::F32, Arity::Unary, "f32_trunc"),
        (Ty::F32, Arity::Unary, "f32_nearest"),
        (Ty::F32, Arity::Unary, "f32_sqrt"),
        (Ty::F32, Arity::Binary, "f32_copysign"),
        (Ty::F32, Arity::Binary, "f32_min"),
        (Ty::F32, Arity::Binary, "f32_max"),
        (Ty::F64, Arity::Unary, "f64_neg"),
        (Ty::F64, Arity::Unary, "f64_abs"),
        (Ty::F64, Arity::Unary, "f64_ceil"),
        (Ty::F64, Arity::Unary, "f64_floor"),
        (Ty::F64, Arity::Unary, "f64_trunc"),
        (Ty::F64, Arity::Unary, "f64_nearest"),
        (Ty::F64, Arity::Unary, "f64_sqrt"),
        (Ty::F64, Arity::Binary, "f64_copysign"),
        (Ty::F64, Arity::Binary, "f64_min"),
        (Ty::F64, Arity::Binary, "f64_max"),
        (Ty::I32, Arity::Unary, "i32_clz"),
        (Ty::I32, Arity::Unary, "i32_ctz"),
        (Ty::I32, Arity::Unary, "i32_popcnt"),
        (Ty::I32, Arity::Binary, "i32_rotl"),
        (Ty::I32, Arity::Binary, "i32_rotr"),
        (Ty::I32, Arity::Binary, "i32_and"),
        (Ty::I32, Arity::Binary, "i32_or"),
        (Ty::I32, Arity::Binary, "i32_xor"),
        (Ty::I32, Arity::Binary, "i32_rem"),
        (Ty::I32, Arity::Binary, "i32_shl"),
        (Ty::I32, Arity::Binary, "i32_shr"),
        (Ty::I64, Arity::Unary, "i64_clz"),
        (Ty::I64, Arity::Unary, "i64_ctz"),
        (Ty::I64, Arity::Unary, "i64_popcnt"),
        (Ty::I64, Arity::Binary, "i64_rotl"),
        (Ty::I64, Arity::Binary, "i64_rotr"),
        (Ty::I64, Arity::Binary, "i64_and"),
        (Ty::I64, Arity::Binary, "i64_or"),
        (Ty::I64, Arity::Binary, "i64_xor"),
        (Ty::I64, Arity::Binary, "i64_rem"),
        (Ty::I64, Arity::Binary, "i64_shl"),
        (Ty::I64, Arity::Binary, "i64_shr"),
        (Ty::U32, Arity::Unary, "u32_clz"),
        (Ty::U32, Arity::Unary, "u32_ctz"),
        (Ty::U32, Arity::Unary, "u32_popcnt"),
        (Ty::U32, Arity::Binary, "u32_rotl"),
        (Ty::U32, Arity::Binary, "u32_rotr"),
        (Ty::U32, Arity::Binary, "u32_and"),
        (Ty::U32, Arity::Binary, "u32_or"),
        (Ty::U32, Arity::Binary, "u32_xor"),
        (Ty::U32, Arity::Binary, "u32_rem"),
        (Ty::U32, Arity::Binary, "u32_rem"),
        (Ty::U32, Arity::Binary, "u32_shl"),
        (Ty::U32, Arity::Binary, "u32_shr"),
        (Ty::U64, Arity::Unary, "u64_clz"),
        (Ty::U64, Arity::Unary, "u64_ctz"),
        (Ty::U64, Arity::Unary, "u64_popcnt"),
        (Ty::U64, Arity::Binary, "u64_rotl"),
        (Ty::U64, Arity::Binary, "u64_rotr"),
        (Ty::U64, Arity::Binary, "u64_and"),
        (Ty::U64, Arity::Binary, "u64_or"),
        (Ty::U64, Arity::Binary, "u64_xor"),
        (Ty::U64, Arity::Binary, "u64_rem"),
        (Ty::U64, Arity::Binary, "u64_rem"),
        (Ty::U64, Arity::Binary, "u64_shl"),
        (Ty::U64, Arity::Binary, "u64_shr"),
    ];

    let conversions = [
        (Ty::I32, Ty::F32, "f32_convert_i32"),
        (Ty::U32, Ty::F32, "f32_convert_u32"),
        (Ty::F32, Ty::I32, "i32_trunc_f32"),
        (Ty::F32, Ty::U32, "u32_trunc_f32"),
        (Ty::F32, Ty::I32, "i32_reinterpret_f32"),
        (Ty::I32, Ty::U32, "i32_to_u32"),
        (Ty::U32, Ty::I32, "u32_to_i32"),
        (Ty::I64, Ty::F64, "f64_convert_i64"),
        (Ty::U64, Ty::F64, "f64_convert_u64"),
        (Ty::F64, Ty::I64, "i64_trunc_f64"),
        (Ty::F64, Ty::U64, "u64_trunc_f64"),
        (Ty::F64, Ty::I64, "i64_reinterpret_f64"),
        (Ty::I64, Ty::U64, "i64_to_u64"),
        (Ty::U64, Ty::I64, "u64_to_i64"),

        (Ty::I32, Ty::I64, "i64_extend_i32"),
        (Ty::U32, Ty::U64, "u64_extend_u32"),
        (Ty::F32, Ty::F64, "f64_promote_f32"),
        (Ty::F64, Ty::F32, "f32_demote_f64"),
        (Ty::I64, Ty::I32, "i32_wrap_i64"),
        (Ty::U64, Ty::U32, "u32_wrap_u64"),
    ];

    let func_name = |n| {
        let sym = ctx.get_interner().get_or_intern_static(n);
        let name = names.func_idx(ModuleId::PRIM, sym, TextRange::default());
        (sym, name)
    };

    let ty_param = || {
        let sym = ctx.get_interner().get_or_intern_static("prim_ty_var");
        names.type_var_idx(ModuleId::PRIM, sym, TextRange::default())
    };

    for (ty, arity, name) in numeric_prims {
        let (sym, name) = func_name(name);
        functions.insert(
            sym,
            FuncDef {
                name,
                ty_params: vec![],
                ty: FuncTy {
                    arguments: match arity {
                        Arity::Unary => vec![ty.clone()],
                        Arity::Binary => vec![ty.clone(), ty.clone()],
                    },
                    result: ty,
                },
            },
        );
    }
    for (from, to, name) in conversions {
        let (sym, name) = func_name(name);
        functions.insert(
            sym,
            FuncDef {
                name,
                ty_params: vec![],
                ty: FuncTy {
                    arguments: vec![from],
                    result: to,
                },
            },
        );
    }
    {
        let (sym, name) = func_name("array_len");
        let ty_name = ty_param();
        functions.insert(
            sym,
            FuncDef {
                name,
                ty_params: vec![ty_name],
                ty: FuncTy {
                    arguments: vec![Ty::Array(Box::new(Ty::Var(ty_name)))],
                    result: Ty::I32,
                },
            },
        );
    }
    {
        let (sym, name) = func_name("array_new");
        let ty_name = ty_param();
        functions.insert(
            sym,
            FuncDef {
                name,
                ty_params: vec![ty_name],
                ty: FuncTy {
                    arguments: vec![Ty::Var(ty_name), Ty::I32],
                    result: Ty::Array(Box::new(Ty::Var(ty_name))),
                },
            },
        );
    }

    {
        let (sym, name) = func_name("array_copy");
        let ty_name = ty_param();
        functions.insert(
            sym,
            FuncDef {
                name,
                ty_params: vec![ty_name],
                ty: FuncTy {
                    arguments: vec![
                        Ty::Array(Box::new(Ty::Var(ty_name))),
                        Ty::I32,
                        Ty::Array(Box::new(Ty::Var(ty_name))),
                        Ty::I32,
                        Ty::I32,
                    ],
                    result: Ty::Unit,
                },
            },
        );
    }
    {
        let (sym, name) = func_name("panic");
        functions.insert(
            sym,
            FuncDef {
                name,
                ty_params: vec![],
                ty: FuncTy {
                    arguments: vec![],
                    result: Ty::Diverge,
                },
            },
        );
    }
    {
        let (sym, name) = func_name("bytes_get");
        functions.insert(
            sym,
            FuncDef {
                name,
                ty_params: vec![],
                ty: FuncTy {
                    arguments: vec![Ty::Bytes, Ty::I32],
                    result: Ty::I32,
                },
            },
        );
    }
    {
        let (sym, name) = func_name("bytes_set");
        functions.insert(
            sym,
            FuncDef {
                name,
                ty_params: vec![],
                ty: FuncTy {
                    arguments: vec![Ty::Bytes, Ty::I32, Ty::I32],
                    result: Ty::Unit,
                },
            },
        );
    }
    {
        let (sym, name) = func_name("bytes_len");
        functions.insert(
            sym,
            FuncDef {
                name,
                ty_params: vec![],
                ty: FuncTy {
                    arguments: vec![Ty::Bytes],
                    result: Ty::I32,
                },
            },
        );
    }
    {
        let (sym, name) = func_name("bytes_new");
        functions.insert(
            sym,
            FuncDef {
                name,
                ty_params: vec![],
                ty: FuncTy {
                    arguments: vec![Ty::I32, Ty::I32],
                    result: Ty::Bytes,
                },
            },
        );
    }
    {
        let (sym, name) = func_name("bytes_copy");
        functions.insert(
            sym,
            FuncDef {
                name,
                ty_params: vec![],
                ty: FuncTy {
                    arguments: vec![Ty::Bytes, Ty::I32, Ty::Bytes, Ty::I32, Ty::I32],
                    result: Ty::Unit,
                },
            },
        );
    }

    let interface = Interface {
        functions,
        type_names: HashMap::new(),
        types: HashMap::new(),
    };

    ctx.set_module(
        ModuleId::PRIM,
        "prim".to_string(),
        Utf8Path::new("<prim>").to_owned(),
        interface,
        names,
    );
}
