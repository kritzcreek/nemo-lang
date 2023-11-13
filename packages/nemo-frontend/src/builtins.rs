use crate::types::{FuncTy, Ty};
use lazy_static::lazy_static;
use std::collections::HashMap;

pub struct Fn {
    pub name: &'static str,
    pub ty: FuncTy,
    // TODO: Change this to reference the relevant Wasm instruction
    pub instr: &'static str,
}

fn f32_func_unary(name: &'static str, instr: &'static str) -> Fn {
    Fn {
        name,
        ty: FuncTy {
            arguments: vec![Ty::F32],
            result: Ty::F32,
        },
        instr,
    }
}

fn f32_func_binary(name: &'static str, instr: &'static str) -> Fn {
    Fn {
        name,
        ty: FuncTy {
            arguments: vec![Ty::F32, Ty::F32],
            result: Ty::F32,
        },
        instr,
    }
}

fn i32_func_unary(name: &'static str, instr: &'static str) -> Fn {
    Fn {
        name,
        ty: FuncTy {
            arguments: vec![Ty::I32],
            result: Ty::I32,
        },
        instr,
    }
}

fn i32_func_binary(name: &'static str, instr: &'static str) -> Fn {
    Fn {
        name,
        ty: FuncTy {
            arguments: vec![Ty::I32, Ty::I32],
            result: Ty::I32,
        },
        instr,
    }
}

lazy_static! {
    pub static ref BUILTINS: HashMap<&'static str, Fn> = {
        let mut m = HashMap::new();
        m.insert("f32_neg", f32_func_unary("f32_neg", "f32_neg"));
        m.insert("f32_abs", f32_func_unary("f32_abs", "f32_abs"));
        m.insert("f32_ceil", f32_func_unary("f32_ceil", "f32_ceil"));
        m.insert("f32_floor", f32_func_unary("f32_floor", "f32_floor"));
        m.insert("f32_trunc", f32_func_unary("f32_trunc", "f32_trunc"));
        m.insert("f32_nearest", f32_func_unary("f32_nearest", "f32_nearest"));
        m.insert("f32_sqrt", f32_func_unary("f32_sqrt", "f32_sqrt"));
        m.insert(
            "f32_copysign",
            f32_func_binary("f32_copysign", "f32_copysign"),
        );
        m.insert("f32_min", f32_func_binary("f32_min", "f32_min"));
        m.insert("f32_max", f32_func_binary("f32_max", "f32_max"));
        m.insert(
            "f32_convert_i32_s",
            Fn {
                name: "f32_convert_i32_s",
                ty: FuncTy {
                    arguments: vec![Ty::I32],
                    result: Ty::F32,
                },
                instr: "f32_convert_i32_s",
            },
        );
        m.insert(
            "f32_convert_i32_u",
            Fn {
                name: "f32_convert_i32_u",
                ty: FuncTy {
                    arguments: vec![Ty::I32],
                    result: Ty::F32,
                },
                instr: "f32_convert_i32_u",
            },
        );

        m.insert("i32_clz", i32_func_unary("i32_clz", "i32_clz"));
        m.insert("i32_ctz", i32_func_unary("i32_ctz", "i32_ctz"));
        m.insert("i32_popcnt", i32_func_unary("i32_popcnt", "i32_popcnt"));
        m.insert("i32_rotl", i32_func_unary("i32_rotl", "i32_rotl"));
        m.insert("i32_rotr", i32_func_unary("i32_rotr", "i32_rotr"));
        m.insert("i32_rem_s", i32_func_binary("i32_rem_s", "i32_rem_s"));
        m.insert("i32_shl", i32_func_binary("i32_shl", "i32_shl"));
        m.insert("i32_shr_s", i32_func_binary("i32_shr_s", "i32_shr_s"));
        m.insert(
            "i32_trunc_f32_s",
            Fn {
                name: "i32_trunc_f32_s",
                ty: FuncTy {
                    arguments: vec![Ty::F32],
                    result: Ty::I32,
                },
                instr: "i32_trunc_f32_s",
            },
        );
        m.insert(
            "i32_reinterpret_f32",
            Fn {
                name: "i32_reinterpret_f32",
                ty: FuncTy {
                    arguments: vec![Ty::F32],
                    result: Ty::I32,
                },
                instr: "i32_reinterpret_f32",
            },
        );
        m
    };
}

pub fn lookup_builtin(name: &str) -> Option<&'static Fn> {
    BUILTINS.get(name)
}
