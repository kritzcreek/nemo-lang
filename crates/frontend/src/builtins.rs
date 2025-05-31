use crate::ir::{FuncTy, ModuleId, Name, NameTag, Ty};
use std::collections::HashMap;
use std::sync::LazyLock;

pub struct Fn {
    pub name: &'static str,
    pub ty_params: Vec<Name>,
    pub ty: FuncTy,
}

fn f32_func_unary(name: &'static str) -> Fn {
    Fn {
        name,
        ty_params: vec![],
        ty: FuncTy {
            arguments: vec![Ty::F32],
            result: Ty::F32,
        },
    }
}

fn f32_func_binary(name: &'static str) -> Fn {
    Fn {
        name,
        ty_params: vec![],
        ty: FuncTy {
            arguments: vec![Ty::F32, Ty::F32],
            result: Ty::F32,
        },
    }
}

fn i32_func_unary(name: &'static str) -> Fn {
    Fn {
        name,
        ty_params: vec![],
        ty: FuncTy {
            arguments: vec![Ty::I32],
            result: Ty::I32,
        },
    }
}

fn u32_func_unary(name: &'static str) -> Fn {
    Fn {
        name,
        ty_params: vec![],
        ty: FuncTy {
            arguments: vec![Ty::U32],
            result: Ty::U32,
        },
    }
}

fn i32_func_binary(name: &'static str) -> Fn {
    Fn {
        name,
        ty_params: vec![],
        ty: FuncTy {
            arguments: vec![Ty::I32, Ty::I32],
            result: Ty::I32,
        },
    }
}

fn u32_func_binary(name: &'static str) -> Fn {
    Fn {
        name,
        ty_params: vec![],
        ty: FuncTy {
            arguments: vec![Ty::U32, Ty::U32],
            result: Ty::U32,
        },
    }
}

// Should be initialized after namemap/context is created
static TODO_NAME: Name = Name {
    tag: NameTag::Gen,
    idx: 0,
    module: ModuleId::PRIM,
};

static BUILTINS: LazyLock<HashMap<&'static str, Fn>> = LazyLock::new(|| {
    let mut m = HashMap::new();
    m.insert("f32_neg", f32_func_unary("f32_neg"));
    m.insert("f32_abs", f32_func_unary("f32_abs"));
    m.insert("f32_ceil", f32_func_unary("f32_ceil"));
    m.insert("f32_floor", f32_func_unary("f32_floor"));
    m.insert("f32_trunc", f32_func_unary("f32_trunc"));
    m.insert("f32_nearest", f32_func_unary("f32_nearest"));
    m.insert("f32_sqrt", f32_func_unary("f32_sqrt"));
    m.insert("f32_copysign", f32_func_binary("f32_copysign"));
    m.insert("f32_min", f32_func_binary("f32_min"));
    m.insert("f32_max", f32_func_binary("f32_max"));
    m.insert(
        "f32_convert_i32",
        Fn {
            name: "f32_convert_i32",
            ty_params: vec![],
            ty: FuncTy {
                arguments: vec![Ty::I32],
                result: Ty::F32,
            },
        },
    );
    m.insert(
        "f32_convert_u32",
        Fn {
            name: "f32_convert_u32",
            ty_params: vec![],
            ty: FuncTy {
                arguments: vec![Ty::U32],
                result: Ty::F32,
            },
        },
    );

    m.insert("i32_clz", i32_func_unary("i32_clz"));
    m.insert("i32_ctz", i32_func_unary("i32_ctz"));
    m.insert("i32_popcnt", i32_func_unary("i32_popcnt"));
    m.insert("i32_rotl", i32_func_unary("i32_rotl"));
    m.insert("i32_rotr", i32_func_unary("i32_rotr"));
    m.insert("i32_and", i32_func_binary("i32_and"));
    m.insert("i32_or", i32_func_binary("i32_or"));
    m.insert("i32_xor", i32_func_binary("i32_xor"));
    m.insert("i32_rem", i32_func_binary("i32_rem"));
    m.insert("u32_rem", i32_func_binary("u32_rem"));
    m.insert("i32_shl", i32_func_binary("i32_shl"));
    m.insert("i32_shr", i32_func_binary("i32_shr"));

    m.insert("u32_clz", u32_func_unary("u32_clz"));
    m.insert("u32_ctz", u32_func_unary("u32_ctz"));
    m.insert("u32_popcnt", u32_func_unary("u32_popcnt"));
    m.insert("u32_rotl", u32_func_unary("u32_rotl"));
    m.insert("u32_rotr", u32_func_unary("u32_rotr"));
    m.insert("u32_and", u32_func_binary("u32_and"));
    m.insert("u32_or", u32_func_binary("u32_or"));
    m.insert("u32_xor", u32_func_binary("u32_xor"));
    m.insert("u32_rem", u32_func_binary("u32_rem"));
    m.insert("u32_rem", u32_func_binary("u32_rem"));
    m.insert("u32_shl", u32_func_binary("u32_shl"));
    m.insert("u32_shr", u32_func_binary("u32_shr"));
    m.insert(
        "i32_trunc_f32_s",
        Fn {
            name: "i32_trunc_f32_s",
            ty_params: vec![],
            ty: FuncTy {
                arguments: vec![Ty::F32],
                result: Ty::I32,
            },
        },
    );
    m.insert(
        "i32_reinterpret_f32",
        Fn {
            name: "i32_reinterpret_f32",
            ty_params: vec![],
            ty: FuncTy {
                arguments: vec![Ty::F32],
                result: Ty::I32,
            },
        },
    );
    m.insert(
        "i32_to_u32",
        Fn {
            name: "i32_to_u32",
            ty_params: vec![],
            ty: FuncTy {
                arguments: vec![Ty::I32],
                result: Ty::U32,
            },
        },
    );
    m.insert(
        "u32_to_i32",
        Fn {
            name: "u32_to_i32",
            ty_params: vec![],
            ty: FuncTy {
                arguments: vec![Ty::U32],
                result: Ty::I32,
            },
        },
    );
    m.insert(
        "array_len",
        Fn {
            name: "array_len",
            ty_params: vec![TODO_NAME],
            ty: FuncTy {
                arguments: vec![Ty::Array(Box::new(Ty::Var(TODO_NAME)))],
                result: Ty::I32,
            },
        },
    );
    m.insert(
        "array_new",
        Fn {
            name: "array_new",
            ty_params: vec![TODO_NAME],
            ty: FuncTy {
                arguments: vec![Ty::Var(TODO_NAME), Ty::I32],
                result: Ty::Array(Box::new(Ty::Var(TODO_NAME))),
            },
        },
    );
    m.insert(
        "array_copy",
        Fn {
            name: "array_copy",
            ty_params: vec![TODO_NAME],
            ty: FuncTy {
                arguments: vec![
                    Ty::Array(Box::new(Ty::Var(TODO_NAME))),
                    Ty::I32,
                    Ty::Array(Box::new(Ty::Var(TODO_NAME))),
                    Ty::I32,
                    Ty::I32,
                ],
                result: Ty::Unit,
            },
        },
    );
    m.insert(
        "bytes_get",
        Fn {
            name: "bytes_get",
            ty_params: vec![],
            ty: FuncTy {
                arguments: vec![Ty::Bytes, Ty::I32],
                result: Ty::I32,
            },
        },
    );
    m.insert(
        "bytes_set",
        Fn {
            name: "bytes_set",
            ty_params: vec![],
            ty: FuncTy {
                arguments: vec![Ty::Bytes, Ty::I32, Ty::I32],
                result: Ty::Unit,
            },
        },
    );
    m.insert(
        "bytes_len",
        Fn {
            name: "bytes_len",
            ty_params: vec![],
            ty: FuncTy {
                arguments: vec![Ty::Bytes],
                result: Ty::I32,
            },
        },
    );
    m.insert(
        "bytes_new",
        Fn {
            name: "bytes_new",
            ty_params: vec![],
            ty: FuncTy {
                arguments: vec![Ty::I32, Ty::I32],
                result: Ty::Bytes,
            },
        },
    );
    m.insert(
        "bytes_copy",
        Fn {
            name: "bytes_copy",
            ty_params: vec![],
            ty: FuncTy {
                arguments: vec![Ty::Bytes, Ty::I32, Ty::Bytes, Ty::I32, Ty::I32],
                result: Ty::Unit,
            },
        },
    );
    m
});

pub fn lookup_builtin(name: &str) -> Option<&'static Fn> {
    BUILTINS.get(name)
}
