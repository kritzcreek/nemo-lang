"struct" @keyword
"variant" @keyword
"fn" @keyword
"global" @keyword
"let" @keyword
"set" @keyword
"import" @keyword
"from" @keyword
"if" @keyword
"else" @keyword
"match" @keyword
"while" @keyword
"true" @keyword
"false" @keyword

(ty_i32) @type
(ty_f32) @type
(ty_bool) @type
(ty_unit) @type
(ty_struct) @type
(top_struct name: (upper_ident) @type)
(top_variant name: (upper_ident) @type)
(struct_e struct: (upper_ident) @type)
(variant_pat (upper_ident) @type)
(qualifier (upper_ident) @type)

(top_func name: (lower_ident) @function)
(call_e function: (var_e (lower_ident)) @function)
(intrinsic_ident) @function

(struct_field_e name: (lower_ident) @property)
(struct_field_top name: (lower_ident) @property)
(struct_idx_e index: (lower_ident) @property)
(set_struct_idx index: (lower_ident) @property)

(float_lit) @number
(int_lit) @number

"&&" @operator
"||" @operator
"==" @operator
"!=" @operator
"<" @operator
"<=" @operator
">" @operator
">=" @operator
"+" @operator
"-" @operator
"*" @operator
"/" @operator

(comment) @comment
