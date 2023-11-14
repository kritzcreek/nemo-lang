"struct" @keyword
"fn" @keyword
"let" @keyword
"set" @keyword
"import" @keyword
"from" @keyword
"if" @keyword
"else" @keyword
"while" @keyword

(ty_i32) @type
(ty_f32) @type
(ty_bool) @type
(ty_unit) @type
(ty_array) @type
(ty_struct) @type
(struct_e struct: (upper_ident) @type)

(top_func name: (lower_ident) @function)
(call_e function: (lower_ident) @function)
(intrinsic_ident) @function

(struct_field_e name: (lower_ident) @property)
(struct_field_top name: (lower_ident) @property)
(struct_idx_e index: (lower_ident) @property)
(set_struct_idx index: (lower_ident) @property)

(float_lit) @literal
(int_lit) @literal
(bool_lit) @literal

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
