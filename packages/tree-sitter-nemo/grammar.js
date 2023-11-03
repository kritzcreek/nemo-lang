function comma_sep_trailing(rule) {
  return seq(repeat(seq(rule, ',')), optional(rule))
}

function semi_sep_trailing(rule) {
  return seq(repeat(seq(rule, ';')), optional(rule))
}

module.exports = grammar({
    name: 'nemo',
    word: $ => $.lower_ident,
  
    rules: {
      source_file: $ => repeat($._toplevel),

      lower_ident: $ => /[a-z_][a-zA-Z0-9_]*/,
      upper_ident: $ => /[A-Z][a-zA-Z0-9_]*/,
      intrinsic_ident: $ => /@[a-z_][a-zA-Z0-9_]*/,

      // Literals
      int_lit: $ => choice('0', /[1-9][0-9]*/),
      float_lit: $ => token(seq(choice('0', /[0-9]*/), '.', /[0-9]+/)),
      bool_lit: $ => choice('true', 'false'),
      _lit: $ => choice($.int_lit, $.float_lit, $.bool_lit),

      // Expressions
      var_e: $ => $.lower_ident,
      array_e: $ => seq('[', comma_sep_trailing($._expr), ']'),
      array_idx_e: $ => seq($._expr, '[', $._expr, ']'),

      struct_field_e: $ => seq($.lower_ident, '=', $._expr),
      struct_e: $ => seq($.upper_ident, '{', comma_sep_trailing($.struct_field_e), '}'),
      struct_idx_e: $ => seq($._expr, '.', $.lower_ident),

      if_e: $ => seq('if', $._expr, $.block_e, 'else', $.block_e),

      call_args: $ => seq('(', comma_sep_trailing($._expr), ')'),
      call_e: $ => seq($.lower_ident, $.call_args),
      block_e: $ => seq('{', semi_sep_trailing($._decl), '}'),

      intrinsic_e: $ => seq($.intrinsic_ident, $.call_args),
      bin_e: $ => choice(
        prec.left(5, seq($._expr, choice('*', '/'), $._expr)),
        prec.left(4, seq($._expr, choice('+', '-'), $._expr)),
        prec.left(3, seq($._expr, choice('==', '!=', '<', '<=', '>', '>='), $._expr)),
        prec.left(2, seq($._expr, '||', $._expr)),
        prec.left(1, seq($._expr, '&&', $._expr)),
      ),

      // Extracted this rule to make it clear that we want the parser to greedily parse:
      // my_func(args) into `(call_e ident args)` and not `(var_e ERROR)`
      _call_or_var: $ => choice($.call_e, $.var_e),

      _expr: $ => choice(
        $._lit, 
        $._call_or_var,
        $.array_e,
        $.array_idx_e,
        $.struct_e,
        $.struct_idx_e,
        $.if_e,
        $.block_e,
        $.bin_e,
        $.intrinsic_e
      ),

      // Declarations
      set_var: $ => $.lower_ident,
      set_array_idx: $ => seq($.lower_ident, '[', $._expr, ']'),
      set_struct_idx: $ => seq($.lower_ident, '.', $.lower_ident),
      _set_target: $ => choice(
        $.set_var,
        $.set_array_idx,
        $.set_struct_idx
      ),

      let_decl: $ => seq('let', $.lower_ident, '=', $._expr),
      set_decl: $ => seq('set', $._set_target, '=', $._expr),
      while_decl: $ => seq('while', $._expr, $.block_e),
      expr_decl: $ => $._expr,

      _decl: $ => choice(
        $.let_decl,
        $.set_decl,
        $.while_decl,
        $.expr_decl
      ),

      // Types
      ty_i32: $ => 'i32',
      ty_f32: $ => 'f32',
      ty_bool: $ => 'bool',
      ty_unit: $ => 'unit',
      ty_array: $ => seq('[', $._type ,']'),
      ty_struct: $ => $.upper_ident,

      _type: $ => choice(
        $.ty_i32,
        $.ty_f32,
        $.ty_bool,
        $.ty_unit,
        $.ty_array,
        $.ty_struct
      ),

      func_type: $ => seq('(', comma_sep_trailing($._type) ,')', '->', $._type),

      // Toplevel

      top_let: $ => seq(
        'let', 
        $.lower_ident, 
        optional(seq(':', $._type)), 
        '=', 
        $._expr,
        ';'
      ),

      top_import: $ => seq(
        'import',
        $.lower_ident,
        seq(':', $.func_type),
        'from',
        $.lower_ident,
        ';'
      ),

      func_param: $ => seq($.lower_ident, ':', $._type),
      func_params: $ => seq('(', comma_sep_trailing($.func_param), ')'),
      top_func: $ => seq(
        'fn',
        $.lower_ident,
        $.func_params,
        optional(seq(':', $._type)),
        '=',
        $.block_e
      ),

      struct_field_top: $ => seq($.lower_ident, ':', $._type),
      top_struct: $ => seq(
        'struct',
        $.upper_ident,
        '{',
        comma_sep_trailing($.struct_field_top),
        '}'
      ),

      _toplevel: $ => choice(
        $.top_let,
        $.top_import,
        $.top_func,
        $.top_struct
      )
    }
  });
  