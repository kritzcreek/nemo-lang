function comma_sep_trailing(rule) {
  return seq(repeat(seq(rule, ',')), optional(rule))
}

function semi_sep_trailing(rule) {
  return seq(repeat(seq(rule, ';')), optional(rule))
}

const binary_ops = [
  ['&&'],
  ['||'],
  ['==', '!=', '<', '<=', '>', '>='],
  ['+', '-'],
  ['*', '/'],
];

function make_binary_rules(expr) {
  let p = 0;
  const levels = binary_ops.map(ops =>
    prec.left(++p, seq(
      field('left', expr),
      field('op', choice(...ops)),
      field('right', expr)
    ))
  );
  return choice(...levels)
}

module.exports = grammar({
  name: 'nemo',
  word: $ => $.lower_ident,
  extras: $ => [/\s+/, $.comment],

  rules: {
    source_file: $ => repeat($._toplevel),

    comment: $ => seq('//', /[^\n]*/),

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
    array_idx_e: $ => prec(100, seq(
      field('array', $._expr),
      '[', field('index', $._expr), ']'
    )),

    struct_field_e: $ => seq(
      field('name', $.lower_ident),
      '=',
      field('expr', $._expr)
    ),
    struct_e: $ => seq(
      field('struct', $.upper_ident),
      '{', comma_sep_trailing($.struct_field_e), '}'
    ),
    // Needs to be higher than the highest operator precedence
    struct_idx_e: ($) => prec(100, seq(
      field('expr', $._expr),
      ".",
      field('index', $.lower_ident)
    )),
    if_e: $ => seq(
      'if',
      field('condition', $._expr),
      field('then', $.block_e),
      'else',
      field('else', $.block_e)
    ),


    call_args: $ => seq('(', comma_sep_trailing($._expr), ')'),
    call_e: $ => seq(
      field('function', $._callee),
      field('arguments', $.call_args)
    ),
    block_e: $ => seq(
      '{',
       semi_sep_trailing(field('block_decl', $._decl)),
      '}'
    ),

    intrinsic_e: $ => seq(
      field('function', $.intrinsic_ident),
      field('arguments', $.call_args)
    ),
    binary_e: $ => make_binary_rules($._expr),

    parenthesized_e: $ => seq('(', field ('expr', $._expr), ')'),

    _callee: $ => choice(
      $._lit,
      $._call_or_var,
      $.parenthesized_e,
      $.array_e,
      $.struct_e,
      $.if_e,
      $.intrinsic_e,
      $.array_idx_e,
      $.struct_idx_e,
    ),

    // Extracted this rule to make it clear that we want the parser to greedily parse:
    // my_func(args) into `(call_e ident args)` and not `(var_e ERROR)`
    _call_or_var: $ => choice($.call_e, $.var_e),

    _expr: $ => choice(
      $._lit,
      $._call_or_var,
      $.parenthesized_e,
      $.array_e,
      $.struct_e,
      $.if_e,
      $.block_e,
      $.binary_e,
      $.intrinsic_e,
      $.array_idx_e,
      $.struct_idx_e,
    ),

    // Declarations
    set_var: $ => field('name', $.lower_ident),
    set_array_idx: $ => seq(
      field('target', $._set_target),
      '[', field('index', $._expr), ']'
    ),
    set_struct_idx: $ => seq(
      field('target', $._set_target),
      '.', field('index', $.lower_ident)
    ),
    _set_target: $ => choice(
      $.set_var,
      $.set_array_idx,
      $.set_struct_idx
    ),

    let_decl: $ => seq(
      'let',
      field('binder', $.lower_ident),
      optional(seq(':', field('annotation', $._type))),
      '=',
      field('expr', $._expr)
    ),
    set_decl: $ => seq(
      'set',
      field('target', $._set_target),
      '=',
      field('expr', $._expr)
    ),
    while_decl: $ => seq(
      'while',
      field('condition', $._expr),
      field('body', $.block_e)
    ),
    expr_decl: $ => field('expr', $._expr),

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
    ty_array: $ => seq('[', field('elem_ty', $._type) ,']'),
    ty_struct: $ => $.upper_ident,
    ty_func: $ => seq(
      'fn',
      '(',
      comma_sep_trailing(field('argument', $._type)),
      ')',
      '->',
      field('result', $._type)
    ),
    _type: $ => choice(
      $.ty_i32,
      $.ty_f32,
      $.ty_bool,
      $.ty_unit,
      $.ty_array,
      $.ty_struct,
      $.ty_func
    ),

    func_type: $ => seq(
      '(',
      comma_sep_trailing(field('argument', $._type))
      ,')',
      '->',
      field('result', $._type)
    ),

    // Toplevel

    top_let: $ => seq(
      'let',
      field('binder', $.lower_ident),
      optional(seq(':', field('annotation', $._type))),
      '=',
      field('expr', $._expr),
      ';'
    ),

    top_import: $ => seq(
      'import',
      field('internal', $.lower_ident),
      ':',
      field('type', $.func_type),
      'from',
      field('external', $.lower_ident)
    ),

    func_param: $ => seq(
      field('name', $.lower_ident),
      ':',
      field('type', $._type)
    ),
    func_params: $ => seq('(', comma_sep_trailing($.func_param), ')'),
    top_func: $ => seq(
      'fn',
      field('name', $.lower_ident),
      field('params', $.func_params),
      optional(seq(':', field('result', $._type))),
      '=',
      field('body', $.block_e)
    ),

    struct_field_top: $ => seq(
      field('name', $.lower_ident),
      ':',
      field('type', $._type)
    ),
    top_struct: $ => seq(
      'struct',
      field('name', $.upper_ident),
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
