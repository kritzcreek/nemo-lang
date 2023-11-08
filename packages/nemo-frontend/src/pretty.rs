use pretty::RcDoc;

use crate::syntax::{
    Declaration, Expr, FuncTy, Intrinsic, Lit, Op, Program, SetTarget, Spanned, Toplevel, Ty,
    Typed, TypedDeclaration, TypedExpr,
};

type Doc<'a> = RcDoc<'a, ()>;

pub struct Printer {
    show_let_types: bool,
}

impl Printer {
    pub fn new(show_let_types: bool) -> Printer {
        Printer { show_let_types }
    }

    fn pretty_op(&self, op: &Op) -> Doc {
        match op {
            Op::Add => Doc::text("+"),
            Op::Sub => Doc::text("-"),
            Op::Mul => Doc::text("*"),
            Op::Div => Doc::text("/"),
            Op::Lt => Doc::text("<"),
            Op::Le => Doc::text("<="),
            Op::Gt => Doc::text(">"),
            Op::Ge => Doc::text(">="),
            Op::Eq => Doc::text("=="),
            Op::Ne => Doc::text("!="),
            Op::And => Doc::text("&&"),
            Op::Or => Doc::text("||"),
        }
    }

    fn pretty_ty(&self, ty: &Ty) -> Doc {
        match ty {
            Ty::I32 => Doc::text("i32"),
            Ty::F32 => Doc::text("f32"),
            Ty::Bool => Doc::text("bool"),
            Ty::Unit => Doc::text("unit"),
            Ty::Array(ref t) => Doc::text("[")
                .append(self.pretty_ty(t))
                .append(Doc::text("]")),
            Ty::Struct(ref t) => Doc::text(t.to_string()),
        }
    }

    // TODO: handle indentation
    fn pretty_func_ty(&self, func_ty: &FuncTy) -> Doc {
        let mut doc = Doc::text("(");
        for (i, arg) in func_ty.arguments.iter().enumerate() {
            if i > 0 {
                doc = doc.append(Doc::text(", "));
            }
            doc = doc.append(self.pretty_ty(arg));
        }
        doc = doc
            .append(Doc::text(") -> "))
            .append(self.pretty_ty(&func_ty.result));
        doc
    }

    fn pretty_lit(&self, lit: &Lit) -> Doc {
        match lit {
            Lit::Bool(b) => Doc::text(b.to_string()),
            Lit::I32(i) => Doc::text(i.to_string()),
            Lit::F32(f) => Doc::text(f.to_string()),
        }
    }

    fn pretty_expr_list(&self, exprs: &Vec<TypedExpr>) -> Doc {
        Doc::intersperse(
            exprs.iter().map(|e| self.pretty_expr(e)),
            Doc::text(",").append(Doc::line()),
        )
        .nest(2)
        .group()
    }

    fn pretty_expr(&self, expr: &TypedExpr) -> Doc {
        match expr.it {
            Expr::Lit(ref l) => self.pretty_lit(l),
            Expr::Var(ref v) => Doc::text(v.to_string()),
            Expr::Call {
                ref func,
                func_ty: _,
                ref arguments,
            } => Doc::text(func.it.to_string())
                .append(Doc::text("("))
                .append(self.pretty_expr_list(arguments))
                .append(")"),
            Expr::Binary {
                ref op,
                ref left,
                ref right,
            } => self
                .pretty_expr(left)
                .append(Doc::space())
                .append(self.pretty_op(&op.it))
                .append(Doc::space())
                .append(self.pretty_expr(right)),
            Expr::Array(ref elements) => Doc::text("[")
                .append(Doc::line())
                .append(self.pretty_expr_list(elements))
                .append(Doc::line())
                .append(Doc::text("]")).group(),
            Expr::ArrayIdx {
                ref array,
                ref index,
            } => self
                .pretty_expr(array)
                .append(Doc::text("["))
                .append(self.pretty_expr(index))
                .append(Doc::text("]")),
            Expr::If {
                ref condition,
                ref then_branch,
                ref else_branch,
            } => Doc::text("if")
                .append(Doc::space())
                .append(self.pretty_expr(condition))
                .append(Doc::space())
                .append(self.pretty_expr(then_branch))
                .append(Doc::space())
                .append(Doc::text("else"))
                .append(Doc::space())
                .append(self.pretty_expr(else_branch)),
            Expr::Block { ref declarations } => {
                if declarations.is_empty() {
                    Doc::text("{}")
                } else {
                let decls = Doc::intersperse(
                    declarations.iter().map(|d| self.pretty_decl(d)),
                    Doc::text(";").append(Doc::hardline()),
                );
                Doc::text("{")
                    .append(Doc::hardline())
                    .append(decls.group())
                    .append(Doc::hardline()).nest(2)
                    .append(Doc::text("}"))
                }
            }
            Expr::Struct {
                ref name,
                ref fields,
            } => {
                let fields = Doc::intersperse(
                    fields.iter().map(|f| {
                        Doc::text(f.name.it.to_string())
                            .append(Doc::text(" = "))
                            .append(self.pretty_expr(&f.expr))
                    }),
                    Doc::text(",").append(Doc::line()),
                );
                Doc::text(name.it.to_string())
                .append(Doc::space())
                .append(Doc::text("{")
                  .append(Doc::line())
                  .append(fields)
                  .append(Doc::line())
                  .nest(2)
                  .append(Doc::text("}"))
                  .group())
            }
            Expr::StructIdx {
                ref expr,
                ref index,
            } => self
                .pretty_expr(expr)
                .append(Doc::text("."))
                .append(index.it.to_string()),
            Expr::Intrinsic {
                intrinsic,
                ref arguments,
            } => self
                .pretty_intrinsic(intrinsic)
                .append(Doc::text("("))
                .append(self.pretty_expr_list(arguments))
                .append(Doc::text(")")),
        }
    }

    fn pretty_intrinsic(&self, intrinsic: Intrinsic) -> Doc {
        match intrinsic {
            Intrinsic::ArrayLen => Doc::text("@array_len"),
            Intrinsic::ArrayNew => Doc::text("@array_new"),
        }
    }

    fn pretty_set_target(&self, set_target: &Typed<SetTarget>) -> Doc {
        match set_target.it {
            SetTarget::Var { ref name } => Doc::text(name.it.to_string()),
            SetTarget::Array {
                ref name,
                ref index,
            } => Doc::text(name.it.to_string())
                .append(Doc::text("["))
                .append(self.pretty_expr(index))
                .append(Doc::text("]")),
            SetTarget::Struct {
                ref name,
                ref index,
            } => Doc::text(name.it.to_string())
                .append(Doc::text("."))
                .append(index.it.to_string()),
        }
    }

    fn pretty_decl(&self, decl: &TypedDeclaration) -> Doc {
        match decl.it {
            Declaration::Expr(ref e) => self.pretty_expr(e),
            Declaration::Let {
                ref binder,
                ref expr,
            } => Doc::text("let ")
                .append(Doc::text(binder.it.to_string()))
                .append(if self.show_let_types {
                    Doc::text(" : ").append(self.pretty_ty(&expr.ty))
                } else {
                    Doc::nil()
                })
                .append(Doc::space())
                .append(Doc::text("="))
                .append(Doc::line())
                .append(self.pretty_expr(expr).nest(1)).group(),
            Declaration::Set {
                ref set_target,
                ref expr,
            } => Doc::text("set ")
                .append(self.pretty_set_target(set_target))
                .append(Doc::space())
                .append(Doc::text("="))
                .append(Doc::line())
                .append(self.pretty_expr(expr).nest(2)).group(),
            Declaration::While {
                ref condition,
                ref body,
            } => Doc::text("while")
                .append(Doc::space())
                .append(self.pretty_expr(condition))
                .append(Doc::space())
                .append(self.pretty_expr(body)),
        }
    }

    fn pretty_return_ty(&self, return_ty: &Option<Spanned<Ty>>) -> Doc {
        match return_ty {
            Some(ty) => Doc::text(" : ")
                .append(self.pretty_ty(&ty.it))
                .append(Doc::space()),
            None => Doc::space(),
        }
    }

    fn pretty_toplevel(&self, toplevel: &Toplevel) -> Doc {
        match toplevel {
            Toplevel::TopImport {
                ref internal,
                ref func_ty,
                ref external,
            } => Doc::text("import ")
                .append(Doc::text(internal.it.to_string()))
                .append(Doc::text(" : "))
                .append(self.pretty_func_ty(&func_ty.it))
                .append(Doc::text(" from "))
                .append(Doc::text(external.it.to_string())),
            Toplevel::TopLet { binder, expr } => Doc::text("let ")
                .append(Doc::text(binder.it.to_string()))
                .append(if self.show_let_types {
                    Doc::text(" : ").append(self.pretty_ty(&expr.ty))
                } else {
                    Doc::nil()
                })
                .append(Doc::space())
                .append(Doc::text("="))
                .append(Doc::softline())
                .append(self.pretty_expr(expr).nest(1)),
            Toplevel::TopStruct { name, fields } => Doc::text("struct ")
                .append(Doc::text(name.it.to_string()))
                .append(Doc::space())
                .append(Doc::text("{"))
                .append(Doc::line())
                .append(Doc::intersperse(
                    fields.iter().map(|f| {
                        Doc::text(f.name.it.to_string())
                            .append(Doc::text(" : "))
                            .append(self.pretty_ty(&f.ty.it))
                    }),
                    Doc::text(",").append(Doc::hardline()),
                ).group()).nest(2)
                .append(Doc::line())
                .append(Doc::text("}")),
            Toplevel::TopFunc {
                name,
                params,
                return_ty,
                body,
            } => Doc::text("fn ")
                .append(Doc::text(name.it.to_string()))
                .append(Doc::text("("))
                .append(Doc::intersperse(
                    params.iter().map(|f| {
                        Doc::text(f.name.it.to_string())
                            .append(Doc::text(" : "))
                            .append(self.pretty_ty(&f.ty.it))
                    }),
                    Doc::text(",").append(Doc::line()),
                ).group())
                .append(Doc::text(")"))
                .append(self.pretty_return_ty(return_ty))
                .append(self.pretty_expr(body)),
        }
    }

    fn pretty_program(&self, program: &Program) -> Doc {
        Doc::intersperse(
            program.toplevels.iter().map(|tl| self.pretty_toplevel(tl)),
            Doc::hardline().append(Doc::hardline()),
        )
    }

    pub fn print_program(&self, program: &Program) -> String {
        let doc = self.pretty_program(program);

        let mut w = Vec::new();
        doc.render(80, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}
