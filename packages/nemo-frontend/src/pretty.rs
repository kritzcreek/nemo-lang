use pretty::RcDoc;

use crate::syntax::{
    Declaration, DeclarationData, Expr, ExprData, FuncType, Intrinsic, IntrinsicData, Lit, LitData,
    Op, OpData, Program, SetTarget, SetTargetData, Toplevel, ToplevelData, Type, TypeData,
};
use crate::types::Ty;

type Doc<'a> = RcDoc<'a, ()>;

pub struct Printer {
    show_let_types: bool,
}

impl Printer {
    pub fn new(show_let_types: bool) -> Printer {
        Printer { show_let_types }
    }

    fn pretty_op(&self, op: &Op) -> Doc {
        match op.it {
            OpData::Add => Doc::text("+"),
            OpData::Sub => Doc::text("-"),
            OpData::Mul => Doc::text("*"),
            OpData::Div => Doc::text("/"),
            OpData::Lt => Doc::text("<"),
            OpData::Le => Doc::text("<="),
            OpData::Gt => Doc::text(">"),
            OpData::Ge => Doc::text(">="),
            OpData::Eq => Doc::text("=="),
            OpData::Ne => Doc::text("!="),
            OpData::And => Doc::text("&&"),
            OpData::Or => Doc::text("||"),
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

    fn pretty_type(&self, ty: &Type) -> Doc {
        match *ty.it {
            TypeData::I32 => Doc::text("i32"),
            TypeData::F32 => Doc::text("f32"),
            TypeData::Bool => Doc::text("bool"),
            TypeData::Unit => Doc::text("unit"),
            TypeData::Array(ref t) => Doc::text("[")
                .append(self.pretty_type(t))
                .append(Doc::text("]")),
            TypeData::Struct(ref t) => Doc::text(t.to_string()),
        }
    }

    // TODO: handle indentation
    fn pretty_func_type(&self, func_ty: &FuncType) -> Doc {
        let mut doc = Doc::text("(");
        for (i, arg) in func_ty.it.arguments.iter().enumerate() {
            if i > 0 {
                doc = doc.append(Doc::text(", "));
            }
            doc = doc.append(self.pretty_type(arg));
        }
        doc = doc
            .append(Doc::text(") -> "))
            .append(self.pretty_type(&func_ty.it.result));
        doc
    }

    fn pretty_lit(&self, lit: &Lit) -> Doc {
        match lit.it {
            LitData::Bool(b) => Doc::text(b.to_string()),
            LitData::I32(i) => Doc::text(i.to_string()),
            LitData::F32(f) => Doc::text(f.to_string()),
        }
    }

    fn pretty_expr_list(&self, exprs: &[Expr]) -> Doc {
        Doc::intersperse(
            exprs.iter().map(|e| self.pretty_expr(e)),
            Doc::text(",").append(Doc::line()),
        )
        .nest(2)
        .group()
    }

    fn pretty_expr(&self, expr: &Expr) -> Doc {
        match *expr.it {
            ExprData::Lit(ref l) => self.pretty_lit(l),
            ExprData::Var(ref v) => Doc::text(v.it.to_string()),
            ExprData::Call {
                ref func,
                ref arguments,
            } => Doc::text(func.it.to_string())
                .append(Doc::text("("))
                .append(self.pretty_expr_list(arguments))
                .append(")"),
            ExprData::Binary {
                ref op,
                ref left,
                ref right,
            } => self
                .pretty_expr(left)
                .append(Doc::space())
                .append(self.pretty_op(op))
                .append(Doc::space())
                .append(self.pretty_expr(right)),
            ExprData::Array(ref elements) => Doc::text("[")
                .append(Doc::line())
                .append(self.pretty_expr_list(elements))
                .append(Doc::line())
                .append(Doc::text("]"))
                .group(),
            ExprData::ArrayIdx {
                ref array,
                ref index,
            } => self
                .pretty_expr(array)
                .append(Doc::text("["))
                .append(self.pretty_expr(index))
                .append(Doc::text("]")),
            ExprData::If {
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
            ExprData::Block { ref declarations } => {
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
                        .append(Doc::hardline())
                        .nest(2)
                        .append(Doc::text("}"))
                }
            }
            ExprData::Struct {
                ref name,
                ref fields,
            } => {
                let fields = Doc::intersperse(
                    fields.iter().map(|(name, expr)| {
                        Doc::text(name.it.to_string())
                            .append(Doc::text(" = "))
                            .append(self.pretty_expr(expr))
                    }),
                    Doc::text(",").append(Doc::line()),
                );
                Doc::text(name.it.to_string()).append(Doc::space()).append(
                    Doc::text("{")
                        .append(Doc::line())
                        .append(fields)
                        .append(Doc::line())
                        .nest(2)
                        .append(Doc::text("}"))
                        .group(),
                )
            }
            ExprData::StructIdx {
                ref expr,
                ref index,
            } => self
                .pretty_expr(expr)
                .append(Doc::text("."))
                .append(index.it.to_string()),
            ExprData::Intrinsic {
                ref intrinsic,
                ref arguments,
            } => self
                .pretty_intrinsic(intrinsic)
                .append(Doc::text("("))
                .append(self.pretty_expr_list(arguments))
                .append(Doc::text(")")),
        }
    }

    fn pretty_intrinsic(&self, intrinsic: &Intrinsic) -> Doc {
        match intrinsic.it {
            IntrinsicData::ArrayLen => Doc::text("@array_len"),
            IntrinsicData::ArrayNew => Doc::text("@array_new"),
        }
    }

    fn pretty_set_target(&self, set_target: &SetTarget) -> Doc {
        match *set_target.it {
            SetTargetData::Var { ref name } => Doc::text(name.it.to_string()),
            SetTargetData::Array {
                ref target,
                ref index,
            } => self
                .pretty_set_target(target)
                .append(Doc::text("["))
                .append(self.pretty_expr(index))
                .append(Doc::text("]")),
            SetTargetData::Struct {
                ref target,
                ref index,
            } => self
                .pretty_set_target(target)
                .append(Doc::text("."))
                .append(index.it.to_string()),
        }
    }

    fn pretty_decl(&self, decl: &Declaration) -> Doc {
        match decl.it {
            DeclarationData::Expr(ref e) => self.pretty_expr(e),
            DeclarationData::Let {
                ref binder,
                annotation: _,
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
                .append(self.pretty_expr(expr).nest(1))
                .group(),
            DeclarationData::Set {
                ref set_target,
                ref expr,
            } => Doc::text("set ")
                .append(self.pretty_set_target(set_target))
                .append(if self.show_let_types {
                    Doc::text(" : ").append(self.pretty_ty(&set_target.ty))
                } else {
                    Doc::nil()
                })
                .append(Doc::space())
                .append(Doc::text("="))
                .append(Doc::line())
                .append(self.pretty_expr(expr).nest(2))
                .group(),
            DeclarationData::While {
                ref condition,
                ref body,
            } => Doc::text("while")
                .append(Doc::space())
                .append(self.pretty_expr(condition))
                .append(Doc::space())
                .append(self.pretty_expr(body)),
        }
    }

    fn pretty_return_ty(&self, return_ty: &Option<Type>) -> Doc {
        match return_ty {
            Some(ty) => Doc::text(" : ")
                .append(self.pretty_type(ty))
                .append(Doc::space()),
            None => Doc::space(),
        }
    }

    fn pretty_toplevel(&self, toplevel: &Toplevel) -> Doc {
        match toplevel.it {
            ToplevelData::Import {
                ref internal,
                ref func_ty,
                ref external,
            } => Doc::text("import ")
                .append(Doc::text(internal.it.to_string()))
                .append(Doc::text(" : "))
                .append(self.pretty_func_type(func_ty))
                .append(Doc::text(" from "))
                .append(Doc::text(external.it.to_string())),
            ToplevelData::Global {
                ref binder,
                annotation: _,
                ref init,
            } => Doc::text("let ")
                .append(Doc::text(binder.it.to_string()))
                .append(if self.show_let_types {
                    Doc::text(" : ").append(self.pretty_ty(&init.ty))
                } else {
                    Doc::nil()
                })
                .append(Doc::space())
                .append(Doc::text("="))
                .append(Doc::softline())
                .append(self.pretty_expr(init).nest(4)),
            ToplevelData::Struct {
                ref name,
                ref fields,
            } => Doc::text("struct ")
                .append(Doc::text(name.it.to_string()))
                .append(Doc::space())
                .append(Doc::text("{"))
                .append(Doc::line())
                .append(
                    Doc::intersperse(
                        fields.iter().map(|(name, ty)| {
                            Doc::text(name.it.to_string())
                                .append(Doc::text(" : "))
                                .append(self.pretty_type(ty))
                        }),
                        Doc::text(",").append(Doc::hardline()),
                    )
                    .group(),
                )
                .nest(2)
                .append(Doc::line())
                .append(Doc::text("}")),
            ToplevelData::Func {
                ref name,
                ref params,
                ref return_ty,
                ref body,
            } => Doc::text("fn ")
                .append(Doc::text(name.it.to_string()))
                .append(Doc::text("("))
                .append(
                    Doc::intersperse(
                        params.iter().map(|(name, ty)| {
                            Doc::text(name.it.to_string())
                                .append(Doc::text(" : "))
                                .append(self.pretty_type(ty))
                        }),
                        Doc::text(",").append(Doc::line()),
                    )
                    .group(),
                )
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
