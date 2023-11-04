use crate::syntax::{Expr, Lit, Op, Span, Toplevel, Ty, Typed, TypedExpr};
use tree_sitter::{Node, Parser};
use tree_sitter_nemo;

type Ctx = ();

pub struct Typechecker<'a> {
    source: &'a [u8],
}

impl<'a> Typechecker<'a> {
    pub fn new(source: &'a str) -> Typechecker<'a> {
        Typechecker {
            source: source.as_bytes(),
        }
    }

    fn text(&self, node: Node<'_>) -> &str {
        node.utf8_text(self.source).unwrap()
    }

    pub fn infer_top_let(&self, node: Node<'_>) -> Option<Toplevel> {
        assert!(node.kind() == "top_let");
        let binder = node.child_by_field_name("binder")?;
        // let ty = node.child_by_field_name("ty");
        let expr_node = node.child_by_field_name("expr")?;
        let typed_expr = self.infer_expr(&(), expr_node)?;
        let typed_binder = Typed {
            ty: typed_expr.ty.clone(),
            at: Span::SYN,
            it: self.text(binder).to_string(),
        };

        Some(Toplevel::TopLet {
            binder: typed_binder,
            expr: typed_expr,
        })
    }

    pub fn infer_expr(&self, ctx: &Ctx, node: Node<'_>) -> Option<TypedExpr> {
        let at = Span::SYN;
        match node.kind() {
            "int_lit" => {
                let lit = Lit::I32(self.text(node).parse().ok()?);
                Some(Typed {
                    ty: Ty::I32,
                    at,
                    it: Expr::Lit(lit),
                })
            }
            "float_lit" => {
                let lit = Lit::F32(self.text(node).parse().ok()?);
                Some(Typed {
                    ty: Ty::F32,
                    at,
                    it: Expr::Lit(lit),
                })
            }
            "bool_lit" => {
                let lit = Lit::Bool(self.text(node).parse().ok()?);
                Some(Typed {
                    ty: Ty::Bool,
                    at,
                    it: Expr::Lit(lit),
                })
            }
            "binary_e" => {
                let left_node = node.child_by_field_name("left")?;
                let left = self.infer_expr(ctx, left_node)?;

                let right_node = node.child_by_field_name("right")?;
                let right = self.infer_expr(ctx, right_node)?;

                let op_node = node.child_by_field_name("op")?;
                let op: Op = self.text(op_node).parse().ok()?;

                None
            }
            _ => None,
        }
    }
}
