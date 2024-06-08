/// A bunch of builders for backend ir, so the type checker can continue to be
/// written in an imperative style
use crate::ir::*;
use rowan::TextRange;

pub(crate) fn var(name: Name) -> Option<ExprData> {
    Some(ExprData::Var(name))
}

pub(crate) fn var_pat(name: Name) -> Option<PatternData> {
    Some(PatternData::PatVar(name))
}

pub(crate) fn unit_lit(range: TextRange) -> Option<Expr> {
    Some(Expr {
        at: range,
        ty: Ty::Unit,
        it: Box::new(ExprData::Lit(Lit {
            at: range,
            ty: Ty::Unit,
            it: LitData::Unit,
        })),
    })
}

pub(crate) fn lit(lit: Option<Lit>) -> Option<ExprData> {
    Some(ExprData::Lit(lit?))
}

pub(crate) fn expr_decl(expr: Option<Expr>) -> Option<DeclarationData> {
    Some(DeclarationData::Expr(expr?))
}
