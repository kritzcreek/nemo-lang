use frontend::ir::{
    Declaration, DeclarationData, Expr, ExprData, MatchBranch, Program, SetTarget, SetTargetData,
};

fn set_target_size(st: &SetTarget) -> u32 {
    match &st.it {
        SetTargetData::SetArray { target, index } => expr_size(target) + expr_size(index),
        SetTargetData::SetStruct { target, .. } => expr_size(target) + 1,
        SetTargetData::SetVar { .. } => 1,
    }
}
fn decl_size(d: &Declaration) -> u32 {
    match &d.it {
        DeclarationData::Let { expr, .. } => expr_size(expr),
        DeclarationData::Set { set_target, expr } => set_target_size(set_target) + expr_size(expr),
        DeclarationData::Expr { expr } => expr_size(expr),
        DeclarationData::While { condition, body } => expr_size(condition) + expr_size(body) + 1,
    }
}
fn branch_size(d: &MatchBranch) -> u32 {
    // TODO: Might need to compute pattern size
    expr_size(&d.body)
}

fn expr_size(e: &Expr) -> u32 {
    let size: u32 = match e.it.as_ref() {
        ExprData::Lit { .. } => 1,
        ExprData::Var { .. } => 1,
        ExprData::Call { arguments, .. } => arguments.iter().map(expr_size).sum::<u32>() + 1,
        ExprData::Unary { expr, .. } => expr_size(expr) + 1,
        ExprData::Binary { left, right, .. } => expr_size(left) + expr_size(right) + 1,
        ExprData::Array { elems } => elems.iter().map(expr_size).sum::<u32>() + 1,
        ExprData::ArrayIdx { array, index } => expr_size(array) + expr_size(index) + 1,
        ExprData::If {
            condition,
            then_branch,
            else_branch,
        } => expr_size(condition) + expr_size(then_branch) + expr_size(else_branch) + 1,
        ExprData::Block { declarations, expr } => {
            declarations.iter().map(decl_size).sum::<u32>() + expr_size(expr)
        }
        ExprData::Struct { fields, .. } => {
            fields.iter().map(|(_, expr)| expr_size(expr)).sum::<u32>() + 1
        }
        ExprData::StructIdx { expr, .. } => expr_size(expr) + 1,
        ExprData::Return { expr } => expr_size(expr) + 1,
        ExprData::Match {
            scrutinee,
            branches,
        } => expr_size(scrutinee) + branches.iter().map(branch_size).sum::<u32>(),
        ExprData::Lambda { body, .. } => expr_size(body),
    };
    e.size.set(size).expect("double-set size for {expr:?}");
    size
}

pub fn compute_sizes(p: &Program) {
    for global in &p.globals {
        expr_size(&global.init);
    }
    for func in &p.funcs {
        expr_size(&func.body);
    }
}
