use rowan::Checkpoint;

use super::Parser;
use crate::lexer::SyntaxKind;
use crate::T;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Progress {
    Made,
    None,
}

impl Progress {
    pub fn made_progress(&self) -> bool {
        *self == Progress::Made
    }
}

pub fn prog(p: &mut Parser) {
    while !p.at(SyntaxKind::EOF) {
        if !toplevel(p).made_progress() {
            let c = p.checkpoint();
            // RECOVERY
            p.error("expected a top-level declaration");
            while !p.at(SyntaxKind::EOF) && !TOP_LEVEL_FIRST.contains(&p.current()) {
                p.bump_any();
            }
            p.finish_at(c, SyntaxKind::Error)
        }
    }
}

const TOP_LEVEL_FIRST: [SyntaxKind; 4] = [T![global], T![fn], T![import], T![struct]];
fn toplevel(p: &mut Parser) -> Progress {
    match p.current() {
        T![global] => {
            let c = p.checkpoint();
            p.bump(T![global]);
            p.expect(SyntaxKind::IDENT);
            typ_annot(p);
            p.expect(SyntaxKind::EQUALS);
            if !expr(p).made_progress() {
                p.error("expected an expression")
            }
            p.finish_at(c, SyntaxKind::TopGlobal);
            Progress::Made
        }
        T![fn] => {
            top_fn(p);
            Progress::Made
        }
        T![import] => {
            top_import(p);
            Progress::Made
        }
        T![struct] => {
            top_struct(p);
            Progress::Made
        }
        T![variant] => {
            top_variant(p);
            Progress::Made
        }
        _ => Progress::None,
    }
}

fn top_import(p: &mut Parser) {
    let c = p.checkpoint();
    p.bump(T![import]);
    p.start_node(SyntaxKind::ImpInternal);
    p.expect(T![ident]);
    p.finish_node();
    p.expect(T![:]);
    if !typ(p).made_progress() {
        p.error("expected a function type")
    }
    p.expect(T![from]);
    p.start_node(SyntaxKind::ImpExternal);
    p.expect(T![ident]);
    p.finish_node();
    p.finish_at(c, SyntaxKind::TopImport)
}

fn top_struct(p: &mut Parser) {
    let c = p.checkpoint();
    p.bump(T![struct]);
    p.expect(T![upper_ident]);
    p.expect(T!['{']);
    while !p.at(SyntaxKind::EOF) && !p.at(T!['}']) {
        let c = p.checkpoint();
        p.expect(T![ident]);
        p.expect(T![:]);
        if !typ(p).made_progress() {
            p.error("expected a type")
        }
        p.finish_at(c, SyntaxKind::StructField);

        if !p.at(T!['}']) && !p.expect(T![,]) {
            break;
        }
    }
    p.expect(T!['}']);
    p.finish_at(c, SyntaxKind::TopStruct)
}

fn top_variant(p: &mut Parser) {
    let c = p.checkpoint();
    p.bump(T![variant]);
    p.expect(T![upper_ident]);
    p.expect(T!['{']);
    while !p.at(SyntaxKind::EOF) && !p.at(T!['}']) {
        if !p.at(T![struct]) {
            p.error("expected a struct");
            break;
        }
        top_struct(p);
        if !p.at(T!['}']) && !p.expect(T![,]) {
            break;
        }
    }
    p.expect(T!['}']);
    p.finish_at(c, SyntaxKind::TopVariant)
}

fn top_fn(p: &mut Parser) {
    let c = p.checkpoint();
    p.bump(SyntaxKind::FN_KW);
    if !p.eat(SyntaxKind::IDENT) {
        p.error("expected a function name")
    }
    param_list(p);
    if p.eat(T![->]) && !typ(p).made_progress() {
        p.error("expected a return type")
    }
    if !block_expr(p).made_progress() {
        p.error("expected a function body")
    }
    p.finish_at(c, SyntaxKind::TopFn)
}

fn param_list(p: &mut Parser) {
    if !p.eat(SyntaxKind::L_PAREN) {
        p.error("expected a parameter list");
        return;
    }
    while p.at(SyntaxKind::IDENT) {
        let c = p.checkpoint();
        p.bump(SyntaxKind::IDENT);
        if !typ_annot(p).made_progress() {
            p.error("expected a type annotation")
        }
        p.eat(SyntaxKind::COMMA);
        p.finish_at(c, SyntaxKind::Param)
    }
    if !p.eat(SyntaxKind::R_PAREN) {
        p.error("expected a closing paren");
        // TODO recover
    }
}

fn qualifier(p: &mut Parser) -> Progress {
    if p.at(T![upper_ident]) && p.nth_at(1, T![::]) {
        let c = p.checkpoint();
        p.bump(T![upper_ident]);
        p.bump(T![::]);
        p.finish_at(c, SyntaxKind::Qualifier);
        Progress::Made
    } else {
        Progress::None
    }
}

fn typ_annot(p: &mut Parser) -> Progress {
    if !p.eat(SyntaxKind::COLON) {
        return Progress::None;
    }
    if !typ(p).made_progress() {
        p.error("expected a type")
    };
    Progress::Made
}

fn typ(p: &mut Parser) -> Progress {
    let c = p.checkpoint();
    match p.current() {
        T![i32] => {
            p.bump(T![i32]);
            p.finish_at(c, SyntaxKind::TyInt)
        }
        T![f32] => {
            p.bump(T![f32]);
            p.finish_at(c, SyntaxKind::TyFloat)
        }
        T![bool] => {
            p.bump(T![bool]);
            p.finish_at(c, SyntaxKind::TyBool)
        }
        T![unit] => {
            p.bump(T![unit]);
            p.finish_at(c, SyntaxKind::TyUnit)
        }
        T![upper_ident] => {
            qualifier(p);
            p.expect(T![upper_ident]);
            p.finish_at(c, SyntaxKind::TyCons)
        }
        T!['['] => {
            p.bump(T!['[']);
            if !typ(p).made_progress() {
                p.error("expected a type")
            }
            p.expect(T![']']);
            p.finish_at(c, SyntaxKind::TyArray)
        }
        T![fn] => {
            p.bump(T![fn]);
            let c_arg = p.checkpoint();
            p.expect(T!['(']);
            while !p.at(SyntaxKind::EOF) && !p.at(T![')']) {
                typ(p);
                if !p.at(T![')']) && !p.expect(T![,]) {
                    break;
                }
            }
            p.expect(T![')']);
            p.finish_at(c_arg, SyntaxKind::TyArgList);
            p.expect(T![->]);
            if !typ(p).made_progress() {
                p.error("expected a return type")
            }
            p.finish_at(c, SyntaxKind::TyFn)
        }
        _ => return Progress::None,
    }

    Progress::Made
}

// const EXPR_FIRST: [SyntaxKind; 11] = [
//     T![true],
//     T![false],
//     T![int_lit],
//     T![float_lit],
//     T![ident],
//     T![upper_ident],
//     T!['('],
//     T![if],
//     T!['{'],
//     T!['['],
//     T![at_ident],
// ];

fn lit(p: &mut Parser) -> Progress {
    let c = p.checkpoint();
    match p.current() {
        T![true] | T![false] => {
            p.bump_any();
            p.finish_at(c, SyntaxKind::LitBool)
        }
        T![int_lit] | T![binary_lit] | T![hex_lit] => {
            p.bump_any();
            p.finish_at(c, SyntaxKind::LitInt)
        }
        T![float_lit] => {
            p.bump_any();
            p.finish_at(c, SyntaxKind::LitFloat)
        }
        _ => return Progress::None,
    }

    Progress::Made
}

fn expr_bp(p: &mut Parser, min_bp: u32) -> Progress {
    let c = p.checkpoint();
    if !atom(p).made_progress() {
        return Progress::None;
    }
    postfix_expr(p, c);

    loop {
        let Some((op_bp, op)) = current_bin_op(p) else {
            break;
        };

        if op_bp < min_bp {
            break;
        }

        p.start_node(SyntaxKind::BinOp);
        p.bump(op);
        p.finish_node();
        if !expr_bp(p, op_bp).made_progress() {
            p.error("expected an expression");
        }
        p.finish_at(c, SyntaxKind::EBinary)
    }

    Progress::Made
}

fn current_bin_op(p: &mut Parser) -> Option<(u32, SyntaxKind)> {
    let c = p.current();
    match c {
        T![&&] => Some((1, c)),
        T![||] => Some((2, c)),
        T![==] | T![!=] | T![<] | T![>] | T![<=] | T![>=] => Some((3, c)),
        T![+] | T![-] => Some((4, c)),
        T![*] | T![/] => Some((5, c)),
        _ => None,
    }
}

fn expr(p: &mut Parser) -> Progress {
    if p.at(T![if]) {
        if_expr(p);
        return Progress::Made;
    }
    if p.at(T!['{']) {
        return block_expr(p);
    }
    if p.at(T![match]) {
        match_expr(p);
        return Progress::Made;
    }
    expr_bp(p, 0)
}

fn if_expr(p: &mut Parser) {
    let c = p.checkpoint();
    p.bump(T![if]);
    if !expr(p).made_progress() {
        p.error("expected a condition")
    }
    if !block_expr(p).made_progress() {
        p.error("expected a block as the then branch")
    }
    p.expect(T![else]);
    if !block_expr(p).made_progress() {
        p.error("expected a block as the else branch")
    }
    p.finish_at(c, SyntaxKind::EIf)
}

fn block_expr(p: &mut Parser) -> Progress {
    let c = p.checkpoint();
    if !p.eat(T!['{']) {
        return Progress::None;
    }
    while !p.at(SyntaxKind::EOF) && !p.at(T!['}']) {
        if !decl(p).made_progress() {
            break;
        }

        if !p.at(T!['}']) && !p.expect(T![;]) {
            break;
        }
    }
    p.expect(T!['}']);
    p.finish_at(c, SyntaxKind::EBlock);
    Progress::Made
}

fn match_expr(p: &mut Parser) {
    let c = p.checkpoint();
    p.bump(T![match]);
    expr_bp(p, 0);
    p.expect(T!['{']);
    while !p.at(SyntaxKind::EOF) && !p.at(T!['}']) {
        if !match_branch(p).made_progress() {
            break;
        }
        if !p.at(T!['}']) && !p.expect(T![,]) {
            break;
        }
    }
    p.expect(T!['}']);
    p.finish_at(c, SyntaxKind::EMatch)
}

fn match_branch(p: &mut Parser) -> Progress {
    let c = p.checkpoint();
    if !pattern(p).made_progress() {
        return Progress::None;
    }
    p.expect(T![=>]);
    if !block_expr(p).made_progress() {
        p.error("expected a body for this branch")
    }
    p.finish_at(c, SyntaxKind::EMatchBranch);
    Progress::Made
}

fn pattern(p: &mut Parser) -> Progress {
    let c = p.checkpoint();
    match p.current() {
        T![ident] => {
            p.bump(T![ident]);
            p.finish_at(c, SyntaxKind::PatVar);
            Progress::Made
        }
        T![upper_ident] => {
            qualifier(p);
            p.expect(T![upper_ident]);
            p.expect(T![ident]);
            p.finish_at(c, SyntaxKind::PatVariant);
            Progress::Made
        }
        _ => Progress::None,
    }
}

fn call_arg_list(p: &mut Parser) {
    let c = p.checkpoint();
    p.bump(T!['(']);
    while !p.at(SyntaxKind::EOF) && !p.at(T![')']) {
        if !expr(p).made_progress() {
            break;
        }

        if !p.at(T![')']) && !p.expect(T![,]) {
            break;
        }
    }
    p.expect(T![')']);
    p.finish_at(c, SyntaxKind::EArgList);
}

fn postfix_expr(p: &mut Parser, c: Checkpoint) {
    loop {
        match p.current() {
            T!['('] => {
                call_arg_list(p);
                p.finish_at(c, SyntaxKind::ECall)
            }
            T!['['] => {
                p.bump(T!['[']);
                if !expr(p).made_progress() {
                    p.error("expected an index")
                }
                p.expect(T![']']);
                p.finish_at(c, SyntaxKind::EArrayIdx)
            }
            T![.] => {
                p.bump(T![.]);
                if !p.eat(T![ident]) {
                    p.error("expected a field index")
                }
                p.finish_at(c, SyntaxKind::EStructIdx)
            }
            _ => break,
        }
    }
}

fn atom(p: &mut Parser) -> Progress {
    let c = p.checkpoint();

    if lit(p).made_progress() {
        p.finish_at(c, SyntaxKind::ELit);
        return Progress::Made;
    }

    match p.current() {
        T!['('] => {
            p.bump(T!['(']);
            expr(p);
            p.expect(T![')']);
            p.finish_at(c, SyntaxKind::EParen);
        }
        T!['['] => {
            p.bump(T!['[']);
            while !p.at(SyntaxKind::EOF) && !p.at(T![']']) {
                if !expr(p).made_progress() {
                    break;
                }

                if !p.at(T![']']) && !p.expect(T![,]) {
                    break;
                }
            }

            p.expect(T![']']);
            p.finish_at(c, SyntaxKind::EArray)
        }
        T![upper_ident] => {
            qualifier(p);
            p.expect(T![upper_ident]);
            if p.expect(T!['{']) {
                while !p.at(SyntaxKind::EOF) && !p.at(T!['}']) {
                    let c = p.checkpoint();
                    p.expect(T![ident]);
                    p.expect(T![=]);
                    if !expr(p).made_progress() {
                        p.error("expected a field expression")
                    }
                    p.finish_at(c, SyntaxKind::EStructField);

                    if !p.at(T!['}']) && !p.expect(T![,]) {
                        break;
                    }
                }
                p.expect(T!['}']);
                p.finish_at(c, SyntaxKind::EStruct)
            }
        }
        T![ident] => {
            p.bump(T![ident]);
            p.finish_at(c, SyntaxKind::EVar)
        }
        T![at_ident] => {
            p.bump(T![at_ident]);
            if p.at(T!['(']) {
                call_arg_list(p);
                p.finish_at(c, SyntaxKind::EIntrinsic)
            } else {
                p.error("expected an open parenthesis");
                p.finish_at(c, SyntaxKind::Error)
            }
        }
        _ => return Progress::None,
    }

    Progress::Made
}

fn decl(p: &mut Parser) -> Progress {
    match p.current() {
        T![let] => let_decl(p),
        T![set] => set_decl(p),
        T![while] => while_decl(p),
        _ => {
            let c = p.checkpoint();
            if expr(p).made_progress() {
                p.finish_at(c, SyntaxKind::DExpr)
            } else {
                return Progress::None;
            }
        }
    }

    Progress::Made
}

fn let_decl(p: &mut Parser) {
    let c = p.checkpoint();
    p.bump(T![let]);
    p.expect(T![ident]);
    typ_annot(p);
    p.expect(T![=]);
    if !expr(p).made_progress() {
        p.error("expected an expression")
    }
    p.finish_at(c, SyntaxKind::DLet)
}

fn set_decl(p: &mut Parser) {
    let c = p.checkpoint();
    p.bump(T![set]);
    set_target(p);
    p.expect(T![=]);
    if !expr(p).made_progress() {
        p.error("expected an expression")
    }
    p.finish_at(c, SyntaxKind::DSet)
}

fn set_target(p: &mut Parser) {
    let c = p.checkpoint();
    if p.expect(T![ident]) {
        p.finish_at(c, SyntaxKind::EVar)
    }
    while !p.at(SyntaxKind::EOF) && !p.at(T![=]) {
        match p.current() {
            T![.] | T!['['] => postfix_expr(p, c),
            _ => {
                p.error("expected a set target");
                break;
            }
        }
    }
    p.finish_at(c, SyntaxKind::SetTarget)
}

fn while_decl(p: &mut Parser) {
    let c = p.checkpoint();
    p.bump(T![while]);
    if !expr(p).made_progress() {
        p.error("expected an expression")
    }
    if !block_expr(p).made_progress() {
        p.error("expected a while body")
    }
    p.finish_at(c, SyntaxKind::DWhile)
}
