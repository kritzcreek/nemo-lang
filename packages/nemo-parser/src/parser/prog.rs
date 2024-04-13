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
    let c = p.checkpoint();
    while toplevel(p) == Progress::Made {}
    p.finish_at(c, SyntaxKind::Root)
}

fn toplevel(p: &mut Parser) -> Progress {
    match p.current() {
        T![let] => {
            let c = p.checkpoint();
            p.bump(SyntaxKind::LET_KW);
            p.expect(SyntaxKind::IDENT);
            typ_annot(p);
            p.expect(SyntaxKind::EQUALS);
            if !expr(p).made_progress() {
                p.error("expected an expression")
            }
            p.expect(SyntaxKind::SEMICOLON);
            p.finish_at(c, SyntaxKind::TopLet);
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
        _ => Progress::None,
    }
}

fn top_import(p: &mut Parser) {
    let c = p.checkpoint();
    p.bump(T![import]);
    p.expect(T![ident]);
    p.expect(T![:]);
    // TODO: Check for only function types
    if !typ(p).made_progress() {
        p.error("expected a function type")
    }
    p.expect(T![from]);
    p.expect(T![ident]);
    p.finish_at(c, SyntaxKind::TopImport)
}

fn top_struct(p: &mut Parser) {
    let c = p.checkpoint();
    p.bump(T![struct]);
    p.expect(T![upper_ident]);
    p.expect(T!['{']);
    while !p.at(SyntaxKind::EOF) && !p.at(T!['}']) {
        p.expect(T![ident]);
        p.expect(T![:]);
        if !typ(p).made_progress() {
            p.error("expected a type")
        }

        if !p.at(T!['}']) && !p.expect(T![,]) {
            break;
        }
    }
    p.expect(T!['}']);
    p.finish_at(c, SyntaxKind::TopStruct)
}

fn top_fn(p: &mut Parser) {
    let c = p.checkpoint();
    p.bump(SyntaxKind::FN_KW);
    if !p.eat(SyntaxKind::IDENT) {
        p.error("expected a function name")
    }
    param_list(p);
    p.finish_at(c, SyntaxKind::TopFn)
}

fn param_list(p: &mut Parser) {
    let c = p.checkpoint();
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
    p.finish_at(c, SyntaxKind::ParamList);
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
            p.finish_at(c, SyntaxKind::TyI32)
        }
        T![f32] => {
            p.bump(T![f32]);
            p.finish_at(c, SyntaxKind::TyF32)
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
            p.bump(T![upper_ident]);
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
            p.expect(T!['(']);
            while !p.at(T![')']) {
                typ(p);
                p.eat(T![,]);
            }
            p.expect(T![')']);
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

fn lit(p: &mut Parser) -> Progress {
    let c = p.checkpoint();
    match p.current() {
        T![true] | T![false] => {
            p.bump_any();
            p.finish_at(c, SyntaxKind::LitBool)
        }
        T![int_lit] => {
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
        block_expr(p);
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
    block_expr(p);
    p.expect(T![else]);
    block_expr(p);
    p.finish_at(c, SyntaxKind::EIf)
}

fn block_expr(p: &mut Parser) {
    let c = p.checkpoint();
    p.expect(T!['{']);
    while !p.at(SyntaxKind::EOF) && !p.at(T!['}']) {
        if !decl(p).made_progress() {
            break;
        }

        if !p.at(T!['}']) && !p.expect(T![;]) {
            break;
        }
    }
    p.expect(T!['}']);
    p.finish_at(c, SyntaxKind::EBlock)
}

fn postfix_expr(p: &mut Parser, c: Checkpoint) {
    loop {
        match p.current() {
            T!['('] => {
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
            p.bump(T![upper_ident]);
            if p.expect(T!['{']) {
                while !p.at(SyntaxKind::EOF) && !p.at(T!['}']) {
                    p.expect(T![ident]);
                    p.expect(T![=]);
                    if !expr(p).made_progress() {
                        p.error("expected a field expression")
                    }

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
    p.expect(T![ident]);
    while !p.at(SyntaxKind::EOF) && !p.at(T![=]) {
        match p.current() {
            T![.] => {
                let c = p.checkpoint();
                p.bump(T![.]);
                p.expect(T![ident]);
                p.finish_at(c, SyntaxKind::SetStruct)
            }
            T!['['] => {
                let c = p.checkpoint();
                p.bump(T!['[']);
                if !expr(p).made_progress() {
                    p.error("expected an expression")
                }
                p.expect(T![']']);
                p.finish_at(c, SyntaxKind::SetArray)
            }
            _ => break,
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
    block_expr(p);
    p.finish_at(c, SyntaxKind::DWhile)
}
