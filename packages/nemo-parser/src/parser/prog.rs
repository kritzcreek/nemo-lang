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
        _ => Progress::None,
    }
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

fn expr(p: &mut Parser) -> Progress {
    let c = p.checkpoint();
    if atom(p).made_progress() {
        postfix_expr(p, c);
        return Progress::Made;
    }
    Progress::None
}

fn postfix_expr(p: &mut Parser, c: Checkpoint) {
    loop {
        match p.current() {
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
