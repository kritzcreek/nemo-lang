use super::Parser;
use crate::lexer::SyntaxKind;
use crate::T;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Progress {
    Made,
    None,
}

pub fn prog(p: &mut Parser) {
    let c = p.checkpoint();
    loop {
        if toplevel(p) == Progress::None {
            break;
        }
    }
    p.finish_at(c, SyntaxKind::Root)
}

fn toplevel(p: &mut Parser) -> Progress {
    match p.current() {
        T![let] => {
            let c = p.checkpoint();
            p.bump(SyntaxKind::LET_KW);
            if !p.eat(SyntaxKind::IDENT) {
                p.error("expected a name")
            }
            p.finish_at(c, SyntaxKind::TopLet);
            Progress::Made
        }
        _ => Progress::None,
    }
}
