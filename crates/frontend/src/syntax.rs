mod ast;
#[allow(clippy::all)]
mod nodes;
pub mod token_ptr;

use crate::parser::SyntaxKind;
use num_traits::{FromPrimitive, ToPrimitive};
pub use nodes::*;
pub use ast::*;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum NemoLanguage {}

pub type SyntaxNode = rowan::SyntaxNode<NemoLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<NemoLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<NemoLanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<NemoLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<NemoLanguage>;
pub type SyntaxNodePtr = rowan::ast::SyntaxNodePtr<NemoLanguage>;

impl rowan::Language for NemoLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        Self::Kind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.to_u16().unwrap())
    }
}
