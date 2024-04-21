/// Like rowan::ast::SyntaxNodePtr, but for tokens
use text_size::TextRange;

use super::{SyntaxNode, SyntaxToken};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct SyntaxTokenPtr(pub TextRange);

impl SyntaxTokenPtr {
    pub fn new(tkn: &SyntaxToken) -> Self {
        SyntaxTokenPtr(tkn.text_range())
    }

    /// Like [`Self::try_to_node`] but panics instead of returning `None` on
    /// failure.
    pub fn to_token(&self, root: &SyntaxNode) -> SyntaxToken {
        self.try_to_token(root)
            .unwrap_or_else(|| panic!("can't resolve {self:?} with {root:?}"))
    }

    /// "Dereferences" the pointer to get the [`SyntaxToken`] it points to.
    ///
    /// Returns `None` if the node is not found, so make sure that the `root`
    /// syntax tree is equivalent to (i.e. is build from the same text from) the
    /// tree which was originally used to get this [`SyntaxTokenPtr`].
    ///
    /// Also returns `None` if `root` is not actually a root (i.e. it has a
    /// parent).
    ///
    /// The complexity is linear in the depth of the tree and logarithmic in
    /// tree width. As most trees are shallow, thinking about this as
    /// `O(log(N))` in the size of the tree is not too wrong!
    pub fn try_to_token(&self, root: &SyntaxNode) -> Option<SyntaxToken> {
        if root.parent().is_some() {
            return None;
        }
        match root.token_at_offset(self.0.start()) {
            rowan::TokenAtOffset::Single(t) if t.text_range() == self.0 => Some(t),
            _ => None,
        }
    }

    /// Returns the range of the syntax node this points to.
    pub fn text_range(&self) -> TextRange {
        self.0
    }
}
