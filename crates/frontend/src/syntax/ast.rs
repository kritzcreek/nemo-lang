use rowan::GreenNode;

use crate::syntax::{SyntaxKind, SyntaxNode, SyntaxNodeChildren, SyntaxToken};
use std::marker::PhantomData;

use super::{
    EUnary,
    nodes::{EArrayIdx, EBinary, EIf, Expr, Module, Root},
};

/// The main trait to go from untyped `SyntaxNode`  to a typed ast. The
/// conversion itself has zero runtime cost: ast and syntax nodes have exactly
/// the same representation: a pointer to the tree root and a pointer to the
/// node itself.
pub trait AstNode {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;
}

/// Like `AstNode`, but wraps tokens rather than interior nodes.
pub trait AstToken {
    fn can_cast(token: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken;

    fn text(&self) -> &str {
        self.syntax().text()
    }
}

/// An iterator over `SyntaxNode` children of a particular AST type.
#[derive(Debug, Clone)]
pub struct AstChildren<N> {
    inner: SyntaxNodeChildren,
    ph: PhantomData<N>,
}

impl<N> AstChildren<N> {
    fn new(parent: &SyntaxNode) -> Self {
        AstChildren {
            inner: parent.children(),
            ph: PhantomData,
        }
    }
}

impl<N: AstNode> Iterator for AstChildren<N> {
    type Item = N;
    fn next(&mut self) -> Option<N> {
        self.inner.find_map(N::cast)
    }
}

pub(crate) mod support {
    use super::{AstChildren, AstNode, SyntaxKind, SyntaxNode, SyntaxToken};

    pub(crate) fn child<N: AstNode>(parent: &SyntaxNode) -> Option<N> {
        parent.children().find_map(N::cast)
    }

    pub(crate) fn children<N: AstNode>(parent: &SyntaxNode) -> AstChildren<N> {
        AstChildren::new(parent)
    }

    pub(crate) fn token(parent: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
        parent
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find(|it| it.kind() == kind)
    }
}

impl Module {
    pub fn from_root(root: GreenNode) -> Module {
        Root::cast(SyntaxNode::new_root(root.clone()))
            .unwrap()
            .module()
            .unwrap()
    }
}

impl EIf {
    pub(crate) fn then_branch(&self) -> Option<Expr> {
        support::children(self.syntax()).nth(0)
    }
    // TODO: This isn't right if the then branch is missing
    pub(crate) fn else_branch(&self) -> Option<Expr> {
        support::children(self.syntax()).nth(1)
    }
}
impl EArrayIdx {
    pub(crate) fn index(&self) -> Option<Expr> {
        support::children(self.syntax()).nth(1)
    }
}
impl EUnary {
    pub(crate) fn op(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children()
            .find(|n| n.kind() == SyntaxKind::UnOp)?
            .children_with_tokens()
            .find_map(|t| {
                let tkn = t.as_token()?;
                if tkn.kind().is_whitespace() {
                    None
                } else {
                    Some(tkn.clone())
                }
            })
    }
}

impl EBinary {
    pub(crate) fn lhs(&self) -> Option<Expr> {
        support::children(self.syntax()).nth(0)
    }
    pub(crate) fn rhs(&self) -> Option<Expr> {
        support::children(self.syntax()).nth(1)
    }
    pub(crate) fn op(&self) -> Option<SyntaxToken> {
        self.syntax()
            .children()
            .find(|n| n.kind() == SyntaxKind::BinOp)?
            .children_with_tokens()
            .find_map(|t| {
                let tkn = t.as_token()?;
                if tkn.kind().is_whitespace() {
                    None
                } else {
                    Some(tkn.clone())
                }
            })
    }
}
