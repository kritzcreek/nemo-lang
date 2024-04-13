use super::{
    ast::{self, support, AstChildren, AstNode},
    SyntaxKind::{self, *},
    SyntaxNode, SyntaxToken,
};
use crate::T;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TopImport {
    pub(crate) syntax: SyntaxNode,
}
impl TopImport {
    pub fn import_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![import])
    }
    pub fn imp_internal(&self) -> Option<ImpInternal> {
        support::child(&self.syntax)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![:])
    }
    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
    pub fn from_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![from])
    }
    pub fn imp_external(&self) -> Option<ImpExternal> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TopLet {
    pub(crate) syntax: SyntaxNode,
}
impl TopLet {
    pub fn let_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![let])
    }
    pub fn ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![ident])
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![:])
    }
    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
    pub fn eq_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![=])
    }
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TopStruct {
    pub(crate) syntax: SyntaxNode,
}
impl TopStruct {
    pub fn struct_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![struct])
    }
    pub fn upper_ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![upper_ident])
    }
    pub fn l_brace_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['{'])
    }
    pub fn struct_fields(&self) -> AstChildren<StructField> {
        support::children(&self.syntax)
    }
    pub fn r_brace_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['}'])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImpInternal {
    pub(crate) syntax: SyntaxNode,
}
impl ImpInternal {
    pub fn ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![ident])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImpExternal {
    pub(crate) syntax: SyntaxNode,
}
impl ImpExternal {
    pub fn ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![ident])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub(crate) syntax: SyntaxNode,
}
impl StructField {
    pub fn ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![ident])
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![:])
    }
    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyInt {
    pub(crate) syntax: SyntaxNode,
}
impl TyInt {
    pub fn i32_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![i32])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyFloat {
    pub(crate) syntax: SyntaxNode,
}
impl TyFloat {
    pub fn f32_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![f32])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyBool {
    pub(crate) syntax: SyntaxNode,
}
impl TyBool {
    pub fn bool_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![bool])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyUnit {
    pub(crate) syntax: SyntaxNode,
}
impl TyUnit {
    pub fn unit_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![unit])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyArray {
    pub(crate) syntax: SyntaxNode,
}
impl TyArray {
    pub fn l_brack_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['['])
    }
    pub fn elem(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
    pub fn r_brack_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![']'])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyCons {
    pub(crate) syntax: SyntaxNode,
}
impl TyCons {
    pub fn upper_ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![upper_ident])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyFn {
    pub(crate) syntax: SyntaxNode,
}
impl TyFn {
    pub fn fn_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![fn])
    }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['('])
    }
    pub fn arguments(&self) -> AstChildren<Type> {
        support::children(&self.syntax)
    }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![')'])
    }
    pub fn arrow_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![->])
    }
    pub fn result(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LitBool {
    pub(crate) syntax: SyntaxNode,
}
impl LitBool {
    pub fn true_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![true])
    }
    pub fn false_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![false])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LitFloat {
    pub(crate) syntax: SyntaxNode,
}
impl LitFloat {
    pub fn float_lit_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![float_lit])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LitInt {
    pub(crate) syntax: SyntaxNode,
}
impl LitInt {
    pub fn int_lit_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![int_lit])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ELit {
    pub(crate) syntax: SyntaxNode,
}
impl ELit {
    pub fn literal(&self) -> Option<Literal> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EVar {
    pub(crate) syntax: SyntaxNode,
}
impl EVar {
    pub fn ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![ident])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TopLevel {
    TopImport(TopImport),
    TopLet(TopLet),
    TopStruct(TopStruct),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    TyInt(TyInt),
    TyFloat(TyFloat),
    TyBool(TyBool),
    TyUnit(TyUnit),
    TyArray(TyArray),
    TyCons(TyCons),
    TyFn(TyFn),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    ELit(ELit),
    EVar(EVar),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    LitBool(LitBool),
    LitFloat(LitFloat),
    LitInt(LitInt),
}
impl AstNode for TopImport {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TopImport
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for TopLet {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TopLet
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for TopStruct {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TopStruct
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for ImpInternal {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == ImpInternal
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for ImpExternal {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == ImpExternal
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for StructField {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == StructField
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for TyInt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TyInt
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for TyFloat {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TyFloat
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for TyBool {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TyBool
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for TyUnit {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TyUnit
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for TyArray {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TyArray
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for TyCons {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TyCons
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for TyFn {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TyFn
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for LitBool {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == LitBool
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for LitFloat {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == LitFloat
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for LitInt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == LitInt
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for ELit {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == ELit
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for EVar {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EVar
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl From<TopImport> for TopLevel {
    fn from(node: TopImport) -> TopLevel {
        TopLevel::TopImport(node)
    }
}
impl From<TopLet> for TopLevel {
    fn from(node: TopLet) -> TopLevel {
        TopLevel::TopLet(node)
    }
}
impl From<TopStruct> for TopLevel {
    fn from(node: TopStruct) -> TopLevel {
        TopLevel::TopStruct(node)
    }
}
impl AstNode for TopLevel {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            TopImport | TopLet | TopStruct => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            TopImport => TopLevel::TopImport(TopImport { syntax }),
            TopLet => TopLevel::TopLet(TopLet { syntax }),
            TopStruct => TopLevel::TopStruct(TopStruct { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            TopLevel::TopImport(it) => &it.syntax,
            TopLevel::TopLet(it) => &it.syntax,
            TopLevel::TopStruct(it) => &it.syntax,
        }
    }
}
impl From<TyInt> for Type {
    fn from(node: TyInt) -> Type {
        Type::TyInt(node)
    }
}
impl From<TyFloat> for Type {
    fn from(node: TyFloat) -> Type {
        Type::TyFloat(node)
    }
}
impl From<TyBool> for Type {
    fn from(node: TyBool) -> Type {
        Type::TyBool(node)
    }
}
impl From<TyUnit> for Type {
    fn from(node: TyUnit) -> Type {
        Type::TyUnit(node)
    }
}
impl From<TyArray> for Type {
    fn from(node: TyArray) -> Type {
        Type::TyArray(node)
    }
}
impl From<TyCons> for Type {
    fn from(node: TyCons) -> Type {
        Type::TyCons(node)
    }
}
impl From<TyFn> for Type {
    fn from(node: TyFn) -> Type {
        Type::TyFn(node)
    }
}
impl AstNode for Type {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            TyInt | TyFloat | TyBool | TyUnit | TyArray | TyCons | TyFn => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            TyInt => Type::TyInt(TyInt { syntax }),
            TyFloat => Type::TyFloat(TyFloat { syntax }),
            TyBool => Type::TyBool(TyBool { syntax }),
            TyUnit => Type::TyUnit(TyUnit { syntax }),
            TyArray => Type::TyArray(TyArray { syntax }),
            TyCons => Type::TyCons(TyCons { syntax }),
            TyFn => Type::TyFn(TyFn { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Type::TyInt(it) => &it.syntax,
            Type::TyFloat(it) => &it.syntax,
            Type::TyBool(it) => &it.syntax,
            Type::TyUnit(it) => &it.syntax,
            Type::TyArray(it) => &it.syntax,
            Type::TyCons(it) => &it.syntax,
            Type::TyFn(it) => &it.syntax,
        }
    }
}
impl From<ELit> for Expr {
    fn from(node: ELit) -> Expr {
        Expr::ELit(node)
    }
}
impl From<EVar> for Expr {
    fn from(node: EVar) -> Expr {
        Expr::EVar(node)
    }
}
impl AstNode for Expr {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            ELit | EVar => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            ELit => Expr::ELit(ELit { syntax }),
            EVar => Expr::EVar(EVar { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Expr::ELit(it) => &it.syntax,
            Expr::EVar(it) => &it.syntax,
        }
    }
}
impl From<LitBool> for Literal {
    fn from(node: LitBool) -> Literal {
        Literal::LitBool(node)
    }
}
impl From<LitFloat> for Literal {
    fn from(node: LitFloat) -> Literal {
        Literal::LitFloat(node)
    }
}
impl From<LitInt> for Literal {
    fn from(node: LitInt) -> Literal {
        Literal::LitInt(node)
    }
}
impl AstNode for Literal {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            LitBool | LitFloat | LitInt => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            LitBool => Literal::LitBool(LitBool { syntax }),
            LitFloat => Literal::LitFloat(LitFloat { syntax }),
            LitInt => Literal::LitInt(LitInt { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Literal::LitBool(it) => &it.syntax,
            Literal::LitFloat(it) => &it.syntax,
            Literal::LitInt(it) => &it.syntax,
        }
    }
}
impl std::fmt::Display for TopLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TopImport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TopLet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TopStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for ImpInternal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for ImpExternal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for StructField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TyInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TyFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TyBool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TyUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TyArray {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TyCons {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TyFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for LitBool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for LitFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for LitInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for ELit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for EVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}