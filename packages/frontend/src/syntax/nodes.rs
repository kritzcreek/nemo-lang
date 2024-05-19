use super::{
    ast::{support, AstChildren, AstNode},
    SyntaxKind::{self, *},
    SyntaxNode, SyntaxToken,
};
use crate::T;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Root {
    pub(crate) syntax: SyntaxNode,
}
impl Root {
    pub fn top_levels(&self) -> AstChildren<TopLevel> {
        support::children(&self.syntax)
    }
}
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
pub struct TopGlobal {
    pub(crate) syntax: SyntaxNode,
}
impl TopGlobal {
    pub fn global_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![global])
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
    pub fn type_params(&self) -> AstChildren<ParamTy> {
        support::children(&self.syntax)
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
pub struct TopVariant {
    pub(crate) syntax: SyntaxNode,
}
impl TopVariant {
    pub fn variant_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![variant])
    }
    pub fn upper_ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![upper_ident])
    }
    pub fn type_params(&self) -> AstChildren<ParamTy> {
        support::children(&self.syntax)
    }
    pub fn l_brace_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['{'])
    }
    pub fn top_structs(&self) -> AstChildren<TopStruct> {
        support::children(&self.syntax)
    }
    pub fn r_brace_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['}'])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TopFn {
    pub(crate) syntax: SyntaxNode,
}
impl TopFn {
    pub fn fn_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![fn])
    }
    pub fn ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![ident])
    }
    pub fn param_tys(&self) -> AstChildren<ParamTy> {
        support::children(&self.syntax)
    }
    pub fn l_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['('])
    }
    pub fn params(&self) -> AstChildren<Param> {
        support::children(&self.syntax)
    }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![')'])
    }
    pub fn arrow_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![->])
    }
    pub fn ty(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
    pub fn body(&self) -> Option<EBlock> {
        support::child(&self.syntax)
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
pub struct ParamTy {
    pub(crate) syntax: SyntaxNode,
}
impl ParamTy {
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
pub struct Param {
    pub(crate) syntax: SyntaxNode,
}
impl Param {
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
pub struct EBlock {
    pub(crate) syntax: SyntaxNode,
}
impl EBlock {
    pub fn l_brace_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['{'])
    }
    pub fn declarations(&self) -> AstChildren<Declaration> {
        support::children(&self.syntax)
    }
    pub fn r_brace_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['}'])
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
pub struct TyVar {
    pub(crate) syntax: SyntaxNode,
}
impl TyVar {
    pub fn ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![ident])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyCons {
    pub(crate) syntax: SyntaxNode,
}
impl TyCons {
    pub fn qualifier(&self) -> Option<Qualifier> {
        support::child(&self.syntax)
    }
    pub fn upper_ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![upper_ident])
    }
    pub fn type_args(&self) -> AstChildren<Type> {
        support::children(&self.syntax)
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
    pub fn ty_arg_list(&self) -> Option<TyArgList> {
        support::child(&self.syntax)
    }
    pub fn arrow_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![->])
    }
    pub fn result(&self) -> Option<Type> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Qualifier {
    pub(crate) syntax: SyntaxNode,
}
impl Qualifier {
    pub fn upper_ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![upper_ident])
    }
    pub fn coloncolon_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![::])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyArgList {
    pub(crate) syntax: SyntaxNode,
}
impl TyArgList {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['('])
    }
    pub fn types(&self) -> AstChildren<Type> {
        support::children(&self.syntax)
    }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![')'])
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
    pub fn binary_lit_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![binary_lit])
    }
    pub fn hex_lit_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![hex_lit])
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
pub struct EArray {
    pub(crate) syntax: SyntaxNode,
}
impl EArray {
    pub fn l_brack_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['['])
    }
    pub fn exprs(&self) -> AstChildren<Expr> {
        support::children(&self.syntax)
    }
    pub fn r_brack_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![']'])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EStruct {
    pub(crate) syntax: SyntaxNode,
}
impl EStruct {
    pub fn qualifier(&self) -> Option<Qualifier> {
        support::child(&self.syntax)
    }
    pub fn upper_ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![upper_ident])
    }
    pub fn e_ty_arg_list(&self) -> Option<ETyArgList> {
        support::child(&self.syntax)
    }
    pub fn l_brace_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['{'])
    }
    pub fn e_struct_fields(&self) -> AstChildren<EStructField> {
        support::children(&self.syntax)
    }
    pub fn r_brace_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['}'])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ECall {
    pub(crate) syntax: SyntaxNode,
}
impl ECall {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    pub fn e_ty_arg_list(&self) -> Option<ETyArgList> {
        support::child(&self.syntax)
    }
    pub fn e_arg_list(&self) -> Option<EArgList> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EParen {
    pub(crate) syntax: SyntaxNode,
}
impl EParen {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['('])
    }
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![')'])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EBinary {
    pub(crate) syntax: SyntaxNode,
}
impl EBinary {}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EArrayIdx {
    pub(crate) syntax: SyntaxNode,
}
impl EArrayIdx {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    pub fn l_brack_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['['])
    }
    pub fn r_brack_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![']'])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EStructIdx {
    pub(crate) syntax: SyntaxNode,
}
impl EStructIdx {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    pub fn dot_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![.])
    }
    pub fn ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![ident])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EIf {
    pub(crate) syntax: SyntaxNode,
}
impl EIf {
    pub fn if_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![if])
    }
    pub fn condition(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    pub fn else_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![else])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EMatch {
    pub(crate) syntax: SyntaxNode,
}
impl EMatch {
    pub fn match_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![match])
    }
    pub fn scrutinee(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    pub fn l_brace_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['{'])
    }
    pub fn e_match_branchs(&self) -> AstChildren<EMatchBranch> {
        support::children(&self.syntax)
    }
    pub fn r_brace_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['}'])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EIntrinsic {
    pub(crate) syntax: SyntaxNode,
}
impl EIntrinsic {
    pub fn at_ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![at_ident])
    }
    pub fn e_arg_list(&self) -> Option<EArgList> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ETyArgList {
    pub(crate) syntax: SyntaxNode,
}
impl ETyArgList {
    pub fn types(&self) -> AstChildren<Type> {
        support::children(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EArgList {
    pub(crate) syntax: SyntaxNode,
}
impl EArgList {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T!['('])
    }
    pub fn exprs(&self) -> AstChildren<Expr> {
        support::children(&self.syntax)
    }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![')'])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EStructField {
    pub(crate) syntax: SyntaxNode,
}
impl EStructField {
    pub fn ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![ident])
    }
    pub fn eq_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![=])
    }
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EMatchBranch {
    pub(crate) syntax: SyntaxNode,
}
impl EMatchBranch {
    pub fn pattern(&self) -> Option<Pattern> {
        support::child(&self.syntax)
    }
    pub fn fat_arrow_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![=>])
    }
    pub fn body(&self) -> Option<EBlock> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DLet {
    pub(crate) syntax: SyntaxNode,
}
impl DLet {
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
pub struct DSet {
    pub(crate) syntax: SyntaxNode,
}
impl DSet {
    pub fn set_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![set])
    }
    pub fn set_target(&self) -> Option<SetTarget> {
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
pub struct DWhile {
    pub(crate) syntax: SyntaxNode,
}
impl DWhile {
    pub fn while_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![while])
    }
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    pub fn e_block(&self) -> Option<EBlock> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DExpr {
    pub(crate) syntax: SyntaxNode,
}
impl DExpr {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SetTarget {
    pub(crate) syntax: SyntaxNode,
}
impl SetTarget {
    pub fn set_target_expr(&self) -> Option<SetTargetExpr> {
        support::child(&self.syntax)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatVariant {
    pub(crate) syntax: SyntaxNode,
}
impl PatVariant {
    pub fn qualifier(&self) -> Option<Qualifier> {
        support::child(&self.syntax)
    }
    pub fn upper_ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![upper_ident])
    }
    pub fn ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![ident])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatVar {
    pub(crate) syntax: SyntaxNode,
}
impl PatVar {
    pub fn ident_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, T![ident])
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TopLevel {
    TopImport(TopImport),
    TopGlobal(TopGlobal),
    TopStruct(TopStruct),
    TopVariant(TopVariant),
    TopFn(TopFn),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    TyInt(TyInt),
    TyFloat(TyFloat),
    TyBool(TyBool),
    TyUnit(TyUnit),
    TyArray(TyArray),
    TyVar(TyVar),
    TyCons(TyCons),
    TyFn(TyFn),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    ELit(ELit),
    EVar(EVar),
    EArray(EArray),
    EStruct(EStruct),
    ECall(ECall),
    EParen(EParen),
    EBinary(EBinary),
    EArrayIdx(EArrayIdx),
    EStructIdx(EStructIdx),
    EIf(EIf),
    EMatch(EMatch),
    EBlock(EBlock),
    EIntrinsic(EIntrinsic),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    LitBool(LitBool),
    LitFloat(LitFloat),
    LitInt(LitInt),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    PatVariant(PatVariant),
    PatVar(PatVar),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Declaration {
    DLet(DLet),
    DSet(DSet),
    DWhile(DWhile),
    DExpr(DExpr),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SetTargetExpr {
    EVar(EVar),
    EArrayIdx(EArrayIdx),
    EStructIdx(EStructIdx),
}
impl AstNode for Root {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == Root
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
impl AstNode for TopGlobal {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TopGlobal
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
impl AstNode for TopVariant {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TopVariant
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
impl AstNode for TopFn {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TopFn
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
impl AstNode for ParamTy {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == ParamTy
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
impl AstNode for Param {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == Param
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
impl AstNode for EBlock {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EBlock
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
impl AstNode for TyVar {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TyVar
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
impl AstNode for Qualifier {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == Qualifier
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
impl AstNode for TyArgList {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TyArgList
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
impl AstNode for EArray {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EArray
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
impl AstNode for EStruct {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EStruct
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
impl AstNode for ECall {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == ECall
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
impl AstNode for EParen {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EParen
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
impl AstNode for EBinary {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EBinary
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
impl AstNode for EArrayIdx {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EArrayIdx
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
impl AstNode for EStructIdx {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EStructIdx
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
impl AstNode for EIf {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EIf
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
impl AstNode for EMatch {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EMatch
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
impl AstNode for EIntrinsic {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EIntrinsic
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
impl AstNode for ETyArgList {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == ETyArgList
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
impl AstNode for EArgList {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EArgList
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
impl AstNode for EStructField {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EStructField
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
impl AstNode for EMatchBranch {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EMatchBranch
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
impl AstNode for DLet {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == DLet
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
impl AstNode for DSet {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == DSet
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
impl AstNode for DWhile {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == DWhile
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
impl AstNode for DExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == DExpr
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
impl AstNode for SetTarget {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SetTarget
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
impl AstNode for PatVariant {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PatVariant
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
impl AstNode for PatVar {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PatVar
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
impl From<TopGlobal> for TopLevel {
    fn from(node: TopGlobal) -> TopLevel {
        TopLevel::TopGlobal(node)
    }
}
impl From<TopStruct> for TopLevel {
    fn from(node: TopStruct) -> TopLevel {
        TopLevel::TopStruct(node)
    }
}
impl From<TopVariant> for TopLevel {
    fn from(node: TopVariant) -> TopLevel {
        TopLevel::TopVariant(node)
    }
}
impl From<TopFn> for TopLevel {
    fn from(node: TopFn) -> TopLevel {
        TopLevel::TopFn(node)
    }
}
impl AstNode for TopLevel {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            TopImport | TopGlobal | TopStruct | TopVariant | TopFn => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            TopImport => TopLevel::TopImport(TopImport { syntax }),
            TopGlobal => TopLevel::TopGlobal(TopGlobal { syntax }),
            TopStruct => TopLevel::TopStruct(TopStruct { syntax }),
            TopVariant => TopLevel::TopVariant(TopVariant { syntax }),
            TopFn => TopLevel::TopFn(TopFn { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            TopLevel::TopImport(it) => &it.syntax,
            TopLevel::TopGlobal(it) => &it.syntax,
            TopLevel::TopStruct(it) => &it.syntax,
            TopLevel::TopVariant(it) => &it.syntax,
            TopLevel::TopFn(it) => &it.syntax,
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
impl From<TyVar> for Type {
    fn from(node: TyVar) -> Type {
        Type::TyVar(node)
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
            TyInt | TyFloat | TyBool | TyUnit | TyArray | TyVar | TyCons | TyFn => true,
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
            TyVar => Type::TyVar(TyVar { syntax }),
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
            Type::TyVar(it) => &it.syntax,
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
impl From<EArray> for Expr {
    fn from(node: EArray) -> Expr {
        Expr::EArray(node)
    }
}
impl From<EStruct> for Expr {
    fn from(node: EStruct) -> Expr {
        Expr::EStruct(node)
    }
}
impl From<ECall> for Expr {
    fn from(node: ECall) -> Expr {
        Expr::ECall(node)
    }
}
impl From<EParen> for Expr {
    fn from(node: EParen) -> Expr {
        Expr::EParen(node)
    }
}
impl From<EBinary> for Expr {
    fn from(node: EBinary) -> Expr {
        Expr::EBinary(node)
    }
}
impl From<EArrayIdx> for Expr {
    fn from(node: EArrayIdx) -> Expr {
        Expr::EArrayIdx(node)
    }
}
impl From<EStructIdx> for Expr {
    fn from(node: EStructIdx) -> Expr {
        Expr::EStructIdx(node)
    }
}
impl From<EIf> for Expr {
    fn from(node: EIf) -> Expr {
        Expr::EIf(node)
    }
}
impl From<EMatch> for Expr {
    fn from(node: EMatch) -> Expr {
        Expr::EMatch(node)
    }
}
impl From<EBlock> for Expr {
    fn from(node: EBlock) -> Expr {
        Expr::EBlock(node)
    }
}
impl From<EIntrinsic> for Expr {
    fn from(node: EIntrinsic) -> Expr {
        Expr::EIntrinsic(node)
    }
}
impl AstNode for Expr {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            ELit | EVar | EArray | EStruct | ECall | EParen | EBinary | EArrayIdx | EStructIdx
            | EIf | EMatch | EBlock | EIntrinsic => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            ELit => Expr::ELit(ELit { syntax }),
            EVar => Expr::EVar(EVar { syntax }),
            EArray => Expr::EArray(EArray { syntax }),
            EStruct => Expr::EStruct(EStruct { syntax }),
            ECall => Expr::ECall(ECall { syntax }),
            EParen => Expr::EParen(EParen { syntax }),
            EBinary => Expr::EBinary(EBinary { syntax }),
            EArrayIdx => Expr::EArrayIdx(EArrayIdx { syntax }),
            EStructIdx => Expr::EStructIdx(EStructIdx { syntax }),
            EIf => Expr::EIf(EIf { syntax }),
            EMatch => Expr::EMatch(EMatch { syntax }),
            EBlock => Expr::EBlock(EBlock { syntax }),
            EIntrinsic => Expr::EIntrinsic(EIntrinsic { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Expr::ELit(it) => &it.syntax,
            Expr::EVar(it) => &it.syntax,
            Expr::EArray(it) => &it.syntax,
            Expr::EStruct(it) => &it.syntax,
            Expr::ECall(it) => &it.syntax,
            Expr::EParen(it) => &it.syntax,
            Expr::EBinary(it) => &it.syntax,
            Expr::EArrayIdx(it) => &it.syntax,
            Expr::EStructIdx(it) => &it.syntax,
            Expr::EIf(it) => &it.syntax,
            Expr::EMatch(it) => &it.syntax,
            Expr::EBlock(it) => &it.syntax,
            Expr::EIntrinsic(it) => &it.syntax,
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
impl From<PatVariant> for Pattern {
    fn from(node: PatVariant) -> Pattern {
        Pattern::PatVariant(node)
    }
}
impl From<PatVar> for Pattern {
    fn from(node: PatVar) -> Pattern {
        Pattern::PatVar(node)
    }
}
impl AstNode for Pattern {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            PatVariant | PatVar => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            PatVariant => Pattern::PatVariant(PatVariant { syntax }),
            PatVar => Pattern::PatVar(PatVar { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Pattern::PatVariant(it) => &it.syntax,
            Pattern::PatVar(it) => &it.syntax,
        }
    }
}
impl From<DLet> for Declaration {
    fn from(node: DLet) -> Declaration {
        Declaration::DLet(node)
    }
}
impl From<DSet> for Declaration {
    fn from(node: DSet) -> Declaration {
        Declaration::DSet(node)
    }
}
impl From<DWhile> for Declaration {
    fn from(node: DWhile) -> Declaration {
        Declaration::DWhile(node)
    }
}
impl From<DExpr> for Declaration {
    fn from(node: DExpr) -> Declaration {
        Declaration::DExpr(node)
    }
}
impl AstNode for Declaration {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            DLet | DSet | DWhile | DExpr => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            DLet => Declaration::DLet(DLet { syntax }),
            DSet => Declaration::DSet(DSet { syntax }),
            DWhile => Declaration::DWhile(DWhile { syntax }),
            DExpr => Declaration::DExpr(DExpr { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Declaration::DLet(it) => &it.syntax,
            Declaration::DSet(it) => &it.syntax,
            Declaration::DWhile(it) => &it.syntax,
            Declaration::DExpr(it) => &it.syntax,
        }
    }
}
impl From<EVar> for SetTargetExpr {
    fn from(node: EVar) -> SetTargetExpr {
        SetTargetExpr::EVar(node)
    }
}
impl From<EArrayIdx> for SetTargetExpr {
    fn from(node: EArrayIdx) -> SetTargetExpr {
        SetTargetExpr::EArrayIdx(node)
    }
}
impl From<EStructIdx> for SetTargetExpr {
    fn from(node: EStructIdx) -> SetTargetExpr {
        SetTargetExpr::EStructIdx(node)
    }
}
impl AstNode for SetTargetExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            EVar | EArrayIdx | EStructIdx => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            EVar => SetTargetExpr::EVar(EVar { syntax }),
            EArrayIdx => SetTargetExpr::EArrayIdx(EArrayIdx { syntax }),
            EStructIdx => SetTargetExpr::EStructIdx(EStructIdx { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            SetTargetExpr::EVar(it) => &it.syntax,
            SetTargetExpr::EArrayIdx(it) => &it.syntax,
            SetTargetExpr::EStructIdx(it) => &it.syntax,
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
impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for SetTargetExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for Root {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TopImport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TopGlobal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TopStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TopVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TopFn {
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
impl std::fmt::Display for ParamTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for StructField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for EBlock {
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
impl std::fmt::Display for TyVar {
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
impl std::fmt::Display for Qualifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TyArgList {
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
impl std::fmt::Display for EArray {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for EStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for ECall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for EParen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for EBinary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for EArrayIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for EStructIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for EIf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for EMatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for EIntrinsic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for ETyArgList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for EArgList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for EStructField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for EMatchBranch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for DLet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for DSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for DWhile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for DExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for SetTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for PatVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for PatVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
