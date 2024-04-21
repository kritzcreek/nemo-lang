/// A bunch of builders for backend ir, so the type checker can continue to be
/// written in an imperative style
use backend::ir::{self, *};
use rowan::TextRange;

pub(crate) fn var(name: Name) -> Option<ExprData> {
    Some(ExprData::Var(name))
}

pub(crate) fn unit_lit(range: TextRange) -> Option<Expr> {
    Some(Expr {
        at: range,
        ty: Ty::Unit,
        it: Box::new(ExprData::Lit(Lit {
            at: range,
            ty: Ty::Unit,
            it: LitData::Unit,
        })),
    })
}

pub(crate) fn lit(lit: Option<Lit>) -> Option<ExprData> {
    Some(ExprData::Lit(lit?))
}

pub(crate) fn array_len(at: TextRange) -> Option<Intrinsic> {
    Some(ir::Intrinsic {
        it: ir::IntrinsicData::ArrayLen,
        at,
    })
}

pub(crate) fn array_new(at: TextRange) -> Option<Intrinsic> {
    Some(ir::Intrinsic {
        it: ir::IntrinsicData::ArrayNew,
        at,
    })
}

pub(crate) fn expr_decl(expr: Option<Expr>) -> Option<DeclarationData> {
    Some(DeclarationData::Expr(expr?))
}

#[derive(Debug)]
pub(crate) struct FuncBuilder {
    name: Option<Name>,
    params: Option<Vec<(Name, Ty)>>,
    return_ty: Option<Ty>,
    body: Option<Expr>,
}

impl FuncBuilder {
    pub(crate) fn new() -> Self {
        FuncBuilder {
            name: None,
            params: Some(vec![]),
            return_ty: None,
            body: None,
        }
    }

    pub(crate) fn name(&mut self, name: Name) -> &mut Self {
        self.name = Some(name);
        self
    }

    pub(crate) fn return_ty(&mut self, return_ty: Option<Ty>) -> &mut Self {
        self.return_ty = return_ty;
        self
    }

    pub(crate) fn param(&mut self, name: Name, param: Option<Ty>) -> &mut Self {
        if let Some(params) = &mut self.params {
            if let Some(param) = param {
                params.push((name, param))
            } else {
                self.params = None
            }
        }
        self
    }

    pub(crate) fn body(&mut self, body: Option<Expr>) -> &mut Self {
        self.body = body;
        self
    }

    pub(crate) fn build(self) -> Option<ir::Func> {
        Some(Func {
            name: self.name?,
            params: self.params?,
            return_ty: self.return_ty?,
            body: self.body?,
        })
    }
}

#[derive(Default)]
pub(crate) struct WhileBuilder {
    condition: Option<Expr>,
    body: Option<Expr>,
}

impl WhileBuilder {
    pub(crate) fn new() -> WhileBuilder {
        Self::default()
    }

    pub(crate) fn condition(&mut self, condition: Option<Expr>) -> &mut Self {
        self.condition = condition;
        self
    }

    pub(crate) fn body(&mut self, body: Option<Expr>) -> &mut Self {
        self.body = body;
        self
    }

    pub(crate) fn build(self) -> Option<ir::DeclarationData> {
        Some(DeclarationData::While {
            condition: self.condition?,
            body: self.body?,
        })
    }
}

#[derive(Default)]
pub(crate) struct LetBuilder {
    binder: Option<Name>,
    expr: Option<Expr>,
}

impl LetBuilder {
    pub(crate) fn new() -> LetBuilder {
        Self::default()
    }

    pub(crate) fn binder(&mut self, binder: Name) -> &mut Self {
        self.binder = Some(binder);
        self
    }

    pub(crate) fn expr(&mut self, expr: Option<Expr>) -> &mut Self {
        self.expr = expr;
        self
    }

    pub(crate) fn build(self) -> Option<ir::DeclarationData> {
        Some(DeclarationData::Let {
            binder: self.binder?,
            expr: self.expr?,
        })
    }
}

#[derive(Default, Debug)]
pub(crate) struct SetBuilder {
    set_target: Option<SetTarget>,
    expr: Option<Expr>,
}

impl SetBuilder {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn set_target(&mut self, set_target: Option<SetTarget>) -> &mut Self {
        self.set_target = set_target;
        self
    }

    pub(crate) fn expr(&mut self, expr: Option<Expr>) -> &mut Self {
        self.expr = expr;
        self
    }

    pub(crate) fn build(self) -> Option<ir::DeclarationData> {
        Some(DeclarationData::Set {
            set_target: self.set_target?,
            expr: self.expr?,
        })
    }
}

#[derive(Debug)]
pub(crate) struct ArrayBuilder {
    elems: Option<Vec<Expr>>,
}

impl ArrayBuilder {
    pub(crate) fn new() -> Self {
        ArrayBuilder {
            elems: Some(vec![]),
        }
    }

    pub(crate) fn elem(&mut self, elem: Option<Expr>) -> &mut Self {
        if let Some(elems) = &mut self.elems {
            if let Some(elem) = elem {
                elems.push(elem)
            } else {
                self.elems = None
            }
        }
        self
    }

    pub(crate) fn build(self) -> Option<ExprData> {
        Some(ExprData::Array(self.elems?))
    }
}

#[derive(Default)]
pub(crate) struct ArrayIdxBuilder {
    array: Option<Expr>,
    index: Option<Expr>,
}

impl ArrayIdxBuilder {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn array(&mut self, array: Option<Expr>) -> &mut Self {
        self.array = array;
        self
    }

    pub(crate) fn index(&mut self, index: Option<Expr>) -> &mut Self {
        self.index = index;
        self
    }

    pub(crate) fn build(self) -> Option<ExprData> {
        Some(ExprData::ArrayIdx {
            array: self.array?,
            index: self.index?,
        })
    }
}

pub(crate) struct StructBuilder {
    name: Option<Name>,
    fields: Option<Vec<(Name, Expr)>>,
}

impl StructBuilder {
    pub(crate) fn new() -> Self {
        StructBuilder {
            name: None,
            fields: Some(vec![]),
        }
    }

    pub(crate) fn name(&mut self, name: Option<Name>) -> &mut Self {
        self.name = name;
        self
    }

    pub(crate) fn field(&mut self, name: Name, expr: Option<Expr>) -> &mut Self {
        if let Some(fields) = &mut self.fields {
            if let Some(expr) = expr {
                fields.push((name, expr))
            } else {
                self.fields = None
            }
        }
        self
    }

    pub(crate) fn cancel(&mut self) {
        self.fields = None
    }

    pub(crate) fn build(self) -> Option<ExprData> {
        Some(ExprData::Struct {
            name: self.name?,
            fields: self.fields?,
        })
    }
}

#[derive(Default)]
pub(crate) struct StructIdxBuilder {
    expr: Option<Expr>,
    index: Option<Name>,
}

impl StructIdxBuilder {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn expr(&mut self, expr: Option<Expr>) -> &mut Self {
        self.expr = expr;
        self
    }

    pub(crate) fn index(&mut self, index: Name) -> &mut Self {
        self.index = Some(index);
        self
    }

    pub(crate) fn build(self) -> Option<ExprData> {
        Some(ExprData::StructIdx {
            expr: self.expr?,
            index: self.index?,
        })
    }
}

#[derive(Default)]
pub(crate) struct IfBuilder {
    condition: Option<Expr>,
    then_branch: Option<Expr>,
    else_branch: Option<Expr>,
}

impl IfBuilder {
    pub(crate) fn new() -> IfBuilder {
        Self::default()
    }
    pub(crate) fn condition(&mut self, condition: Option<Expr>) -> &mut Self {
        self.condition = condition;
        self
    }

    pub(crate) fn then_branch(&mut self, then_branch: Option<Expr>) -> &mut Self {
        self.then_branch = then_branch;
        self
    }

    pub(crate) fn else_branch(&mut self, else_branch: Option<Expr>) -> &mut Self {
        self.else_branch = else_branch;
        self
    }

    pub(crate) fn build(self) -> Option<ir::ExprData> {
        Some(ExprData::If {
            condition: self.condition?,
            then_branch: self.then_branch?,
            else_branch: self.else_branch?,
        })
    }
}

pub(crate) struct CallBuilder {
    func: Option<Callee>,
    arguments: Option<Vec<Expr>>,
}

impl CallBuilder {
    pub(crate) fn new() -> Self {
        CallBuilder {
            func: None,
            arguments: Some(vec![]),
        }
    }

    pub(crate) fn func(&mut self, func: Option<Callee>) -> &mut Self {
        self.func = func;
        self
    }

    pub(crate) fn argument(&mut self, elem: Option<Expr>) -> &mut Self {
        if let Some(arguments) = &mut self.arguments {
            if let Some(elem) = elem {
                arguments.push(elem)
            } else {
                self.arguments = None
            }
        }
        self
    }

    pub(crate) fn cancel(&mut self) {
        self.arguments = None
    }

    pub(crate) fn build(self) -> Option<ExprData> {
        Some(ExprData::Call {
            func: self.func?,
            arguments: self.arguments?,
        })
    }
}

pub(crate) struct IntrinsicBuilder {
    intrinsic: Option<Intrinsic>,
    arguments: Option<Vec<Expr>>,
}

impl IntrinsicBuilder {
    pub(crate) fn new() -> Self {
        IntrinsicBuilder {
            intrinsic: None,
            arguments: Some(vec![]),
        }
    }

    pub(crate) fn intrinsic(&mut self, intrinsic: Option<Intrinsic>) -> &mut Self {
        self.intrinsic = intrinsic;
        self
    }

    pub(crate) fn argument(&mut self, elem: Option<Expr>) -> &mut Self {
        if let Some(arguments) = &mut self.arguments {
            if let Some(elem) = elem {
                arguments.push(elem)
            } else {
                self.arguments = None
            }
        }
        self
    }

    pub(crate) fn build(self) -> Option<ExprData> {
        Some(ExprData::Intrinsic {
            intrinsic: self.intrinsic?,
            arguments: self.arguments?,
        })
    }
}

#[derive(Default)]
pub(crate) struct BinaryBuilder {
    op: Option<Op>,
    left: Option<Expr>,
    right: Option<Expr>,
}

impl BinaryBuilder {
    pub(crate) fn new() -> Self {
        Self::default()
    }
    pub(crate) fn op(&mut self, op: Op) -> &mut Self {
        self.op = Some(op);
        self
    }

    pub(crate) fn left(&mut self, left: Option<Expr>) -> &mut Self {
        self.left = left;
        self
    }

    pub(crate) fn right(&mut self, right: Option<Expr>) -> &mut Self {
        self.right = right;
        self
    }

    pub(crate) fn build(self) -> Option<ir::ExprData> {
        Some(ExprData::Binary {
            op: self.op?,
            left: self.left?,
            right: self.right?,
        })
    }
}

#[derive(Debug)]
pub(crate) struct BlockBuilder {
    declarations: Option<Vec<Declaration>>,
    expr: Option<Expr>,
}

impl BlockBuilder {
    pub(crate) fn new() -> Self {
        BlockBuilder {
            expr: None,
            declarations: Some(vec![]),
        }
    }

    pub(crate) fn declaration(&mut self, elem: Option<Declaration>) -> &mut Self {
        if let Some(declarations) = &mut self.declarations {
            if let Some(elem) = elem {
                declarations.push(elem)
            } else {
                self.declarations = None
            }
        }
        self
    }

    pub(crate) fn expr(&mut self, expr: Option<Expr>) -> &mut Self {
        self.expr = expr;
        self
    }

    pub(crate) fn build(self) -> Option<ExprData> {
        Some(ExprData::Block {
            declarations: self.declarations?,
            expr: self.expr?,
        })
    }
}

#[derive(Default)]
pub(crate) struct SetArrayBuilder {
    target: Option<Expr>,
    index: Option<Expr>,
}

impl SetArrayBuilder {
    pub(crate) fn new() -> Self {
        Self::default()
    }
    pub(crate) fn target(&mut self, target: Option<Expr>) -> &mut Self {
        self.target = target;
        self
    }

    pub(crate) fn index(&mut self, index: Option<Expr>) -> &mut Self {
        self.index = index;
        self
    }

    pub(crate) fn build(self) -> Option<ir::SetTargetData> {
        Some(SetTargetData::Array {
            target: self.target?,
            index: self.index?,
        })
    }
}

#[derive(Default)]
pub(crate) struct SetStructBuilder {
    target: Option<Expr>,
    index: Option<Name>,
}

impl SetStructBuilder {
    pub(crate) fn new() -> Self {
        Self::default()
    }
    pub(crate) fn target(&mut self, target: Option<Expr>) -> &mut Self {
        self.target = target;
        self
    }

    pub(crate) fn index(&mut self, index: Name) -> &mut Self {
        self.index = Some(index);
        self
    }

    pub(crate) fn build(self) -> Option<ir::SetTargetData> {
        Some(SetTargetData::Struct {
            target: self.target?,
            index: self.index?,
        })
    }
}
