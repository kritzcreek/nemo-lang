// THIS FILE IS GENERATED
use tree_sitter::Node;

#[derive(Clone, Debug)]
enum CalleeNode<'a> {
    Lit(LitNode<'a>),
    ArrayIdxE(ArrayIdxENode<'a>),
    CallE(CallENode<'a>),
    IntrinsicE(IntrinsicENode<'a>),
    ParenthesizedE(ParenthesizedENode<'a>),
    StructIdxE(StructIdxENode<'a>),
    VarE(VarENode<'a>),
}

impl<'a> CalleeNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        LitNode::can_cast(node)
            || ArrayIdxENode::can_cast(node)
            || CallENode::can_cast(node)
            || IntrinsicENode::can_cast(node)
            || ParenthesizedENode::can_cast(node)
            || StructIdxENode::can_cast(node)
            || VarENode::can_cast(node)
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if let Some(casted_node) = LitNode::cast(node) {
            Some(Self::Lit(casted_node))
        } else if let Some(casted_node) = ArrayIdxENode::cast(node) {
            Some(Self::ArrayIdxE(casted_node))
        } else if let Some(casted_node) = CallENode::cast(node) {
            Some(Self::CallE(casted_node))
        } else if let Some(casted_node) = IntrinsicENode::cast(node) {
            Some(Self::IntrinsicE(casted_node))
        } else if let Some(casted_node) = ParenthesizedENode::cast(node) {
            Some(Self::ParenthesizedE(casted_node))
        } else if let Some(casted_node) = StructIdxENode::cast(node) {
            Some(Self::StructIdxE(casted_node))
        } else if let Some(casted_node) = VarENode::cast(node) {
            Some(Self::VarE(casted_node))
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
enum DeclNode<'a> {
    ExprDecl(ExprDeclNode<'a>),
    LetDecl(LetDeclNode<'a>),
    SetDecl(SetDeclNode<'a>),
    WhileDecl(WhileDeclNode<'a>),
}

impl<'a> DeclNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        ExprDeclNode::can_cast(node)
            || LetDeclNode::can_cast(node)
            || SetDeclNode::can_cast(node)
            || WhileDeclNode::can_cast(node)
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if let Some(casted_node) = ExprDeclNode::cast(node) {
            Some(Self::ExprDecl(casted_node))
        } else if let Some(casted_node) = LetDeclNode::cast(node) {
            Some(Self::LetDecl(casted_node))
        } else if let Some(casted_node) = SetDeclNode::cast(node) {
            Some(Self::SetDecl(casted_node))
        } else if let Some(casted_node) = WhileDeclNode::cast(node) {
            Some(Self::WhileDecl(casted_node))
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
enum ExprNode<'a> {
    Lit(LitNode<'a>),
    ArrayE(ArrayENode<'a>),
    ArrayIdxE(ArrayIdxENode<'a>),
    BinaryE(BinaryENode<'a>),
    BlockE(BlockENode<'a>),
    CallE(CallENode<'a>),
    IfE(IfENode<'a>),
    IntrinsicE(IntrinsicENode<'a>),
    ParenthesizedE(ParenthesizedENode<'a>),
    StructE(StructENode<'a>),
    StructIdxE(StructIdxENode<'a>),
    VarE(VarENode<'a>),
}

impl<'a> ExprNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        LitNode::can_cast(node)
            || ArrayENode::can_cast(node)
            || ArrayIdxENode::can_cast(node)
            || BinaryENode::can_cast(node)
            || BlockENode::can_cast(node)
            || CallENode::can_cast(node)
            || IfENode::can_cast(node)
            || IntrinsicENode::can_cast(node)
            || ParenthesizedENode::can_cast(node)
            || StructENode::can_cast(node)
            || StructIdxENode::can_cast(node)
            || VarENode::can_cast(node)
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if let Some(casted_node) = LitNode::cast(node) {
            Some(Self::Lit(casted_node))
        } else if let Some(casted_node) = ArrayENode::cast(node) {
            Some(Self::ArrayE(casted_node))
        } else if let Some(casted_node) = ArrayIdxENode::cast(node) {
            Some(Self::ArrayIdxE(casted_node))
        } else if let Some(casted_node) = BinaryENode::cast(node) {
            Some(Self::BinaryE(casted_node))
        } else if let Some(casted_node) = BlockENode::cast(node) {
            Some(Self::BlockE(casted_node))
        } else if let Some(casted_node) = CallENode::cast(node) {
            Some(Self::CallE(casted_node))
        } else if let Some(casted_node) = IfENode::cast(node) {
            Some(Self::IfE(casted_node))
        } else if let Some(casted_node) = IntrinsicENode::cast(node) {
            Some(Self::IntrinsicE(casted_node))
        } else if let Some(casted_node) = ParenthesizedENode::cast(node) {
            Some(Self::ParenthesizedE(casted_node))
        } else if let Some(casted_node) = StructENode::cast(node) {
            Some(Self::StructE(casted_node))
        } else if let Some(casted_node) = StructIdxENode::cast(node) {
            Some(Self::StructIdxE(casted_node))
        } else if let Some(casted_node) = VarENode::cast(node) {
            Some(Self::VarE(casted_node))
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
enum LitNode<'a> {
    BoolLit(BoolLitNode<'a>),
    FloatLit(FloatLitNode<'a>),
    IntLit(IntLitNode<'a>),
}

impl<'a> LitNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        BoolLitNode::can_cast(node) || FloatLitNode::can_cast(node) || IntLitNode::can_cast(node)
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if let Some(casted_node) = BoolLitNode::cast(node) {
            Some(Self::BoolLit(casted_node))
        } else if let Some(casted_node) = FloatLitNode::cast(node) {
            Some(Self::FloatLit(casted_node))
        } else if let Some(casted_node) = IntLitNode::cast(node) {
            Some(Self::IntLit(casted_node))
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
enum SetTargetNode<'a> {
    SetArrayIdx(SetArrayIdxNode<'a>),
    SetStructIdx(SetStructIdxNode<'a>),
    SetVar(SetVarNode<'a>),
}

impl<'a> SetTargetNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        SetArrayIdxNode::can_cast(node)
            || SetStructIdxNode::can_cast(node)
            || SetVarNode::can_cast(node)
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if let Some(casted_node) = SetArrayIdxNode::cast(node) {
            Some(Self::SetArrayIdx(casted_node))
        } else if let Some(casted_node) = SetStructIdxNode::cast(node) {
            Some(Self::SetStructIdx(casted_node))
        } else if let Some(casted_node) = SetVarNode::cast(node) {
            Some(Self::SetVar(casted_node))
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
enum ToplevelNode<'a> {
    TopFunc(TopFuncNode<'a>),
    TopImport(TopImportNode<'a>),
    TopLet(TopLetNode<'a>),
    TopStruct(TopStructNode<'a>),
}

impl<'a> ToplevelNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        TopFuncNode::can_cast(node)
            || TopImportNode::can_cast(node)
            || TopLetNode::can_cast(node)
            || TopStructNode::can_cast(node)
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if let Some(casted_node) = TopFuncNode::cast(node) {
            Some(Self::TopFunc(casted_node))
        } else if let Some(casted_node) = TopImportNode::cast(node) {
            Some(Self::TopImport(casted_node))
        } else if let Some(casted_node) = TopLetNode::cast(node) {
            Some(Self::TopLet(casted_node))
        } else if let Some(casted_node) = TopStructNode::cast(node) {
            Some(Self::TopStruct(casted_node))
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
enum TypeNode<'a> {
    TyArray(TyArrayNode<'a>),
    TyBool(TyBoolNode<'a>),
    TyF32(TyF32Node<'a>),
    TyFunc(TyFuncNode<'a>),
    TyI32(TyI32Node<'a>),
    TyStruct(TyStructNode<'a>),
    TyUnit(TyUnitNode<'a>),
}

impl<'a> TypeNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        TyArrayNode::can_cast(node)
            || TyBoolNode::can_cast(node)
            || TyF32Node::can_cast(node)
            || TyFuncNode::can_cast(node)
            || TyI32Node::can_cast(node)
            || TyStructNode::can_cast(node)
            || TyUnitNode::can_cast(node)
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if let Some(casted_node) = TyArrayNode::cast(node) {
            Some(Self::TyArray(casted_node))
        } else if let Some(casted_node) = TyBoolNode::cast(node) {
            Some(Self::TyBool(casted_node))
        } else if let Some(casted_node) = TyF32Node::cast(node) {
            Some(Self::TyF32(casted_node))
        } else if let Some(casted_node) = TyFuncNode::cast(node) {
            Some(Self::TyFunc(casted_node))
        } else if let Some(casted_node) = TyI32Node::cast(node) {
            Some(Self::TyI32(casted_node))
        } else if let Some(casted_node) = TyStructNode::cast(node) {
            Some(Self::TyStruct(casted_node))
        } else if let Some(casted_node) = TyUnitNode::cast(node) {
            Some(Self::TyUnit(casted_node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct ArrayENode<'a>(Node<'a>);

impl<'a> ArrayENode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "array_e"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct ArrayIdxENode<'a>(Node<'a>);

impl<'a> ArrayIdxENode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "array_idx_e"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct BinaryENode<'a>(Node<'a>);

impl<'a> BinaryENode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "binary_e"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct BlockENode<'a>(Node<'a>);

impl<'a> BlockENode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "block_e"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct BoolLitNode<'a>(Node<'a>);

impl<'a> BoolLitNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "bool_lit"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct CallArgsNode<'a>(Node<'a>);

impl<'a> CallArgsNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "call_args"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct CallENode<'a>(Node<'a>);

impl<'a> CallENode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "call_e"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct CommentNode<'a>(Node<'a>);

impl<'a> CommentNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "comment"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct ExprDeclNode<'a>(Node<'a>);

impl<'a> ExprDeclNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "expr_decl"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct FuncParamNode<'a>(Node<'a>);

impl<'a> FuncParamNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "func_param"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct FuncParamsNode<'a>(Node<'a>);

impl<'a> FuncParamsNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "func_params"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct IfENode<'a>(Node<'a>);

impl<'a> IfENode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "if_e"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct IntLitNode<'a>(Node<'a>);

impl<'a> IntLitNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "int_lit"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct IntrinsicENode<'a>(Node<'a>);

impl<'a> IntrinsicENode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "intrinsic_e"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct LetDeclNode<'a>(Node<'a>);

impl<'a> LetDeclNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "let_decl"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct ParenthesizedENode<'a>(Node<'a>);

impl<'a> ParenthesizedENode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "parenthesized_e"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct SetArrayIdxNode<'a>(Node<'a>);

impl<'a> SetArrayIdxNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "set_array_idx"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct SetDeclNode<'a>(Node<'a>);

impl<'a> SetDeclNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "set_decl"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct SetStructIdxNode<'a>(Node<'a>);

impl<'a> SetStructIdxNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "set_struct_idx"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct SetVarNode<'a>(Node<'a>);

impl<'a> SetVarNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "set_var"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct SourceFileNode<'a>(Node<'a>);

impl<'a> SourceFileNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "source_file"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct StructENode<'a>(Node<'a>);

impl<'a> StructENode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "struct_e"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct StructFieldENode<'a>(Node<'a>);

impl<'a> StructFieldENode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "struct_field_e"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct StructFieldTopNode<'a>(Node<'a>);

impl<'a> StructFieldTopNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "struct_field_top"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct StructIdxENode<'a>(Node<'a>);

impl<'a> StructIdxENode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "struct_idx_e"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TopFuncNode<'a>(Node<'a>);

impl<'a> TopFuncNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "top_func"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TopImportNode<'a>(Node<'a>);

impl<'a> TopImportNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "top_import"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TopLetNode<'a>(Node<'a>);

impl<'a> TopLetNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "top_let"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TopStructNode<'a>(Node<'a>);

impl<'a> TopStructNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "top_struct"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TyArrayNode<'a>(Node<'a>);

impl<'a> TyArrayNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "ty_array"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TyFuncNode<'a>(Node<'a>);

impl<'a> TyFuncNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "ty_func"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TyStructNode<'a>(Node<'a>);

impl<'a> TyStructNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "ty_struct"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct VarENode<'a>(Node<'a>);

impl<'a> VarENode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "var_e"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct WhileDeclNode<'a>(Node<'a>);

impl<'a> WhileDeclNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "while_decl"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct FloatLitNode<'a>(Node<'a>);

impl<'a> FloatLitNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "float_lit"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct IntrinsicIdentNode<'a>(Node<'a>);

impl<'a> IntrinsicIdentNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "intrinsic_ident"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct LowerIdentNode<'a>(Node<'a>);

impl<'a> LowerIdentNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "lower_ident"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TyBoolNode<'a>(Node<'a>);

impl<'a> TyBoolNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "ty_bool"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TyF32Node<'a>(Node<'a>);

impl<'a> TyF32Node<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "ty_f32"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TyI32Node<'a>(Node<'a>);

impl<'a> TyI32Node<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "ty_i32"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TyUnitNode<'a>(Node<'a>);

impl<'a> TyUnitNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "ty_unit"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct UpperIdentNode<'a>(Node<'a>);

impl<'a> UpperIdentNode<'a> {
    pub fn can_cast(node: &Node<'a>) -> bool {
        node.kind() == "upper_ident"
    }

    pub fn cast(node: &Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(*node))
        } else {
            None
        }
    }
}
