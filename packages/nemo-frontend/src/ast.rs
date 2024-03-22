// THIS FILE IS GENERATED
use tree_sitter::Node;

#[derive(Clone, Debug)]
    pub enum CalleeNode<'a> {
  Lit(LitNode<'a>),
  ArrayIdxE(ArrayIdxENode<'a>),
  CallE(CallENode<'a>),
  IntrinsicE(IntrinsicENode<'a>),
  ParenthesizedE(ParenthesizedENode<'a>),
  StructIdxE(StructIdxENode<'a>),
  VarE(VarENode<'a>),
}

  impl<'a> CalleeNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
LitNode::can_cast(node)
 || ArrayIdxENode::can_cast(node) || CallENode::can_cast(node) || IntrinsicENode::can_cast(node) || ParenthesizedENode::can_cast(node) || StructIdxENode::can_cast(node) || VarENode::can_cast(node)
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {

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
    } else { None }
    }
}


#[derive(Clone, Debug)]
    pub enum DeclNode<'a> {
  ExprDecl(ExprDeclNode<'a>),
  LetDecl(LetDeclNode<'a>),
  SetDecl(SetDeclNode<'a>),
  WhileDecl(WhileDeclNode<'a>),
}

  impl<'a> DeclNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
ExprDeclNode::can_cast(node)
 || LetDeclNode::can_cast(node) || SetDeclNode::can_cast(node) || WhileDeclNode::can_cast(node)
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {

    if let Some(casted_node) = ExprDeclNode::cast(node) {
        Some(Self::ExprDecl(casted_node))
    } else if let Some(casted_node) = LetDeclNode::cast(node) {
        Some(Self::LetDecl(casted_node))
    } else if let Some(casted_node) = SetDeclNode::cast(node) {
        Some(Self::SetDecl(casted_node))
    } else if let Some(casted_node) = WhileDeclNode::cast(node) {
        Some(Self::WhileDecl(casted_node))
    } else { None }
    }
}


#[derive(Clone, Debug)]
    pub enum ExprNode<'a> {
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
    pub fn can_cast(node: Node<'a>) -> bool {
LitNode::can_cast(node)
 || ArrayENode::can_cast(node) || ArrayIdxENode::can_cast(node) || BinaryENode::can_cast(node) || BlockENode::can_cast(node) || CallENode::can_cast(node) || IfENode::can_cast(node) || IntrinsicENode::can_cast(node) || ParenthesizedENode::can_cast(node) || StructENode::can_cast(node) || StructIdxENode::can_cast(node) || VarENode::can_cast(node)
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {

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
    } else { None }
    }
}


#[derive(Clone, Debug)]
    pub enum LitNode<'a> {
  BoolLit(BoolLitNode<'a>),
  FloatLit(FloatLitNode<'a>),
  IntLit(IntLitNode<'a>),
}

  impl<'a> LitNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
BoolLitNode::can_cast(node)
 || FloatLitNode::can_cast(node) || IntLitNode::can_cast(node)
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {

    if let Some(casted_node) = BoolLitNode::cast(node) {
        Some(Self::BoolLit(casted_node))
    } else if let Some(casted_node) = FloatLitNode::cast(node) {
        Some(Self::FloatLit(casted_node))
    } else if let Some(casted_node) = IntLitNode::cast(node) {
        Some(Self::IntLit(casted_node))
    } else { None }
    }
}


#[derive(Clone, Debug)]
    pub enum SetTargetNode<'a> {
  SetArrayIdx(SetArrayIdxNode<'a>),
  SetStructIdx(SetStructIdxNode<'a>),
  SetVar(SetVarNode<'a>),
}

  impl<'a> SetTargetNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
SetArrayIdxNode::can_cast(node)
 || SetStructIdxNode::can_cast(node) || SetVarNode::can_cast(node)
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {

    if let Some(casted_node) = SetArrayIdxNode::cast(node) {
        Some(Self::SetArrayIdx(casted_node))
    } else if let Some(casted_node) = SetStructIdxNode::cast(node) {
        Some(Self::SetStructIdx(casted_node))
    } else if let Some(casted_node) = SetVarNode::cast(node) {
        Some(Self::SetVar(casted_node))
    } else { None }
    }
}


#[derive(Clone, Debug)]
    pub enum ToplevelNode<'a> {
  TopFunc(TopFuncNode<'a>),
  TopImport(TopImportNode<'a>),
  TopLet(TopLetNode<'a>),
  TopStruct(TopStructNode<'a>),
}

  impl<'a> ToplevelNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
TopFuncNode::can_cast(node)
 || TopImportNode::can_cast(node) || TopLetNode::can_cast(node) || TopStructNode::can_cast(node)
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {

    if let Some(casted_node) = TopFuncNode::cast(node) {
        Some(Self::TopFunc(casted_node))
    } else if let Some(casted_node) = TopImportNode::cast(node) {
        Some(Self::TopImport(casted_node))
    } else if let Some(casted_node) = TopLetNode::cast(node) {
        Some(Self::TopLet(casted_node))
    } else if let Some(casted_node) = TopStructNode::cast(node) {
        Some(Self::TopStruct(casted_node))
    } else { None }
    }
}


#[derive(Clone, Debug)]
    pub enum TypeNode<'a> {
  TyArray(TyArrayNode<'a>),
  TyBool(TyBoolNode<'a>),
  TyF32(TyF32Node<'a>),
  TyFunc(TyFuncNode<'a>),
  TyI32(TyI32Node<'a>),
  TyStruct(TyStructNode<'a>),
  TyUnit(TyUnitNode<'a>),
}

  impl<'a> TypeNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
TyArrayNode::can_cast(node)
 || TyBoolNode::can_cast(node) || TyF32Node::can_cast(node) || TyFuncNode::can_cast(node) || TyI32Node::can_cast(node) || TyStructNode::can_cast(node) || TyUnitNode::can_cast(node)
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {

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
    } else { None }
    }
}


#[derive(Clone, Copy, Debug)]
pub struct ArrayENode<'a>(pub Node<'a>);

  impl<'a> ArrayENode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "array_e"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> ArrayENode<'a> {

            pub fn exprs(&self) -> Vec<ExprNode<'a>> {
                let mut cursor = self.0.walk();
                self.0
                  .children(&mut cursor)
                  .filter_map(ExprNode::cast).collect()
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct ArrayIdxENode<'a>(pub Node<'a>);

  impl<'a> ArrayIdxENode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "array_idx_e"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> ArrayIdxENode<'a> {

            pub fn array(&self) -> Option<ExprNode<'a>> {
                self.0.child_by_field_name("array").and_then(ExprNode::cast)
            }
            

            pub fn index(&self) -> Option<ExprNode<'a>> {
                self.0.child_by_field_name("index").and_then(ExprNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct BinaryENode<'a>(pub Node<'a>);

  impl<'a> BinaryENode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "binary_e"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> BinaryENode<'a> {

            pub fn left(&self) -> Option<ExprNode<'a>> {
                self.0.child_by_field_name("left").and_then(ExprNode::cast)
            }
            

            pub fn op(&self) -> Option<Node<'a>> {
                self.0.child_by_field_name("op")
            }
            

            pub fn right(&self) -> Option<ExprNode<'a>> {
                self.0.child_by_field_name("right").and_then(ExprNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct BlockENode<'a>(pub Node<'a>);

  impl<'a> BlockENode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "block_e"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> BlockENode<'a> {

            pub fn block_decls(&self) -> Vec<DeclNode<'a>> {
                let mut cursor = self.0.walk();
                self.0
                  .children_by_field_name("block_decl", &mut cursor)
                  .filter_map(DeclNode::cast).collect()
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct BoolLitNode<'a>(pub Node<'a>);

  impl<'a> BoolLitNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "bool_lit"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> BoolLitNode<'a> {
}

#[derive(Clone, Copy, Debug)]
pub struct CallArgsNode<'a>(pub Node<'a>);

  impl<'a> CallArgsNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "call_args"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> CallArgsNode<'a> {

            pub fn exprs(&self) -> Vec<ExprNode<'a>> {
                let mut cursor = self.0.walk();
                self.0
                  .children(&mut cursor)
                  .filter_map(ExprNode::cast).collect()
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct CallENode<'a>(pub Node<'a>);

  impl<'a> CallENode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "call_e"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> CallENode<'a> {

            pub fn arguments(&self) -> Option<CallArgsNode<'a>> {
                self.0.child_by_field_name("arguments").and_then(CallArgsNode::cast)
            }
            

            pub fn function(&self) -> Option<CalleeNode<'a>> {
                self.0.child_by_field_name("function").and_then(CalleeNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct CommentNode<'a>(pub Node<'a>);

  impl<'a> CommentNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "comment"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> CommentNode<'a> {
}

#[derive(Clone, Copy, Debug)]
pub struct ExprDeclNode<'a>(pub Node<'a>);

  impl<'a> ExprDeclNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "expr_decl"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> ExprDeclNode<'a> {

            pub fn expr(&self) -> Option<ExprNode<'a>> {
                self.0.child_by_field_name("expr").and_then(ExprNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct FuncParamNode<'a>(pub Node<'a>);

  impl<'a> FuncParamNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "func_param"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> FuncParamNode<'a> {

            pub fn name(&self) -> Option<LowerIdentNode<'a>> {
                self.0.child_by_field_name("name").and_then(LowerIdentNode::cast)
            }
            

            pub fn type_(&self) -> Option<TypeNode<'a>> {
                self.0.child_by_field_name("type").and_then(TypeNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct FuncParamsNode<'a>(pub Node<'a>);

  impl<'a> FuncParamsNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "func_params"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> FuncParamsNode<'a> {

            pub fn func_params(&self) -> Vec<FuncParamNode<'a>> {
                let mut cursor = self.0.walk();
                self.0
                  .children(&mut cursor)
                  .filter_map(FuncParamNode::cast).collect()
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct IfENode<'a>(pub Node<'a>);

  impl<'a> IfENode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "if_e"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> IfENode<'a> {

            pub fn condition(&self) -> Option<ExprNode<'a>> {
                self.0.child_by_field_name("condition").and_then(ExprNode::cast)
            }
            

            pub fn else_(&self) -> Option<BlockENode<'a>> {
                self.0.child_by_field_name("else").and_then(BlockENode::cast)
            }
            

            pub fn then(&self) -> Option<BlockENode<'a>> {
                self.0.child_by_field_name("then").and_then(BlockENode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct IntLitNode<'a>(pub Node<'a>);

  impl<'a> IntLitNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "int_lit"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> IntLitNode<'a> {
}

#[derive(Clone, Copy, Debug)]
pub struct IntrinsicENode<'a>(pub Node<'a>);

  impl<'a> IntrinsicENode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "intrinsic_e"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> IntrinsicENode<'a> {

            pub fn arguments(&self) -> Option<CallArgsNode<'a>> {
                self.0.child_by_field_name("arguments").and_then(CallArgsNode::cast)
            }
            

            pub fn function(&self) -> Option<IntrinsicIdentNode<'a>> {
                self.0.child_by_field_name("function").and_then(IntrinsicIdentNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct LetDeclNode<'a>(pub Node<'a>);

  impl<'a> LetDeclNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "let_decl"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> LetDeclNode<'a> {

            pub fn annotation(&self) -> Option<TypeNode<'a>> {
                self.0.child_by_field_name("annotation").and_then(TypeNode::cast)
            }
            

            pub fn binder(&self) -> Option<LowerIdentNode<'a>> {
                self.0.child_by_field_name("binder").and_then(LowerIdentNode::cast)
            }
            

            pub fn expr(&self) -> Option<ExprNode<'a>> {
                self.0.child_by_field_name("expr").and_then(ExprNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct ParenthesizedENode<'a>(pub Node<'a>);

  impl<'a> ParenthesizedENode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "parenthesized_e"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> ParenthesizedENode<'a> {

            pub fn expr(&self) -> Option<ExprNode<'a>> {
                self.0.child_by_field_name("expr").and_then(ExprNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct SetArrayIdxNode<'a>(pub Node<'a>);

  impl<'a> SetArrayIdxNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "set_array_idx"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> SetArrayIdxNode<'a> {

            pub fn index(&self) -> Option<ExprNode<'a>> {
                self.0.child_by_field_name("index").and_then(ExprNode::cast)
            }
            

            pub fn target(&self) -> Option<SetTargetNode<'a>> {
                self.0.child_by_field_name("target").and_then(SetTargetNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct SetDeclNode<'a>(pub Node<'a>);

  impl<'a> SetDeclNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "set_decl"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> SetDeclNode<'a> {

            pub fn expr(&self) -> Option<ExprNode<'a>> {
                self.0.child_by_field_name("expr").and_then(ExprNode::cast)
            }
            

            pub fn target(&self) -> Option<SetTargetNode<'a>> {
                self.0.child_by_field_name("target").and_then(SetTargetNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct SetStructIdxNode<'a>(pub Node<'a>);

  impl<'a> SetStructIdxNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "set_struct_idx"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> SetStructIdxNode<'a> {

            pub fn index(&self) -> Option<LowerIdentNode<'a>> {
                self.0.child_by_field_name("index").and_then(LowerIdentNode::cast)
            }
            

            pub fn target(&self) -> Option<SetTargetNode<'a>> {
                self.0.child_by_field_name("target").and_then(SetTargetNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct SetVarNode<'a>(pub Node<'a>);

  impl<'a> SetVarNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "set_var"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> SetVarNode<'a> {

            pub fn name(&self) -> Option<LowerIdentNode<'a>> {
                self.0.child_by_field_name("name").and_then(LowerIdentNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct SourceFileNode<'a>(pub Node<'a>);

  impl<'a> SourceFileNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "source_file"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> SourceFileNode<'a> {

            pub fn toplevels(&self) -> Vec<ToplevelNode<'a>> {
                let mut cursor = self.0.walk();
                self.0
                  .children(&mut cursor)
                  .filter_map(ToplevelNode::cast).collect()
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct StructENode<'a>(pub Node<'a>);

  impl<'a> StructENode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "struct_e"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> StructENode<'a> {

            pub fn struct_(&self) -> Option<UpperIdentNode<'a>> {
                self.0.child_by_field_name("struct").and_then(UpperIdentNode::cast)
            }
            

            pub fn struct_field_es(&self) -> Vec<StructFieldENode<'a>> {
                let mut cursor = self.0.walk();
                self.0
                  .children(&mut cursor)
                  .filter_map(StructFieldENode::cast).collect()
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct StructFieldENode<'a>(pub Node<'a>);

  impl<'a> StructFieldENode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "struct_field_e"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> StructFieldENode<'a> {

            pub fn expr(&self) -> Option<ExprNode<'a>> {
                self.0.child_by_field_name("expr").and_then(ExprNode::cast)
            }
            

            pub fn name(&self) -> Option<LowerIdentNode<'a>> {
                self.0.child_by_field_name("name").and_then(LowerIdentNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct StructFieldTopNode<'a>(pub Node<'a>);

  impl<'a> StructFieldTopNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "struct_field_top"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> StructFieldTopNode<'a> {

            pub fn name(&self) -> Option<LowerIdentNode<'a>> {
                self.0.child_by_field_name("name").and_then(LowerIdentNode::cast)
            }
            

            pub fn type_(&self) -> Option<TypeNode<'a>> {
                self.0.child_by_field_name("type").and_then(TypeNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct StructIdxENode<'a>(pub Node<'a>);

  impl<'a> StructIdxENode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "struct_idx_e"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> StructIdxENode<'a> {

            pub fn expr(&self) -> Option<ExprNode<'a>> {
                self.0.child_by_field_name("expr").and_then(ExprNode::cast)
            }
            

            pub fn index(&self) -> Option<LowerIdentNode<'a>> {
                self.0.child_by_field_name("index").and_then(LowerIdentNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct TopFuncNode<'a>(pub Node<'a>);

  impl<'a> TopFuncNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "top_func"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> TopFuncNode<'a> {

            pub fn body(&self) -> Option<BlockENode<'a>> {
                self.0.child_by_field_name("body").and_then(BlockENode::cast)
            }
            

            pub fn name(&self) -> Option<LowerIdentNode<'a>> {
                self.0.child_by_field_name("name").and_then(LowerIdentNode::cast)
            }
            

            pub fn params(&self) -> Option<FuncParamsNode<'a>> {
                self.0.child_by_field_name("params").and_then(FuncParamsNode::cast)
            }
            

            pub fn result(&self) -> Option<TypeNode<'a>> {
                self.0.child_by_field_name("result").and_then(TypeNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct TopImportNode<'a>(pub Node<'a>);

  impl<'a> TopImportNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "top_import"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> TopImportNode<'a> {

            pub fn external(&self) -> Option<LowerIdentNode<'a>> {
                self.0.child_by_field_name("external").and_then(LowerIdentNode::cast)
            }
            

            pub fn internal(&self) -> Option<LowerIdentNode<'a>> {
                self.0.child_by_field_name("internal").and_then(LowerIdentNode::cast)
            }
            

            pub fn type_(&self) -> Option<TyFuncNode<'a>> {
                self.0.child_by_field_name("type").and_then(TyFuncNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct TopLetNode<'a>(pub Node<'a>);

  impl<'a> TopLetNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "top_let"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> TopLetNode<'a> {

            pub fn annotation(&self) -> Option<TypeNode<'a>> {
                self.0.child_by_field_name("annotation").and_then(TypeNode::cast)
            }
            

            pub fn binder(&self) -> Option<LowerIdentNode<'a>> {
                self.0.child_by_field_name("binder").and_then(LowerIdentNode::cast)
            }
            

            pub fn expr(&self) -> Option<ExprNode<'a>> {
                self.0.child_by_field_name("expr").and_then(ExprNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct TopStructNode<'a>(pub Node<'a>);

  impl<'a> TopStructNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "top_struct"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> TopStructNode<'a> {

            pub fn name(&self) -> Option<UpperIdentNode<'a>> {
                self.0.child_by_field_name("name").and_then(UpperIdentNode::cast)
            }
            

            pub fn struct_field_tops(&self) -> Vec<StructFieldTopNode<'a>> {
                let mut cursor = self.0.walk();
                self.0
                  .children(&mut cursor)
                  .filter_map(StructFieldTopNode::cast).collect()
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct TyArrayNode<'a>(pub Node<'a>);

  impl<'a> TyArrayNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "ty_array"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> TyArrayNode<'a> {

            pub fn elem_ty(&self) -> Option<TypeNode<'a>> {
                self.0.child_by_field_name("elem_ty").and_then(TypeNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct TyFuncNode<'a>(pub Node<'a>);

  impl<'a> TyFuncNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "ty_func"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> TyFuncNode<'a> {

            pub fn arguments(&self) -> Vec<TypeNode<'a>> {
                let mut cursor = self.0.walk();
                self.0
                  .children_by_field_name("argument", &mut cursor)
                  .filter_map(TypeNode::cast).collect()
            }
            

            pub fn result(&self) -> Option<TypeNode<'a>> {
                self.0.child_by_field_name("result").and_then(TypeNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct TyStructNode<'a>(pub Node<'a>);

  impl<'a> TyStructNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "ty_struct"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> TyStructNode<'a> {

            pub fn upper_ident(&self) -> Option<UpperIdentNode<'a>> {
                let mut cursor = self.0.walk();
                let child = self.0
                  .children(&mut cursor)
                  .find_map(UpperIdentNode::cast);
                child
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct VarENode<'a>(pub Node<'a>);

  impl<'a> VarENode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "var_e"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> VarENode<'a> {

            pub fn lower_ident(&self) -> Option<LowerIdentNode<'a>> {
                let mut cursor = self.0.walk();
                let child = self.0
                  .children(&mut cursor)
                  .find_map(LowerIdentNode::cast);
                child
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct WhileDeclNode<'a>(pub Node<'a>);

  impl<'a> WhileDeclNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "while_decl"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> WhileDeclNode<'a> {

            pub fn body(&self) -> Option<BlockENode<'a>> {
                self.0.child_by_field_name("body").and_then(BlockENode::cast)
            }
            

            pub fn condition(&self) -> Option<ExprNode<'a>> {
                self.0.child_by_field_name("condition").and_then(ExprNode::cast)
            }
            
}

#[derive(Clone, Copy, Debug)]
pub struct FloatLitNode<'a>(pub Node<'a>);

  impl<'a> FloatLitNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "float_lit"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> FloatLitNode<'a> {
}

#[derive(Clone, Copy, Debug)]
pub struct IntrinsicIdentNode<'a>(pub Node<'a>);

  impl<'a> IntrinsicIdentNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "intrinsic_ident"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> IntrinsicIdentNode<'a> {
}

#[derive(Clone, Copy, Debug)]
pub struct LowerIdentNode<'a>(pub Node<'a>);

  impl<'a> LowerIdentNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "lower_ident"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> LowerIdentNode<'a> {
}

#[derive(Clone, Copy, Debug)]
pub struct TyBoolNode<'a>(pub Node<'a>);

  impl<'a> TyBoolNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "ty_bool"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> TyBoolNode<'a> {
}

#[derive(Clone, Copy, Debug)]
pub struct TyF32Node<'a>(pub Node<'a>);

  impl<'a> TyF32Node<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "ty_f32"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> TyF32Node<'a> {
}

#[derive(Clone, Copy, Debug)]
pub struct TyI32Node<'a>(pub Node<'a>);

  impl<'a> TyI32Node<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "ty_i32"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> TyI32Node<'a> {
}

#[derive(Clone, Copy, Debug)]
pub struct TyUnitNode<'a>(pub Node<'a>);

  impl<'a> TyUnitNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "ty_unit"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> TyUnitNode<'a> {
}

#[derive(Clone, Copy, Debug)]
pub struct UpperIdentNode<'a>(pub Node<'a>);

  impl<'a> UpperIdentNode<'a> {
    pub fn can_cast(node: Node<'a>) -> bool {
        node.kind() == "upper_ident"
    }

    pub fn cast(node: Node<'a>) -> Option<Self> {
        if Self::can_cast(node) {
            Some(Self(node))
        } else {
            None
        }
    }
}


  impl<'a> UpperIdentNode<'a> {
}

