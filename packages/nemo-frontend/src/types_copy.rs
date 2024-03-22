use core::fmt;
use std::collections::HashMap;

use crate::builtins;
use crate::syntax::{
    Declaration, DeclarationData, Expr, ExprData, FuncId, FuncType, FuncTypeData, Id, Intrinsic,
    IntrinsicData, Lit, LitData, Op, OpData, Program, SetTarget, SetTargetData, Span, Toplevel,
    ToplevelData, Type, TypeData,
};
use crate::type_errors::{TyError, TyErrorData};
use crate::types::{FuncTy, Ty, TyResult};
use tree_sitter::Node;
use crate::ast;

pub struct Typechecker<'a> {
    source: &'a [u8],
    structs: HashMap<String, Vec<(Id, Type)>>,
    functions: HashMap<String, FuncTy>,
}

impl<'a> Typechecker<'a> {
    fn new(source: &'a str) -> Typechecker<'a> {
        Typechecker {
            source: source.as_bytes(),
            structs: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn lookup_function(&self, name: &str) -> Option<&FuncTy> {
        match self.functions.get(name) {
            Some(ty) => Some(ty),
            None => match builtins::lookup_builtin(name) {
                Some(f) => Some(&f.ty),
                None => None,
            },
        }
    }

    fn lookup_struct(&self, name: &str, span: &Span) -> TyResult<&[(Id, Type)]> {
        match self.structs.get(name) {
            Some(fields) => Ok(fields),
            None => Err(TyError {
                at: span.clone(),
                it: TyErrorData::UnknownType(name.to_string()),
            }),
        }
    }

    fn infer_lit(&mut self, lit: ast::LitNode<'_>) -> TyResult<Lit> {
        // match lit {
        //     ast::LitNode::BoolLit(b) => Lit { 
        //         it: LitData::Bool(), 
        //         at: b.0.into(),
        //         ty: Ty::Bool 
        //     },
        //     ast::LitNode::FloatLit(_) => todo!(),
        //     ast::LitNode::IntLit(_) => todo!(),
        // };
        todo!()
    }

    fn infer_expr(&mut self, expr: ast::ExprNode<'_>) -> TyResult<Expr> {
        match expr {
            ast::ExprNode::Lit(lit) => {
                let lit = self.infer_lit(lit);
                todo!()
            }
            ast::ExprNode::ArrayE(_) => todo!(),
            ast::ExprNode::ArrayIdxE(_) => todo!(),
            ast::ExprNode::BinaryE(_) => todo!(),
            ast::ExprNode::BlockE(_) => todo!(),
            ast::ExprNode::CallE(_) => todo!(),
            ast::ExprNode::IfE(_) => todo!(),
            ast::ExprNode::IntrinsicE(_) => todo!(),
            ast::ExprNode::ParenthesizedE(_) => todo!(),
            ast::ExprNode::StructE(_) => todo!(),
            ast::ExprNode::StructIdxE(_) => todo!(),
            ast::ExprNode::VarE(_) => todo!(),
        }
    }

    fn infer_prog(mut self, prog: ast::SourceFileNode<'_>) -> TyResult<Program> {
        todo!()
    }
}

pub fn typecheck(source: &str, program: Node<'_>) -> TyResult<Program> {
    let typechecker = Typechecker::new(source);
    let prog = ast::SourceFileNode::cast(program).expect("Expected a source file node");
    typechecker.infer_prog(prog)
}
