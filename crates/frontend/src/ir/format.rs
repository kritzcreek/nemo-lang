use crate::ir::{
    Callee, Declaration, DeclarationData, Expr, ExprData, LitData, Name, NameSupply, Program,
    SetTarget, SetTargetData, Ty,
};
use lexpr::Value;

pub fn format_ir(names: &NameSupply, ir: &Program) -> String {
    format!("{}", Formatter { names }.program(ir))
}

struct Formatter<'a> {
    names: &'a NameSupply,
}

impl Formatter<'_> {
    fn name(&self, n: &Name) -> Value {
        Value::symbol(self.names.lookup(*n).unwrap().it.as_ref())
    }

    fn ty(&self, ty: &Ty) -> Value {
        ty.display(&self.names.name_map).to_string().into()
    }

    fn set_target(&self, set_target: &SetTarget) -> Value {
        match &set_target.it {
            SetTargetData::SetArray { target, index } => Value::list(vec![
                Value::symbol("set_array"),
                self.expr(target),
                self.expr(index),
            ]),
            SetTargetData::SetStruct { target, index } => Value::list(vec![
                Value::symbol("set_struct"),
                self.expr(target),
                self.name(index),
            ]),
            SetTargetData::SetVar { name } => {
                Value::list(vec![Value::symbol("set_var"), self.name(name)])
            }
        }
    }

    fn decl(&self, decl: &Declaration) -> Value {
        match &decl.it {
            DeclarationData::Let { binder, expr } => Value::list(vec![
                Value::symbol("let"),
                self.name(binder),
                self.expr(expr),
            ]),
            DeclarationData::Set { set_target, expr } => Value::list(vec![
                Value::symbol("set"),
                self.set_target(set_target),
                self.expr(expr),
            ]),
            DeclarationData::Expr { expr } => self.expr(expr),
            DeclarationData::While { condition, body } => Value::list(vec![
                Value::symbol("while"),
                self.expr(condition),
                self.expr(body),
            ]),
        }
    }

    fn expr(&self, e: &Expr) -> Value {
        match e.it.as_ref() {
            ExprData::Lit { lit } => match lit.it {
                LitData::I32(i) => i.into(),
                LitData::F32(f) => f.into(),
                LitData::Bool(b) => b.into(),
                LitData::Unit => Value::symbol("unit"),
            },
            ExprData::Var { name } => self.name(name),
            ExprData::Call { func, arguments } => {
                let mut elems: Vec<Value> = vec![];
                match func {
                    Callee::Func { name, type_args } => {
                        elems.push(self.name(name));
                        if !type_args.is_empty() {
                            elems.push(Value::vector(
                                type_args.tys().into_iter().map(|t| self.ty(t)),
                            ))
                        }
                    }
                    Callee::FuncRef(e) => elems.push(self.expr(e)),
                    Callee::Builtin(s) => elems.push(Value::string(*s)),
                }
                elems.extend(arguments.into_iter().map(|e| self.expr(e)));
                Value::list(elems)
            }
            ExprData::Binary { op, left, right } => Value::list(vec![
                Value::symbol(format!("{:?}", op.it)),
                self.expr(left),
                self.expr(right),
            ]),
            ExprData::Array { elems } => {
                let mut els = vec![Value::symbol("array")];
                els.extend(elems.into_iter().map(|e| self.expr(e)));
                Value::list(els)
            }
            ExprData::ArrayIdx { array, index } => Value::list(vec![
                Value::symbol("array_idx"),
                self.expr(array),
                self.expr(index),
            ]),
            ExprData::If {
                condition,
                then_branch,
                else_branch,
            } => Value::list(vec![
                Value::symbol("if"),
                self.expr(condition),
                self.expr(then_branch),
                self.expr(else_branch),
            ]),
            ExprData::Block { declarations, expr } => {
                let mut elems = vec![Value::symbol("block")];
                elems.extend(declarations.into_iter().map(|d| self.decl(d)));
                elems.push(self.expr(expr));
                Value::list(elems)
            }
            ExprData::Struct { name, fields } => {
                let mut elems = vec![self.name(name)];
                elems.extend(
                    fields
                        .into_iter()
                        .map(|(name, expr)| Value::from((self.name(name), self.expr(expr)))),
                );
                Value::list(elems)
            }
            ExprData::StructIdx { expr, index } => Value::list(vec![
                Value::symbol("struct_idx"),
                self.expr(expr),
                self.name(index),
            ]),
            ExprData::Match { .. } => "match".into(),
            ExprData::Lambda {
                captures,
                params,
                return_ty,
                body,
            } => {
                let mut elems = vec![Value::symbol("lambda")];
                elems.push(Value::vector(
                    captures
                        .into_iter()
                        .map(|(n, ty)| Value::from((self.name(n), self.ty(ty)))),
                ));
                elems.push(Value::list(
                    params
                        .into_iter()
                        .map(|(n, ty)| Value::from((self.name(n), self.ty(ty)))),
                ));
                elems.push(self.ty(return_ty));
                elems.push(self.expr(body));
                Value::list(elems)
            }
            ExprData::Return { expr } => Value::list(vec!["return".into(), self.expr(expr)]),
        }
    }
    fn program(&self, program: &Program) -> Value {
        let mut elems = vec![Value::symbol("program")];
        elems.extend(program.funcs.iter().map(|f| self.expr(&f.body)));
        Value::list(elems)
    }
}
