use crate::ir::{self, ModuleId, Name, Symbol};
use crate::syntax::SyntaxToken;

#[derive(Debug)]
pub struct NameSupply(ir::NameSupply, ModuleId);

impl NameSupply {
    pub fn new(module: ModuleId) -> Self {
        Self(ir::NameSupply::new(), module)
    }

    pub fn take(self) -> (ir::NameSupply, ModuleId) {
        (self.0, self.1)
    }

    pub fn local_idx(&self, tkn: &SyntaxToken) -> (Name, Symbol) {
        self.0.local_idx(self.1, tkn.text(), tkn.text_range())
    }

    pub fn global_idx(&self, tkn: &SyntaxToken) -> (Name, Symbol) {
        self.0.global_idx(self.1, tkn.text(), tkn.text_range())
    }

    pub fn func_idx(&self, tkn: &SyntaxToken) -> (Name, Symbol) {
        self.0.func_idx(self.1, tkn.text(), tkn.text_range())
    }

    pub fn type_idx(&mut self, tkn: &SyntaxToken) -> (Name, Symbol) {
        self.0.type_idx(self.1, tkn.text(), tkn.text_range())
    }

    pub fn type_var(&mut self, tkn: &SyntaxToken) -> (Name, Symbol) {
        self.0.type_var_idx(self.1, tkn.text(), tkn.text_range())
    }

    pub fn field_idx(&self, tkn: &SyntaxToken) -> (Name, Symbol) {
        self.0.field_idx(self.1, tkn.text(), tkn.text_range())
    }

    pub fn gen_idx(&self, it: &str) -> Name {
        self.0.gen_idx(self.1, it).0
    }

    pub fn resolve(&self, name: Name) -> ir::Symbol {
        self.0.lookup(name).it
    }

    pub fn get_or_intern(&self, s: &str) -> ir::Symbol {
        self.0.get_or_intern(s)
    }
}
