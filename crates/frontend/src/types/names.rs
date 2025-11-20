use std::sync::Arc;

use lasso::ThreadedRodeo;

use crate::ir::{self, ModuleId, Name, Symbol};
use crate::syntax::SyntaxToken;

#[derive(Debug)]
pub struct NameSupply {
    module_id: ModuleId,
    inner: ir::NameSupply,
    interner: Arc<ThreadedRodeo>,
}

impl NameSupply {
    pub fn new(module: ModuleId, interner: Arc<ThreadedRodeo>) -> Self {
        Self {
            module_id: module,
            inner: ir::NameSupply::new(),
            interner,
        }
    }

    pub fn take(self) -> (ir::NameSupply, ModuleId) {
        (self.inner, self.module_id)
    }

    pub fn local_idx(&self, tkn: &SyntaxToken) -> (Name, Symbol) {
        let symbol = self.get_or_intern(tkn.text());
        let name = self
            .inner
            .local_idx(self.module_id, symbol, tkn.text_range());
        (name, symbol)
    }

    pub fn global_idx(&self, tkn: &SyntaxToken) -> (Name, Symbol) {
        let symbol = self.get_or_intern(tkn.text());
        let name = self
            .inner
            .global_idx(self.module_id, symbol, tkn.text_range());
        (name, symbol)
    }

    pub fn func_idx(&self, tkn: &SyntaxToken) -> (Name, Symbol) {
        let symbol = self.get_or_intern(tkn.text());
        let name = self
            .inner
            .func_idx(self.module_id, symbol, tkn.text_range());
        (name, symbol)
    }

    pub fn type_idx(&self, tkn: &SyntaxToken) -> (Name, Symbol) {
        let symbol = self.get_or_intern(tkn.text());
        let name = self
            .inner
            .type_idx(self.module_id, symbol, tkn.text_range());
        (name, symbol)
    }

    pub fn type_var(&self, tkn: &SyntaxToken) -> (Name, Symbol) {
        let symbol = self.get_or_intern(tkn.text());
        let name = self
            .inner
            .type_var_idx(self.module_id, symbol, tkn.text_range());
        (name, symbol)
    }

    pub fn field_idx(&self, tkn: &SyntaxToken) -> (Name, Symbol) {
        let symbol = self.get_or_intern(tkn.text());
        let name = self
            .inner
            .field_idx(self.module_id, symbol, tkn.text_range());
        (name, symbol)
    }

    pub fn gen_idx(&self, it: &str) -> Name {
        let symbol = self.interner.get_or_intern(it);
        self.inner.gen_idx(self.module_id, symbol)
    }

    pub fn resolve(&self, name: Name) -> ir::Symbol {
        self.inner.lookup(name).it
    }

    pub fn get_or_intern(&self, s: &str) -> ir::Symbol {
        self.interner.get_or_intern(s)
    }
}
