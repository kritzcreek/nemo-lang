use crate::ir::{self, Id, Name, NameMap};
use crate::syntax::SyntaxToken;
use text_size::TextRange;

#[derive(Debug, Clone, Default)]
pub struct NameSupply(ir::NameSupply);

fn token_into_id(tkn: &SyntaxToken) -> Id {
    Id {
        it: tkn.text().to_string(),
        at: tkn.text_range(),
    }
}

impl NameSupply {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn inner(&self) -> &ir::NameSupply {
        &self.0
    }

    pub fn take(self) -> ir::NameSupply {
        self.0
    }

    pub fn local_idx(&mut self, local: &SyntaxToken) -> Name {
        self.0.local_idx(token_into_id(local))
    }

    pub fn global_idx(&mut self, global: &SyntaxToken) -> Name {
        self.0.global_idx(token_into_id(global))
    }

    pub fn func_idx(&mut self, func: &SyntaxToken) -> Name {
        self.0.func_idx(token_into_id(func))
    }

    pub fn type_idx(&mut self, typ: &SyntaxToken) -> Name {
        self.0.type_idx(token_into_id(typ))
    }

    pub fn type_var(&mut self, typ_var: &SyntaxToken) -> Name {
        self.0.type_var(token_into_id(typ_var))
    }

    pub fn field_idx(&mut self, field: &SyntaxToken) -> Name {
        self.0.field_idx(token_into_id(field))
    }

    pub fn start_idx(&mut self) -> Name {
        self.0.func_idx(Id {
            it: "$start".to_string(),
            at: TextRange::default(),
        })
    }

    pub fn name_map(&self) -> &NameMap {
        &self.0.name_map
    }
}
