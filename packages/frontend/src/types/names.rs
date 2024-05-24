use backend::ir::NameMap;
pub use backend::ir::{self, Id, Name};
use rowan::TextRange;

use crate::syntax::SyntaxToken;

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

    pub fn gen_idx(&mut self) -> Name {
        self.0.gen_idx()
    }

    pub fn lookup(&self, name: Name) -> Option<&Id> {
        self.0.lookup(name)
    }

    pub fn name_map(&self) -> &NameMap {
        &self.0.name_map
    }
}
