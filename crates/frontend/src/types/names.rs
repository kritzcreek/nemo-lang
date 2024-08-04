use crate::ir::{self, Id, ModuleId, Name, NameMap};
use crate::syntax::SyntaxToken;
use text_size::TextRange;

#[derive(Debug, Clone)]
pub struct NameSupply(ir::NameSupply, ModuleId);

fn token_into_id(tkn: &SyntaxToken) -> Id {
    Id {
        it: tkn.text().to_string(),
        at: tkn.text_range(),
    }
}

impl NameSupply {
    pub fn new(module: ModuleId) -> Self {
        Self(ir::NameSupply::default(), module)
    }

    pub fn take(self) -> ir::NameSupply {
        self.0
    }

    pub fn local_idx(&mut self, local: &SyntaxToken) -> Name {
        self.0.local_idx(self.1, token_into_id(local))
    }

    pub fn global_idx(&mut self, global: &SyntaxToken) -> Name {
        self.0.global_idx(self.1, token_into_id(global))
    }

    pub fn func_idx(&mut self, func: &SyntaxToken) -> Name {
        self.0.func_idx(self.1, token_into_id(func))
    }

    pub fn type_idx(&mut self, typ: &SyntaxToken) -> Name {
        self.0.type_idx(self.1, token_into_id(typ))
    }

    pub fn type_var(&mut self, typ_var: &SyntaxToken) -> Name {
        self.0.type_var(self.1, token_into_id(typ_var))
    }

    pub fn field_idx(&mut self, field: &SyntaxToken) -> Name {
        self.0.field_idx(self.1, token_into_id(field))
    }

    pub fn gen_idx(&mut self) -> Name {
        self.0.gen_idx()
    }

    pub fn start_idx(&mut self) -> Name {
        self.0.func_idx(self.1, Id {
            it: "$start".to_string(),
            at: TextRange::default(),
        })
    }

    pub fn name_map(&self) -> &NameMap {
        &self.0.name_map
    }
}
