use std::collections::HashMap;

pub use nemo_backend::ir::{Id, Name};

use crate::syntax::SyntaxToken;

#[derive(Debug, Clone, Default)]
pub struct NameSupply {
    local: u32,
    global: u32,
    func: u32,
    typ: u32,
    field: u32,
    name_map: HashMap<Name, Id>,
}

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

    pub fn local_idx(&mut self, local: &SyntaxToken) -> Name {
        self.local += 1;
        let name = Name::Local(self.local);
        self.name_map.insert(name, token_into_id(local));
        name
    }

    pub fn global_idx(&mut self, global: &SyntaxToken) -> Name {
        self.global += 1;
        let name = Name::Global(self.global);
        self.name_map.insert(name, token_into_id(global));
        name
    }

    pub fn func_idx(&mut self, func: &SyntaxToken) -> Name {
        self.func += 1;
        let name = Name::Func(self.func);
        self.name_map.insert(name, token_into_id(func));
        name
    }

    pub fn type_idx(&mut self, typ: &SyntaxToken) -> Name {
        self.typ += 1;
        let name = Name::Type(self.typ);
        self.name_map.insert(name, token_into_id(typ));
        name
    }

    pub fn field_idx(&mut self, field: &SyntaxToken) -> Name {
        self.field += 1;
        let name = Name::Field(self.field);
        self.name_map.insert(name, token_into_id(field));
        name
    }

    pub fn lookup(&self, name: Name) -> Option<&Id> {
        self.name_map.get(&name)
    }
}
