use camino::{Utf8Path, Utf8PathBuf};
use std::{
    cell::{Cell, RefCell},
    fmt,
    num::NonZeroU16,
};
use string_interner::{DefaultStringInterner, DefaultSymbol};

use text_size::TextRange;

use crate::types::Interface;

// Indexed by ModuleId
#[derive(Debug)]
pub struct Ctx {
    module_names: Vec<String>,
    module_paths: Vec<Utf8PathBuf>,
    interfaces: Vec<Interface>,
    name_supplies: Vec<NameSupply>,
}

impl Ctx {
    pub fn new(module_count: u16) -> Ctx {
        // Includes the reserved modules
        let all_module_count = (module_count + ModuleId::FIRST_NON_RESERVED.get() - 1) as usize;

        let mut module_names = Vec::with_capacity(all_module_count);
        let mut module_paths = Vec::with_capacity(all_module_count);
        let mut interfaces = Vec::with_capacity(all_module_count);
        let mut name_supplies = Vec::with_capacity(all_module_count);
        for _ in 0..name_supplies.capacity() {
            module_names.push("NOT INITIALIZED".to_string());
            module_paths.push(Utf8Path::new("NOT INITIALIZED").to_path_buf());
            interfaces.push(Interface::default());
            name_supplies.push(NameSupply::new());
        }
        let mut ctx = Ctx {
            module_names,
            module_paths,
            interfaces,
            name_supplies,
        };
        ctx.set_module_name(ModuleId::PRIM, "#prim".to_string());
        ctx.set_module_name(ModuleId::CODEGEN, "#codegen".to_string());
        ctx
    }
    pub fn set_module_name(&mut self, module: ModuleId, name: String) {
        self.module_names[(module.0.get() - 1) as usize] = name
    }
    pub fn set_module_path(&mut self, module: ModuleId, path: Utf8PathBuf) {
        self.module_paths[(module.0.get() - 1) as usize] = path
    }
    pub fn set_interface(&mut self, module: ModuleId, interface: Interface) {
        self.interfaces[(module.0.get() - 1) as usize] = interface
    }
    pub fn set_name_supply(&mut self, module: ModuleId, supply: NameSupply) {
        self.name_supplies[(module.0.get() - 1) as usize] = supply
    }
    pub fn get_module_name(&self, module: ModuleId) -> &str {
        &self.module_names[(module.0.get() - 1) as usize]
    }
    pub fn get_module_path(&self, module: ModuleId) -> &Utf8Path {
        &self.module_paths[(module.0.get() - 1) as usize]
    }
    pub fn get_interface(&self, module: ModuleId) -> &Interface {
        &self.interfaces[(module.0.get() - 1) as usize]
    }
    pub fn get_name_supply(&self, module: ModuleId) -> &NameSupply {
        &self.name_supplies[(module.0.get() - 1) as usize]
    }
    pub fn resolve(&self, name: Name) -> (String, &Utf8Path, TextRange) {
        let supply = self.get_name_supply(name.module);
        let id = supply.lookup(name);
        (
            supply.lookup_symbol(id.it),
            self.get_module_path(name.module),
            id.at,
        )
    }
    pub fn display_qualified_name(&self, name: Name) -> String {
        format!(
            "{}::{}",
            self.get_module_name(name.module),
            self.resolve(name).0
        )
    }
    pub fn display_name(&self, name: Name) -> String {
        self.resolve(name).0
    }
    pub fn fmt_name(&self, f: &mut fmt::Formatter<'_>, name: Name) -> fmt::Result {
        write!(f, "{}", self.display_name(name))
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Id {
    pub it: String,
    pub at: TextRange,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum NameTag {
    Local,
    Global,
    Func,
    Type,
    TypeVar,
    Field,
    Gen,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct ModuleId(NonZeroU16);

impl ModuleId {
    pub fn new(id: u16) -> Self {
        debug_assert!(id > 2, "module ids 1 and 2 are reserved");
        Self(NonZeroU16::new(id).expect("ModuleId must be non-zero"))
    }

    pub const PRIM: Self = ModuleId(NonZeroU16::new(1).unwrap());
    pub const CODEGEN: Self = ModuleId(NonZeroU16::new(2).unwrap());
    const FIRST_NON_RESERVED: NonZeroU16 = NonZeroU16::new(3).unwrap();
}

impl From<ModuleId> for u16 {
    fn from(val: ModuleId) -> Self {
        val.0.get()
    }
}

pub struct ModuleIdGen(NonZeroU16);
impl ModuleIdGen {
    pub fn new() -> Self {
        Self(ModuleId::FIRST_NON_RESERVED)
    }
    pub fn next_id(&mut self) -> ModuleId {
        let tmp = ModuleId(self.0);
        self.0 = self.0.checked_add(1).unwrap();
        tmp
    }
}

impl Default for ModuleIdGen {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Name {
    pub tag: NameTag,
    pub module: ModuleId,
    pub idx: u32,
}

pub type Symbol = DefaultSymbol;

#[derive(Debug, Copy, Clone)]
pub struct CompactId {
    pub it: Symbol,
    pub at: TextRange,
}

#[derive(Debug)]
pub struct NameSupply {
    supply: Cell<u32>,
    name_map: RefCell<Vec<CompactId>>,
    strings: RefCell<DefaultStringInterner>,
}

impl Default for NameSupply {
    fn default() -> Self {
        Self::new()
    }
}

impl NameSupply {
    pub fn new() -> Self {
        Self {
            supply: Cell::new(0),
            name_map: RefCell::new(Vec::new()),
            strings: RefCell::new(DefaultStringInterner::default()),
        }
    }
    pub fn new_name(
        &self,
        tag: NameTag,
        module: ModuleId,
        it: &str,
        at: TextRange,
    ) -> (Name, Symbol) {
        let idx = self.supply.get();
        self.supply.set(idx + 1);
        let it = self.strings.borrow_mut().get_or_intern(it);
        self.name_map.borrow_mut().push(CompactId { it, at });
        (Name { tag, module, idx }, it)
    }
    pub fn local_idx(&self, module: ModuleId, it: &str, at: TextRange) -> (Name, Symbol) {
        self.new_name(NameTag::Local, module, it, at)
    }
    pub fn global_idx(&self, module: ModuleId, it: &str, at: TextRange) -> (Name, Symbol) {
        self.new_name(NameTag::Global, module, it, at)
    }
    pub fn func_idx(&self, module: ModuleId, it: &str, at: TextRange) -> (Name, Symbol) {
        self.new_name(NameTag::Func, module, it, at)
    }
    pub fn type_idx(&self, module: ModuleId, it: &str, at: TextRange) -> (Name, Symbol) {
        self.new_name(NameTag::Type, module, it, at)
    }
    pub fn type_var_idx(&self, module: ModuleId, it: &str, at: TextRange) -> (Name, Symbol) {
        self.new_name(NameTag::TypeVar, module, it, at)
    }
    pub fn field_idx(&self, module: ModuleId, it: &str, at: TextRange) -> (Name, Symbol) {
        self.new_name(NameTag::Field, module, it, at)
    }
    pub fn gen_idx(&self, module: ModuleId, it: &str) -> (Name, Symbol) {
        self.new_name(NameTag::Gen, module, it, TextRange::default())
    }
    pub fn get_or_intern(&self, s: &str) -> Symbol {
        self.strings.borrow_mut().get_or_intern(s)
    }
    pub fn lookup(&self, name: Name) -> CompactId {
        *self.name_map.borrow().get(name.idx as usize).unwrap()
    }
    pub fn lookup_symbol(&self, sym: Symbol) -> String {
        self.strings.borrow().resolve(sym).unwrap().to_string()
    }
}
