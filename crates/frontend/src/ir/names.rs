use crate::types::Interface;
use camino::{Utf8Path, Utf8PathBuf};
use lasso::{Spur, ThreadedRodeo};
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt,
    num::NonZeroU16,
    sync::{atomic::AtomicU16, Arc, OnceLock},
};
use text_size::TextRange;

#[derive(Debug)]
pub struct ModuleData {
    name: String,
    path: Utf8PathBuf,
    interface: Interface,
    names: NameData,
}

#[derive(Debug)]
pub struct NameData(Vec<CompactId>);
impl NameData {
    pub fn lookup(&self, name: Name) -> CompactId {
        *self.0.get(name.idx as usize).unwrap()
    }
}

// Indexed by ModuleId. Each module is set exactly once. Any read for a module that hasn't been set yet will panic.
#[derive(Debug)]
pub struct Ctx {
    datas: Vec<OnceLock<ModuleData>>,
    module_lookup: HashMap<String, ModuleId>,
    interner: Arc<ThreadedRodeo>,
}

impl Ctx {
    pub fn new(module_lookup: HashMap<String, ModuleId>) -> Ctx {
        let module_count = module_lookup.len() as u16;
        let interner = Arc::new(ThreadedRodeo::default());
        // Includes the reserved modules
        let all_module_count = (module_count + ModuleId::FIRST_NON_RESERVED.get() - 1) as usize;
        let mut datas = Vec::with_capacity(all_module_count);
        for _ in 0..datas.capacity() {
            datas.push(OnceLock::new());
        }
        let ctx = Ctx {
            datas,
            module_lookup,
            interner,
        };
        ctx.set_module(
            ModuleId::PRIM,
            "#builtin".to_string(),
            Utf8Path::new("<builtin>").to_path_buf(),
            Interface::default(),
            NameSupply::new(),
        );
        ctx
    }

    pub fn set_module(
        &self,
        module: ModuleId,
        name: String,
        path: Utf8PathBuf,
        interface: Interface,
        supply: NameSupply,
    ) {
        let data = ModuleData {
            name,
            path,
            interface,
            names: NameData(supply.name_map.take()),
        };
        self.datas[(module.0.get() - 1) as usize]
            .set(data)
            .expect("Tried to set the same module twice")
    }

    pub fn get_module_id(&self, module: &str) -> Option<ModuleId> {
        self.module_lookup.get(module).copied()
    }

    pub fn get_module(&self, module: ModuleId) -> &ModuleData {
        self.datas[(module.0.get() - 1) as usize].get().unwrap()
    }

    pub fn get_module_name(&self, module: ModuleId) -> &str {
        &self.get_module(module).name
    }

    pub fn get_module_path(&self, module: ModuleId) -> &Utf8Path {
        &self.get_module(module).path
    }

    pub fn get_interface(&self, module: ModuleId) -> &Interface {
        &self.get_module(module).interface
    }

    pub fn get_names(&self, module: ModuleId) -> &NameData {
        &self.get_module(module).names
    }

    pub fn get_interner(&self) -> Arc<ThreadedRodeo> {
        Arc::clone(&self.interner)
    }

    pub fn resolve(&self, name: Name) -> (String, &Utf8Path, TextRange) {
        let supply = self.get_names(name.module);
        let id = supply.lookup(name);
        (
            self.interner.resolve(&id.it).to_owned(),
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

pub struct ModuleIdGen(AtomicU16);
impl ModuleIdGen {
    pub fn new() -> Self {
        Self(AtomicU16::new(ModuleId::FIRST_NON_RESERVED.into()))
    }
    pub fn next_id(&self) -> ModuleId {
        let new_id = self.0.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        // Safety: Starts out at a value > 0 and only ever increases
        ModuleId(unsafe { NonZeroU16::new_unchecked(new_id) })
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

pub type Symbol = Spur;

#[derive(Debug, Copy, Clone)]
pub struct CompactId {
    pub it: Symbol,
    pub at: TextRange,
}

#[derive(Debug)]
pub struct NameSupply {
    supply: Cell<u32>,
    name_map: RefCell<Vec<CompactId>>,
}

impl Default for NameSupply {
    fn default() -> NameSupply {
        NameSupply::new()
    }
}

impl NameSupply {
    pub fn new() -> Self {
        Self {
            supply: Cell::new(0),
            name_map: RefCell::new(Vec::new()),
        }
    }
    pub fn new_name(&self, tag: NameTag, module: ModuleId, it: Symbol, at: TextRange) -> Name {
        let idx = self.supply.get();
        self.supply.set(idx + 1);
        self.name_map.borrow_mut().push(CompactId { it, at });
        Name { tag, module, idx }
    }
    pub fn local_idx(&self, module: ModuleId, it: Symbol, at: TextRange) -> Name {
        self.new_name(NameTag::Local, module, it, at)
    }
    pub fn global_idx(&self, module: ModuleId, it: Symbol, at: TextRange) -> Name {
        self.new_name(NameTag::Global, module, it, at)
    }
    pub fn func_idx(&self, module: ModuleId, it: Symbol, at: TextRange) -> Name {
        self.new_name(NameTag::Func, module, it, at)
    }
    pub fn type_idx(&self, module: ModuleId, it: Symbol, at: TextRange) -> Name {
        self.new_name(NameTag::Type, module, it, at)
    }
    pub fn type_var_idx(&self, module: ModuleId, it: Symbol, at: TextRange) -> Name {
        self.new_name(NameTag::TypeVar, module, it, at)
    }
    pub fn field_idx(&self, module: ModuleId, it: Symbol, at: TextRange) -> Name {
        self.new_name(NameTag::Field, module, it, at)
    }
    pub fn gen_idx(&self, module: ModuleId, it: Symbol) -> Name {
        self.new_name(NameTag::Gen, module, it, TextRange::default())
    }
    pub fn lookup(&self, name: Name) -> CompactId {
        *self.name_map.borrow().get(name.idx as usize).unwrap()
    }
}
