use std::collections::HashMap;
use std::iter;

use crate::ir::{FuncTy, Id, Import, Name, NameMap, Struct, Ty, Variant};
use wasm_encoder::{
    self, ArrayType, CodeSection, CompositeType, ConstExpr, EntityType, ExportKind, ExportSection,
    FieldType, FuncType, Function, FunctionSection, GlobalSection, GlobalType, HeapType,
    ImportSection, IndirectNameMap, Instruction, Module, NameMap as WasmNameMap, NameSection,
    RefType, StartSection, StorageType, StructType, SubType, TypeSection, ValType,
};

type FuncIdx = u32;
type TypeIdx = u32;
type FieldIdx = u32;
type GlobalIdx = u32;
type LocalIdx = u32;

pub struct FuncData<'a> {
    index: FuncIdx,
    ty: TypeIdx,
    locals: Option<FnLocals>,
    body: Option<Vec<Instruction<'a>>>,
}

pub struct GlobalData {
    index: GlobalIdx,
    name: Name,
    ty: GlobalType,
    init: ConstExpr,
}

pub struct ImportData {
    index: FuncIdx,
    ty_idx: TypeIdx,
    ns: String,
    func: String,
}

pub struct Export {
    name: String,
    desc: (ExportKind, u32),
}

#[derive(Debug)]
pub struct StructInfo {
    pub type_idx: TypeIdx,
    pub fields: Vec<Name>,
    pub variant_tag: Option<i32>,
}

#[derive(Debug)]
pub struct VariantInfo {
    pub type_idx: TypeIdx,
    pub alternatives: Vec<Name>,
}

impl VariantInfo {
    pub fn tag(&self, alternative: Name) -> i32 {
        self.alternatives
            .iter()
            .position(|a| *a == alternative)
            .expect("unknown alternative") as i32
    }
}

pub struct Builder<'a> {
    name_map: &'a NameMap,
    funcs: HashMap<Name, FuncData<'a>>,
    globals: HashMap<Name, GlobalData>,
    types: Vec<SubType>,
    imports: HashMap<Name, ImportData>,
    exports: Vec<Export>,
    start_fn: Option<FuncIdx>,
    // Stores the type index for all array types we've declared so far.
    // Uses the arrays _ELEM TYPE_ as the key
    arrays: HashMap<ValType, TypeIdx>,
    func_tys: HashMap<FuncType, TypeIdx>,
    structs: HashMap<Name, StructInfo>,
    variants: HashMap<Name, VariantInfo>,
    fields: HashMap<Name, (TypeIdx, FieldIdx)>,
}

impl<'a> Builder<'a> {
    pub fn new(name_map: &'a HashMap<Name, Id>) -> Builder<'a> {
        Builder {
            name_map,
            funcs: HashMap::new(),
            globals: HashMap::new(),
            types: vec![],
            arrays: HashMap::new(),
            func_tys: HashMap::new(),
            structs: HashMap::new(),
            variants: HashMap::new(),
            fields: HashMap::new(),
            imports: HashMap::new(),
            exports: vec![],
            start_fn: None,
        }
    }

    fn _print_funcs(&self) {
        for (name, id) in self.name_map {
            if let Name::Func(n) = name {
                eprintln!("$fn:{n} = {id:?}")
            }
        }
    }

    pub fn finish(self) -> Vec<u8> {
        let mut module = Module::new();
        // self._print_funcs();

        // type_section
        let mut type_names = WasmNameMap::new();
        let mut type_section = TypeSection::new();
        for ty in self.types {
            type_section.subtype(&ty);
        }

        for (name, info) in self.structs {
            type_names.append(info.type_idx, &self.name_map.get(&name).unwrap().it);
        }

        for (name, info) in self.variants {
            type_names.append(info.type_idx, &self.name_map.get(&name).unwrap().it);
        }

        let mut field_names = HashMap::new();
        for (name, (ty_idx, field_idx)) in self.fields {
            field_names
                .entry(ty_idx)
                .or_insert(WasmNameMap::new())
                .append(field_idx, &self.name_map.get(&name).unwrap().it);
        }
        let mut all_field_names = IndirectNameMap::new();
        for (ty_idx, names) in field_names {
            all_field_names.append(ty_idx, &names);
        }

        // import_section
        let mut function_names = WasmNameMap::new();

        let mut import_section = ImportSection::new();
        let _import_count = self.imports.len();
        let mut imports: Vec<_> = self.imports.into_iter().collect();
        imports.sort_by_key(|(_, v)| v.index);
        for (name, data) in imports {
            import_section.import(&data.ns, &data.func, EntityType::Function(data.ty_idx));
            function_names.append(data.index, &self.name_map.get(&name).unwrap().it);
        }
        // function_section
        let mut function_section = FunctionSection::new();
        let mut funcs: Vec<_> = self.funcs.into_iter().collect();
        funcs.sort_by_key(|(_, v)| v.index);
        for (name, func) in funcs.iter() {
            function_names.append(func.index, &self.name_map.get(name).unwrap().it);
            function_section.function(func.ty);
        }

        // table_section
        // memory_section
        // global_section
        let mut globals: Vec<GlobalData> = self.globals.into_values().collect();
        globals.sort_by_key(|v| v.index);
        let mut global_section = GlobalSection::new();
        let mut global_names = WasmNameMap::new();
        for data in globals {
            global_section.global(data.ty, &data.init);
            global_names.append(data.index, &self.name_map.get(&data.name).unwrap().it)
        }
        // export_section
        let mut export_section = ExportSection::new();
        for ele in self.exports.iter() {
            export_section.export(&ele.name, ele.desc.0, ele.desc.1);
        }

        // start_section
        let start_section = self
            .start_fn
            .map(|function_index| StartSection { function_index });
        // elem_section
        // code_section
        let mut code_section = CodeSection::new();
        let mut all_local_names = IndirectNameMap::new();
        for (_ix, (_name, func)) in funcs.into_iter().enumerate() {
            let Some(FnLocals { locals, names }) = func.locals else {
                panic!(
                    "No locals for function {}",
                    self.name_map.get(&_name).unwrap().it
                )
            };
            let mut func_body = Function::new_with_locals_types(locals);
            for instr in func.body.unwrap() {
                func_body.instruction(&instr);
            }
            func_body.instruction(&Instruction::End);
            code_section.function(&func_body);

            let mut local_names = WasmNameMap::new();
            for (index, name) in names {
                local_names.append(index, &self.name_map.get(&name).unwrap().it);
            }

            let ix = (_ix + _import_count) as u32;
            all_local_names.append(ix, &local_names);
        }
        // data_section

        // name section
        let mut name_section = NameSection::new();
        name_section.functions(&function_names);
        name_section.locals(&all_local_names);
        name_section.types(&type_names);
        name_section.globals(&global_names);
        name_section.fields(&all_field_names);

        module.section(&type_section);
        module.section(&import_section);
        module.section(&function_section);
        module.section(&global_section);
        module.section(&export_section);
        start_section.map(|s| module.section(&s));
        module.section(&code_section);
        module.section(&name_section);
        module.finish()
    }

    pub fn resolve_name(&self, name: Name) -> Id {
        self.name_map
            .get(&name)
            .cloned()
            .unwrap_or_else(|| panic!("Failed to resolve: {name:?}"))
    }

    pub fn declare_variant(&mut self, ty: Variant) {
        let idx = self.types.len() as u32;
        self.types.push(SubType {
            is_final: false,
            supertype_idx: None,
            composite_type: CompositeType::Struct(StructType {
                fields: vec![FieldType {
                    element_type: StorageType::Val(ValType::I32),
                    mutable: false,
                }]
                .into_boxed_slice(),
            }),
        });
        self.variants.insert(
            ty.name,
            VariantInfo {
                type_idx: idx,
                alternatives: ty.alternatives,
            },
        );
    }

    pub fn declare_struct(&mut self, ty: Struct) {
        let mut supertype_idx = None;
        let mut variant_tag = None;
        let mut fields = if let Some(v) = ty.variant {
            supertype_idx = Some(self.variant_type(v).type_idx);
            variant_tag = Some(self.variant_tag(v, ty.name));

            let mut fields = Vec::with_capacity(ty.fields.len() + 1);
            fields.push(FieldType {
                element_type: StorageType::Val(ValType::I32),
                mutable: false,
            });
            fields
        } else {
            Vec::with_capacity(ty.fields.len())
        };
        let mut field_names = vec![];
        for (name, ty) in ty.fields {
            field_names.push(name);
            let val_ty = self.val_ty(&ty);
            fields.push(FieldType {
                element_type: StorageType::Val(val_ty),
                mutable: true,
            })
        }
        let index = self.types.len() as u32;

        for (field_idx, field_name) in field_names.iter().enumerate() {
            let mut idx = field_idx;
            if ty.variant.is_some() {
                idx += 1
            };
            self.fields.insert(*field_name, (index, idx as u32));
        }

        self.types.push(SubType {
            is_final: true,
            supertype_idx,
            composite_type: CompositeType::Struct(StructType {
                fields: fields.into_boxed_slice(),
            }),
        });

        let struct_info = StructInfo {
            type_idx: index,
            fields: field_names,
            variant_tag,
        };

        self.structs.insert(ty.name, struct_info);
    }

    pub fn func_type(&mut self, ty: &FuncTy) -> TypeIdx {
        let res_ty = self.val_ty(&ty.result);
        let arguments = ty.arguments.iter().map(|t| self.val_ty(t));
        let func_type = FuncType::new(arguments, iter::once(res_ty));
        match self.func_tys.get(&func_type) {
            Some(idx) => *idx,
            None => {
                let idx = self.types.len() as TypeIdx;
                self.types.push(SubType {
                    is_final: true,
                    supertype_idx: None,
                    composite_type: CompositeType::Func(func_type),
                });
                idx
            }
        }
    }

    pub fn variant_tag(&self, variant: Name, alternative: Name) -> i32 {
        self.variant_type(variant).tag(alternative)
    }

    pub fn heap_type(&'a self, name: Name) -> TypeIdx {
        if let Some(v) = self.variants.get(&name) {
            v.type_idx
        } else if let Some(str) = self.structs.get(&name) {
            str.type_idx
        } else {
            panic!("tried to get heap type before declaring it")
        }
    }

    pub fn variant_type(&'a self, name: Name) -> &'a VariantInfo {
        self.variants
            .get(&name)
            .expect("Tried to get variant type before declaring it")
    }

    pub fn struct_type(&'a self, name: Name) -> &'a StructInfo {
        self.structs
            .get(&name)
            .expect("Tried to get struct type before declaring it")
    }

    pub fn val_ty(&mut self, ty: &Ty) -> ValType {
        match ty {
            Ty::F32 => ValType::F32,
            Ty::I32 | Ty::Unit | Ty::Bool => ValType::I32,
            Ty::Struct(s) => {
                let idx = self.heap_type(*s);
                // Could make these non-nullable, but then we'd need separate
                // nullable types for lazily initialized globals
                ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Concrete(idx),
                })
            }
            Ty::Array(el_ty) => {
                let idx = self.array_type_elem(el_ty);
                ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Concrete(idx),
                })
            }
            Ty::Func(func_ty) => {
                let ty_idx = self.func_type(func_ty);
                ValType::Ref(RefType {
                    nullable: false,
                    heap_type: HeapType::Concrete(ty_idx),
                })
            }
            Ty::Any => {
                unreachable!("ANY shouldn't make it into codegen")
            }
        }
    }

    pub fn array_type(&mut self, array_ty: &Ty) -> TypeIdx {
        let elem_ty = match array_ty {
            Ty::Array(elem_ty) => elem_ty,
            t => unreachable!("non-array type passed to array_type: {t:?}"),
        };
        self.array_type_elem(elem_ty)
    }

    pub fn array_type_elem(&mut self, elem_ty: &Ty) -> TypeIdx {
        let elem_val_ty = self.val_ty(elem_ty);
        match self.arrays.get(&elem_val_ty) {
            Some(ix) => *ix,
            None => {
                let ix = self.types.len() as u32;
                self.arrays.insert(elem_val_ty, ix);
                self.types.push(SubType {
                    is_final: true,
                    supertype_idx: None,
                    composite_type: CompositeType::Array(ArrayType(FieldType {
                        element_type: StorageType::Val(elem_val_ty),
                        mutable: true,
                    })),
                });
                ix
            }
        }
    }

    pub fn lookup_field(&self, name: &Name) -> (TypeIdx, FieldIdx) {
        match self.fields.get(name) {
            Some(f) => *f,
            None => panic!("{}", self.resolve_name(*name).it),
        }
    }

    pub fn declare_import(&mut self, import: Import) {
        let index = self.imports.len() as u32;
        let ty_idx = self.func_type(&import.func_ty);
        let import_data = ImportData {
            index,
            ty_idx,
            ns: "env".to_string(),
            func: import.external,
        };
        self.imports.insert(import.internal, import_data);
    }

    pub fn lookup_import(&self, name: &Name) -> Option<FuncIdx> {
        self.imports.get(name).map(|data| data.index)
    }

    pub fn declare_global(&mut self, name: Name, ty: &Ty, init: ConstExpr) -> GlobalIdx {
        let index = self.globals.len() as u32;
        let val_type = self.val_ty(ty);
        let ty = GlobalType {
            val_type,
            shared: false,
            mutable: true,
        };
        let global_data = GlobalData {
            index,
            ty,
            init,
            name,
        };
        self.globals.insert(name, global_data);
        index
    }

    pub fn lookup_global(&self, name: &Name) -> GlobalIdx {
        self.globals.get(name).unwrap().index
    }

    pub fn declare_export(&mut self, name: Name, external: String) {
        let idx = self.lookup_func(&name);
        self.exports.push(Export {
            name: external,
            desc: (ExportKind::Func, idx),
        });
    }

    // TODO: bit a weird reimplementation of a couple functions in here
    // this can be much nicer with StackReps for unit returning functions
    pub fn declare_start(&mut self, name: Name) {
        let index = (self.imports.len() + self.funcs.len()) as u32;
        let ty = self.types.len() as u32;
        self.types.push(SubType {
            is_final: true,
            supertype_idx: None,
            composite_type: CompositeType::Func(FuncType::new(vec![], vec![])),
        });
        self.funcs.insert(
            name,
            FuncData {
                index,
                ty,
                locals: None,
                body: None,
            },
        );
        self.start_fn = Some(index)
    }

    pub fn declare_func(&mut self, name: Name, func_ty: FuncTy) {
        let index = (self.imports.len() + self.funcs.len()) as u32;
        let ty = self.func_type(&func_ty);
        self.funcs.insert(
            name,
            FuncData {
                index,
                ty,
                locals: None,
                body: None,
            },
        );
    }

    pub fn fill_func(&mut self, name: Name, locals: FnLocals, body: Vec<Instruction<'a>>) {
        self.funcs.entry(name).and_modify(|f| {
            f.locals = Some(locals);
            f.body = Some(body)
        });
    }

    pub fn lookup_func(&self, name: &Name) -> FuncIdx {
        match self
            .funcs
            .get(name)
            .map(|f| f.index)
            .or_else(|| self.lookup_import(name))
        {
            Some(f) => f,
            None => panic!(
                "Couldn't find a function for {:?}",
                self.resolve_name(*name)
            ),
        }
    }
}

pub struct BodyBuilder {
    param_count: usize,
    locals: Vec<ValType>,
    named_locals: HashMap<Name, u32>,
}

pub struct FnLocals {
    locals: Vec<ValType>,
    names: HashMap<LocalIdx, Name>,
}

impl BodyBuilder {
    pub fn new(params: Vec<(Name, ValType)>) -> BodyBuilder {
        let param_count = params.len();
        let mut locals = vec![];
        let mut named_locals = HashMap::new();
        for (idx, (name, ty)) in params.into_iter().enumerate() {
            locals.push(ty);
            named_locals.insert(name, idx as u32);
        }
        BodyBuilder {
            param_count,
            locals,
            named_locals,
        }
    }

    pub fn new_local(&mut self, name: Name, ty: ValType) -> LocalIdx {
        let index = self.fresh_local(ty);
        self.named_locals.insert(name, index);
        index
    }

    pub fn fresh_local(&mut self, ty: ValType) -> LocalIdx {
        let index = self.locals.len() as u32;
        self.locals.push(ty);
        index
    }

    pub fn lookup_local(&self, name: &Name) -> Option<LocalIdx> {
        self.named_locals.get(name).copied()
    }

    pub fn get_locals(self) -> FnLocals {
        let mut names = HashMap::new();
        for (k, v) in self.named_locals {
            names.insert(v, k);
        }
        let locals = self.locals[self.param_count..].to_vec();
        FnLocals { locals, names }
    }
}
