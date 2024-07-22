use std::fmt::Write;
use std::iter;
use std::{collections::HashMap, mem};

use frontend::ir::{FuncTy, Id, Import, Name, NameSupply, Struct, Substitution, Ty, Variant};
use text_size::TextRange;
use wasm_encoder::{
    self, ArrayType, CodeSection, CompositeType, ConstExpr, ElementSection, Elements, EntityType,
    ExportKind, ExportSection, FieldType, FuncType, Function, FunctionSection, GlobalSection,
    GlobalType, HeapType, ImportSection, IndirectNameMap, Instruction, Module,
    NameMap as WasmNameMap, NameSection, RefType, StartSection, StorageType, StructType, SubType,
    TypeSection, ValType,
};

type FuncIdx = u32;
type TypeIdx = u32;
type FieldIdx = u32;
type GlobalIdx = u32;
type LocalIdx = u32;

#[derive(Debug)]
pub struct FuncData<'a> {
    index: FuncIdx,
    ty: TypeIdx,
    locals: Option<FnLocals>,
    body: Option<Vec<Instruction<'a>>>,
}

#[derive(Debug)]
pub struct GlobalData {
    index: GlobalIdx,
    name: Name,
    ty: GlobalType,
    init: ConstExpr,
}

#[derive(Debug)]
pub struct ImportData {
    index: FuncIdx,
    ty_idx: TypeIdx,
    ns: String,
    func: String,
}

#[derive(Debug)]
pub struct Export {
    name: String,
    desc: (ExportKind, u32),
}

#[derive(Debug, Clone)]
pub struct StructInfo {
    // TODO Vec<ValTy>
    pub instances: HashMap<Vec<Ty>, TypeIdx>,
    pub definition: Struct,
}

impl StructInfo {
    pub fn field_idx(&self, name: Name) -> FieldIdx {
        let offset = if self.definition.variant.is_some() {
            1
        } else {
            0
        };
        self.definition
            .fields
            .iter()
            .position(|(f, _)| *f == name)
            .unwrap() as u32
            + offset
    }
}

#[derive(Debug, Clone)]
pub struct VariantInfo {
    pub instances: HashMap<Vec<Ty>, TypeIdx>,
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

#[derive(Debug, Clone, Copy)]
pub struct ClosureInfo {
    pub closure_struct_ty: TypeIdx,
    pub closure_func_ty: TypeIdx,
}

#[derive(Debug)]
pub struct Builder<'a> {
    pub(crate) name_supply: NameSupply,
    funcs: HashMap<Name, FuncData<'a>>,
    globals: HashMap<Name, GlobalData>,
    types: Vec<SubType>,
    imports: HashMap<Name, ImportData>,
    exports: Vec<Export>,
    start_fn: Option<FuncIdx>,
    substitution: Substitution,
    // Stores the type index for all array types we've declared so far.
    // Uses the arrays _ELEM TYPE_ as the key
    func_tys: HashMap<FuncType, TypeIdx>,
    arrays: HashMap<ValType, TypeIdx>,
    structs: HashMap<Name, StructInfo>,
    variants: HashMap<Name, VariantInfo>,
    // TODO: Could use `FuncType` as the key, and avoid creating duplicated closure types
    // for equivalent function types.
    closure_tys: HashMap<FuncTy, ClosureInfo>,
    func_refs: HashMap<Name, FuncIdx>,
}

impl<'a> Builder<'a> {
    pub fn new(name_supply: NameSupply) -> Builder<'a> {
        Builder {
            name_supply,
            funcs: HashMap::new(),
            globals: HashMap::new(),
            types: vec![],
            arrays: HashMap::new(),
            func_tys: HashMap::new(),
            structs: HashMap::new(),
            variants: HashMap::new(),
            closure_tys: HashMap::new(),
            func_refs: HashMap::new(),
            imports: HashMap::new(),
            exports: vec![],
            start_fn: None,
            substitution: Substitution::new(&[], &[]),
        }
    }

    fn _print_funcs(&self) {
        for (name, id) in self.name_supply.name_map.iter() {
            if let Name::Func(n) = name {
                eprintln!("$fn:{n} = {id:?}")
            }
        }
    }

    pub fn finish(self) -> (Vec<u8>, NameSupply) {
        let mut module = Module::new();
        let names = self.name_supply;
        // self._print_funcs();

        // type_section
        let mut type_names = WasmNameMap::new();
        let mut type_section = TypeSection::new();
        let mut all_field_names = IndirectNameMap::new();

        // I remember this didn't work on some more complicated programs
        // but it passes for all the current example programs. Keep an eye on it
        type_section.rec(self.types);
        // for ty in self.types {
        //     type_section.subtype(&ty);
        // }

        for (name, info) in self.structs {
            for (tys, type_idx) in &info.instances {
                let mut it = names.lookup(name).unwrap().it.clone();
                if !tys.is_empty() {
                    it.push('#');
                }
                for param in tys {
                    write!(&mut it, "_{}", param.display(&names.name_map)).unwrap()
                }
                type_names.append(*type_idx, &it);
                let mut field_names = WasmNameMap::new();
                for (field, _) in info.definition.fields.iter() {
                    field_names.append(info.field_idx(*field), &names.lookup(name).unwrap().it)
                }
                all_field_names.append(*type_idx, &field_names);
            }
        }

        for (name, info) in self.variants {
            for (tys, type_idx) in info.instances {
                let mut it = names.lookup(name).unwrap().it.clone();
                if !tys.is_empty() {
                    it.push('#');
                }
                for param in tys {
                    write!(&mut it, "_{}", param.display(&names.name_map)).unwrap()
                }
                type_names.append(type_idx, &it);
            }
        }

        // import_section
        let mut function_names = WasmNameMap::new();

        let mut import_section = ImportSection::new();
        let _import_count = self.imports.len();
        let mut imports: Vec<_> = self.imports.into_iter().collect();
        imports.sort_by_key(|(_, v)| v.index);
        let mut import_indices = vec![];
        for (name, data) in imports {
            import_section.import(&data.ns, &data.func, EntityType::Function(data.ty_idx));
            function_names.append(data.index, &names.lookup(name).unwrap().it);
            import_indices.push(data.index);
        }
        // function_section
        let mut function_section = FunctionSection::new();
        let mut funcs: Vec<_> = self.funcs.into_iter().collect();
        let mut function_indices = vec![];
        funcs.sort_by_key(|(_, v)| v.index);
        for (name, func) in funcs.iter() {
            function_names.append(func.index, &names.lookup(*name).unwrap().it);
            function_section.function(func.ty);
            function_indices.push(func.index);
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
            global_names.append(data.index, &names.lookup(data.name).unwrap().it)
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
        let mut elem_section = ElementSection::new();
        elem_section.declared(Elements::Functions(&import_indices));
        elem_section.declared(Elements::Functions(&function_indices));

        // code_section
        let mut code_section = CodeSection::new();
        let mut all_local_names = IndirectNameMap::new();
        for (_ix, (name, func)) in funcs.into_iter().enumerate() {
            let Some(FnLocals {
                locals,
                names: local_names,
            }) = func.locals
            else {
                panic!("No locals for function {}", names.lookup(name).unwrap().it)
            };
            let mut func_body = Function::new_with_locals_types(locals);
            for instr in func.body.unwrap() {
                func_body.instruction(&instr);
            }
            func_body.instruction(&Instruction::End);
            code_section.function(&func_body);

            let mut local_names_map = WasmNameMap::new();
            for (index, name) in local_names {
                local_names_map.append(index, &names.lookup(name).unwrap().it);
            }

            let ix = (_ix + _import_count) as u32;
            all_local_names.append(ix, &local_names_map);
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
        module.section(&elem_section);
        module.section(&code_section);
        module.section(&name_section);
        (module.finish(), names)
    }

    pub fn resolve_name(&self, name: Name) -> Id {
        self.name_supply
            .lookup(name)
            .cloned()
            .unwrap_or_else(|| panic!("Failed to resolve: {name:?}"))
    }

    pub fn declare_variant(&mut self, ty: Variant) {
        self.variants.insert(
            ty.name,
            VariantInfo {
                instances: HashMap::new(),
                alternatives: ty.alternatives,
            },
        );
    }

    pub fn declare_struct(&mut self, ty: Struct) {
        let struct_info = StructInfo {
            instances: HashMap::new(),
            definition: ty,
        };
        self.structs
            .insert(struct_info.definition.name, struct_info);
    }

    pub fn func_type(&mut self, ty: &FuncTy) -> TypeIdx {
        let res_ty = self.val_ty(&ty.result);
        let arguments = ty.arguments.iter().map(|t| self.val_ty(t));
        let func_type = FuncType::new(arguments, iter::once(res_ty));
        match self.func_tys.get(&func_type) {
            Some(idx) => *idx,
            None => {
                let idx = self.types.len() as TypeIdx;
                self.func_tys.insert(func_type.clone(), idx);
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

    pub fn heap_type(&mut self, name: Name, ty_params: &Substitution) -> TypeIdx {
        let tys = self
            .substitution()
            .apply_subst(ty_params.clone())
            .tys_owned();
        if let Some(v) = self.variants.get_mut(&name) {
            if let Some(type_idx) = v.instances.get(&tys) {
                return *type_idx;
            }

            let idx = self.types.len() as u32;
            v.instances.insert(tys, idx);
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
            idx
        } else if let Some(str) = self.structs.get_mut(&name).cloned() {
            if let Some(type_idx) = str.instances.get(&tys) {
                return *type_idx;
            }

            let mut fields = vec![];
            let mut supertype_idx = None;

            let definition = &str.definition;
            if let Some(variant) = definition.variant {
                supertype_idx = Some(self.heap_type(variant, ty_params));
                fields.push(FieldType {
                    element_type: StorageType::Val(ValType::I32),
                    mutable: false,
                });
            };
            for (_name, ty) in &definition.fields {
                let ty = ty_params.apply(ty.clone());
                let val_ty = self.val_ty(&ty);
                fields.push(FieldType {
                    element_type: StorageType::Val(val_ty),
                    mutable: true,
                })
            }

            let idx = self.types.len() as u32;
            self.structs
                .get_mut(&name)
                .unwrap()
                .instances
                .insert(tys, idx);

            self.types.push(SubType {
                is_final: true,
                supertype_idx,
                composite_type: CompositeType::Struct(StructType {
                    fields: fields.into_boxed_slice(),
                }),
            });
            idx
        } else {
            panic!(
                "tried to get heap type {} before declaring it",
                self.resolve_name(name).it
            )
        }
    }

    pub fn variant_type(&self, name: Name) -> &VariantInfo {
        self.variants
            .get(&name)
            .expect("Tried to get variant type before declaring it")
    }

    pub fn struct_type(&self, name: Name) -> &StructInfo {
        self.structs.get(&name).unwrap_or_else(|| {
            panic!(
                "Tried to get struct type before declaring it: {}",
                self.resolve_name(name).it
            )
        })
    }

    pub fn val_ty(&mut self, ty: &Ty) -> ValType {
        match ty {
            Ty::F32 => ValType::F32,
            Ty::I32 | Ty::Unit | Ty::Bool => ValType::I32,
            Ty::Cons { name: s, ty_args } => {
                let idx = self.heap_type(*s, ty_args);
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
                let info = self.closure_type(func_ty);
                ValType::Ref(RefType {
                    nullable: false,
                    heap_type: HeapType::Concrete(info.closure_struct_ty),
                })
            }
            Ty::Var(v) => {
                let Some(ty) = self.substitution.lookup(*v) else {
                    panic!("Tried to compile a VAR with no matching substitution")
                };
                self.val_ty(&ty.clone())
            }
            Ty::Diverge => unreachable!("val_ty of Diverge"),
            Ty::Error => unreachable!("ERROR shouldn't make it into codegen"),
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

    // Registers a closure type
    pub fn closure_type(&mut self, func_ty: &FuncTy) -> ClosureInfo {
        let func_ty = self.substitution().apply_func(func_ty.clone());
        if let Some(closure_info) = self.closure_tys.get(&func_ty) {
            *closure_info
        } else {
            let param_tys: Vec<ValType> =
                func_ty.arguments.iter().map(|t| self.val_ty(t)).collect();
            let result_ty = self.val_ty(&func_ty.result);

            let closure_func_ty = self.types.len() as u32;
            let closure_struct_ty = closure_func_ty + 1;

            let mut params = vec![ValType::Ref(RefType {
                nullable: false,
                heap_type: HeapType::Concrete(closure_struct_ty),
            })];
            params.extend(param_tys);

            self.types.push(SubType {
                is_final: true,
                supertype_idx: None,
                composite_type: CompositeType::Func(FuncType::new(params, iter::once(result_ty))),
            });
            self.types.push(SubType {
                is_final: false,
                supertype_idx: None,
                composite_type: CompositeType::Struct(StructType {
                    fields: [FieldType {
                        element_type: StorageType::Val(ValType::Ref(RefType {
                            nullable: false,
                            heap_type: HeapType::Concrete(closure_func_ty),
                        })),
                        mutable: false,
                    }]
                    .into(),
                }),
            });
            let closure_info = ClosureInfo {
                closure_struct_ty,
                closure_func_ty,
            };
            self.closure_tys.insert(func_ty, closure_info);
            closure_info
        }
    }

    // TODO: We should also cache the concrete types here?
    pub fn closure_type_concrete(
        &mut self,
        closure_info: ClosureInfo,
        captures: &[&Ty],
    ) -> TypeIdx {
        let mut fields = vec![FieldType {
            element_type: StorageType::Val(ValType::Ref(RefType {
                nullable: false,
                heap_type: HeapType::Concrete(closure_info.closure_func_ty),
            })),
            mutable: false,
        }];
        fields.extend(captures.into_iter().map(|t| FieldType {
            // Careful! May change self.types length
            element_type: StorageType::Val(self.val_ty(t)),
            mutable: false,
        }));
        let concrete_type = self.types.len() as u32;
        self.types.push(SubType {
            is_final: true,
            supertype_idx: Some(closure_info.closure_struct_ty),
            composite_type: CompositeType::Struct(StructType {
                fields: fields.into_boxed_slice(),
            }),
        });
        concrete_type
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

    pub fn declare_anon_func(&mut self, at: TextRange, ty: TypeIdx) -> (Name, FuncIdx) {
        let index = (self.imports.len() + self.funcs.len()) as u32;
        let name = self.name_supply.func_idx(Id {
            // TODO: Use surrounding function name or something?
            it: format!("closure-{index}"),
            at,
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
        (name, index)
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

    pub fn func_ref(&mut self, name: Name, closure_info: ClosureInfo, ty: &FuncTy) -> FuncIdx {
        if let Some(idx) = self.func_refs.get(&name) {
            return *idx;
        }
        let Id { it, at } = self.resolve_name(name);
        let func_name = self.name_supply.func_idx(Id {
            it: format!("{it}#ref"),
            at,
        });
        let mut instrs: Vec<Instruction> = (0..ty.arguments.len())
            .map(|i| Instruction::LocalGet(i as u32 + 1))
            .collect();
        instrs.push(Instruction::Call(self.lookup_func(&name)));
        let func_idx = (self.imports.len() + self.funcs.len()) as u32;
        self.funcs.insert(
            func_name,
            FuncData {
                index: func_idx,
                ty: closure_info.closure_func_ty,
                locals: Some(FnLocals {
                    locals: vec![],
                    names: HashMap::new(),
                }),
                body: Some(instrs),
            },
        );
        self.func_refs.insert(name, func_idx);
        func_idx
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

    pub(crate) fn substitution(&mut self) -> &Substitution {
        &self.substitution
    }

    pub(crate) fn set_substitution(&mut self, subst: Substitution) -> Substitution {
        mem::replace(&mut self.substitution, subst)
    }

    pub(crate) fn restore_substitution(&mut self, subst: Substitution) {
        self.substitution = subst
    }
}

#[derive(Debug)]
pub struct BodyBuilder {
    param_count: usize,
    locals: Vec<ValType>,
    named_locals: HashMap<Name, u32>,
}

#[derive(Debug)]
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
