use std::borrow::Cow;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

use crate::{err, read_u1, read_u2, read_u4, read_u8, BootstrapMethodRef};

#[derive(Debug)]
pub(crate) enum ConstantPoolRef<'a> {
    Unresolved(u16),
    Resolved(Rc<ConstantPoolEntry<'a>>),
}

impl<'a> ConstantPoolRef<'a> {
    fn resolve(&mut self, my_index: usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<bool, String> {
        match self {
            ConstantPoolRef::Unresolved(ix) => {
                let target = *ix as usize;
                if target == my_index {
                    return Err(format!("Constant pool entry at index {} could not be resolved due to self-reference", my_index));
                }
                if target >= pool.len() {
                    return Err(format!("Constant pool entry at index {} references out-of-bounds index {}", my_index, target));
                }
                if !pool[target].is_resolved() {
                    return Ok(false);
                }
                *self = ConstantPoolRef::Resolved(pool[target].clone());
                Ok(true)
            }
            ConstantPoolRef::Resolved(_) => Ok(true),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            ConstantPoolRef::Unresolved(_) => false,
            ConstantPoolRef::Resolved(_) => true,
        }
    }

    fn get(&self) -> &Rc<ConstantPoolEntry<'a>> {
        match self {
            ConstantPoolRef::Unresolved(_) => panic!("Called get on a unresolved ConstantPoolRef"),
            ConstantPoolRef::Resolved(target) => target,
        }
    }
}

trait RefCellDeref<'a> {
    fn resolve(&self, cp_index: usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<bool, String>;
    fn ensure_type(&self, allowed: ConstantPoolEntryTypes) -> Result<bool, String>;
}

impl<'a> RefCellDeref<'a> for RefCell<ConstantPoolRef<'a>> {
    fn resolve(&self, cp_index: usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<bool, String> {
        self.borrow_mut().resolve(cp_index, pool)
    }

    fn ensure_type(&self, allowed: ConstantPoolEntryTypes) -> Result<bool, String> {
        self.borrow().get().ensure_type(allowed)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ReferenceKind {
    GetField,
    GetStatic,
    PutField,
    PutStatic,
    InvokeVirtual,
    InvokeStatic,
    InvokeSpecial,
    NewInvokeSpecial,
    InvokeInterface,
}

bitflags! {
    pub(crate) struct ConstantPoolEntryTypes: u32 {
        const ZERO = 0x00000001;
        const UTF8 = 0x00000002;
        const INTEGER = 0x00000004;
        const FLOAT = 0x00000008;
        const LONG = 0x00000010;
        const DOUBLE = 0x00000020;
        const CLASS_INFO = 0x00000040;
        const STRING = 0x00000080;
        const FIELD_REF = 0x00000100;
        const METHOD_REF = 0x00000200;
        const INTERFACE_METHOD_REF = 0x00000400;
        const NAME_AND_TYPE = 0x00000800;
        const METHOD_HANDLE = 0x00001000;
        const METHOD_TYPE = 0x00002000;
        const DYNAMIC = 0x00004000;
        const INVOKE_DYNAMIC = 0x00008000;
        const MODULE_INFO = 0x00010000;
        const PACKAGE_INFO = 0x00020000;
        const UNUSED = 0x00040000;

        const NEW_METHOD_REFS = Self::METHOD_REF.bits() | Self::INTERFACE_METHOD_REF.bits();
        const CONSTANTS = Self::INTEGER.bits() | Self::FLOAT.bits() | Self::LONG.bits() | Self::DOUBLE.bits() | Self::STRING.bits();
        const BOOTSTRAP_ARGUMENT = Self::CONSTANTS.bits() | Self::CLASS_INFO.bits() | Self::METHOD_HANDLE.bits() | Self::METHOD_TYPE.bits();
    }
}

#[derive(Debug)]
pub(crate) enum ConstantPoolEntry<'a> {
    Zero,
    Utf8(Cow<'a, str>),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    ClassInfo(RefCell<ConstantPoolRef<'a>>),
    String(RefCell<ConstantPoolRef<'a>>),
    FieldRef(RefCell<ConstantPoolRef<'a>>, RefCell<ConstantPoolRef<'a>>),
    MethodRef(RefCell<ConstantPoolRef<'a>>, RefCell<ConstantPoolRef<'a>>),
    InterfaceMethodRef(RefCell<ConstantPoolRef<'a>>, RefCell<ConstantPoolRef<'a>>),
    NameAndType(RefCell<ConstantPoolRef<'a>>, RefCell<ConstantPoolRef<'a>>),
    MethodHandle(ReferenceKind, RefCell<ConstantPoolRef<'a>>),
    MethodType(RefCell<ConstantPoolRef<'a>>),
    Dynamic(BootstrapMethodRef, RefCell<ConstantPoolRef<'a>>),
    InvokeDynamic(BootstrapMethodRef, RefCell<ConstantPoolRef<'a>>),
    ModuleInfo(RefCell<ConstantPoolRef<'a>>),
    PackageInfo(RefCell<ConstantPoolRef<'a>>),
    Unused,
}

impl<'a> ConstantPoolEntry<'a> {
    fn resolve(&self, my_index: usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<bool, String> {
        match self {
            ConstantPoolEntry::ClassInfo(x) => x.resolve(my_index, pool),
            ConstantPoolEntry::String(x) => x.resolve(my_index, pool),
            ConstantPoolEntry::FieldRef(x, y) => Ok(x.resolve(my_index, pool)? && y.resolve(my_index, pool)?),
            ConstantPoolEntry::MethodRef(x, y) => Ok(x.resolve(my_index, pool)? && y.resolve(my_index, pool)?),
            ConstantPoolEntry::InterfaceMethodRef(x, y) => Ok(x.resolve(my_index, pool)? && y.resolve(my_index, pool)?),
            ConstantPoolEntry::NameAndType(x, y) => Ok(x.resolve(my_index, pool)? && y.resolve(my_index, pool)?),
            ConstantPoolEntry::MethodHandle(_, y) => y.resolve(my_index, pool),
            ConstantPoolEntry::MethodType(x) => x.resolve(my_index, pool),
            ConstantPoolEntry::Dynamic(_, y) => y.resolve(my_index, pool),
            ConstantPoolEntry::InvokeDynamic(_, y) => y.resolve(my_index, pool),
            ConstantPoolEntry::ModuleInfo(x) => x.resolve(my_index, pool),
            ConstantPoolEntry::PackageInfo(x) => x.resolve(my_index, pool),
            _ => Ok(true),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            ConstantPoolEntry::ClassInfo(x) => x.borrow().is_resolved(),
            ConstantPoolEntry::String(x) => x.borrow().is_resolved(),
            ConstantPoolEntry::FieldRef(x, y) => x.borrow().is_resolved() && y.borrow().is_resolved(),
            ConstantPoolEntry::MethodRef(x, y) => x.borrow().is_resolved() && y.borrow().is_resolved(),
            ConstantPoolEntry::InterfaceMethodRef(x, y) => x.borrow().is_resolved() && y.borrow().is_resolved(),
            ConstantPoolEntry::NameAndType(x, y) => x.borrow().is_resolved() && y.borrow().is_resolved(),
            ConstantPoolEntry::MethodHandle(_, y) => y.borrow().is_resolved(),
            ConstantPoolEntry::MethodType(x) => x.borrow().is_resolved(),
            ConstantPoolEntry::Dynamic(_, y) => y.borrow().is_resolved(),
            ConstantPoolEntry::InvokeDynamic(_, y) => y.borrow().is_resolved(),
            ConstantPoolEntry::ModuleInfo(x) => x.borrow().is_resolved(),
            ConstantPoolEntry::PackageInfo(x) => x.borrow().is_resolved(),
            _ => true,
        }
    }

    fn get_type(&self) -> ConstantPoolEntryTypes {
        match self {
            ConstantPoolEntry::Zero => ConstantPoolEntryTypes::ZERO,
            ConstantPoolEntry::Utf8(_) => ConstantPoolEntryTypes::UTF8,
            ConstantPoolEntry::Integer(_) => ConstantPoolEntryTypes::INTEGER,
            ConstantPoolEntry::Float(_) => ConstantPoolEntryTypes::FLOAT,
            ConstantPoolEntry::Long(_) => ConstantPoolEntryTypes::LONG,
            ConstantPoolEntry::Double(_) => ConstantPoolEntryTypes::DOUBLE,
            ConstantPoolEntry::ClassInfo(_) => ConstantPoolEntryTypes::CLASS_INFO,
            ConstantPoolEntry::String(_) => ConstantPoolEntryTypes::STRING,
            ConstantPoolEntry::FieldRef(_, _) => ConstantPoolEntryTypes::FIELD_REF,
            ConstantPoolEntry::MethodRef(_, _) => ConstantPoolEntryTypes::METHOD_REF,
            ConstantPoolEntry::InterfaceMethodRef(_, _) => ConstantPoolEntryTypes::INTERFACE_METHOD_REF,
            ConstantPoolEntry::NameAndType(_, _) => ConstantPoolEntryTypes::NAME_AND_TYPE,
            ConstantPoolEntry::MethodHandle(_, _) => ConstantPoolEntryTypes::METHOD_HANDLE,
            ConstantPoolEntry::MethodType(_) => ConstantPoolEntryTypes::METHOD_TYPE,
            ConstantPoolEntry::Dynamic(_, _) => ConstantPoolEntryTypes::DYNAMIC,
            ConstantPoolEntry::InvokeDynamic(_, _) => ConstantPoolEntryTypes::INVOKE_DYNAMIC,
            ConstantPoolEntry::ModuleInfo(_) => ConstantPoolEntryTypes::MODULE_INFO,
            ConstantPoolEntry::PackageInfo(_) => ConstantPoolEntryTypes::PACKAGE_INFO,
            ConstantPoolEntry::Unused => ConstantPoolEntryTypes::UNUSED,
        }
    }

    fn validate(&self, major_version: u16) -> Result<bool, String> {
        match self {
            ConstantPoolEntry::ClassInfo(x) => x.ensure_type(ConstantPoolEntryTypes::UTF8),
            ConstantPoolEntry::String(x) => x.ensure_type(ConstantPoolEntryTypes::UTF8),
            ConstantPoolEntry::FieldRef(x, y) => Ok(x.ensure_type(ConstantPoolEntryTypes::CLASS_INFO)? && y.ensure_type(ConstantPoolEntryTypes::NAME_AND_TYPE)?),
            ConstantPoolEntry::MethodRef(x, y) => Ok(x.ensure_type(ConstantPoolEntryTypes::CLASS_INFO)? && y.ensure_type(ConstantPoolEntryTypes::NAME_AND_TYPE)?),
            ConstantPoolEntry::InterfaceMethodRef(x, y) => Ok(x.ensure_type(ConstantPoolEntryTypes::CLASS_INFO)? && y.ensure_type(ConstantPoolEntryTypes::NAME_AND_TYPE)?),
            ConstantPoolEntry::NameAndType(x, y) => Ok(x.ensure_type(ConstantPoolEntryTypes::UTF8)? && y.ensure_type(ConstantPoolEntryTypes::UTF8)?),
            ConstantPoolEntry::MethodHandle(x, y) => y.ensure_type(match x {
                ReferenceKind::GetField |
                ReferenceKind::GetStatic |
                ReferenceKind::PutField |
                ReferenceKind::PutStatic => ConstantPoolEntryTypes::FIELD_REF,
                ReferenceKind::InvokeVirtual |
                ReferenceKind::NewInvokeSpecial => ConstantPoolEntryTypes::METHOD_REF,
                ReferenceKind::InvokeStatic |
                ReferenceKind::InvokeSpecial => if major_version < 52 { ConstantPoolEntryTypes::METHOD_REF } else { ConstantPoolEntryTypes::NEW_METHOD_REFS },
                ReferenceKind::InvokeInterface => ConstantPoolEntryTypes::INTERFACE_METHOD_REF,
            }),
            ConstantPoolEntry::MethodType(x) => x.ensure_type(ConstantPoolEntryTypes::UTF8),
            ConstantPoolEntry::Dynamic(_, y) => y.ensure_type(ConstantPoolEntryTypes::NAME_AND_TYPE),
            ConstantPoolEntry::InvokeDynamic(_, y) => y.ensure_type(ConstantPoolEntryTypes::NAME_AND_TYPE),
            ConstantPoolEntry::ModuleInfo(x) => x.ensure_type(ConstantPoolEntryTypes::UTF8),
            ConstantPoolEntry::PackageInfo(x) => x.ensure_type(ConstantPoolEntryTypes::UTF8),
            _ => Ok(true),
        }
    }

    fn ensure_type(&self, allowed: ConstantPoolEntryTypes) -> Result<bool, String> {
        if allowed.contains(self.get_type()) {
            Ok(true)
        } else {
            err("Unexpected constant pool reference type for")
        }
    }

    fn utf8(&self) -> Cow<'a, str> {
        match self {
            ConstantPoolEntry::Utf8(x) => x.clone(),
            _ => panic!("Attempting to get utf-8 data from non-utf8 constant pool entry!"),
        }
    }

    fn classinfo(&self) -> Cow<'a, str> {
        match self {
            ConstantPoolEntry::ClassInfo(x) => x.borrow().get().utf8(),
            _ => panic!("Attempting to get classinfo data from non-classinfo constant pool entry!"),
        }
    }

    fn name_and_type(&self) -> NameAndType<'a> {
        match self {
            ConstantPoolEntry::NameAndType(x, y) => NameAndType { name: x.borrow().get().utf8(), descriptor: y.borrow().get().utf8() },
            _ => panic!("Attempting to get name and type data from non-name-and-type constant pool entry!"),
        }
    }
}

fn read_unresolved_cp_ref<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<RefCell<ConstantPoolRef<'a>>, String> {
    Ok(RefCell::new(ConstantPoolRef::Unresolved(read_u2(bytes, ix)?)))
}
fn read_constant_utf8<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let length = read_u2(bytes, ix)? as usize;
    if bytes.len() < *ix + length {
        return Err(format!("Unexpected end of stream reading CONSTANT_Utf8 at index {}", *ix));
    }
    let modified_utf8_data = &bytes[*ix .. *ix + length];
    *ix += length;
    Ok(ConstantPoolEntry::Utf8(cesu8::from_java_cesu8(modified_utf8_data).map_err(|e| format!("Error reading CONSTANT_Utf8 at indices {}..{}: {}", *ix - length, *ix, e))?))
}

fn read_constant_integer<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    Ok(ConstantPoolEntry::Integer(read_u4(bytes, ix)? as i32))
}

fn read_constant_float<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    Ok(ConstantPoolEntry::Float(f32::from_bits(read_u4(bytes, ix)?)))
}

fn read_constant_long<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    Ok(ConstantPoolEntry::Long(read_u8(bytes, ix)? as i64))
}

fn read_constant_double<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    Ok(ConstantPoolEntry::Double(f64::from_bits(read_u8(bytes, ix)?)))
}

fn read_constant_class<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let name_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::ClassInfo(name_ref))
}

fn read_constant_string<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let value_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::String(value_ref))
}

fn read_constant_fieldref<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let class_ref = read_unresolved_cp_ref(bytes, ix)?;
    let name_and_type_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::FieldRef(class_ref, name_and_type_ref))
}

fn read_constant_methodref<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let class_ref = read_unresolved_cp_ref(bytes, ix)?;
    let name_and_type_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::MethodRef(class_ref, name_and_type_ref))
}

fn read_constant_interfacemethodref<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let class_ref = read_unresolved_cp_ref(bytes, ix)?;
    let name_and_type_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::InterfaceMethodRef(class_ref, name_and_type_ref))
}

fn read_constant_nameandtype<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let name_ref = read_unresolved_cp_ref(bytes, ix)?;
    let descriptor_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::NameAndType(name_ref, descriptor_ref))
}

fn read_constant_methodhandle<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let reference_kind = match read_u1(bytes, ix)? {
        1 => ReferenceKind::GetField,
        2 => ReferenceKind::GetStatic,
        3 => ReferenceKind::PutField,
        4 => ReferenceKind::PutStatic,
        5 => ReferenceKind::InvokeVirtual,
        6 => ReferenceKind::InvokeStatic,
        7 => ReferenceKind::InvokeSpecial,
        8 => ReferenceKind::NewInvokeSpecial,
        9 => ReferenceKind::InvokeInterface,
        n => return Err(format!("Unexpected reference kind {} when reading CONSTANT_methodhandle at index {}", n, *ix - 1)),
    };
    let reference_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::MethodHandle(reference_kind, reference_ref))
}

fn read_constant_methodtype<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let descriptor_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::MethodType(descriptor_ref))
}

fn read_constant_dynamic<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let bootstrap_method_ref = BootstrapMethodRef::Unresolved(read_u2(bytes, ix)?);
    let name_and_type_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::Dynamic(bootstrap_method_ref, name_and_type_ref))
}

fn read_constant_invokedynamic<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let bootstrap_method_ref = BootstrapMethodRef::Unresolved(read_u2(bytes, ix)?);
    let name_and_type_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::InvokeDynamic(bootstrap_method_ref, name_and_type_ref))
}

fn read_constant_module<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let name_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::ModuleInfo(name_ref))
}

fn read_constant_package<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let name_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::PackageInfo(name_ref))
}

fn resolve_constant_pool<'a>(constant_pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<(), String> {
    let mut resolved_count = 0;
    while resolved_count < constant_pool.len() {
        let mut count = 0;
        for (i, cp_entry) in constant_pool.iter().enumerate() {
            if cp_entry.resolve(i, &constant_pool)? {
                count += 1;
            }
        }
        if count == resolved_count {
            return err("Unable to resolve all constant pool entries");
        }
        resolved_count = count;
    }
    Ok(())
}

fn validate_constant_pool<'a>(constant_pool: &[Rc<ConstantPoolEntry<'a>>], major_version: u16) -> Result<(), String> {
    for (i, cp_entry) in constant_pool.iter().enumerate() {
        cp_entry.validate(major_version).map_err(|e| format!("{} constant pool entry {}", e, i))?;
    }
    Ok(())
}

pub(crate) fn read_constant_pool<'a>(bytes: &'a [u8], ix: &mut usize, major_version: u16) -> Result<Vec<Rc<ConstantPoolEntry<'a>>>, String> {
    let count = read_u2(bytes, ix)?;
    let mut constant_pool = Vec::with_capacity(count.into());
    constant_pool.push(Rc::new(ConstantPoolEntry::Zero));
    let mut cp_ix = 1;
    while cp_ix < count {
        let constant_type = read_u1(bytes, ix)?;
        constant_pool.push(Rc::new(match constant_type {
            1 => read_constant_utf8(bytes, ix)?,
            3 => read_constant_integer(bytes, ix)?,
            4 => read_constant_float(bytes, ix)?,
            5 => read_constant_long(bytes, ix)?,
            6 => read_constant_double(bytes, ix)?,
            7 => read_constant_class(bytes, ix)?,
            8 => read_constant_string(bytes, ix)?,
            9 => read_constant_fieldref(bytes, ix)?,
            10 => read_constant_methodref(bytes, ix)?,
            11 => read_constant_interfacemethodref(bytes, ix)?,
            12 => read_constant_nameandtype(bytes, ix)?,
            15 if major_version >= 51 => read_constant_methodhandle(bytes, ix)?,
            16 if major_version >= 51 => read_constant_methodtype(bytes, ix)?,
            17 if major_version >= 55 => read_constant_dynamic(bytes, ix)?,
            18 if major_version >= 51 => read_constant_invokedynamic(bytes, ix)?,
            19 if major_version >= 53 => read_constant_module(bytes, ix)?,
            20 if major_version >= 53 => read_constant_package(bytes, ix)?,
            n => return Err(format!("Unexpected constant pool entry type {} at index {} for classfile major version {}", n, *ix - 1, major_version)),
        }));
        cp_ix += 1;
        if constant_type == 5 || constant_type == 6 {
            // long and double types take up two entries in the constant pool,
            // so eat up another index.
            cp_ix += 1;
            constant_pool.push(Rc::new(ConstantPoolEntry::Unused));
        }
    }
    resolve_constant_pool(&constant_pool)?;
    validate_constant_pool(&constant_pool, major_version)?;
    Ok(constant_pool)
}

fn read_cp_ref_any<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Rc<ConstantPoolEntry<'a>>, String> {
    let cp_index = read_u2(bytes, ix)? as usize;
    if cp_index >= pool.len() {
        return Err(format!("Out-of-bounds index {} in constant pool reference for", cp_index));
    }
    Ok(pool[cp_index].clone())
}

pub(crate) fn read_cp_utf8<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Cow<'a, str>, String> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Utf8(x) => Ok(x.clone()),
        _ => err("Unexpected constant pool reference type for")
    }
}

pub(crate) fn read_cp_utf8_opt<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Option<Cow<'a, str>>, String> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Zero => Ok(None),
        ConstantPoolEntry::Utf8(x) => Ok(Some(x.clone())),
        _ => err("Unexpected constant pool reference type for")
    }
}

pub(crate) fn read_cp_classinfo<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Cow<'a, str>, String> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::ClassInfo(x) => Ok(x.borrow().get().utf8()),
        _ => err("Unexpected constant pool reference type for")
    }
}

pub(crate) fn read_cp_classinfo_opt<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Option<Cow<'a, str>>, String> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Zero => Ok(None),
        ConstantPoolEntry::ClassInfo(x) => Ok(Some(x.borrow().get().utf8())),
        _ => err("Unexpected constant pool reference type for")
    }
}

pub(crate) fn read_cp_moduleinfo<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Cow<'a, str>, String> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::ModuleInfo(x) => Ok(x.borrow().get().utf8()),
        _ => err("Unexpected constant pool reference type for")
    }
}

#[derive(Debug)]
pub struct NameAndType<'a> {
    pub name: Cow<'a, str>,
    pub descriptor: Cow<'a, str>,
}

pub(crate) fn read_cp_nameandtype_opt<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Option<NameAndType<'a>>, String> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Zero => Ok(None),
        ConstantPoolEntry::NameAndType(x, y) => Ok(Some(NameAndType { name: x.borrow().get().utf8(), descriptor: y.borrow().get().utf8() })),
        _ => err("Unexpected constant pool reference type for")
    }
}

#[derive(Debug)]
pub enum LiteralConstant<'a> {
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    String(Cow<'a, str>),
}

pub(crate) fn read_cp_literalconstant<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<LiteralConstant<'a>, String> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Integer(v) => Ok(LiteralConstant::Integer(*v)),
        ConstantPoolEntry::Float(v) => Ok(LiteralConstant::Float(*v)),
        ConstantPoolEntry::Long(v) => Ok(LiteralConstant::Long(*v)),
        ConstantPoolEntry::Double(v) => Ok(LiteralConstant::Double(*v)),
        ConstantPoolEntry::String(v) => Ok(LiteralConstant::String(v.borrow().get().utf8())),
        _ => err("Unexpected constant pool reference type for")
    }
}

pub(crate) fn read_cp_integer<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<i32, String> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Integer(v) => Ok(*v),
        _ => err("Unexpected constant pool reference type for")
    }
}

pub(crate) fn read_cp_float<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<f32, String> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Float(v) => Ok(*v),
        _ => err("Unexpected constant pool reference type for")
    }
}

pub(crate) fn read_cp_long<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<i64, String> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Long(v) => Ok(*v),
        _ => err("Unexpected constant pool reference type for")
    }
}

pub(crate) fn read_cp_double<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<f64, String> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Double(v) => Ok(*v),
        _ => err("Unexpected constant pool reference type for")
    }
}

#[derive(Clone, Copy, Debug)]
pub enum MemberKind {
    Field,
    Method,
    InterfaceMethod,
}

#[derive(Debug)]
pub struct MethodHandle<'a> {
    pub kind: ReferenceKind,
    pub class_name: Cow<'a, str>,
    pub member_kind: MemberKind,
    pub member_ref: NameAndType<'a>,
}

fn make_method_handle<'a>(x: &ReferenceKind, y: &RefCell<ConstantPoolRef<'a>>) -> Result<MethodHandle<'a>, String> {
    let (class_name, member_kind, member_ref) = match y.borrow().get().deref() {
        ConstantPoolEntry::FieldRef(c, m) => (c.borrow().get().classinfo(), MemberKind::Field, m.borrow().get().name_and_type()),
        ConstantPoolEntry::MethodRef(c, m) => (c.borrow().get().classinfo(), MemberKind::Method, m.borrow().get().name_and_type()),
        ConstantPoolEntry::InterfaceMethodRef(c, m) => (c.borrow().get().classinfo(), MemberKind::InterfaceMethod, m.borrow().get().name_and_type()),
        _ => return err("Unexpected constant pool reference type for"),
    };
    Ok(MethodHandle {
        kind: *x,
        class_name,
        member_kind,
        member_ref
    })
}

pub(crate) fn read_cp_methodhandle<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<MethodHandle<'a>, String> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::MethodHandle(x, y) => make_method_handle(x, y),
        _ => err("Unexpected constant pool reference type for")
    }
}

#[derive(Debug)]
pub enum BootstrapArgument<'a> {
    LiteralConstant(LiteralConstant<'a>),
    ClassInfo(Cow<'a, str>),
    MethodHandle(MethodHandle<'a>),
    MethodType(Cow<'a, str>),
}

pub(crate) fn read_cp_bootstrap_argument<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<BootstrapArgument<'a>, String> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Integer(v) => Ok(BootstrapArgument::LiteralConstant(LiteralConstant::Integer(*v))),
        ConstantPoolEntry::Float(v) => Ok(BootstrapArgument::LiteralConstant(LiteralConstant::Float(*v))),
        ConstantPoolEntry::Long(v) => Ok(BootstrapArgument::LiteralConstant(LiteralConstant::Long(*v))),
        ConstantPoolEntry::Double(v) => Ok(BootstrapArgument::LiteralConstant(LiteralConstant::Double(*v))),
        ConstantPoolEntry::String(v) => Ok(BootstrapArgument::LiteralConstant(LiteralConstant::String(v.borrow().get().utf8()))),
        ConstantPoolEntry::ClassInfo(x) => Ok(BootstrapArgument::ClassInfo(x.borrow().get().utf8())),
        ConstantPoolEntry::MethodHandle(x, y) => Ok(BootstrapArgument::MethodHandle(make_method_handle(x, y)?)),
        ConstantPoolEntry::MethodType(x) => Ok(BootstrapArgument::MethodType(x.borrow().get().utf8())),
        _ => err("Unexpected constant pool reference type for")
    }
}