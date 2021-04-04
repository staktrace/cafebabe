#[macro_use]
extern crate bitflags;
extern crate cesu8;

use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::Rc;

fn err<T>(msg: &'static str) -> Result<T, String> {
    Err(msg.to_string())
}

fn read_u1(bytes: &[u8], ix: &mut usize) -> Result<u8, String> {
    if bytes.len() < *ix + 1 {
        return Err(format!("Unexpected end of stream reading u2 at index {}", *ix));
    }
    let result = bytes[*ix];
    *ix += 1;
    Ok(result)
}

fn read_u2(bytes: &[u8], ix: &mut usize) -> Result<u16, String> {
    if bytes.len() < *ix + 2 {
        return Err(format!("Unexpected end of stream reading u2 at index {}", *ix));
    }
    let result =
        ((bytes[*ix + 0] as u16) << 8) |
        ((bytes[*ix + 1] as u16));
    *ix += 2;
    Ok(result)
}

fn read_u4(bytes: &[u8], ix: &mut usize) -> Result<u32, String> {
    if bytes.len() < *ix + 4 {
        return Err(format!("Unexpected end of stream reading u4 at index {}", *ix));
    }
    let result =
        ((bytes[*ix + 0] as u32) << 24) |
        ((bytes[*ix + 1] as u32) << 16) |
        ((bytes[*ix + 2] as u32) <<  8) |
        ((bytes[*ix + 3] as u32));
    *ix += 4;
    Ok(result)
}

fn read_u8(bytes: &[u8], ix: &mut usize) -> Result<u64, String> {
    if bytes.len() < *ix + 8 {
        return Err(format!("Unexpected end of stream reading u8 at index {}", *ix));
    }
    let result =
        ((bytes[*ix + 0] as u64) << 56) |
        ((bytes[*ix + 1] as u64) << 48) |
        ((bytes[*ix + 2] as u64) << 40) |
        ((bytes[*ix + 3] as u64) << 32) |
        ((bytes[*ix + 4] as u64) << 24) |
        ((bytes[*ix + 5] as u64) << 16) |
        ((bytes[*ix + 6] as u64) << 8) |
        ((bytes[*ix + 7] as u64));
    *ix += 8;
    Ok(result)
}

#[derive(Debug)]
enum ConstantPoolRef<'a> {
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

    fn get(&self) -> Rc<ConstantPoolEntry<'a>> {
        match self {
            ConstantPoolRef::Unresolved(_) => panic!("Called get on a unresolved ConstantPoolRef"),
            ConstantPoolRef::Resolved(target) => target.clone(),
        }
    }
}

trait RefCellDeref<'a> {
    fn resolve(&self, cp_index: usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<bool, String>;
    fn is_type(&self, allowed: ConstantPoolEntryTypes) -> Result<bool, String>;
}

impl<'a> RefCellDeref<'a> for RefCell<ConstantPoolRef<'a>> {
    fn resolve(&self, cp_index: usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<bool, String> {
        self.borrow_mut().resolve(cp_index, pool)
    }

    fn is_type(&self, allowed: ConstantPoolEntryTypes) -> Result<bool, String> {
        self.borrow().get().is_type(allowed)
    }
}

#[derive(Debug)]
enum BootstrapMethodRef {
    Unresolved(u16),
}

#[derive(Debug)]
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
    struct ConstantPoolEntryTypes: u16 {
        const ZERO = 0x0001;
        const UTF8 = 0x0002;
        const INTEGER = 0x0004;
        const FLOAT = 0x0008;
        const LONG = 0x0010;
        const DOUBLE = 0x0020;
        const CLASS_INFO = 0x0040;
        const STRING = 0x0080;
        const FIELD_REF = 0x0100;
        const METHOD_REF = 0x0200;
        const INTERFACE_METHOD_REF = 0x0400;
        const NAME_AND_TYPE = 0x0800;
        const METHOD_HANDLE = 0x1000;
        const METHOD_TYPE = 0x2000;
        const INVOKE_DYNAMIC = 0x4000;
        const UNUSED = 0x8000;

        const SUPERCLASS = Self::ZERO.bits() | Self::CLASS_INFO.bits();
        const NEW_METHOD_REFS = Self::METHOD_REF.bits() | Self::INTERFACE_METHOD_REF.bits();
    }
}

#[derive(Debug)]
enum ConstantPoolEntry<'a> {
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
    InvokeDynamic(BootstrapMethodRef, RefCell<ConstantPoolRef<'a>>),
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
            ConstantPoolEntry::InvokeDynamic(_, y) => y.resolve(my_index, pool),
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
            ConstantPoolEntry::InvokeDynamic(_, y) => y.borrow().is_resolved(),
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
            ConstantPoolEntry::InvokeDynamic(_, _) => ConstantPoolEntryTypes::INVOKE_DYNAMIC,
            ConstantPoolEntry::Unused => ConstantPoolEntryTypes::UNUSED,
        }
    }

    fn validate(&self, major_version: u16) -> Result<bool, String> {
        match self {
            ConstantPoolEntry::ClassInfo(x) => x.is_type(ConstantPoolEntryTypes::UTF8),
            ConstantPoolEntry::String(x) => x.is_type(ConstantPoolEntryTypes::UTF8),
            ConstantPoolEntry::FieldRef(x, y) => Ok(x.is_type(ConstantPoolEntryTypes::CLASS_INFO)? && y.is_type(ConstantPoolEntryTypes::NAME_AND_TYPE)?),
            ConstantPoolEntry::MethodRef(x, y) => Ok(x.is_type(ConstantPoolEntryTypes::CLASS_INFO)? && y.is_type(ConstantPoolEntryTypes::NAME_AND_TYPE)?),
            ConstantPoolEntry::InterfaceMethodRef(x, y) => Ok(x.is_type(ConstantPoolEntryTypes::CLASS_INFO)? && y.is_type(ConstantPoolEntryTypes::NAME_AND_TYPE)?),
            ConstantPoolEntry::NameAndType(x, y) => Ok(x.is_type(ConstantPoolEntryTypes::UTF8)? && y.is_type(ConstantPoolEntryTypes::UTF8)?),
            ConstantPoolEntry::MethodHandle(x, y) => y.is_type(match x {
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
            ConstantPoolEntry::MethodType(x) => x.is_type(ConstantPoolEntryTypes::UTF8),
            ConstantPoolEntry::InvokeDynamic(_, y) => y.is_type(ConstantPoolEntryTypes::NAME_AND_TYPE),
            _ => Ok(true),
        }
    }

    fn is_type(&self, allowed: ConstantPoolEntryTypes) -> Result<bool, String> {
        if allowed.contains(self.get_type()) {
            Ok(true)
        } else {
            err("Unexpected constant pool reference type for")
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

fn read_constant_invokedynamic<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let bootstrap_method_ref = BootstrapMethodRef::Unresolved(read_u2(bytes, ix)?);
    let name_and_type_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::InvokeDynamic(bootstrap_method_ref, name_and_type_ref))
}

fn read_constant_pool<'a>(bytes: &'a [u8], ix: &mut usize, constant_pool_count: u16) -> Result<Vec<Rc<ConstantPoolEntry<'a>>>, String> {
    let mut constant_pool = Vec::new();
    constant_pool.push(Rc::new(ConstantPoolEntry::Zero));
    let mut cp_ix = 1;
    while cp_ix < constant_pool_count {
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
            15 => read_constant_methodhandle(bytes, ix)?,
            16 => read_constant_methodtype(bytes, ix)?,
            18 => read_constant_invokedynamic(bytes, ix)?,
            n => return Err(format!("Unexpected constant pool entry type {} at index {}", n, *ix - 1)),
        }));
        cp_ix += 1;
        if constant_type == 5 || constant_type == 6 {
            // long and double types take up two entries in the constant pool,
            // so eat up another index.
            cp_ix += 1;
            constant_pool.push(Rc::new(ConstantPoolEntry::Unused));
        }
    }
    Ok(constant_pool)
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

fn read_cp_ref<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Rc<ConstantPoolEntry<'a>>, String> {
    let cp_index = read_u2(bytes, ix)? as usize;
    if cp_index >= pool.len() {
        return Err(format!("Out-of-bounds index {} in constant pool reference", cp_index));
    }
    Ok(pool[cp_index].clone())
}

fn read_interfaces<'a>(bytes: &'a [u8], ix: &mut usize, interfaces_count: u16, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<Rc<ConstantPoolEntry<'a>>>, String> {
    let mut interfaces = Vec::new();
    for _i in 0..interfaces_count {
        interfaces.push(read_cp_ref(bytes, ix, pool)?);
    }
    Ok(interfaces)
}

#[derive(Debug)]
pub struct AttributeInfo<'a> {
    name: Rc<ConstantPoolEntry<'a>>,
    info: &'a [u8],
}

impl<'a> AttributeInfo<'a> {
    fn validate(&self) -> Result<(), String> {
        self.name.is_type(ConstantPoolEntryTypes::UTF8).map_err(|e| format!("{} name field of", e))?;
        Ok(())
    }
}

fn read_attributes<'a>(bytes: &'a [u8], ix: &mut usize, attributes_count: u16, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<AttributeInfo<'a>>, String> {
    let mut attributes = Vec::new();
    for _i in 0..attributes_count {
        let name = read_cp_ref(bytes, ix, pool)?;
        let length = read_u4(bytes, ix)? as usize;
        if bytes.len() < *ix + length {
            return Err(format!("Unexpected end of stream reading attributes at index {}", *ix));
        }
        let info = &bytes[*ix .. *ix + length];
        *ix += length;
        attributes.push(AttributeInfo {
            name,
            info,
        });
    }
    Ok(attributes)
}

bitflags! {
    pub struct AccessFlags: u16 {
        const PUBLIC = 0x0001;
        const PRIVATE = 0x0002;
        const PROTECTED = 0x0004;
        const STATIC = 0x0008;
        const FINAL = 0x0010;
        const SUPER = 0x0020;
        const SYNCHRONIZED = 0x0020;
        const VOLATILE = 0x0040;
        const BRIDGE = 0x0040;
        const TRANSIENT = 0x0080;
        const VARARGS = 0x0080;
        const NATIVE = 0x0100;
        const INTERFACE = 0x0200;
        const ABSTRACT = 0x0400;
        const STRICT = 0x0800;
        const SYNTHETIC = 0x1000;
        const ANNOTATION = 0x2000;
        const ENUM = 0x4000;
    }
}

bitflags! {
    pub struct FieldAccessFlags: u16 {
        const PUBLIC = AccessFlags::PUBLIC.bits();
        const PRIVATE = AccessFlags::PRIVATE.bits();
        const PROTECTED = AccessFlags::PROTECTED.bits();
        const STATIC = AccessFlags::STATIC.bits();
        const FINAL = AccessFlags::FINAL.bits();
        const VOLATILE = AccessFlags::VOLATILE.bits();
        const TRANSIENT = AccessFlags::TRANSIENT.bits();
        const SYNTHETIC = AccessFlags::SYNTHETIC.bits();
        const ENUM = AccessFlags::ENUM.bits();
    }
}

#[derive(Debug)]
pub struct FieldInfo<'a> {
    pub access_flags: FieldAccessFlags,
    name: Rc<ConstantPoolEntry<'a>>,
    descriptor: Rc<ConstantPoolEntry<'a>>,
    pub attributes: Vec<AttributeInfo<'a>>,
}

impl<'a> FieldInfo<'a> {
    fn validate(&self) -> Result<(), String> {
        self.name.is_type(ConstantPoolEntryTypes::UTF8).map_err(|e| format!("{} name field of", e))?;
        self.descriptor.is_type(ConstantPoolEntryTypes::UTF8).map_err(|e| format!("{} descriptor field of", e))?;
        for (i, attribute) in self.attributes.iter().enumerate() {
            attribute.validate().map_err(|e| format!("{} attribute {} of", e, i))?;
        }
        Ok(())
    }
}

fn read_fields<'a>(bytes: &'a [u8], ix: &mut usize, fields_count: u16, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<FieldInfo<'a>>, String> {
    let mut fields = Vec::new();
    for _i in 0..fields_count {
        let access_flags = FieldAccessFlags::from_bits(read_u2(bytes, ix)?).ok_or("Invalid access flags found on field")?;
        let name = read_cp_ref(bytes, ix, pool)?;
        let descriptor = read_cp_ref(bytes, ix, pool)?;
        let attributes_count = read_u2(bytes, ix)?;
        let attributes = read_attributes(bytes, ix, attributes_count, pool)?;
        fields.push(FieldInfo {
            access_flags,
            name,
            descriptor,
            attributes,
        });
    }
    Ok(fields)
}

bitflags! {
    pub struct MethodAccessFlags: u16 {
        const PUBLIC = AccessFlags::PUBLIC.bits();
        const PRIVATE = AccessFlags::PRIVATE.bits();
        const PROTECTED = AccessFlags::PROTECTED.bits();
        const STATIC = AccessFlags::STATIC.bits();
        const FINAL = AccessFlags::FINAL.bits();
        const SYNCHRONIZED = AccessFlags::SYNCHRONIZED.bits();
        const BRIDGE = AccessFlags::BRIDGE.bits();
        const VARARGS = AccessFlags::VARARGS.bits();
        const NATIVE = AccessFlags::NATIVE.bits();
        const ABSTRACT = AccessFlags::ABSTRACT.bits();
        const STRICT = AccessFlags::STRICT.bits();
        const SYNTHETIC = AccessFlags::SYNTHETIC.bits();
    }
}

#[derive(Debug)]
pub struct MethodInfo<'a> {
    pub access_flags: MethodAccessFlags,
    name: Rc<ConstantPoolEntry<'a>>,
    descriptor: Rc<ConstantPoolEntry<'a>>,
    pub attributes: Vec<AttributeInfo<'a>>,
}

impl<'a> MethodInfo<'a> {
    fn validate(&self) -> Result<(), String> {
        self.name.is_type(ConstantPoolEntryTypes::UTF8).map_err(|e| format!("{} name field of", e))?;
        self.descriptor.is_type(ConstantPoolEntryTypes::UTF8).map_err(|e| format!("{} descriptor field of", e))?;
        for (i, attribute) in self.attributes.iter().enumerate() {
            attribute.validate().map_err(|e| format!("{} attribute {} of", e, i))?;
        }
        Ok(())
    }
}

fn read_methods<'a>(bytes: &'a [u8], ix: &mut usize, methods_count: u16, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<MethodInfo<'a>>, String> {
    let mut methods = Vec::new();
    for _i in 0..methods_count {
        let access_flags = MethodAccessFlags::from_bits(read_u2(bytes, ix)?).ok_or("Invalid access flags found on method")?;
        let name = read_cp_ref(bytes, ix, pool)?;
        let descriptor = read_cp_ref(bytes, ix, pool)?;
        let attributes_count = read_u2(bytes, ix)?;
        let attributes = read_attributes(bytes, ix, attributes_count, pool)?;
        methods.push(MethodInfo {
            access_flags,
            name,
            descriptor,
            attributes,
        });
    }
    Ok(methods)
}

bitflags! {
    pub struct ClassAccessFlags: u16 {
        const PUBLIC = AccessFlags::PUBLIC.bits();
        const FINAL = AccessFlags::FINAL.bits();
        const SUPER = AccessFlags::SUPER.bits();
        const INTERFACE = AccessFlags::INTERFACE.bits();
        const ABSTRACT = AccessFlags::ABSTRACT.bits();
        const SYNTHETIC = AccessFlags::SYNTHETIC.bits();
        const ANNOTATION = AccessFlags::ANNOTATION.bits();
        const ENUM = AccessFlags::ENUM.bits();
    }
}

#[derive(Debug)]
pub struct ClassFile<'a> {
    pub major_version: u16,
    pub minor_version: u16,
    constant_pool: Vec<Rc<ConstantPoolEntry<'a>>>,
    pub access_flags: ClassAccessFlags,
    this_class: Rc<ConstantPoolEntry<'a>>,
    super_class: Rc<ConstantPoolEntry<'a>>,
    interfaces: Vec<Rc<ConstantPoolEntry<'a>>>,
    pub fields: Vec<FieldInfo<'a>>,
    pub methods: Vec<MethodInfo<'a>>,
    pub attributes: Vec<AttributeInfo<'a>>,
}

impl<'a> ClassFile<'a> {
    fn validate(&self) -> Result<(), String> {
        for (i, cp_entry) in self.constant_pool.iter().enumerate() {
            cp_entry.validate(self.major_version).map_err(|e| format!("{} constant pool entry {}", e, i))?;
        }
        self.this_class.is_type(ConstantPoolEntryTypes::CLASS_INFO).map_err(|e| format!("{} this_class", e))?;
        self.super_class.is_type(ConstantPoolEntryTypes::SUPERCLASS).map_err(|e| format!("{} super_class", e))?;
        for (i, interface) in self.interfaces.iter().enumerate() {
            interface.is_type(ConstantPoolEntryTypes::CLASS_INFO).map_err(|e| format!("{} interface {}", e, i))?;
        }
        for (i, field) in self.fields.iter().enumerate() {
            field.validate().map_err(|e| format!("{} class field {}", e, i))?;
        }
        for (i, method) in self.methods.iter().enumerate() {
            method.validate().map_err(|e| format!("{} class method {}", e, i))?;
        }
        for (i, attribute) in self.attributes.iter().enumerate() {
            attribute.validate().map_err(|e| format!("{} class attribute {}", e, i))?;
        }
        Ok(())
    }
}

pub fn parse_class<'a>(raw_bytes: &'a [u8]) -> Result<ClassFile<'a>, String> {
    let mut ix = 0;
    if read_u4(raw_bytes, &mut ix)? != 0xCAFEBABE {
        return err("Unexpected magic header");
    }
    let major_version = read_u2(raw_bytes, &mut ix)?;
    let minor_version = read_u2(raw_bytes, &mut ix)?;
    let constant_pool_count = read_u2(raw_bytes, &mut ix)?;
    let constant_pool = read_constant_pool(raw_bytes, &mut ix, constant_pool_count)?;
    resolve_constant_pool(&constant_pool)?;

    let access_flags = ClassAccessFlags::from_bits(read_u2(raw_bytes, &mut ix)?).ok_or("Invalid access flags found on class")?;
    let this_class = read_cp_ref(raw_bytes, &mut ix, &constant_pool)?;
    let super_class = read_cp_ref(raw_bytes, &mut ix, &constant_pool)?;
    let interfaces_count = read_u2(raw_bytes, &mut ix)?;
    let interfaces = read_interfaces(raw_bytes, &mut ix, interfaces_count, &constant_pool)?;
    let fields_count = read_u2(raw_bytes, &mut ix)?;
    let fields = read_fields(raw_bytes, &mut ix, fields_count, &constant_pool)?;
    let methods_count = read_u2(raw_bytes, &mut ix)?;
    let methods = read_methods(raw_bytes, &mut ix, methods_count, &constant_pool)?;
    let attributes_count = read_u2(raw_bytes, &mut ix)?;
    let attributes = read_attributes(raw_bytes, &mut ix, attributes_count, &constant_pool)?;

    let class_file = ClassFile {
        major_version,
        minor_version,
        constant_pool,
        access_flags,
        this_class,
        super_class,
        interfaces,
        fields,
        methods,
        attributes,
    };
    class_file.validate()?;
    Ok(class_file)
}
