#[macro_use]
extern crate bitflags;
extern crate cesu8;

use std::borrow::Cow;

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
pub enum ConstantPoolRef<'a> {
    Unresolved(u16),
    Resolved(&'a ConstantPoolEntry<'a>),
}

fn read_cp_ref<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolRef<'a>, String> {
    Ok(ConstantPoolRef::Unresolved(read_u2(bytes, ix)?))
}

#[derive(Debug)]
pub enum BootstrapMethodRef {
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

#[derive(Debug)]
pub enum ConstantPoolEntry<'a> {
    Unused,
    Utf8(Cow<'a, str>),
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    ClassInfo(ConstantPoolRef<'a>),
    String(ConstantPoolRef<'a>),
    FieldRef(ConstantPoolRef<'a>, ConstantPoolRef<'a>),
    MethodRef(ConstantPoolRef<'a>, ConstantPoolRef<'a>),
    InterfaceMethodRef(ConstantPoolRef<'a>, ConstantPoolRef<'a>),
    NameAndType(ConstantPoolRef<'a>, ConstantPoolRef<'a>),
    MethodHandle(ReferenceKind, ConstantPoolRef<'a>),
    MethodType(ConstantPoolRef<'a>),
    InvokeDynamic(BootstrapMethodRef, ConstantPoolRef<'a>),
}

fn read_constant_utf8<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let length = read_u2(bytes, ix)? as usize;
    if bytes.len() < *ix + length {
        return Err(format!("Unexpected end of stream reading CONSTANT_Utf8 at index {}", *ix));
    }
    let modified_utf8_data = &bytes[*ix .. *ix + length];
    *ix += length;
    Ok(ConstantPoolEntry::Utf8(cesu8::from_java_cesu8(modified_utf8_data).map_err(|e| format!("Error reading CONSTANT_Utf8: {}", e))?))
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
    let name_ref = read_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::ClassInfo(name_ref))
}

fn read_constant_string<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let value_ref = read_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::String(value_ref))
}

fn read_constant_fieldref<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let class_ref = read_cp_ref(bytes, ix)?;
    let name_and_type_ref = read_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::FieldRef(class_ref, name_and_type_ref))
}

fn read_constant_methodref<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let class_ref = read_cp_ref(bytes, ix)?;
    let name_and_type_ref = read_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::MethodRef(class_ref, name_and_type_ref))
}

fn read_constant_interfacemethodref<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let class_ref = read_cp_ref(bytes, ix)?;
    let name_and_type_ref = read_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::InterfaceMethodRef(class_ref, name_and_type_ref))
}

fn read_constant_nameandtype<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let name_ref = read_cp_ref(bytes, ix)?;
    let descriptor_ref = read_cp_ref(bytes, ix)?;
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
        n => return Err(format!("Unexpected reference kind {} when reading CONSANT_methodhandle", n)),
    };
    let reference_ref = read_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::MethodHandle(reference_kind, reference_ref))
}

fn read_constant_methodtype<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let descriptor_ref = read_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::MethodType(descriptor_ref))
}

fn read_constant_invokedynamic<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let bootstrap_method_ref = BootstrapMethodRef::Unresolved(read_u2(bytes, ix)?);
    let name_and_type_ref = read_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::InvokeDynamic(bootstrap_method_ref, name_and_type_ref))
}

fn read_constant_pool<'a>(bytes: &'a [u8], ix: &mut usize, constant_pool_count: u16) -> Result<Vec<ConstantPoolEntry<'a>>, String> {
    let mut constant_pool = Vec::new();
    constant_pool.push(ConstantPoolEntry::Unused);
    for _i in 1..constant_pool_count {
        let constant_type = read_u1(bytes, ix)?;
        constant_pool.push(match constant_type {
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
            _ => return Err(format!("Unexpected constant pool entry type {:?}", constant_type)),
        });
        if constant_type == 5 || constant_type == 6 {
            // long and double types take up two entries in the constant pool,
            // so eat up another index.
            constant_pool.push(ConstantPoolEntry::Unused);
        }
    }
    Ok(constant_pool)
}

fn read_interfaces<'a>(bytes: &'a [u8], ix: &mut usize, interfaces_count: u16) -> Result<Vec<ConstantPoolRef<'a>>, String> {
    let mut interfaces = Vec::new();
    for _i in 0..interfaces_count {
        interfaces.push(read_cp_ref(bytes, ix)?);
    }
    Ok(interfaces)
}

#[derive(Debug)]
pub struct AttributeInfo<'a> {
    pub name: ConstantPoolRef<'a>,
    info: &'a [u8],
}

fn read_attributes<'a>(bytes: &'a [u8], ix: &mut usize, attributes_count: u16) -> Result<Vec<AttributeInfo<'a>>, String> {
    let mut attributes = Vec::new();
    for _i in 0..attributes_count {
        let name = read_cp_ref(bytes, ix)?;
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
    pub name: ConstantPoolRef<'a>,
    pub descriptor: ConstantPoolRef<'a>,
    pub attributes: Vec<AttributeInfo<'a>>,
}

fn read_fields<'a>(bytes: &'a [u8], ix: &mut usize, fields_count: u16) -> Result<Vec<FieldInfo<'a>>, String> {
    let mut fields = Vec::new();
    for _i in 0..fields_count {
        let access_flags = FieldAccessFlags::from_bits(read_u2(bytes, ix)?).ok_or("Invalid access flags found on field")?;
        let name = read_cp_ref(bytes, ix)?;
        let descriptor = read_cp_ref(bytes, ix)?;
        let attributes_count = read_u2(bytes, ix)?;
        let attributes = read_attributes(bytes, ix, attributes_count)?;
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
    pub name: ConstantPoolRef<'a>,
    pub descriptor: ConstantPoolRef<'a>,
    pub attributes: Vec<AttributeInfo<'a>>,
}

fn read_methods<'a>(bytes: &'a [u8], ix: &mut usize, methods_count: u16) -> Result<Vec<MethodInfo<'a>>, String> {
    let mut methods = Vec::new();
    for _i in 0..methods_count {
        let access_flags = MethodAccessFlags::from_bits(read_u2(bytes, ix)?).ok_or("Invalid access flags found on method")?;
        let name = read_cp_ref(bytes, ix)?;
        let descriptor = read_cp_ref(bytes, ix)?;
        let attributes_count = read_u2(bytes, ix)?;
        let attributes = read_attributes(bytes, ix, attributes_count)?;
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
    pub constant_pool: Vec<ConstantPoolEntry<'a>>,
    pub access_flags: ClassAccessFlags,
    pub this_class: ConstantPoolRef<'a>,
    pub super_class: ConstantPoolRef<'a>,
    pub interfaces: Vec<ConstantPoolRef<'a>>,
    pub fields: Vec<FieldInfo<'a>>,
    pub methods: Vec<MethodInfo<'a>>,
    pub attributes: Vec<AttributeInfo<'a>>,
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
    let access_flags = ClassAccessFlags::from_bits(read_u2(raw_bytes, &mut ix)?).ok_or("Invalid access flags found on class")?;
    let this_class = read_cp_ref(raw_bytes, &mut ix)?;
    let super_class = read_cp_ref(raw_bytes, &mut ix)?;
    let interfaces_count = read_u2(raw_bytes, &mut ix)?;
    let interfaces = read_interfaces(raw_bytes, &mut ix, interfaces_count)?;
    let fields_count = read_u2(raw_bytes, &mut ix)?;
    let fields = read_fields(raw_bytes, &mut ix, fields_count)?;
    let methods_count = read_u2(raw_bytes, &mut ix)?;
    let methods = read_methods(raw_bytes, &mut ix, methods_count)?;
    let attributes_count = read_u2(raw_bytes, &mut ix)?;
    let attributes = read_attributes(raw_bytes, &mut ix, attributes_count)?;

    Ok(ClassFile {
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
    })
}
