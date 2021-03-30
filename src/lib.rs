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
    let name_ref = ConstantPoolRef::Unresolved(read_u2(bytes, ix)?);
    Ok(ConstantPoolEntry::ClassInfo(name_ref))
}

fn read_constant_string<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let value_ref = ConstantPoolRef::Unresolved(read_u2(bytes, ix)?);
    Ok(ConstantPoolEntry::String(value_ref))
}

fn read_constant_fieldref<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let class_ref = ConstantPoolRef::Unresolved(read_u2(bytes, ix)?);
    let name_and_type_ref = ConstantPoolRef::Unresolved(read_u2(bytes, ix)?);
    Ok(ConstantPoolEntry::FieldRef(class_ref, name_and_type_ref))
}

fn read_constant_methodref<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let class_ref = ConstantPoolRef::Unresolved(read_u2(bytes, ix)?);
    let name_and_type_ref = ConstantPoolRef::Unresolved(read_u2(bytes, ix)?);
    Ok(ConstantPoolEntry::MethodRef(class_ref, name_and_type_ref))
}

fn read_constant_interfacemethodref<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let class_ref = ConstantPoolRef::Unresolved(read_u2(bytes, ix)?);
    let name_and_type_ref = ConstantPoolRef::Unresolved(read_u2(bytes, ix)?);
    Ok(ConstantPoolEntry::InterfaceMethodRef(class_ref, name_and_type_ref))
}

fn read_constant_nameandtype<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let name_ref = ConstantPoolRef::Unresolved(read_u2(bytes, ix)?);
    let descriptor_ref = ConstantPoolRef::Unresolved(read_u2(bytes, ix)?);
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
    let reference_ref = ConstantPoolRef::Unresolved(read_u2(bytes, ix)?);
    Ok(ConstantPoolEntry::MethodHandle(reference_kind, reference_ref))
}

fn read_constant_methodtype<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let descriptor_ref = ConstantPoolRef::Unresolved(read_u2(bytes, ix)?);
    Ok(ConstantPoolEntry::MethodType(descriptor_ref))
}

fn read_constant_invokedynamic<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<ConstantPoolEntry<'a>, String> {
    let bootstrap_method_ref = BootstrapMethodRef::Unresolved(read_u2(bytes, ix)?);
    let name_and_type_ref = ConstantPoolRef::Unresolved(read_u2(bytes, ix)?);
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

#[derive(Debug)]
pub struct ClassFile<'a> {
    pub major_version: u16,
    pub minor_version: u16,
    pub constant_pool: Vec<ConstantPoolEntry<'a>>,
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

    Ok(ClassFile {
        major_version,
        minor_version,
        constant_pool,
    })
}
