#[macro_use]
extern crate bitflags;
extern crate cesu8;

mod attributes;
mod constant_pool;

use std::borrow::Cow;
use std::ops::Deref;
use std::rc::Rc;

use crate::attributes::{AttributeInfo, read_attributes};
use crate::constant_pool::{ConstantPoolEntry, ConstantPoolEntryTypes, read_constant_pool, read_cp_ref};

pub(crate) fn err<T>(msg: &'static str) -> Result<T, String> {
    Err(msg.to_string())
}

pub(crate) fn read_u1(bytes: &[u8], ix: &mut usize) -> Result<u8, String> {
    if bytes.len() < *ix + 1 {
        return Err(format!("Unexpected end of stream reading u2 at index {}", *ix));
    }
    let result = bytes[*ix];
    *ix += 1;
    Ok(result)
}

pub(crate) fn read_u2(bytes: &[u8], ix: &mut usize) -> Result<u16, String> {
    if bytes.len() < *ix + 2 {
        return Err(format!("Unexpected end of stream reading u2 at index {}", *ix));
    }
    let result =
        ((bytes[*ix + 0] as u16) << 8) |
        ((bytes[*ix + 1] as u16));
    *ix += 2;
    Ok(result)
}

pub(crate) fn read_u4(bytes: &[u8], ix: &mut usize) -> Result<u32, String> {
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

pub(crate) fn read_u8(bytes: &[u8], ix: &mut usize) -> Result<u64, String> {
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
pub(crate) enum BootstrapMethodRef {
    Unresolved(u16),
}

fn read_interfaces<'a>(bytes: &'a [u8], ix: &mut usize, interfaces_count: u16, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<Rc<ConstantPoolEntry<'a>>>, String> {
    let mut interfaces = Vec::new();
    for i in 0..interfaces_count {
        interfaces.push(read_cp_ref(bytes, ix, pool, ConstantPoolEntryTypes::CLASS_INFO).map_err(|e| format!("{} interface {}", e, i))?);
    }
    Ok(interfaces)
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
    pub fn name(&self) -> Cow<'a, str> {
        self.name.utf8()
    }

    pub fn descriptor(&self) -> Cow<'a, str> {
        self.descriptor.utf8()
    }
}

fn read_fields<'a>(bytes: &'a [u8], ix: &mut usize, fields_count: u16, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<FieldInfo<'a>>, String> {
    let mut fields = Vec::new();
    for i in 0..fields_count {
        let access_flags = FieldAccessFlags::from_bits(read_u2(bytes, ix)?).ok_or("Invalid access flags found on field")?;
        let name = read_cp_ref(bytes, ix, pool, ConstantPoolEntryTypes::UTF8).map_err(|e| format!("{} name of class field {}", e, i))?;
        let descriptor = read_cp_ref(bytes, ix, pool, ConstantPoolEntryTypes::UTF8).map_err(|e| format!("{} descriptor of class field {}", e, i))?;
        let attributes_count = read_u2(bytes, ix)?;
        let attributes = read_attributes(bytes, ix, attributes_count, pool).map_err(|e| format!("{} of class field {}", e, i))?;
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
    pub fn name(&self) -> Cow<'a, str> {
        self.name.utf8()
    }

    pub fn descriptor(&self) -> Cow<'a, str> {
        self.descriptor.utf8()
    }
}

fn read_methods<'a>(bytes: &'a [u8], ix: &mut usize, methods_count: u16, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<MethodInfo<'a>>, String> {
    let mut methods = Vec::new();
    for i in 0..methods_count {
        let access_flags = MethodAccessFlags::from_bits(read_u2(bytes, ix)?).ok_or("Invalid access flags found on method")?;
        let name = read_cp_ref(bytes, ix, pool, ConstantPoolEntryTypes::UTF8).map_err(|e| format!("{} name of class method {}", e, i))?;
        let descriptor = read_cp_ref(bytes, ix, pool, ConstantPoolEntryTypes::UTF8).map_err(|e| format!("{} descriptor of class method {}", e, i))?;
        let attributes_count = read_u2(bytes, ix)?;
        let attributes = read_attributes(bytes, ix, attributes_count, pool).map_err(|e| format!("{} of class method {}", e, i))?;
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
    pub fn this_class_name(&self) -> Cow<'a, str> {
        self.this_class.classinfo_utf8()
    }

    pub fn super_class_name(&self) -> Option<Cow<'a, str>> {
        match self.super_class.deref() {
            ConstantPoolEntry::Zero => None,
            _ => Some(self.super_class.classinfo_utf8()),
        }
    }

    pub fn interface_names(&self) -> Vec<Cow<'a, str>> {
        self.interfaces
            .iter()
            .map(|interface| interface.classinfo_utf8())
            .collect()
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
    let constant_pool = read_constant_pool(raw_bytes, &mut ix, constant_pool_count, major_version)?;

    let access_flags = ClassAccessFlags::from_bits(read_u2(raw_bytes, &mut ix)?).ok_or("Invalid access flags found on class")?;
    let this_class = read_cp_ref(raw_bytes, &mut ix, &constant_pool, ConstantPoolEntryTypes::CLASS_INFO).map_err(|e| format!("{} this_class", e))?;
    let super_class = read_cp_ref(raw_bytes, &mut ix, &constant_pool, ConstantPoolEntryTypes::CLASS_OR_ZERO).map_err(|e| format!("{} super_class", e))?;
    let interfaces_count = read_u2(raw_bytes, &mut ix)?;
    let interfaces = read_interfaces(raw_bytes, &mut ix, interfaces_count, &constant_pool)?;
    let fields_count = read_u2(raw_bytes, &mut ix)?;
    let fields = read_fields(raw_bytes, &mut ix, fields_count, &constant_pool)?;
    let methods_count = read_u2(raw_bytes, &mut ix)?;
    let methods = read_methods(raw_bytes, &mut ix, methods_count, &constant_pool)?;
    let attributes_count = read_u2(raw_bytes, &mut ix)?;
    let attributes = read_attributes(raw_bytes, &mut ix, attributes_count, &constant_pool).map_err(|e| format!("{} of class", e))?;

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
    Ok(class_file)
}
