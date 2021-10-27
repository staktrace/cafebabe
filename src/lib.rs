#![deny(clippy::all)]

#[macro_use]
extern crate bitflags;
extern crate cesu8;

#[macro_use]
pub mod error;

pub mod attributes;
pub mod bytecode;
pub mod constant_pool;
pub mod names;

use std::borrow::Cow;
use std::collections::HashSet;
use std::ops::Deref;
use std::rc::Rc;

use crate::attributes::{read_attributes, AttributeData, AttributeInfo};
use crate::constant_pool::{
    read_constant_pool, read_cp_classinfo, read_cp_classinfo_opt, read_cp_utf8, ConstantPoolEntry,
    ConstantPoolIter,
};
pub use crate::error::ParseError;
use crate::names::{is_field_descriptor, is_method_descriptor, is_unqualified_name};

pub(crate) fn read_u1(bytes: &[u8], ix: &mut usize) -> Result<u8, ParseError> {
    if bytes.len() < *ix + 1 {
        fail!("Unexpected end of stream reading u1 at index {}", *ix);
    }
    let result = bytes[*ix];
    *ix += 1;
    Ok(result)
}

#[allow(clippy::double_parens, clippy::identity_op)]
pub(crate) fn read_u2(bytes: &[u8], ix: &mut usize) -> Result<u16, ParseError> {
    if bytes.len() < *ix + 2 {
        fail!("Unexpected end of stream reading u2 at index {}", *ix);
    }
    let result = ((bytes[*ix + 0] as u16) << 8) | (bytes[*ix + 1] as u16);
    *ix += 2;
    Ok(result)
}

#[allow(clippy::double_parens, clippy::identity_op)]
pub(crate) fn read_u4(bytes: &[u8], ix: &mut usize) -> Result<u32, ParseError> {
    if bytes.len() < *ix + 4 {
        fail!("Unexpected end of stream reading u4 at index {}", *ix);
    }
    let result = ((bytes[*ix + 0] as u32) << 24)
        | ((bytes[*ix + 1] as u32) << 16)
        | ((bytes[*ix + 2] as u32) << 8)
        | (bytes[*ix + 3] as u32);
    *ix += 4;
    Ok(result)
}

#[allow(clippy::double_parens, clippy::identity_op)]
pub(crate) fn read_u8(bytes: &[u8], ix: &mut usize) -> Result<u64, ParseError> {
    if bytes.len() < *ix + 8 {
        fail!("Unexpected end of stream reading u8 at index {}", *ix);
    }
    let result = ((bytes[*ix + 0] as u64) << 56)
        | ((bytes[*ix + 1] as u64) << 48)
        | ((bytes[*ix + 2] as u64) << 40)
        | ((bytes[*ix + 3] as u64) << 32)
        | ((bytes[*ix + 4] as u64) << 24)
        | ((bytes[*ix + 5] as u64) << 16)
        | ((bytes[*ix + 6] as u64) << 8)
        | (bytes[*ix + 7] as u64);
    *ix += 8;
    Ok(result)
}

fn read_interfaces<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<Vec<Cow<'a, str>>, ParseError> {
    let count = read_u2(bytes, ix)?;
    let mut interfaces = Vec::with_capacity(count.into());
    for i in 0..count {
        interfaces
            .push(read_cp_classinfo(bytes, ix, pool).map_err(|e| err!(e, "interface {}", i))?);
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
        const OPEN = 0x0020;
        const TRANSITIVE = 0x0020;
        const VOLATILE = 0x0040;
        const BRIDGE = 0x0040;
        const STATIC_PHASE = 0x0040;
        const TRANSIENT = 0x0080;
        const VARARGS = 0x0080;
        const NATIVE = 0x0100;
        const INTERFACE = 0x0200;
        const ABSTRACT = 0x0400;
        const STRICT = 0x0800;
        const SYNTHETIC = 0x1000;
        const ANNOTATION = 0x2000;
        const ENUM = 0x4000;
        const MANDATED = 0x8000;
        const MODULE = 0x8000;
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
    pub name: Cow<'a, str>,
    pub descriptor: Cow<'a, str>,
    pub attributes: Vec<AttributeInfo<'a>>,
}

fn read_fields<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
    opts: &ParseOptions,
) -> Result<Vec<FieldInfo<'a>>, ParseError> {
    let count = read_u2(bytes, ix)?;
    let mut fields = Vec::with_capacity(count.into());
    let mut unique_ids: HashSet<(Cow<'a, str>, Cow<'a, str>)> = HashSet::new();
    for i in 0..count {
        let access_flags = FieldAccessFlags::from_bits_truncate(read_u2(bytes, ix)?);
        let name =
            read_cp_utf8(bytes, ix, pool).map_err(|e| err!(e, "name of class field {}", i))?;
        if !is_unqualified_name(&name, false, false) {
            fail!("Invalid unqualified name for class field {}", i);
        }
        let descriptor = read_cp_utf8(bytes, ix, pool)
            .map_err(|e| err!(e, "descriptor of class field {}", i))?;
        if !is_field_descriptor(&descriptor) {
            fail!("Invalid descriptor for class field {}", i);
        }
        let unique_id = (name.clone(), descriptor.clone());
        if !unique_ids.insert(unique_id) {
            fail!(
                "Class field {} is duplicate of previously-encountered field",
                i
            );
        }
        let attributes =
            read_attributes(bytes, ix, pool, opts).map_err(|e| err!(e, "class field {}", i))?;
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
    pub name: Cow<'a, str>,
    pub descriptor: Cow<'a, str>,
    pub attributes: Vec<AttributeInfo<'a>>,
}

fn read_methods<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
    opts: &ParseOptions,
    in_interface: bool,
    major_version: u16,
) -> Result<Vec<MethodInfo<'a>>, ParseError> {
    let count = read_u2(bytes, ix)?;
    let mut methods = Vec::with_capacity(count.into());
    let mut unique_ids: HashSet<(Cow<'a, str>, Cow<'a, str>)> = HashSet::new();
    for i in 0..count {
        let access_flags = MethodAccessFlags::from_bits_truncate(read_u2(bytes, ix)?);
        let name =
            read_cp_utf8(bytes, ix, pool).map_err(|e| err!(e, "name of class method {}", i))?;
        let allow_init = !in_interface;
        if !is_unqualified_name(&name, allow_init, true) {
            fail!("Invalid unqualified name for class method {}", i);
        }
        let descriptor = read_cp_utf8(bytes, ix, pool)
            .map_err(|e| err!(e, "descriptor of class method {}", i))?;
        if !is_method_descriptor(&descriptor) {
            fail!("Invalid descriptor for class method {}", i);
        }
        if allow_init && name == "<init>" && !descriptor.ends_with('V') {
            fail!("Non-void method descriptor for init method {}", i);
        }
        if name == "<clinit>" {
            if !descriptor.ends_with('V') {
                fail!("Non-void method descriptor for clinit method {}", i);
            }
            if major_version >= 51 && !descriptor.starts_with("()") {
                fail!("Arguments found in descriptor for clinit method {}", i);
            }
        }
        let unique_id = (name.clone(), descriptor.clone());
        if !unique_ids.insert(unique_id) {
            fail!(
                "Class method {} is duplicate of previously-encountered method",
                i
            );
        }
        let attributes =
            read_attributes(bytes, ix, pool, opts).map_err(|e| err!(e, "class method {}", i))?;
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
        const MODULE = AccessFlags::MODULE.bits();
    }
}

fn validate_bootstrap_methods<'a>(
    pool: &[Rc<ConstantPoolEntry<'a>>],
    attributes: &[AttributeInfo<'a>],
) -> Result<(), ParseError> {
    for cp_entry in pool {
        match cp_entry.deref() {
            ConstantPoolEntry::Dynamic(x, _) | ConstantPoolEntry::InvokeDynamic(x, _) => {
                let mut found = 0;
                for attr in attributes {
                    match &attr.data {
                        AttributeData::BootstrapMethods(methods) => {
                            found += 1;
                            if usize::from(*x) >= methods.len() {
                                fail!("Constant pool item contained invalid index into BootstrapMethods class attribute");
                            }
                        }
                        _ => continue,
                    }
                }
                if found != 1 {
                    fail!("Found unexpected number of BootstrapMethods attributes in class; found {}, expected 1", found);
                }
            }
            _ => continue,
        };
    }
    Ok(())
}

#[derive(Debug)]
pub struct ClassFile<'a> {
    pub major_version: u16,
    pub minor_version: u16,
    constant_pool: Vec<Rc<ConstantPoolEntry<'a>>>,
    pub access_flags: ClassAccessFlags,
    pub this_class: Cow<'a, str>,
    pub super_class: Option<Cow<'a, str>>,
    pub interfaces: Vec<Cow<'a, str>>,
    pub fields: Vec<FieldInfo<'a>>,
    pub methods: Vec<MethodInfo<'a>>,
    pub attributes: Vec<AttributeInfo<'a>>,
}

impl<'a> ClassFile<'a> {
    #[must_use]
    pub fn constantpool_iter(&'a self) -> ConstantPoolIter<'a> {
        ConstantPoolIter::new(&self.constant_pool)
    }
}

#[derive(Debug)]
pub struct ParseOptions {
    parse_bytecode: bool,
}

impl ParseOptions {
    pub fn default() -> Self {
        Self {
            parse_bytecode: true,
        }
    }

    /// Turns on or off parsing of bytecode from the Code attributes of methods. If parsing
    /// is enabled, the CodeData structure's optional bytecode field will be populated
    /// (or parsing will fail entirely if bytecode parsing failed). If parsing is disabled,
    /// the CodeData structure's optional bytecode field will be set to None. Parsing is
    /// enabled by default, but can be disabled to speed up parsing in cases where the
    /// parsed bytecode is not needed.
    pub fn parse_bytecode(&mut self, parse: bool) -> &mut ParseOptions {
        self.parse_bytecode = parse;
        self
    }
}

#[allow(clippy::needless_lifetimes)]
pub fn parse_class<'a>(raw_bytes: &'a [u8]) -> Result<ClassFile<'a>, ParseError> {
    parse_class_with_options(raw_bytes, &ParseOptions::default())
}

pub fn parse_class_with_options<'a>(
    raw_bytes: &'a [u8],
    opts: &ParseOptions,
) -> Result<ClassFile<'a>, ParseError> {
    let mut ix = 0;
    if read_u4(raw_bytes, &mut ix)? != 0xCAFE_BABE {
        fail!("Unexpected magic header");
    }
    let minor_version = read_u2(raw_bytes, &mut ix)?;
    let major_version = read_u2(raw_bytes, &mut ix)?;
    let constant_pool = read_constant_pool(raw_bytes, &mut ix, major_version)?;

    let access_flags = ClassAccessFlags::from_bits_truncate(read_u2(raw_bytes, &mut ix)?);
    let is_module = access_flags.contains(ClassAccessFlags::MODULE);
    if is_module {
        if major_version < 53 {
            fail!(
                "Found invalid MODULE class access flag on class file of major version {}",
                major_version
            );
        }
        if access_flags != ClassAccessFlags::MODULE {
            fail!(
                "Found invalid class access flags {:?}; no other flags should be set with MODULE",
                access_flags
            );
        }
    }
    let this_class =
        read_cp_classinfo(raw_bytes, &mut ix, &constant_pool).map_err(|e| err!(e, "this_class"))?;
    let super_class = read_cp_classinfo_opt(raw_bytes, &mut ix, &constant_pool)
        .map_err(|e| err!(e, "super_class"))?;
    let interfaces = read_interfaces(raw_bytes, &mut ix, &constant_pool)?;
    let fields = read_fields(raw_bytes, &mut ix, &constant_pool, opts)?;
    let methods = read_methods(
        raw_bytes,
        &mut ix,
        &constant_pool,
        opts,
        access_flags.contains(ClassAccessFlags::INTERFACE),
        major_version,
    )?;
    let attributes =
        read_attributes(raw_bytes, &mut ix, &constant_pool, opts).map_err(|e| err!(e, "class"))?;
    // Section 4.8 "Format Checking" says the class file must not have extra bytes at the end
    if ix != raw_bytes.len() {
        fail!("Extra bytes found at index {} after reading class file", ix);
    }

    if is_module {
        if let Some(super_class) = super_class {
            fail!(
                "Found non-empty super_class {}; expected none for module",
                super_class
            );
        }
        if !interfaces.is_empty() {
            fail!(
                "Found {} interfaces; expected 0 for module",
                interfaces.len()
            );
        }
        if !fields.is_empty() {
            fail!("Found {} fields; expected 0 for module", fields.len());
        }
        if !methods.is_empty() {
            fail!("Found {} methods; expected 0 for module", methods.len());
        }
    }

    validate_bootstrap_methods(&constant_pool, &attributes)?;

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
