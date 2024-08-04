use std::borrow::Cow;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

use crate::names::{
    is_array_descriptor, is_binary_name, is_field_descriptor, is_method_descriptor, is_module_name,
    is_unqualified_method_name, is_unqualified_name,
};
use crate::{read_u1, read_u2, read_u4, read_u8, ParseError};

#[derive(Debug)]
pub(crate) enum ConstantPoolRef<'a> {
    Unresolved(u16),
    Resolved(Rc<ConstantPoolEntry<'a>>),
}

impl<'a> ConstantPoolRef<'a> {
    fn resolve(
        &mut self,
        my_index: usize,
        pool: &[Rc<ConstantPoolEntry<'a>>],
    ) -> Result<(), ParseError> {
        match self {
            ConstantPoolRef::Unresolved(ix) => {
                let target = *ix as usize;
                if target == my_index {
                    fail!("Constant pool entry at index {} could not be resolved due to self-reference", my_index);
                }
                if target >= pool.len() {
                    fail!(
                        "Constant pool entry at index {} references out-of-bounds index {}",
                        my_index,
                        target
                    );
                }
                *self = ConstantPoolRef::Resolved(pool[target].clone());
                Ok(())
            }
            ConstantPoolRef::Resolved(_) => Ok(()),
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
    fn resolve(
        &self,
        cp_index: usize,
        pool: &[Rc<ConstantPoolEntry<'a>>],
    ) -> Result<(), ParseError>;
    fn ensure_type(&self, allowed: ConstantPoolEntryTypes) -> Result<(), ParseError>;
}

impl<'a> RefCellDeref<'a> for RefCell<ConstantPoolRef<'a>> {
    fn resolve(
        &self,
        cp_index: usize,
        pool: &[Rc<ConstantPoolEntry<'a>>],
    ) -> Result<(), ParseError> {
        self.borrow_mut().resolve(cp_index, pool)
    }

    fn ensure_type(&self, allowed: ConstantPoolEntryTypes) -> Result<(), ParseError> {
        self.borrow().get().ensure_type(allowed)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Hash)]
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
        const ZERO = 0x0000_0001;
        const UTF8 = 0x0000_0002;
        const INTEGER = 0x0000_0004;
        const FLOAT = 0x0000_0008;
        const LONG = 0x0000_0010;
        const DOUBLE = 0x0000_0020;
        const CLASS_INFO = 0x0000_0040;
        const STRING = 0x0000_0080;
        const FIELD_REF = 0x0000_0100;
        const METHOD_REF = 0x0000_0200;
        const INTERFACE_METHOD_REF = 0x0000_0400;
        const NAME_AND_TYPE = 0x0000_0800;
        const METHOD_HANDLE = 0x0000_1000;
        const METHOD_TYPE = 0x0000_2000;
        const DYNAMIC = 0x0000_4000;
        const INVOKE_DYNAMIC = 0x0000_8000;
        const MODULE_INFO = 0x0001_0000;
        const PACKAGE_INFO = 0x0002_0000;
        const UNUSED = 0x0004_0000;

        const NEW_METHOD_REFS = Self::METHOD_REF.bits() | Self::INTERFACE_METHOD_REF.bits();
        const CONSTANTS = Self::INTEGER.bits() | Self::FLOAT.bits() | Self::LONG.bits() | Self::DOUBLE.bits() | Self::STRING.bits();
        const BOOTSTRAP_ARGUMENT = Self::CONSTANTS.bits() | Self::CLASS_INFO.bits() | Self::METHOD_HANDLE.bits() | Self::METHOD_TYPE.bits();
        const LOADABLE = Self::CONSTANTS.bits() | Self::CLASS_INFO.bits() | Self::METHOD_HANDLE.bits() | Self::METHOD_TYPE.bits() | Self::DYNAMIC.bits();
    }
}

type BootstrapMethodRef = u16;

#[derive(Debug)]
pub(crate) enum ConstantPoolEntry<'a> {
    Zero,
    Utf8(Cow<'a, str>),
    Utf8Bytes(&'a [u8]),
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
    fn resolve(
        &self,
        my_index: usize,
        pool: &[Rc<ConstantPoolEntry<'a>>],
    ) -> Result<(), ParseError> {
        match self {
            // Entry types that do not reference other entries:
            ConstantPoolEntry::Zero
            | ConstantPoolEntry::Utf8(_)
            | ConstantPoolEntry::Utf8Bytes(_)
            | ConstantPoolEntry::Integer(_)
            | ConstantPoolEntry::Float(_)
            | ConstantPoolEntry::Long(_)
            | ConstantPoolEntry::Double(_)
            | ConstantPoolEntry::Unused => Ok(()),

            // Entry types that reference one other entry:
            ConstantPoolEntry::ClassInfo(x)
            | ConstantPoolEntry::String(x)
            | ConstantPoolEntry::MethodHandle(_, x)
            | ConstantPoolEntry::MethodType(x)
            | ConstantPoolEntry::Dynamic(_, x)
            | ConstantPoolEntry::InvokeDynamic(_, x)
            | ConstantPoolEntry::ModuleInfo(x)
            | ConstantPoolEntry::PackageInfo(x) => x.resolve(my_index, pool),

            // Entry types that reference two other entries:
            ConstantPoolEntry::FieldRef(x, y)
            | ConstantPoolEntry::MethodRef(x, y)
            | ConstantPoolEntry::InterfaceMethodRef(x, y)
            | ConstantPoolEntry::NameAndType(x, y) => {
                x.resolve(my_index, pool)?;
                y.resolve(my_index, pool)
            }
        }
    }

    fn get_type(&self) -> ConstantPoolEntryTypes {
        match self {
            ConstantPoolEntry::Zero => ConstantPoolEntryTypes::ZERO,
            ConstantPoolEntry::Utf8(_) | ConstantPoolEntry::Utf8Bytes(_) => {
                ConstantPoolEntryTypes::UTF8
            }
            ConstantPoolEntry::Integer(_) => ConstantPoolEntryTypes::INTEGER,
            ConstantPoolEntry::Float(_) => ConstantPoolEntryTypes::FLOAT,
            ConstantPoolEntry::Long(_) => ConstantPoolEntryTypes::LONG,
            ConstantPoolEntry::Double(_) => ConstantPoolEntryTypes::DOUBLE,
            ConstantPoolEntry::ClassInfo(_) => ConstantPoolEntryTypes::CLASS_INFO,
            ConstantPoolEntry::String(_) => ConstantPoolEntryTypes::STRING,
            ConstantPoolEntry::FieldRef(_, _) => ConstantPoolEntryTypes::FIELD_REF,
            ConstantPoolEntry::MethodRef(_, _) => ConstantPoolEntryTypes::METHOD_REF,
            ConstantPoolEntry::InterfaceMethodRef(_, _) => {
                ConstantPoolEntryTypes::INTERFACE_METHOD_REF
            }
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

    fn validate(&self, major_version: u16) -> Result<(), ParseError> {
        match self {
            ConstantPoolEntry::ClassInfo(x) => {
                x.ensure_type(ConstantPoolEntryTypes::UTF8)?;
                x.borrow().get().validate_classinfo_name()
            }
            ConstantPoolEntry::String(x) => x.ensure_type(ConstantPoolEntryTypes::UTF8),
            ConstantPoolEntry::FieldRef(x, y) => {
                x.ensure_type(ConstantPoolEntryTypes::CLASS_INFO)?;
                y.ensure_type(ConstantPoolEntryTypes::NAME_AND_TYPE)?;
                y.borrow().get().validate_field_descriptor()
            }
            ConstantPoolEntry::MethodRef(x, y) => {
                x.ensure_type(ConstantPoolEntryTypes::CLASS_INFO)?;
                y.ensure_type(ConstantPoolEntryTypes::NAME_AND_TYPE)?;
                y.borrow().get().validate_method_descriptor()
            }
            ConstantPoolEntry::InterfaceMethodRef(x, y) => {
                x.ensure_type(ConstantPoolEntryTypes::CLASS_INFO)?;
                y.ensure_type(ConstantPoolEntryTypes::NAME_AND_TYPE)?;
                y.borrow().get().validate_method_descriptor()
            }
            ConstantPoolEntry::NameAndType(x, y) => {
                x.ensure_type(ConstantPoolEntryTypes::UTF8)?;
                x.borrow().get().validate_unqualified_name()?;
                y.ensure_type(ConstantPoolEntryTypes::UTF8)
                // y is validated as part of FieldRef/MethodRef/InterfaceMethodRef/Dynamic/InvokeDynamic pool item validation
            }
            ConstantPoolEntry::MethodHandle(x, y) => y.ensure_type(match x {
                ReferenceKind::GetField
                | ReferenceKind::GetStatic
                | ReferenceKind::PutField
                | ReferenceKind::PutStatic => ConstantPoolEntryTypes::FIELD_REF,
                ReferenceKind::InvokeVirtual | ReferenceKind::NewInvokeSpecial => {
                    ConstantPoolEntryTypes::METHOD_REF
                }
                ReferenceKind::InvokeStatic | ReferenceKind::InvokeSpecial => {
                    if major_version < 52 {
                        ConstantPoolEntryTypes::METHOD_REF
                    } else {
                        ConstantPoolEntryTypes::NEW_METHOD_REFS
                    }
                }
                ReferenceKind::InvokeInterface => ConstantPoolEntryTypes::INTERFACE_METHOD_REF,
            }),
            ConstantPoolEntry::MethodType(x) => {
                x.ensure_type(ConstantPoolEntryTypes::UTF8)?;
                x.borrow().get().validate_method_descriptor()
            }
            ConstantPoolEntry::Dynamic(_, y) => {
                y.ensure_type(ConstantPoolEntryTypes::NAME_AND_TYPE)?;
                y.borrow().get().validate_field_descriptor()
            }
            ConstantPoolEntry::InvokeDynamic(_, y) => {
                y.ensure_type(ConstantPoolEntryTypes::NAME_AND_TYPE)?;
                y.borrow().get().validate_method_descriptor()
            }
            ConstantPoolEntry::ModuleInfo(x) => {
                x.ensure_type(ConstantPoolEntryTypes::UTF8)?;
                x.borrow().get().validate_module_name()
            }
            ConstantPoolEntry::PackageInfo(x) => {
                x.ensure_type(ConstantPoolEntryTypes::UTF8)?;
                x.borrow().get().validate_binary_name()
            }

            // Entry types that do not reference other entries:
            ConstantPoolEntry::Zero
            | ConstantPoolEntry::Utf8(_)
            | ConstantPoolEntry::Utf8Bytes(_)
            | ConstantPoolEntry::Integer(_)
            | ConstantPoolEntry::Float(_)
            | ConstantPoolEntry::Long(_)
            | ConstantPoolEntry::Double(_)
            | ConstantPoolEntry::Unused => Ok(()),
        }
    }

    fn ensure_type(&self, allowed: ConstantPoolEntryTypes) -> Result<(), ParseError> {
        if allowed.contains(self.get_type()) {
            Ok(())
        } else {
            fail!("Unexpected constant pool reference type")
        }
    }

    /// Returns &str if self if Utf8. Returns parse error otherwise.
    /// The parse error will have a special message if self is Utf8Bytes
    /// because such an odd case might be difficult to debug otherwise.
    fn str(&self) -> Result<&str, ParseError> {
        match self {
            ConstantPoolEntry::Utf8(x) => Ok(x),
            ConstantPoolEntry::Utf8Bytes(_) => {
                fail!("Attempting to get utf-8 data from non-utf8 constant pool entry!")
            }
            _ => fail!("Unexpected constant pool reference type"),
        }
    }

    fn validate_classinfo_name(&self) -> Result<(), ParseError> {
        let x = self.str()?;
        // Per 4.4.1, classinfo names are allowed to be array descriptors too. This happens in the java 16 modules file.
        if is_binary_name(x) || is_array_descriptor(x) {
            Ok(())
        } else {
            fail!("Invalid classinfo name")
        }
    }

    fn validate_binary_name(&self) -> Result<(), ParseError> {
        if is_binary_name(self.str()?) {
            Ok(())
        } else {
            fail!("Invalid binary name")
        }
    }

    fn validate_unqualified_name(&self) -> Result<(), ParseError> {
        let s = self.str()?;
        if is_unqualified_name(s) || is_unqualified_method_name(s, true, false) {
            Ok(())
        } else {
            fail!("Invalid unqualified name")
        }
    }

    fn validate_module_name(&self) -> Result<(), ParseError> {
        if is_module_name(self.str()?) {
            Ok(())
        } else {
            fail!("Invalid module name")
        }
    }

    fn validate_field_descriptor(&self) -> Result<(), ParseError> {
        match self {
            ConstantPoolEntry::NameAndType(_, y) => {
                if is_field_descriptor(y.borrow().get().str()?) {
                    Ok(())
                } else {
                    fail!("Invalid field descriptor")
                }
            }
            _ => panic!("Attempting to get descriptor from non-NameAndType constant pool entry!"),
        }
    }

    fn validate_method_descriptor(&self) -> Result<(), ParseError> {
        match self {
            ConstantPoolEntry::NameAndType(_, y) => y.borrow().get().validate_method_descriptor(),
            _ => {
                if is_method_descriptor(self.str()?) {
                    Ok(())
                } else {
                    fail!("Invalid method descriptor")
                }
            }
        }
    }

    fn utf8(&self) -> Cow<'a, str> {
        match self {
            ConstantPoolEntry::Utf8(x) => x.clone(),
            _ => panic!("Attempting to get utf-8 data from non-utf8 constant pool entry!"),
        }
    }

    fn string_literal(&self) -> LiteralConstant<'a> {
        match self {
            ConstantPoolEntry::Utf8(x) => LiteralConstant::String(x.clone()),
            ConstantPoolEntry::Utf8Bytes(x) => LiteralConstant::StringBytes(x),
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
            ConstantPoolEntry::NameAndType(x, y) => NameAndType {
                name: x.borrow().get().utf8(),
                descriptor: y.borrow().get().utf8(),
            },
            _ => panic!(
                "Attempting to get name and type data from non-name-and-type constant pool entry!"
            ),
        }
    }
}

fn read_unresolved_cp_ref<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<RefCell<ConstantPoolRef<'a>>, ParseError> {
    Ok(RefCell::new(ConstantPoolRef::Unresolved(read_u2(
        bytes, ix,
    )?)))
}

fn read_constant_utf8<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
    let length = read_u2(bytes, ix)? as usize;
    if bytes.len() < *ix + length {
        fail!(
            "Unexpected end of stream reading CONSTANT_Utf8 at index {}",
            *ix
        );
    }
    let modified_utf8_data = &bytes[*ix..*ix + length];
    *ix += length;
    // If a Java file contains a literal string such as:
    //   String watchThis = "\uDAB9\uBAF5";
    // then that gets encoded into the constant pool as a CONSTANT_Utf8 entry with bytes:
    //   0xED, 0xAA, 0xB9, 0xEB, 0xAB, 0xB4
    // which is not representable as a Rust string because decodes to an unpaired surrogate.
    // So for cases like these we need to be able to fall back to storing the raw bytes,
    // and we use the Utf8Bytes internal type for that. These should only occur in Java
    // literal constants, so we can still expose other things (like descriptors and classnames)
    // as Rust strings. Only literal Java strings need to be able to expose the raw bytes.
    match cesu8::from_java_cesu8(modified_utf8_data) {
        Ok(rust_str) => Ok(ConstantPoolEntry::Utf8(rust_str)),
        _ => Ok(ConstantPoolEntry::Utf8Bytes(modified_utf8_data)),
    }
}

fn read_constant_integer<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
    #![allow(clippy::cast_possible_wrap)] // Wrapping is allowed and desired.
    Ok(ConstantPoolEntry::Integer(read_u4(bytes, ix)? as i32))
}

fn read_constant_float<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
    Ok(ConstantPoolEntry::Float(f32::from_bits(read_u4(
        bytes, ix,
    )?)))
}

fn read_constant_long<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
    #![allow(clippy::cast_possible_wrap)] // Wrapping is allowed and desired.
    Ok(ConstantPoolEntry::Long(read_u8(bytes, ix)? as i64))
}

fn read_constant_double<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
    Ok(ConstantPoolEntry::Double(f64::from_bits(read_u8(
        bytes, ix,
    )?)))
}

fn read_constant_class<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
    let name_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::ClassInfo(name_ref))
}

fn read_constant_string<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
    let value_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::String(value_ref))
}

fn read_constant_fieldref<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
    let class_ref = read_unresolved_cp_ref(bytes, ix)?;
    let name_and_type_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::FieldRef(class_ref, name_and_type_ref))
}

fn read_constant_methodref<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
    let class_ref = read_unresolved_cp_ref(bytes, ix)?;
    let name_and_type_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::MethodRef(class_ref, name_and_type_ref))
}

fn read_constant_interfacemethodref<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
    let class_ref = read_unresolved_cp_ref(bytes, ix)?;
    let name_and_type_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::InterfaceMethodRef(
        class_ref,
        name_and_type_ref,
    ))
}

fn read_constant_nameandtype<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
    let name_ref = read_unresolved_cp_ref(bytes, ix)?;
    let descriptor_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::NameAndType(name_ref, descriptor_ref))
}

fn read_constant_methodhandle<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
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
        n => fail!(
            "Unexpected reference kind {} when reading CONSTANT_methodhandle at index {}",
            n,
            *ix - 1
        ),
    };
    let reference_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::MethodHandle(
        reference_kind,
        reference_ref,
    ))
}

fn read_constant_methodtype<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
    let descriptor_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::MethodType(descriptor_ref))
}

fn read_constant_dynamic<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
    let bootstrap_method_ref = read_u2(bytes, ix)?;
    let name_and_type_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::Dynamic(
        bootstrap_method_ref,
        name_and_type_ref,
    ))
}

fn read_constant_invokedynamic<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
    let bootstrap_method_ref = read_u2(bytes, ix)?;
    let name_and_type_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::InvokeDynamic(
        bootstrap_method_ref,
        name_and_type_ref,
    ))
}

fn read_constant_module<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
    let name_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::ModuleInfo(name_ref))
}

fn read_constant_package<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
) -> Result<ConstantPoolEntry<'a>, ParseError> {
    let name_ref = read_unresolved_cp_ref(bytes, ix)?;
    Ok(ConstantPoolEntry::PackageInfo(name_ref))
}

fn resolve_constant_pool(constant_pool: &[Rc<ConstantPoolEntry>]) -> Result<(), ParseError> {
    for (i, cp_entry) in constant_pool.iter().enumerate() {
        cp_entry.resolve(i, constant_pool)?;
    }
    Ok(())
}

fn validate_constant_pool(
    constant_pool: &[Rc<ConstantPoolEntry>],
    major_version: u16,
) -> Result<(), ParseError> {
    for (i, cp_entry) in constant_pool.iter().enumerate() {
        cp_entry
            .validate(major_version)
            .map_err(|e| err!(e, "constant pool entry {}", i))?;
    }
    Ok(())
}

pub(crate) fn read_constant_pool<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    major_version: u16,
) -> Result<Vec<Rc<ConstantPoolEntry<'a>>>, ParseError> {
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
            n => fail!(
                "Unexpected constant pool entry type {} at index {} for classfile major version {}",
                n,
                *ix - 1,
                major_version
            ),
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

fn read_cp_ref_any<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<Rc<ConstantPoolEntry<'a>>, ParseError> {
    let cp_index = read_u2(bytes, ix)? as usize;
    if cp_index >= pool.len() {
        fail!(
            "Out-of-bounds index {} in constant pool reference",
            cp_index
        );
    }
    Ok(pool[cp_index].clone())
}

pub(crate) fn read_cp_utf8<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<Cow<'a, str>, ParseError> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Utf8(x) => Ok(x.clone()),
        _ => fail!("Unexpected constant pool reference type"),
    }
}

pub(crate) fn read_cp_utf8_opt<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<Option<Cow<'a, str>>, ParseError> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Zero => Ok(None),
        ConstantPoolEntry::Utf8(x) => Ok(Some(x.clone())),
        _ => fail!("Unexpected constant pool reference type"),
    }
}

pub(crate) fn read_cp_classinfo<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<Cow<'a, str>, ParseError> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::ClassInfo(x) => Ok(x.borrow().get().utf8()),
        _ => fail!("Unexpected constant pool reference type"),
    }
}

pub(crate) fn read_cp_classinfo_opt<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<Option<Cow<'a, str>>, ParseError> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Zero => Ok(None),
        ConstantPoolEntry::ClassInfo(x) => Ok(Some(x.borrow().get().utf8())),
        _ => fail!("Unexpected constant pool reference type"),
    }
}

pub(crate) fn read_cp_moduleinfo<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<Cow<'a, str>, ParseError> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::ModuleInfo(x) => Ok(x.borrow().get().utf8()),
        _ => fail!("Unexpected constant pool reference type"),
    }
}

pub(crate) fn read_cp_packageinfo<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<Cow<'a, str>, ParseError> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::PackageInfo(x) => Ok(x.borrow().get().utf8()),
        _ => fail!("Unexpected constant pool reference type"),
    }
}

#[derive(Clone, Debug)]
pub struct NameAndType<'a> {
    pub name: Cow<'a, str>,
    pub descriptor: Cow<'a, str>,
}

pub(crate) fn read_cp_nameandtype_opt<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<Option<NameAndType<'a>>, ParseError> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Zero => Ok(None),
        ConstantPoolEntry::NameAndType(x, y) => Ok(Some(NameAndType {
            name: x.borrow().get().utf8(),
            descriptor: y.borrow().get().utf8(),
        })),
        _ => fail!("Unexpected constant pool reference type"),
    }
}

#[derive(Clone, Debug)]
pub enum LiteralConstant<'a> {
    Integer(i32),
    Float(f32),
    Long(i64),
    Double(f64),
    String(Cow<'a, str>),
    StringBytes(&'a [u8]),
}

pub(crate) fn read_cp_literalconstant<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<LiteralConstant<'a>, ParseError> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Integer(v) => Ok(LiteralConstant::Integer(*v)),
        ConstantPoolEntry::Float(v) => Ok(LiteralConstant::Float(*v)),
        ConstantPoolEntry::Long(v) => Ok(LiteralConstant::Long(*v)),
        ConstantPoolEntry::Double(v) => Ok(LiteralConstant::Double(*v)),
        ConstantPoolEntry::String(v) => Ok(v.borrow().get().string_literal()),
        _ => fail!("Unexpected constant pool reference type"),
    }
}

pub(crate) fn read_cp_integer<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<i32, ParseError> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Integer(v) => Ok(*v),
        _ => fail!("Unexpected constant pool reference type"),
    }
}

pub(crate) fn read_cp_float<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<f32, ParseError> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Float(v) => Ok(*v),
        _ => fail!("Unexpected constant pool reference type"),
    }
}

pub(crate) fn read_cp_long<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<i64, ParseError> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Long(v) => Ok(*v),
        _ => fail!("Unexpected constant pool reference type"),
    }
}

pub(crate) fn read_cp_double<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<f64, ParseError> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Double(v) => Ok(*v),
        _ => fail!("Unexpected constant pool reference type"),
    }
}

#[derive(Clone, Debug)]
pub struct MemberRef<'a> {
    pub class_name: Cow<'a, str>,
    pub name_and_type: NameAndType<'a>,
}

pub(crate) fn read_cp_memberref<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
    allowed: ConstantPoolEntryTypes,
) -> Result<MemberRef<'a>, ParseError> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    // The caller can restrict the specific member types allowed here such
    // that we return an Err if it's not one of the allowed types.
    cp_ref.ensure_type(allowed)?;
    match cp_ref.deref() {
        ConstantPoolEntry::FieldRef(c, m)
        | ConstantPoolEntry::MethodRef(c, m)
        | ConstantPoolEntry::InterfaceMethodRef(c, m) => Ok(MemberRef {
            class_name: c.borrow().get().classinfo(),
            name_and_type: m.borrow().get().name_and_type(),
        }),
        _ => fail!("Unexpected constant pool reference type"),
    }
}

#[derive(Clone, Debug)]
pub struct InvokeDynamic<'a> {
    pub attr_index: u16,
    pub name_and_type: NameAndType<'a>,
}

pub(crate) fn read_cp_invokedynamic<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<InvokeDynamic<'a>, ParseError> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::InvokeDynamic(x, y) => Ok(InvokeDynamic {
            attr_index: *x,
            name_and_type: y.borrow().get().name_and_type(),
        }),
        _ => fail!("Unexpected constant pool reference type"),
    }
}

#[derive(Clone, Debug)]
pub struct Dynamic<'a> {
    pub attr_index: u16,
    pub name_and_type: NameAndType<'a>,
}

#[derive(Clone, Debug)]
pub enum Loadable<'a> {
    LiteralConstant(LiteralConstant<'a>),
    ClassInfo(Cow<'a, str>),
    MethodHandle(MethodHandle<'a>),
    MethodType(Cow<'a, str>),
    Dynamic(Dynamic<'a>),
}

pub(crate) fn get_cp_loadable<'a>(
    cp_index: usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<Loadable<'a>, ParseError> {
    if cp_index >= pool.len() {
        fail!(
            "Out-of-bounds index {} in constant pool reference",
            cp_index
        );
    }
    match pool[cp_index].deref() {
        ConstantPoolEntry::Integer(v) => {
            Ok(Loadable::LiteralConstant(LiteralConstant::Integer(*v)))
        }
        ConstantPoolEntry::Float(v) => Ok(Loadable::LiteralConstant(LiteralConstant::Float(*v))),
        ConstantPoolEntry::Long(v) => Ok(Loadable::LiteralConstant(LiteralConstant::Long(*v))),
        ConstantPoolEntry::Double(v) => Ok(Loadable::LiteralConstant(LiteralConstant::Double(*v))),
        ConstantPoolEntry::String(v) => {
            Ok(Loadable::LiteralConstant(v.borrow().get().string_literal()))
        }
        ConstantPoolEntry::ClassInfo(x) => Ok(Loadable::ClassInfo(x.borrow().get().utf8())),
        ConstantPoolEntry::MethodHandle(x, y) => {
            Ok(Loadable::MethodHandle(make_method_handle(x, y)?))
        }
        ConstantPoolEntry::MethodType(x) => Ok(Loadable::MethodType(x.borrow().get().utf8())),
        ConstantPoolEntry::Dynamic(x, y) => Ok(Loadable::Dynamic(Dynamic {
            attr_index: *x,
            name_and_type: y.borrow().get().name_and_type(),
        })),
        _ => fail!("Unexpected non-loadable constant pool reference found"),
    }
}

#[derive(Clone, Copy, Debug)]
pub enum MemberKind {
    Field,
    Method,
    InterfaceMethod,
}

#[derive(Clone, Debug)]
pub struct MethodHandle<'a> {
    pub kind: ReferenceKind,
    pub class_name: Cow<'a, str>,
    pub member_kind: MemberKind,
    pub member_ref: NameAndType<'a>,
}

fn make_method_handle<'a>(
    x: &ReferenceKind,
    y: &RefCell<ConstantPoolRef<'a>>,
) -> Result<MethodHandle<'a>, ParseError> {
    let (class_name, member_kind, member_ref) = match y.borrow().get().deref() {
        ConstantPoolEntry::FieldRef(c, m) => (
            c.borrow().get().classinfo(),
            MemberKind::Field,
            m.borrow().get().name_and_type(),
        ),
        ConstantPoolEntry::MethodRef(c, m) => (
            c.borrow().get().classinfo(),
            MemberKind::Method,
            m.borrow().get().name_and_type(),
        ),
        ConstantPoolEntry::InterfaceMethodRef(c, m) => (
            c.borrow().get().classinfo(),
            MemberKind::InterfaceMethod,
            m.borrow().get().name_and_type(),
        ),
        _ => fail!("Unexpected constant pool reference type"),
    };
    Ok(MethodHandle {
        kind: *x,
        class_name,
        member_kind,
        member_ref,
    })
}

pub(crate) fn read_cp_methodhandle<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<MethodHandle<'a>, ParseError> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::MethodHandle(x, y) => make_method_handle(x, y),
        _ => fail!("Unexpected constant pool reference type"),
    }
}

#[derive(Debug)]
pub enum BootstrapArgument<'a> {
    LiteralConstant(LiteralConstant<'a>),
    ClassInfo(Cow<'a, str>),
    MethodHandle(MethodHandle<'a>),
    MethodType(Cow<'a, str>),
}

pub(crate) fn read_cp_bootstrap_argument<'a>(
    bytes: &'a [u8],
    ix: &mut usize,
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<BootstrapArgument<'a>, ParseError> {
    let cp_ref = read_cp_ref_any(bytes, ix, pool)?;
    match cp_ref.deref() {
        ConstantPoolEntry::Integer(v) => Ok(BootstrapArgument::LiteralConstant(
            LiteralConstant::Integer(*v),
        )),
        ConstantPoolEntry::Float(v) => Ok(BootstrapArgument::LiteralConstant(
            LiteralConstant::Float(*v),
        )),
        ConstantPoolEntry::Long(v) => Ok(BootstrapArgument::LiteralConstant(
            LiteralConstant::Long(*v),
        )),
        ConstantPoolEntry::Double(v) => Ok(BootstrapArgument::LiteralConstant(
            LiteralConstant::Double(*v),
        )),
        ConstantPoolEntry::String(v) => Ok(BootstrapArgument::LiteralConstant(
            v.borrow().get().string_literal(),
        )),
        ConstantPoolEntry::ClassInfo(x) => {
            Ok(BootstrapArgument::ClassInfo(x.borrow().get().utf8()))
        }
        ConstantPoolEntry::MethodHandle(x, y) => {
            Ok(BootstrapArgument::MethodHandle(make_method_handle(x, y)?))
        }
        ConstantPoolEntry::MethodType(x) => {
            Ok(BootstrapArgument::MethodType(x.borrow().get().utf8()))
        }
        _ => fail!("Unexpected constant pool reference type"),
    }
}

#[derive(Debug)]
pub enum ConstantPoolItem<'a> {
    LiteralConstant(LiteralConstant<'a>),
    ClassInfo(Cow<'a, str>),
    FieldRef(MemberRef<'a>),
    MethodRef(MemberRef<'a>),
    InterfaceMethodRef(MemberRef<'a>),
    NameAndType(NameAndType<'a>),
    MethodHandle(MethodHandle<'a>),
    MethodType(Cow<'a, str>),
    Dynamic(Dynamic<'a>),
    InvokeDynamic(InvokeDynamic<'a>),
    ModuleInfo(Cow<'a, str>),
    PackageInfo(Cow<'a, str>),
}

pub struct ConstantPoolIter<'a> {
    constant_pool: &'a [Rc<ConstantPoolEntry<'a>>],
    index: usize,
}

impl<'a> ConstantPoolIter<'a> {
    pub(crate) fn new(constant_pool: &'a [Rc<ConstantPoolEntry<'a>>]) -> Self {
        ConstantPoolIter {
            constant_pool,
            index: 0,
        }
    }
}

impl<'a> Iterator for ConstantPoolIter<'a> {
    type Item = ConstantPoolItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.index + 1 < self.constant_pool.len() {
            self.index += 1;
            let item = match self.constant_pool[self.index].deref() {
                ConstantPoolEntry::Zero => panic!("This iterator should never see a Zero item"),
                ConstantPoolEntry::Utf8(_) | ConstantPoolEntry::Utf8Bytes(_) => continue,
                ConstantPoolEntry::Integer(v) => {
                    ConstantPoolItem::LiteralConstant(LiteralConstant::Integer(*v))
                }
                ConstantPoolEntry::Float(v) => {
                    ConstantPoolItem::LiteralConstant(LiteralConstant::Float(*v))
                }
                ConstantPoolEntry::Long(v) => {
                    ConstantPoolItem::LiteralConstant(LiteralConstant::Long(*v))
                }
                ConstantPoolEntry::Double(v) => {
                    ConstantPoolItem::LiteralConstant(LiteralConstant::Double(*v))
                }
                ConstantPoolEntry::ClassInfo(x) => {
                    ConstantPoolItem::ClassInfo(x.borrow().get().utf8())
                }
                ConstantPoolEntry::String(x) => {
                    ConstantPoolItem::LiteralConstant(x.borrow().get().string_literal())
                }
                ConstantPoolEntry::FieldRef(c, m) => ConstantPoolItem::FieldRef(MemberRef {
                    class_name: c.borrow().get().classinfo(),
                    name_and_type: m.borrow().get().name_and_type(),
                }),
                ConstantPoolEntry::MethodRef(c, m) => ConstantPoolItem::MethodRef(MemberRef {
                    class_name: c.borrow().get().classinfo(),
                    name_and_type: m.borrow().get().name_and_type(),
                }),
                ConstantPoolEntry::InterfaceMethodRef(c, m) => {
                    ConstantPoolItem::InterfaceMethodRef(MemberRef {
                        class_name: c.borrow().get().classinfo(),
                        name_and_type: m.borrow().get().name_and_type(),
                    })
                }
                ConstantPoolEntry::NameAndType(x, y) => {
                    ConstantPoolItem::NameAndType(NameAndType {
                        name: x.borrow().get().utf8(),
                        descriptor: y.borrow().get().utf8(),
                    })
                }
                ConstantPoolEntry::MethodHandle(x, y) => {
                    ConstantPoolItem::MethodHandle(make_method_handle(x, y).unwrap())
                }
                ConstantPoolEntry::MethodType(x) => {
                    ConstantPoolItem::MethodType(x.borrow().get().utf8())
                }
                ConstantPoolEntry::Dynamic(x, y) => ConstantPoolItem::Dynamic(Dynamic {
                    attr_index: *x,
                    name_and_type: y.borrow().get().name_and_type(),
                }),
                ConstantPoolEntry::InvokeDynamic(x, y) => {
                    ConstantPoolItem::InvokeDynamic(InvokeDynamic {
                        attr_index: *x,
                        name_and_type: y.borrow().get().name_and_type(),
                    })
                }
                ConstantPoolEntry::ModuleInfo(x) => {
                    ConstantPoolItem::ModuleInfo(x.borrow().get().utf8())
                }
                ConstantPoolEntry::PackageInfo(x) => {
                    ConstantPoolItem::PackageInfo(x.borrow().get().utf8())
                }
                ConstantPoolEntry::Unused => continue,
            };
            return Some(item);
        }
        None
    }
}

#[cfg(test)]
mod tests;
