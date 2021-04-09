use std::borrow::Cow;
use std::ops::Deref;
use std::rc::Rc;

use crate::{read_u1, read_u2, read_u4, AccessFlags};
use crate::constant_pool::{ConstantPoolEntry, NameAndType, LiteralConstant, MethodHandle, BootstrapArgument};
use crate::constant_pool::{read_cp_utf8, read_cp_utf8_opt, read_cp_classinfo, read_cp_classinfo_opt, read_cp_nameandtype_opt, read_cp_literalconstant, read_cp_methodhandle, read_cp_bootstrap_argument};

#[derive(Debug)]
pub struct ExceptionTableEntry<'a> {
    pub start_pc: u16,
    pub end_pc: u16,
    pub handler_pc: u16,
    pub catch_type: Option<Cow<'a, str>>,
}

#[derive(Debug)]
pub struct CodeData<'a> {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code: &'a [u8],
    pub exception_table: Vec<ExceptionTableEntry<'a>>,
    pub attributes: Vec<AttributeInfo<'a>>,
}

#[derive(Debug)]
pub enum VerificationType<'a> {
    Top,
    Integer,
    Float,
    Long,
    Double,
    Null,
    UninitializedThis,
    Uninitialized(u16),
    Object(Cow<'a, str>),
}

#[derive(Debug)]
pub enum StackMapEntry<'a> {
    Same(u16),
    SameLocals1StackItem(u16, VerificationType<'a>),
    Chop(u16, u16),
    Append(u16, Vec<VerificationType<'a>>),
    FullFrame(u16, Vec<VerificationType<'a>>, Vec<VerificationType<'a>>),
}

bitflags! {
    pub struct InnerClassAccessFlags: u16 {
        const PUBLIC = AccessFlags::PUBLIC.bits();
        const PRIVATE = AccessFlags::PRIVATE.bits();
        const PROTECTED = AccessFlags::PROTECTED.bits();
        const STATIC = AccessFlags::STATIC.bits();
        const FINAL = AccessFlags::FINAL.bits();
        const INTERFACE = AccessFlags::INTERFACE.bits();
        const ABSTRACT = AccessFlags::ABSTRACT.bits();
        const SYNTHETIC = AccessFlags::SYNTHETIC.bits();
        const ANNOTATION = AccessFlags::ANNOTATION.bits();
        const ENUM = AccessFlags::ENUM.bits();
    }
}

#[derive(Debug)]
pub struct InnerClassEntry<'a> {
    pub inner_class_info: Cow<'a, str>,
    pub outer_class_info: Option<Cow<'a, str>>,
    pub inner_name: Option<Cow<'a, str>>,
    pub access_flags: InnerClassAccessFlags,
}

#[derive(Debug)]
pub struct LineNumberEntry {
    pub start_pc: u16,
    pub line_number: u16,
}

#[derive(Debug)]
pub struct LocalVariableEntry<'a> {
    pub start_pc: u16,
    pub length: u16,
    pub name: Cow<'a, str>,
    pub descriptor: Cow<'a, str>,
    pub index: u16,
}

#[derive(Debug)]
pub struct LocalVariableTypeEntry<'a> {
    pub start_pc: u16,
    pub length: u16,
    pub name: Cow<'a, str>,
    pub signature: Cow<'a, str>,
    pub index: u16,
}

#[derive(Debug)]
pub struct BootstrapMethodEntry<'a> {
    pub method: MethodHandle<'a>,
    pub arguments: Vec<BootstrapArgument<'a>>,
}

bitflags! {
    pub struct MethodParameterAccessFlags: u16 {
        const FINAL = AccessFlags::FINAL.bits();
        const SYNTHETIC = AccessFlags::SYNTHETIC.bits();
        const MANDATED = AccessFlags::MANDATED.bits();
    }
}

#[derive(Debug)]
pub struct MethodParameterEntry<'a> {
    pub name: Option<Cow<'a, str>>,
    pub access_flags: MethodParameterAccessFlags,
}

#[derive(Debug)]
pub enum AttributeData<'a> {
    ConstantValue(LiteralConstant<'a>),
    Code(CodeData<'a>),
    StackMapTable(Vec<StackMapEntry<'a>>),
    Exceptions(Vec<Cow<'a, str>>),
    InnerClasses(Vec<InnerClassEntry<'a>>),
    EnclosingMethod(Cow<'a, str>, Option<NameAndType<'a>>),
    Synthetic,
    Signature(Cow<'a, str>),
    SourceFile(Cow<'a, str>),
    SourceDebugExtension(Cow<'a, str>),
    LineNumberTable(Vec<LineNumberEntry>),
    LocalVariableTable(Vec<LocalVariableEntry<'a>>),
    LocalVariableTypeTable(Vec<LocalVariableTypeEntry<'a>>),
    Deprecated,
    // TODO: all the annotation ones
    BootstrapMethods(Vec<BootstrapMethodEntry<'a>>),
    MethodParameters(Vec<MethodParameterEntry<'a>>),
    Other(&'a [u8]),
}

#[derive(Debug)]
pub struct AttributeInfo<'a> {
    pub name: Cow<'a, str>,
    pub data: AttributeData<'a>,
}

fn ensure_length(length: usize, expected: usize) -> Result<(), String> {
    if length != expected {
        return Err(format!("Unexpected length {} for", length));
    }
    Ok(())
}

fn read_code_data<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<CodeData<'a>, String> {
    let max_stack = read_u2(bytes, ix)?;
    let max_locals = read_u2(bytes, ix)?;
    let code_length = read_u4(bytes, ix)? as usize;
    if bytes.len() < *ix + code_length {
        return Err(format!("Unexpected end of stream reading code attribute at index {}", *ix));
    }
    let code = &bytes[*ix .. *ix + code_length];
    *ix += code_length;
    let exception_table_count = read_u2(bytes, ix)?;
    let mut exception_table = Vec::new();
    for i in 0..exception_table_count {
        let start_pc = read_u2(bytes, ix)?;
        let end_pc = read_u2(bytes, ix)?;
        let handler_pc = read_u2(bytes, ix)?;
        let catch_type = read_cp_classinfo_opt(bytes, ix, pool).map_err(|e| format!("{} catch type of exception table entry {}", e, i))?;
        exception_table.push(ExceptionTableEntry {
            start_pc,
            end_pc,
            handler_pc,
            catch_type,
        });
    }
    let code_attributes = read_attributes(bytes, ix, pool).map_err(|e| format!("{} of code attribute", e))?;
    Ok(CodeData {
        max_stack,
        max_locals,
        code,
        exception_table,
        attributes: code_attributes,
    })
}

fn read_stackmaptable_verification<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<VerificationType<'a>, String> {
    let verification_type = match read_u1(bytes, ix)? {
        0 => VerificationType::Top,
        1 => VerificationType::Integer,
        2 => VerificationType::Float,
        3 => VerificationType::Double,
        4 => VerificationType::Long,
        5 => VerificationType::Null,
        6 => VerificationType::UninitializedThis,
        7 => {
            let name = read_cp_classinfo(bytes, ix, pool).map_err(|e| format!("{} object verification type", e))?;
            VerificationType::Object(name)
        }
        8 => {
            let offset = read_u2(bytes, ix)?;
            VerificationType::Uninitialized(offset)
        }
        v => return Err(format!("Unrecognized verification type {}", v)),
    };
    Ok(verification_type)
}

fn read_stackmaptable_data<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<StackMapEntry<'a>>, String> {
    let mut stackmapframes = Vec::new();
    let count = read_u2(bytes, ix)?;
    for i in 0..count {
        let entry = match read_u1(bytes, ix)? {
            v @ 0..=63 => StackMapEntry::Same(v.into()),
            v @ 64..=127 => {
                let verification = read_stackmaptable_verification(bytes, ix, pool).map_err(|e| format!("{} for same_locals_1_stack_item_frame stack map entry {}", e, i))?;
                StackMapEntry::SameLocals1StackItem((v - 64).into(), verification)
            }
            v @ 128..=246 => return Err(format!("Unrecognized discriminant {} for stack map entry {}", v, i)),
            247 => {
                let offset_delta = read_u2(bytes, ix)?;
                let verification = read_stackmaptable_verification(bytes, ix, pool).map_err(|e| format!("{} for same_locals_1_stack_item_frame_extended stack map entry {}", e, i))?;
                StackMapEntry::SameLocals1StackItem(offset_delta, verification)
            }
            v @ 248..=250 => {
                let offset_delta = read_u2(bytes, ix)?;
                StackMapEntry::Chop(offset_delta, (251 - v).into())
            }
            251 => {
                let offset_delta = read_u2(bytes, ix)?;
                StackMapEntry::Same(offset_delta)
            }
            v @ 252..=254 => {
                let offset_delta = read_u2(bytes, ix)?;
                let mut verifications = Vec::new();
                let verification_count = v - 251;
                for j in 0..verification_count {
                    verifications.push(read_stackmaptable_verification(bytes, ix, pool).map_err(|e| format!("{} for local entry {} of append stack map entry {}", e, j, i))?);
                }
                StackMapEntry::Append(offset_delta, verifications)
            }
            255 => {
                let offset_delta = read_u2(bytes, ix)?;
                let mut locals = Vec::new();
                let locals_count = read_u2(bytes, ix)?;
                for j in 0..locals_count {
                    locals.push(read_stackmaptable_verification(bytes, ix, pool).map_err(|e| format!("{} for local entry {} of full-frame stack map entry {}", e, j, i))?);
                }
                let mut stack = Vec::new();
                let stack_count = read_u2(bytes, ix)?;
                for j in 0..stack_count {
                    stack.push(read_stackmaptable_verification(bytes, ix, pool).map_err(|e| format!("{} for stack entry {} of full-frame stack map entry {}", e, j, i))?);
                }
                StackMapEntry::FullFrame(offset_delta, locals, stack)
            }
        };
        stackmapframes.push(entry);
    }
    Ok(stackmapframes)
}

fn read_exceptions_data<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<Cow<'a, str>>, String> {
    let mut exceptions = Vec::new();
    let exceptions_count = read_u2(bytes, ix)?;
    for i in 0..exceptions_count {
        let exception = read_cp_classinfo(bytes, ix, pool).map_err(|e| format!("{} exception {}", e, i))?;
        exceptions.push(exception);
    }
    Ok(exceptions)
}

fn read_innerclasses_data<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<InnerClassEntry<'a>>, String> {
    let mut innerclasses = Vec::new();
    let count = read_u2(bytes, ix)?;
    for i in 0..count {
        let inner_class_info = read_cp_classinfo(bytes, ix, pool).map_err(|e| format!("{} inner class info for inner class {}", e, i))?;
        let outer_class_info = read_cp_classinfo_opt(bytes, ix, pool).map_err(|e| format!("{} outer class info for inner class {}", e, i))?;
        let inner_name = read_cp_utf8_opt(bytes, ix, pool).map_err(|e| format!("{} inner name for inner class {}", e, i))?;
        let access_flags = InnerClassAccessFlags::from_bits(read_u2(bytes, ix)?).ok_or_else(|| format!("Invalid access flags found on inner class {}", i))?;
        innerclasses.push(InnerClassEntry {
            inner_class_info,
            outer_class_info,
            inner_name,
            access_flags,
        });
    }
    Ok(innerclasses)
}

fn read_linenumber_data<'a>(bytes: &'a [u8], ix: &mut usize) -> Result<Vec<LineNumberEntry>, String> {
    let mut linenumbers = Vec::new();
    let count = read_u2(bytes, ix)?;
    for _i in 0..count {
        let start_pc = read_u2(bytes, ix)?;
        let line_number = read_u2(bytes, ix)?;
        linenumbers.push(LineNumberEntry {
            start_pc,
            line_number,
        });
    }
    Ok(linenumbers)
}

fn read_localvariable_data<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<LocalVariableEntry<'a>>, String> {
    let mut localvariables = Vec::new();
    let count = read_u2(bytes, ix)?;
    for i in 0..count {
        let start_pc = read_u2(bytes, ix)?;
        let length = read_u2(bytes, ix)?;
        let name = read_cp_utf8(bytes, ix, pool).map_err(|e| format!("{} name for variable {}", e, i))?;
        let descriptor = read_cp_utf8(bytes, ix, pool).map_err(|e| format!("{} descriptor for variable {}", e, i))?;
        let index = read_u2(bytes, ix)?;
        localvariables.push(LocalVariableEntry {
            start_pc,
            length,
            name,
            descriptor,
            index,
        });
    }
    Ok(localvariables)
}

fn read_localvariabletype_data<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<LocalVariableTypeEntry<'a>>, String> {
    let mut localvariabletypes = Vec::new();
    let count = read_u2(bytes, ix)?;
    for i in 0..count {
        let start_pc = read_u2(bytes, ix)?;
        let length = read_u2(bytes, ix)?;
        let name = read_cp_utf8(bytes, ix, pool).map_err(|e| format!("{} name for variable {}", e, i))?;
        let signature = read_cp_utf8(bytes, ix, pool).map_err(|e| format!("{} signature for variable {}", e, i))?;
        let index = read_u2(bytes, ix)?;
        localvariabletypes.push(LocalVariableTypeEntry {
            start_pc,
            length,
            name,
            signature,
            index,
        });
    }
    Ok(localvariabletypes)
}

fn read_bootstrapmethods_data<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<BootstrapMethodEntry<'a>>, String> {
    let mut bootstrapmethods = Vec::new();
    let count = read_u2(bytes, ix)?;
    for i in 0..count {
        let method = read_cp_methodhandle(bytes, ix, pool).map_err(|e| format!("{} method ref for bootstrap method {}", e, i))?;
        let mut arguments = Vec::new();
        let arg_count = read_u2(bytes, ix)?;
        for j in 0..arg_count {
            let argument = read_cp_bootstrap_argument(bytes, ix, pool).map_err(|e| format!("{} argument {} of bootstrap method {}", e, j, i))?;
            arguments.push(argument);
        }
        bootstrapmethods.push(BootstrapMethodEntry {
            method,
            arguments,
        });
    }
    Ok(bootstrapmethods)
}

fn read_methodparameters_data<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<MethodParameterEntry<'a>>, String> {
    let mut methodparameters = Vec::new();
    let count = read_u1(bytes, ix)?;
    for i in 0..count {
        let name = read_cp_utf8_opt(bytes, ix, pool).map_err(|e| format!("{} name of method parameter {}", e, i))?;
        let access_flags = MethodParameterAccessFlags::from_bits(read_u2(bytes, ix)?).ok_or_else(|| format!("Invalid access flags found on method parameter {}", i))?;
        methodparameters.push(MethodParameterEntry {
            name,
            access_flags,
        });
    }
    Ok(methodparameters)
}

pub(crate) fn read_attributes<'a>(bytes: &'a [u8], ix: &mut usize, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<AttributeInfo<'a>>, String> {
    let mut attributes = Vec::new();
    let count = read_u2(bytes, ix)?;
    for i in 0..count {
        let name = read_cp_utf8(bytes, ix, pool).map_err(|e| format!("{} name field of attribute {}", e, i))?;
        let length = read_u4(bytes, ix)? as usize;
        let expected_end_ix = *ix + length;
        if bytes.len() < expected_end_ix {
            return Err(format!("Unexpected end of stream reading attributes at index {}", *ix));
        }
        let data = match name.deref() {
            "ConstantValue" => {
                ensure_length(length, 2).map_err(|e| format!("{} ConstantValue attribute {}", e, i))?;
                AttributeData::ConstantValue(read_cp_literalconstant(bytes, ix, pool).map_err(|e| format!("{} value field of ConstantValue attribute {}", e, i))?)
            }
            "Code" => {
                let code_data = read_code_data(bytes, ix, pool).map_err(|e| format!("{} of Code attribute {}", e, i))?;
                AttributeData::Code(code_data)
            }
            "StackMapTable" => {
                let stackmaptable_data = read_stackmaptable_data(bytes, ix, pool).map_err(|e| format!("{} of StackMapTable attribute {}", e, i))?;
                AttributeData::StackMapTable(stackmaptable_data)
            }
            "Exceptions" => {
                let exceptions_data = read_exceptions_data(bytes, ix, pool).map_err(|e| format!("{} of Exceptions attribute {}", e, i))?;
                AttributeData::Exceptions(exceptions_data)
            }
            "InnerClasses" => {
                let innerclasses_data = read_innerclasses_data(bytes, ix, pool).map_err(|e| format!("{} of InnerClasses attribute {}", e, i))?;
                AttributeData::InnerClasses(innerclasses_data)
            }
            "EnclosingMethod" => {
                ensure_length(length, 4).map_err(|e| format!("{} EnclosingMethod attribute {}", e, i))?;
                let class = read_cp_classinfo(bytes, ix, pool).map_err(|e| format!("{} class info of EnclosingMethod attribute {}", e, i))?;
                let method = read_cp_nameandtype_opt(bytes, ix, pool).map_err(|e| format!("{} method info of EnclosingMethod attribute {}", e, i))?;
                AttributeData::EnclosingMethod(class, method)
            }
            "Synthetic" => {
                ensure_length(length, 0).map_err(|e| format!("{} Synthetic attribute {}", e, i))?;
                AttributeData::Synthetic
            }
            "Signature" => {
                ensure_length(length, 2).map_err(|e| format!("{} Signature attribute {}", e, i))?;
                AttributeData::Signature(read_cp_utf8(bytes, ix, pool).map_err(|e| format!("{} signature field of Signature attribute {}", e, i))?)
            }
            "SourceFile" => {
                ensure_length(length, 2).map_err(|e| format!("{} SourceFile attribute {}", e, i))?;
                AttributeData::SourceFile(read_cp_utf8(bytes, ix, pool).map_err(|e| format!("{} signature field of SourceFile attribute {}", e, i))?)
            }
            "SourceDebugExtension" => {
                let modified_utf8_data = &bytes[*ix .. *ix + length];
                *ix += length;
                let debug_str = cesu8::from_java_cesu8(modified_utf8_data).map_err(|e| format!("{} modified utf8 data of SourceDebugExtension attribute {}", e, i))?;
                AttributeData::SourceDebugExtension(debug_str)
            }
            "LineNumberTable" => {
                let linenumber_data = read_linenumber_data(bytes, ix).map_err(|e| format!("{} of LineNumberTable attribute {}", e, i))?;
                AttributeData::LineNumberTable(linenumber_data)
            }
            "LocalVariableTable" => {
                let localvariable_data = read_localvariable_data(bytes, ix, pool).map_err(|e| format!("{} of LocalVariableTable attribute {}", e, i))?;
                AttributeData::LocalVariableTable(localvariable_data)
            }
            "LocalVariableTypeTable" => {
                let localvariabletype_data = read_localvariabletype_data(bytes, ix, pool).map_err(|e| format!("{} of LocalVariableTypeTable attribute {}", e, i))?;
                AttributeData::LocalVariableTypeTable(localvariabletype_data)
            }
            "Deprecated" => {
                ensure_length(length, 0).map_err(|e| format!("{} Deprecated attribute {}", e, i))?;
                AttributeData::Deprecated
            }
            "BootstrapMethods" => {
                let bootstrapmethods_data = read_bootstrapmethods_data(bytes, ix, pool).map_err(|e| format!("{} of BootstrapMethods attribute {}", e, i))?;
                AttributeData::BootstrapMethods(bootstrapmethods_data)
            }
            "MethodParameters" => {
                let methodparameters_data = read_methodparameters_data(bytes, ix, pool).map_err(|e| format!("{} of MethodParameters attribute {}", e, i))?;
                AttributeData::MethodParameters(methodparameters_data)
            }
            _ => {
                *ix += length;
                AttributeData::Other(&bytes[*ix - length .. *ix])
            }
        };
        if expected_end_ix != *ix {
            return Err(format!("Length mismatch when reading attribute {}", i));
        }
        attributes.push(AttributeInfo {
            name,
            data,
        });
    }
    Ok(attributes)
}
