use std::borrow::Cow;
use std::ops::Deref;
use std::rc::Rc;

use crate::{ConstantPoolEntry, ConstantPoolEntryTypes};
use crate::{read_u2, read_u4, read_cp_ref};

#[derive(Debug)]
pub struct ExceptionTableEntry<'a> {
    pub start_pc: u16,
    pub end_pc: u16,
    pub handler_pc: u16,
    catch_type: Rc<ConstantPoolEntry<'a>>,
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
enum AttributeData<'a> {
    ConstantValue(Rc<ConstantPoolEntry<'a>>),
    Code(CodeData<'a>),
    Other(&'a [u8]),
}

#[derive(Debug)]
pub struct AttributeInfo<'a> {
    name: Rc<ConstantPoolEntry<'a>>,
    data: AttributeData<'a>,
}

impl<'a> AttributeInfo<'a> {
    pub fn name(&self) -> Cow<'a, str> {
        self.name.utf8()
    }
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
    for j in 0..exception_table_count {
        let start_pc = read_u2(bytes, ix)?;
        let end_pc = read_u2(bytes, ix)?;
        let handler_pc = read_u2(bytes, ix)?;
        let catch_type = read_cp_ref(bytes, ix, pool, ConstantPoolEntryTypes::CLASS_OR_ZERO).map_err(|e| format!("{} catch type of exception table entry {}", e, j))?;
        exception_table.push(ExceptionTableEntry {
            start_pc,
            end_pc,
            handler_pc,
            catch_type,
        });
    }
    let code_attributes_count = read_u2(bytes, ix)?;
    let code_attributes = read_attributes(bytes, ix, code_attributes_count, pool).map_err(|e| format!("{} of code attribute", e))?;
    Ok(CodeData {
        max_stack,
        max_locals,
        code,
        exception_table,
        attributes: code_attributes,
    })
}

pub(crate) fn read_attributes<'a>(bytes: &'a [u8], ix: &mut usize, attributes_count: u16, pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<AttributeInfo<'a>>, String> {
    let mut attributes = Vec::new();
    for i in 0..attributes_count {
        let name = read_cp_ref(bytes, ix, pool, ConstantPoolEntryTypes::UTF8).map_err(|e| format!("{} name field of attribute {}", e, i))?;
        let length = read_u4(bytes, ix)? as usize;
        let expected_end_ix = *ix + length;
        if bytes.len() < expected_end_ix {
            return Err(format!("Unexpected end of stream reading attributes at index {}", *ix));
        }
        let data = match name.utf8().deref() {
            "ConstantValue" => {
                if length != 2 {
                    return Err(format!("Unexpected length {} for ConstantValue attribute {}", length, i));
                }
                AttributeData::ConstantValue(read_cp_ref(bytes, ix, pool, ConstantPoolEntryTypes::CONSTANTS).map_err(|e| format!("{} value field of attribute {}", e, i))?)
            }
            "Code" => {
                let code_data = read_code_data(bytes, ix, pool).map_err(|e| format!("{} of code attribute {}", e, i))?;
                AttributeData::Code(code_data)
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
