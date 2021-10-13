use std::borrow::Cow;
use std::rc::Rc;

use crate::{read_u1, ParseError};
use crate::constant_pool::{read_cp_classinfo, ConstantPoolEntry};

#[derive(Debug)]
pub enum Opcode<'a> {
    Instanceof(Cow<'a, str>),
}

#[derive(Debug)]
pub struct ByteCode<'a> {
    pub opcodes: Vec<Opcode<'a>>,
}

pub(crate) fn read_opcodes<'a>(code: &'a [u8], pool: &[Rc<ConstantPoolEntry<'a>>]) -> Result<Vec<Opcode<'a>>, ParseError> {
    let mut opcodes = Vec::new();
    let mut ix = 0;
    while ix < code.len() {
        let opcode = match read_u1(code, &mut ix)? {
            0xc1 => Opcode::Instanceof(read_cp_classinfo(code, &mut ix, pool)?),
            _ => continue,
        };
        opcodes.push(opcode);
    }
    Ok(opcodes)
}
