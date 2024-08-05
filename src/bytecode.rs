use std::borrow::Cow;
use std::convert::TryFrom;
use std::rc::Rc;

use crate::constant_pool::{
    get_cp_loadable, read_cp_classinfo, read_cp_invokedynamic, read_cp_memberref,
};
use crate::constant_pool::{
    ConstantPoolEntry, ConstantPoolEntryTypes, InvokeDynamic, Loadable, MemberRef,
};
use crate::descriptor::{FieldType, Ty};
use crate::{read_u1, read_u2, read_u4, ParseError};

pub type JumpOffset = i32;

#[derive(Clone, Debug)]
pub struct LookupTable {
    pub default: JumpOffset,
    pub match_offsets: Vec<(i32, JumpOffset)>,
}

#[derive(Clone, Debug)]
pub struct RangeTable {
    pub default: JumpOffset,
    pub low: i32,
    pub high: i32,
    pub jumps: Vec<JumpOffset>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrimitiveArrayType {
    Boolean,
    Char,
    Float,
    Double,
    Byte,
    Short,
    Int,
    Long,
}

#[derive(Clone, Debug)]
pub enum Opcode<'a> {
    Aaload,
    Aastore,
    AconstNull,
    Aload(u16), // both wide and narrow
    Anewarray(FieldType<'a>),
    Areturn,
    Arraylength,
    Astore(u16), // both wide and narrow
    Athrow,
    Baload,
    Bastore,
    Bipush(i8),
    Breakpoint,
    Caload,
    Castore,
    Checkcast(FieldType<'a>),
    D2f,
    D2i,
    D2l,
    Dadd,
    Daload,
    Dastore,
    Dcmpg,
    Dcmpl,
    Dconst0,
    Dconst1,
    Ddiv,
    Dload(u16), // both wide and narrow
    Dmul,
    Dneg,
    Drem,
    Dreturn,
    Dstore(u16), // both wide and narrow
    Dsub,
    Dup,
    DupX1,
    DupX2,
    Dup2,
    Dup2X1,
    Dup2X2,
    F2d,
    F2i,
    F2l,
    Fadd,
    Faload,
    Fastore,
    Fcmpg,
    Fcmpl,
    Fconst0,
    Fconst1,
    Fconst2,
    Fdiv,
    Fload(u16), // both wide and narrow
    Fmul,
    Fneg,
    Frem,
    Freturn,
    Fstore(u16), // both wide and narrow
    Fsub,
    Getfield(MemberRef<'a>),
    Getstatic(MemberRef<'a>),
    Goto(JumpOffset), // both wide and narrow
    I2b,
    I2c,
    I2d,
    I2f,
    I2l,
    I2s,
    Iadd,
    Iaload,
    Iand,
    Iastore,
    IconstM1,
    Iconst0,
    Iconst1,
    Iconst2,
    Iconst3,
    Iconst4,
    Iconst5,
    Idiv,
    IfAcmpeq(JumpOffset),
    IfAcmpne(JumpOffset),
    IfIcmpeq(JumpOffset),
    IfIcmpge(JumpOffset),
    IfIcmpgt(JumpOffset),
    IfIcmple(JumpOffset),
    IfIcmplt(JumpOffset),
    IfIcmpne(JumpOffset),
    Ifeq(JumpOffset),
    Ifge(JumpOffset),
    Ifgt(JumpOffset),
    Ifle(JumpOffset),
    Iflt(JumpOffset),
    Ifne(JumpOffset),
    Ifnonnull(JumpOffset),
    Ifnull(JumpOffset),
    Iinc(u16, i16), // both wide and narrow
    Iload(u16),     // both wide and narrow
    Impdep1,
    Impdep2,
    Imul,
    Ineg,
    Instanceof(FieldType<'a>),
    Invokedynamic(InvokeDynamic<'a>),
    Invokeinterface(MemberRef<'a>, u8),
    Invokespecial(MemberRef<'a>),
    Invokestatic(MemberRef<'a>),
    Invokevirtual(MemberRef<'a>),
    Ior,
    Irem,
    Ireturn,
    Ishl,
    Ishr,
    Istore(u16), // both wide and narrow
    Isub,
    Iushr,
    Ixor,
    Jsr(JumpOffset), // both wide and narrow
    L2d,
    L2f,
    L2i,
    Ladd,
    Laload,
    Land,
    Lastore,
    Lcmp,
    Lconst0,
    Lconst1,
    Ldc(Loadable<'a>), // This doesn't validate the Loadable is not Long/Double types
    LdcW(Loadable<'a>), // This doesn't validate the Loadable is not Long/Double types
    Ldc2W(Loadable<'a>), // This doesn't validate the Loadable is only Long/Double types
    Ldiv,
    Lload(u16), // both wide and narrow
    Lmul,
    Lneg,
    Lookupswitch(LookupTable),
    Lor,
    Lrem,
    Lreturn,
    Lshl,
    Lshr,
    Lstore(u16), // both wide and narrow
    Lsub,
    Lushr,
    Lxor,
    Monitorenter,
    Monitorexit,
    Multianewarray(FieldType<'a>, u8),
    New(Cow<'a, str>),
    Newarray(PrimitiveArrayType),
    Nop,
    Pop,
    Pop2,
    Putfield(MemberRef<'a>),
    Putstatic(MemberRef<'a>),
    Ret(u16), // both wide and narrow
    Return,
    Saload,
    Sastore,
    Sipush(i16),
    Swap,
    Tableswitch(RangeTable),
}

#[derive(Debug)]
pub struct ByteCode<'a> {
    /// This contains pairs of (offset, opcode) where offset is the offset of the start
    /// of the opcode in bytes from the beginning of the data section of the Code attribute.
    /// This array will always be sorted in increasing offset order. The `get_opcode_index`
    /// function can be used to look up the vector index for a particular offset.
    pub opcodes: Vec<(usize, Opcode<'a>)>,
}

impl<'a> ByteCode<'a> {
    pub(crate) fn from(
        code: &'a [u8],
        pool: &[Rc<ConstantPoolEntry<'a>>],
    ) -> Result<Self, ParseError> {
        let bytecode = Self {
            opcodes: read_opcodes(code, pool)?,
        };
        bytecode.validate_jumps()?;
        Ok(bytecode)
    }

    /// Given an offset (in bytes) into the bytecode array, return the index into
    /// `self.opcodes` of the corresponding opcode. If there is no corresponding
    /// opcode at that offset, returns None.
    pub fn get_opcode_index(&self, offset: usize) -> Option<usize> {
        let mut min = 0_usize;
        let mut max = self.opcodes.len();
        while min < max {
            let mid = (min + max) / 2;
            let mid_offset = self.opcodes[mid].0;
            if mid_offset == offset {
                return Some(mid);
            }
            if mid_offset > offset {
                max = mid;
            } else if min == mid {
                break;
            } else {
                min = mid;
            }
        }
        None
    }

    fn validate_jump(&self, source_offset: i32, jump: JumpOffset) -> Result<(), ParseError> {
        let target_offset = usize::try_from(source_offset + jump)
            .map_err(|_| err!("Invalid destination after applying jump"))?;
        if self.get_opcode_index(target_offset).is_none() {
            fail!("Invalid opcode offset after applying jump");
        }
        Ok(())
    }

    fn validate_opcode_jumps(&self, offset: &usize, opcode: &Opcode) -> Result<(), ParseError> {
        let source_offset =
            i32::try_from(*offset).map_err(|_| err!("Unable to convert offset to i32"))?;
        match opcode {
            Opcode::Goto(j)
            | Opcode::IfAcmpeq(j)
            | Opcode::IfAcmpne(j)
            | Opcode::IfIcmpeq(j)
            | Opcode::IfIcmpge(j)
            | Opcode::IfIcmpgt(j)
            | Opcode::IfIcmple(j)
            | Opcode::IfIcmplt(j)
            | Opcode::IfIcmpne(j)
            | Opcode::Ifeq(j)
            | Opcode::Ifge(j)
            | Opcode::Ifgt(j)
            | Opcode::Ifle(j)
            | Opcode::Iflt(j)
            | Opcode::Ifne(j)
            | Opcode::Ifnonnull(j)
            | Opcode::Ifnull(j)
            | Opcode::Jsr(j) => self.validate_jump(source_offset, *j)?,
            Opcode::Lookupswitch(table) => {
                self.validate_jump(source_offset, table.default)
                    .map_err(|e| err!(e, "default jump offset"))?;
                for (i, jump) in &table.match_offsets {
                    self.validate_jump(source_offset, *jump)
                        .map_err(|e| err!(e, "match offset {}", i))?;
                }
            }
            Opcode::Tableswitch(table) => {
                self.validate_jump(source_offset, table.default)
                    .map_err(|e| err!(e, "default jump offset"))?;
                for (i, jump) in table.jumps.iter().enumerate() {
                    self.validate_jump(source_offset, *jump)
                        .map_err(|e| err!(e, "range offset {}", i))?;
                }
            }
            _ => (),
        };
        Ok(())
    }

    fn validate_jumps(&self) -> Result<(), ParseError> {
        for (offset, opcode) in &self.opcodes {
            self.validate_opcode_jumps(offset, opcode)
                .map_err(|e| err!(e, "opcode at offset {}", offset))?;
        }
        Ok(())
    }
}

fn read_opcodes<'a>(
    code: &'a [u8],
    pool: &[Rc<ConstantPoolEntry<'a>>],
) -> Result<Vec<(usize, Opcode<'a>)>, ParseError> {
    let mut opcodes = Vec::new();
    let mut ix = 0;
    while ix < code.len() {
        let opcode_ix = ix;
        let opcode = match read_u1(code, &mut ix)? {
            0x00 => Opcode::Nop,
            0x01 => Opcode::AconstNull,
            0x02 => Opcode::IconstM1,
            0x03 => Opcode::Iconst0,
            0x04 => Opcode::Iconst1,
            0x05 => Opcode::Iconst2,
            0x06 => Opcode::Iconst3,
            0x07 => Opcode::Iconst4,
            0x08 => Opcode::Iconst5,
            0x09 => Opcode::Lconst0,
            0x0a => Opcode::Lconst1,
            0x0b => Opcode::Fconst0,
            0x0c => Opcode::Fconst1,
            0x0d => Opcode::Fconst2,
            0x0e => Opcode::Dconst0,
            0x0f => Opcode::Dconst1,
            0x10 => Opcode::Bipush(read_u1(code, &mut ix)? as i8),
            0x11 => Opcode::Sipush(read_u2(code, &mut ix)? as i16),
            0x12 => Opcode::Ldc(get_cp_loadable(read_u1(code, &mut ix)?.into(), pool)?),
            0x13 => Opcode::LdcW(get_cp_loadable(read_u2(code, &mut ix)?.into(), pool)?),
            0x14 => Opcode::Ldc2W(get_cp_loadable(read_u2(code, &mut ix)?.into(), pool)?),
            0x15 => Opcode::Iload(read_u1(code, &mut ix)?.into()),
            0x16 => Opcode::Lload(read_u1(code, &mut ix)?.into()),
            0x17 => Opcode::Fload(read_u1(code, &mut ix)?.into()),
            0x18 => Opcode::Dload(read_u1(code, &mut ix)?.into()),
            0x19 => Opcode::Aload(read_u1(code, &mut ix)?.into()),
            0x1a => Opcode::Iload(0),
            0x1b => Opcode::Iload(1),
            0x1c => Opcode::Iload(2),
            0x1d => Opcode::Iload(3),
            0x1e => Opcode::Lload(0),
            0x1f => Opcode::Lload(1),
            0x20 => Opcode::Lload(2),
            0x21 => Opcode::Lload(3),
            0x22 => Opcode::Fload(0),
            0x23 => Opcode::Fload(1),
            0x24 => Opcode::Fload(2),
            0x25 => Opcode::Fload(3),
            0x26 => Opcode::Dload(0),
            0x27 => Opcode::Dload(1),
            0x28 => Opcode::Dload(2),
            0x29 => Opcode::Dload(3),
            0x2a => Opcode::Aload(0),
            0x2b => Opcode::Aload(1),
            0x2c => Opcode::Aload(2),
            0x2d => Opcode::Aload(3),
            0x2e => Opcode::Iaload,
            0x2f => Opcode::Laload,
            0x30 => Opcode::Faload,
            0x31 => Opcode::Daload,
            0x32 => Opcode::Aaload,
            0x33 => Opcode::Baload,
            0x34 => Opcode::Caload,
            0x35 => Opcode::Saload,
            0x36 => Opcode::Istore(read_u1(code, &mut ix)?.into()),
            0x37 => Opcode::Lstore(read_u1(code, &mut ix)?.into()),
            0x38 => Opcode::Fstore(read_u1(code, &mut ix)?.into()),
            0x39 => Opcode::Dstore(read_u1(code, &mut ix)?.into()),
            0x3a => Opcode::Astore(read_u1(code, &mut ix)?.into()),
            0x3b => Opcode::Istore(0),
            0x3c => Opcode::Istore(1),
            0x3d => Opcode::Istore(2),
            0x3e => Opcode::Istore(3),
            0x3f => Opcode::Lstore(0),
            0x40 => Opcode::Lstore(1),
            0x41 => Opcode::Lstore(2),
            0x42 => Opcode::Lstore(3),
            0x43 => Opcode::Fstore(0),
            0x44 => Opcode::Fstore(1),
            0x45 => Opcode::Fstore(2),
            0x46 => Opcode::Fstore(3),
            0x47 => Opcode::Dstore(0),
            0x48 => Opcode::Dstore(1),
            0x49 => Opcode::Dstore(2),
            0x4a => Opcode::Dstore(3),
            0x4b => Opcode::Astore(0),
            0x4c => Opcode::Astore(1),
            0x4d => Opcode::Astore(2),
            0x4e => Opcode::Astore(3),
            0x4f => Opcode::Iastore,
            0x50 => Opcode::Lastore,
            0x51 => Opcode::Fastore,
            0x52 => Opcode::Dastore,
            0x53 => Opcode::Aastore,
            0x54 => Opcode::Bastore,
            0x55 => Opcode::Castore,
            0x56 => Opcode::Sastore,
            0x57 => Opcode::Pop,
            0x58 => Opcode::Pop2,
            0x59 => Opcode::Dup,
            0x5a => Opcode::DupX1,
            0x5b => Opcode::DupX2,
            0x5c => Opcode::Dup2,
            0x5d => Opcode::Dup2X1,
            0x5e => Opcode::Dup2X2,
            0x5f => Opcode::Swap,
            0x60 => Opcode::Iadd,
            0x61 => Opcode::Ladd,
            0x62 => Opcode::Fadd,
            0x63 => Opcode::Dadd,
            0x64 => Opcode::Isub,
            0x65 => Opcode::Lsub,
            0x66 => Opcode::Fsub,
            0x67 => Opcode::Dsub,
            0x68 => Opcode::Imul,
            0x69 => Opcode::Lmul,
            0x6a => Opcode::Fmul,
            0x6b => Opcode::Dmul,
            0x6c => Opcode::Idiv,
            0x6d => Opcode::Ldiv,
            0x6e => Opcode::Fdiv,
            0x6f => Opcode::Ddiv,
            0x70 => Opcode::Irem,
            0x71 => Opcode::Lrem,
            0x72 => Opcode::Frem,
            0x73 => Opcode::Drem,
            0x74 => Opcode::Ineg,
            0x75 => Opcode::Lneg,
            0x76 => Opcode::Fneg,
            0x77 => Opcode::Dneg,
            0x78 => Opcode::Ishl,
            0x79 => Opcode::Lshl,
            0x7a => Opcode::Ishr,
            0x7b => Opcode::Lshr,
            0x7c => Opcode::Iushr,
            0x7d => Opcode::Lushr,
            0x7e => Opcode::Iand,
            0x7f => Opcode::Land,
            0x80 => Opcode::Ior,
            0x81 => Opcode::Lor,
            0x82 => Opcode::Ixor,
            0x83 => Opcode::Lxor,
            0x84 => Opcode::Iinc(
                read_u1(code, &mut ix)?.into(),
                (read_u1(code, &mut ix)? as i8).into(),
            ),
            0x85 => Opcode::I2l,
            0x86 => Opcode::I2f,
            0x87 => Opcode::I2d,
            0x88 => Opcode::L2i,
            0x89 => Opcode::L2f,
            0x8a => Opcode::L2d,
            0x8b => Opcode::F2i,
            0x8c => Opcode::F2l,
            0x8d => Opcode::F2d,
            0x8e => Opcode::D2i,
            0x8f => Opcode::D2l,
            0x90 => Opcode::D2f,
            0x91 => Opcode::I2b,
            0x92 => Opcode::I2c,
            0x93 => Opcode::I2s,
            0x94 => Opcode::Lcmp,
            0x95 => Opcode::Fcmpl,
            0x96 => Opcode::Fcmpg,
            0x97 => Opcode::Dcmpl,
            0x98 => Opcode::Dcmpg,
            0x99 => Opcode::Ifeq((read_u2(code, &mut ix)? as i16).into()),
            0x9a => Opcode::Ifne((read_u2(code, &mut ix)? as i16).into()),
            0x9b => Opcode::Iflt((read_u2(code, &mut ix)? as i16).into()),
            0x9c => Opcode::Ifge((read_u2(code, &mut ix)? as i16).into()),
            0x9d => Opcode::Ifgt((read_u2(code, &mut ix)? as i16).into()),
            0x9e => Opcode::Ifle((read_u2(code, &mut ix)? as i16).into()),
            0x9f => Opcode::IfIcmpeq((read_u2(code, &mut ix)? as i16).into()),
            0xa0 => Opcode::IfIcmpne((read_u2(code, &mut ix)? as i16).into()),
            0xa1 => Opcode::IfIcmplt((read_u2(code, &mut ix)? as i16).into()),
            0xa2 => Opcode::IfIcmpge((read_u2(code, &mut ix)? as i16).into()),
            0xa3 => Opcode::IfIcmpgt((read_u2(code, &mut ix)? as i16).into()),
            0xa4 => Opcode::IfIcmple((read_u2(code, &mut ix)? as i16).into()),
            0xa5 => Opcode::IfAcmpeq((read_u2(code, &mut ix)? as i16).into()),
            0xa6 => Opcode::IfAcmpne((read_u2(code, &mut ix)? as i16).into()),
            0xa7 => Opcode::Goto((read_u2(code, &mut ix)? as i16).into()),
            0xa8 => Opcode::Jsr((read_u2(code, &mut ix)? as i16).into()),
            0xa9 => Opcode::Ret(read_u1(code, &mut ix)?.into()),
            0xaa => {
                // Skip past padding to reach 4-byte alignment
                ix = (ix + 3) & !0x3;
                let default = read_u4(code, &mut ix)? as JumpOffset;
                let low = read_u4(code, &mut ix)? as i32;
                let high = read_u4(code, &mut ix)? as i32;
                if low > high {
                    fail!("The low value must be less than or equal to the high value in tableswitch at index {}", ix - 4);
                }
                let jump_count = match usize::try_from(high - low + 1) {
                    Ok(n) => n,
                    _ => fail!(
                        "Unable to convert range to usize in tableswitch at index {}",
                        ix - 4
                    ),
                };
                let mut jumps = Vec::with_capacity(jump_count);
                for _ in 0..jump_count {
                    jumps.push(read_u4(code, &mut ix)? as JumpOffset);
                }
                Opcode::Tableswitch(RangeTable {
                    default,
                    low,
                    high,
                    jumps,
                })
            }
            0xab => {
                // Skip past padding to reach 4-byte alignment
                ix = (ix + 3) & !0x3;
                let default = read_u4(code, &mut ix)? as JumpOffset;
                let npairs = read_u4(code, &mut ix)? as i32;
                if npairs < 0 {
                    fail!(
                        "Number of pairs in lookupswitch must be non-negative at index {}",
                        ix - 4
                    );
                }
                let pair_count = match usize::try_from(npairs) {
                    Ok(n) => n,
                    _ => fail!(
                        "Unable to convert number of pairs in lookupswitch to usize at index {}",
                        ix - 4
                    ),
                };
                let mut match_offsets = Vec::with_capacity(pair_count);
                for _ in 0..pair_count {
                    let match_part = read_u4(code, &mut ix)? as i32;
                    let offset_part = read_u4(code, &mut ix)? as JumpOffset;
                    match_offsets.push((match_part, offset_part));
                }
                Opcode::Lookupswitch(LookupTable {
                    default,
                    match_offsets,
                })
            }
            0xac => Opcode::Ireturn,
            0xad => Opcode::Lreturn,
            0xae => Opcode::Freturn,
            0xaf => Opcode::Dreturn,
            0xb0 => Opcode::Areturn,
            0xb1 => Opcode::Return,
            0xb2 => Opcode::Getstatic(read_cp_memberref(
                code,
                &mut ix,
                pool,
                ConstantPoolEntryTypes::FIELD_REF,
            )?),
            0xb3 => Opcode::Putstatic(read_cp_memberref(
                code,
                &mut ix,
                pool,
                ConstantPoolEntryTypes::FIELD_REF,
            )?),
            0xb4 => Opcode::Getfield(read_cp_memberref(
                code,
                &mut ix,
                pool,
                ConstantPoolEntryTypes::FIELD_REF,
            )?),
            0xb5 => Opcode::Putfield(read_cp_memberref(
                code,
                &mut ix,
                pool,
                ConstantPoolEntryTypes::FIELD_REF,
            )?),
            0xb6 => Opcode::Invokevirtual(read_cp_memberref(
                code,
                &mut ix,
                pool,
                ConstantPoolEntryTypes::METHOD_REF,
            )?),
            0xb7 => Opcode::Invokespecial(read_cp_memberref(
                code,
                &mut ix,
                pool,
                ConstantPoolEntryTypes::NEW_METHOD_REFS,
            )?),
            0xb8 => Opcode::Invokestatic(read_cp_memberref(
                code,
                &mut ix,
                pool,
                ConstantPoolEntryTypes::NEW_METHOD_REFS,
            )?),
            0xb9 => {
                let interfacemethod = read_cp_memberref(
                    code,
                    &mut ix,
                    pool,
                    ConstantPoolEntryTypes::INTERFACE_METHOD_REF,
                )?;
                let count = read_u1(code, &mut ix)?;
                if read_u1(code, &mut ix)? != 0 {
                    fail!("Nonzero byte found where zero byte expected in invokeinterface opcode at index {}", ix - 1);
                }
                Opcode::Invokeinterface(interfacemethod, count)
            }
            0xba => {
                let invokedynamic = read_cp_invokedynamic(code, &mut ix, pool)?;
                if read_u2(code, &mut ix)? != 0 {
                    fail!("Nonzero bytes found where zero bytes expected in invokedynamic opcode at index {}", ix - 2);
                }
                Opcode::Invokedynamic(invokedynamic)
            }
            0xbb => Opcode::New(read_cp_classinfo(code, &mut ix, pool)?),
            0xbc => {
                let primitive_type = match read_u1(code, &mut ix)? {
                    4 => PrimitiveArrayType::Boolean,
                    5 => PrimitiveArrayType::Char,
                    6 => PrimitiveArrayType::Float,
                    7 => PrimitiveArrayType::Double,
                    8 => PrimitiveArrayType::Byte,
                    9 => PrimitiveArrayType::Short,
                    10 => PrimitiveArrayType::Int,
                    11 => PrimitiveArrayType::Long,
                    _ => fail!(
                        "Unexpected array type for newarray opcode at index {}",
                        ix - 1
                    ),
                };
                Opcode::Newarray(primitive_type)
            }
            0xbd => Opcode::Anewarray({
                let ty = read_cp_classinfo(code, &mut ix, pool)?;
                let ty = match FieldType::parse(&ty) {
                    Ok(ty) => ty,
                    Err(_) => FieldType::Ty(Ty::Object(ty)),
                };
                match ty {
                    FieldType::Ty(Ty::Base(base)) => FieldType::Ty(Ty::Object(Cow::Owned(base.to_string()))),
                    ty => ty,
                }
            }),
            0xbe => Opcode::Arraylength,
            0xbf => Opcode::Athrow,
            0xc0 => Opcode::Checkcast({
                let ty = read_cp_classinfo(code, &mut ix, pool)?;
                let ty = match FieldType::parse(&ty) {
                    Ok(ty) => ty,
                    Err(_) => FieldType::Ty(Ty::Object(ty)),
                };
                match ty {
                    FieldType::Ty(Ty::Base(base)) => FieldType::Ty(Ty::Object(Cow::Owned(base.to_string()))),
                    ty => ty,
                }
            }),
            0xc1 => Opcode::Instanceof({
                let ty = read_cp_classinfo(code, &mut ix, pool)?;
                let ty = match FieldType::parse(&ty) {
                    Ok(ty) => ty,
                    Err(_) => FieldType::Ty(Ty::Object(ty)),
                };
                match ty {
                    FieldType::Ty(Ty::Base(base)) => FieldType::Ty(Ty::Object(Cow::Owned(base.to_string()))),
                    ty => ty,
                }
            }),
            0xc2 => Opcode::Monitorenter,
            0xc3 => Opcode::Monitorexit,
            0xc4 => {
                // wide modifier
                match read_u1(code, &mut ix)? {
                    0x15 => Opcode::Iload(read_u2(code, &mut ix)?),
                    0x16 => Opcode::Lload(read_u2(code, &mut ix)?),
                    0x17 => Opcode::Fload(read_u2(code, &mut ix)?),
                    0x18 => Opcode::Dload(read_u2(code, &mut ix)?),
                    0x19 => Opcode::Aload(read_u2(code, &mut ix)?),
                    0x36 => Opcode::Istore(read_u2(code, &mut ix)?),
                    0x37 => Opcode::Lstore(read_u2(code, &mut ix)?),
                    0x38 => Opcode::Fstore(read_u2(code, &mut ix)?),
                    0x39 => Opcode::Dstore(read_u2(code, &mut ix)?),
                    0x3a => Opcode::Astore(read_u2(code, &mut ix)?),
                    0x84 => Opcode::Iinc(read_u2(code, &mut ix)?, read_u2(code, &mut ix)? as i16),
                    0xa9 => Opcode::Ret(read_u2(code, &mut ix)?),
                    v => fail!(
                        "Unexpected opcode {} inside wide modifier at index {}",
                        v,
                        ix - 1
                    ),
                }
            }
            0xc5 => Opcode::Multianewarray(
                FieldType::parse(&read_cp_classinfo(code, &mut ix, pool)?)?,
                read_u1(code, &mut ix)?,
            ),
            0xc6 => Opcode::Ifnull((read_u2(code, &mut ix)? as i16).into()),
            0xc7 => Opcode::Ifnonnull((read_u2(code, &mut ix)? as i16).into()),
            0xc8 => Opcode::Goto(read_u4(code, &mut ix)? as JumpOffset),
            0xc9 => Opcode::Jsr(read_u4(code, &mut ix)? as JumpOffset),
            0xca => Opcode::Breakpoint,
            0xfe => Opcode::Impdep1,
            0xff => Opcode::Impdep2,
            v => fail!("Unexpected opcode {} at index {}", v, ix - 1),
        };
        opcodes.push((opcode_ix, opcode));
    }
    Ok(opcodes)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_opcode() {
        let bytecode = ByteCode { opcodes: vec![] };
        assert_eq!(bytecode.get_opcode_index(0), None);
        assert_eq!(bytecode.get_opcode_index(1), None);

        let bytecode = ByteCode {
            opcodes: vec![(0, Opcode::Nop)],
        };
        assert_eq!(bytecode.get_opcode_index(0), Some(0));
        assert_eq!(bytecode.get_opcode_index(1), None);

        let bytecode = ByteCode {
            opcodes: vec![(0, Opcode::Nop), (3, Opcode::Nop)],
        };
        assert_eq!(bytecode.get_opcode_index(0), Some(0));
        assert_eq!(bytecode.get_opcode_index(1), None);
        assert_eq!(bytecode.get_opcode_index(2), None);
        assert_eq!(bytecode.get_opcode_index(3), Some(1));
        assert_eq!(bytecode.get_opcode_index(4), None);

        let bytecode = ByteCode {
            opcodes: vec![(0, Opcode::Nop), (3, Opcode::Nop), (4, Opcode::Nop)],
        };
        assert_eq!(bytecode.get_opcode_index(0), Some(0));
        assert_eq!(bytecode.get_opcode_index(1), None);
        assert_eq!(bytecode.get_opcode_index(2), None);
        assert_eq!(bytecode.get_opcode_index(3), Some(1));
        assert_eq!(bytecode.get_opcode_index(4), Some(2));
        assert_eq!(bytecode.get_opcode_index(5), None);
    }
}
