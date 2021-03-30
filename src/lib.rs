pub struct ClassFile<'a> {
    raw_bytes: &'a [u8]
}

pub fn parse_class<'a>(raw_bytes: &'a [u8]) -> Result<ClassFile<'a>, String> {
    Ok(ClassFile {
        raw_bytes
    })
}
