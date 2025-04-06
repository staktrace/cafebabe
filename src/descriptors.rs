use std::{
    borrow::Cow,
    fmt::{self, Write},
};

use crate::ParseError;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct UnqualifiedSegment<'a> {
    pub name: Cow<'a, str>,
}

// Returns the unqualified segment and the following char (either '/' or ';')
// or an error. This only extracts the unqualified segment at the start of
// the given data, and ignores anything following.
fn parse_unqualified_segment<'a>(
    data: &Cow<'a, str>,
    start_index: usize,
) -> Result<(UnqualifiedSegment<'a>, char), ParseError> {
    for (ix, c) in data[start_index..].char_indices() {
        match c {
            '/' if ix == 0 => fail!("Unexpected / at start of unqualified segment"),
            ';' if ix == 0 => fail!("Unexpected ; at start of unqualified segment"),
            '/' | ';' => {
                let name = match data {
                    Cow::Borrowed(borrowed_str) => {
                        Cow::Borrowed(&borrowed_str[start_index..start_index + ix])
                    }
                    Cow::Owned(ref owned_str) => {
                        Cow::Owned(owned_str[start_index..start_index + ix].to_string())
                    }
                };
                let segment = UnqualifiedSegment { name };
                return Ok((segment, c));
            }
            '.' | '[' | '<' | '>' => fail!("Disallowed character in unqualified segment"),
            _ => (),
        };
    }
    fail!("Unterminated unqualified segment");
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ClassName<'a> {
    pub segments: Vec<UnqualifiedSegment<'a>>,
}

impl<'a> ClassName<'a> {
    fn byte_len(&self) -> usize {
        self.segments
            .iter()
            .fold(0, |sum, segment| sum + segment.name.len() + 1)
    }
}
impl<'a> fmt::Display for ClassName<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let segments: Vec<Cow<'a, str>> = self.segments.iter().map(|s| s.name.clone()).collect();
        write!(f, "{}", segments.join("/"))
    }
}

// Returns the classname descriptor at the start of the given data, and ignores anything following.
// Returns an error if there was no such classname.
fn parse_class_descriptor<'a>(
    data: &Cow<'a, str>,
    index: usize,
) -> Result<ClassName<'a>, ParseError> {
    let mut segments = vec![];
    let mut remaining_index = index;
    loop {
        match parse_unqualified_segment(data, remaining_index)? {
            (segment, ';') => {
                segments.push(segment);
                return Ok(ClassName { segments });
            }
            (segment, '/') => {
                remaining_index += segment.name.len() + 1;
                segments.push(segment);
                continue;
            }
            _ => panic!("Got unexpected return value from parse_unqualified_segment"),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FieldType<'a> {
    Byte,
    Char,
    Double,
    Float,
    Integer,
    Long,
    Short,
    Boolean,
    Object(ClassName<'a>),
}

impl<'a> FieldType<'a> {
    fn byte_len(&self) -> usize {
        match self {
            FieldType::Object(class_name) => 1 + class_name.byte_len(),
            _ => 1,
        }
    }
}

impl fmt::Display for FieldType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Byte => write!(f, "B"),
            Self::Char => write!(f, "C"),
            Self::Double => write!(f, "D"),
            Self::Float => write!(f, "F"),
            Self::Integer => write!(f, "I"),
            Self::Long => write!(f, "J"),
            Self::Short => write!(f, "S"),
            Self::Boolean => write!(f, "Z"),
            Self::Object(obj) => write!(f, "L{};", obj),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FieldDescriptor<'a> {
    pub dimensions: u8,
    pub field_type: FieldType<'a>,
}

impl<'a> FieldDescriptor<'a> {
    fn byte_len(&self) -> usize {
        (self.dimensions as usize) + self.field_type.byte_len()
    }
}

impl fmt::Display for FieldDescriptor<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.dimensions > 0 {
            write!(
                f,
                "{}{}",
                "[".repeat(self.dimensions as usize),
                self.field_type
            )
        } else {
            write!(f, "{}", self.field_type)
        }
    }
}

// Parse the field descriptor at the start of the given data, and ignores anything
// following. Returns an error if the data don't start with a field descriptor.
pub(crate) fn parse_field_descriptor<'a>(
    data: &Cow<'a, str>,
    index: usize,
) -> Result<FieldDescriptor<'a>, ParseError> {
    let mut dimensions: usize = 0;
    for c in data[index..].chars() {
        if c == '[' {
            dimensions += 1;
            if dimensions > 255 {
                fail!("Dimensions in field descriptor exceeded allowed limit");
            }
            continue;
        }
        let field_type = match c {
            'B' => FieldType::Byte,
            'C' => FieldType::Char,
            'D' => FieldType::Double,
            'F' => FieldType::Float,
            'I' => FieldType::Integer,
            'J' => FieldType::Long,
            'S' => FieldType::Short,
            'Z' => FieldType::Boolean,
            'L' => FieldType::Object(parse_class_descriptor(data, index + dimensions + 1)?),
            _ => fail!("Unexpected field type"),
        };
        return Ok(FieldDescriptor {
            dimensions: dimensions as u8,
            field_type,
        });
    }
    fail!("Empty string is not a field descriptor");
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ReturnDescriptor<'a> {
    Return(FieldDescriptor<'a>),
    Void,
}
impl fmt::Display for ReturnDescriptor<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Void => f.write_char('V'),
            Self::Return(field) => write!(f, "{}", field),
        }
    }
}

impl<'a> ReturnDescriptor<'a> {
    fn byte_len(&self) -> usize {
        match self {
            Self::Return(d) => d.byte_len(),
            Self::Void => 1,
        }
    }
}

fn parse_return_descriptor<'a>(
    data: &Cow<'a, str>,
    index: usize,
) -> Result<ReturnDescriptor<'a>, ParseError> {
    if &data[index..] == "V" {
        Ok(ReturnDescriptor::Void)
    } else {
        Ok(ReturnDescriptor::Return(parse_field_descriptor(
            data, index,
        )?))
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct MethodDescriptor<'a> {
    pub parameters: Vec<FieldDescriptor<'a>>,
    pub return_type: ReturnDescriptor<'a>,
}

impl fmt::Display for MethodDescriptor<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.write_char('(')?;

        for param in &self.parameters {
            write!(f, "{}", param)?;
        }

        write!(f, "){}", self.return_type)
    }
}

impl<'a> MethodDescriptor<'a> {
    fn byte_len(&self) -> usize {
        1 + self
            .parameters
            .iter()
            .fold(0, |sum, param| sum + param.byte_len())
            + 1
            + self.return_type.byte_len()
    }
}

pub(crate) fn parse_method_descriptor<'a>(
    data: &Cow<'a, str>,
    mut index: usize,
) -> Result<MethodDescriptor<'a>, ParseError> {
    let bytes = data.as_bytes();
    if bytes.len() <= index || bytes[index] != b'(' {
        fail!("Method descriptor must start with '('")
    }
    index += 1;
    let mut parameters = vec![];
    loop {
        if bytes.len() > index && bytes[index] == b')' {
            break;
        }
        let parameter = parse_field_descriptor(data, index)?;
        index += parameter.byte_len();
        parameters.push(parameter);
    }
    index += 1;
    let return_type = parse_return_descriptor(data, index)?;
    Ok(MethodDescriptor {
        parameters,
        return_type,
    })
}

pub(crate) fn parse_array_descriptor<'a>(
    data: &Cow<'a, str>,
) -> Result<Option<FieldDescriptor<'a>>, ParseError> {
    if data.len() == 0 || data.as_bytes()[0] != b'[' {
        return Ok(None);
    }
    let desc = parse_field_descriptor(data, 0)?;
    if data.len() != desc.byte_len() {
        fail!("Not a field descriptor")
    }
    Ok(Some(desc))
}

pub(crate) fn is_field_descriptor(name: &str) -> bool {
    match parse_field_descriptor(&Cow::Borrowed(name), 0) {
        Ok(desc) => name.len() == desc.byte_len(),
        Err(_) => false,
    }
}

pub(crate) fn is_array_descriptor(name: &str) -> bool {
    is_field_descriptor(name) && name.as_bytes()[0] == b'['
}

pub(crate) fn is_method_descriptor(name: &str) -> bool {
    match parse_method_descriptor(&Cow::Borrowed(name), 0) {
        Ok(desc) => name.len() == desc.byte_len(),
        Err(_) => false,
    }
}

pub(crate) fn is_return_descriptor(name: &str) -> bool {
    match parse_return_descriptor(&Cow::Borrowed(name), 0) {
        Ok(desc) => name.len() == desc.byte_len(),
        Err(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_field_descriptors() {
        assert!(is_field_descriptor("I"));
        assert!(is_field_descriptor("[Ljava;"));
        assert!(is_field_descriptor("[Ljava/lang/Object;"));
        assert!(is_field_descriptor("[[Z"));

        assert!(!is_field_descriptor("M"));
        assert!(!is_field_descriptor("[[L;"));
        assert!(!is_field_descriptor("[[Ljava/;"));
        assert!(!is_field_descriptor("[[L/java;"));
        assert!(!is_field_descriptor("[[Ljava"));
        assert!(!is_field_descriptor("[[Ljava/lang/Object;stuff"));
        assert!(!is_field_descriptor("Istuff"));
    }

    #[test]
    fn test_method_descriptors() {
        assert!(is_method_descriptor("()V"));
        assert!(is_method_descriptor("(II)V"));
        assert!(is_method_descriptor("([Ljava/lang/Object;)V"));
        assert!(is_method_descriptor("(Ljava/lang/Object;I)V"));
        assert!(is_method_descriptor("()Ljava/lang/Obejct;"));
        assert!(is_method_descriptor("()I"));

        assert!(!is_method_descriptor("(V)V"));
        assert!(!is_method_descriptor("("));
        assert!(!is_method_descriptor(")"));
        assert!(!is_method_descriptor("()"));
        assert!(!is_method_descriptor("()VV"));
        assert!(!is_method_descriptor("()II"));
        assert!(!is_method_descriptor("()ILjava/lang/Object;"));
    }

    #[test]
    fn test_void_void() {
        let chars = Cow::from("()V");

        let descriptor = parse_method_descriptor(&chars, 0).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let return_type = descriptor.return_type;

        assert!(parameters.next().is_none());
        assert_eq!(return_type, ReturnDescriptor::Void);
    }

    #[test]
    fn test_single_param() {
        let chars = Cow::from("(J)V");

        let descriptor = parse_method_descriptor(&chars, 0).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let return_type = descriptor.return_type;

        assert_eq!(
            parameters.next().unwrap(),
            FieldDescriptor {
                dimensions: 0,
                field_type: FieldType::Long
            },
        );
        assert!(parameters.next().is_none());
        assert_eq!(return_type, ReturnDescriptor::Void);
    }

    #[test]
    fn test_basetype_return() {
        let chars = Cow::from("()J");

        let descriptor = parse_method_descriptor(&chars, 0).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let return_type = descriptor.return_type;

        assert!(parameters.next().is_none());
        assert_eq!(
            return_type,
            ReturnDescriptor::Return(FieldDescriptor {
                dimensions: 0,
                field_type: FieldType::Long
            }),
        );
    }

    #[test]
    fn test_all_basetype_params() {
        let chars = Cow::from("(BCDFIJSZ)V");

        let descriptor = parse_method_descriptor(&chars, 0).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let return_type = descriptor.return_type;

        assert_eq!(
            parameters.next().unwrap(),
            FieldDescriptor {
                dimensions: 0,
                field_type: FieldType::Byte
            },
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldDescriptor {
                dimensions: 0,
                field_type: FieldType::Char
            },
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldDescriptor {
                dimensions: 0,
                field_type: FieldType::Double
            },
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldDescriptor {
                dimensions: 0,
                field_type: FieldType::Float
            },
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldDescriptor {
                dimensions: 0,
                field_type: FieldType::Integer
            },
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldDescriptor {
                dimensions: 0,
                field_type: FieldType::Long
            },
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldDescriptor {
                dimensions: 0,
                field_type: FieldType::Short
            },
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldDescriptor {
                dimensions: 0,
                field_type: FieldType::Boolean
            },
        );
        assert!(parameters.next().is_none());
        assert_eq!(return_type, ReturnDescriptor::Void);
    }

    #[test]
    fn test_object_param() {
        let chars = Cow::from("(Ljava/lang/Object;)V");

        let descriptor = parse_method_descriptor(&chars, 0).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let return_type = descriptor.return_type;

        assert_eq!(
            parameters.next().unwrap(),
            FieldDescriptor {
                dimensions: 0,
                field_type: FieldType::Object(ClassName {
                    segments: vec![
                        UnqualifiedSegment {
                            name: Cow::Borrowed("java")
                        },
                        UnqualifiedSegment {
                            name: Cow::Borrowed("lang")
                        },
                        UnqualifiedSegment {
                            name: Cow::Borrowed("Object")
                        },
                    ],
                }),
            },
        );
        assert!(parameters.next().is_none());
        assert_eq!(return_type, ReturnDescriptor::Void);
    }

    #[test]
    fn test_owned_cow() {
        let chars = Cow::from("(Ljava/lang/Object;)V".to_string());

        let descriptor = parse_method_descriptor(&chars, 0).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let return_type = descriptor.return_type;

        assert_eq!(
            parameters.next().unwrap(),
            FieldDescriptor {
                dimensions: 0,
                field_type: FieldType::Object(ClassName {
                    segments: vec![
                        UnqualifiedSegment {
                            name: Cow::Borrowed("java")
                        },
                        UnqualifiedSegment {
                            name: Cow::Borrowed("lang")
                        },
                        UnqualifiedSegment {
                            name: Cow::Borrowed("Object")
                        },
                    ],
                }),
            },
        );
        assert!(parameters.next().is_none());
        assert_eq!(return_type, ReturnDescriptor::Void);
    }

    #[test]
    fn test_multi_object_param() {
        let chars = Cow::from("(Ljava/lang/Object;Ljava/lang/String;)V");

        let descriptor = parse_method_descriptor(&chars, 0).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let return_type = descriptor.return_type;

        assert_eq!(
            parameters.next().unwrap(),
            FieldDescriptor {
                dimensions: 0,
                field_type: FieldType::Object(ClassName {
                    segments: vec![
                        UnqualifiedSegment {
                            name: Cow::Borrowed("java")
                        },
                        UnqualifiedSegment {
                            name: Cow::Borrowed("lang")
                        },
                        UnqualifiedSegment {
                            name: Cow::Borrowed("Object")
                        },
                    ],
                }),
            },
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldDescriptor {
                dimensions: 0,
                field_type: FieldType::Object(ClassName {
                    segments: vec![
                        UnqualifiedSegment {
                            name: Cow::Borrowed("java")
                        },
                        UnqualifiedSegment {
                            name: Cow::Borrowed("lang")
                        },
                        UnqualifiedSegment {
                            name: Cow::Borrowed("String")
                        },
                    ],
                }),
            },
        );
        assert!(parameters.next().is_none());
        assert_eq!(return_type, ReturnDescriptor::Void);
    }

    #[test]
    fn test_object_return() {
        let chars = Cow::from("()Ljava/lang/Object;");

        let descriptor = parse_method_descriptor(&chars, 0).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let return_type = descriptor.return_type;

        assert!(parameters.next().is_none());
        assert_eq!(
            return_type,
            ReturnDescriptor::Return(FieldDescriptor {
                dimensions: 0,
                field_type: FieldType::Object(ClassName {
                    segments: vec![
                        UnqualifiedSegment {
                            name: Cow::Borrowed("java")
                        },
                        UnqualifiedSegment {
                            name: Cow::Borrowed("lang")
                        },
                        UnqualifiedSegment {
                            name: Cow::Borrowed("Object")
                        },
                    ],
                }),
            }),
        );
    }

    #[test]
    fn test_array_basetype_param() {
        let chars = Cow::from("([J)V");

        let descriptor = parse_method_descriptor(&chars, 0).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let return_type = descriptor.return_type;

        assert_eq!(
            parameters.next().unwrap(),
            FieldDescriptor {
                dimensions: 1,
                field_type: FieldType::Long,
            }
        );
        assert!(parameters.next().is_none());
        assert_eq!(return_type, ReturnDescriptor::Void);
    }

    #[test]
    fn test_multi_array_param() {
        let chars = Cow::from("([[J)V");

        let descriptor = parse_method_descriptor(&chars, 0).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let return_type = descriptor.return_type;

        assert_eq!(
            parameters.next().unwrap(),
            FieldDescriptor {
                dimensions: 2,
                field_type: FieldType::Long,
            }
        );
        assert!(parameters.next().is_none());
        assert_eq!(return_type, ReturnDescriptor::Void);
    }

    #[test]
    fn test_array_return() {
        let chars = Cow::from("()[J");

        let descriptor = parse_method_descriptor(&chars, 0).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let return_type = descriptor.return_type;

        assert!(parameters.next().is_none());
        assert_eq!(
            return_type,
            ReturnDescriptor::Return(FieldDescriptor {
                dimensions: 1,
                field_type: FieldType::Long,
            })
        );
    }

    #[test]
    fn test_max_array_depth() {
        let chars_ok = Cow::from(format!("({}J)V", "[".repeat(255)));
        let chars_bad = Cow::from(format!("({}J)V", "[".repeat(256)));

        assert!(parse_method_descriptor(&chars_ok, 0).is_ok());
        assert!(parse_method_descriptor(&chars_bad, 0).is_err());
    }
}
