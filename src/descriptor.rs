#![allow(clippy::ptr_arg)]

use std::{
    borrow::Cow,
    fmt::{self, Write},
    str::CharIndices,
};

use crate::ParseError;

/// MethodDescriptor as described in section 4.3.3 of the [JVM 18 specification](https://docs.oracle.com/javase/specs/jvms/se18/html/jvms-4.html#jvms-4.3.3)
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct MethodDescriptor<'a> {
    pub parameters: Vec<FieldType<'a>>,
    pub result: ReturnDescriptor<'a>,
}

impl<'a> MethodDescriptor<'a> {
    pub(crate) fn parse(chars: &Cow<'a, str>) -> Result<Self, ParseError> {
        let mut chars_idx = chars.char_indices();
        match chars_idx.next().map(|(_, ch)| ch) {
            Some('(') => (),
            Some(c) => fail!("Invalid start of method descriptor {}", c),
            None => fail!("Invalid start of method descriptor, missing ("),
        };

        let mut parameters: Vec<FieldType> = Vec::new();

        'done: loop {
            // preserve the next item for use in the FieldType parser
            let field = match chars_idx.as_str().chars().next() {
                Some(')') => {
                    chars_idx.next(); // consume the final ')'
                    break 'done;
                }
                Some(_) => FieldType::parse_from_chars_idx(chars, &mut chars_idx)?,
                None => fail!("Invalid method descriptor, missing end )"),
            };

            parameters.push(field);
        }

        let result = ReturnDescriptor::parse(chars, &mut chars_idx)?;

        Ok(MethodDescriptor { parameters, result })
    }
}

impl<'a> fmt::Display for MethodDescriptor<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.write_char('(')?;

        for param in &self.parameters {
            write!(f, "{}", param)?;
        }

        write!(f, "){}", self.result)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Ty<'a> {
    Base(BaseType),
    Object(Cow<'a, str>),
}

impl<'a> fmt::Display for Ty<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Base(base) => write!(f, "{}", base),
            Self::Object(obj) => write!(f, "L{};", obj),
        }
    }
}

/// FieldType as described in section 4.3.2 of the [JVM 18 specification](https://docs.oracle.com/javase/specs/jvms/se18/html/jvms-4.html#jvms-4.3.2)
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FieldType<'a> {
    Ty(Ty<'a>),
    Array { dimensions: usize, ty: Ty<'a> },
}

impl<'a> FieldType<'a> {
    pub(crate) fn parse(chars: &Cow<'a, str>) -> Result<Self, ParseError> {
        let mut chars_idx = chars.char_indices();
        Self::parse_from_chars_idx(chars, &mut chars_idx)
    }

    fn parse_from_chars_idx(
        chars: &Cow<'a, str>,
        chars_idx: &mut CharIndices,
    ) -> Result<Self, ParseError> {
        let mut field = None::<Ty>;
        let mut array_depth = 0;

        while let Some(ch) = chars_idx.next().map(|(_, ch)| ch) {
            match ch {
                'L' => {
                    field = Some(Ty::Object(parse_object(chars, chars_idx)?));
                    break;
                }
                '[' => {
                    array_depth += 1;

                    // A field descriptor representing an array type is valid only if it represents a type with 255 or fewer dimensions.
                    //  see: https://docs.oracle.com/javase/specs/jvms/se18/html/jvms-4.html#jvms-4.3.2
                    if array_depth > 255 {
                        fail!("Array exceeds 255 dimensions");
                    }
                }
                ch => {
                    field = Some(Ty::Base(BaseType::parse(ch)?));
                    break;
                }
            };
        }

        let field = field.ok_or_else(|| err!("FieldType not specified"))?;
        if array_depth > 0 {
            Ok(FieldType::Array {
                dimensions: array_depth,
                ty: field,
            })
        } else {
            Ok(FieldType::Ty(field))
        }
    }
}

impl<'a> fmt::Display for FieldType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Ty(ty) => write!(f, "{}", ty),
            Self::Array { dimensions, ty } => write!(f, "{}{}", "[".repeat(*dimensions), ty),
        }
    }
}

/// BaseType as described in Table 4.3-A. of the [JVM 18 specification](https://docs.oracle.com/javase/specs/jvms/se18/html/jvms-4.html#jvms-4.3.2-200)
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum BaseType {
    /// B, byte, signed byte
    Byte,
    /// C, char, Unicode character code point in the Basic Multilingual Plane, encoded with UTF-16
    Char,
    /// D, double, double-precision floating-point value
    Double,
    /// F, float, single-precision floating-point value
    Float,
    /// I, int, integer
    Int,
    /// J, long, long integer
    Long,
    /// S, short, signed short
    Short,
    /// Z, boolean, true or false
    Boolean,
}

impl BaseType {
    fn parse(ch: char) -> Result<Self, ParseError> {
        let this = match ch {
            'B' => Self::Byte,
            'C' => Self::Char,
            'D' => Self::Double,
            'F' => Self::Float,
            'I' => Self::Int,
            'J' => Self::Long,
            'S' => Self::Short,
            'Z' => Self::Boolean,
            _ => fail!("Invalid base type {}", ch),
        };

        Ok(this)
    }
}

impl fmt::Display for BaseType {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let c = match self {
            Self::Byte => 'B',
            Self::Char => 'C',
            Self::Double => 'D',
            Self::Float => 'F',
            Self::Int => 'I',
            Self::Long => 'J',
            Self::Short => 'S',
            Self::Boolean => 'Z',
        };

        f.write_char(c)
    }
}

/// ReturnDescriptor
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ReturnDescriptor<'a> {
    Return(FieldType<'a>),
    Void,
}

impl<'a> ReturnDescriptor<'a> {
    fn parse(chars: &Cow<'a, str>, chars_idx: &mut CharIndices) -> Result<Self, ParseError> {
        // preserve the next item for use in the FieldType parser
        let result = match chars_idx.as_str().chars().next() {
            Some('V') => {
                chars_idx.next(); // for correctness
                Self::Void
            }
            Some(_) => Self::Return(FieldType::parse_from_chars_idx(chars, chars_idx)?),
            None => fail!("Invalid return descriptor, missing value"),
        };

        Ok(result)
    }
}

impl<'a> fmt::Display for ReturnDescriptor<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Void => f.write_char('V'),
            Self::Return(field) => write!(f, "{}", field),
        }
    }
}

/// Parses the object less the beginning L, e.g. this expects `java/lang/Object;`
fn parse_object<'a>(
    chars: &Cow<'a, str>,
    chars_idx: &mut CharIndices,
) -> Result<Cow<'a, str>, ParseError> {
    let start_idx = chars_idx
        .next()
        .map(|ch_idx| ch_idx.0)
        .ok_or_else(|| err!("Invalid object descriptor, expected ;"))?;

    let end_idx = chars_idx
        .find_map(|(idx, ch)| if ch == ';' { Some(idx) } else { None })
        .ok_or_else(|| err!("Invalid object descriptor, expected ;"))?;

    // Because a Cow can be either Borrowed or Owned, we need to create an Owned String in the case that it's not a reference.
    // This should be rare, if ever.
    let object = match *chars {
        Cow::Borrowed(chars) => {
            let object = chars
                .get(start_idx..end_idx)
                .ok_or_else(|| err!("Invalid object descriptor, out of bounds"))?;
            Cow::Borrowed(object)
        }
        Cow::Owned(ref chars) => {
            let object = chars
                .get(start_idx..end_idx)
                .ok_or_else(|| err!("Invalid object descriptor, out of bounds"))?;
            Cow::Owned(object.to_string())
        }
    };

    Ok(object)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_void_void() {
        let chars = Cow::from("()V");

        let descriptor = MethodDescriptor::parse(&chars).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let result = descriptor.result;

        assert!(parameters.next().is_none());
        assert_eq!(result, ReturnDescriptor::Void);
    }

    #[test]
    fn test_single_param() {
        let chars = Cow::from("(J)V");

        let descriptor = MethodDescriptor::parse(&chars).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let result = descriptor.result;

        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Ty(Ty::Base(BaseType::Long))
        );
        assert!(parameters.next().is_none());
        assert_eq!(result, ReturnDescriptor::Void);
    }

    #[test]
    fn test_basetype_return() {
        let chars = Cow::from("()J");

        let descriptor = MethodDescriptor::parse(&chars).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let result = descriptor.result;

        assert!(parameters.next().is_none());
        assert_eq!(
            result,
            ReturnDescriptor::Return(FieldType::Ty(Ty::Base(BaseType::Long)))
        );
    }

    #[test]
    fn test_all_basetype_params() {
        let chars = Cow::from("(BCDFIJSZ)V");

        let descriptor = MethodDescriptor::parse(&chars).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let result = descriptor.result;

        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Ty(Ty::Base(BaseType::Byte))
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Ty(Ty::Base(BaseType::Char))
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Ty(Ty::Base(BaseType::Double))
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Ty(Ty::Base(BaseType::Float))
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Ty(Ty::Base(BaseType::Int))
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Ty(Ty::Base(BaseType::Long))
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Ty(Ty::Base(BaseType::Short))
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Ty(Ty::Base(BaseType::Boolean))
        );
        assert!(parameters.next().is_none());
        assert_eq!(result, ReturnDescriptor::Void);
    }

    #[test]
    fn test_object_param() {
        let chars = Cow::from("(Ljava/lang/Object;)V");

        let descriptor = MethodDescriptor::parse(&chars).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let result = descriptor.result;

        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Ty(Ty::Object(Cow::Borrowed("java/lang/Object")))
        );
        assert!(parameters.next().is_none());
        assert_eq!(result, ReturnDescriptor::Void);
    }

    #[test]
    fn test_owned_cow() {
        let chars = Cow::from("(Ljava/lang/Object;)V".to_string());

        let descriptor = MethodDescriptor::parse(&chars).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let result = descriptor.result;

        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Ty(Ty::Object(Cow::Borrowed("java/lang/Object")))
        );
        assert!(parameters.next().is_none());
        assert_eq!(result, ReturnDescriptor::Void);
    }

    #[test]
    fn test_multi_object_param() {
        let chars = Cow::from("(Ljava/lang/Object;Ljava/lang/String;)V");

        let descriptor = MethodDescriptor::parse(&chars).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let result = descriptor.result;

        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Ty(Ty::Object(Cow::Borrowed("java/lang/Object")))
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Ty(Ty::Object(Cow::Borrowed("java/lang/String")))
        );
        assert!(parameters.next().is_none());
        assert_eq!(result, ReturnDescriptor::Void);
    }

    #[test]
    fn test_object_return() {
        let chars = Cow::from("()Ljava/lang/Object;");

        let descriptor = MethodDescriptor::parse(&chars).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let result = descriptor.result;

        assert!(parameters.next().is_none());
        assert_eq!(
            result,
            ReturnDescriptor::Return(FieldType::Ty(Ty::Object(Cow::Borrowed("java/lang/Object"))))
        );
    }

    #[test]
    fn test_array_basetype_param() {
        let chars = Cow::from("([J)V");

        let descriptor = MethodDescriptor::parse(&chars).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let result = descriptor.result;

        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Array {
                dimensions: 1,
                ty: Ty::Base(BaseType::Long)
            }
        );
        assert!(parameters.next().is_none());
        assert_eq!(result, ReturnDescriptor::Void);
    }

    #[test]
    fn test_multi_array_param() {
        let chars = Cow::from("([[J)V");

        let descriptor = MethodDescriptor::parse(&chars).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let result = descriptor.result;

        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Array {
                dimensions: 2,
                ty: Ty::Base(BaseType::Long)
            }
        );
        assert!(parameters.next().is_none());
        assert_eq!(result, ReturnDescriptor::Void);
    }

    #[test]
    fn test_array_return() {
        let chars = Cow::from("()[J");

        let descriptor = MethodDescriptor::parse(&chars).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let result = descriptor.result;

        assert!(parameters.next().is_none());
        assert_eq!(
            result,
            ReturnDescriptor::Return(FieldType::Array {
                dimensions: 1,
                ty: Ty::Base(BaseType::Long)
            })
        );
    }

    #[test]
    fn test_display() {
        let descriptor = MethodDescriptor {
            parameters: vec![
                FieldType::Ty(Ty::Base(BaseType::Long)),
                FieldType::Ty(Ty::Object(Cow::Borrowed("java/lang/Object"))),
                FieldType::Array {
                    dimensions: 2,
                    ty: Ty::Base(BaseType::Byte),
                },
            ],
            result: ReturnDescriptor::Void,
        };

        assert_eq!(descriptor.to_string(), "(JLjava/lang/Object;[[B)V");
    }

    #[test]
    fn test_max_array_depth() {
        let chars_ok = Cow::from(format!("({}J)V", "[".repeat(255)));
        let chars_bad = Cow::from(format!("({}J)V", "[".repeat(256)));

        assert!(MethodDescriptor::parse(&chars_ok).is_ok());
        assert!(MethodDescriptor::parse(&chars_bad).is_err());
    }
}
