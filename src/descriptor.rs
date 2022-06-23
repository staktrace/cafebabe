#![allow(clippy::ptr_arg)]

use std::{
    borrow::Cow,
    fmt::{self, Write},
    str::CharIndices,
};

use crate::ParseError;

/// MethodDescriptor as described in section 4.3.3 of the [JVM 18 specification](https://docs.oracle.com/javase/specs/jvms/se18/jvms18.pdf)
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

/// FieldType as described in section 4.3.2 of the [JVM 18 specification](https://docs.oracle.com/javase/specs/jvms/se18/jvms18.pdf)
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FieldType<'a> {
    Base(BaseType),
    Object(Cow<'a, str>),
    Array(Box<FieldType<'a>>),
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
        let field = match chars_idx.next().map(|(_, ch)| ch) {
            Some('L') => Self::Object(parse_object(chars, chars_idx)?),
            Some('[') => Self::Array(Box::new(FieldType::parse_from_chars_idx(chars, chars_idx)?)),
            Some(ch) => Self::Base(BaseType::parse(ch)?),
            None => fail!("Invalid FieldType"),
        };

        Ok(field)
    }
}

impl<'a> fmt::Display for FieldType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Base(base) => write!(f, "{}", base),
            Self::Object(obj) => write!(f, "L{};", obj),
            Self::Array(arr) => write!(f, "[{}", arr),
        }
    }
}

/// BaseType as described in Table 4.3-A. of the [JVM 18 specification](https://docs.oracle.com/javase/specs/jvms/se18/jvms18.pdf)
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BaseType {
    /// B, byte, signed byte
    BYTE,
    /// C, char, Unicode character code point in the Basic Multilingual Plane, encoded with UTF-16
    CHAR,
    /// D, double, double-precision floating-point value
    DOUBLE,
    /// F, float, single-precision floating-point value
    FLOAT,
    /// I, int, integer
    INT,
    /// J, long, long integer
    LONG,
    /// S, short, signed short
    SHORT,
    /// Z, boolean, true or false
    BOOLEAN,
}

impl BaseType {
    fn parse(ch: char) -> Result<Self, ParseError> {
        let this = match ch {
            'B' => Self::BYTE,
            'C' => Self::CHAR,
            'D' => Self::DOUBLE,
            'F' => Self::FLOAT,
            'I' => Self::INT,
            'J' => Self::LONG,
            'S' => Self::SHORT,
            'Z' => Self::BOOLEAN,
            _ => fail!("Invalid base type {}", ch),
        };

        Ok(this)
    }
}

impl fmt::Display for BaseType {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let c = match self {
            Self::BYTE => 'B',
            Self::CHAR => 'C',
            Self::DOUBLE => 'D',
            Self::FLOAT => 'F',
            Self::INT => 'I',
            Self::LONG => 'J',
            Self::SHORT => 'S',
            Self::BOOLEAN => 'Z',
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

        assert_eq!(parameters.next().unwrap(), FieldType::Base(BaseType::LONG));
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
            ReturnDescriptor::Return(FieldType::Base(BaseType::LONG))
        );
    }

    #[test]
    fn test_all_basetype_params() {
        let chars = Cow::from("(BCDFIJSZ)V");

        let descriptor = MethodDescriptor::parse(&chars).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let result = descriptor.result;

        assert_eq!(parameters.next().unwrap(), FieldType::Base(BaseType::BYTE));
        assert_eq!(parameters.next().unwrap(), FieldType::Base(BaseType::CHAR));
        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Base(BaseType::DOUBLE)
        );
        assert_eq!(parameters.next().unwrap(), FieldType::Base(BaseType::FLOAT));
        assert_eq!(parameters.next().unwrap(), FieldType::Base(BaseType::INT));
        assert_eq!(parameters.next().unwrap(), FieldType::Base(BaseType::LONG));
        assert_eq!(parameters.next().unwrap(), FieldType::Base(BaseType::SHORT));
        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Base(BaseType::BOOLEAN)
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
            FieldType::Object(Cow::Borrowed("java/lang/Object"))
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
            FieldType::Object(Cow::Borrowed("java/lang/Object"))
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
            FieldType::Object(Cow::Borrowed("java/lang/Object"))
        );
        assert_eq!(
            parameters.next().unwrap(),
            FieldType::Object(Cow::Borrowed("java/lang/String"))
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
            ReturnDescriptor::Return(FieldType::Object(Cow::Borrowed("java/lang/Object")))
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
            FieldType::Array(Box::new(FieldType::Base(BaseType::LONG)))
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
            FieldType::Array(Box::new(FieldType::Array(Box::new(FieldType::Base(
                BaseType::LONG
            )))))
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
            ReturnDescriptor::Return(FieldType::Array(Box::new(FieldType::Base(BaseType::LONG))))
        );
    }

    #[test]
    fn test_display() {
        let descriptor = MethodDescriptor {
            parameters: vec![
                FieldType::Base(BaseType::LONG),
                FieldType::Object(Cow::Borrowed("java/lang/Object")),
                FieldType::Array(Box::new(FieldType::Base(BaseType::BYTE))),
            ],
            result: ReturnDescriptor::Void,
        };

        assert_eq!(descriptor.to_string(), "(JLjava/lang/Object;[B)V");
    }
}
