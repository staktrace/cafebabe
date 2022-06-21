use std::{
    borrow::Cow,
    fmt::{self, Write},
    str::Chars,
};

use crate::ParseError;

/// MethodDescriptor as described in section 4.3.3 of the [JVM 18 specification](https://docs.oracle.com/javase/specs/jvms/se18/jvms18.pdf)
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct MethodDescriptor {
    pub parameters: Vec<FieldType>,
    pub result: ReturnDescriptor,
}

impl MethodDescriptor {
    pub(crate) fn parse(chars: &mut Chars) -> Result<Self, ParseError> {
        match chars.next() {
            Some('(') => (),
            Some(c) => fail!("Invalid start of method descriptor {}", c),
            None => fail!("Invalid start of method descriptor, missing ("),
        };

        let mut parameters: Vec<FieldType> = Vec::new();

        'done: loop {
            let field = match chars.as_str().chars().next() {
                Some(')') => break 'done,
                Some(_) => FieldType::parse(chars)?,
                None => fail!("Invalid method descriptor, missing end )"),
            };

            parameters.push(field);
        }

        // consume the final ')'
        chars.next();
        let result = ReturnDescriptor::parse(chars)?;

        Ok(MethodDescriptor { parameters, result })
    }
}

impl fmt::Display for MethodDescriptor {
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
pub enum FieldType {
    Base(BaseType),
    Object(Cow<'static, str>),
    Array(Box<FieldType>),
}

impl FieldType {
    pub(crate) fn parse(chars: &mut Chars) -> Result<Self, ParseError> {
        let field = match chars.next() {
            Some('L') => Self::Object(parse_object(chars)?),
            Some('[') => Self::Array(Box::new(FieldType::parse(chars)?)),
            Some(ch) => Self::Base(BaseType::parse(ch)?),
            None => fail!("Invalid FieldType"),
        };

        Ok(field)
    }
}

impl fmt::Display for FieldType {
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
pub enum ReturnDescriptor {
    Return(FieldType),
    Void,
}

impl ReturnDescriptor {
    fn parse(chars: &mut Chars) -> Result<Self, ParseError> {
        let result = match chars.as_str().chars().next() {
            Some('V') => Self::Void,
            Some(_) => Self::Return(FieldType::parse(chars)?),
            None => fail!("Invalid return descriptor, missing value"),
        };

        Ok(result)
    }
}

impl fmt::Display for ReturnDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::Void => f.write_char('V'),
            Self::Return(field) => write!(f, "{}", field),
        }
    }
}

/// Parses the object less the beginning L, e.g. this expects `java/lang/Object;`
fn parse_object(chars: &mut Chars) -> Result<Cow<'static, str>, ParseError> {
    if !chars.clone().any(|ch| ch == ';') {
        fail!("Invalid object descriptor, expected ;");
    }

    let object: String = chars.by_ref().take_while(|ch| *ch != ';').collect();
    Ok(Cow::Owned(object))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_void_void() {
        let string = "()V";
        let mut chars = string.chars();

        let descriptor = MethodDescriptor::parse(&mut chars).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let result = descriptor.result;

        assert!(parameters.next().is_none());
        assert_eq!(result, ReturnDescriptor::Void);
    }

    #[test]
    fn test_single_param() {
        let string = "(J)V";
        let mut chars = string.chars();

        let descriptor = MethodDescriptor::parse(&mut chars).unwrap();
        let mut parameters = descriptor.parameters.into_iter();
        let result = descriptor.result;

        assert_eq!(parameters.next().unwrap(), FieldType::Base(BaseType::LONG));
        assert!(parameters.next().is_none());
        assert_eq!(result, ReturnDescriptor::Void);
    }

    #[test]
    fn test_basetype_return() {
        let string = "()J";
        let mut chars = string.chars();

        let descriptor = MethodDescriptor::parse(&mut chars).unwrap();
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
        let string = "(BCDFIJSZ)V";
        let mut chars = string.chars();

        let descriptor = MethodDescriptor::parse(&mut chars).unwrap();
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
        let string = "(Ljava/lang/Object;)V";
        let mut chars = string.chars();

        let descriptor = MethodDescriptor::parse(&mut chars).unwrap();
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
        let string = "(Ljava/lang/Object;Ljava/lang/String;)V";
        let mut chars = string.chars();

        let descriptor = MethodDescriptor::parse(&mut chars).unwrap();
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
        let string = "()Ljava/lang/Object;";
        let mut chars = string.chars();

        let descriptor = MethodDescriptor::parse(&mut chars).unwrap();
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
        let string = "([J)V";
        let mut chars = string.chars();

        let descriptor = MethodDescriptor::parse(&mut chars).unwrap();
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
        let string = "([[J)V";
        let mut chars = string.chars();

        let descriptor = MethodDescriptor::parse(&mut chars).unwrap();
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
        let string = "()[J";
        let mut chars = string.chars();

        let descriptor = MethodDescriptor::parse(&mut chars).unwrap();
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
