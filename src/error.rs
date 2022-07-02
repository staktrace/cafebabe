use std::fmt;

macro_rules! err {
    ($base:ident, $msg:literal) => {
        ParseError::with_context($base, $msg.to_string())
    };
    ($base:ident, $fmtstr:literal, $($args:tt)*) => {
        ParseError::with_context($base, format!($fmtstr, $($args)*))
    };
    (($msg:literal), ($contextfmt:literal, $($contextargs:tt)*)) => {
        ParseError::with_context(ParseError::new($msg.to_string()), format!($contextfmt, $($contextargs)*))
    };
    (($fmtstr:literal, $($args:tt)*), ($contextfmt:literal, $($contextargs:tt)*)) => {
        ParseError::with_context(ParseError::new(format!($fmtstr, $($args)*)), format!($contextfmt, $($contextargs)*))
    };
    ($msg:literal) => {
        ParseError::new($msg.to_string())
    };
    ($fmtstr:literal, $($args:tt)*) => {
        ParseError::new(format!($fmtstr, $($args)*))
    };
}

macro_rules! fail {
    ($msg:literal) => {
        return Err(ParseError::new($msg.to_string()))
    };
    (($msg:literal), ($context:literal)) => {
        return Err(ParseError::with_context(ParseError::new($msg.to_string()), $context.to_string()))
    };
    ($fmtstr:literal, $($args:tt)*) => {
        return Err(ParseError::new(format!($fmtstr, $($args)*)))
    };
    (($fmtstr:literal, $($args:tt)*), ($context:literal)) => {
        return Err(ParseError::with_context(ParseError::new(format!($fmtstr, $($args)*)), $context.to_string()))
    };
    (($fmtstr:literal, $($args:tt)*), ($contextfmt:literal, $($contextargs:tt)*)) => {
        return Err(ParseError::with_context(ParseError::new(format!($fmtstr, $($args)*)), format!($contextfmt, $($contextargs)*)))
    };
}

#[derive(Debug, PartialEq)]
pub struct ParseError {
    msg: String,
    contexts: Vec<String>,
}

impl ParseError {
    pub(crate) fn new(msg: String) -> Self {
        ParseError {
            msg,
            contexts: Vec::new(),
        }
    }

    pub(crate) fn with_context(base: ParseError, context: String) -> Self {
        let mut contexts = base.contexts;
        contexts.push(context);
        ParseError {
            msg: base.msg,
            contexts,
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.msg)?;
        let mut connector = " for ";
        for context in &self.contexts {
            write!(f, "{}{}", connector, context)?;
            connector = " of ";
        }
        Ok(())
    }
}

impl std::error::Error for ParseError {}
