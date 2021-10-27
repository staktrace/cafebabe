use std::str::Chars;

pub(crate) fn is_binary_name(name: &str) -> bool {
    for segment in name.split('/') {
        if !is_unqualified_name(segment, false, false) {
            return false;
        }
    }
    true
}

pub(crate) fn is_unqualified_name(name: &str, allow_init: bool, allow_clinit: bool) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        None => return false,
        Some('<') => {
            return (allow_init && name == "<init>") || (allow_clinit && name == "<clinit>")
        }
        Some(x) if !is_valid_unqualified_char(x) => return false,
        Some(_) => (),
    };
    chars.all(is_valid_unqualified_char)
}

fn is_valid_unqualified_char(c: char) -> bool {
    !matches!(c, '.' | ';' | '[' | '/' | '<' | '>')
}

pub(crate) fn is_module_name(name: &str) -> bool {
    let mut chars = name.chars();
    while let Some(c) = chars.next() {
        match c {
            '\0'..='\x1F' => return false,
            '\\' => match chars.next() {
                None => return false,
                Some('\\') | Some(':') | Some('@') => (),
                Some(_) => return false,
            },
            ':' | '@' => return false,
            _ => (),
        };
    }
    true
}

/// Returns None if there was a parse error.
/// Returns Some('/') for an unqualified segment followed by /
/// Returns Some(';') for an unqualified segment followed by ;
fn consume_unqualified_segment(chars: &mut Chars) -> Option<char> {
    let mut first = true;
    for c in chars {
        match c {
            '/' if first => return None,
            ';' if first => return None,
            '/' | ';' => return Some(c),
            '.' | '[' | '<' | '>' => return None,
            _ => first = false,
        };
    }
    None
}

/// Returns false if there was a parse error.
/// Returns true if exactly one class descriptor (including terminating semicolon) was consumed.
fn consume_class_descriptor(chars: &mut Chars) -> bool {
    loop {
        match consume_unqualified_segment(chars) {
            None => return false,
            Some(';') => return true,
            Some('/') => continue,
            _ => panic!("Got unexpected return value from consume_unqualified_segment"),
        };
    }
}

/// Returns (false, None) if there was a parse error.
/// Returns (false, Some(x)) if a field descriptor was not read, but a character x was consumed.
/// Returns (true, None) after consuming exactly one field descriptor.
fn consume_field_descriptor(chars: &mut Chars) -> (bool, Option<char>) {
    let mut dimensions = 0;
    while let Some(c) = chars.next() {
        match c {
            '[' => {
                dimensions += 1;
                if dimensions > 255 {
                    return (false, None);
                }
                continue;
            }
            'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' => (),
            'L' => {
                if !consume_class_descriptor(chars) {
                    return (false, None);
                }
            }
            _ => return (false, Some(c)),
        };
        return (true, None);
    }
    (false, None)
}

pub(crate) fn is_field_descriptor(name: &str) -> bool {
    let mut chars = name.chars();
    match consume_field_descriptor(&mut chars) {
        (false, _) => false,
        (true, _) => chars.next().is_none(),
    }
}

pub(crate) fn is_array_descriptor(name: &str) -> bool {
    is_field_descriptor(name) && name.as_bytes()[0] == b'['
}

pub(crate) fn is_method_descriptor(name: &str) -> bool {
    let mut chars = name.chars();
    if chars.next() != Some('(') {
        return false;
    }
    loop {
        match consume_field_descriptor(&mut chars) {
            (false, Some(')')) => break,
            (false, _) => return false,
            (true, _) => continue,
        }
    }
    // ')' was already consumed, so now consume the return descriptor
    match consume_field_descriptor(&mut chars) {
        (false, Some('V')) => chars.next().is_none(),
        (false, _) => false,
        (true, _) => chars.next().is_none(),
    }
}

pub(crate) fn is_return_descriptor(name: &str) -> bool {
    let mut chars = name.chars();
    match consume_field_descriptor(&mut chars) {
        (false, Some('V')) => chars.next().is_none(),
        (false, _) => false,
        (true, _) => chars.next().is_none(),
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
}
