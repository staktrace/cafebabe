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
        Some('<') => return (allow_init && name == "<init>") || (allow_clinit && name == "<clinit>"),
        Some(x) if !is_valid_unqualified_char(x) => return false,
        Some(_) => (),
    };
    while let Some(x) = chars.next() {
        if !is_valid_unqualified_char(x) {
            return false;
        }
    }
    true
}

fn is_valid_unqualified_char(c: char) -> bool {
    match c {
        '.' | ';' | '[' | '/' | '<' | '>' => false,
        _ => true,
    }
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

fn consume_unqualified_segment(chars: &mut Chars) -> Option<char> {
    let mut first = true;
    while let Some(c) = chars.next() {
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

pub(crate) fn is_field_descriptor(name: &str) -> bool {
    if name.len() == 0 {
        return false;
    }
    let mut dimensions = 0;
    let mut chars = name.chars();
    while let Some(c) = chars.next() {
        match c {
            '[' => {
                dimensions += 1;
                if dimensions > 255 {
                    return false;
                }
                continue;
            }
            'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' => (),
            'L' => {
                if !consume_class_descriptor(&mut chars) {
                    return false;
                }
            }
            _ => return false,
        };
        break;
    }
    chars.next().is_none()
}

pub(crate) fn is_array_descriptor(name: &str) -> bool {
    is_field_descriptor(name) && name.as_bytes()[0] == b'['
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_descriptors() {
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
}
