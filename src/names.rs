pub(crate) fn is_binary_name(name: &str) -> bool {
    for segment in name.split('/') {
        if !is_unqualified_name(segment) {
            return false;
        }
    }
    true
}

pub(crate) fn is_unqualified_name(name: &str) -> bool {
    !name.is_empty() && name.chars().all(is_valid_unqualified_char)
}

fn is_valid_unqualified_char(c: char) -> bool {
    !matches!(c, '.' | ';' | '[' | '/')
}

pub(crate) fn is_unqualified_method_name(name: &str, allow_init: bool, allow_clinit: bool) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        None => return false,
        Some('<') => {
            return (allow_init && name == "<init>") || (allow_clinit && name == "<clinit>")
        }
        Some(x) if !is_valid_unqualified_method_char(x) => return false,
        Some(_) => (),
    };
    chars.all(is_valid_unqualified_method_char)
}

fn is_valid_unqualified_method_char(c: char) -> bool {
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
