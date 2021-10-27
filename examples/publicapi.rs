use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    for arg in env::args().skip(1) {
        let mut file = File::open(&arg).unwrap();
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes).unwrap();
        match cafebabe::parse_class(&bytes) {
            Ok(class) => {
                println!("{}", class.this_class);
                for field in class.fields {
                    if field
                        .access_flags
                        .contains(cafebabe::FieldAccessFlags::PRIVATE)
                    {
                        continue;
                    }
                    println!("    {} {}", field.name, field.descriptor);
                }
                for method in class.methods {
                    if method
                        .access_flags
                        .contains(cafebabe::MethodAccessFlags::PRIVATE)
                    {
                        continue;
                    }
                    println!("    {} {}", method.name, method.descriptor);
                }
            }
            Err(e) => eprintln!("Error: {} when parsing {:?}", e, arg),
        };
    }
}
