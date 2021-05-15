use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    for arg in env::args().skip(1) {
        let mut file = File::open(&arg).unwrap();
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes).unwrap();
        println!("Parsing {:?}...", arg);
        match cafebabe::parse_class(&bytes) {
            Ok(class) => println!("Successfully parsed {:?}", class.this_class),
            Err(e) => println!("Error: {}", e),
        };
    }
}
