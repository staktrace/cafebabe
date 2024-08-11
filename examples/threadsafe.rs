use rayon::prelude::*;
use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    let mut class_data = Vec::new();
    for arg in env::args().skip(1) {
        let mut file = File::open(&arg).unwrap();
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes).unwrap();
        class_data.push(bytes);
    }

    let results: Vec<Result<_, _>> = class_data
        .par_iter()
        .map(|data| {
            cafebabe::parse_class_with_options(
                &data,
                cafebabe::ParseOptions::default().parse_bytecode(true),
            )
        })
        .collect();

    for result in results {
        match result {
            Ok(class) => println!("Successfully parsed {:?}\n{:#?}", class.this_class, class),
            Err(e) => println!("Error: {}", e),
        }
    }
}
