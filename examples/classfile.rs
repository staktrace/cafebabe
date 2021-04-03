use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    for arg in env::args().skip(1) {
        let mut file = File::open(&arg).unwrap();
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes).unwrap();
        println!("Dumping {:?}", arg);
        let class = cafebabe::parse_class(&bytes).unwrap();
        println!("{:?}", class);
    }
}
