use std::env;
use std::fs::File;
use std::io::Read;

use cafebabe::constant_pool::ConstantPoolItem::ClassInfo;
use cafebabe::constant_pool::ConstantPoolItem::FieldRef;
use cafebabe::constant_pool::ConstantPoolItem::InterfaceMethodRef;
use cafebabe::constant_pool::ConstantPoolItem::MethodRef;

fn main() {
    for arg in env::args().skip(1) {
        let mut file = File::open(&arg).unwrap();
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes).unwrap();
        match cafebabe::parse_class(&bytes) {
            Ok(class) => {
                for cp in class.constantpool_iter() {
                    match cp {
                        ClassInfo(c) => println!("{}", c),
                        FieldRef(r) | MethodRef(r) | InterfaceMethodRef(r) => {
                            println!("{}", r.class_name)
                        }
                        _ => (),
                    }
                }
            }
            Err(e) => eprintln!("Error: {} when parsing {:?}", e, arg),
        };
    }
}
