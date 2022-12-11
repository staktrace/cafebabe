use std::fs::File;
use std::io::Read;

use cafebabe::constant_pool::*;

#[test]
fn iterator_basic() {
    let mut file = File::open("tests/iterator/Object.class").unwrap();
    let mut bytes = Vec::new();
    file.read_to_end(&mut bytes).unwrap();
    let class = cafebabe::parse_class(&bytes).unwrap();
    let mut iter = class.constantpool_iter();

    // javap -v output for this class shows:
    //   #1 = Class              #2             // java/lang/StringBuilder
    //   #2 = Utf8               java/lang/StringBuilder
    //   #3 = Methodref          #1.#4          // java/lang/StringBuilder."<init>":()V
    //   #4 = NameAndType        #5:#6          // "<init>":()V
    //   #5 = Utf8               <init>
    //   #6 = Utf8               ()V
    //    ... (more entries omitted) ...
    //  #84 = Class              #85            // java/lang/Throwable
    //  #85 = Utf8               java/lang/Throwable
    //  #86 = Utf8               Deprecated
    //  #87 = Utf8               Ljava/lang/Deprecated;
    //  #88 = Utf8               since
    //  #89 = Utf8               9
    //  #90 = Utf8               SourceFile
    //  #91 = Utf8               Object.java

    assert!(match iter.next() {
        Some(ConstantPoolItem::ClassInfo(x)) => x == "java/lang/StringBuilder",
        _ => false,
    });
    assert!(match iter.next() {
        Some(ConstantPoolItem::MethodRef(MemberRef {
            class_name: x,
            name_and_type:
                NameAndType {
                    name: y,
                    descriptor: z,
                },
        })) => x == "java/lang/StringBuilder" && y == "<init>" && z == "()V",
        _ => false,
    });
    let mut iter = iter.skip(29);
    assert!(match iter.next() {
        Some(ConstantPoolItem::ClassInfo(x)) => x == "java/lang/Throwable",
        _ => false,
    });
    assert!(iter.next().is_none());
}
