use std::ffi::OsStr;
use std::fs::{read_dir, remove_file, File};
use std::io;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

#[test]
fn parse_success() {
    let classes = generate_classes().unwrap();
    println!("Parsing {} class files...", classes.len());
    for classfile in classes {
        let mut file = File::open(&classfile).unwrap();
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes).unwrap();
        match cafebabe::parse_class(&bytes) {
            Ok(_) => {
                println!("[OK] {:?}", classfile);
                remove_file(&classfile).unwrap();
            }
            Err(e) => panic!("[FAIL]: {:?}\n{}", classfile, e),
        };
    }
}

fn generate_classes() -> io::Result<Vec<PathBuf>> {
    let mut base_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    base_dir.push("tests");
    base_dir.push("parse");

    println!("Running gen_classes.sh to generate class files...");
    let mut script = base_dir.clone();
    script.push("gen_classes.sh");
    let status = Command::new(script)
        .current_dir(&base_dir)
        .stdout(Stdio::null())
        .stdin(Stdio::null())
        .status()
        .expect("Failed to execute gen_classes.sh");
    if !status.success() {
        panic!("Exit code {:?} from gen_classes.sh", status.code());
    }

    let mut classes = Vec::new();
    println!("Scanning for class files...");
    find_classes(&base_dir, &mut classes)?;
    println!("Found {} class files.", classes.len());
    Ok(classes)
}

fn find_classes<P: AsRef<Path>>(path: P, dst: &mut Vec<PathBuf>) -> io::Result<()> {
    for dir_entry in read_dir(path)? {
        let entry = dir_entry?;
        let file_type = entry.file_type()?;
        if file_type.is_dir() {
            find_classes(entry.path(), dst)?;
        } else if file_type.is_file() {
            let path = entry.path();
            if path.extension() == Some(OsStr::new("class")) {
                dst.push(path);
            }
        }
    }
    Ok(())
}

#[test]
fn parse_nameandtype_utf8bytes_descriptor_returns_err() {
    // Regression test for https://github.com/staktrace/cafebabe/issues/61
    // A NameAndType whose descriptor is not valid modified UTF-8 (Utf8Bytes),
    // referenced only from EnclosingMethod, must return Err rather than panic.
    let hex = "cafebabe000000340009010001410700010100106a6176612f6c616e672f4f626a65637407000301000f456e636c6f73696e674d6574686f640100016d010001ff0c00060007002100020004000000000000000100050000000400040008";
    let bytes: Vec<u8> = (0..hex.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&hex[i..i + 2], 16).unwrap())
        .collect();
    assert_eq!(bytes.len(), 94);
    assert!(cafebabe::parse_class(&bytes).is_err());
}
