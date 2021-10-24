use super::*;
use ConstantPoolEntry::*;

macro_rules! assert_validate_passes {
    ($entry:expr) => {
        assert_validate_passes!(0, $entry);
    };
    ($version:expr, $entry:expr) => {
        assert_eq!($entry.validate($version), Ok(()));
    };
}

macro_rules! assert_validate_fails {
    ($entry:expr, $message:literal) => {
        assert_validate_fails!(0, $entry, $message);
    };
    ($version:expr, $entry:expr, $message:literal) => {
        assert_eq!(
            $entry.validate($version),
            Err(ParseError::new($message.to_string()))
        );
    };
}

// Helper for creating the smart pointer types (RefCell, ConstantPoolRef,
// Rc) required to nest ConstantPoolEntry instances.
fn wrap(entry: ConstantPoolEntry) -> RefCell<ConstantPoolRef> {
    RefCell::new(ConstantPoolRef::Resolved(Rc::new(entry)))
}

#[test]
fn test_validate_trivial() {
    assert_validate_passes!(Zero);
    assert_validate_passes!(Utf8(Cow::from("some UTF-8")));
    assert_validate_passes!(Utf8Bytes(&[]));
    assert_validate_passes!(Integer(1));
    assert_validate_passes!(Float(2.0));
    assert_validate_passes!(Long(3));
    assert_validate_passes!(Double(4.0));
    assert_validate_passes!(Unused);
}

#[test]
fn test_validate_class_info() {
    assert_validate_passes!(ClassInfo(wrap(Utf8(Cow::from("some/package/Class")))));
    assert_validate_passes!(ClassInfo(wrap(Utf8(Cow::from("[Lsome/package/Class;")))));

    assert_validate_fails!(ClassInfo(wrap(Utf8(Cow::from("")))), "Invalid binary name");
    assert_validate_fails!(
        ClassInfo(wrap(Utf8Bytes(&[]))),
        "Attempting to get utf-8 data from non-utf8 constant pool entry!"
    );
    assert_validate_fails!(
        ClassInfo(wrap(Zero)),
        "Unexpected constant pool reference type"
    );
}

#[test]
fn test_validate_string() {
    assert_validate_passes!(String(wrap(Utf8(Cow::from("some UTF-8")))));
    assert_validate_passes!(String(wrap(Utf8Bytes(&[]))));

    assert_validate_fails!(
        String(wrap(Zero)),
        "Unexpected constant pool reference type"
    );
}

#[test]
fn test_validate_field_ref() {
    assert_validate_passes!(FieldRef(
        wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
        wrap(NameAndType(
            wrap(Utf8(Cow::from("someField"))),
            wrap(Utf8(Cow::from("I"))),
        )),
    ));

    assert_validate_fails!(
        FieldRef(
            wrap(Zero),
            wrap(NameAndType(
                wrap(Utf8(Cow::from("someField"))),
                wrap(Utf8(Cow::from("I"))),
            )),
        ),
        "Unexpected constant pool reference type"
    );
    assert_validate_fails!(
        FieldRef(
            wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
            wrap(Zero),
        ),
        "Unexpected constant pool reference type"
    );
    assert_validate_fails!(
        FieldRef(
            wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
            wrap(NameAndType(
                wrap(Utf8(Cow::from("someField"))),
                wrap(Utf8(Cow::from(""))),
            )),
        ),
        "Invalid field descriptor"
    );
    assert_validate_fails!(
        FieldRef(
            wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
            wrap(NameAndType(
                wrap(Utf8(Cow::from("someField"))),
                wrap(Utf8Bytes(&[])),
            )),
        ),
        "Attempting to get utf-8 data from non-utf8 constant pool entry!"
    );
    assert_validate_fails!(
        FieldRef(
            wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
            wrap(NameAndType(wrap(Utf8(Cow::from("someField"))), wrap(Zero))),
        ),
        "Unexpected constant pool reference type"
    );
}

#[test]
fn test_validate_method_ref() {
    assert_validate_passes!(MethodRef(
        wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
        wrap(NameAndType(
            wrap(Utf8(Cow::from("someMethod"))),
            wrap(Utf8(Cow::from("()V"))),
        )),
    ));

    assert_validate_fails!(
        MethodRef(
            wrap(Zero),
            wrap(NameAndType(
                wrap(Utf8(Cow::from("someMethod"))),
                wrap(Utf8(Cow::from("()V"))),
            )),
        ),
        "Unexpected constant pool reference type"
    );
    assert_validate_fails!(
        MethodRef(
            wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
            wrap(Zero),
        ),
        "Unexpected constant pool reference type"
    );
    assert_validate_fails!(
        MethodRef(
            wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
            wrap(NameAndType(
                wrap(Utf8(Cow::from("someMethod"))),
                wrap(Utf8(Cow::from(""))),
            )),
        ),
        "Invalid method descriptor"
    );
    assert_validate_fails!(
        MethodRef(
            wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
            wrap(NameAndType(
                wrap(Utf8(Cow::from("someMethod"))),
                wrap(Utf8Bytes(&[])),
            )),
        ),
        "Attempting to get utf-8 data from non-utf8 constant pool entry!"
    );
    assert_validate_fails!(
        MethodRef(
            wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
            wrap(NameAndType(wrap(Utf8(Cow::from("someMethod"))), wrap(Zero))),
        ),
        "Unexpected constant pool reference type"
    );
}

#[test]
fn test_validate_interface_method_ref() {
    assert_validate_passes!(InterfaceMethodRef(
        wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
        wrap(NameAndType(
            wrap(Utf8(Cow::from("someMethod"))),
            wrap(Utf8(Cow::from("()V"))),
        )),
    ));

    assert_validate_fails!(
        InterfaceMethodRef(
            wrap(Zero),
            wrap(NameAndType(
                wrap(Utf8(Cow::from("someMethod"))),
                wrap(Utf8(Cow::from("()V"))),
            )),
        ),
        "Unexpected constant pool reference type"
    );
    assert_validate_fails!(
        InterfaceMethodRef(
            wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
            wrap(Zero),
        ),
        "Unexpected constant pool reference type"
    );
    assert_validate_fails!(
        InterfaceMethodRef(
            wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
            wrap(NameAndType(
                wrap(Utf8(Cow::from("someMethod"))),
                wrap(Utf8(Cow::from(""))),
            )),
        ),
        "Invalid method descriptor"
    );
    assert_validate_fails!(
        InterfaceMethodRef(
            wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
            wrap(NameAndType(
                wrap(Utf8(Cow::from("someMethod"))),
                wrap(Utf8Bytes(&[])),
            )),
        ),
        "Attempting to get utf-8 data from non-utf8 constant pool entry!"
    );
    assert_validate_fails!(
        InterfaceMethodRef(
            wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
            wrap(NameAndType(wrap(Utf8(Cow::from("someMethod"))), wrap(Zero))),
        ),
        "Unexpected constant pool reference type"
    );
}

#[test]
fn test_validate_name_and_type() {
    assert_validate_passes!(NameAndType(
        wrap(Utf8(Cow::from("someUnqualifiedName"))),
        wrap(Utf8(Cow::from("anything goes"))),
    ));
    assert_validate_passes!(NameAndType(
        wrap(Utf8(Cow::from("someUnqualifiedName"))),
        wrap(Utf8Bytes(&[])),
    ));

    assert_validate_fails!(
        NameAndType(
            wrap(Utf8(Cow::from(""))),
            wrap(Utf8(Cow::from("anything goes"))),
        ),
        "Invalid unqualified name"
    );
    assert_validate_fails!(
        NameAndType(wrap(Zero), wrap(Utf8(Cow::from("anything goes"))),),
        "Unexpected constant pool reference type"
    );
    assert_validate_fails!(
        NameAndType(wrap(Utf8Bytes(&[])), wrap(Utf8(Cow::from("anything goes"))),),
        "Attempting to get utf-8 data from non-utf8 constant pool entry!"
    );
    assert_validate_fails!(
        NameAndType(wrap(Utf8(Cow::from("someUnqualifiedName"))), wrap(Zero)),
        "Unexpected constant pool reference type"
    );
}

#[test]
fn test_validate_method_handle() {
    use ReferenceKind::*;

    for kind in [GetField, GetStatic, PutField, PutStatic] {
        assert_validate_passes!(MethodHandle(
            kind,
            wrap(FieldRef(
                wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
                wrap(NameAndType(
                    wrap(Utf8(Cow::from("someField"))),
                    wrap(Utf8(Cow::from("I"))),
                )),
            ))
        ));

        assert_validate_fails!(
            MethodHandle(kind, wrap(Zero)),
            "Unexpected constant pool reference type"
        );
    }

    for kind in [InvokeVirtual, NewInvokeSpecial] {
        assert_validate_passes!(MethodHandle(
            kind,
            wrap(MethodRef(
                wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
                wrap(NameAndType(
                    wrap(Utf8(Cow::from("someMethod"))),
                    wrap(Utf8(Cow::from("()V"))),
                )),
            ))
        ));

        assert_validate_fails!(
            MethodHandle(kind, wrap(Zero)),
            "Unexpected constant pool reference type"
        );
    }

    for kind in [InvokeStatic, InvokeSpecial] {
        assert_validate_passes!(MethodHandle(
            kind,
            wrap(MethodRef(
                wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
                wrap(NameAndType(
                    wrap(Utf8(Cow::from("someMethod"))),
                    wrap(Utf8(Cow::from("()V"))),
                )),
            ))
        ));
        assert_validate_passes!(
            52, // Major version
            MethodHandle(
                kind,
                wrap(InterfaceMethodRef(
                    wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
                    wrap(NameAndType(
                        wrap(Utf8(Cow::from("someMethod"))),
                        wrap(Utf8(Cow::from("()V"))),
                    )),
                ))
            )
        );

        assert_validate_fails!(
            51, // Major version
            MethodHandle(
                kind,
                wrap(InterfaceMethodRef(
                    wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
                    wrap(NameAndType(
                        wrap(Utf8(Cow::from("someMethod"))),
                        wrap(Utf8(Cow::from("()V"))),
                    )),
                ))
            ),
            "Unexpected constant pool reference type"
        );
        assert_validate_fails!(
            MethodHandle(kind, wrap(Zero)),
            "Unexpected constant pool reference type"
        );
    }

    for kind in [InvokeInterface] {
        assert_validate_passes!(MethodHandle(
            kind,
            wrap(InterfaceMethodRef(
                wrap(ClassInfo(wrap(Utf8(Cow::from("some/package/Class"))))),
                wrap(NameAndType(
                    wrap(Utf8(Cow::from("someMethod"))),
                    wrap(Utf8(Cow::from("()V"))),
                )),
            ))
        ));

        assert_validate_fails!(
            MethodHandle(kind, wrap(Zero)),
            "Unexpected constant pool reference type"
        );
    }
}

#[test]
fn test_validate_method_type() {
    assert_validate_passes!(MethodType(wrap(Utf8(Cow::from("()V")))));

    assert_validate_fails!(
        MethodType(wrap(Utf8(Cow::from("")))),
        "Invalid method descriptor"
    );
    assert_validate_fails!(
        MethodType(wrap(Utf8Bytes(&[]))),
        "Attempting to get utf-8 data from non-utf8 constant pool entry!"
    );
    assert_validate_fails!(
        MethodType(wrap(Zero)),
        "Unexpected constant pool reference type"
    );
}

#[test]
fn test_validate_dynamic() {
    assert_validate_passes!(Dynamic(
        0,
        wrap(NameAndType(
            wrap(Utf8(Cow::from("someField"))),
            wrap(Utf8(Cow::from("I"))),
        )),
    ));
    assert_validate_passes!(Dynamic(
        0,
        wrap(NameAndType(
            wrap(Utf8(Cow::from(""))),
            wrap(Utf8(Cow::from("I"))),
        )),
    ));
    assert_validate_passes!(Dynamic(
        0,
        wrap(NameAndType(
            wrap(Utf8Bytes(&[])),
            wrap(Utf8(Cow::from("I"))),
        )),
    ));

    assert_validate_fails!(
        Dynamic(0, wrap(Zero)),
        "Unexpected constant pool reference type"
    );
    assert_validate_fails!(
        Dynamic(
            0,
            wrap(NameAndType(
                wrap(Utf8(Cow::from("someField"))),
                wrap(Utf8(Cow::from(""))),
            )),
        ),
        "Invalid field descriptor"
    );
    assert_validate_fails!(
        Dynamic(
            0,
            wrap(NameAndType(
                wrap(Utf8(Cow::from("someField"))),
                wrap(Utf8Bytes(&[]))
            )),
        ),
        "Attempting to get utf-8 data from non-utf8 constant pool entry!"
    );
}

#[test]
fn test_validate_invoke_dynamic() {
    assert_validate_passes!(InvokeDynamic(
        0,
        wrap(NameAndType(
            wrap(Utf8(Cow::from("someMethod"))),
            wrap(Utf8(Cow::from("()V"))),
        )),
    ));
    assert_validate_passes!(InvokeDynamic(
        0,
        wrap(NameAndType(
            wrap(Utf8(Cow::from(""))),
            wrap(Utf8(Cow::from("()V"))),
        )),
    ));
    assert_validate_passes!(InvokeDynamic(
        0,
        wrap(NameAndType(
            wrap(Utf8Bytes(&[])),
            wrap(Utf8(Cow::from("()V"))),
        )),
    ));

    assert_validate_fails!(
        InvokeDynamic(0, wrap(Zero)),
        "Unexpected constant pool reference type"
    );
    assert_validate_fails!(
        InvokeDynamic(
            0,
            wrap(NameAndType(
                wrap(Utf8(Cow::from("someMethod"))),
                wrap(Utf8(Cow::from(""))),
            )),
        ),
        "Invalid method descriptor"
    );
    assert_validate_fails!(
        InvokeDynamic(
            0,
            wrap(NameAndType(
                wrap(Utf8(Cow::from("someMethod"))),
                wrap(Utf8Bytes(&[]))
            )),
        ),
        "Attempting to get utf-8 data from non-utf8 constant pool entry!"
    );
}

#[test]
fn test_validate_module_info() {
    assert_validate_passes!(ModuleInfo(wrap(Utf8(Cow::from("some.module")))));

    assert_validate_fails!(
        ModuleInfo(wrap(Utf8(Cow::from("@")))),
        "Invalid module name"
    );
    assert_validate_fails!(
        ModuleInfo(wrap(Utf8Bytes(&[]))),
        "Attempting to get utf-8 data from non-utf8 constant pool entry!"
    );
    assert_validate_fails!(
        ModuleInfo(wrap(Zero)),
        "Unexpected constant pool reference type"
    );
}

#[test]
fn test_validate_invoke_package_info() {
    assert_validate_passes!(PackageInfo(wrap(Utf8(Cow::from("some/package")))));

    assert_validate_fails!(
        PackageInfo(wrap(Utf8(Cow::from("")))),
        "Invalid binary name"
    );
    assert_validate_fails!(
        PackageInfo(wrap(Utf8Bytes(&[]))),
        "Attempting to get utf-8 data from non-utf8 constant pool entry!"
    );
    assert_validate_fails!(
        PackageInfo(wrap(Zero)),
        "Unexpected constant pool reference type"
    );
}
