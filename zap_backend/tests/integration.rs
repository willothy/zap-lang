use std::path::PathBuf;

use tir::{Expression::*, Function::*, Literal::*, Statement::*};
use zap_backend::ty::Type::*;
use zap_backend::{ops, tir};
use zap_backend::{
    ops::{BinaryOperator, UnaryOperator},
    ty::PrimitiveType,
};

macro_rules! function {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        &name[..name.len() - 3]
    }};
}

#[cfg(test)]
fn runner(
    name: &str,
    unit: zap_backend::tir::Unit,
    expected_out: &str,
    expected_ret: i32,
    args: Vec<&str>,
    tmpdir: PathBuf,
) {
    let mut unit = unit;
    let tmpdir = tmpdir.join(name.trim());
    unit.path = tmpdir.join("test.asm");
    std::fs::create_dir_all(unit.path.parent().unwrap()).unwrap();
    println!("path: {:?}", unit.path);
    let mut gen = zap_backend::codegen::Generator::new(unit);
    gen.generate();
    let _output = gen.builder.finalize();
    gen.builder.write();

    let output = std::process::Command::new("nasm")
        .arg("-felf64")
        .arg(tmpdir.join("test.asm"))
        .arg("-o")
        .arg(tmpdir.join("test.o"))
        .output()
        .expect("failed to execute process");

    assert!(
        output.status.success(),
        "Failed to run nasm: {}",
        String::from_utf8(output.stderr).unwrap()
    );

    let output = std::process::Command::new("ld")
        .arg(tmpdir.join("test.o"))
        .arg("-o")
        .arg(tmpdir.join("test"))
        .output()
        .expect("failed to execute process");

    assert!(output.status.success(), "Failed to run ld");

    let mut output = std::process::Command::new(tmpdir.join("test"));
    let output = output
        .args(args)
        .output()
        .expect("failed to execute process");
    assert!(
        output.status.code() == Some(expected_ret as i32),
        "Program exited with incorrect code {}, expected {}",
        output.status.code().unwrap(),
        expected_ret
    );
    assert!(
        String::from_utf8(output.stdout.clone()).unwrap() == expected_out,
        "Program output incorrect, expected {}, got {}",
        expected_out,
        String::from_utf8(output.stdout).unwrap()
    );
}

#[test]
fn if_statements() {
    let unit = tir::Unit {
        name: "test".to_string(),
        path: PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap()).join("test.asm"),
        imports: Vec::new(),
        structs: Vec::new(),
        consts: Vec::new(),
        functions: vec![
            // make this on a new line! c'mon rustfmt
            Definition {
                name: "main".to_owned(),
                return_type: Primitive(PrimitiveType::I32),
                params: Vec::new(),
                body: vec![
                    VariableDeclaration {
                        name: "a".to_owned(),
                        ty: Primitive(PrimitiveType::I32),
                        initializer: Some(Literal {
                            value: Int(15),
                            ty: Primitive(PrimitiveType::I32),
                        }),
                    },
                    VariableDeclaration {
                        name: "b".to_owned(),
                        ty: Primitive(PrimitiveType::I32),
                        initializer: Some(Literal {
                            value: Int(39),
                            ty: Primitive(PrimitiveType::I32),
                        }),
                    },
                    VariableAssignment {
                        name: "a".to_owned(),

                        op: ops::AssignmentOperator::Assign,
                        value: Literal {
                            value: Int(25),
                            ty: Primitive(PrimitiveType::I32),
                        },
                    },
                    If {
                        condition: Binary {
                            lhs: Box::new(Variable {
                                name: "a".to_owned(),
                                ty: Primitive(PrimitiveType::I32),
                            }),
                            rhs: Box::new(Variable {
                                name: "b".to_owned(),
                                ty: Primitive(PrimitiveType::I32),
                            }),
                            op: ops::BinaryOperator::Lt,
                            ty: Primitive(PrimitiveType::Bool),
                        },
                        then_body: vec![Return(Some(Variable {
                            name: "a".to_owned(),
                            ty: Primitive(PrimitiveType::I32),
                        }))],
                        else_body: vec![Return(Some(Variable {
                            name: "b".to_owned(),
                            ty: Primitive(PrimitiveType::I32),
                        }))],
                    },
                ],
            },
        ],
    };
    runner(
        function!(),
        unit,
        "",
        25,
        Vec::new(),
        PathBuf::from(env!("CARGO_TARGET_TMPDIR")),
    );
}

#[test]
fn call_and_args() {
    let unit = tir::Unit {
        name: "test".to_string(),
        path: PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap()).join("test.asm"),
        imports: Vec::new(),
        structs: Vec::new(),
        consts: Vec::new(),
        functions: vec![
            Definition {
                name: "main".to_owned(),
                return_type: Primitive(PrimitiveType::I32),
                params: Vec::new(),
                body: vec![Return(Some(Call {
                    name: "test".to_owned(),
                    args: vec![
                        Literal {
                            value: Int(1),
                            ty: Primitive(PrimitiveType::I32),
                        },
                        Literal {
                            value: Int(2),
                            ty: Primitive(PrimitiveType::I32),
                        },
                        Literal {
                            value: Int(3),
                            ty: Primitive(PrimitiveType::I32),
                        },
                        Literal {
                            value: Int(4),
                            ty: Primitive(PrimitiveType::I32),
                        },
                        Literal {
                            value: Int(5),
                            ty: Primitive(PrimitiveType::I32),
                        },
                        Literal {
                            value: Int(6),
                            ty: Primitive(PrimitiveType::I32),
                        },
                        Literal {
                            value: Int(34),
                            ty: Primitive(PrimitiveType::I32),
                        },
                        Literal {
                            value: Int(35),
                            ty: Primitive(PrimitiveType::I32),
                        },
                    ],
                    ty: Primitive(PrimitiveType::I32),
                }))],
            },
            Definition {
                name: "test".to_owned(),
                return_type: Primitive(PrimitiveType::I32),
                params: vec![
                    (String::from("a"), Primitive(PrimitiveType::I32)),
                    (String::from("b"), Primitive(PrimitiveType::I32)),
                    (String::from("c"), Primitive(PrimitiveType::I32)),
                    (String::from("d"), Primitive(PrimitiveType::I32)),
                    (String::from("e"), Primitive(PrimitiveType::I32)),
                    (String::from("f"), Primitive(PrimitiveType::I32)),
                    (String::from("g"), Primitive(PrimitiveType::I32)),
                    (String::from("h"), Primitive(PrimitiveType::I32)),
                ],
                body: vec![Return(Some(Binary {
                    lhs: Box::new(Variable {
                        name: "h".to_owned(),
                        ty: Primitive(PrimitiveType::I32),
                    }),
                    rhs: Box::new(Variable {
                        name: "g".to_owned(),
                        ty: Primitive(PrimitiveType::I32),
                    }),
                    op: BinaryOperator::Add,
                    ty: Primitive(PrimitiveType::I32),
                }))],
            },
        ],
    };
    runner(
        function!(),
        unit,
        "",
        69,
        vec![],
        PathBuf::from(env!("CARGO_TARGET_TMPDIR")),
    );
}

#[test]
fn ref_and_deref() {
    let unit = tir::Unit {
        name: "test".to_string(),
        path: PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap()).join("test.asm"),
        imports: Vec::new(),
        consts: Vec::new(),
        structs: Vec::new(),
        functions: vec![Definition {
            name: "main".to_owned(),
            return_type: Primitive(PrimitiveType::I32),
            params: Vec::new(),
            body: vec![
                // MAKE IT MULTIPLE LINES!!
                VariableDeclaration {
                    name: "a".to_owned(),
                    ty: Primitive(PrimitiveType::I32),
                    initializer: Some(Literal {
                        value: Int(0),
                        ty: Primitive(PrimitiveType::I32),
                    }),
                },
                VariableDeclaration {
                    name: "b".to_owned(),
                    ty: Pointer(Box::new(Primitive(PrimitiveType::I32))),
                    initializer: Some(Unary {
                        expr: Box::new(Variable {
                            name: "a".to_owned(),
                            ty: Primitive(PrimitiveType::I32),
                        }),
                        op: UnaryOperator::Ref,
                        ty: Pointer(Box::new(Primitive(PrimitiveType::I32))),
                    }),
                },
                Return(Some(Unary {
                    expr: Box::new(Variable {
                        name: "b".to_owned(),
                        ty: Pointer(Box::new(Primitive(PrimitiveType::I32))),
                    }),
                    op: UnaryOperator::Deref,
                    ty: Primitive(PrimitiveType::I32),
                })),
            ],
        }],
    };
    runner(
        function!(),
        unit,
        "",
        0,
        Vec::new(),
        PathBuf::from(env!("CARGO_TARGET_TMPDIR")),
    );
}

#[test]
fn cli_args_and_asm() {
    let unit = tir::Unit {
        name: "test".to_string(),
        path: PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap()).join("test.asm"),
        imports: Vec::new(),
        consts: Vec::new(),
        structs: Vec::new(),
        functions: vec![Definition {
            name: "main".to_owned(),
            return_type: Primitive(PrimitiveType::I32),
            params: Vec::new(),
            body: vec![
                // MAKE IT MULTIPLE LINES!!
                Expression(InlineAssembly {
                    code: vec![
                        // write to stdout
                        "mov rax, 1",
                        "mov rdi, 1",
                        "mov rsi, qword [rsp + 32]",
                        "mov rdx, 4",
                        "syscall",
                    ]
                    .iter()
                    .map(|s| s.trim().to_string())
                    .collect(),
                }),
                Return(Some(Literal {
                    value: Int(0),
                    ty: Primitive(PrimitiveType::I32),
                })),
            ],
        }],
    };
    runner(
        function!(),
        unit,
        "test",
        0,
        vec!["test"],
        PathBuf::from(env!("CARGO_TARGET_TMPDIR")),
    );
}

#[test]
fn str_literal_and_syscall() {
    let unit = tir::Unit {
        name: "test".to_string(),
        path: PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap()).join("test.asm"),
        imports: Vec::new(),
        structs: Vec::new(),
        consts: Vec::new(),
        functions: vec![
            // Multiple lines!
            Definition {
                name: "main".to_owned(),
                return_type: Primitive(PrimitiveType::I32),
                params: Vec::new(),
                body: vec![
                    // MAKE IT MULTIPLE LINES!!
                    Return(Some(Call {
                        name: "write".to_owned(),
                        args: vec![
                            Literal {
                                value: Str("Hello, world!\n".to_owned()),
                                ty: Pointer(Box::new(Primitive(PrimitiveType::Char))),
                            },
                            Literal {
                                value: Int(14),
                                ty: Primitive(PrimitiveType::I32),
                            },
                        ],
                        ty: Primitive(PrimitiveType::I32),
                    })),
                ],
            },
            Definition {
                name: "write".to_owned(),
                return_type: Primitive(PrimitiveType::I32),
                params: Vec::new(),
                body: vec![
                    // MAKE IT MULTIPLE LINES!!
                    Return(Some(InlineAssembly {
                        code: vec![
                            // write to stdout
                            "mov rdx, rsi",
                            "mov rsi, rdi", // move the string to rsi
                            "mov rdi, 1",
                            "mov rax, 1",
                            "syscall",
                        ]
                        .iter()
                        .map(|s| s.trim().to_string())
                        .collect(),
                    })),
                ],
            },
        ],
    };
    runner(
        function!(),
        unit,
        "Hello, world!\n",
        14,
        vec![],
        PathBuf::from(env!("CARGO_TARGET_TMPDIR")),
    );
}
