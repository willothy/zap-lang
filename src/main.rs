use std::{path::PathBuf, process::ExitCode};

use self::{
    ops::{AssignmentOperator, BinaryOperator, UnaryOperator},
    ty::PrimitiveType,
};

use crate::ty::Type::*;
use tir::{Expression::*, Function::*, Literal::*, Statement::*};

mod ast;
mod codegen;
mod ops;
mod tir;
mod ty;
mod util;

fn main() -> ExitCode {
    let unit = tir::Unit {
        name: "test".to_string(),
        path: PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap()).join("test.asm"),
        imports: Vec::new(),
        structs: Vec::new(),
        consts: Vec::new(),
        functions: vec![
            Extern {
                name: "printf".to_owned(),
                return_type: Primitive(PrimitiveType::I32),
                params: vec![(
                    "".to_owned(),
                    Pointer(Box::new(Primitive(PrimitiveType::Char))),
                )],
            },
            Extern {
                name: "gets".to_owned(),
                return_type: Pointer(Box::new(Primitive(PrimitiveType::Char))),
                params: vec![(
                    "buf".to_owned(),
                    Pointer(Box::new(Primitive(PrimitiveType::Char))),
                )],
            },
            Extern {
                name: "malloc".to_owned(),
                return_type: Pointer(Box::new(Primitive(PrimitiveType::Char))),
                params: vec![("size".to_owned(), Primitive(PrimitiveType::I32))],
            },
            // Multiple lines!
            Definition {
                name: "main".to_owned(),
                return_type: Primitive(PrimitiveType::I32),
                params: Vec::new(),
                body: vec![
                    // MAKE IT MULTIPLE LINES!!
                    VariableDeclaration {
                        name: "a".to_owned(),
                        ty: Pointer(Box::new(Primitive(PrimitiveType::Char))),
                        initializer: Some(Call {
                            name: "malloc".to_owned(),
                            args: vec![Literal {
                                value: Int(128),
                                ty: Primitive(PrimitiveType::I32),
                            }],
                            ty: Pointer(Box::new(Primitive(PrimitiveType::Char))),
                        }),
                    },
                    Expression(Call {
                        name: "gets".to_owned(),
                        args: vec![
                            // align!
                            Variable {
                                name: "a".to_owned(),
                                ty: Pointer(Box::new(Primitive(PrimitiveType::Char))),
                            },
                        ],
                        ty: Pointer(Box::new(Primitive(PrimitiveType::Char))),
                    }),
                    Return(Some(Call {
                        name: "printf".to_owned(),
                        args: vec![
                            Literal {
                                value: Str("Hello, %s!\n".to_owned()),
                                ty: Pointer(Box::new(Primitive(PrimitiveType::Char))),
                            },
                            Variable {
                                name: "a".to_owned(),
                                ty: Pointer(Box::new(Primitive(PrimitiveType::Char))),
                            },
                        ],
                        ty: Primitive(PrimitiveType::I32),
                    })),
                ],
            },
        ],
    };

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
    let mut gen = codegen::Generator::new(unit);
    gen.generate();
    let _output = gen.builder.finalize();
    gen.builder.write();

    ExitCode::SUCCESS
}
