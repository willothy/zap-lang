use std::{path::PathBuf, process::ExitCode};

use self::{
    ops::{BinaryOperator, UnaryOperator},
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
    let mut gen = codegen::Generator::new(unit);
    gen.generate();
    let _output = gen.builder.finalize();
    gen.builder.write();

    ExitCode::SUCCESS
}
