use std::{path::PathBuf, process::ExitCode};

use self::{tir::Function, ty::PrimitiveType};

mod ast;
mod codegen;
mod ops;
mod tir;
mod ty;

fn main() -> ExitCode {
    let unit = tir::Unit {
        name: "test".to_string(),
        path: PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap()).join("test.asm"),
        imports: Vec::new(),
        structs: Vec::new(),
        functions: vec![Function::Definition {
            name: "main".to_owned(),
            return_type: ty::Type::Primitive(PrimitiveType::I32),
            params: Vec::new(),
            body: vec![
                tir::Statement::VariableDeclaration {
                    name: "a".to_owned(),
                    ty: ty::Type::Primitive(PrimitiveType::I32),
                    initializer: Some(tir::Expression::Literal {
                        value: tir::Literal::Int(15),
                        ty: ty::Type::Primitive(PrimitiveType::I32),
                    }),
                },
                tir::Statement::VariableDeclaration {
                    name: "b".to_owned(),
                    ty: ty::Type::Primitive(PrimitiveType::I32),
                    initializer: Some(tir::Expression::Literal {
                        value: tir::Literal::Int(39),
                        ty: ty::Type::Primitive(PrimitiveType::I32),
                    }),
                },
                tir::Statement::VariableAssignment {
                    name: "a".to_owned(),

                    op: ops::AssignmentOperator::Assign,
                    value: tir::Expression::Literal {
                        value: tir::Literal::Int(25),
                        ty: ty::Type::Primitive(PrimitiveType::I32),
                    },
                },
                tir::Statement::If {
                    condition: tir::Expression::Binary {
                        lhs: Box::new(tir::Expression::Variable {
                            name: "a".to_owned(),
                            ty: ty::Type::Primitive(PrimitiveType::I32),
                        }),
                        rhs: Box::new(tir::Expression::Variable {
                            name: "b".to_owned(),
                            ty: ty::Type::Primitive(PrimitiveType::I32),
                        }),
                        op: ops::BinaryOperator::Lt,
                        ty: ty::Type::Primitive(PrimitiveType::Bool),
                    },
                    then_body: vec![tir::Statement::Return(Some(tir::Expression::Variable {
                        name: "a".to_owned(),
                        ty: ty::Type::Primitive(PrimitiveType::I32),
                    }))],
                    else_body: vec![tir::Statement::Return(Some(tir::Expression::Variable {
                        name: "b".to_owned(),
                        ty: ty::Type::Primitive(PrimitiveType::I32),
                    }))],
                },
            ],
        }],
    };
    let mut gen = codegen::Generator::new(unit);
    gen.generate();
    let _output = gen.builder.finalize();
    gen.builder.write();

    ExitCode::SUCCESS
}
