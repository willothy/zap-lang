use std::path::PathBuf;

use zap_backend::codegen::Generator;
use zap_backend::ops;
use zap_backend::tir::Expression::*;
use zap_backend::tir::Function::*;
use zap_backend::tir::Literal::*;
use zap_backend::tir::Statement::*;
use zap_backend::tir::Unit;
use zap_backend::ty::PrimitiveType;
use zap_backend::ty::Type::*;

pub fn main() {
    let unit = Unit {
        name: "test".to_string(),
        path: PathBuf::from(std::env::var("PWD").unwrap()).join("test.asm"),
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
    let mut generator = Generator::new(unit);
    generator.generate();
    generator.builder.write();
}
