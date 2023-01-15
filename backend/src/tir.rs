//! The TIR is a typed intermediate representation of the AST.
//! The TIR is used to represent the AST after name resolution and type checking.

use std::path::PathBuf;

use crate::{
    ops::{AssignmentOperator, BinaryOperator, UnaryOperator},
    ty::Type,
};

#[derive(Debug)]
pub struct Unit<'tir> {
    pub name: String,
    pub path: PathBuf,
    pub imports: Vec<Import<'tir>>,
    pub structs: Vec<Struct>,
    pub consts: Vec<Const>,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Import<'tir> {
    pub unit: &'tir Unit<'tir>,
}

#[derive(Debug)]
pub struct Const {
    pub name: String,
    pub ty: Type,
    pub value: Expression,
}

#[derive(Debug)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

#[derive(Debug)]
pub enum Function {
    Definition {
        name: String,
        return_type: Type,
        params: Vec<(String, Type)>,
        body: Vec<Statement>,
    },
    Extern {
        name: String,
        return_type: Type,
        params: Vec<(String, Type)>,
    },
}

#[derive(Debug)]
pub enum Statement {
    Return(Option<Expression>),
    Result(Expression),
    Expression(Expression),
    VariableDeclaration {
        name: String,
        ty: Type,
        initializer: Option<Expression>,
    },
    VariableAssignment {
        name: String,
        op: AssignmentOperator,
        value: Expression,
    },
    If {
        condition: Expression,
        then_body: Vec<Statement>,
        else_body: Vec<Statement>,
    },
    While {
        condition: Expression,
        body: Vec<Statement>,
    },
    For {
        initializer: Option<Box<Statement>>,
        condition: Option<Expression>,
        increment: Option<Expression>,
        body: Vec<Statement>,
    },
    Break,
    Continue,
}

/// Expression represents an expression in the AST
#[derive(Debug)]
pub enum Expression {
    InlineAssembly {
        code: Vec<String>,
    },
    Binary {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        op: BinaryOperator,
        ty: Type,
    },
    Unary {
        expr: Box<Expression>,
        op: UnaryOperator,
        ty: Type,
    },
    Call {
        name: String,
        args: Vec<Expression>,
        ty: Type,
    },
    Variable {
        name: String,
        ty: Type,
    },
    Literal {
        value: Literal,
        ty: Type,
    },
    AsExpr {
        expr: Box<Expression>,
        ty: Type,
    },
}

impl Expression {
    pub fn ty(&self) -> &Type {
        match self {
            Expression::Binary { ty, .. } => ty,
            Expression::Unary { ty, .. } => ty,
            Expression::Call { ty, .. } => ty,
            Expression::Variable { ty, .. } => ty,
            Expression::Literal { ty, .. } => ty,
            Expression::AsExpr { ty, .. } => ty,
            Expression::InlineAssembly { code } => {
                panic!("Cannot get type of inline assembly: {:?}", code)
            }
        }
    }
}

/// Literal represents a literal value
#[derive(Debug)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    Str(String),
}
