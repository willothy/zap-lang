//! The TIR is a typed intermediate representation of the AST.
//! The TIR is used to represent the AST after name resolution and type checking.

use std::path::PathBuf;

use crate::{
    ast::NodeLocTriple,
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
    pub loc: NodeLocTriple,
}

#[derive(Debug)]
pub struct Const {
    pub name: String,
    pub ty: Type,
    pub value: Expression,
    pub loc: NodeLocTriple,
}

#[derive(Debug)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<(String, Type)>,
    pub loc: NodeLocTriple,
}

#[derive(Debug)]
pub enum Function {
    Definition {
        name: String,
        return_type: Type,
        params: Vec<(String, Type)>,
        body: Vec<Statement>,
        loc: NodeLocTriple,
    },
    Extern {
        name: String,
        return_type: Type,
        params: Vec<(String, Type)>,
        loc: NodeLocTriple,
    },
}

#[derive(Debug)]
pub enum Statement {
    Return {
        value: Option<Expression>,
        loc: NodeLocTriple,
    },
    Result {
        value: Expression,
        loc: NodeLocTriple,
    },
    Expression {
        expr: Expression,
        loc: NodeLocTriple,
    },
    VariableDeclaration {
        name: String,
        ty: Type,
        initializer: Option<Expression>,
        loc: NodeLocTriple,
    },
    VariableAssignment {
        name: String,
        op: AssignmentOperator,
        value: Expression,
        loc: NodeLocTriple,
    },
    If {
        condition: Expression,
        then_body: Vec<Statement>,
        else_body: Vec<Statement>,
        loc: NodeLocTriple,
    },
    While {
        condition: Expression,
        body: Vec<Statement>,
        loc: NodeLocTriple,
    },
    For {
        initializer: Option<Box<Statement>>,
        condition: Option<Expression>,
        increment: Option<Expression>,
        body: Vec<Statement>,
        loc: NodeLocTriple,
    },
    Break {
        loc: NodeLocTriple,
    },
    Continue {
        loc: NodeLocTriple,
    },
}

/// Expression represents an expression in the AST
#[derive(Debug)]
pub enum Expression {
    InlineAssembly {
        code: Vec<String>,
        loc: NodeLocTriple,
    },
    Binary {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        op: BinaryOperator,
        ty: Type,
        loc: NodeLocTriple,
    },
    Unary {
        expr: Box<Expression>,
        op: UnaryOperator,
        ty: Type,
        loc: NodeLocTriple,
    },
    Call {
        name: String,
        args: Vec<Expression>,
        ty: Type,
        loc: NodeLocTriple,
    },
    Variable {
        name: String,
        ty: Type,
        loc: NodeLocTriple,
    },
    Literal {
        value: Literal,
        ty: Type,
        loc: NodeLocTriple,
    },
    AsExpr {
        expr: Box<Expression>,
        ty: Type,
        loc: NodeLocTriple,
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
            Expression::InlineAssembly { code, .. } => {
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
