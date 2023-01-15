//! The AST is a simple, untyped, unoptimized representation of the source code.
//! Name resolution and type checking are done on the AST, and the AST is then
//! converted to a typed IR. This IR is then compiled to native x86-64 code.

use std::path::PathBuf;

use crate::ops::{AssignmentOperator, BinaryOperator, UnaryOperator};

/// Unit represents one compilation unit (file)
pub struct Unit {
    pub name: String,
    pub path: PathBuf,
    pub imports: Vec<Import>,
    pub structs: Vec<Struct>,
    pub functions: Vec<Function>,
}

/// Import represents an import statement, with a path and a name
pub struct Import {
    pub name: String,
    pub path: String,
}

/// Struct represents a struct definition
pub struct Struct {
    pub name: String,
    pub fields: Vec<(String, TypeName)>,
}

/// Function represents a function definition or extern declaration
pub enum Function {
    Definition {
        name: String,
        return_type: TypeName,
        params: Vec<(String, TypeName)>,
        body: Vec<Statement>,
    },
    Extern {
        name: String,
        return_type: TypeName,
        params: Vec<(String, TypeName)>,
    },
}

/// TypeName represents a type name, with a name and a pointer depth
/// (e.g. `i32` has a pointer depth of 0, `i32*` has a pointer depth of 1)
pub struct TypeName {
    pub name: String,
    pub ptr_depth: usize,
}

/// Statement represents a statement in the AST
pub enum Statement {
    Return(Option<Expression>),
    Result(Expression),
    Expression(Expression),
    VariableDeclaration {
        name: String,
        type_name: TypeName,
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
pub enum Expression {
    InlineAssembly {
        code: Vec<String>,
    },
    Binary {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        op: BinaryOperator,
    },
    Unary {
        expr: Box<Expression>,
        op: UnaryOperator,
    },
    Call {
        name: String,
        args: Vec<Expression>,
    },
    Variable(String),
    Literal(Literal),
    AsExpr {
        expr: Box<Expression>,
        type_name: TypeName,
    },
}

/// Literal represents a literal value
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
}
