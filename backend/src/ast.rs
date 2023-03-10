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

impl Unit {
    pub fn new(
        name: String,
        path: PathBuf,
        imports: Vec<Import>,
        structs: Vec<Struct>,
        functions: Vec<Function>,
    ) -> Self {
        Self {
            name,
            path,
            imports,
            structs,
            functions,
        }
    }

    pub fn new_empty(name: String, path: PathBuf) -> Self {
        Self {
            name,
            path,
            imports: Vec::new(),
            structs: Vec::new(),
            functions: Vec::new(),
        }
    }
}

/// Represents a file location with a path, line, and column
pub type NodeLocTriple = (String, usize, usize);

/// Generalized utility enum for top level items
pub enum Item {
    Import(Import),
    Struct(Struct),
    Function(Function),
}

/// Import represents an import statement, with a path and a name
pub struct Import {
    pub name: String,
    pub path: String,
    pub loc: NodeLocTriple,
}

/// Struct represents a struct definition
pub struct Struct {
    pub name: String,
    pub fields: Vec<(String, TypeName)>,
    pub loc: NodeLocTriple,
}

/// Function represents a function definition or extern declaration
pub enum Function {
    Definition {
        name: String,
        return_type: TypeName,
        params: Vec<(String, TypeName)>,
        body: Vec<Statement>,
        loc: NodeLocTriple,
    },
    Extern {
        name: String,
        return_type: TypeName,
        params: Vec<(String, TypeName)>,
        loc: NodeLocTriple,
    },
}

/// TypeName represents a type name, with a name and a pointer depth
/// (e.g. `i32` has a pointer depth of 0, `i32*` has a pointer depth of 1)
pub struct TypeName {
    pub name: String,
    pub ptr_depth: usize,
}

impl TypeName {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_owned(),
            ptr_depth: 0,
        }
    }

    pub fn new_ptr(name: &str, ptr_depth: usize) -> Self {
        Self {
            name: name.to_owned(),
            ptr_depth,
        }
    }
}

/// Statement represents a statement in the AST
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
        type_name: TypeName,
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
pub enum Expression {
    InlineAssembly {
        code: Vec<String>,
        loc: NodeLocTriple,
    },
    Binary {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        op: BinaryOperator,
        loc: NodeLocTriple,
    },
    Unary {
        expr: Box<Expression>,
        op: UnaryOperator,
        loc: NodeLocTriple,
    },
    Call {
        name: String,
        args: Vec<Expression>,
        loc: NodeLocTriple,
    },
    Variable {
        name: String,
        loc: NodeLocTriple,
    },
    Literal {
        value: Literal,
        loc: NodeLocTriple,
    },
    AsExpr {
        expr: Box<Expression>,
        type_name: TypeName,
        loc: NodeLocTriple,
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
