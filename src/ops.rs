//! This module defines the operators used in the language
//! It is used by the parser, AST, TIR and codegen so needed to be separated
//! to avoid duplication or a messy import structure.

/// BinaryOperator represents a binary operator
#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    BitShl,
    BitShr,
}

/// UnaryOperator represents a unary operator
#[derive(Debug)]
pub enum UnaryOperator {
    Neg,
    BitNot,
    Not,
    Deref,
    Ref,
}

/// AssignmentOperator represents an assignment operator
#[derive(Debug)]
pub enum AssignmentOperator {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    ShlAssign,
    ShrAssign,
}
