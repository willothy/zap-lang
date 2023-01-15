//! Type system for the compiler.

/// The recursive Type enum is ued to represent any type in the language.
#[derive(Debug, Clone)]
pub enum Type {
    /// PrimitiveType represents all primitive types in the language.
    Primitive(PrimitiveType),
    /// The pointer type represents a pointer to another type.
    Pointer(Box<Type>),
    /// The function type represents a function with a list of parameters and a return type.
    Function {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },
    /// The array type represents an array of a certain size and type.
    Array {
        size: usize,
        element_type: Box<Type>,
    },
    /// The Struct type represents a struct definition, with a name and a list of fields.
    Struct {
        name: String,
        fields: Vec<(String, Type)>,
    },
    /// The Enum type represents an enum definition, with a name and a list of variants.
    Enum { name: String, variants: Vec<String> },
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Function { return_type, .. } => return_type.size(),
            Type::Primitive(p) => p.size(),
            Type::Pointer(_) => 8,
            Type::Array { size, element_type } => size * element_type.size(),
            Type::Struct { fields, .. } => fields.iter().map(|(_, t)| t.size()).sum(),
            Type::Enum { .. } => 8,
        }
    }
}

/// PrimitiveType represents all primitive types in the language.
#[derive(Debug, Clone)]
pub enum PrimitiveType {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Bool,
    Char,
    /// Const string type (for string literals - String will be a data structure)
    Str,
}

impl PrimitiveType {
    pub fn size(&self) -> usize {
        match self {
            PrimitiveType::U8 => 1,
            PrimitiveType::U16 => 2,
            PrimitiveType::U32 => 4,
            PrimitiveType::U64 => 8,
            PrimitiveType::I8 => 1,
            PrimitiveType::I16 => 2,
            PrimitiveType::I32 => 4,
            PrimitiveType::I64 => 8,
            PrimitiveType::F32 => 4,
            PrimitiveType::F64 => 8,
            PrimitiveType::Bool => 1,
            PrimitiveType::Char => 1,
            PrimitiveType::Str => 8,
        }
    }
}
