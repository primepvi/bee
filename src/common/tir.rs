use crate::common::ast::TypeDescriptor;
use std::fmt;

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Int8,
    Int16,
    Int32,
    Int64,
    Float32,
    Float64,
    String,

    Static(Box<Type>),
}

impl Type {
    pub fn from_descriptor(descriptor: TypeDescriptor) -> Option<Self> {
        let base = match descriptor.identifier.lexeme.as_str() {
            "int8" => Some(Type::Int8),
            "int16" => Some(Type::Int16),
            "int32" => Some(Type::Int32),
            "int64" => Some(Type::Int64),
            "uint8" => Some(Type::UInt8),
            "uint16" => Some(Type::UInt16),
            "uint32" => Some(Type::UInt32),
            "uint64" => Some(Type::UInt64),
            "float32" => Some(Type::Float32),
            "float64" => Some(Type::Float64),
            "string" => Some(Type::String),
            _ => None,
        };

        let base = if descriptor.lifetime.is_some_and(|lt| lt.lexeme == "static") {
            Some(Type::Static(Box::new(base.unwrap())))
        } else {
            base
        };

        base
    }

    pub fn compare(&self, other: &Type) -> bool {
        self.without_static() == other.without_static()
    }

    pub fn without_static(&self) -> Type {
        match self {
            Type::Static(t) => *t.clone(),
            _ => self.clone()
        }
    }

    pub fn is_static(&self) -> bool {
        matches!(self, Type::Static(_))
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::UInt8 => write!(f, "uint8"),
            Type::UInt16 => write!(f, "uint16"),
            Type::UInt32 => write!(f, "uint32"),
            Type::UInt64 => write!(f, "uint64"),
            Type::Int8 => write!(f, "int8"),
            Type::Int16 => write!(f, "int16"),
            Type::Int32 => write!(f, "int32"),
            Type::Int64 => write!(f, "int64"),
            Type::Float32 => write!(f, "float32"),
            Type::Float64 => write!(f, "float64"),
            Type::String => write!(f, "string"),
            Type::Static(t) => write!(f, "@static {}", t),
        }
    }
}

#[derive(Clone)]
pub enum TIRExpr {
    Literal {
        value: String,
        typing: Type,
    },
    Variable {
        name: String,
        typing: Type,
    },
    Assign {
        name: String,
        value: Box<TIRExpr>,
        typing: Type
    },
}

impl<'ctx> TIRExpr {
    pub fn get_typing(&self) -> &Type {
        match self {
            TIRExpr::Literal { typing, .. } => typing,
            TIRExpr::Variable { typing, .. } => typing,
            TIRExpr::Assign { typing, .. } => typing,
        }
    }
}

#[derive(Clone)]
pub enum TIRStmt {
    Declare {
        name: String,
        constant: bool,
        typing: Type,
        value: Option<TIRExpr>,
    },
    Expression(TIRExpr),
    Block {
        label: Option<String>,
        statements: Vec<TIRStmt>,
    },
}

#[derive(Clone)]
pub struct TIRProgram {
    pub statements: Vec<TIRStmt>,
}
