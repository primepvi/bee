use std::fmt::{self, Display};

#[derive(Copy, Clone, Debug)]
pub struct Span {
    pub len: usize,
    pub line: usize,
    pub col: usize
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum TokenKind {
    Eof,

    Const,
    Var,
    Begin,
    End,

    Integer,
    Float,
    String,
    Identifier,

    Equal,
    Colon,
    SemiColon,
    At,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::Eof => write!(f, "End of File"),
            TokenKind::Const => write!(f, "const"),
            TokenKind::Var => write!(f, "var"),
            TokenKind::Begin => write!(f, "begin"),
            TokenKind::End => write!(f, "end"),
            TokenKind::Integer => write!(f, "Integer"),
            TokenKind::Float => write!(f, "Float"),
            TokenKind::String => write!(f, "String"),
            TokenKind::Identifier => write!(f, "Identifier"),

            TokenKind::Equal => write!(f, "="),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::SemiColon => write!(f, ";"),
            TokenKind::At => write!(f, "@"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub span: Span
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:<12} '{}'", self.kind, self.lexeme)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
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

        if descriptor.lifetime.is_some_and(|lt| lt.lexeme == "static") {
            Some(Type::Static(Box::new(base.unwrap())))
        } else {
            base
        }
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

impl Display for Type {
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

#[derive(Clone, Debug)]
pub struct TypeDescriptor {
    pub identifier: Token,
    pub lifetime: Option<Token>,
    pub comptime: bool
}

#[derive(Clone, Debug)]
pub struct VariableAssignmentExpressionData {
    pub left: Token,
    pub equal: Token,
    pub right: Box<Expression>,
    pub span: Span,
    pub typing: Option<Type>
}

#[derive(Clone, Debug)]
pub struct LiteralExpressionData {
    pub value: Token,
    pub span: Span,
    pub typing: Option<Type>
}

#[derive(Clone, Debug)]
pub struct IdentifierExpressionData {
    pub value: Token,
    pub span: Span,
    pub typing: Option<Type>
}

#[derive(Clone, Debug)]
pub enum Expression {
    VariableAssignment(VariableAssignmentExpressionData),
    Literal(LiteralExpressionData),
    Identifier(IdentifierExpressionData),
}

impl Expression {
    pub fn get_span(&self) -> Span {
        match self {
            Expression::VariableAssignment(d) => d.span,
            Expression::Literal(d) => d.span,
            Expression::Identifier(d) => d.span,
         }
    }

    pub fn get_typing(&self) -> Option<&Type> {
        match self {
            Expression::VariableAssignment(d) => d.typing.as_ref(),
            Expression::Literal(d) => d.typing.as_ref(),
            Expression::Identifier(d) => d.typing.as_ref()
        }
    }
}

#[derive(Clone, Debug)]
pub struct DeclareVariableStatementData {
    pub constant: bool,
    pub identifier: Token,
    pub type_descriptor: Option<TypeDescriptor>,
    pub value: Option<Expression>,
    pub span: Span,
    pub typing: Option<Type>
}

#[derive(Clone, Debug)]
pub struct BlockStatementData {
    pub label: Option<Token>,
    pub statements: Vec<Statement>,
    pub span: Span,
    pub typing: Option<Type>
}

#[derive(Clone, Debug)]
pub struct ExpressionStatementData {
    pub expr: Expression,
    pub span: Span,
    pub typing: Option<Type>
}

#[derive(Clone, Debug)]
pub enum Statement {
    DeclareVariable(DeclareVariableStatementData),
    Block(BlockStatementData),
    Expression(ExpressionStatementData)
}
