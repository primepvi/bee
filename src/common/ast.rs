use std::fmt::{self, Display};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Span {
    pub len: usize,
    pub line: usize,
    pub col: usize,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum TokenKind {
    Eof,

    Const,
    Var,
    Begin,
    End,
    True,
    False,
    Undefined,
    Null,

    Integer,
    Float,
    String,
    Identifier,

    Equal,     // =
    Colon,     // :
    SemiColon, // ;
    At,        // @
    Question,  // ?
    Star,      // *
    Ampersand, // &
    Comma,     // ,

    OpenParen,  // (
    CloseParen, // )

    OpenBrace,  // [
    CloseBrace, // ]
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::Eof => write!(f, "End of File"),
            TokenKind::Const => write!(f, "Const"),
            TokenKind::Var => write!(f, "Var"),
            TokenKind::Begin => write!(f, "Begin"),
            TokenKind::End => write!(f, "End"),
            TokenKind::Integer => write!(f, "Integer"),
            TokenKind::Float => write!(f, "Float"),
            TokenKind::String => write!(f, "String"),
            TokenKind::Identifier => write!(f, "Identifier"),
            TokenKind::Equal => write!(f, "Equal"),
            TokenKind::Colon => write!(f, "Colon"),
            TokenKind::SemiColon => write!(f, "SemiColon"),
            TokenKind::At => write!(f, "At"),
            TokenKind::True => write!(f, "True"),
            TokenKind::False => write!(f, "False"),
            TokenKind::Undefined => write!(f, "Undefined"),
            TokenKind::Null => write!(f, "Null"),
            TokenKind::Question => write!(f, "Question"),
            TokenKind::Star => write!(f, "Star"),
            TokenKind::Ampersand => write!(f, "Ampersand"),
            TokenKind::OpenParen => write!(f, "OpenParen"),
            TokenKind::CloseParen => write!(f, "CloseParen"),
            TokenKind::OpenBrace => write!(f, "OpenBrace"),
            TokenKind::CloseBrace => write!(f, "CloseBrace"),
            TokenKind::Comma => write!(f, "Comma"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub span: Span,
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}'", self.lexeme)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PrimitiveTypeKind {
    Bool,
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
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct PrimitiveType {
    pub kind: PrimitiveTypeKind,
    pub optional: bool,
}

impl PrimitiveType {
    pub fn compatible(a: PrimitiveType, b: PrimitiveType) -> bool {
        if a.optional && !b.optional {
            return false;
        }

        if a.is_bool() || b.is_bool() {
            return a.is_bool() == b.is_bool();
        }

        if a.is_string() || b.is_string() {
            return a.is_string() == b.is_string();
        }

        match (a.is_float(), b.is_float()) {
            // float -> float
            (true, true) => a.get_size_in_bits() <= b.get_size_in_bits(),

            // int -> float
            (false, true) => {
                let mantissa = if b.get_size_in_bits() == 32 { 24 } else { 53 };
                a.get_size_in_bits() <= mantissa
            }

            // int -> int
            (false, false) => {
                if a.is_signed_integer() == a.is_signed_integer() {
                    a.get_size_in_bits() <= b.get_size_in_bits()
                } else {
                    a.is_signed_integer()
                        && !b.is_signed_integer()
                        && a.get_size_in_bits() <= b.get_size_in_bits()
                }
            }

            // float -> int
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self.kind,
            PrimitiveTypeKind::UInt8
                | PrimitiveTypeKind::UInt16
                | PrimitiveTypeKind::UInt32
                | PrimitiveTypeKind::UInt64
                | PrimitiveTypeKind::Int8
                | PrimitiveTypeKind::Int16
                | PrimitiveTypeKind::Int32
                | PrimitiveTypeKind::Int64
        )
    }

    pub fn is_signed_integer(&self) -> bool {
        matches!(
            self.kind,
            PrimitiveTypeKind::Int8
                | PrimitiveTypeKind::Int16
                | PrimitiveTypeKind::Int32
                | PrimitiveTypeKind::Int64
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(
            self.kind,
            PrimitiveTypeKind::Float32 | PrimitiveTypeKind::Float64
        )
    }

    pub fn is_string(&self) -> bool {
        matches!(self.kind, PrimitiveTypeKind::String)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self.kind, PrimitiveTypeKind::Bool)
    }

    pub fn get_size_in_bits(&self) -> usize {
        match self.kind {
            PrimitiveTypeKind::Bool => 1,
            PrimitiveTypeKind::UInt8 | PrimitiveTypeKind::Int8 => 8,
            PrimitiveTypeKind::UInt16 | PrimitiveTypeKind::Int16 => 16,
            PrimitiveTypeKind::UInt32 | PrimitiveTypeKind::Int32 | PrimitiveTypeKind::Float32 => 32,
            PrimitiveTypeKind::UInt64 | PrimitiveTypeKind::Int64 | PrimitiveTypeKind::Float64 => 64,
            _ => 0,
        }
    }
}

impl fmt::Display for PrimitiveTypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            PrimitiveTypeKind::Bool => "bool",
            PrimitiveTypeKind::UInt8 => "uint8",
            PrimitiveTypeKind::UInt16 => "uint16",
            PrimitiveTypeKind::UInt32 => "uint32",
            PrimitiveTypeKind::UInt64 => "uint64",
            PrimitiveTypeKind::Int8 => "int8",
            PrimitiveTypeKind::Int16 => "int16",
            PrimitiveTypeKind::Int32 => "int32",
            PrimitiveTypeKind::Int64 => "int64",
            PrimitiveTypeKind::Float32 => "float32",
            PrimitiveTypeKind::Float64 => "float64",
            PrimitiveTypeKind::String => "string",
        };
        write!(f, "{s}")
    }
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.optional {
            write!(f, "{}?", self.kind)
        } else {
            write!(f, "{}", self.kind)
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum PointerTypeKind {
    Thin,
    Fat,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct PointerType {
    pub kind: PointerTypeKind,
    pub inner: Box<Type>,
    pub capacity: Option<usize>,
    pub mutable: bool,
    pub nullable: bool,
}

impl fmt::Display for PointerType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let nullable = if self.nullable { "?" } else { "" };
        let mutable = if self.mutable { "var" } else { "" };

        match self.kind {
            PointerTypeKind::Thin => write!(f, "*{}{} {}", nullable, mutable, self.inner),
            PointerTypeKind::Fat => write!(f, "[*{}]{} {}", nullable, mutable, self.inner),
        }
    }
}

impl PointerType {
pub fn compatible(a: &PointerType, b: &PointerType) -> bool {
        if a.kind != b.kind {
            return false;
        }

        if a.nullable && !b.nullable {
            return false;
        }

        if !a.mutable && b.mutable {
            return false;
        }

        if let PointerTypeKind::Fat = a.kind
            && !(a.capacity == b.capacity || (a.capacity.is_some() && b.capacity.is_none())) {
                return false;
            }

        a.inner == b.inner
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ArrayType {
    pub inner: Box<Type>,
    pub capacity: Option<usize>,
    pub mutable: bool,
}

impl fmt::Display for ArrayType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}]{} {}",
            self.capacity
                .map(|c| c.to_string())
                .unwrap_or("".to_string()),
            if self.mutable { "var" } else { "" },
            self.inner
        )
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Primitive(PrimitiveType),
    Pointer(PointerType),
    Array(ArrayType),
    Null,
    Undefined,
}

impl Type {
    pub fn from_literal_expr(expr: &LiteralExpressionData) -> Option<Type> {
        match expr.value.kind {
            TokenKind::Integer => Some(Type::Primitive(PrimitiveType {
                kind: PrimitiveTypeKind::Int32,
                optional: false,
            })),
            TokenKind::Float => Some(Type::Primitive(PrimitiveType {
                kind: PrimitiveTypeKind::Float32,
                optional: false,
            })),
            TokenKind::True | TokenKind::False => Some(Type::Primitive(PrimitiveType {
                kind: PrimitiveTypeKind::Bool,
                optional: false,
            })),
            TokenKind::String => Some(Type::Primitive(PrimitiveType {
                kind: PrimitiveTypeKind::String,
                optional: false,
            })),
            TokenKind::Undefined => Some(Type::Undefined),
            TokenKind::Null => Some(Type::Null),
            _ => None,
        }
    }

    pub fn from_type_annotation(annotation: &TypeAnnotation) -> Option<Type> {
        if let TypeAnnotation::Identifier { token, optional } = annotation.clone() {
            return match token.lexeme.as_str() {
                "uint8" => Some(Type::Primitive(PrimitiveType {
                    kind: PrimitiveTypeKind::UInt8,
                    optional,
                })),
                "uint16" => Some(Type::Primitive(PrimitiveType {
                    kind: PrimitiveTypeKind::UInt16,
                    optional,
                })),
                "uint32" => Some(Type::Primitive(PrimitiveType {
                    kind: PrimitiveTypeKind::UInt32,
                    optional,
                })),
                "uint64" => Some(Type::Primitive(PrimitiveType {
                    kind: PrimitiveTypeKind::UInt64,
                    optional,
                })),
                "int8" => Some(Type::Primitive(PrimitiveType {
                    kind: PrimitiveTypeKind::Int8,
                    optional,
                })),
                "int16" => Some(Type::Primitive(PrimitiveType {
                    kind: PrimitiveTypeKind::Int16,
                    optional,
                })),
                "int32" => Some(Type::Primitive(PrimitiveType {
                    kind: PrimitiveTypeKind::Int32,
                    optional,
                })),
                "int64" => Some(Type::Primitive(PrimitiveType {
                    kind: PrimitiveTypeKind::Int64,
                    optional,
                })),
                "float32" => Some(Type::Primitive(PrimitiveType {
                    kind: PrimitiveTypeKind::Float32,
                    optional,
                })),
                "float64" => Some(Type::Primitive(PrimitiveType {
                    kind: PrimitiveTypeKind::Float64,
                    optional,
                })),
                "string" => Some(Type::Primitive(PrimitiveType {
                    kind: PrimitiveTypeKind::String,
                    optional,
                })),
                "bool" => Some(Type::Primitive(PrimitiveType {
                    kind: PrimitiveTypeKind::Bool,
                    optional,
                })),
                "undefined" => Some(Type::Undefined),
                "null" => Some(Type::Null),
                _ => None,
            };
        }

        if let TypeAnnotation::Pointer {
            kind,
            capacity,
            inner,
            nullable,
            mutable,
        } = annotation.clone()
        {
            let inner_type = Type::from_type_annotation(&inner);
            return inner_type.map(|t| {
                Type::Pointer(PointerType {
                    kind,
                    inner: Box::new(t),
                    capacity: capacity.map(|t| t.lexeme.parse::<usize>().unwrap()),
                    mutable,
                    nullable,
                })
            });
        }

        if let TypeAnnotation::Array {
            capacity,
            inner,
            mutable,
        } = annotation.clone()
        {
            let inner_type = Type::from_type_annotation(&inner);
            return inner_type.map(|t| {
                Type::Array(ArrayType {
                    inner: Box::new(t),
                    capacity: capacity.map(|t| t.lexeme.parse::<usize>().unwrap()),
                    mutable,
                })
            });
        }

        None
    }

    pub fn compatible(a: &Type, b: &Type) -> bool {
        match (a, b) {
            (Type::Null, Type::Pointer(p)) => p.nullable,
            (Type::Undefined, Type::Primitive(p)) => p.optional,
            (Type::Primitive(pa), Type::Primitive(pb)) => PrimitiveType::compatible(*pa, *pb),
            (Type::Pointer(pa), Type::Pointer(pb)) => PointerType::compatible(pa, pb),
            (Type::Array(aa), Type::Array(bb)) => {
                (aa.capacity == bb.capacity || aa.capacity.is_some() && bb.capacity.is_none() ) && Type::compatible(&aa.inner, &bb.inner)
            }
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Type::Pointer(_))
    }

    pub fn is_primitive(&self) -> bool {
        matches!(self, Type::Primitive(_))
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(_))
    }

    pub fn is_undefined(&self) -> bool {
        matches!(self, Type::Undefined)
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Type::Null)
    }

    pub fn get_primitive(&self) -> Option<PrimitiveType> {
        match self {
            Type::Primitive(p) => Some(*p),
            _ => None
        }
    }

    pub fn get_array(&self) -> Option<ArrayType> {
        match self {
            Type::Array(a) => Some(a.clone()),
            _ => None
        }
    }

    pub fn get_mut_array(&mut self) -> Option<&mut ArrayType> {
        match self {
            Type::Array(a) => Some(a),
            _ => None
        }
    }

    pub fn get_pointer(&self) -> Option<PointerType> {
        match self {
            Type::Pointer(p) => Some(p.clone()),
            _ => None
        }
    }

    pub fn get_mut_pointer(&mut self) -> Option<&mut PointerType> {
        match self {
            Type::Pointer(p) => Some(p),
            _ => None
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Primitive(t) => write!(f, "{t}"),
            Type::Pointer(t) => write!(f, "{t}"),
            Type::Array(t) => write!(f, "{t}"),
            Type::Null => write!(f, "null"),
            Type::Undefined => write!(f, "undefined"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeAnnotation {
    Identifier {
        token: Token,
        optional: bool,
    },
    Pointer {
        kind: PointerTypeKind,
        capacity: Option<Token>,
        inner: Box<TypeAnnotation>,
        nullable: bool,
        mutable: bool,
    },
    Array {
        capacity: Option<Token>,
        inner: Box<TypeAnnotation>,
        mutable: bool,
    },
}

impl fmt::Display for TypeAnnotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeAnnotation::Identifier { token, optional } => {
                if *optional {
                    write!(f, "{}?", token.lexeme)
                } else {
                    write!(f, "{}", token.lexeme)
                }
            }

            TypeAnnotation::Pointer {
                kind,
                capacity: _,
                inner,
                nullable,
                mutable,
            } => {
                match kind {
                    PointerTypeKind::Thin => write!(f, "*{}", if *mutable { "?" } else { "" })?,
                    PointerTypeKind::Fat => write!(f, "[*{}]", if *mutable { "?" } else { "" })?,
                }

                if *mutable {
                    write!(f, "var ")?;
                }

                write!(f, "{}", inner)?;

                Ok(())
            }

            TypeAnnotation::Array {
                capacity,
                inner,
                mutable,
            } => {
                match capacity {
                    Some(token) => write!(f, "[{}]", token.lexeme)?,
                    None => write!(f, "[]")?,
                }

                if *mutable {
                    write!(f, "var ")?;
                }

                write!(f, "{}", inner)
            }
        }
    }
}

impl TypeAnnotation {
    pub fn get_span(&self) -> Span {       
        match self {
            TypeAnnotation::Identifier { token, optional } => token.span,
            TypeAnnotation::Pointer { kind, capacity, inner, nullable, mutable } => inner.get_span(),
            TypeAnnotation::Array { capacity, inner, mutable } => inner.get_span(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct VariableAssignmentExpressionData {
    pub left: Box<Expression>,
    pub equal: Token,
    pub right: Box<Expression>,
    pub span: Span,
    pub typing: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct LiteralExpressionData {
    pub value: Token,
    pub span: Span,
    pub typing: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct IdentifierExpressionData {
    pub value: Token,
    pub span: Span,
    pub typing: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct ArrayLiteralExpressionData {
    pub values: Vec<Expression>,
    pub span: Span,
    pub typing: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct ArrayAccessExpressionData {
    pub left: Box<Expression>,
    pub index: Box<Expression>,
    pub span: Span,
    pub typing: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct BuiltinCallExpressionData {
    pub callee: Token,
    pub arguments: Vec<Expression>,
    pub typing: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct DereferenceExpressionData {
    pub identifier: Box<Expression>,
    pub span: Span,
    pub typing: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct ReferenceExpressionData {
    pub identifier: Token,
    pub mutable: bool,
    pub span: Span,
    pub typing: Option<Type>,
}

#[derive(Clone, Debug)]
pub enum Expression {
    VariableAssignment(VariableAssignmentExpressionData),
    Literal(LiteralExpressionData),
    Identifier(IdentifierExpressionData),
    ArrayLiteral(ArrayLiteralExpressionData),
    ArrayAccess(ArrayAccessExpressionData),
    Dereference(DereferenceExpressionData),
    Reference(ReferenceExpressionData),
    BuiltinCall(BuiltinCallExpressionData),
}

impl Expression {
    pub fn get_span(&self) -> Span {
        match self {
            Expression::VariableAssignment(d) => d.span,
            Expression::Literal(d) => d.span,
            Expression::Identifier(d) => d.span,
            Expression::ArrayLiteral(d) => d.span,
            Expression::ArrayAccess(d) => d.span,
            Expression::BuiltinCall(d) => d.callee.span,
            Expression::Dereference(d) => d.span,
            Expression::Reference(d) => d.span,
        }
    }

    pub fn get_typing(&self) -> Option<&Type> {
        match self {
            Expression::VariableAssignment(d) => d.typing.as_ref(),
            Expression::Literal(d) => d.typing.as_ref(),
            Expression::Identifier(d) => d.typing.as_ref(),
            Expression::Dereference(d) => d.typing.as_ref(),
            Expression::Reference(d) => d.typing.as_ref(),
            Expression::ArrayLiteral(d) => d.typing.as_ref(),
            Expression::ArrayAccess(d) => d.typing.as_ref(),
            Expression::BuiltinCall(d) => d.typing.as_ref(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct DeclareVariableStatementData {
    pub constant: bool,
    pub identifier: Token,
    pub type_annotation: Option<TypeAnnotation>,
    pub value: Option<Expression>,
    pub span: Span,
    pub typing: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct BlockStatementData {
    pub label: Option<Token>,
    pub statements: Vec<Statement>,
    pub span: Span,
    pub typing: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct ExpressionStatementData {
    pub expr: Expression,
    pub span: Span,
    pub typing: Option<Type>,
}

#[derive(Clone, Debug)]
pub enum Statement {
    DeclareVariable(DeclareVariableStatementData),
    Block(BlockStatementData),
    Expression(ExpressionStatementData),
}
