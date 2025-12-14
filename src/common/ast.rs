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

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct PointerTypeData {
    pub capacity: Option<usize>,
    pub mutable: bool,
    pub nullable: bool,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum PointerType {
    Thin(PointerTypeData),
    Fat(PointerTypeData),
    Array(PointerTypeData),
}

impl fmt::Display for PointerType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PointerType::Thin(d) => write!(
                f,
                "*{}{}",
                if d.nullable { "?" } else { "" },
                if d.mutable { "var" } else { "" }
            ),
            PointerType::Fat(d) => write!(
                f,
                "[*{}]{}",
                if d.nullable { "?" } else { "" },
                if d.mutable { "var" } else { "" }
            ),
            PointerType::Array(d) => {
                write!(
                    f,
                    "[{}]{}",
                    d.capacity.map(|t| t.to_string()).unwrap_or("".to_string()),
                    if d.mutable { "var" } else { "" }
                )
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypeData {
    pub pointer: Option<PointerType>,
    pub is_static: bool,
    pub optional: bool,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Bool(TypeData),
    UInt8(TypeData),
    UInt16(TypeData),
    UInt32(TypeData),
    UInt64(TypeData),
    Int8(TypeData),
    Int16(TypeData),
    Int32(TypeData),
    Int64(TypeData),
    Float32(TypeData),
    Float64(TypeData),
    String(TypeData),
    Null,
    Undefined,
}

impl Type {
    pub fn from_literal_expr(expr: &LiteralExpressionData) -> Option<Type> {
        let data = TypeData {
            pointer: None,
            is_static: false,
            optional: false,
        };

        match expr.value.kind {
            TokenKind::Integer => Some(Type::Int32(data)),
            TokenKind::Float => Some(Type::Float32(data)),
            TokenKind::True | TokenKind::False => Some(Type::Bool(data)),
            TokenKind::Undefined => Some(Type::Undefined),
            TokenKind::Null => Some(Type::Null),
            TokenKind::String => Some(Type::String(TypeData {
                pointer: None,
                is_static: true,
                optional: false,
            })),
            _ => None,
        }
    }

    pub fn from_type_descriptor(descriptor: &TypeDescriptor) -> Option<Type> {
        let pointer_type: Option<PointerType> =
            descriptor.pointer.clone().map(|pointer| match pointer {
                Pointer::Thin(pointer_descriptor) => PointerType::Thin(PointerTypeData {
                    capacity: None,
                    mutable: pointer_descriptor.mutable,
                    nullable: pointer_descriptor.nullable,
                }),
                Pointer::Fat(pointer_descriptor) => PointerType::Fat(PointerTypeData {
                    capacity: None,
                    mutable: pointer_descriptor.mutable,
                    nullable: pointer_descriptor.nullable,
                }),
                Pointer::Array(pointer_descriptor) => PointerType::Array(PointerTypeData {
                    capacity: pointer_descriptor
                        .capacity
                        .map(|t| t.lexeme.parse::<usize>().unwrap()),
                    mutable: pointer_descriptor.mutable,
                    nullable: pointer_descriptor.nullable,
                }),
            });

        let default_type_data = TypeData {
            pointer: pointer_type,
            is_static: descriptor
                .lifetime
                .clone()
                .is_some_and(|lf| lf.lexeme == "static"),
            optional: descriptor.optional,
        };

        match descriptor.identifier.lexeme.as_str() {
            "uint8" => Some(Type::UInt8(default_type_data)),
            "uint16" => Some(Type::UInt16(default_type_data)),
            "uint32" => Some(Type::UInt32(default_type_data)),
            "uint64" => Some(Type::UInt64(default_type_data)),
            "int8" => Some(Type::Int8(default_type_data)),
            "int16" => Some(Type::Int16(default_type_data)),
            "int32" => Some(Type::Int32(default_type_data)),
            "int64" => Some(Type::Int64(default_type_data)),
            "float32" => Some(Type::Float32(default_type_data)),
            "float64" => Some(Type::Float64(default_type_data)),
            "string" => Some(Type::String(default_type_data)),
            "bool" => Some(Type::Bool(default_type_data)),
            "undefined" => Some(Type::Undefined),
            "null" => Some(Type::Null),
            _ => None,
        }
    }

    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Type::Int8(_) | Type::Int16(_) | Type::Int32(_) | Type::Int64(_)
        )
    }

    pub fn is_integer(&self) -> bool {
        if self.is_signed() {
            return true;
        }

        matches!(
            self,
            Type::UInt8(_) | Type::UInt16(_) | Type::UInt32(_) | Type::UInt64(_)
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::Float32(_) | Type::Float64(_))
    }

    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Type::String(_))
    }

    pub fn is_pointer(&self) -> bool {
        if self.is_array() {
            return false;
        }

        match self {
            Type::Bool(type_data) => type_data.pointer.is_some(),
            Type::UInt8(type_data) => type_data.pointer.is_some(),
            Type::UInt16(type_data) => type_data.pointer.is_some(),
            Type::UInt32(type_data) => type_data.pointer.is_some(),
            Type::UInt64(type_data) => type_data.pointer.is_some(),
            Type::Int8(type_data) => type_data.pointer.is_some(),
            Type::Int16(type_data) => type_data.pointer.is_some(),
            Type::Int32(type_data) => type_data.pointer.is_some(),
            Type::Int64(type_data) => type_data.pointer.is_some(),
            Type::Float32(type_data) => type_data.pointer.is_some(),
            Type::Float64(type_data) => type_data.pointer.is_some(),
            Type::String(type_data) => type_data.pointer.is_some(),
            _ => false,
        }
    }

    pub fn is_nullable(&self) -> bool {
        if !self.is_pointer() {
            return false;
        }

        match self.get_type_data().unwrap().pointer.clone().unwrap() {
            PointerType::Thin(pointer_type_data) => pointer_type_data.nullable,
            PointerType::Fat(pointer_type_data) => pointer_type_data.nullable,
            _ => false,
        }
    }

    pub fn is_optional(&self) -> bool {
        self.get_type_data().is_some_and(|t| t.optional)
    }

    pub fn get_type_data(&self) -> Option<&TypeData> {
        match self {
            Type::Bool(type_data) => Some(type_data),
            Type::UInt8(type_data) => Some(type_data),
            Type::UInt16(type_data) => Some(type_data),
            Type::UInt32(type_data) => Some(type_data),
            Type::UInt64(type_data) => Some(type_data),
            Type::Int8(type_data) => Some(type_data),
            Type::Int16(type_data) => Some(type_data),
            Type::Int32(type_data) => Some(type_data),
            Type::Int64(type_data) => Some(type_data),
            Type::Float32(type_data) => Some(type_data),
            Type::Float64(type_data) => Some(type_data),
            Type::String(type_data) => Some(type_data),
            _ => None,
        }
    }

    pub fn get_mut_type_data(&mut self) -> Option<&mut TypeData> {
        match self {
            Type::Bool(type_data) => Some(type_data),
            Type::UInt8(type_data) => Some(type_data),
            Type::UInt16(type_data) => Some(type_data),
            Type::UInt32(type_data) => Some(type_data),
            Type::UInt64(type_data) => Some(type_data),
            Type::Int8(type_data) => Some(type_data),
            Type::Int16(type_data) => Some(type_data),
            Type::Int32(type_data) => Some(type_data),
            Type::Int64(type_data) => Some(type_data),
            Type::Float32(type_data) => Some(type_data),
            Type::Float64(type_data) => Some(type_data),
            Type::String(type_data) => Some(type_data),
            _ => None,
        }
    }

    pub fn is_array(&self) -> bool {
        self.get_type_data().is_some_and(|t| {
            t.pointer
                .clone()
                .is_some_and(|p| matches!(p, PointerType::Array(_)))
        })
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Type::Bool(_))
    }

    pub fn is_undefined(&self) -> bool {
        matches!(self, Type::Undefined)
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Type::Null)
    }

    pub fn is_compatible_with(&self, other: &Type) -> bool {
        if self.is_null() && other.is_nullable() {
            return true;
        }

        if self.is_pointer() != other.is_pointer() {
            return false;
        }

        // only bool -> bool
        if self.is_bool() || other.is_bool() {
            return self.is_bool() && other.is_bool();
        }

        if self.is_undefined() && other.is_optional() {
            return true;
        }

        // only string -> string
        if self.is_string() || other.is_string() {
            return self.is_string() && other.is_string();
        }

        if self.is_array() != other.is_array() {
            return false;
        }

        if (self.is_pointer() || self.is_array()) && (other.is_pointer() || other.is_array()) {
            let self_ptr = self.get_type_data().unwrap().pointer.clone().unwrap();
            let other_ptr = self.get_type_data().unwrap().pointer.clone().unwrap();

            let mutable_case = |ap: &PointerTypeData, bp: &PointerTypeData| {
                ap.mutable || !ap.mutable && !bp.mutable
            };
            let nullable_case = |ap: &PointerTypeData, bp: &PointerTypeData| {
                !ap.nullable || ap.nullable && bp.nullable
            };

            let ptr_case = match (self_ptr, other_ptr) {
                (PointerType::Thin(ap), PointerType::Thin(bp)) => {
                    mutable_case(&ap, &bp) && nullable_case(&ap, &bp)
                }
                (PointerType::Thin(ap), PointerType::Fat(bp)) => {
                    mutable_case(&ap, &bp) && nullable_case(&ap, &bp)
                }
                (PointerType::Fat(ap), PointerType::Fat(bp)) => {
                    mutable_case(&ap, &bp) && nullable_case(&ap, &bp) && ap.capacity == bp.capacity
                }
                (PointerType::Array(ap), PointerType::Array(bp)) => {
                    mutable_case(&ap, &bp) && nullable_case(&ap, &bp) && ap.capacity == bp.capacity
                }
                _ => false,
            };

            if !ptr_case {
                return false;
            }

            let mut at = self.clone();
            let mut bt = self.clone();

            at.get_mut_type_data().unwrap().pointer = None;
            bt.get_mut_type_data().unwrap().pointer = None;

            return at.is_compatible_with(&bt);
        }

        // Numerics
        if !self.is_numeric() || !other.is_numeric() {
            return false;
        }

        match (self.is_float(), other.is_float()) {
            // int → int
            (false, false) => {
                if self.is_signed() == other.is_signed() {
                    self.get_numeric_size() <= other.get_numeric_size()
                } else {
                    self.is_signed()
                        && !other.is_signed()
                        && self.get_numeric_size() <= other.get_numeric_size()
                }
            }

            // int → float
            (false, true) => {
                let mantissa = if other.get_numeric_size() == 32 {
                    24
                } else {
                    53
                };
                self.get_numeric_size() <= mantissa
            }

            // float → float
            (true, true) => self.get_numeric_size() <= other.get_numeric_size(),

            // float → int
            _ => false,
        }
    }

    pub fn get_numeric_size(&self) -> usize {
        match self {
            Type::Bool(_) => 1,
            Type::UInt8(_) | Type::Int8(_) => 8,
            Type::UInt16(_) | Type::Int16(_) => 16,
            Type::UInt32(_) | Type::Int32(_) | Type::Float32(_) => 32,
            Type::UInt64(_) | Type::Int64(_) | Type::Float64(_) => 64,
            _ => 0,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let write_ptr = |p: Option<PointerType>| {
            if let Some(ptr) = p {
                format!("{} ", ptr)
            } else {
                "".to_string()
            }
        };

        match self {
            Type::UInt8(d) => write!(f, "{}uint8", write_ptr(d.pointer.clone())),
            Type::UInt16(d) => write!(f, "{}uint16", write_ptr(d.pointer.clone())),
            Type::UInt32(d) => write!(f, "{}uint32", write_ptr(d.pointer.clone())),
            Type::UInt64(d) => write!(f, "{}uint64", write_ptr(d.pointer.clone())),
            Type::Int8(d) => write!(f, "{}int8", write_ptr(d.pointer.clone())),
            Type::Int16(d) => write!(f, "{}int16", write_ptr(d.pointer.clone())),
            Type::Int32(d) => write!(f, "{}int32", write_ptr(d.pointer.clone())),
            Type::Int64(d) => write!(f, "{}int64", write_ptr(d.pointer.clone())),
            Type::Float32(d) => write!(f, "{}float32", write_ptr(d.pointer.clone())),
            Type::Float64(d) => write!(f, "{}float64", write_ptr(d.pointer.clone())),
            Type::String(d) => write!(f, "{}string", write_ptr(d.pointer.clone())),
            Type::Bool(d) => write!(f, "{}bool", write_ptr(d.pointer.clone())),
            Type::Undefined => write!(f, "undefined"),
            Type::Null => write!(f, "null"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct PointerDescriptor {
    pub nullable: bool,
    pub capacity: Option<Token>,
    pub mutable: bool,
}

#[derive(Clone, Debug)]
pub enum Pointer {
    Thin(PointerDescriptor),
    Fat(PointerDescriptor),
    Array(PointerDescriptor),
}

#[derive(Clone, Debug)]
pub struct TypeDescriptor {
    pub identifier: Token,
    pub lifetime: Option<Token>,
    pub comptime: bool,
    pub optional: bool,
    pub pointer: Option<Pointer>,
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
    pub type_descriptor: Option<TypeDescriptor>,
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
