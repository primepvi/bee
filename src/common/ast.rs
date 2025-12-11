use std::fmt::{self, Display};

#[derive(Copy, Clone)]
pub struct Span {
    pub len: usize,
    pub line: usize,
    pub col: usize
}

#[derive(Copy, Clone, PartialEq, Eq)]
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

#[derive(Clone)]
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

#[derive(Clone)]
pub struct TypeDescriptor {
    pub identifier: Token,
    pub lifetime: Option<Token>,
    pub comptime: bool
}

pub struct VariableAssignmentExpressionData {
    pub left: Token,
    pub equal: Token,
    pub right: Box<Expression>,
    pub span: Span,
}

pub struct LiteralExpressionData {
    pub value: Token,
    pub span: Span,
}

pub struct IdentifierExpressionData {
    pub value: Token,
    pub span: Span
}

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
            _ => unimplemented!()
        }
    }
}

pub struct DeclareVariableStatementData {
    pub constant: bool,
    pub identifier: Token,
    pub type_descriptor: Option<TypeDescriptor>,
    pub value: Option<Expression>,
    pub span: Span,
}

pub struct BlockStatementData {
    pub label: Option<Token>,
    pub statements: Vec<Statement>,
    pub span: Span,
}

pub struct ExpressionStatementData {
    pub expr: Expression,
    pub span: Span,
}

pub enum Statement {
    DeclareVariable(DeclareVariableStatementData),
    Block(BlockStatementData),
    Expression(ExpressionStatementData)
}
