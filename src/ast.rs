use std::{fmt, usize};

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
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::Eof => write!(f, "Eof"),
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
        }
    }
}

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub span: Span
}

impl fmt::Display for Token {
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

impl fmt::Display for TypeDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TypeDesc(identifier={}, ", self.identifier)?;

        if let Some(l) = &self.lifetime {
            write!(f, "lifetime={}, ", l)?;
        } else {
            write!(f, "lifetime=None, ")?;
        }

        write!(f, "comptime={})", self.comptime)
    }
}

pub enum Expression {
    VariableAssignment { left: Token, equal: Token, right: Box<Expression> },
    Literal { value: Token },
    Identifier { value: Token }
}

impl Expression {
    pub fn print(&self, indent: usize) {
        let pad = " ".repeat(indent);
        match self {
            Expression::VariableAssignment { left, equal, right } => {
                println!("{}Assignment: {}", pad, left.lexeme);
                right.print(indent + 2);
            }
            Expression::Literal { value } => {
                println!("{}Literal: {}", pad, value.lexeme);
            }
            Expression::Identifier { value } => {
                println!("{}Identifier: {}", pad, value.lexeme);
            }
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::VariableAssignment { left, equal, right } => {
                write!(f, "(assign {} = {})", left, right)
            }

            Expression::Literal { value } => {
                write!(f, "(literal {})", value)
            }

            Expression::Identifier { value } => {
                write!(f, "(id {})", value)
            }
        }
    }
}

pub enum Statement {
    DeclareVariable { constant: bool, identifier: Token, type_descriptor: Option<TypeDescriptor>, value: Option<Expression> },
    Block { label: Option<Token>, statements: Vec<Statement> },
    Expression { expr: Expression }
}

impl Statement {
    pub fn print(&self, indent: usize) {
        let pad = " ".repeat(indent);
        match self {
            Statement::DeclareVariable { constant, identifier, type_descriptor, value } => {
                println!("{}DeclareVariable: {} {}", pad, if *constant { "const" } else { "var" }, identifier.lexeme);
                if let Some(td) = type_descriptor {
                    println!("{}  TypeDescriptor: {}", pad, td.identifier.lexeme);
                    if let Some(lifetime) = &td.lifetime {
                        println!("{}    Lifetime: {}", pad, lifetime.lexeme);
                    }
                    println!("{}    Comptime: {}", pad, td.comptime);
                }
                if let Some(expr) = value {
                    println!("{}  Value:", pad);
                    expr.print(indent + 4);
                }
            }
            Statement::Block { label, statements } => {
                if let Some(lbl) = label {
                    println!("{}Block: {}", pad, lbl.lexeme);
                } else {
                    println!("{}Block:", pad);
                }
                for stmt in statements {
                    stmt.print(indent + 2);
                }
            }
            Statement::Expression { expr } => {
                println!("{}ExpressionStatement: ", pad);
                expr.print(indent + 2);
            }
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::DeclareVariable {
                constant,
                identifier,
                type_descriptor,
                value,
            } => {
                write!(f, "DeclareVar(constant={}, id={}", constant, identifier)?;

                if let Some(td) = type_descriptor {
                    write!(f, ", type={}, ", td)?;
                } else {
                    write!(f, ", type=None, ")?;
                }

                if let Some(val) = value {
                    write!(f, "value={})", val)
                } else {
                    write!(f, "value=None)")
                }
            }

            Statement::Block { label, statements } => {
                write!(f, "Block(")?;
                if let Some(lbl) = label {
                    write!(f, "label={}, ", lbl)?;
                } else {
                    write!(f, "label=None, ")?;
                }

                write!(f, "statements=[")?;
                for (i, stmt) in statements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", stmt)?;
                }
                write!(f, "])")
            }
            Statement::Expression { expr } => write!(f, "ExpressionStatement: {}", expr),
        }
    }
}
