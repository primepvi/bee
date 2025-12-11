use std::{
    iter::{Iterator, Peekable},
    vec::IntoIter,
};

use crate::{ast::*, error::ErrorFormatter};

pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
    err_fmt: ErrorFormatter,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, source: String, filepath: String) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
            err_fmt: ErrorFormatter::new("parser", source, filepath),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, String> {
        let mut program: Vec<Statement> = Vec::with_capacity(128);

        while self.tokens.peek().is_some_and(|t| t.kind != TokenKind::Eof) {
            program.push(self.parse_statement()?);
        }

        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        let current = self.tokens.peek().unwrap();

        match current.kind {
            TokenKind::Const | TokenKind::Var => self.parse_declare_var_statement(),
            TokenKind::Begin => self.parse_block_statement(),
            TokenKind::Identifier | TokenKind::Integer | TokenKind::Float | TokenKind::String => {
                let stmt = Statement::Expression {
                    expr: self.parse_expr()?,
                };

                self.tokens
                    .next_if(|t| t.kind == TokenKind::SemiColon)
                    .ok_or_else(|| {
                        let current = self.tokens.peek().unwrap();
                        self.err_fmt.format(
                            current.span,
                            format!("expected ';', but received: {}.", current),
                        )
                    })?;

                Ok(stmt)
            }
            _ => Err(self.err_fmt.format(
                current.span,
                format!("unexpected token found in statement parse: {}.", current),
            )),
        }
    }

    fn parse_declare_var_statement(&mut self) -> Result<Statement, String> {
        let var_token = self.tokens.next().unwrap();

        let constant = var_token.kind == TokenKind::Const;

        let identifier = self
            .tokens
            .next_if(|t| t.kind == TokenKind::Identifier)
            .ok_or_else(|| {
                let current = self.tokens.peek().unwrap();
                self.err_fmt.format(
                    current.span,
                    format!("expected identifier, but received: {}.", current),
                )
            })?;

        let punc = self
            .tokens
            .next_if(|t| {
                matches!(
                    t.kind,
                    TokenKind::Colon | TokenKind::Equal | TokenKind::SemiColon
                )
            })
            .ok_or_else(|| {
                let current = self.tokens.peek().unwrap();
                self.err_fmt.format(
                    current.span,
                    format!("expected ('=' or '=' or ';'), but received: {}.", current),
                )
            })?;

        if punc.kind == TokenKind::Equal || punc.kind == TokenKind::SemiColon {
            let value = if punc.kind == TokenKind::Equal {
                Some(self.parse_expr()?)
            } else {
                None
            };

            if punc.kind == TokenKind::Equal {
                self.tokens
                    .next_if(|t| t.kind == TokenKind::SemiColon)
                    .ok_or_else(|| {
                        let current = self.tokens.peek().unwrap();
                        self.err_fmt.format(
                            current.span,
                            format!("expected ';', but received: {}.", current),
                        )
                    })?;
            }

            return Ok(Statement::DeclareVariable {
                constant,
                identifier,
                type_descriptor: None,
                value,
            });
        }

        let type_descriptor = self.parse_type_anotation()?;
        
        let punc = self
            .tokens
            .next_if(|t| t.kind == TokenKind::Equal || t.kind == TokenKind::SemiColon)
            .ok_or_else(|| {
                let current = self.tokens.peek().unwrap();
                self.err_fmt.format(
                    current.span,
                    format!("expected ('=' or ';'), but received: {}.", current),
                )
            })?;

        let value = if punc.kind == TokenKind::Equal {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.tokens
            .next_if(|t| t.kind == TokenKind::SemiColon)
            .ok_or_else(|| {
                let current = self.tokens.peek().unwrap();
                self.err_fmt.format(
                    current.span,
                    format!("expected ';', but received: {}", current),
                )
            })?;

        Ok(Statement::DeclareVariable {
            constant,
            identifier,
            type_descriptor: Some(type_descriptor),
            value,
        })
    }

    fn parse_type_anotation(&mut self) -> Result<TypeDescriptor, String> {
        let mut lifetime: Option<Token> = None;
        
        if self.tokens.next_if(|t| t.kind == TokenKind::At).is_some() {
            lifetime = Some(self.tokens.next_if(|t| t.kind == TokenKind::Identifier).ok_or_else(|| {
                let current = self.tokens.peek().unwrap();
                self.err_fmt.format(current.span, format!("expected Identifier, but received: {}", current))
            })?);
        }

        let identifier = self.tokens.next_if(|t| t.kind == TokenKind::Identifier).ok_or_else(|| {
                let current = self.tokens.peek().unwrap();
                self.err_fmt.format(current.span, format!("expected Identifier, but received: {}", current))
        })?;

        Ok(TypeDescriptor {
            identifier,
            lifetime,
            comptime: false
        })
    }

    fn parse_block_statement(&mut self) -> Result<Statement, String> {
        self.tokens.next().unwrap();

        let mut statements: Vec<Statement> = Vec::with_capacity(128);
        let label = if self
            .tokens
            .next_if(|t| t.kind == TokenKind::Colon)
            .is_some()
        {
            let label = self
                .tokens
                .next_if(|t| t.kind == TokenKind::Identifier)
                .ok_or_else(|| {
                    let current = self.tokens.peek().unwrap();
                    self.err_fmt.format(
                        current.span,
                        format!("expected identifier, but received: {}.", current),
                    )
                })?;

            Some(label)
        } else {
            None
        };

        while self
            .tokens
            .peek()
            .is_some_and(|t| t.kind != TokenKind::End && t.kind != TokenKind::Eof)
        {
            statements.push(self.parse_statement()?);
        }

        self.tokens
            .next_if(|t| t.kind == TokenKind::End)
            .ok_or_else(|| {
                let current = self.tokens.peek().unwrap();
                self.err_fmt.format(
                    current.span,
                    format!("expected 'end', but received: {}.", current),
                )
            })?;

        Ok(Statement::Block { label, statements })
    }

    fn parse_expr(&mut self) -> Result<Expression, String> {
        let current = self.tokens.peek().unwrap();

        match current.kind {
            TokenKind::Integer | TokenKind::Float | TokenKind::String => self.parse_literal(),
            TokenKind::Identifier => self.parse_identifier(),
            _ => Err(self.err_fmt.format(
                current.span,
                format!("unexpected token found in expression parse: {}.", current),
            )),
        }
    }

    fn parse_literal(&mut self) -> Result<Expression, String> {
        let value = self
            .tokens
            .next_if(|t| {
                matches!(
                    t.kind,
                    TokenKind::Integer | TokenKind::Float | TokenKind::String
                )
            })
            .ok_or_else(|| {
                let current = self.tokens.peek().unwrap();
                self.err_fmt.format(
                    current.span,
                    format!("expected literal, but received: {}", current),
                )
            })?;

        Ok(Expression::Literal { value })
    }

    fn parse_identifier(&mut self) -> Result<Expression, String> {
        let identifier = self.tokens.next().ok_or_else(|| {
            let current = self.tokens.peek().unwrap();
            self.err_fmt.format(
                current.span,
                format!("expected identifier, but received: {}.", current),
            )
        })?;

        if self
            .tokens
            .peek()
            .is_some_and(|t| t.kind == TokenKind::Equal)
        {
            return self.parse_assignment_expr(identifier);
        }

        Ok(Expression::Identifier { value: identifier })
    }

    fn parse_assignment_expr(&mut self, identifier: Token) -> Result<Expression, String> {
        let equal = self.tokens.next().unwrap();
        let expr = self.parse_expr()?;

        Ok(Expression::VariableAssignment {
            left: identifier,
            equal,
            right: Box::new(expr),
        })
    }
}
