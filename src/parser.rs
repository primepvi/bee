use std::{
    iter::{Iterator, Peekable},
    vec::IntoIter,
};

use crate::ast::*;

pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut program: Vec<Statement> = Vec::with_capacity(128);

        while self.tokens.peek().is_some_and(|t| t.kind != TokenKind::Eof) {
            program.push(self.parse_statement());
        }

        program
    }

    fn parse_statement(&mut self) -> Statement {
        let current = self
            .tokens
            .peek()
            .expect("parser -> unexpected end of file.");

        match current.kind {
            TokenKind::Const | TokenKind::Var => self.parse_declare_var_statement(),
            TokenKind::Begin => self.parse_block_statement(),
            TokenKind::Identifier | TokenKind::Integer | TokenKind::Float | TokenKind::String => {
                let stmt = Statement::Expression {
                    expr: self.parse_expr(),
                };

                self.tokens
                    .next_if(|t| t.kind == TokenKind::SemiColon)
                    .expect("parser -> expected SemiColon token.");

                stmt
            }
            _ => panic!(
                "parser -> unexpected token found {} in parse_statement.",
                current
            ),
        }
    }

    fn parse_declare_var_statement(&mut self) -> Statement {
        let var_token = self
            .tokens
            .next()
            .expect("parser -> expected Const or Var token.");

        let constant = var_token.kind == TokenKind::Const;

        let identifier = self
            .tokens
            .next_if(|t| t.kind == TokenKind::Identifier)
            .expect("parser -> expected Identifier token.");

        let punc = self
            .tokens
            .next_if(|t| {
                t.kind == TokenKind::Colon
                    || t.kind == TokenKind::Equal
                    || t.kind == TokenKind::SemiColon
            })
            .expect("parser -> expected Colon or Equal or SemiColon token.");

        if punc.kind == TokenKind::Equal || punc.kind == TokenKind::SemiColon {
            let value = if punc.kind == TokenKind::Equal {
                Some(self.parse_expr())
            } else {
                None
            };

            self.tokens
                .next_if(|t| t.kind == TokenKind::SemiColon)
                .expect("parser -> expected SemiColon token.");

            return Statement::DeclareVariable {
                constant,
                identifier,
                type_descriptor: None,
                value,
            };
        }

        let type_identifier = self
            .tokens
            .next_if(|t| t.kind == TokenKind::Identifier)
            .expect("parser -> expected Identifier token.");

        let type_descriptor = TypeDescriptor {
            identifier: type_identifier,
            lifetime: None,
            comptime: false,
        };

        let punc = self
            .tokens
            .next_if(|t| t.kind == TokenKind::Equal || t.kind == TokenKind::SemiColon)
            .expect("parser -> expected Equal or SemiColon token.");

        let value = if punc.kind == TokenKind::Equal {
            Some(self.parse_expr())
        } else {
            None
        };

        self.tokens
            .next_if(|t| t.kind == TokenKind::SemiColon)
            .expect("parser -> expected SemiColon token.");

        Statement::DeclareVariable {
            constant,
            identifier,
            type_descriptor: Some(type_descriptor),
            value,
        }
    }

    fn parse_block_statement(&mut self) -> Statement {
        self.tokens.next().expect("parser -> expected Begin token.");

        let mut statements: Vec<Statement> = Vec::with_capacity(128);
        let label = if self
            .tokens
            .next_if(|t| t.kind == TokenKind::Colon)
            .is_some()
        {
            let label = self
                .tokens
                .next_if(|t| t.kind == TokenKind::Identifier)
                .expect("parser -> expected Identifier token.");
            Some(label)
        } else {
            None
        };

        while self
            .tokens
            .peek()
            .is_some_and(|t| t.kind != TokenKind::End && t.kind != TokenKind::Eof)
        {
            statements.push(self.parse_statement());
        }

        self.tokens
            .next_if(|t| t.kind == TokenKind::End)
            .expect("parser -> expected End token.");

        Statement::Block { label, statements }
    }

    fn parse_expr(&mut self) -> Expression {
        let current = self
            .tokens
            .peek()
            .expect("parser -> unexpected end of file.");

        match current.kind {
            TokenKind::Integer | TokenKind::Float | TokenKind::String => self.parse_literal(),
            TokenKind::Identifier => self.parse_identifier(),
            _ => panic!(
                "parser -> unexpected token found {} in parse expression.",
                current
            ),
        }
    }

    fn parse_literal(&mut self) -> Expression {
        let value = self
            .tokens
            .next_if(|t| {
                matches!(
                    t.kind,
                    TokenKind::Integer | TokenKind::Float | TokenKind::String
                )
            })
            .expect("parser -> expected literal token.");

        Expression::Literal { value }
    }

    fn parse_identifier(&mut self) -> Expression {
        let identifier = self
            .tokens
            .next()
            .expect("parser -> expected Identifier token.");
        if self
            .tokens
            .next_if(|t| t.kind == TokenKind::Equal)
            .is_some()
        {
            return self.parse_assignment_expr(identifier);
        }

        Expression::Identifier { value: identifier }
    }

    fn parse_assignment_expr(&mut self, identifier: Token) -> Expression {
        let expr = self.parse_expr();

        Expression::VariableAssignment {
            left: identifier,
            right: Box::new(expr),
        }
    }
}
