use std::{
    iter::{Iterator, Peekable},
    vec::IntoIter,
};

use crate::common::{ast::*, error_formatter::ErrorFormatter};

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

    fn expect_error(&self, expected: &str, current: &Token) -> String {
        self.err_fmt.format(
            current.span,
            format!("expected {expected}, but received: {current}."),
        )
    }

    fn get_current_token(&mut self) -> &Token {
        self.tokens.peek().unwrap()
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, String> {
        let current = self.get_current_token().clone();
        self.tokens
            .next_if(|t| t.kind == kind)
            .ok_or_else(|| self.expect_error(&kind.to_string(), &current))
    }

    fn expect_any<F>(&mut self, expected: &str, pred: F) -> Result<Token, String>
    where
        F: Fn(&TokenKind) -> bool,
    {
        let current = self.get_current_token().clone();
        self.tokens
            .next_if(|t| pred(&t.kind))
            .ok_or_else(|| self.expect_error(expected, &current))
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
                self.parse_expr_stmt()
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
        let mut type_descriptor: Option<TypeDescriptor> = None;

        let identifier = self.expect(TokenKind::Identifier)?;
        let punc = self.expect_any("(':' or '=' or ';')", |k| {
            matches!(
                k,
                TokenKind::Colon | TokenKind::Equal | TokenKind::SemiColon
            )
        })?;

        if punc.kind == TokenKind::Equal || punc.kind == TokenKind::SemiColon {
            let mut value: Option<Expression> = None;
            if punc.kind == TokenKind::Equal {
                value = Some(self.parse_expr()?);
            }

            if punc.kind == TokenKind::Equal {
                self.expect(TokenKind::SemiColon)?;
            }

            let span = Span {
                len: punc.span.col - var_token.span.col + 1,
                col: var_token.span.col,
                line: var_token.span.line,
            };

            let data = DeclareVariableStatementData {
                constant,
                identifier,
                type_descriptor,
                value,
                span,
            };

            return Ok(Statement::DeclareVariable(data));
        }

        type_descriptor = Some(self.parse_type_anotation()?);

        let punc = self.expect_any("('=' or ';')", |k| {
            matches!(k, TokenKind::Equal | TokenKind::SemiColon)
        })?;

        let mut value: Option<Expression> = None;
        if punc.kind == TokenKind::Equal {
            value = Some(self.parse_expr()?);
        }

        if punc.kind == TokenKind::Equal {
            self.expect(TokenKind::SemiColon)?;
        }

        let span = Span {
            len: punc.span.col - var_token.span.col + 1,
            col: var_token.span.col,
            line: var_token.span.line,
        };

        let data = DeclareVariableStatementData {
            constant,
            identifier,
            type_descriptor,
            value,
            span,
        };

        Ok(Statement::DeclareVariable(data))
    }

    fn parse_type_anotation(&mut self) -> Result<TypeDescriptor, String> {
        let mut lifetime: Option<Token> = None;

        if self.expect(TokenKind::At).is_ok() {
            lifetime = Some(self.expect(TokenKind::Identifier)?);
        }

        let identifier = self.expect(TokenKind::Identifier)?;

        Ok(TypeDescriptor {
            identifier,
            lifetime,
            comptime: false,
        })
    }

    fn parse_block_statement(&mut self) -> Result<Statement, String> {
        let begin = self.tokens.next().unwrap();

        let mut statements: Vec<Statement> = Vec::with_capacity(128);
        let mut label: Option<Token> = None;
        if self.expect(TokenKind::Colon).is_ok() {
            label = Some(self.expect(TokenKind::Identifier)?);
        }

        while self
            .tokens
            .peek()
            .is_some_and(|t| !matches!(t.kind, TokenKind::End | TokenKind::Eof))
        {
            statements.push(self.parse_statement()?);
        }

        self.expect(TokenKind::End)?;

        let data = BlockStatementData {
            label,
            statements,
            span: begin.span,
        };

        Ok(Statement::Block(data))
    }

    fn parse_expr_stmt(&mut self) -> Result<Statement, String> {
        let expr = self.parse_expr()?;
        let span = expr.get_span();

        let data = ExpressionStatementData { expr, span };

        self.expect(TokenKind::SemiColon)?;

        Ok(Statement::Expression(data))
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
        let value = self.expect_any("literal", |k| {
            matches!(k, TokenKind::Integer | TokenKind::Float | TokenKind::String)
        })?;

        let span = value.span;
        let data = LiteralExpressionData { value, span };

        Ok(Expression::Literal(data))
    }

    fn parse_identifier(&mut self) -> Result<Expression, String> {
        let identifier = self.expect(TokenKind::Identifier)?;

        if self
            .tokens
            .peek()
            .is_some_and(|t| t.kind == TokenKind::Equal)
        {
            return self.parse_assignment_expr(identifier);
        }

        let span = identifier.span;
        let data = IdentifierExpressionData {
            value: identifier,
            span,
        };

        Ok(Expression::Identifier(data))
    }

    fn parse_assignment_expr(&mut self, identifier: Token) -> Result<Expression, String> {
        let equal = self.tokens.next().unwrap();
        let expr = self.parse_expr()?;

        let span = Span {
            len: expr.get_span().col - identifier.span.col + 1,
            line: equal.span.line,
            col: identifier.span.col,
        };

        let data = VariableAssignmentExpressionData {
            left: identifier,
            equal,
            right: Box::new(expr),
            span
        };

        Ok(Expression::VariableAssignment(data))
    }
}
