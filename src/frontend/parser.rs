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
            TokenKind::At | TokenKind::Identifier | TokenKind::Star => self.parse_expr_stmt(),
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
        let typing: Option<Type> = None;

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
                typing,
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
            typing,
        };

        Ok(Statement::DeclareVariable(data))
    }

    fn parse_type_anotation(&mut self) -> Result<TypeDescriptor, String> {
        let mut lifetime: Option<Token> = None;
        let mut ptr: Option<Pointer> = None;

        if self.expect(TokenKind::At).is_ok() {
            lifetime = Some(self.expect(TokenKind::Identifier)?);
        }

        if self.expect(TokenKind::Star).is_ok() {
            // thin pointer
            let ptr_descriptor = PointerDescriptor {
                nullable: self.expect(TokenKind::Question).is_ok(),
                capacity: None,
                mutable: self.expect(TokenKind::Var).is_ok(),
            };

            ptr = Some(Pointer::Thin(ptr_descriptor));
        } else if self.expect(TokenKind::OpenBrace).is_ok() {
            // fat pointer or array
            let mut ptr_descriptor = PointerDescriptor {
                nullable: false,
                capacity: None,
                mutable: false,
            };

            if self.expect(TokenKind::Star).is_ok() {
                // fat pointer
                ptr_descriptor.nullable = self.expect(TokenKind::Question).is_ok();
                self.expect(TokenKind::CloseBrace)?;
                ptr_descriptor.mutable = self.expect(TokenKind::Var).is_ok();
                ptr = Some(Pointer::Fat(ptr_descriptor));
            } else {
                // array
                if let Ok(cap_token) = self.expect(TokenKind::Integer) {
                    ptr_descriptor.capacity = Some(cap_token);
                }

                self.expect(TokenKind::CloseBrace)?;
                ptr_descriptor.mutable = self.expect(TokenKind::Var).is_ok();
                ptr = Some(Pointer::Array(ptr_descriptor));
            }
        }

        let identifier = self.expect(TokenKind::Identifier)?;
        let optional = self.expect(TokenKind::Question).is_ok();

        Ok(TypeDescriptor {
            identifier,
            lifetime,
            comptime: false,
            optional,
            pointer: ptr,
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
            typing: None,
        };

        Ok(Statement::Block(data))
    }

    fn parse_expr_stmt(&mut self) -> Result<Statement, String> {
        let expr = self.parse_expr()?;
        let span = expr.get_span();

        let data = ExpressionStatementData {
            expr,
            span,
            typing: None,
        };

        self.expect(TokenKind::SemiColon)?;

        Ok(Statement::Expression(data))
    }

    fn parse_expr(&mut self) -> Result<Expression, String> {
        let current = self.tokens.peek().unwrap();

        match current.kind {
            TokenKind::At => self.parse_builtin_call_expr(),
            TokenKind::Star => self.parse_deref_expr(),
            TokenKind::Ampersand => self.parse_ref_expr(),
            TokenKind::OpenBrace => self.parse_array_expr(),
            TokenKind::Integer
            | TokenKind::Float
            | TokenKind::String
            | TokenKind::Undefined
            | TokenKind::Null
            | TokenKind::True
            | TokenKind::False => self.parse_literal(),
            TokenKind::Identifier => self.parse_identifier(),
            _ => Err(self.err_fmt.format(
                current.span,
                format!("unexpected token found in expression parse: {}.", current),
            )),
        }
    }

    fn parse_builtin_call_expr(&mut self) -> Result<Expression, String> {
        self.expect(TokenKind::At)?;

        let callee = self.expect(TokenKind::Identifier)?;
        let mut arguments: Vec<Expression> = Vec::new();

        self.expect(TokenKind::OpenParen)?;
        while !matches!(self.get_current_token().kind, TokenKind::Eof | TokenKind::CloseParen) {
            arguments.push(self.parse_expr()?);
            
            if self.expect(TokenKind::Comma).is_err() {
                break;
            }
        }
        self.expect(TokenKind::CloseParen)?;

        let data = BuiltinCallExpressionData {
            callee,
            arguments,
            typing: None
        };

        Ok(Expression::BuiltinCall(data))
    }

    fn parse_deref_expr(&mut self) -> Result<Expression, String> {
        self.expect(TokenKind::Star)?;
        
        let identifier = self.parse_expr()?;
        let span = identifier.get_span();
        
        let data = DereferenceExpressionData {
            identifier: Box::new(identifier),
            span,
            typing: None
        };

        let expr = Expression::Dereference(data);

        if self.get_current_token().kind == TokenKind::Equal {
            return self.parse_assignment_expr(expr);
        }

        Ok(expr)
    }

    fn parse_ref_expr(&mut self) -> Result<Expression, String> {
        self.expect(TokenKind::Ampersand)?;

        let mutable = self.expect(TokenKind::Var).is_ok();
        
        let identifier = self.parse_expr()?;
        let span = identifier.get_span();
        
        let data = ReferenceExpressionData {
            identifier: Box::new(identifier),
            mutable,
            span,
            typing: None
        };

        Ok(Expression::Reference(data))
    }

    fn parse_array_expr(&mut self) -> Result<Expression, String> {
        let open = self.expect(TokenKind::OpenBrace)?;
        let mut values: Vec<Expression> = Vec::new();
        
        while !matches!(self.get_current_token().kind, TokenKind::Eof | TokenKind::CloseBrace) {
            values.push(self.parse_expr()?);
            
            if self.expect(TokenKind::Comma).is_err() {
                break;
            }
        }

        let close = self.expect(TokenKind::CloseBrace)?;
        let span = Span {
            len: close.span.col - open.span.col + 1,
            line: close.span.line,
            col: open.span.col
        };
        
        let data = ArrayLiteralExpressionData {
            values,
            span,
            typing: None
        };

        Ok(Expression::ArrayLiteral(data))
    }

    fn parse_literal(&mut self) -> Result<Expression, String> {
        let value = self.expect_any("literal", |k| {
            matches!(
                k,
                TokenKind::Integer
                    | TokenKind::Float
                    | TokenKind::String
                    | TokenKind::Null
                    | TokenKind::Undefined
                    | TokenKind::True
                    | TokenKind::False
            )
        })?;

        let span = value.span;
        let data = LiteralExpressionData {
            value,
            span,
            typing: None,
        };

        Ok(Expression::Literal(data))
    }

    fn parse_identifier(&mut self) -> Result<Expression, String> {
        let identifier = self.expect(TokenKind::Identifier)?;

        let span = identifier.span;
        let data = IdentifierExpressionData {
            value: identifier,
            span,
            typing: None,
        };

        let expr = Expression::Identifier(data);

        if self.get_current_token().kind == TokenKind::Equal {
            return self.parse_assignment_expr(expr);
        }

        if self.get_current_token().kind == TokenKind::OpenBrace {
            return self.parse_array_access_expr(expr);
        }

        Ok(expr)
    }

    fn parse_assignment_expr(&mut self, identifier: Expression) -> Result<Expression, String> {
        let equal = self.tokens.next().unwrap();
        let expr = self.parse_expr()?;

        let span = Span {
            len: expr.get_span().col - identifier.get_span().col + 1,
            line: equal.span.line,
            col: identifier.get_span().col,
        };

        let data = VariableAssignmentExpressionData {
            left: Box::new(identifier),
            equal,
            right: Box::new(expr),
            span,
            typing: None,
        };

        Ok(Expression::VariableAssignment(data))
    }

    fn parse_array_access_expr(&mut self, left: Expression) -> Result<Expression, String> {
        let open = self.expect(TokenKind::OpenBrace)?;
        let index = self.parse_expr()?;
        let close = self.expect(TokenKind::CloseBrace)?;

        let span = Span {
            len: close.span.col - open.span.col + 1,
            line: close.span.line,
            col: open.span.col,
        };

        let data = ArrayAccessExpressionData {
            left: Box::new(left),
            index: Box::new(index),
            span,
            typing: None
        };

        let expr = Expression::ArrayAccess(data);

        if self.get_current_token().kind == TokenKind::Equal {
            return self.parse_assignment_expr(expr);
        }

        Ok(expr)
    }
}
