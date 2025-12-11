use crate::common::{
    ast::*,
    error_formatter::ErrorFormatter,
    tir::*,
    visitors::{ExpressionVisitor, StatementVisitor},
};

use std::{collections::HashMap, iter::Peekable, vec::IntoIter};

#[derive(Clone)]
pub struct Symbol {
    constant: bool,
    identifier: Token,
    typing: Type,
}

pub struct SymbolTable {
    pub label: Option<String>,
    pub symbols: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new(label: Option<String>) -> Self {
        Self {
            label,
            symbols: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: &str, symbol: Symbol) -> Result<(), String> {
        if self.symbols.contains_key(name) {
            return Err(format!(
                "symbol '{name}' already defined in scope: {}",
                self.label.clone().unwrap_or("unnamed".to_string())
            ));
        }
        self.symbols.insert(name.to_string(), symbol);
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Option<Symbol> {
        self.symbols.get(name).cloned()
    }
}

pub struct SemanticAnalyzer {
    program: Peekable<IntoIter<Statement>>,
    pub err_fmt: ErrorFormatter,
    scopes: Vec<SymbolTable>,
    tir: TIRProgram,
}

type SemaStmtResult = Result<TIRStmt, String>;
type SemaExprResult = Result<TIRExpr, String>;

impl StatementVisitor<SemaStmtResult> for SemanticAnalyzer {
    fn visit_stmt(&mut self, stmt: &Statement) -> SemaStmtResult {
        match stmt {
            Statement::DeclareVariable(d) => self.visit_declare_variable_stmt(d),
            Statement::Block(d) => self.visit_block_stmt(d),
            Statement::Expression(d) => self.visit_expression_stmt(d),
        }
    }

    fn visit_declare_variable_stmt(
        &mut self,
        stmt: &DeclareVariableStatementData,
    ) -> SemaStmtResult {
        let mut descriptor_typing: Option<Type> = None;
        if let Some(d) = &stmt.type_descriptor {
            descriptor_typing = Some(Type::from_descriptor(d.clone()).ok_or_else(|| {
                self.err_fmt.format(
                    d.identifier.span,
                    format!("invalid type provided: {}", d.identifier.lexeme),
                )
            })?);
        }

        let mut expr: Option<TIRExpr> = None;
        if let Some(e) = &stmt.value {
            expr = Some(self.visit_expr(e)?);
        }

        let value_typing = expr.as_ref().map(|e| e.get_typing());
        if descriptor_typing.is_none() && value_typing.is_none() {
            return Err(self.err_fmt.format(
                stmt.identifier.span,
                "uninitialized variables must have type descriptors.".to_string(),
            ));
        }

        if let (Some(v_type), Some(d_type)) = (value_typing, descriptor_typing.clone())
            && !v_type.compare(&d_type)
        {
            return Err(self.err_fmt.format(
                stmt.type_descriptor.as_ref().unwrap().identifier.span,
                format!("type descriptor is {}, but received {}", d_type, v_type),
            ));
        }

        let typing = descriptor_typing
            .or(value_typing.cloned())
            .unwrap();

        let scope = self.current_scope_mut();
        let symbol = Symbol {
            constant: stmt.constant,
            identifier: stmt.identifier.clone(),
            typing: typing.clone(),
        };
        scope
            .insert(&stmt.identifier.lexeme, symbol)
            .map_err(|e| self.err_fmt.format(stmt.identifier.span, e))?;

        Ok(TIRStmt::Declare {
            name: stmt.identifier.lexeme.clone(),
            constant: stmt.constant,
            typing,
            value: expr,
        })
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStatementData) -> SemaStmtResult {
        let label_opt = stmt.label.as_ref().map(|t| t.lexeme.clone());
        self.open_scope(label_opt.clone());

        let mut tir_stmts = Vec::with_capacity(stmt.statements.len());
        for stmt in stmt.statements.iter() {
            tir_stmts.push(self.visit_stmt(stmt)?);
        }

        self.close_scope();
        Ok(TIRStmt::Block {
            label: label_opt,
            statements: tir_stmts,
        })
    }

    fn visit_expression_stmt(&mut self, stmt: &ExpressionStatementData) -> SemaStmtResult {
        let tir_expr = self.visit_expr(&stmt.expr)?;
        Ok(TIRStmt::Expression(tir_expr))
    }
}

impl ExpressionVisitor<SemaExprResult> for SemanticAnalyzer {
    fn visit_expr(&mut self, expr: &Expression) -> SemaExprResult {
        match expr {
            Expression::VariableAssignment(d) => self.visit_variable_assignment_expr(d),
            Expression::Literal(d) => self.visit_literal_expr(d),
            Expression::Identifier(d) => self.visit_identifier_expr(d),
        }
    }

    fn visit_variable_assignment_expr(
        &mut self,
        expr: &VariableAssignmentExpressionData,
    ) -> SemaExprResult {
        let symbol = self.lookup(&expr.left.lexeme).ok_or_else(|| {
            self.err_fmt.format(
                expr.left.span,
                format!("cannot assign to undeclared variable: {}", expr.left.lexeme),
            )
        })?;

        if symbol.constant {
            return Err(self.err_fmt.format(
                expr.left.span,
                "cannot assign value to constant".to_string(),
            ));
        }

        let value = self.visit_expr(&expr.right)?;
        let value_typing = value.get_typing().clone();

        if value_typing != symbol.typing {
            return Err(self.err_fmt.format(
                expr.equal.span,
                format!(
                    "expected an {} value, but received an {} value",
                    symbol.typing, value_typing
                ),
            ));
        }

        Ok(TIRExpr::Assign {
            name: symbol.identifier.lexeme,
            value: Box::new(value),
            typing: value_typing,
        })
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpressionData) -> SemaExprResult {
        let typing = match expr.value.kind {
            TokenKind::Integer => Type::Int32,
            TokenKind::Float => Type::Float32,
            TokenKind::String => Type::Static(Box::new(Type::String)),
            _ => unreachable!(),
        };

        Ok(TIRExpr::Literal {
            value: expr.value.lexeme.clone(),
            typing,
        })
    }

    fn visit_identifier_expr(&mut self, expr: &IdentifierExpressionData) -> SemaExprResult {
        let symbol = self.lookup(&expr.value.lexeme).ok_or_else(|| {
            self.err_fmt
                .format(expr.value.span, format!("undeclared variable: {}", expr.value.lexeme))
        })?;

        Ok(TIRExpr::Variable {
            name: symbol.identifier.lexeme,
            typing: symbol.typing,
        })
    }
}

impl SemanticAnalyzer {
    pub fn new(program: Vec<Statement>, source: String, filepath: String) -> Self {
        let scopes = vec![SymbolTable::new(Some("global".to_string()))];

        Self {
            program: program.into_iter().peekable(),
            err_fmt: ErrorFormatter::new("semantic-analyzer", source, filepath),
            scopes,
            tir: TIRProgram {
                statements: Vec::new(),
            },
        }
    }

    fn open_scope(&mut self, label: Option<String>) {
        self.scopes.push(SymbolTable::new(label));
    }

    fn close_scope(&mut self) {
        self.scopes.pop().expect("internal err: pop empty scope.");
    }

    fn current_scope_mut(&mut self) -> &mut SymbolTable {
        self.scopes
            .last_mut()
            .expect("internal err: pop empty scope.")
    }

    fn lookup(&self, name: &str) -> Option<Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(s) = scope.lookup(name) {
                return Some(s);
            }
        }
        None
    }

    pub fn analyze(&mut self) -> Result<TIRProgram, String> {
        while let Some(stmt) = self.program.next() {
            let tir_stmt = self.visit_stmt(&stmt)?;
            self.tir.statements.push(tir_stmt);
        }
        Ok(self.tir.clone())
    }
}
