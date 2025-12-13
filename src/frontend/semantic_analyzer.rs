use crate::common::{
    ast::*,
    error_formatter::ErrorFormatter,
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
}

type SemaStmtResult = Result<Statement, String>;
type SemaExprResult = Result<Expression, String>;

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
            descriptor_typing = Some(Type::from_type_descriptor(d).ok_or_else(|| {
                self.err_fmt.format(
                    d.identifier.span,
                    format!("invalid type provided: {}", d.identifier.lexeme),
                )
            })?);
        }

        let mut expr: Option<Expression> = None;
        if let Some(e) = &stmt.value {
            expr = Some(self.visit_expr(e)?);
        }

        let value_typing = expr.as_ref().map(|e| e.get_typing().unwrap());
        if descriptor_typing.is_none() && value_typing.is_none() {
            return Err(self.err_fmt.format(
                stmt.identifier.span,
                "uninitialized variables must have type descriptors.".to_string(),
            ));
        }

        if let (Some(v_type), Some(d_type)) = (value_typing, descriptor_typing.clone())
            && !v_type.is_compatible_with(&d_type)
        {
            return Err(self.err_fmt.format(
                stmt.type_descriptor.as_ref().unwrap().identifier.span,
                format!("type {} is not assignable to type {}", v_type, d_type),
            ));
        }

        let typing = descriptor_typing.or(value_typing.cloned()).unwrap();

        let scope = self.current_scope_mut();
        let symbol = Symbol {
            constant: stmt.constant,
            identifier: stmt.identifier.clone(),
            typing: typing.clone(),
        };

        scope
            .insert(&stmt.identifier.lexeme, symbol)
            .map_err(|e| self.err_fmt.format(stmt.identifier.span, e))?;

        let data = DeclareVariableStatementData {
            typing: Some(typing),
            value: expr,
            ..stmt.clone()
        };

        Ok(Statement::DeclareVariable(data))
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStatementData) -> SemaStmtResult {
        let label_opt = stmt.label.as_ref().map(|t| t.lexeme.clone());
        self.open_scope(label_opt.clone());

        let mut tir_stmts = Vec::with_capacity(stmt.statements.len());
        for stmt in stmt.statements.iter() {
            tir_stmts.push(self.visit_stmt(stmt)?);
        }

        self.close_scope();

        let data = BlockStatementData {
            statements: tir_stmts,
            ..stmt.clone()
        };

        Ok(Statement::Block(data))
    }

    fn visit_expression_stmt(&mut self, stmt: &ExpressionStatementData) -> SemaStmtResult {
        let expr = self.visit_expr(&stmt.expr)?;
        let data = ExpressionStatementData {
            typing: expr.get_typing().cloned(),
            expr,
            ..stmt.clone()
        };

        Ok(Statement::Expression(data))
    }
}

impl ExpressionVisitor<SemaExprResult> for SemanticAnalyzer {
    fn visit_expr(&mut self, expr: &Expression) -> SemaExprResult {
        match expr {
            Expression::VariableAssignment(d) => self.visit_variable_assignment_expr(d),
            Expression::Literal(d) => self.visit_literal_expr(d),
            Expression::Identifier(d) => self.visit_identifier_expr(d),
            Expression::ArrayLiteral(d) => self.visit_array_literal_expr(d),
            Expression::ArrayAccess(d) => self.visit_array_access_expr(d),
            Expression::BuiltinCall(d) => self.visit_builtin_call_expr(d),
            Expression::Dereference(d) => self.visit_deref_expr(d),
            Expression::Reference(d) => self.visit_ref_expr(d),
        }
    }

    fn visit_variable_assignment_expr(
        &mut self,
        expr: &VariableAssignmentExpressionData,
    ) -> SemaExprResult {
        let identifier = self.resolve_identifier(&expr.left);

        let symbol = self.lookup(&identifier.lexeme).ok_or_else(|| {
            self.err_fmt.format(
                identifier.span,
                format!(
                    "cannot assign to undeclared variable: {}",
                    identifier.lexeme
                ),
            )
        })?;

        if symbol.constant {
            return Err(self.err_fmt.format(
                identifier.span,
                "cannot assign value to constant".to_string(),
            ));
        }

        let value = self.visit_expr(&expr.right)?;
        let value_typing = value.get_typing().unwrap();

        if !value_typing.is_compatible_with(&symbol.typing) {
            return Err(self.err_fmt.format(
                expr.equal.span,
                format!(
                    "type {} is not assignable to type {}.",
                    value_typing, symbol.typing
                ),
            ));
        }

        let data = VariableAssignmentExpressionData {
            typing: Some(value_typing.clone()),
            right: Box::new(value),
            ..expr.clone()
        };

        Ok(Expression::VariableAssignment(data))
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpressionData) -> SemaExprResult {
        let data = LiteralExpressionData {
            typing: Type::from_literal_expr(expr),
            ..expr.clone()
        };

        Ok(Expression::Literal(data))
    }

    fn visit_identifier_expr(&mut self, expr: &IdentifierExpressionData) -> SemaExprResult {
        let symbol = self.lookup(&expr.value.lexeme).ok_or_else(|| {
            self.err_fmt.format(
                expr.value.span,
                format!("undeclared variable: {}", expr.value.lexeme),
            )
        })?;

        let data = IdentifierExpressionData {
            typing: Some(symbol.typing),
            ..expr.clone()
        };

        Ok(Expression::Identifier(data))
    }

    fn visit_array_literal_expr(&mut self, expr: &ArrayLiteralExpressionData) -> SemaExprResult {
        let mut expected_typing: Option<Type> = None;
        let mut values: Vec<Expression> = Vec::new();

        for expr in &expr.values {
            let current_expr = self.visit_expr(&expr)?;
            let current_typing = current_expr.get_typing().unwrap();

            if !current_typing.is_undefined() || !current_typing.is_null() {
                expected_typing = Some(current_typing.clone());
            }

            if expected_typing
                .clone()
                .is_some_and(|et| !current_typing.is_compatible_with(&et))
            {
                return Err(self.err_fmt.format(current_expr.get_span(), format!(
                    "the type of this array has been inferred as {}, and type {} is not assignable to that.",
                    expected_typing.unwrap(), current_typing
                )));
            }

            values.push(current_expr);
        }

        if expected_typing.clone().is_some_and(|t| t.is_undefined() || t.is_null()) {
            return Err(self.err_fmt.format(expr.span, format!("cannot create all-null or all-undefined arrays.")));
        }

        if expected_typing.clone().is_none() {
            return Err(self.err_fmt.format(expr.span, format!("array type cannot be inferred.")));
        }

        let pointer_type = PointerType::Array(PointerTypeData {
            capacity: Some(values.len()),
            mutable: true,
            nullable: false,
        });

        let rest = expr.clone();

        let mut expected_typing = expected_typing.unwrap();
        let type_data = expected_typing.get_mut_type_data().unwrap();
        type_data.pointer = Some(pointer_type);
        
        let data = ArrayLiteralExpressionData {
            values,
            typing: Some(expected_typing),
            ..rest
        };

        Ok(Expression::ArrayLiteral(data))
    }

    fn visit_array_access_expr(&mut self, expr: &ArrayAccessExpressionData) -> SemaExprResult {
        let identifier = self.resolve_identifier(&expr.left);

        let symbol = self.lookup(&identifier.lexeme).ok_or_else(|| {
            self.err_fmt.format(
                identifier.span,
                format!("undeclared variable: {}", identifier.lexeme),
            )
        })?;

        if !symbol.typing.is_array() {
            return Err(self.err_fmt.format(
                identifier.span,
                format!("the variable {} is not an array.", identifier.lexeme),
            ));
        }

        /*let array_type = match symbol.typing.get_type_data().unwrap().pointer.unwrap() {
            PointerType::Array(d) => d,
            _ => unreachable!()
        };*/

        let index = self.visit_expr(&expr.index)?;
        let index_typing = index.get_typing().unwrap();

        if !index_typing.is_integer() {
            return Err(self.err_fmt.format(
                identifier.span,
                format!(
                    "only can access array with an integer type, but received: {}.",
                    index_typing
                ),
            ));
        }

        let mut value_type = symbol.typing.clone();
        let type_data = value_type.get_mut_type_data().unwrap();
        type_data.pointer = None;

        let data = ArrayAccessExpressionData {
            left: Box::new(self.visit_expr(&expr.left)?),
            index: Box::new(index),
            typing: Some(value_type),
            ..expr.clone()
        };

        Ok(Expression::ArrayAccess(data))
    }

    fn visit_builtin_call_expr(&mut self, expr: &BuiltinCallExpressionData) -> SemaExprResult {
        todo!()
    }

    fn visit_deref_expr(&mut self, expr: &DereferenceExpressionData) -> SemaExprResult {
        todo!()
    }

    fn visit_ref_expr(&mut self, expr: &ReferenceExpressionData) -> SemaExprResult {
        todo!()
    }
}

impl SemanticAnalyzer {
    pub fn new(program: Vec<Statement>, source: String, filepath: String) -> Self {
        let scopes = vec![SymbolTable::new(Some("global".to_string()))];

        Self {
            program: program.into_iter().peekable(),
            err_fmt: ErrorFormatter::new("semantic-analyzer", source, filepath),
            scopes,
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

    pub fn analyze(&mut self) -> Result<Vec<Statement>, String> {
        let mut program = Vec::new();

        while let Some(stmt) = self.program.next() {
            let tir_stmt = self.visit_stmt(&stmt)?;
            program.push(tir_stmt);
        }

        Ok(program)
    }

    pub fn resolve_identifier(&self, expr: &Expression) -> Token {
        match expr {
            Expression::Identifier(d) => d.value.clone(),
            Expression::Dereference(d) => self.resolve_identifier(&d.identifier),
            Expression::ArrayAccess(d) => self.resolve_identifier(&d.left),
            _ => unreachable!(),
        }
    }
}
