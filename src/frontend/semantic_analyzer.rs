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

#[derive(Clone)]
pub enum LeftValue {
    Variable(Symbol),
    Deref {
        base: Expression,
        typing: Type,
        mutable: bool,
    },
    ArrayAccess {
        base: Expression,
        typing: Type,
        mutable: bool,
    },
}

impl LeftValue {
    pub fn is_mutable(&self) -> bool {
        match self {
            LeftValue::Variable(symbol) => !symbol.constant,
            LeftValue::Deref { mutable, .. } => *mutable,
            LeftValue::ArrayAccess { mutable, .. } => *mutable,
        }
    }

    pub fn get_typing(&self) -> &Type {
        match self {
            LeftValue::Variable(symbol) => &symbol.typing,
            LeftValue::Deref { typing, .. } => typing,
            LeftValue::ArrayAccess { typing, .. } => typing,
        }
    }
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
        let mut annotation_typing: Option<Type> = None;
        if let Some(t) = &stmt.type_annotation {
            annotation_typing = Some(Type::from_type_annotation(t).ok_or_else(|| {
                self.err_fmt.format(
                    t.get_span(),
                    format!("invalid type provided: '{}'", t),
                )
            })?);
        }

        let mut expr: Option<Expression> = None;
        if let Some(e) = &stmt.value {
            expr = Some(self.visit_expr(e)?);
        }

        let value_typing = expr.as_ref().map(|e| e.get_typing().unwrap());
        if annotation_typing.is_none() && value_typing.is_none() {
            return Err(self.err_fmt.format(
                stmt.identifier.span,
                "uninitialized variables must have type annotation.".to_string(),
            ));
        }

        if let (Some(v_type), Some(d_type)) = (value_typing, annotation_typing.clone())
            && !Type::compatible(v_type, &d_type)
        {
            return Err(self.err_fmt.format(
                stmt.type_annotation.clone().unwrap().get_span(),
                format!("type '{}' is not assignable to type '{}'", v_type, d_type),
            ));
        }

        let mut typing = annotation_typing.clone().or(value_typing.cloned()).unwrap();

        if stmt.constant && (stmt.value.is_none() || (typing.is_primitive() && typing.get_primitive().unwrap().optional)) {
            return Err(self
                .err_fmt
                .format(stmt.span, "constant cannot be undefined.".to_string()));
        }

        if stmt.value.is_none() && (typing.is_primitive() && !typing.get_primitive().unwrap().optional) {
            return Err(self.err_fmt.format(
                stmt.span,
                "non-optional variables cannot be undefined.".to_string(),
            ));
        }

        if typing.is_array() {
            let mut annotation_data: Option<usize> = None;
            let mut value_data: Option<usize> = None;

            if annotation_typing.clone().is_some_and(|t| t.is_array()) {
                annotation_data = annotation_typing.unwrap().get_array().map(|a| a.capacity).unwrap_or(None);
            }

            if value_typing.is_some_and(|t| t.is_array()) {
                value_data = value_typing.unwrap().get_array().map(|a| a.capacity).unwrap_or(None);
            }

            if annotation_data.clone().is_none()
                && value_data.clone().is_none()
            {
                return Err(self.err_fmt.format(
                    stmt.span,
                    "array capacity must be know in comptime.".to_string(),
                ));
            }

            let t = typing.get_mut_array().unwrap();
            
            t.capacity = annotation_data.or(value_data);
        }

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
        let left = self.visit_expr(&expr.left)?;
        let lvalue = self.analyze_left_value(&left)?;

        if !lvalue.is_mutable() {
            return Err(self.err_fmt.format(
                expr.left.get_span(),
                "cannot assign value to constant".to_string(),
            ));
        }

        let value = self.visit_expr(&expr.right)?;
        let value_typing = value.get_typing().unwrap();

        if !Type::compatible(value_typing, lvalue.get_typing()) {
            return Err(self.err_fmt.format(
                expr.equal.span,
                format!(
                    "type '{}' is not assignable to type '{}'.",
                    value_typing,
                    lvalue.get_typing()
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
                format!("undeclared variable: '{}'", expr.value.lexeme),
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
            let current_expr = self.visit_expr(expr)?;
            let current_typing = current_expr.get_typing().unwrap();

            if (!current_typing.is_undefined() || !current_typing.is_null())
                && expected_typing.is_none()
            {
                expected_typing = Some(current_typing.clone());
            }

            if expected_typing
                .clone()
                .is_some_and(|et| !Type::compatible(current_typing, &et))
            {
                return Err(self.err_fmt.format(current_expr.get_span(), format!(
                    "the type of this array elements has been inferred as '{}', and type '{}' is not assignable to that.",
                    expected_typing.unwrap(), current_typing
                )));
            }

            values.push(current_expr);
        }

        if expected_typing
            .clone()
            .is_some_and(|t| t.is_undefined() || t.is_null())
        {
            return Err(self.err_fmt.format(
                expr.span,
                "cannot create all-null or all-undefined arrays.".to_string(),
            ));
        }

        if expected_typing.clone().is_none() {
            return Err(self
                .err_fmt
                .format(expr.span, "array type cannot be inferred.".to_string()));
        }

        let pointer_type = Type::Array(ArrayType {
            capacity: Some(values.len()),
            inner: Box::new(expected_typing.unwrap()),
            mutable: true,
        });

        let data = ArrayLiteralExpressionData {
            values,
            typing: Some(pointer_type),
            ..expr.clone()
        };

        Ok(Expression::ArrayLiteral(data))
    }

    fn visit_array_access_expr(&mut self, expr: &ArrayAccessExpressionData) -> SemaExprResult {

        let left = self.visit_expr(&expr.left)?;
        let lvalue = self.analyze_left_value(&left)?;

        dbg!(lvalue.get_typing());
        if !lvalue.get_typing().is_array() {
            return Err(self.err_fmt.format(
                expr.left.get_span(),
                "cannot index non-array type".to_string(),
            ));
        }

        let index = self.visit_expr(&expr.index)?;
        let index_typing = index.get_typing().unwrap().get_primitive().unwrap();

        if !index_typing.is_integer() {
            return Err(self.err_fmt.format(
                expr.index.get_span(),
                format!(
                    "array index must be integer type, but received index of type '{}'.",
                    index_typing
                ),
            ));
        }

        let array_type = lvalue.get_typing().get_array().unwrap();
        
        let data = ArrayAccessExpressionData {
            left: Box::new(self.visit_expr(&expr.left)?),
            index: Box::new(index),
            typing: Some(*array_type.inner),
            ..expr.clone()
        };

        Ok(Expression::ArrayAccess(data))
    }

    fn visit_builtin_call_expr(&mut self, expr: &BuiltinCallExpressionData) -> SemaExprResult {
        todo!()
    }

    fn visit_deref_expr(&mut self, expr: &DereferenceExpressionData) -> SemaExprResult {
        let base = self.visit_expr(&expr.identifier)?;
        let base_type = base.get_typing().unwrap();

        if !base_type.is_pointer() {
            return Err(self.err_fmt.format(
                base.get_span(),
                "cannot dereference non-pointer type.".to_string(),
            ));
        }

        let ptr_type = base_type.get_pointer().unwrap();
        
        let data = DereferenceExpressionData {
            identifier: Box::new(base),
            typing: Some(*ptr_type.inner),
            ..expr.clone()
        };

        Ok(Expression::Dereference(data))
    }

    fn visit_ref_expr(&mut self, expr: &ReferenceExpressionData) -> SemaExprResult {
        let symbol = self.lookup(&expr.identifier.lexeme).ok_or_else(|| {
            self.err_fmt.format(
                expr.identifier.span,
                format!(
                    "cannot create an ref to undeclared variable: '{}'",
                    expr.identifier.lexeme
                ),
            )
        })?;

        let typing = symbol.typing;

        if typing.is_null() || typing.is_undefined() {
            return Err(self.err_fmt.format(
                expr.identifier.span,
                format!("cannot create a reference to type: '{}'.", typing),
            ));
        }

        if expr.mutable && symbol.constant {
            return Err(self.err_fmt.format(
                expr.identifier.span,
                "cannot create mutable reference of constant value.".to_string(),
            ));
        }

        
        let ref_ptr_kind = if typing.is_array() { PointerTypeKind::Fat } else { PointerTypeKind::Thin };
        let ref_ptr_cap = if typing.is_array() { typing.get_array().unwrap().capacity } else { None };

        let typing = if typing.is_array() {
            *typing.get_array().unwrap().inner
        } else {
            typing
        };
        
        let ref_typing = Type::Pointer(PointerType{
            kind: ref_ptr_kind, inner: Box::new(typing), capacity: ref_ptr_cap, mutable: expr.mutable, nullable: false
        });

        let data = ReferenceExpressionData {
            typing: Some(ref_typing),
            ..expr.clone()
        };

        Ok(Expression::Reference(data))
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

    pub fn analyze_left_value(&mut self, expr: &Expression) -> Result<LeftValue, String> {

        
        match expr.clone() {
            Expression::Identifier(d) => {
                let symbol = self.lookup(&d.value.lexeme).ok_or_else(|| {
                    self.err_fmt.format(
                        d.value.span,
                        format!("cannot assign to undeclared variable: '{}'", d.value.lexeme),
                    )
                })?;

                Ok(LeftValue::Variable(symbol))
            }
            Expression::Dereference(d) => {
                let ptr = d.identifier.get_typing().unwrap().get_pointer().unwrap();

                dbg!(d.typing.clone());

                Ok(LeftValue::Deref {
                    base: *d.identifier,
                    typing: d.typing.unwrap(),
                    mutable: ptr.mutable,
                })
            }
            Expression::ArrayAccess(d) => {
                let arr = d.left.get_typing().unwrap().get_array().unwrap();
                
                Ok(LeftValue::ArrayAccess {
                    base: *d.left,
                    typing: d.typing.unwrap(),
                    mutable: arr.mutable,
                })
            }
            _ => Err(self.err_fmt.format(
                expr.get_span(),
                "unexpected left-value expression has provided.".to_string(),
            )),
        }
    }
}
