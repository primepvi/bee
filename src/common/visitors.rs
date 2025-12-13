use crate::common::ast::*;

pub trait StatementVisitor<T> {
    fn visit_stmt(&mut self, stmt: &Statement) -> T {
        match stmt {
            Statement::DeclareVariable(d) => self.visit_declare_variable_stmt(d),
            Statement::Block(d) => self.visit_block_stmt(d),
            Statement::Expression(d) => self.visit_expression_stmt(d),
            _ => unimplemented!(),
        }
    }
    
    fn visit_declare_variable_stmt(&mut self, stmt: &DeclareVariableStatementData) -> T;
    fn visit_block_stmt(&mut self, stmt: &BlockStatementData) -> T;
    fn visit_expression_stmt(&mut self, stmt: &ExpressionStatementData) -> T;
}

pub trait ExpressionVisitor<T> {
    fn visit_expr(&mut self, expr: &Expression) -> T {
        match expr {
            Expression::VariableAssignment(d) => self.visit_variable_assignment_expr(d),
            Expression::Literal(d) => self.visit_literal_expr(d),
            Expression::Identifier(d) => self.visit_identifier_expr(d),
            Expression::ArrayLiteral(d) => self.visit_array_literal_expr(d),
            Expression::ArrayAccess(d) => self.visit_array_access_expr(d),
            Expression::Dereference(d) => self.visit_deref_expr(d),
            Expression::Reference(d) => self.visit_ref_expr(d),
            Expression::BuiltinCall(d) => self.visit_builtin_call_expr(d),
        }
    }
    
    fn visit_variable_assignment_expr(&mut self, expr: &VariableAssignmentExpressionData) -> T;
    fn visit_literal_expr(&mut self, expr: &LiteralExpressionData) -> T;
    fn visit_identifier_expr(&mut self, expr: &IdentifierExpressionData) -> T;
    fn visit_array_literal_expr(&mut self, expr: &ArrayLiteralExpressionData) -> T;
    fn visit_array_access_expr(&mut self, expr: &ArrayAccessExpressionData) -> T;
    fn visit_deref_expr(&mut self, expr: &DereferenceExpressionData) -> T;
    fn visit_ref_expr(&mut self, expr: &ReferenceExpressionData) -> T;
    fn visit_builtin_call_expr(&mut self, expr: &BuiltinCallExpressionData) -> T;
}
