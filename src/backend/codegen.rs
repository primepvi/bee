use std::collections::HashMap;

use inkwell::{
    builder::Builder, context::Context, execution_engine::ExecutionEngine, module::{Linkage, Module}, types::{AnyType, AsTypeRef, BasicType, BasicTypeEnum}, values::{BasicValueEnum, PointerValue}, AddressSpace, OptimizationLevel
};

use crate::common::{ast::*, visitors::*};

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
    program: Vec<Statement>,
    scopes: Vec<HashMap<String, PointerValue<'ctx>>>,
}

type CodegenStmtResult = Result<(), String>;
type CodegenExprResult<'ctx> = Result<BasicValueEnum<'ctx>, String>;

impl<'ctx> StatementVisitor<CodegenStmtResult> for Codegen<'ctx> {
    fn visit_stmt(&mut self, stmt: &Statement) -> CodegenStmtResult {
        match stmt {
            Statement::DeclareVariable(d) => self.visit_declare_variable_stmt(d),
            Statement::Block(d) => self.visit_block_stmt(d),
            Statement::Expression(d) => self.visit_expression_stmt(d),
        }
    }

    fn visit_declare_variable_stmt(
        &mut self,
        stmt: &DeclareVariableStatementData,
    ) -> CodegenStmtResult {
        let typing = stmt.typing.clone().unwrap();

        let llvm_typing = self.get_llvm_type(typing.clone());
        let ptr = self
            .builder
            .build_alloca(llvm_typing, &stmt.identifier.lexeme)
            .unwrap();
        let scope = self.current_scope_mut();

        scope.insert(stmt.identifier.lexeme.clone(), ptr);

        if let Some(e) = &stmt.value {
            let llvm_typing = self.visit_expr(e)?;
            self.builder.build_store(ptr, llvm_typing).unwrap();
        }

        Ok(())
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStatementData) -> CodegenStmtResult {
        self.open_scope();

        for stmt in stmt.statements.clone() {
            self.visit_stmt(&stmt)?;
        }

        self.close_scope();

        Ok(())
    }

    fn visit_expression_stmt(&mut self, stmt: &ExpressionStatementData) -> CodegenStmtResult {
        self.visit_expr(&stmt.expr).map(|_| {})
    }
}

impl<'ctx> ExpressionVisitor<CodegenExprResult<'ctx>> for Codegen<'ctx> {
    fn visit_expr(&mut self, expr: &Expression) -> CodegenExprResult<'ctx> {
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

    fn visit_variable_assignment_expr(
        &mut self,
        expr: &VariableAssignmentExpressionData,
    ) -> CodegenExprResult<'ctx> {
        let val = self.visit_expr(&expr.right)?;
        let ptr = self.lookup(&expr.left.lexeme).unwrap();

        _ = self.builder.build_store(ptr, val).unwrap();
        Ok(val)
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpressionData) -> CodegenExprResult<'ctx> {
        let typing = expr.typing.clone().unwrap();

        if let Type::String = typing.without_static() {
            let g = self
                .builder
                .build_global_string_ptr(&expr.value.lexeme, "str")
                .unwrap();
            return Ok(g.as_pointer_value().into());
        }

        match self.get_llvm_type(typing.clone()) {
            BasicTypeEnum::FloatType(float_type) => {
                let value: f64 = expr.value.lexeme.parse().unwrap();
                Ok(float_type.const_float(value).into())
            }
            BasicTypeEnum::IntType(int_type) => {
                let value: u64 = expr.value.lexeme.parse().unwrap();
                Ok(int_type.const_int(value, false).into())
            }
            _ => unreachable!(),
        }
    }

    fn visit_identifier_expr(
        &mut self,
        expr: &IdentifierExpressionData,
    ) -> CodegenExprResult<'ctx> {
        let typing = expr.typing.clone().unwrap();
        let ptr = self.lookup(&expr.value.lexeme).unwrap();
        let llvm_typing = self.get_llvm_type(typing.clone());

        Ok(self
            .builder
            .build_load(llvm_typing, ptr, &expr.value.lexeme)
            .unwrap())
    }

    fn visit_array_literal_expr(&mut self, expr: &ArrayLiteralExpressionData) -> CodegenExprResult<'ctx> {
        todo!()
    }

    fn visit_array_access_expr(&mut self, expr: &ArrayAccessExpressionData) -> CodegenExprResult<'ctx> {
        todo!()
    }

    fn visit_deref_expr(&mut self, expr: &DereferenceExpressionData) -> CodegenExprResult<'ctx> {
        todo!()
    }

    fn visit_ref_expr(&mut self, expr: &ReferenceExpressionData) -> CodegenExprResult<'ctx> {
        todo!()
    }

    fn visit_builtin_call_expr(&mut self, expr: &BuiltinCallExpressionData) -> CodegenExprResult<'ctx> {
        todo!()
    }
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module_name: &str,
        opt_level: OptimizationLevel,
        program: Vec<Statement>,
    ) -> Self {
        let module = context.create_module(module_name);
        let execution_engine = module.create_jit_execution_engine(opt_level).unwrap();

        Self {
            context,
            module,
            builder: context.create_builder(),
            execution_engine,
            program,
            scopes: vec![HashMap::new()],
        }
    }

    fn open_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn close_scope(&mut self) {
        self.scopes.pop().expect("internal err: pop empty scope.");
    }

    fn current_scope_mut(&mut self) -> &mut HashMap<String, PointerValue<'ctx>> {
        self.scopes
            .last_mut()
            .expect("internal err: pop empty scope.")
    }

    fn lookup(&self, name: &str) -> Option<PointerValue<'ctx>> {
        for scope in self.scopes.iter().rev() {
            if let Some(s) = scope.get(name) {
                return Some(*s);
            }
        }
        None
    }

    pub fn generate(&mut self) -> Result<(), String> {
        let fat_ptr = self.context.opaque_struct_type("FatPtr");
        fat_ptr.set_body(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.i64_type().into(),
            ],
            false,
        );
        
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let func = self.module.add_function("main", fn_type, None);

        let entry = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry);

        for stmt in self.program.clone() {
            self.visit_stmt(&stmt)?;
        }

        _ = self
            .builder
            .build_return(Some(&i32_type.const_int(0, false)));

        Ok(())
    }

    fn get_llvm_type(&self, typing: &Type) -> BasicTypeEnum<'ctx> {
        if typing.is_pointer() {
            return match typing.get_type_data().unwrap().pointer.clone().unwrap() {
                PointerType::Thin(d) => self.context.ptr_type(AddressSpace::default()).into(),
                PointerType::Fat(d) => self.context.get_struct_type("FatPtr").unwrap().into(),
                _ => unreachable!()
            };
        }
        
        let t: BasicTypeEnum<'ctx> = match typing {
            Type::Int8(_) | Type::UInt8(_) => self.context.i8_type().into(),
            Type::Int16(_) | Type::UInt16(_) => self.context.i16_type().into(),
            Type::Int32(_) | Type::UInt32(_) => self.context.i32_type().into(),
            Type::Int64(_) | Type::UInt64(_) => self.context.i64_type().into(),
            Type::Float32(_) => self.context.f32_type().into(),
            Type::Float64(_) => self.context.f64_type().into(),
            Type::String(_) => self.context.ptr_type(AddressSpace::from(0)).into(),
            Type::Bool(_) => self.context.bool_type().into(),
            _ => unreachable!()
        };

        if typing.is_array() {
            t.array_type(0).as_basic_type_enum()
        } else {
            t
        }   
    }
}
