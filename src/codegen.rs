use std::collections::HashMap;

use crate::{
    ast::Statement,
    semantic_analyzer::{TIRExpr, TIRStmt, Type},
};
use inkwell::{
    builder::Builder, context::Context, execution_engine::ExecutionEngine, module::Module, types::{AnyType, AsTypeRef, BasicTypeEnum, StringRadix}, values::{BasicValueEnum, PointerValue}, AddressSpace, OptimizationLevel
};

use crate::semantic_analyzer::TIRProgram;

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
    program: TIRProgram<'ctx>,
    scopes: Vec<HashMap<String, PointerValue<'ctx>>>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module_name: &str,
        opt_level: OptimizationLevel,
        program: TIRProgram<'ctx>,
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
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[], false);
        let func = self.module.add_function("main", fn_type, None);

        let entry = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry);

        let statements = self.program.statements.clone();
        for stmt in statements {
            self.gen_stmt(&stmt)?;
        }

        _ = self
            .builder
            .build_return(Some(&i32_type.const_int(0, false)));

        Ok(())
    }

    fn get_llvm_type(&self, typing: Type) -> BasicTypeEnum<'ctx> {
        match typing {
            Type::Int8 | Type::UInt8 => self.context.i8_type().into(),
            Type::Int16 | Type::UInt16 => self.context.i16_type().into(),
            Type::Int32 | Type::UInt32 => self.context.i32_type().into(),
            Type::Int64 | Type::UInt64 => self.context.i64_type().into(),
            Type::Float32 => self.context.f32_type().into(),
            Type::Float64 => self.context.f64_type().into(),
            Type::String => self.context.ptr_type(AddressSpace::from(0)).into(),
        }
    }

    fn gen_stmt(&mut self, stmt: &TIRStmt<'ctx>) -> Result<(), String> {
        match stmt {
            TIRStmt::Declare {
                name,
                constant,
                typing,
                value,
                llvm_value,
            } => {
                let llvm_typing = self.get_llvm_type(*typing);
                let ptr = self.builder.build_alloca(llvm_typing, name).unwrap();
                let scope = self.current_scope_mut();

                scope.insert(name.clone(), ptr);

                if let Some(e) = value {
                    let llvm_typing = self.gen_expr(e)?;
                    self.builder.build_store(ptr, llvm_typing);
                }

                Ok(())
            }
            TIRStmt::Expression(expr) => self.gen_expr(expr).map(|_| {}),
            TIRStmt::Block { label, statements } => {
                self.open_scope();
                for stmt in statements {
                    self.gen_stmt(stmt)?;
                }
                self.close_scope();

                Ok(())
            },
        }
    }

    fn gen_expr(&mut self, expr: &TIRExpr<'ctx>) -> Result<BasicValueEnum<'ctx>, String> {
        match expr {
            TIRExpr::Literal { value, typing } => {
                match typing {
                    Type::UInt8 | Type::Int8 => 
                        Ok(self.context.i8_type().const_int_from_string(&value.lexeme, StringRadix::Alphanumeric).unwrap().into()),
                    Type::UInt16 | Type::Int16 =>
                        Ok(self.context.i16_type().const_int_from_string(&value.lexeme, StringRadix::Alphanumeric).unwrap().into()),
                    Type::UInt32 | Type::Int32 =>
                        Ok(self.context.i32_type().const_int_from_string(&value.lexeme, StringRadix::Alphanumeric).unwrap().into()),
                    Type::UInt64 | Type::Int64 =>
                        Ok(self.context.i64_type().const_int_from_string(&value.lexeme, StringRadix::Alphanumeric).unwrap().into()),
                    Type::Float32 =>
                        unsafe { Ok(self.context.f32_type().const_float_from_string(&value.lexeme).into()) },
                    Type::Float64 =>
                        unsafe { Ok(self.context.f64_type().const_float_from_string(&value.lexeme).into()) },
                    _  =>
                        unreachable!(),
                }
            },
            TIRExpr::Variable {
                name,
                typing,
                llvm_value,
            } => {
                let ptr = self.lookup(name).unwrap();
                let llvm_typing = self.get_llvm_type(*typing);
                
                Ok(self.builder.build_load(llvm_typing, ptr, name).unwrap())
            },
            TIRExpr::Assign {
                name,
                value,
                typing,
                llvm_value,
            } => {
                let val = self.gen_expr(value)?;
                let ptr = self.lookup(name).unwrap();

                self.builder.build_store(ptr, val);
                Ok(val)
            },
        }
    }
}
