use core::fmt;
use std::{collections::HashMap, iter::Peekable, vec::IntoIter};

use inkwell::values::PointerValue;

use crate::{ast::*, error::ErrorFormatter};

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Type {
    UInt8, UInt16, UInt32, UInt64,
    Int8, Int16, Int32, Int64,
    Float32, Float64,
    String,
}

impl Type {
    pub fn from_descriptor(descriptor: TypeDescriptor) -> Option<Self> {
        match descriptor.identifier.lexeme.as_str() {
            "int8" => Some(Type::Int8),
            "int16" => Some(Type::Int16),
            "int32" => Some(Type::Int32),
            "int64" => Some(Type::Int64),
            "uint8" => Some(Type::UInt8),
            "uint16" => Some(Type::UInt16),
            "uint32" => Some(Type::UInt32),
            "uint64" => Some(Type::UInt64),
            "float32" => Some(Type::Float32),
            "float64" => Some(Type::Float64),
            "string" => Some(Type::String),
            _ => None,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::UInt8 => write!(f, "uint8"),
            Type::UInt16 => write!(f, "uint16"),
            Type::UInt32 => write!(f, "uint32"),
            Type::UInt64 => write!(f, "uint64"),
            Type::Int8 => write!(f, "int8"),
            Type::Int16 => write!(f, "int16"),
            Type::Int32 => write!(f, "int32"),
            Type::Int64 => write!(f, "int64"),
            Type::Float32 => write!(f, "float32"),
            Type::Float64 => write!(f, "float64"),
            Type::String => write!(f, "string"),
        }
    }
}

#[derive(Clone)]
pub enum TIRExpr<'ctx> {
    Literal {
        value: Token,
        typing: Type,
    },
    Variable {
        name: String,
        typing: Type,
        llvm_value: Option<PointerValue<'ctx>>,
    },
    Assign {
        name: String,
        value: Box<TIRExpr<'ctx>>,
        typing: Type,
        llvm_value: Option<PointerValue<'ctx>>,
    },
}

impl<'ctx> TIRExpr<'ctx> {
    pub fn get_typing(&self) -> Type {
        match self {
            TIRExpr::Literal { typing, .. } => *typing,
            TIRExpr::Variable { typing, .. } => *typing,
            TIRExpr::Assign { typing, .. } => *typing,
        }
    }
}

#[derive(Clone)]
pub enum TIRStmt<'ctx> {
    Declare {
        name: String,
        constant: bool,
        typing: Type,
        value: Option<TIRExpr<'ctx>>,
        llvm_value: Option<PointerValue<'ctx>>,
    },
    Expression(TIRExpr<'ctx>),
    Block {
        label: Option<String>,
        statements: Vec<TIRStmt<'ctx>>,
    },
}

#[derive(Clone)]
pub struct Symbol {
    constant: bool,
    identifier: Token,
    typing: Type,
}

#[derive(Clone)]
pub struct TIRProgram<'ctx> {
    pub statements: Vec<TIRStmt<'ctx>>,
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

pub struct SemanticAnalyzer<'ctx> {
    program: Peekable<IntoIter<Statement>>,
    pub err_fmt: ErrorFormatter,
    scopes: Vec<SymbolTable>,
    tir: TIRProgram<'ctx>,
}

impl<'ctx> SemanticAnalyzer<'ctx> {
    pub fn new(program: Vec<Statement>, source: String, filepath: String) -> Self {
        let scopes = vec![SymbolTable::new(Some("global".to_string()))];

        Self {
            program: program.into_iter().peekable(),
            err_fmt: ErrorFormatter::new("semantic-analyzer", source, filepath),
            scopes,
            tir: TIRProgram { statements: Vec::new() },
        }
    }

    fn open_scope(&mut self, label: Option<String>) {
        self.scopes.push(SymbolTable::new(label));
    }

    fn close_scope(&mut self) {
        self.scopes.pop().expect("internal err: pop empty scope.");
    }

    fn current_scope_mut(&mut self) -> &mut SymbolTable {
        self.scopes.last_mut().expect("internal err: pop empty scope.")
    }

    fn lookup(&self, name: &str) -> Option<Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(s) = scope.lookup(name) {
                return Some(s);
            }
        }
        None
    }

    pub fn analyze(&mut self) -> Result<TIRProgram<'ctx>, String> {
        while let Some(stmt) = self.program.next() {
            let tir_stmt = self.analyze_stmt(&stmt)?;
            self.tir.statements.push(tir_stmt);
        }
        Ok(self.tir.clone())
    }

    fn analyze_stmt(&mut self, stmt: &Statement) -> Result<TIRStmt<'ctx>, String> {
        match stmt {
            Statement::DeclareVariable { constant, identifier, type_descriptor, value } => {
                let descriptor_typing = if let Some(d) = type_descriptor {
                    Some(Type::from_descriptor(d.clone()).ok_or_else(|| {
                        self.err_fmt.format(
                            d.identifier.span,
                            format!("invalid type provided: {}", d.identifier.lexeme),
                        )
                    })?)
                } else { None };

                let expr = if let Some(e) = value {
                    Some(self.analyze_expr(e)?)
                } else { None };

                let value_typing = expr.as_ref().map(|e| e.get_typing());

                if descriptor_typing.is_none() && value_typing.is_none() {
                    return Err(self.err_fmt.format(
                        identifier.span,
                        "uninitialized variables must have type descriptors.".to_string(),
                    ));
                }

                if let (Some(v_type), Some(d_type)) = (value_typing, descriptor_typing) {
                    if v_type != d_type {
                        return Err(self.err_fmt.format(
                            type_descriptor.as_ref().unwrap().identifier.span,
                            format!("type descriptor is {}, but received {}", d_type, v_type),
                        ));
                    }
                }

                let typing = descriptor_typing.or(value_typing).unwrap();

                let scope = self.current_scope_mut();
                let symbol = Symbol { constant: *constant, identifier: identifier.clone(), typing };
                scope.insert(&identifier.lexeme, symbol)
                    .map_err(|e| self.err_fmt.format(identifier.span, e))?;

                Ok(TIRStmt::Declare {
                    name: identifier.lexeme.clone(),
                    constant: *constant,
                    typing,
                    value: expr,
                    llvm_value: None,
                })
            }

            Statement::Block { label, statements } => {
                let label_opt = label.as_ref().map(|t| t.lexeme.clone());
                self.open_scope(label_opt.clone());

                let mut tir_stmts = Vec::with_capacity(statements.len());
                for stmt in statements.iter() {
                    tir_stmts.push(self.analyze_stmt(stmt)?);
                }

                self.close_scope();
                Ok(TIRStmt::Block { label: label_opt, statements: tir_stmts })
            }

            Statement::Expression { expr } => {
                let tir_expr = self.analyze_expr(expr)?;
                Ok(TIRStmt::Expression(tir_expr))
            }
        }
    }

    fn analyze_expr(&mut self, expr: &Expression) -> Result<TIRExpr<'ctx>, String> {
        match expr {
            Expression::VariableAssignment { left, equal, right } => {
                let symbol = self.lookup(&left.lexeme)
                    .ok_or_else(|| self.err_fmt.format(
                        left.span,
                        format!("cannot assign to undeclared variable: {}", left.lexeme)
                    ))?;

                if symbol.constant {
                    return Err(self.err_fmt.format(left.span, "cannot assign value to constant".to_string()));
                }

                let value = self.analyze_expr(right)?;
                let value_typing = value.get_typing();

                if value_typing != symbol.typing {
                    return Err(self.err_fmt.format(equal.span, format!(
                        "expected an {} value, but received an {} value",
                        symbol.typing, value_typing
                    )));
                }

                Ok(TIRExpr::Assign {
                    name: symbol.identifier.lexeme,
                    value: Box::new(value),
                    typing: value_typing,
                    llvm_value: None,
                })
            }

            Expression::Literal { value } => {
                let typing = match value.kind {
                    TokenKind::Integer => Type::Int32,
                    TokenKind::Float => Type::Float32,
                    TokenKind::String => Type::String,
                    _ => unreachable!(),
                };
                Ok(TIRExpr::Literal { value: value.clone(), typing })
            }

            Expression::Identifier { value } => {
                let symbol = self.lookup(&value.lexeme)
                    .ok_or_else(|| self.err_fmt.format(value.span, format!("undeclared variable: {}", value.lexeme)))?;

                Ok(TIRExpr::Variable {
                    name: symbol.identifier.lexeme,
                    typing: symbol.typing,
                    llvm_value: None,
                })
            }
        }
    }
}
