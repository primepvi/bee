use core::fmt;
use std::{collections::HashMap, iter::Peekable, vec::IntoIter};

use crate::{ast::*, error::ErrorFormatter};

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    UInt8,
    UInt16,
    UInt32,
    UInt64,

    Int8,
    Int16,
    Int32,
    Int64,

    Float32,
    Float64,

    String,

    SubType { name: String, size: usize },
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
            Type::SubType { name, size } => write!(f, "{}", name),
        }
    }
}

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
                "symbol '{name}' already has defined in this scope: {}",
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
    err_fmt: ErrorFormatter,
    scopes: Vec<SymbolTable>,
}

impl SemanticAnalyzer {
    pub fn new(program: Vec<Statement>, source: &'static str) -> Self {
        let scopes: Vec<SymbolTable> = vec![SymbolTable::new(Some("global".to_string()))];

        Self {
            program: program.into_iter().peekable(),
            err_fmt: ErrorFormatter::new("semantic-analyzer", source, "main.bee"),
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

    pub fn analyze(&mut self) -> Result<(), String> {
        while let Some(stmt) = self.program.next() {
            self.analyze_stmt(&stmt)?;
        }

        Ok(())
    }

    fn analyze_stmt(&mut self, stmt: &Statement) -> Result<(), String> {
        match stmt {
            Statement::DeclareVariable {
                constant,
                identifier,
                type_descriptor,
                value,
            } => {
                let mut descriptor_typing: Option<Type> = None;
                if let Some(d) = type_descriptor {
                    let result = Type::from_descriptor(d.clone()).ok_or_else(|| {
                        self.err_fmt.format(
                            d.identifier.span,
                            format!("invalid type has provided: {}", d.identifier.lexeme),
                        )
                    })?;

                    descriptor_typing = Some(result);
                }

                let mut value_typing: Option<Type> = None;
                if let Some(e) = value {
                    let result = self.analyze_expr(e)?;
                    value_typing = Some(result);
                }

                if value_typing.is_none() && descriptor_typing.is_none() {
                    return Err(self.err_fmt.format(
                        identifier.span,
                        "unitialized variables must have type descriptors.".to_string(),
                    ));
                }

                if value_typing
                    .clone()
                    .is_some_and(|_| descriptor_typing.is_some())
                    && value_typing.as_ref().unwrap() != descriptor_typing.as_ref().unwrap()
                {
                    return Err(self.err_fmt.format(
                        type_descriptor.clone().unwrap().identifier.span,
                        format!(
                            "type descriptor is {}, but received an {} value.",
                            descriptor_typing.unwrap(),
                            value_typing.unwrap()
                        ),
                    ));
                }

                let scope = self.current_scope_mut();
                let symbol = Symbol {
                    constant: *constant,
                    identifier: identifier.clone(),
                    typing: descriptor_typing.or(value_typing).unwrap(),
                };

                scope
                    .insert(&identifier.lexeme, symbol)
                    .map_err(|e| self.err_fmt.format(identifier.span, e))?;

                Ok(())
            }
            Statement::Block { label, statements } => {
                let label: Option<String> = label.as_ref().map(|t| t.lexeme.clone());

                self.open_scope(label);
                for stmt in statements.iter() {
                    self.analyze_stmt(stmt)?;
                }
                self.close_scope();

                Ok(())
            }
            Statement::Expression { expr } => self.analyze_expr(expr).map(|_| ()),
        }
    }

    fn analyze_expr(&mut self, expr: &Expression) -> Result<Type, String> {
        match expr {
            Expression::VariableAssignment { left, equal, right } => {
                let symbol = self.lookup(&left.lexeme).ok_or_else(|| {
                    self.err_fmt.format(
                        left.span,
                        format!("cannot assign to undeclared variable: {}", left.lexeme),
                    )
                })?;

                if symbol.constant {
                    return Err(self
                        .err_fmt
                        .format(left.span, "cannot assign value to constant".to_string()));
                }

                let value_type = self.analyze_expr(right)?;
                if value_type != symbol.clone().typing {
                    return Err(self.err_fmt.format(
                        equal.span,
                        format!(
                            "expected an {} value, but received an {} value.",
                            symbol.typing, value_type
                        ),
                    ));
                }

                Ok(value_type)
            }
            Expression::Literal { value } => {
                let typing = match value.kind {
                    TokenKind::Integer => Type::Int64,
                    TokenKind::Float => Type::Float32,
                    TokenKind::String => Type::String,
                    _ => unreachable!(),
                };

                Ok(typing)
            }

            Expression::Identifier { value } => self
                .lookup(&value.lexeme)
                .ok_or_else(|| {
                    self.err_fmt
                        .format(value.span, format!("undeclared variable: {}", value.lexeme))
                })
                .map(|s| s.typing.clone()),
        }
    }
}
