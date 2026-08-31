#include "interpreter.h"
#include "ast.h"

#include "libs/array_list.h"
#include "libs/error.h"
#include "libs/string_view.h"
#include "libs/symbol_table.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Interpreter interpreter_create(Program *program, Source *source,
                               SymbolTable *symbols) {
  Interpreter interpreter = {0};
  interpreter.program = program;
  interpreter.source = source;
  interpreter.symbols = symbols;

  return interpreter;
}

void interpreter_eval(Interpreter *interpreter) {
  ArrayList *stmts = interpreter->program->stmts;
  for (u32 i = 0; i < array_list_length(stmts); i++) {
    Stmt *stmt = array_list_at(stmts, i);
    interpreter_eval_stmt(interpreter, stmt);
  }
}

Result interpreter_eval_expr_stmt(Interpreter *interpreter, ExprStmt *stmt) {
  interpreter_eval_expr(interpreter, &stmt->expr);
  return RESULT_NORMAL();
}

Result interpreter_eval_variable_decl_stmt(Interpreter *interpreter,
                                           VariableDeclStmt *stmt) {
  Value value = interpreter_eval_expr(interpreter, &stmt->value);
  SymbolVariable variable = {
      .value_expr = &stmt->value,
      .value = malloc(sizeof(Value)),
      .constant =
          string_view_is_equal(stmt->keyword_token.lexeme, SV_LIT("const")),
  };
  memcpy(variable.value, &value, sizeof(Value));

  Symbol symbol = {0};
  symbol.kind = SYMBOL_KIND_VARIABLE;
  symbol.identifier = stmt->identifier_token.lexeme;
  symbol.as.variable = variable;
  symtable_put(interpreter->symbols, symbol);

  return RESULT_NORMAL();
}

Result interpreter_eval_function_decl_stmt(Interpreter *interpreter,
                                           FunctionDeclStmt *stmt) {

  Function function = {0};
  function.identifier = stmt->identifier_token.lexeme;

  Value value = {0};
  value.kind = VALUE_KIND_FUNCTION;
  value.as.function = function;

  SymbolFunction sym_function = {0};
  sym_function.params_variables = NULL;
  sym_function.stmt = stmt;
  sym_function.value = malloc(sizeof(Value));
  memcpy(sym_function.value, &value, sizeof(Value));

  Symbol symbol = {0};
  symbol.kind = SYMBOL_KIND_FUNCTION;
  symbol.as.func = sym_function;
  symbol.identifier = stmt->identifier_token.lexeme;

  symtable_put(interpreter->symbols, symbol);

  return RESULT_NORMAL();
}

Result interpreter_eval_echo_stmt(Interpreter *interpreter, EchoStmt *stmt) {
  Value value = interpreter_eval_expr(interpreter, &stmt->message);
  switch (value.kind) {
  case VALUE_KIND_INTEGER:
    printf("%lld\n", value.as.integer);
    break;
  case VALUE_KIND_STRING:
    printf(SV_FMT "\n", SV_ARG(value.as.string));
    break;
  case VALUE_KIND_BOOLEAN:
    printf("%s\n", value.as.boolean ? "true" : "false");
    break;
  default:
    fprintf(stderr, "ERROR: unreachable (interpreter_eval_echo_stmt).\n");
    exit(1);
  }

  return RESULT_NORMAL();
}

Result interpreter_eval_block_stmt(Interpreter *interpreter, BlockStmt *stmt) {
  SymbolTable scope =
      symtable_new(interpreter->symbols, SYMBOL_TABLE_KIND_BLOCK);
  interpreter->symbols = &scope;

  for (u32 i = 0; i < array_list_length(stmt->stmts); i++) {
    Stmt *cur = array_list_at(stmt->stmts, i);
    Result result = interpreter_eval_stmt(interpreter, cur);
    if (result.kind == RESULT_KIND_RETURN) {
      interpreter->symbols = scope.parent;
      symtable_destroy(&scope);
      return result;
    }
  }

  interpreter->symbols = scope.parent;
  symtable_destroy(&scope);

  return RESULT_NORMAL();
}

Result interpreter_eval_if_stmt(Interpreter *interpreter, IfStmt *stmt) {
  Value condition = interpreter_eval_expr(interpreter, stmt->condition);
  Result result = RESULT_NORMAL();
  if (condition.as.boolean) {
    result = interpreter_eval_stmt(interpreter, stmt->consequent);
  } else if (stmt->alternate != NULL) {
    result = interpreter_eval_stmt(interpreter, stmt->alternate);
  }

  return result;
}

Result interpreter_eval_while_stmt(Interpreter *interpreter, WhileStmt *stmt) {
  SymbolTable scope =
      symtable_new(interpreter->symbols, SYMBOL_TABLE_KIND_BLOCK);
  interpreter->symbols = &scope;

  Value condition = interpreter_eval_expr(interpreter, stmt->condition);
  while (condition.as.boolean) {
    Result result = interpreter_eval_stmt(interpreter, stmt->body);
    if (result.kind == RESULT_KIND_RETURN) {
      interpreter->symbols = scope.parent;
      symtable_destroy(&scope);
      return result;
    }

    condition = interpreter_eval_expr(interpreter, stmt->condition);
  }

  interpreter->symbols = scope.parent;
  symtable_destroy(&scope);
  return RESULT_NORMAL();
}

Result interpreter_eval_for_stmt(Interpreter *interpreter, ForStmt *stmt) {
  SymbolTable scope =
      symtable_new(interpreter->symbols, SYMBOL_TABLE_KIND_BLOCK);
  interpreter->symbols = &scope;
  interpreter_eval_stmt(interpreter, stmt->init);

  Value test = interpreter_eval_expr(interpreter, stmt->test);
  while (test.as.boolean) {
    Result result = interpreter_eval_stmt(interpreter, stmt->body);
    if (result.kind == RESULT_KIND_RETURN) {
      interpreter->symbols = scope.parent;
      symtable_destroy(&scope);
      return result;
    }

    interpreter_eval_expr(interpreter, stmt->update);
    test = interpreter_eval_expr(interpreter, stmt->test);
  }

  interpreter->symbols = scope.parent;
  symtable_destroy(&scope);

  return RESULT_NORMAL();
}

Result interpreter_eval_return_stmt(Interpreter *interpreter,
                                    ReturnStmt *stmt) {
  Value value = interpreter_eval_expr(interpreter, &stmt->expr);
  return RESULT_RETURN(value);
}

Result interpreter_eval_stmt(Interpreter *interpreter, Stmt *stmt) {
  switch (stmt->kind) {
  case STMT_KIND_EXPR:
    return interpreter_eval_expr_stmt(interpreter, &stmt->as.expr);
  case STMT_KIND_VARIABLE_DECL:
    return interpreter_eval_variable_decl_stmt(interpreter,
                                               &stmt->as.variable_decl);
  case STMT_KIND_FUNCTION_DECL:
    return interpreter_eval_function_decl_stmt(interpreter,
                                               &stmt->as.function_decl);
  case STMT_KIND_ECHO:
    return interpreter_eval_echo_stmt(interpreter, &stmt->as.echo);
  case STMT_KIND_BLOCK:
    return interpreter_eval_block_stmt(interpreter, &stmt->as.block);
  case STMT_KIND_IF:
    return interpreter_eval_if_stmt(interpreter, &stmt->as.if_stmt);
  case STMT_KIND_WHILE:
    return interpreter_eval_while_stmt(interpreter, &stmt->as.while_stmt);
  case STMT_KIND_FOR:
    return interpreter_eval_for_stmt(interpreter, &stmt->as.for_stmt);
  case STMT_KIND_RETURN:
    return interpreter_eval_return_stmt(interpreter, &stmt->as.return_stmt);
  default:
    fprintf(stderr, "ERROR: unreachable (interpreter_eval_stmt).\n");
    exit(1);
  }
}

Value interpreter_eval_literal_expr(Interpreter *interpreter,
                                    LiteralExpr *expr) {
  Value value = {0};
  switch (expr->value_token.kind) {
  case TOKEN_KIND_NUMBER: {
    char *buffer = string_view_to_cstr(expr->value_token.lexeme);
    value.kind = VALUE_KIND_INTEGER;
    value.as.integer = atoi(buffer);
    free(buffer);
    break;
  }
  case TOKEN_KIND_STRING: {
    value.kind = VALUE_KIND_STRING;
    value.as.string = expr->value_token.lexeme;
    break;
  }
  case TOKEN_KIND_TRUE:
  case TOKEN_KIND_FALSE: {
    value.kind = VALUE_KIND_BOOLEAN;
    value.as.boolean =
        string_view_is_equal(expr->value_token.lexeme, SV_LIT("true"));
    break;
  }
  default: {
    fprintf(stderr, "ERROR: unreachable (interpreter_eval_literal_expr).\n");
    exit(1);
  }
  }

  return value;
}

Value interpreter_eval_assignment_expr(Interpreter *interpreter,
                                       AssignmentExpr *expr) {
  Value value = interpreter_eval_expr(interpreter, expr->value);
  Symbol *symbol =
      symtable_get(interpreter->symbols, expr->identifier_token.lexeme);
  SymbolVariable *variable = &symbol->as.variable;
  variable->value = malloc(sizeof(Symbol));
  memcpy(variable->value, &value, sizeof(Value));

  return value;
}

Value interpreter_eval_identifier_expr(Interpreter *interpreter,
                                       IdentifierExpr *expr) {
  Symbol *symbol =
      symtable_get(interpreter->symbols, expr->identifier_token.lexeme);
  return symbol->kind == SYMBOL_KIND_FUNCTION ? *symbol->as.func.value
                                              : *symbol->as.variable.value;
}

Value interpreter_eval_binary_expr(Interpreter *interpreter, BinaryExpr *expr) {
  Value left = interpreter_eval_expr(interpreter, expr->left);
  Value right = interpreter_eval_expr(interpreter, expr->right);
  Value value = {0};

  switch (expr->operator_token.kind) {
  case TOKEN_KIND_PLUS:
    value.kind = VALUE_KIND_INTEGER;
    value.as.integer = left.as.integer + right.as.integer;
    break;
  case TOKEN_KIND_MINUS:
    value.kind = VALUE_KIND_INTEGER;
    value.as.integer = left.as.integer - right.as.integer;
    break;
  case TOKEN_KIND_STAR:
    value.kind = VALUE_KIND_INTEGER;
    value.as.integer = left.as.integer * right.as.integer;
    break;
  case TOKEN_KIND_SLASH: {
    if (right.as.integer == 0) {
      ErrorContext ctx = {.source = interpreter->source,
                          .span = expr->right->span};
      error_throw(&ctx, "attempt to divide by zero.");
    }

    value.kind = VALUE_KIND_INTEGER;
    value.as.integer = left.as.integer / right.as.integer;
    break;
  }
  case TOKEN_KIND_PERCENTAGE:
    value.kind = VALUE_KIND_INTEGER;
    value.as.integer = left.as.integer % right.as.integer;
    break;
  case TOKEN_KIND_GT:
    value.kind = VALUE_KIND_BOOLEAN;
    value.as.boolean = left.as.integer > right.as.integer;
    break;
  case TOKEN_KIND_GTE:
    value.kind = VALUE_KIND_BOOLEAN;
    value.as.boolean = left.as.integer >= right.as.integer;
    break;
  case TOKEN_KIND_LT:
    value.kind = VALUE_KIND_BOOLEAN;
    value.as.boolean = left.as.integer < right.as.integer;
    break;
  case TOKEN_KIND_LTE:
    value.kind = VALUE_KIND_BOOLEAN;
    value.as.boolean = left.as.integer <= right.as.integer;
    break;

  case TOKEN_KIND_EQEQ:
    value.kind = VALUE_KIND_BOOLEAN;
    if (left.kind == VALUE_KIND_INTEGER && right.kind == VALUE_KIND_INTEGER) {
      value.as.boolean = left.as.integer == right.as.integer;
    } else if (left.kind == VALUE_KIND_BOOLEAN &&
               right.kind == VALUE_KIND_BOOLEAN) {
      value.as.boolean = left.as.boolean == right.as.boolean;
    } else if (left.kind == VALUE_KIND_STRING &&
               right.kind == VALUE_KIND_STRING) {
      value.as.boolean = string_view_is_equal(left.as.string, right.as.string);
    } else {
      value.as.boolean = false;
    }

    break;
  case TOKEN_KIND_NEQ:
    value.kind = VALUE_KIND_BOOLEAN;
    if (left.kind == VALUE_KIND_INTEGER && right.kind == VALUE_KIND_INTEGER) {
      value.as.boolean = left.as.integer != right.as.integer;
    } else if (left.kind == VALUE_KIND_BOOLEAN &&
               right.kind == VALUE_KIND_BOOLEAN) {
      value.as.boolean = left.as.boolean != right.as.boolean;
    } else if (left.kind == VALUE_KIND_STRING &&
               right.kind == VALUE_KIND_STRING) {
      value.as.boolean = !string_view_is_equal(left.as.string, right.as.string);
    } else {
      value.as.boolean = true;
    }

    break;
  default:
    fprintf(stderr, "ERROR: unreachable (interpreter_eval_binary_expr).\n");
    exit(1);
  }

  return value;
}

Value interpreter_eval_logical_expr(Interpreter *interpreter,
                                    LogicalExpr *expr) {
  Value value = {0};
  value.kind = VALUE_KIND_BOOLEAN;

  switch (expr->operator_token.kind) {
  case TOKEN_KIND_AND: {
    Value left = interpreter_eval_expr(interpreter, expr->left);
    if (!left.as.boolean) {
      value.as.boolean = false;
      return value;
    }

    Value right = interpreter_eval_expr(interpreter, expr->right);
    value.as.boolean = left.as.boolean && right.as.boolean;

    return value;
  }
  case TOKEN_KIND_OR: {
    Value left = interpreter_eval_expr(interpreter, expr->left);
    if (left.as.boolean) {
      value.as.boolean = true;
      return value;
    }

    Value right = interpreter_eval_expr(interpreter, expr->right);
    value.as.boolean = left.as.boolean || right.as.boolean;
    return value;
  }
  default:
    fprintf(stderr, "ERROR: unreachable (interpreter_eval_logical_expr).\n");
    exit(1);
  }
}

Value interpreter_eval_unary_expr(Interpreter *interpreter, UnaryExpr *unary) {
  Value value = interpreter_eval_expr(interpreter, unary->operand);
  switch (unary->operator_token.kind) {
  case TOKEN_KIND_MINUS:
    value.as.integer *= -1;
    break;
  case TOKEN_KIND_PLUS:
    break;
  case TOKEN_KIND_NOT:
    value.as.boolean = !value.as.boolean;
    break;
  default: {
    fprintf(stderr, "ERROR: unreachable (interpreter_eval_unary_expr).\n");
    exit(1);
  }
  }

  return value;
}

Value interpreter_eval_when_expr(Interpreter *interpreter, WhenExpr *when) {
  Value condition = interpreter_eval_expr(interpreter, when->condition);
  if (condition.as.boolean) {
    return interpreter_eval_expr(interpreter, when->consequent);
  } else {
    return interpreter_eval_expr(interpreter, when->alternate);
  }
}

Value interpreter_eval_call_expr(Interpreter *interpreter, CallExpr *expr) {
  Symbol *symbol =
      symtable_get(interpreter->symbols, expr->identifier_token.lexeme);
  SymbolFunction *function = &symbol->as.func;
  FunctionDeclStmt *function_decl = function->stmt;

  SymbolTable scope =
      symtable_new(interpreter->symbols, SYMBOL_TABLE_KIND_FUNCTION);
  for (u32 i = 0; i < array_list_length(function_decl->params); i++) {
    FunctionDeclParam *param = array_list_at(function_decl->params, i);
    Expr *argument_expr = array_list_at(expr->arguments, i);
    Value argument_value = interpreter_eval_expr(interpreter, argument_expr);

    Symbol symbol = {0};
    if (argument_value.kind != VALUE_KIND_FUNCTION) {
      SymbolVariable variable = {0};
      variable.value = malloc(sizeof(Value));
      memcpy(variable.value, &argument_value, sizeof(Value));      
      
      symbol.kind = SYMBOL_KIND_VARIABLE;
      symbol.identifier = param->identifier_token.lexeme;
      symbol.as.variable = variable;
    } else {
      SymbolFunction function = {0};
      function.value = malloc(sizeof(Value));
      memcpy(function.value, &argument_value, sizeof(Value));

      symbol.kind = SYMBOL_KIND_FUNCTION;
      symbol.identifier = param->identifier_token.lexeme;
      symbol.as.func = function;
    }    
    
    symtable_put(&scope, symbol);
  }

  interpreter->symbols = &scope;  
  Result result = interpreter_eval_stmt(interpreter, function_decl->body);
  interpreter->symbols = scope.parent;
  symtable_destroy(&scope);

  return result.value;
}

Value interpreter_eval_expr(Interpreter *interpreter, Expr *expr) {
  switch (expr->kind) {
  case EXPR_KIND_LITERAL:
    return interpreter_eval_literal_expr(interpreter, &expr->as.literal);
  case EXPR_KIND_ASSIGNMENT:
    return interpreter_eval_assignment_expr(interpreter, &expr->as.assignment);
  case EXPR_KIND_IDENTIFIER:
    return interpreter_eval_identifier_expr(interpreter, &expr->as.identifier);
  case EXPR_KIND_BINARY:
    return interpreter_eval_binary_expr(interpreter, &expr->as.binary);
  case EXPR_KIND_LOGICAL:
    return interpreter_eval_logical_expr(interpreter, &expr->as.logical);
  case EXPR_KIND_PARENTHESIZED:
    return interpreter_eval_expr(interpreter, expr->as.parenthesized.expr);
  case EXPR_KIND_UNARY:
    return interpreter_eval_unary_expr(interpreter, &expr->as.unary);
  case EXPR_KIND_WHEN:
    return interpreter_eval_when_expr(interpreter, &expr->as.when);
  case EXPR_KIND_CALL:
    return interpreter_eval_call_expr(interpreter, &expr->as.call);
  default:
    fprintf(stderr, "ERROR: unreachable (interpreter_eval_expr).\n");
    exit(1);
  }
}
