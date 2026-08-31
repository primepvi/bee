#include "typechecker.h"
#include "ast.h"
#include "libs/array_list.h"
#include "libs/error.h"
#include "libs/string_builder.h"
#include "libs/string_view.h"
#include "libs/symbol_table.h"
#include <stdio.h>
#include <stdlib.h>

TypeChecker tc_create(Program *program, Source *source, SymbolTable *symbols) {
  TypeChecker tc = {0};
  tc.type_env = hashmap_new(32);
  tc.program = program;
  tc.source = source;
  tc.symbols = symbols;
  tc.expected_return_type = (Type){.identifier = SV_LIT("void")};

  // builtin types
  tc_define_type(&tc, (Type){.identifier = SV_LIT("int"), .nullable = false});
  tc_define_type(&tc, (Type){.identifier = SV_LIT("bool"), .nullable = false});
  tc_define_type(&tc,
                 (Type){.identifier = SV_LIT("string"), .nullable = false});
  tc_define_type(&tc, (Type){.identifier = SV_LIT("void"), .nullable = false});
  tc_define_type(&tc, (Type){.identifier = SV_LIT("null"), .nullable = false});
  tc_define_type(&tc,
                 (Type){.identifier = SV_LIT("function"), .nullable = false});

  return tc;
}

void tc_define_type(TypeChecker *tc, Type type) {
  hashmap_put(tc->type_env, type.identifier, sizeof(Type), &type);
}

Type tc_get_type(TypeChecker *tc, TypeAnnotation annotation) {
  Type *type = hashmap_get(tc->type_env, annotation.identifier_token.lexeme);
  if (type == NULL) {
    type = hashmap_get(tc->type_env, SV_LIT("void"));
  } else {
    type->nullable = annotation.nullable;
  }

  return *type;
}

Type tc_get_raw_type(TypeChecker *tc, StringView identifier) {
  Type *type = hashmap_get(tc->type_env, identifier);
  Type result =
      type == NULL ? *(Type *)hashmap_get(tc->type_env, SV_LIT("void")) : *type;
  result.nullable = false;

  return result;
}

void tc_check(TypeChecker *tc) {
  for (u32 i = 0; i < array_list_length(tc->program->stmts); i++) {
    Stmt *stmt = array_list_at(tc->program->stmts, i);
    tc_check_stmt(tc, stmt);
  }
}

Flow tc_check_variable_decl_stmt(TypeChecker *tc, Stmt *stmt) {
  VariableDeclStmt *decl = &stmt->as.variable_decl;
  if (symtable_scope_has(tc->symbols, decl->identifier_token.lexeme)) {
    ErrorContext ctx = {.source = tc->source,
                        .span = decl->identifier_token.span};
    error_throw_fmt(&ctx, "variable " SV_FMT " is already declared.",
                    SV_ARG(decl->identifier_token.lexeme));
  }

  Type value_type = tc_check_expr(tc, &decl->value);
  Type type = value_type;
  if (decl->type_annotation != NULL) {
    Type annotation_type = tc_get_type(tc, *decl->type_annotation);
    if (type_is_empty(annotation_type)) {
      ErrorContext ctx = {.source = tc->source,
                          .span = decl->type_annotation->span};
      error_throw(&ctx, "attempt to define a variable with invalid type.");
    }

    if (!type_is_assignable(annotation_type, value_type)) {
      ErrorContext ctx = {.source = tc->source, decl->value.span};
      error_throw_fmt(&ctx,
                      "variable expects value of type '%s', but received value "
                      "of type '%s'.",
                      type_to_string(annotation_type).buffer,
                      type_to_string(value_type).buffer);
    }

    type = annotation_type;
  } else if (type_is_empty(value_type)) {
    ErrorContext ctx = {.source = tc->source, .span = decl->value.span};
    error_throw(&ctx, "attempt to define a variable with 'void' value.");
  }

  SymbolVariable variable = {0};
  variable.type = type;
  variable.constant =
      string_view_is_equal(decl->keyword_token.lexeme, SV_LIT("const"));
  variable.value_expr = &decl->value;
  variable.value = NULL;

  Symbol symbol = {0};
  symbol.kind = SYMBOL_KIND_VARIABLE;
  symbol.identifier = decl->identifier_token.lexeme;
  symbol.as.variable = variable;

  symtable_put(tc->symbols, symbol);
  return FLOW_CONTINUE;
}

Flow tc_check_function_decl_stmt(TypeChecker *tc, Stmt *stmt) {
  FunctionDeclStmt *decl = &stmt->as.function_decl;
  if (symtable_has(tc->symbols, decl->identifier_token.lexeme)) {
    ErrorContext ctx = {.source = tc->source,
                        .span = decl->identifier_token.span};
    error_throw_fmt(
        &ctx,
        "already has a declared variable or function with name '" SV_FMT "'.",
        SV_ARG(decl->identifier_token.lexeme));
  }

  ArrayList *param_variables =
      array_list_new(array_list_capacity(decl->params), sizeof(SymbolVariable));
  SymbolTable scope = symtable_new(tc->symbols, SYMBOL_TABLE_KIND_FUNCTION);

  for (u32 i = 0; i < array_list_length(decl->params); i++) {
    FunctionDeclParam *param = array_list_at(decl->params, i);

    Type param_type = tc_get_type(tc, param->type_annotation);
    if (type_is_empty(param_type)) {
      ErrorContext ctx = {.source = tc->source,
                          .span = param->type_annotation.span};
      error_throw(&ctx,
                  "attempt to anotate a function param with invalid type.");
    }

    SymbolVariable param_variable = {0};
    param_variable.constant = true;
    param_variable.type = param_type;
    param_variable.value = NULL;
    param_variable.value_expr = NULL;

    Symbol param_symbol = {0};
    param_symbol.kind = SYMBOL_KIND_VARIABLE;
    param_symbol.as.variable = param_variable;
    param_symbol.identifier = param->identifier_token.lexeme;

    symtable_put(&scope, param_symbol);
    array_list_push(param_variables, &param_variable);
  }

  b8 return_void = string_view_is_equal(
      decl->type_annotation.identifier_token.lexeme, SV_LIT("void"));

  Type return_type = tc_get_type(tc, decl->type_annotation);
  if (!return_void && type_is_empty(return_type)) {
    ErrorContext ctx = {.source = tc->source,
                        .span = decl->type_annotation.span};
    error_throw(&ctx,
                "attempt to anotate a function with invalid return type.");
  }

  SymbolFunction function = {0};
  function.stmt = &stmt->as.function_decl;
  function.params_variables = param_variables;
  function.return_type = return_type;

  Symbol symbol = {0};
  symbol.kind = SYMBOL_KIND_FUNCTION;
  symbol.identifier = decl->identifier_token.lexeme;
  symbol.as.func = function;

  if (tc->symbols->kind != SYMBOL_TABLE_KIND_GLOBAL) {
    ErrorContext ctx = {.source = tc->source,
                        .span = decl->identifier_token.span};
    error_throw(&ctx, "attempt to define a function with non-global scope.");
  }

  symtable_put(tc->symbols, symbol);
  symtable_put(&scope, symbol);

  Type prev_return_type = tc->expected_return_type;
  tc->expected_return_type = return_type;
  tc->symbols = &scope;

  Flow flow = tc_check_stmt(tc, decl->body);

  tc->symbols = scope.parent;
  tc->expected_return_type = prev_return_type;

  symtable_destroy(&scope);

  if (!type_is_empty(return_type) && flow.can_continue) {
    ErrorContext ctx = {.source = tc->source, .span = stmt->span};
    error_throw(&ctx, "not all function control paths return a value.");
  }

  return FLOW_CONTINUE;
}

Flow tc_check_block_stmt(TypeChecker *tc, Stmt *stmt) {
  BlockStmt *block = &stmt->as.block;
  SymbolTable scope = symtable_new(tc->symbols, SYMBOL_TABLE_KIND_BLOCK);
  tc->symbols = &scope;

  Flow flow = FLOW_CONTINUE;
  for (u32 i = 0; i < array_list_length(block->stmts); i++) {
    if (!flow.can_continue) {
      break;
    }

    Stmt *stmt = array_list_at(block->stmts, i);
    flow = tc_check_stmt(tc, stmt);
  }

  tc->symbols = scope.parent;
  symtable_destroy(&scope);
  return flow;
}

Flow tc_check_if_stmt(TypeChecker *tc, Stmt *stmt) {
  IfStmt *if_stmt = &stmt->as.if_stmt;
  Type condition_type = tc_check_expr(tc, if_stmt->condition);
  Flow consequent_flow = FLOW_CONTINUE;
  Flow alternate_flow = FLOW_CONTINUE;

  if (!type_is_assignable(tc_get_raw_type(tc, SV_LIT("bool")),
                          condition_type)) {
    ErrorContext ctx = {.source = tc->source, .span = if_stmt->condition->span};
    error_throw_fmt(&ctx,
                    "The 'if' statement 'condition' must be of type 'bool', "
                    "but received an "
                    "condition of type '%s'.",
                    type_to_string(condition_type));
  }

  consequent_flow = tc_check_stmt(tc, if_stmt->consequent);
  if (if_stmt->alternate != NULL) {
    alternate_flow = tc_check_stmt(tc, if_stmt->alternate);
  }

  return FLOW(consequent_flow.can_continue || alternate_flow.can_continue);
}

Flow tc_check_while_stmt(TypeChecker *tc, Stmt *stmt) {
  WhileStmt *while_stmt = &stmt->as.while_stmt;
  Type condition_type = tc_check_expr(tc, while_stmt->condition);

  SymbolTable scope = symtable_new(tc->symbols, SYMBOL_TABLE_KIND_BLOCK);
  tc->symbols = &scope;

  if (!type_is_assignable(tc_get_raw_type(tc, SV_LIT("bool")),
                          condition_type)) {
    ErrorContext ctx = {.source = tc->source,
                        .span = while_stmt->condition->span};
    error_throw_fmt(&ctx,
                    "The 'while' statement 'condition' must be of type 'bool', "
                    "but received an "
                    "condition of type '%s'.",
                    type_to_string(condition_type).buffer);
  }

  Flow flow = tc_check_stmt(tc, while_stmt->body);
  tc->symbols = scope.parent;
  symtable_destroy(&scope);

  return flow;
}

Flow tc_check_for_stmt(TypeChecker *tc, Stmt *stmt) {
  ForStmt *for_stmt = &stmt->as.for_stmt;
  SymbolTable scope = symtable_new(tc->symbols, SYMBOL_TABLE_KIND_BLOCK);
  tc->symbols = &scope;
  tc_check_stmt(tc, for_stmt->init);

  Type test_type = tc_check_expr(tc, for_stmt->test);
  if (!type_is_assignable(tc_get_raw_type(tc, SV_LIT("bool")), test_type)) {
    ErrorContext ctx = {.source = tc->source, .span = for_stmt->test->span};
    error_throw_fmt(&ctx,
                    "The 'for' statement 'test expression' must be of type "
                    "'bool', but received an "
                    "test expression of type '%s'.",
                    type_to_string(test_type).buffer);
  }

  tc_check_expr(tc, for_stmt->update);
  Flow flow = tc_check_stmt(tc, for_stmt->body);
  tc->symbols = scope.parent;
  symtable_destroy(&scope);

  return flow;
}

Flow tc_check_return_stmt(TypeChecker *tc, Stmt *stmt) {
  ReturnStmt *ret = &stmt->as.return_stmt;
  SymbolTable *function_scope = tc->symbols;
  while (function_scope->parent != NULL &&
         function_scope->kind != SYMBOL_TABLE_KIND_FUNCTION) {
    function_scope = function_scope->parent;
  }

  if (function_scope->kind != SYMBOL_TABLE_KIND_FUNCTION) {
    ErrorContext ctx = {.source = tc->source, .span = stmt->span};
    error_throw(&ctx, "attempt to return outside a function block.");
  }

  Type ret_type = ret->expr == NULL ? tc_get_raw_type(tc, SV_LIT("void"))
                                    : tc_check_expr(tc, ret->expr);
  if (!type_is_assignable(tc->expected_return_type, ret_type)) {
    ErrorContext ctx = {.source = tc->source, .span = stmt->span};
    error_throw_fmt(&ctx,
                    "function expects a return value of type '%s', but "
                    "received a value of type '%s'.",
                    type_to_string(tc->expected_return_type).buffer,
                    type_to_string(ret_type).buffer);
  }

  return FLOW_STOP;
}

Flow tc_check_stmt(TypeChecker *tc, Stmt *stmt) {
  switch (stmt->kind) {
  case STMT_KIND_ECHO: {
    EchoStmt *echo = &stmt->as.echo;
    Type type = tc_check_expr(tc, &echo->message);
    if (type_is_empty(type)) {
      ErrorContext ctx = {.source = tc->source, .span = echo->message.span};
      error_throw(&ctx, "attempt to 'echo' a 'void' value.");
    }

    return FLOW_CONTINUE;
  }
  case STMT_KIND_EXPR: {
    ExprStmt *expr = &stmt->as.expr;
    tc_check_expr(tc, &expr->expr);
    return FLOW_CONTINUE;
  }
  case STMT_KIND_VARIABLE_DECL:
    return tc_check_variable_decl_stmt(tc, stmt);
  case STMT_KIND_FUNCTION_DECL:
    return tc_check_function_decl_stmt(tc, stmt);
  case STMT_KIND_BLOCK:
    return tc_check_block_stmt(tc, stmt);
  case STMT_KIND_IF:
    return tc_check_if_stmt(tc, stmt);
  case STMT_KIND_WHILE:
    return tc_check_while_stmt(tc, stmt);
  case STMT_KIND_FOR:
    return tc_check_for_stmt(tc, stmt);
  case STMT_KIND_RETURN:
    return tc_check_return_stmt(tc, stmt);
  default: {
    fprintf(stderr, "ERROR: unreachable (tc_check_stmt).\n");
    exit(1);
  }
  }
}

Type tc_check_literal_expr(TypeChecker *tc, Expr *expr) {
  LiteralExpr *literal = &expr->as.literal;
  switch (literal->value_token.kind) {
  case TOKEN_KIND_NUMBER:
    return tc_get_raw_type(tc, SV_LIT("int"));
  case TOKEN_KIND_TRUE:
  case TOKEN_KIND_FALSE:
    return tc_get_raw_type(tc, SV_LIT("bool"));
  case TOKEN_KIND_STRING:
    return tc_get_raw_type(tc, SV_LIT("string"));
  case TOKEN_KIND_NULL:
    return tc_get_raw_type(tc, SV_LIT("null"));
  default: {
    fprintf(stderr, "ERROR: unreachable (tc_check_literal_expr).\n");
    exit(1);
  }
  }
}

Type tc_check_identifier_expr(TypeChecker *tc, Expr *expr) {
  IdentifierExpr *ident = &expr->as.identifier;
  Symbol *symbol = symtable_get(tc->symbols, ident->identifier_token.lexeme);
  if (symbol == NULL) {
    ErrorContext ctx = {.source = tc->source,
                        .span = ident->identifier_token.span};
    error_throw_fmt(&ctx,
                    "attempt to access a undefined identifier: " SV_FMT ".",
                    SV_ARG(ident->identifier_token.lexeme));
  }

  return symbol->kind == SYMBOL_KIND_FUNCTION
             ? tc_get_raw_type(tc, SV_LIT("function"))
             : symbol->as.variable.type;
}

Type tc_check_unary_expr(TypeChecker *tc, Expr *expr) {
  UnaryExpr *unary = &expr->as.unary;
  Type type = tc_check_expr(tc, unary->operand);
  if (!type_supports_unary_op(type, unary->operator_token.kind)) {
    ErrorContext ctx = {.source = tc->source, .span = expr->span};
    error_throw_fmt(
        &ctx,
        "unary operator '" SV_FMT "' doesn't support '" SV_FMT "' operand.",
        SV_ARG(unary->operator_token.lexeme), SV_ARG(type.identifier));
  }

  return type;
}

Type tc_check_binary_expr(TypeChecker *tc, Expr *expr) {
  BinaryExpr *binary = &expr->as.binary;
  Type left = tc_check_expr(tc, binary->left);
  Type right = tc_check_expr(tc, binary->right);
  if (!type_supports_binary_op(left, right, binary->operator_token.kind)) {
    ErrorContext ctx = {.source = tc->source, .span = expr->span};
    error_throw_fmt(&ctx,
                    "binary operator '" SV_FMT
                    "' doesn't support a left '" SV_FMT
                    "' operand and a right '" SV_FMT "' operand.",
                    SV_ARG(binary->operator_token.lexeme),
                    SV_ARG(left.identifier), SV_ARG(right.identifier));
  }

  switch (binary->operator_token.kind) {
  case TOKEN_KIND_MINUS:
  case TOKEN_KIND_PLUS:
  case TOKEN_KIND_STAR:
  case TOKEN_KIND_SLASH:
  case TOKEN_KIND_PERCENTAGE:
    return tc_get_raw_type(tc, SV_LIT("int"));
  case TOKEN_KIND_LT:
  case TOKEN_KIND_LTE:
  case TOKEN_KIND_GT:
  case TOKEN_KIND_GTE:
  case TOKEN_KIND_EQEQ:
  case TOKEN_KIND_NEQ:
    return tc_get_raw_type(tc, SV_LIT("bool"));
  default: {
    fprintf(stderr, "ERROR: unreachable (tc_check_binary_expr).\n");
    exit(1);
  }
  }
}

Type tc_check_logical_expr(TypeChecker *tc, Expr *expr) {
  LogicalExpr *logical = &expr->as.logical;
  Type left = tc_check_expr(tc, logical->left);
  Type right = tc_check_expr(tc, logical->right);
  if (!type_supports_logical_op(left, right, logical->operator_token.kind)) {
    ErrorContext ctx = {.source = tc->source, .span = expr->span};
    error_throw_fmt(&ctx,
                    "logical operator '" SV_FMT
                    "' doesn't support a left '" SV_FMT
                    "' operand and a right '" SV_FMT "' operand.",
                    SV_ARG(logical->operator_token.lexeme),
                    SV_ARG(left.identifier), SV_ARG(right.identifier));
  }

  switch (logical->operator_token.kind) {
  case TOKEN_KIND_AND:
  case TOKEN_KIND_OR:
    return tc_get_raw_type(tc, SV_LIT("bool"));
  default: {
    fprintf(stderr, "ERROR: unreachable (tc_check_logical_expr).\n");
    exit(1);
  }
  }
}

Type tc_check_assignment_expr(TypeChecker *tc, Expr *expr) {
  AssignmentExpr *assignment = &expr->as.assignment;
  Symbol *symbol =
      symtable_get(tc->symbols, assignment->identifier_token.lexeme);
  if (symbol == NULL) {
    ErrorContext ctx = {.source = tc->source, .span = expr->span};
    error_throw_fmt(&ctx, "attempt to assign a undefined variable: " SV_FMT ".",
                    SV_ARG(assignment->identifier_token.lexeme));
  }

  if (symbol->kind != SYMBOL_KIND_VARIABLE) {
    ErrorContext ctx = {.source = tc->source, .span = expr->span};
    error_throw_fmt(&ctx, "attempt to assign a non-variable: " SV_FMT ".",
                    SV_ARG(assignment->identifier_token.lexeme));
  }

  SymbolVariable *variable = &symbol->as.variable;

  if (variable->constant) {
    ErrorContext ctx = {
        .source = tc->source,
        .span = expr->span,
    };
    error_throw_fmt(&ctx, "attempt to assign a constant variable: " SV_FMT ".",
                    SV_ARG(assignment->identifier_token.lexeme));
  }

  Type type = tc_check_expr(tc, assignment->value);
  if (!type_is_assignable(variable->type, type)) {
    ErrorContext ctx = {
        .source = tc->source,
        .span = expr->span,
    };
    error_throw_fmt(&ctx, "attempt to assign a '%s' value to a '%s' variable.",
                    type_to_string(type).buffer,
                    type_to_string(variable->type).buffer);
  }

  variable->value_expr = assignment->value;
  return type;
}

Type tc_check_when_expr(TypeChecker *tc, Expr *expr) {
  WhenExpr *when = &expr->as.when;
  Type condition_type = tc_check_expr(tc, when->condition);
  if (!type_is(condition_type, SV_LIT("bool"))) {
    ErrorContext ctx = {.source = tc->source, .span = when->condition->span};
    error_throw_fmt(&ctx,
                    "when condition must be of type 'bool', but received an "
                    "condition of type '%s'.",
                    type_to_string(condition_type).buffer);
  }

  Type consequent_type = tc_check_expr(tc, when->consequent);
  Type alternate_type = tc_check_expr(tc, when->alternate);

  Type expected, received;
  if (type_is(consequent_type, SV_LIT("null"))) {
    received = consequent_type;
    expected = alternate_type;
    expected.nullable = true;
  } else if (type_is(alternate_type, SV_LIT("null"))) {
    received = alternate_type;
    expected = consequent_type;
    expected.nullable = true;
  } else {
    expected = consequent_type;
    received = alternate_type;
  }

  if (type_is_equal(expected, received) && alternate_type.nullable) {
    expected.nullable = true;
  }    

  if (!type_is_assignable(expected, received)) {
    ErrorContext ctx = {.source = tc->source, .span = expr->span};
    error_throw_fmt(&ctx,
                    "The 'consequent' and 'alternate' values of a 'when "
                    "expression' must be of same "
                    "type, but received a 'consequent' of type '%s' and an 'alternate' of type '%s'.",
                    type_to_string(consequent_type).buffer,
                    type_to_string(alternate_type).buffer);
  }

  return expected;
}

Type tc_check_call_expr(TypeChecker *tc, Expr *expr) {
  CallExpr *call = &expr->as.call;
  Symbol *symbol = symtable_get(tc->symbols, call->identifier_token.lexeme);
  if (symbol == NULL) {
    ErrorContext ctx = {.source = tc->source, .span = expr->span};
    error_throw(&ctx, "attempt to call a undefined function.");
  }

  if (symbol->kind != SYMBOL_KIND_FUNCTION) {
    ErrorContext ctx = {.source = tc->source, .span = expr->span};
    error_throw(&ctx, "attempt to call a non-function symbol.");
  }

  SymbolFunction *function = &symbol->as.func;
  u32 function_arity = array_list_length(function->params_variables);
  u32 call_arity = array_list_length(call->arguments);
  if (call_arity != function_arity) {
    ErrorContext ctx = {.source = tc->source, .span = expr->span};
    error_throw_fmt(&ctx,
                    "function '" SV_FMT
                    "' expects %u arguments, but has called with %u arguments.",
                    SV_ARG(call->identifier_token.lexeme), function_arity,
                    call_arity);
  }

  for (u32 i = 0; i < function_arity; i++) {
    FunctionDeclParam *param_decl = array_list_at(function->stmt->params, i);
    SymbolVariable *param_symb = array_list_at(function->params_variables, i);
    Expr *argument_expr = array_list_at(call->arguments, i);
    Type argument_type = tc_check_expr(tc, argument_expr);

    if (!type_is_assignable(param_symb->type, argument_type)) {
      ErrorContext ctx = {
          .source = tc->source,
          .span = argument_expr->span,
      };
      error_throw_fmt(&ctx,
                      "function '" SV_FMT "' param '" SV_FMT
                      "' expects argument of type '%s', but has called with "
                      "argument of type '%s'.",
                      SV_ARG(call->identifier_token.lexeme),
                      SV_ARG(param_decl->identifier_token.lexeme),
                      type_to_string(param_symb->type).buffer,
                      type_to_string(argument_type).buffer);
    }
  }

  return function->return_type;
}

Type tc_check_expr(TypeChecker *tc, Expr *expr) {
  switch (expr->kind) {
  case EXPR_KIND_LITERAL:
    return tc_check_literal_expr(tc, expr);
  case EXPR_KIND_IDENTIFIER:
    return tc_check_identifier_expr(tc, expr);
  case EXPR_KIND_UNARY:
    return tc_check_unary_expr(tc, expr);
  case EXPR_KIND_BINARY:
    return tc_check_binary_expr(tc, expr);
  case EXPR_KIND_PARENTHESIZED:
    return tc_check_expr(tc, expr->as.parenthesized.expr);
  case EXPR_KIND_LOGICAL:
    return tc_check_logical_expr(tc, expr);
  case EXPR_KIND_ASSIGNMENT:
    return tc_check_assignment_expr(tc, expr);
  case EXPR_KIND_WHEN:
    return tc_check_when_expr(tc, expr);
  case EXPR_KIND_CALL:
    return tc_check_call_expr(tc, expr);
  default: {
    fprintf(stderr, "ERROR: unreachable (tc_check_expr).\n");
    exit(1);
  }
  }
}

b8 type_supports_unary_op(Type type, TokenKind operator_kind) {
  switch (operator_kind) {
  case TOKEN_KIND_NOT:
    return type_is(type, SV_LIT("bool"));
  case TOKEN_KIND_MINUS:
  case TOKEN_KIND_PLUS:
    return type_is(type, SV_LIT("int"));
  default:
    return false;
  }
}

b8 type_supports_binary_op(Type left, Type right, TokenKind operator_kind) {
  switch (operator_kind) {
  case TOKEN_KIND_MINUS:
  case TOKEN_KIND_PLUS:
  case TOKEN_KIND_STAR:
  case TOKEN_KIND_SLASH:
  case TOKEN_KIND_PERCENTAGE:
  case TOKEN_KIND_LT:
  case TOKEN_KIND_LTE:
  case TOKEN_KIND_GT:
  case TOKEN_KIND_GTE:
    return type_is_numeric(left) && type_is_numeric(right);

  case TOKEN_KIND_EQEQ:
  case TOKEN_KIND_NEQ:
    return !type_is_empty(left) && !type_is_empty(right);

  default:
    return false;
  }
}

b8 type_supports_logical_op(Type left, Type right, TokenKind operator_kind) {
  switch (operator_kind) {
  case TOKEN_KIND_AND:
  case TOKEN_KIND_OR:
    return type_is(left, SV_LIT("bool")) && type_is(right, SV_LIT("bool"));
  default:
    return false;
  }
}

b8 type_is(Type type, StringView identifier) {
  return string_view_is_equal(type.identifier, identifier);
}

b8 type_is_assignable(Type expected, Type received) {
  b8 is_equal = type_is_equal(expected, received);
  b8 received_null = string_view_is_equal(received.identifier, SV_LIT("null"));

  if (expected.nullable && (is_equal || received_null))
    return true;
  else
    return is_equal && !received.nullable;
}

b8 type_is_equal(Type a, Type b) {
  return string_view_is_equal(a.identifier, b.identifier);
}

b8 type_is_numeric(Type type) { return type_is(type, SV_LIT("int")); }
b8 type_is_empty(Type type) { return type_is(type, SV_LIT("void")); }

StringBuilder type_to_string(Type type) {
  StringBuilder sb = string_builder_new(type.identifier.length + 1);
  string_builder_append(&sb, type.identifier);
  if (type.nullable) {
    string_builder_append_char(&sb, '?');
  }

  return sb;
}
