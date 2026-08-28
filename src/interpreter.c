#include "interpreter.h"
#include "ast.h"
#include "libs/array_list.h"
#include "libs/env.h"
#include "libs/error.h"
#include "libs/string_view.h"
#include <stdio.h>
#include <stdlib.h>

Interpreter interpreter_create(Source *source, Program *program) {
  Interpreter interpreter = {0};
  interpreter.source = source;
  interpreter.program = program;
  interpreter.global_env = env_create(NULL);
  return interpreter;
}

void interpreter_eval(Interpreter *interpreter) {
  ArrayList *stmts = interpreter->program->stmts;
  for (u32 i = 0; i < array_list_length(stmts); i++) {
    Stmt *stmt = array_list_at(stmts, i);
    interpreter_eval_stmt(interpreter, stmt);
  }
}

void interpreter_eval_expr_stmt(Interpreter *interpreter, ExprStmt *stmt) {
  interpreter_eval_expr(interpreter, &stmt->expr);
}

void interpreter_eval_variable_decl_stmt(Interpreter *interpreter,
                                         VariableDeclStmt *stmt) {
  if (env_has(&interpreter->global_env, stmt->identifier_token.lexeme)) {
    ErrorContext ctx = {.source = interpreter->source,
                        .span = stmt->identifier_token.span};
    error_throw_fmt(&ctx, "variable " SV_FMT " is already declared.",
                    SV_ARG(stmt->identifier_token.lexeme));
  }

  Value value = interpreter_eval_expr(interpreter, &stmt->value);
  EnvEntry entry = {0};
  entry.kind = ENV_ENTRY_KIND_VARIABLE;
  entry.as.variable =
      (EnvVariable){.constant = string_view_is_equal(stmt->keyword_token.lexeme,
                                                     SV_LIT("const")),
                    .identifier = stmt->identifier_token.lexeme,
                    .value = value};

  env_set(&interpreter->global_env, stmt->identifier_token.lexeme, entry);
}

void interpreter_eval_echo_stmt(Interpreter *interpreter, EchoStmt *stmt) {
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
}

void interpreter_eval_stmt(Interpreter *interpreter, Stmt *stmt) {
  switch (stmt->kind) {
  case STMT_KIND_EXPR:
    interpreter_eval_expr_stmt(interpreter, &stmt->as.expr);
    break;
  case STMT_KIND_VARIABLE_DECL:
    interpreter_eval_variable_decl_stmt(interpreter, &stmt->as.variable_decl);
    break;
  case STMT_KIND_ECHO:
    interpreter_eval_echo_stmt(interpreter, &stmt->as.echo);
    break;
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
  EnvEntry *entry =
      env_get(&interpreter->global_env, expr->identifier_token.lexeme);
  if (entry == NULL) {
    ErrorContext ctx = {
        .source = interpreter->source,
        .span = expr->identifier_token.span,
    };
    error_throw_fmt(&ctx, "attempt to assign a undefined variable: " SV_FMT ".",
                    SV_ARG(expr->identifier_token.lexeme));
  }

  if (entry->kind != ENV_ENTRY_KIND_VARIABLE) {
    ErrorContext ctx = {.source = interpreter->source,
                        .span = expr->identifier_token.span};
    error_throw_fmt(&ctx, "attempt to assign an non-variable: " SV_FMT ".",
                    SV_ARG(expr->identifier_token.lexeme));
  }

  EnvVariable *variable = &entry->as.variable;
  if (variable->constant) {
    ErrorContext ctx = {
        .source = interpreter->source,
        .span = expr->assignment_token.span,
    };
    error_throw_fmt(&ctx,
                    "attempt to reassign a constant variable: " SV_FMT ".",
                    SV_ARG(expr->identifier_token.lexeme));
  }

  Value value = interpreter_eval_expr(interpreter, expr->value);
  variable->value = value;
  return value;
}

Value interpreter_eval_identifier_expr(Interpreter *interpreter,
                                       IdentifierExpr *expr) {
  EnvEntry *entry =
      env_get(&interpreter->global_env, expr->identifier_token.lexeme);
  if (entry == NULL) {
    ErrorContext ctx = {.source = interpreter->source,
                        .span = expr->identifier_token.span};
    error_throw_fmt(&ctx,
                    "attempt to access a undefined identifier: " SV_FMT ".",
                    SV_ARG(expr->identifier_token.lexeme));
  }

  switch (entry->kind) {
  case ENV_ENTRY_KIND_VARIABLE:
    return entry->as.variable.value;
  default:
    fprintf(stderr, "ERROR: unreachable (interpreter_eval_identifier_expr).\n");
    exit(1);
  }
}

Value interpreter_eval_binary_expr(Interpreter *interpreter, BinaryExpr *expr) {
  Value left = interpreter_eval_expr(interpreter, expr->left);
  Value right = interpreter_eval_expr(interpreter, expr->right);

  if (left.kind == VALUE_KIND_STRING) {
    ErrorContext ctx = {.source = interpreter->source,
                        .span = expr->left->span};
    error_throw(&ctx,
                "attempt to perform a binary operation with a string operand.");
  }

  if (right.kind == VALUE_KIND_STRING) {
    ErrorContext ctx = {.source = interpreter->source,
                        .span = expr->right->span};
    error_throw(&ctx,
                "attempt to perform a binary operation with a string operand.");
  }

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
      error_throw(&ctx, "attempt divide by zero.");
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
    value.as.boolean = left.as.integer == right.as.integer;
    break;
  case TOKEN_KIND_NEQ:
    value.kind = VALUE_KIND_BOOLEAN;
    value.as.boolean = left.as.integer != right.as.integer;
    break;
  default:
    fprintf(stderr, "ERROR: unreachable (interpreter_eval_binary_expr).\n");
    exit(1);
  }

  return value;
}

Value interpreter_eval_logical_expr(Interpreter *interpreter,
                                    LogicalExpr *expr) {
  Value left = interpreter_eval_expr(interpreter, expr->left);
  Value right = interpreter_eval_expr(interpreter, expr->right);

  if (left.kind != VALUE_KIND_BOOLEAN) {
    ErrorContext ctx = {.source = interpreter->source,
                        .span = expr->left->span};
    error_throw(
        &ctx,
        "attempt to perform a logical operation with a non-boolean operand.");
  }

  if (right.kind != VALUE_KIND_BOOLEAN) {
    ErrorContext ctx = {.source = interpreter->source,
                        .span = expr->right->span};
    error_throw(
        &ctx,
        "attempt to perform a logical operation with a non-boolean operand.");
  }

  Value value = {0};
  value.kind = VALUE_KIND_BOOLEAN;

  switch (expr->operator_token.kind) {
  case TOKEN_KIND_AND:
    value.as.boolean = left.as.boolean && right.as.boolean;
    break;
  case TOKEN_KIND_OR:
    value.as.boolean = left.as.boolean || right.as.boolean;
    break;
  default:
    fprintf(stderr, "ERROR: unreachable (interpreter_eval_logical_expr).\n");
    exit(1);
  }

  return value;
}

Value interpreter_eval_unary_expr(Interpreter *interpreter, UnaryExpr *unary) {
  Value value = interpreter_eval_expr(interpreter, unary->operand);

  switch (unary->operator_token.kind) {
  case TOKEN_KIND_MINUS: {
    if (value.kind != VALUE_KIND_INTEGER) {
      ErrorContext ctx = {.source = interpreter->source,
                          .span = unary->operand->span};
      error_throw(&ctx, "unary operand for operator '-' must be a integer.");
    }

    value.as.integer *= -1;
    break;
  }
  case TOKEN_KIND_PLUS: {
    if (value.kind != VALUE_KIND_INTEGER) {
      ErrorContext ctx = {.source = interpreter->source,
                          .span = unary->operand->span};
      error_throw(&ctx, "unary operand for operator '+' must be a integer.");
    }
    
    break;
  }
  case TOKEN_KIND_NOT: {
    if (value.kind != VALUE_KIND_BOOLEAN) {
      ErrorContext ctx = {.source = interpreter->source,
                          .span = unary->operand->span};
      error_throw(&ctx, "unary operand for operator 'not' must be a boolean.");
    }
    
    value.as.boolean = !value.as.boolean;
    break;
  }
  default: {
    ErrorContext ctx = {.source = interpreter->source,
                        .span = unary->operator_token.span};
    error_throw(&ctx, "invalid unary operator, expects '-' or '+'.");
  }
  }

  return value;
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
  default:
    fprintf(stderr, "ERROR: unreachable (interpreter_eval_expr).\n");
    exit(1);
  }
}
