#include "interpreter.h"
#include "ast.h"
#include "libs/array_list.h"
#include "libs/env.h"
#include "libs/string_view.h"
#include <stdio.h>
#include <stdlib.h>

Interpreter interpreter_create(Program *program) {
  Interpreter interpreter = {0};
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
  if (env_has(&interpreter->global_env, stmt->identifier)) {
    fprintf(stderr, "ERROR: Variable " SV_FMT " is already declared.\n",
            SV_ARG(stmt->identifier));
    exit(1);
  }

  Value value = interpreter_eval_expr(interpreter, &stmt->value);
  EnvEntry entry = {0};
  entry.kind = ENV_ENTRY_KIND_VARIABLE;
  entry.as.variable = (EnvVariable){.constant = stmt->is_const,
                                    .identifier = stmt->identifier,
                                    .value = value};

  env_set(&interpreter->global_env, stmt->identifier, entry);
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
  switch (expr->value.kind) {
  case TOKEN_KIND_NUMBER: {
    char *buffer = string_view_to_cstr(expr->value.lexeme);
    value.kind = VALUE_KIND_INTEGER;
    value.as.integer = atoi(buffer);
    free(buffer);
    break;
  }
  case TOKEN_KIND_STRING: {
    value.kind = VALUE_KIND_STRING;
    value.as.string = expr->value.lexeme;
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
  EnvEntry *entry = env_get(&interpreter->global_env, expr->identifier);
  if (entry == NULL) {
    fprintf(stderr,
            "ERROR: attempt to assign to a non-declared variable: " SV_FMT "\n",
            SV_ARG(expr->identifier));
    exit(1);
  }

  if (entry->kind != ENV_ENTRY_KIND_VARIABLE) {
    fprintf(stderr,
            "ERROR: invalid assignment: " SV_FMT "\n",
            SV_ARG(expr->identifier));
    exit(1);
  }

  EnvVariable *variable = &entry->as.variable;
  if (variable->constant) {
    fprintf(stderr,
            "ERROR: attempt to reassign the constant: " SV_FMT "\n",
            SV_ARG(expr->identifier));
    exit(1);
  }

  Value value = interpreter_eval_expr(interpreter, expr->value);
  variable->value = value;
  return value;
}

Value interpreter_eval_identifier_expr(Interpreter *interpreter,
                                       IdentifierExpr *expr) {
  EnvEntry *entry = env_get(&interpreter->global_env, expr->name);
  if (entry == NULL) {
    fprintf(stderr,
            "ERROR: attempt to access an undefined identifier: " SV_FMT "\n",
            SV_ARG(expr->name));
    exit(1);
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
  if (left.kind != VALUE_KIND_INTEGER || right.kind != VALUE_KIND_INTEGER) {
    fprintf(stderr, "ERROR: invalid operands in binary expr.\n");
    exit(1);
  }

  Value value = {0};
  value.kind = VALUE_KIND_INTEGER;
  value.as.integer = 0;

  switch (expr->operator_token.kind) {
  case TOKEN_KIND_PLUS:
    value.as.integer = left.as.integer + right.as.integer;
    break;
  case TOKEN_KIND_MINUS:
    value.as.integer = left.as.integer - right.as.integer;
    break;
  case TOKEN_KIND_STAR:
    value.as.integer = left.as.integer * right.as.integer;
    break;
  case TOKEN_KIND_SLASH: {
    if (right.as.integer == 0) {
      fprintf(stderr, "ERROR: division by zero.\n");
      exit(1);
    }
    
    value.as.integer = left.as.integer / right.as.integer;
    break;
  }    
  case TOKEN_KIND_PERCENTAGE:
    value.as.integer = left.as.integer % right.as.integer;
    break;    
  default:
    fprintf(stderr, "ERROR: unreachable (interpreter_eval_binary_expr).\n");
    exit(1);
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
  default:
    fprintf(stderr, "ERROR: unreachable (interpreter_eval_expr).\n");
    exit(1);
  }    
}  

