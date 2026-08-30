#include "typechecker.h"
#include "ast.h"
#include "libs/array_list.h"
#include "libs/error.h"
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

  // builtin types
  tc_define_type(&tc, (Type){.identifier = SV_LIT("int")});
  tc_define_type(&tc, (Type){.identifier = SV_LIT("bool")});
  tc_define_type(&tc, (Type){.identifier = SV_LIT("string")});
  tc_define_type(&tc, (Type){.identifier = SV_LIT("void")});

  return tc;
}

void tc_define_type(TypeChecker *tc, Type type) {
  hashmap_put(tc->type_env, type.identifier, sizeof(Type), &type);
}

Type tc_get_type(TypeChecker *tc, StringView identifier) {
  Type *type = hashmap_get(tc->type_env, identifier);
  if (type == NULL) {
    fprintf(stderr, "ERROR: attempt to get and undefined type: " SV_FMT "\n.",
            SV_ARG(identifier));
    exit(1);
  }

  return *type;
}

void tc_check(TypeChecker *tc) {
  for (u32 i = 0; i < array_list_length(tc->program->stmts); i++) {
    Stmt *stmt = array_list_at(tc->program->stmts, i);
    tc_check_stmt(tc, stmt);
  }
}

void tc_check_variable_decl_stmt(TypeChecker *tc, Stmt *stmt) {
  VariableDeclStmt *decl = &stmt->as.variable_decl;
  if (symtable_has(tc->symbols, decl->identifier_token.lexeme)) {
    ErrorContext ctx = {.source = tc->source,
                        .span = decl->identifier_token.span};
    error_throw_fmt(&ctx, "variable " SV_FMT " is already declared.",
                    SV_ARG(decl->identifier_token.lexeme));
  }

  Symbol symbol = {0};
  symbol.constant =
      string_view_is_equal(decl->keyword_token.lexeme, SV_LIT("const"));
  symbol.value_expr = &decl->value;
  symbol.value = NULL;
  symbol.identifier = decl->identifier_token.lexeme;
  symbol.type = tc_check_expr(tc, &decl->value);

  symtable_put(tc->symbols, symbol);
}

void tc_check_block_stmt(TypeChecker *tc, Stmt *stmt) {
  BlockStmt *block = &stmt->as.block;
  SymbolTable scope = symtable_new(tc->symbols);
  tc->symbols = &scope;

  for (u32 i = 0; i < array_list_length(block->stmts); i++) {
    Stmt *stmt = array_list_at(block->stmts, i);
    tc_check_stmt(tc, stmt);
  }
  
  tc->symbols = scope.parent;
  symtable_destroy(&scope);
}

void tc_check_if_stmt(TypeChecker *tc, Stmt *stmt) {
  IfStmt *if_stmt = &stmt->as.if_stmt;
  Type condition_type = tc_check_expr(tc, if_stmt->condition);
  if (!type_is(condition_type, SV_LIT("bool"))) {
    ErrorContext ctx = {.source = tc->source,
                        .span = if_stmt->condition->span};
    error_throw_fmt(&ctx,
                    "if condition must be of type 'bool', but received an condition of type '" SV_FMT "'.",
                    SV_ARG(condition_type.identifier));
  }

  tc_check_stmt(tc, if_stmt->consequent);
  if (if_stmt->alternate != NULL) {
    tc_check_stmt(tc, if_stmt->alternate);
  }
}  

void tc_check_stmt(TypeChecker *tc, Stmt *stmt) {
  switch (stmt->kind) {
  case STMT_KIND_ECHO: {
    EchoStmt *echo = &stmt->as.echo;
    tc_check_expr(tc, &echo->message);
    break;
  }
  case STMT_KIND_EXPR: {
    ExprStmt *expr = &stmt->as.expr;
    tc_check_expr(tc, &expr->expr);
    break;
  }
  case STMT_KIND_VARIABLE_DECL: {
    tc_check_variable_decl_stmt(tc, stmt);
    break;
  }
  case STMT_KIND_BLOCK: {
    tc_check_block_stmt(tc, stmt);
    break;
  }
  case STMT_KIND_IF: {
    tc_check_if_stmt(tc, stmt);
    break;
  }    
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
    return tc_get_type(tc, SV_LIT("int"));
  case TOKEN_KIND_TRUE:
  case TOKEN_KIND_FALSE:
    return tc_get_type(tc, SV_LIT("bool"));
  case TOKEN_KIND_STRING:
    return tc_get_type(tc, SV_LIT("string"));
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

  return symbol->type;
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
    return tc_get_type(tc, SV_LIT("int"));
  case TOKEN_KIND_LT:
  case TOKEN_KIND_LTE:
  case TOKEN_KIND_GT:
  case TOKEN_KIND_GTE:
  case TOKEN_KIND_EQEQ:
  case TOKEN_KIND_NEQ:
    return tc_get_type(tc, SV_LIT("bool"));
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
    return tc_get_type(tc, SV_LIT("bool"));
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
    ErrorContext ctx = {
        .source = tc->source,
        .span = expr->span
    };
    error_throw_fmt(&ctx, "attempt to assign a undefined variable: " SV_FMT ".",
                    SV_ARG(assignment->identifier_token.lexeme));
  }
  
  if (symbol->constant) {
    ErrorContext ctx = {
        .source = tc->source,
        .span = expr->span,
    };
    error_throw_fmt(&ctx,
                    "attempt to assign a constant variable: " SV_FMT ".",
                    SV_ARG(assignment->identifier_token.lexeme));
  }

  Type type = tc_check_expr(tc, assignment->value);
  if (!type_is_equal(type, symbol->type)) {
    ErrorContext ctx = {
        .source = tc->source,
        .span = expr->span,
    };
    error_throw_fmt(&ctx,
                    "attempt to assign a '" SV_FMT "' value to a '"SV_FMT"' variable.",
                    SV_ARG(type.identifier), SV_ARG(symbol->type.identifier));
  }

  symbol->type = type;
  symbol->value_expr = assignment->value;

  return type;
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
    return true;

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

b8 type_is_equal(Type a, Type b) {
  return string_view_is_equal(a.identifier, b.identifier);
}

b8 type_is_numeric(Type type) { return type_is(type, SV_LIT("int")); }
