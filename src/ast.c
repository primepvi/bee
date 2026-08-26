#include "ast.h"
#include <stdio.h>

const char *const TOKEN_NAMES[TOKEN_KIND_KEY_COUNT] = {
    [TOKEN_KIND_LET] = "Let",
    [TOKEN_KIND_CONST] = "Const",
    [TOKEN_KIND_IDENTIFIER] = "Identifier",
    [TOKEN_KIND_NUMBER] = "Number",
    [TOKEN_KIND_STRING] = "String",
    [TOKEN_KIND_SEMICOLON] = "Semi Colon",
    [TOKEN_KIND_EQUAL] = "Equal",
    [TOKEN_KIND_EOF] = "End of File",
};

static void print_indent(u32 depth) {
  for (u32 i = 0; i < depth; ++i) {
    printf("  ");
  }
}

static void ast_print_expr(const Expr *expr, u32 depth) {
  print_indent(depth);

  if (!expr) {
    printf("<null>\n");
    return;
  }

  switch (expr->kind) {
  case EXPR_KIND_LITERAL: {
    const LiteralExpr *literal = &expr->as.literal;

    printf("Literal\n");

    print_indent(depth + 1);

    switch (literal->kind) {
    case LITERAL_KIND_INTEGER:
      printf("Integer: %lld\n", literal->as.integer);
      break;

    case LITERAL_KIND_STRING:
      printf("String: \"%s\"\n", literal->as.string);
      break;
    }

    break;
  }
  }
}

static void ast_print_stmt(const Stmt *stmt, u32 depth) {
  print_indent(depth);

  if (!stmt) {
    printf("<null>\n");
    return;
  }

  switch (stmt->kind) {
  case STMT_KIND_VARIABLE_DECL: {
    const VariableDeclStmt *decl = &stmt->as.variable_decl;

    printf("VariableDeclaration (%s)\n", decl->is_const ? "const" : "let");

    print_indent(depth + 1);
    printf("Identifier: " SV_FMT "\n", SV_ARG(decl->identifier));

    print_indent(depth + 1);
    printf("Value\n");

    ast_print_expr(&decl->value, depth + 2);

    break;
  }
  }
}

void ast_print(const Program *program) {
  if (!program) {
    printf("<null>\n");
    return;
  }

  printf("Program\n");

  for (u32 i = 0; i < program->stmts_length; ++i) {
    ast_print_stmt(&program->stmts[i], 1);
  }
}
