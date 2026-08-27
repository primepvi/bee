#include "ast.h"
#include "libs/array_list.h"
#include <stdio.h>

const char *const TOKEN_NAMES[TOKEN_KIND_KEY_COUNT] = {
    [TOKEN_KIND_LET] = "Let",
    [TOKEN_KIND_CONST] = "Const",
    [TOKEN_KIND_ECHO] = "Echo",
    [TOKEN_KIND_IDENTIFIER] = "Identifier",
    [TOKEN_KIND_NUMBER] = "Number",
    [TOKEN_KIND_STRING] = "String",
    [TOKEN_KIND_SEMICOLON] = "Semi Colon",
    [TOKEN_KIND_EQUAL] = "Equal",
    [TOKEN_KIND_PLUS] = "Plus",
    [TOKEN_KIND_MINUS] = "Minus",
    [TOKEN_KIND_STAR] = "Star",
    [TOKEN_KIND_SLASH] = "Slash",
    [TOKEN_KIND_PERCENTAGE] = "Percentage",
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
    printf("Literal: " SV_FMT "\n", SV_ARG(literal->value.lexeme));
    break;
  }
  case EXPR_KIND_IDENTIFIER: {
    const IdentifierExpr *identifier = &expr->as.identifier;
    printf("Identifier: " SV_FMT "\n", SV_ARG(identifier->name));
    break;
  }
  case EXPR_KIND_ASSIGNMENT: {
    const AssignmentExpr *assignment = &expr->as.assignment;
    printf("Assignment\n");

    print_indent(depth + 1);
    printf("Identifier: " SV_FMT "\n", SV_ARG(assignment->identifier));

    print_indent(depth + 1);
    printf("Value: \n");
    ast_print_expr(assignment->value, depth + 2);
    break;
  }
  case EXPR_KIND_BINARY: {
    const BinaryExpr *binary = &expr->as.binary;
    printf("Binary\n");

    print_indent(depth + 1);
    printf("Operator: %s\n", TOKEN_NAMES[binary->operator_token.kind]);

    print_indent(depth + 1);
    printf("Left\n");
    ast_print_expr(binary->left, depth + 2);

    print_indent(depth + 1);
    printf("Right\n");
    ast_print_expr(binary->right, depth + 2);
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
  case STMT_KIND_EXPR: {
    const ExprStmt *expr = &stmt->as.expr;
    printf("ExprStmt\n");
    ast_print_expr(&expr->expr, depth + 1);
    break;
  }
  case STMT_KIND_ECHO: {
    const EchoStmt *echo = &stmt->as.echo;
    printf("Echo\n");
    ast_print_expr(&echo->message, depth + 1);
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

  for (u32 i = 0; i < array_list_length(program->stmts); i++) {
    ast_print_stmt(array_list_at(program->stmts, i), 1);
  }
}

u32 ast_binary_operator_priority(TokenKind kind) {
  switch (kind) {
  case TOKEN_KIND_STAR:
  case TOKEN_KIND_SLASH:
  case TOKEN_KIND_PERCENTAGE:
    return 2;

  case TOKEN_KIND_PLUS:
  case TOKEN_KIND_MINUS:
    return 1;

  default:
    return 0;
  }
}
