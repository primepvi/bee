#include "ast.h"
#include "libs/array_list.h"
#include <stdio.h>

const char *const TOKEN_NAMES[TOKEN_KIND_KEY_COUNT] = {
    [TOKEN_KIND_LET] = "Let",
    [TOKEN_KIND_CONST] = "Const",
    [TOKEN_KIND_ECHO] = "Echo",
    [TOKEN_KIND_TRUE] = "True",
    [TOKEN_KIND_AND] = "And",
    [TOKEN_KIND_OR] = "Or",
    [TOKEN_KIND_NOT] = "Not",
    [TOKEN_KIND_FALSE] = "False",
    [TOKEN_KIND_THEN] = "Then",
    [TOKEN_KIND_END] = "End",
    [TOKEN_KIND_IF] = "If",
    [TOKEN_KIND_ELSE] = "Else",
    [TOKEN_KIND_WHILE] = "While",
    [TOKEN_KIND_FOR] = "For",
    [TOKEN_KIND_DO] = "Do",
    [TOKEN_KIND_WHEN] = "When",
    [TOKEN_KIND_OTHERWISE] = "Otherwise",
    [TOKEN_KIND_FN] = "Fn",
    [TOKEN_KIND_IDENTIFIER] = "Identifier",
    [TOKEN_KIND_NUMBER] = "Number",
    [TOKEN_KIND_STRING] = "String",
    [TOKEN_KIND_COLON] = "Colon",
    [TOKEN_KIND_SEMICOLON] = "Semi Colon",
    [TOKEN_KIND_EQUAL] = "Equal",
    [TOKEN_KIND_OPEN_PAREN] = "Open Paren",
    [TOKEN_KIND_CLOSE_PAREN] = "Close Paren",
    [TOKEN_KIND_PLUS] = "Plus",
    [TOKEN_KIND_MINUS] = "Minus",
    [TOKEN_KIND_STAR] = "Star",
    [TOKEN_KIND_SLASH] = "Slash",
    [TOKEN_KIND_GT] = "Greater Than",
    [TOKEN_KIND_GTE] = "Greater Than Equal",
    [TOKEN_KIND_LT] = "Less Than",
    [TOKEN_KIND_LTE] = "Less Than Equal",
    [TOKEN_KIND_ARROW] = "Arrow",
    [TOKEN_KIND_EQEQ] = "Equal Equal",
    [TOKEN_KIND_NEQ] = "Not Equal",
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
    printf("Literal: (%s) " SV_FMT "\n", TOKEN_NAMES[literal->value_token.kind], SV_ARG(literal->value_token.lexeme));
    break;
  }
  case EXPR_KIND_IDENTIFIER: {
    const IdentifierExpr *identifier = &expr->as.identifier;
    printf("Identifier: " SV_FMT "\n", SV_ARG(identifier->identifier_token.lexeme));
    break;
  }
  case EXPR_KIND_ASSIGNMENT: {
    const AssignmentExpr *assignment = &expr->as.assignment;
    printf("Assignment\n");

    print_indent(depth + 1);
    printf("Identifier: " SV_FMT "\n", SV_ARG(assignment->identifier_token.lexeme));

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
  case EXPR_KIND_LOGICAL: {
    const LogicalExpr *logical = &expr->as.logical;
    printf("Logical\n");

    print_indent(depth + 1);
    printf("Operator: %s\n", TOKEN_NAMES[logical->operator_token.kind]);

    print_indent(depth + 1);
    printf("Left\n");
    ast_print_expr(logical->left, depth + 2);

    print_indent(depth + 1);
    printf("Right\n");
    ast_print_expr(logical->right, depth + 2);
    break;
  }    
  case EXPR_KIND_PARENTHESIZED: {
    const ParenthesizedExpr *parenthesized = &expr->as.parenthesized;
    printf("Parenthesized\n");

    print_indent(depth + 1);
    printf("Expr\n");
    ast_print_expr(parenthesized->expr, depth + 2);
    break;
  }
  case EXPR_KIND_UNARY: {
    const UnaryExpr *unary = &expr->as.unary;
    printf("Unary\n");

    print_indent(depth + 1);
    printf("Operator: %s\n", TOKEN_NAMES[unary->operator_token.kind]);

    print_indent(depth + 1);
    printf("Operand\n");
    ast_print_expr(unary->operand, depth + 2);
    break;
  }
  case EXPR_KIND_WHEN: {
    const WhenExpr *when = &expr->as.when;
    printf("When\n");

    print_indent(depth + 1);
    printf("Condition\n");
    ast_print_expr(when->condition, depth + 2);

    print_indent(depth + 1);
    printf("Consequent\n");
    ast_print_expr(when->consequent, depth + 2);

    print_indent(depth + 1);
    printf("Alternate\n");
    ast_print_expr(when->alternate, depth + 2);
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
    printf("VariableDeclaration ("SV_FMT")\n", SV_ARG(decl->keyword_token.lexeme));

    print_indent(depth + 1);
    printf("Identifier: " SV_FMT "\n", SV_ARG(decl->identifier_token.lexeme));

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
  case STMT_KIND_BLOCK: {
    const BlockStmt *block = &stmt->as.block;
    printf("Block\n");
    for (u32 i = 0; i < array_list_length(block->stmts); i++) {
      Stmt *cur = array_list_at(block->stmts, i);
      ast_print_stmt(cur, depth + 1);
    }
    break;
  }
  case STMT_KIND_IF: {
    const IfStmt *if_stmt = &stmt->as.if_stmt;
    printf("If\n");

    print_indent(depth + 1);
    printf("Condition\n");
    ast_print_expr(if_stmt->condition, depth + 2);

    print_indent(depth + 1);
    printf("Consequent\n");
    ast_print_stmt(if_stmt->consequent, depth + 2);

    if (if_stmt->alternate != NULL) {
      print_indent(depth + 1);
      printf("Alternate\n");
      ast_print_stmt(if_stmt->alternate, depth + 2);
    }
    break;
  }
  case STMT_KIND_WHILE: {
    const WhileStmt *while_stmt = &stmt->as.while_stmt;
    printf("While\n");
    
    print_indent(depth + 1);
    printf("Condition\n");
    ast_print_expr(while_stmt->condition, depth + 2);
    
    print_indent(depth + 1);
    printf("Body\n");
    ast_print_stmt(while_stmt->body, depth + 2);
    break;
  }
  case STMT_KIND_FOR: {
    const ForStmt *for_stmt = &stmt->as.for_stmt;
    printf("For\n");

    print_indent(depth + 1);
    printf("Init\n");
    ast_print_stmt(for_stmt->init, depth + 2);

    print_indent(depth + 1);
    printf("Test\n");
    ast_print_expr(for_stmt->test, depth + 2);

    print_indent(depth + 1);
    printf("Update\n");
    ast_print_expr(for_stmt->update, depth + 2);

    print_indent(depth + 1);
    printf("Body\n");
    ast_print_stmt(for_stmt->body, depth + 2);
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

u32 ast_unary_operator_priority(TokenKind kind) {
  switch (kind) {
  case TOKEN_KIND_PLUS:
  case TOKEN_KIND_MINUS:
  case TOKEN_KIND_NOT:    
    return 4;

  default:
    return 0;
  }
}

u32 ast_binary_operator_priority(TokenKind kind) {
  switch (kind) {
  case TOKEN_KIND_STAR:
  case TOKEN_KIND_SLASH:
  case TOKEN_KIND_PERCENTAGE:
    return 3;

  case TOKEN_KIND_PLUS:
  case TOKEN_KIND_MINUS:
    return 2;
  case TOKEN_KIND_GT:
  case TOKEN_KIND_GTE:
  case TOKEN_KIND_LT:
  case TOKEN_KIND_LTE:
  case TOKEN_KIND_EQEQ:
  case TOKEN_KIND_NEQ:
    return 1;    

  default:
    return 0;
  }
}

b8 ast_token_kind_compare(void *current, void *expected) {
  return *(TokenKind*)current == *(TokenKind*)expected;
}  
