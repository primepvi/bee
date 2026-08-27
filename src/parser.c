#include "parser.h"
#include "ast.h"
#include "lexer.h"
#include "libs/array_list.h"
#include "libs/string_view.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Token parser_eat_token(Parser *parser) {
  Token eated = parser->current_token;
  parser->current_token = lexer_next_token(&parser->lexer);
  return eated;
}

Token parser_expect_token(Parser *parser, TokenKind kind) {
  Token current = parser_eat_token(parser);
  if (current.kind != kind) {
    fprintf(stderr, "ERROR: expected token kind %s, but received %s.\n",
            TOKEN_NAMES[kind], TOKEN_NAMES[current.kind]);
    exit(1);
  }

  return current;
}

LiteralExpr parser_parse_literal_expr(Parser *parser) {
  LiteralExpr expr = {0};

  Token token = parser_eat_token(parser);
  switch (token.kind) {
  case TOKEN_KIND_NUMBER: {
    char *buffer = string_view_to_cstr(token.lexeme);
    expr.kind = LITERAL_KIND_INTEGER;
    expr.as.integer = atoi(buffer);
    free(buffer);
    break;
  }
  case TOKEN_KIND_STRING: {
    expr.kind = LITERAL_KIND_STRING;
    expr.as.string = token.lexeme;
    break;
  }
  default: {
    fprintf(stderr, "ERROR: unreachable (parser_parse_literal_expr).");
    exit(1);
  }
  }

  return expr;
}

AssignmentExpr parser_parse_assignment_expr(Parser *parser, Token identifier) {
  parser_expect_token(parser, TOKEN_KIND_EQUAL);
  Expr aux = parser_parse_expr(parser);
  Expr *value = malloc(sizeof(Expr));
  memcpy(value, &aux, sizeof(Expr));

  return (AssignmentExpr){.identifier = identifier.lexeme, .value = value};
}

Expr parser_parse_primary_expr(Parser *parser) {
  Expr expr = {0};
  switch (parser->current_token.kind) {
  case TOKEN_KIND_NUMBER:
  case TOKEN_KIND_STRING: {
    expr.kind = EXPR_KIND_LITERAL;
    expr.as.literal = parser_parse_literal_expr(parser);
    break;
  }
  case TOKEN_KIND_IDENTIFIER: {
    Token identifier = parser_eat_token(parser);
    if (parser->current_token.kind == TOKEN_KIND_EQUAL) {
      expr.kind = EXPR_KIND_ASSIGNMENT;
      expr.as.assignment = parser_parse_assignment_expr(parser, identifier);
    } else {
      expr.kind = EXPR_KIND_IDENTIFIER;
      expr.as.identifier = (IdentifierExpr){identifier.lexeme};
    }
    break;
  }
  default: {
    fprintf(stderr, "ERROR: unexpected token found during stmt parsing: %s\n",
            TOKEN_NAMES[parser->current_token.kind]);
    exit(1);
  }
  }

  return expr;
}

Expr parser_parse_binary_expr(Parser *parser, u32 priority) {
  Expr left = parser_parse_primary_expr(parser);

  while (true) {
    u32 op_priority = ast_binary_operator_priority(parser->current_token.kind);
    if (op_priority == 0 || op_priority <= priority) {
      break;
    }

    BinaryExpr expr = {0};
    expr.operator_token = parser_eat_token(parser);
    expr.left = malloc(sizeof(Expr));
    expr.right = malloc(sizeof(Expr));
    
    Expr right = parser_parse_binary_expr(parser, op_priority);
    memcpy(expr.right, &right, sizeof(Expr));
    memcpy(expr.left, &left, sizeof(Expr));
    
    left.kind = EXPR_KIND_BINARY;
    left.as.binary = expr;
  }

  return left;
}

Expr parser_parse_expr(Parser *parser) {
  return parser_parse_binary_expr(parser, 0);
}  

VariableDeclStmt parser_parse_variable_decl_stmt(Parser *parser) {
  b8 is_const = parser_eat_token(parser).kind == TOKEN_KIND_CONST;
  Token identifier = parser_expect_token(parser, TOKEN_KIND_IDENTIFIER);
  parser_expect_token(parser, TOKEN_KIND_EQUAL);

  Expr value = parser_parse_expr(parser);
  return (VariableDeclStmt){
      .identifier = identifier.lexeme, .is_const = is_const, .value = value};
}

Stmt parser_parse_stmt(Parser *parser) {
  Stmt stmt = {0};

  switch (parser->current_token.kind) {
  case TOKEN_KIND_CONST:
  case TOKEN_KIND_LET: {
    stmt.kind = STMT_KIND_VARIABLE_DECL;
    stmt.as.variable_decl = parser_parse_variable_decl_stmt(parser);
    break;
  }
  default: {
    Expr expr = parser_parse_expr(parser);
    stmt.kind = STMT_KIND_EXPR;
    stmt.as.expr = (ExprStmt){expr};
  }
  }

  return stmt;
}

Parser parser_create(StringView source) {
  Parser parser = {0};
  parser.lexer = lexer_create(source);
  parser.source = source;
  parser.current_token = lexer_next_token(&parser.lexer);

  return parser;
}

Program parser_parse(Parser *parser) {
  ArrayList *stmts = array_list_new(32, sizeof(Stmt));
  while (parser->current_token.kind != TOKEN_KIND_EOF) {
    Stmt stmt = parser_parse_stmt(parser);
    array_list_push(stmts, &stmt);
  }

  return (Program){stmts};
}
