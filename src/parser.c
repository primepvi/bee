#include "parser.h"
#include "ast.h"
#include "lexer.h"
#include "libs/string_view.h"
#include <stdio.h>
#include <stdlib.h>

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
    const char *buffer = string_view_to_cstr(token.lexeme);
    expr.kind = LITERAL_KIND_STRING;
    expr.as.string = buffer;
    break;
  }
  default: {
    fprintf(stderr, "ERROR: unreachable (parser_parse_literal_expr).");
    exit(1);
  }
  }

  return expr;
}

Expr parser_parse_expr(Parser *parser) {
  Expr expr = {0};
  switch (parser->current_token.kind) {
  case TOKEN_KIND_NUMBER:
  case TOKEN_KIND_STRING: {
    expr.kind = EXPR_KIND_LITERAL;
    expr.as.literal = parser_parse_literal_expr(parser);
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

VariableDeclStmt parser_parse_variable_decl_stmt(Parser *parser) {
  b8 is_const = parser_eat_token(parser).kind == TOKEN_KIND_CONST;
  Token identifier = parser_expect_token(parser, TOKEN_KIND_IDENTIFIER);
  parser_expect_token(parser, TOKEN_KIND_EQUAL);

  Expr value = parser_parse_expr(parser);
  parser_expect_token(parser, TOKEN_KIND_SEMICOLON);

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
    fprintf(stderr, "ERROR: unexpected token found during stmt parsing: %s\n",
            TOKEN_NAMES[parser->current_token.kind]);
    exit(1);
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
  u32 stmts_length = 0;
  u32 stmts_capacity = 32;
  Stmt *stmts = malloc(sizeof(Stmt) * stmts_capacity);

  while (parser->current_token.kind != TOKEN_KIND_EOF) {
    if (stmts_length >= stmts_capacity) {
      stmts_capacity *= 2;
      stmts = realloc(stmts, sizeof(Stmt) * stmts_capacity);
    }

    stmts[stmts_length++] = parser_parse_stmt(parser);
  }

  return (Program){stmts, stmts_length, stmts_capacity};
}
