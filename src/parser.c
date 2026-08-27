#include "parser.h"
#include "ast.h"
#include "lexer.h"
#include "libs/array_list.h"
#include "libs/error.h"
#include "libs/string_view.h"
#include "libs/types.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Token parser_eat_token(Parser *parser) {
  Token eated = parser->current_token;
  parser->current_token = lexer_next_token(&parser->lexer);
  return eated;
}

b8 parser_match_token(Parser *parser, TokenKind kind) {
  return kind == parser->current_token.kind;
}

LiteralExpr parser_parse_literal_expr(Parser *parser) {
  return (LiteralExpr){
      .value_token = parser_eat_token(parser),
  };
}

AssignmentExpr parser_parse_assignment_expr(Parser *parser,
                                            Token identifier_token) {
  if (!parser_match_token(parser, TOKEN_KIND_EQUAL)) {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};

    error_throw_fmt(&ctx, "expected '=', but received '" SV_FMT "'",
                    SV_ARG(parser->current_token.lexeme));
  }

  Token assignment_token = parser_eat_token(parser);

  Expr aux = parser_parse_expr(parser);
  Expr *value = malloc(sizeof(Expr));
  memcpy(value, &aux, sizeof(Expr));

  return (AssignmentExpr){
      .identifier_token = identifier_token,
      .assignment_token = assignment_token,
      .value = value,
  };
}

Expr parser_parse_primary_expr(Parser *parser) {
  Expr expr = {0};
  switch (parser->current_token.kind) {
  case TOKEN_KIND_NUMBER:
  case TOKEN_KIND_STRING: {
    LiteralExpr literal = parser_parse_literal_expr(parser);
    expr.kind = EXPR_KIND_LITERAL;
    expr.as.literal = literal;
    expr.span = literal.value_token.span;
    break;
  }
  case TOKEN_KIND_IDENTIFIER: {
    Token identifier_token = parser_eat_token(parser);
    if (parser->current_token.kind == TOKEN_KIND_EQUAL) {
      AssignmentExpr assignment =
          parser_parse_assignment_expr(parser, identifier_token);
      Span span = {.line = identifier_token.span.line,
                   .start = identifier_token.span.start,
                   .end = assignment.value->span.end};

      expr.kind = EXPR_KIND_ASSIGNMENT;
      expr.as.assignment = assignment;
      expr.span = span;
    } else {
      IdentifierExpr identifier = {.identifier_token = identifier_token};
      expr.kind = EXPR_KIND_IDENTIFIER;
      expr.as.identifier = identifier;
      expr.span = identifier_token.span;
    }
    break;
  }
  default: {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};

    error_throw_fmt(&ctx, "unexpected value found '" SV_FMT "'.",
                    SV_ARG(parser->current_token.lexeme));
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

    Span span = {
      .line = expr.left->span.line,
      .start = expr.left->span.start,
      .end = expr.right->span.end
    };
    
    left.kind = EXPR_KIND_BINARY;
    left.as.binary = expr;
    left.span = span;
  }

  return left;
}

Expr parser_parse_expr(Parser *parser) {
  return parser_parse_binary_expr(parser, 0);
}

VariableDeclStmt parser_parse_variable_decl_stmt(Parser *parser) {
  Token keyword_token = parser_eat_token(parser);
  if (!parser_match_token(parser, TOKEN_KIND_IDENTIFIER)) {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};

    error_throw_fmt(&ctx,
                    "expected an variable name, but received '" SV_FMT "'",
                    SV_ARG(parser->current_token.lexeme));
  }

  Token identifier_token = parser_eat_token(parser);
  if (!parser_match_token(parser, TOKEN_KIND_EQUAL)) {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};

    error_throw_fmt(&ctx, "expected '=', but received '" SV_FMT "'",
                    SV_ARG(parser->current_token.lexeme));
  }

  Token assignment_token = parser_eat_token(parser);
  Expr value = parser_parse_expr(parser);
  return (VariableDeclStmt){.keyword_token = keyword_token,
                            .identifier_token = identifier_token,
                            .assignment_token = assignment_token,
                            .value = value};
}

EchoStmt parser_parse_echo_stmt(Parser *parser) {
  Token keyword_token = parser_eat_token(parser);
  Expr message = parser_parse_expr(parser);

  return (EchoStmt){
      .message = message,
      .keyword_token = keyword_token,
  };
}

Stmt parser_parse_stmt(Parser *parser) {
  Stmt stmt = {0};

  switch (parser->current_token.kind) {
  case TOKEN_KIND_CONST:
  case TOKEN_KIND_LET: {
    VariableDeclStmt variable_decl = parser_parse_variable_decl_stmt(parser);
    Span span = {.line = variable_decl.keyword_token.span.line,
                 .start = variable_decl.keyword_token.span.start,
                 .end = variable_decl.value.span.end};

    stmt.kind = STMT_KIND_VARIABLE_DECL;
    stmt.as.variable_decl = variable_decl;
    stmt.span = span;
    break;
  }
  case TOKEN_KIND_ECHO: {
    EchoStmt echo = parser_parse_echo_stmt(parser);
    Span span = {.line = echo.keyword_token.span.line,
                 .start = echo.keyword_token.span.start,
                 .end = echo.message.span.end};

    stmt.kind = STMT_KIND_ECHO;
    stmt.as.echo = echo;
    stmt.span = span;
    break;
  }
  default: {
    Expr expr = parser_parse_expr(parser);
    stmt.kind = STMT_KIND_EXPR;
    stmt.as.expr = (ExprStmt){expr};
    stmt.span = expr.span;
  }
  }

  return stmt;
}

Parser parser_create(Source *source) {
  Parser parser = {0};
  parser.source = source;
  parser.lexer = lexer_create(source);
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
