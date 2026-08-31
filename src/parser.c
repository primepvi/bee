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

b8 parser_has_more_tokens(Parser *parser) {
  return parser->current_token.kind != TOKEN_KIND_EOF;
}

static b8 parser_block_reached_end(Parser *parser, TokenKind end_kind) {
  return parser_match_token(parser, end_kind);
}

static b8 parser_if_block_reached_end(Parser *parser) {
  return parser_match_token(parser, TOKEN_KIND_ELSE) ||
         parser_match_token(parser, TOKEN_KIND_END);
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

ParenthesizedExpr parser_parse_parenthesized_expr(Parser *parser) {
  Token open_paren_token = parser_eat_token(parser);
  Expr aux_expr = parser_parse_expr(parser);
  Expr *expr = malloc(sizeof(Expr));
  memcpy(expr, &aux_expr, sizeof(Expr));

  if (!parser_match_token(parser, TOKEN_KIND_CLOSE_PAREN)) {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};

    error_throw_fmt(
        &ctx,
        "unterminated parenthesized expression has found, expects ')' but "
        "received '" SV_FMT "'.",
        SV_ARG(parser->current_token.lexeme));
  }

  Token close_paren_token = parser_eat_token(parser);
  return (ParenthesizedExpr){.open_paren_token = open_paren_token,
                             .close_paren_token = close_paren_token,
                             .expr = expr};
}

WhenExpr parser_parse_when_expr(Parser *parser) {
  Token when_token = parser_eat_token(parser);
  Expr condition = parser_parse_expr(parser);

  if (!parser_match_token(parser, TOKEN_KIND_THEN)) {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};
    error_throw_fmt(
        &ctx,
        "when expression expects 'then' keyword, but received '" SV_FMT "'.",
        SV_ARG(parser->current_token.lexeme));
  }
  parser_eat_token(parser);
  Expr consequent = parser_parse_expr(parser);

  if (!parser_match_token(parser, TOKEN_KIND_OTHERWISE)) {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};
    error_throw_fmt(
        &ctx,
        "when expression expects 'otherwise' keyword, but received '" SV_FMT
        "'.",
        SV_ARG(parser->current_token.lexeme));
  }
  Token otherwise_token = parser_eat_token(parser);
  Expr alternate = parser_parse_expr(parser);

  WhenExpr when = {0};
  when.when_token = when_token;
  when.otherwise_token = otherwise_token;
  when.condition = malloc(sizeof(Expr));
  when.consequent = malloc(sizeof(Expr));
  when.alternate = malloc(sizeof(Expr));

  memcpy(when.condition, &condition, sizeof(Expr));
  memcpy(when.consequent, &consequent, sizeof(Expr));
  memcpy(when.alternate, &alternate, sizeof(Expr));

  return when;
}

CallExpr parser_parse_call_expr(Parser *parser, Token identifier_token) {
  Token open_paren_token = parser_eat_token(parser);
  ArrayList *arguments = array_list_new(32, sizeof(Expr));
  while (parser_has_more_tokens(parser) &&
         !parser_match_token(parser, TOKEN_KIND_CLOSE_PAREN)) {
    Expr expr = parser_parse_expr(parser);
    array_list_push(arguments, &expr);

    if (parser_match_token(parser, TOKEN_KIND_COMMA)) {
      parser_eat_token(parser);
    } else {
      break;
    }      
  }
  
  if (!parser_match_token(parser, TOKEN_KIND_CLOSE_PAREN)) {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};
    error_throw(&ctx, "expects ')' to close function call.");
  }
  
  Token close_paren_token = parser_eat_token(parser);
  return (CallExpr) {
    .identifier_token = identifier_token, .open_paren_token = open_paren_token,
    .close_paren_token = close_paren_token, .arguments = arguments
  };
}


b8 parser_can_start_expr(Parser *parser) {
  switch (parser->current_token.kind) {
  case TOKEN_KIND_NULL:
  case TOKEN_KIND_TRUE:
  case TOKEN_KIND_FALSE:
  case TOKEN_KIND_NUMBER:
  case TOKEN_KIND_STRING:
  case TOKEN_KIND_OPEN_PAREN:
  case TOKEN_KIND_IDENTIFIER:
  case TOKEN_KIND_WHEN:
    return true;

  default:
    return false;
  }
}

Expr parser_parse_primary_expr(Parser *parser) {
  Expr expr = {0};
  switch (parser->current_token.kind) {
  case TOKEN_KIND_NULL:
  case TOKEN_KIND_TRUE:
  case TOKEN_KIND_FALSE:
  case TOKEN_KIND_NUMBER:
  case TOKEN_KIND_STRING: {
    LiteralExpr literal = parser_parse_literal_expr(parser);
    expr.kind = EXPR_KIND_LITERAL;
    expr.as.literal = literal;
    expr.span = literal.value_token.span;
    break;
  }
  case TOKEN_KIND_OPEN_PAREN: {
    ParenthesizedExpr parenthesized = parser_parse_parenthesized_expr(parser);
    Span span = {
        .line = parenthesized.open_paren_token.span.line,
        .start = parenthesized.open_paren_token.span.start,
        .end = parenthesized.close_paren_token.span.end,
    };

    expr.kind = EXPR_KIND_PARENTHESIZED;
    expr.as.parenthesized = parenthesized;
    expr.span = span;
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
    } else if (parser->current_token.kind == TOKEN_KIND_OPEN_PAREN) {
      CallExpr call = parser_parse_call_expr(parser, identifier_token);
      Span span = {.line = call.identifier_token.span.line,
                   .start = call.identifier_token.span.start,
                   .end = call.close_paren_token.span.end};
      expr.kind = EXPR_KIND_CALL;
      expr.as.call = call;
      expr.span = span;
    } else {
      IdentifierExpr identifier = {.identifier_token = identifier_token};
      expr.kind = EXPR_KIND_IDENTIFIER;
      expr.as.identifier = identifier;
      expr.span = identifier_token.span;
    }
    break;
  }
  case TOKEN_KIND_WHEN: {
    WhenExpr when = parser_parse_when_expr(parser);
    Span span = {.line = when.when_token.span.line,
                 .start = when.when_token.span.start,
                 .end = when.alternate->span.end};
    expr.kind = EXPR_KIND_WHEN;
    expr.as.when = when;
    expr.span = span;
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
  u32 unary_op_priority =
      ast_unary_operator_priority(parser->current_token.kind);
  Expr left;
  if (unary_op_priority != 0 && unary_op_priority >= priority) {
    UnaryExpr unary = {0};
    unary.operator_token = parser_eat_token(parser);
    unary.operand = malloc(sizeof(Expr));

    Expr operand = parser_parse_binary_expr(parser, unary_op_priority);
    memcpy(unary.operand, &operand, sizeof(Expr));

    Span span = {.line = unary.operator_token.span.line,
                 .start = unary.operator_token.span.start,
                 .end = operand.span.end};

    left.kind = EXPR_KIND_UNARY;
    left.as.unary = unary;
    left.span = span;
  } else {
    left = parser_parse_primary_expr(parser);
  }

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

    Span span = {.line = expr.left->span.line,
                 .start = expr.left->span.start,
                 .end = expr.right->span.end};

    left.kind = EXPR_KIND_BINARY;
    left.as.binary = expr;
    left.span = span;
  }

  return left;
}

Expr parser_parse_logical_expr(Parser *parser) {
  Expr left = parser_parse_binary_expr(parser, 0);

  if (parser_match_token(parser, TOKEN_KIND_AND) ||
      parser_match_token(parser, TOKEN_KIND_OR)) {
    LogicalExpr expr = {0};
    expr.operator_token = parser_eat_token(parser);
    expr.left = malloc(sizeof(Expr));
    expr.right = malloc(sizeof(Expr));

    Expr right = parser_parse_logical_expr(parser);
    memcpy(expr.left, &left, sizeof(Expr));
    memcpy(expr.right, &right, sizeof(Expr));

    Span span = {.line = left.span.line,
                 .start = left.span.start,
                 .end = right.span.end};

    left.kind = EXPR_KIND_LOGICAL;
    left.as.logical = expr;
    left.span = span;
  }

  return left;
}

Expr parser_parse_expr(Parser *parser) {
  return parser_parse_logical_expr(parser);
}

ExprStmt parser_parse_expr_stmt(Parser *parser) {
  Expr expr = parser_parse_expr(parser);
  return (ExprStmt){.expr = expr};
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
  Token *type_identifier_token = NULL;
  if (parser_match_token(parser, TOKEN_KIND_COLON)) {
    parser_eat_token(parser);
    if (!parser_match_token(parser, TOKEN_KIND_IDENTIFIER)) {
      ErrorContext ctx = {.source = parser->source,
                          .span = parser->current_token.span};

      error_throw_fmt(&ctx, "expected type name, but received '" SV_FMT "'",
                      SV_ARG(parser->current_token.lexeme));
    }

    type_identifier_token = malloc(sizeof(Token));
    Token type = parser_eat_token(parser);
    memcpy(type_identifier_token, &type, sizeof(Token));
  }

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
                            .type_identifier_token = type_identifier_token,
                            .value = value};
}

BlockStmt parser_parse_block_stmt(Parser *parser, TokenKind end_kind) {
  ArrayList *stmts = array_list_new(32, sizeof(Stmt));

  while (parser_has_more_tokens(parser) &&
         !parser_block_reached_end(parser, end_kind)) {
    Stmt stmt = parser_parse_stmt(parser);
    array_list_push(stmts, &stmt);
  }

  if (!parser_match_token(parser, end_kind)) {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};

    error_throw(&ctx, "expected 'end' keyword to close block.");
  }

  return (BlockStmt){.stmts = stmts};
}

FunctionDeclStmt parser_parse_function_decl_stmt(Parser *parser) {
  Token keyword_token = parser_eat_token(parser);
  if (!parser_match_token(parser, TOKEN_KIND_IDENTIFIER)) {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};

    error_throw_fmt(&ctx,
                    "expected an function name, but received '" SV_FMT "'",
                    SV_ARG(parser->current_token.lexeme));
  }

  Token identifier_token = parser_eat_token(parser);
  if (!parser_match_token(parser, TOKEN_KIND_OPEN_PAREN)) {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};

    error_throw_fmt(&ctx, "expected '(', but received '" SV_FMT "'",
                    SV_ARG(parser->current_token.lexeme));
  }
  parser_eat_token(parser);

  ArrayList *params = array_list_new(32, sizeof(FunctionDeclParam));
  while (parser_has_more_tokens(parser) &&
         !parser_match_token(parser, TOKEN_KIND_CLOSE_PAREN)) {
    // parsing param
    if (!parser_match_token(parser, TOKEN_KIND_IDENTIFIER)) {
      ErrorContext ctx = {.source = parser->source,
                          .span = parser->current_token.span};

      error_throw_fmt(&ctx, "expected an param name, but received '" SV_FMT "'",
                      SV_ARG(parser->current_token.lexeme));
    }

    Token identifier_token = parser_eat_token(parser);
    if (!parser_match_token(parser, TOKEN_KIND_COLON)) {
      ErrorContext ctx = {.source = parser->source,
                          .span = parser->current_token.span};

      error_throw_fmt(&ctx, "expected ':type', but received '" SV_FMT "'",
                      SV_ARG(parser->current_token.lexeme));
    }

    parser_eat_token(parser);
    if (!parser_match_token(parser, TOKEN_KIND_IDENTIFIER)) {
      ErrorContext ctx = {.source = parser->source,
                          .span = parser->current_token.span};

      error_throw_fmt(&ctx,
                      "expected param type name, but received '" SV_FMT "'",
                      SV_ARG(parser->current_token.lexeme));
    }

    Token type_identifier_token = parser_eat_token(parser);
    FunctionDeclParam param = {0};
    param.identifier_token = identifier_token;
    param.type_identifier_token = type_identifier_token;
    array_list_push(params, &param);

    if (parser_match_token(parser, TOKEN_KIND_COMMA)) {
      parser_eat_token(parser);
    } else {
      break;
    }
  }

  if (!parser_match_token(parser, TOKEN_KIND_CLOSE_PAREN)) {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};

    error_throw_fmt(&ctx, "expected ')', but received '" SV_FMT "'",
                    SV_ARG(parser->current_token.lexeme));
  }
  parser_eat_token(parser);

  if (!parser_match_token(parser, TOKEN_KIND_COLON)) {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};

    error_throw_fmt(&ctx, "expected ':type', but received '" SV_FMT "'",
                    SV_ARG(parser->current_token.lexeme));
  }
  parser_eat_token(parser);

  if (!parser_match_token(parser, TOKEN_KIND_IDENTIFIER)) {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};

    error_throw_fmt(&ctx,
                    "expected return type name, but received '" SV_FMT "'",
                    SV_ARG(parser->current_token.lexeme));
  }

  Token return_type_identifier_token = parser_eat_token(parser);

  Stmt body = {0};
  if (parser_match_token(parser, TOKEN_KIND_ARROW)) {
    parser_eat_token(parser);
    body = parser_parse_stmt(parser);
  } else {
    BlockStmt block_stmt = parser_parse_block_stmt(parser, TOKEN_KIND_END);
    body.kind = STMT_KIND_BLOCK;
    body.as.block = block_stmt;
    // TODO: add block span when implement multi-line spans.
  }

  if (body.kind == STMT_KIND_BLOCK) {
    if (!parser_match_token(parser, TOKEN_KIND_END)) {
      ErrorContext ctx = {.source = parser->source,
                          .span = parser->current_token.span};
      error_throw(&ctx, "'end' to close function block.");
    }
    parser_eat_token(parser);
  }

  FunctionDeclStmt func_decl = {0};
  func_decl.keyword_token = keyword_token;
  func_decl.identifier_token = identifier_token;
  func_decl.return_type_identifier_token = return_type_identifier_token;
  func_decl.params = params;
  func_decl.body = malloc(sizeof(Stmt));
  memcpy(func_decl.body, &body, sizeof(Stmt));

  return func_decl;
}

EchoStmt parser_parse_echo_stmt(Parser *parser) {
  Token keyword_token = parser_eat_token(parser);
  Expr message = parser_parse_expr(parser);

  return (EchoStmt){
      .message = message,
      .keyword_token = keyword_token,
  };
}

BlockStmt parser_parse_if_block_stmt(Parser *parser) {
  ArrayList *stmts = array_list_new(32, sizeof(Stmt));

  while (parser_has_more_tokens(parser) &&
         !parser_if_block_reached_end(parser)) {
    Stmt stmt = parser_parse_stmt(parser);
    array_list_push(stmts, &stmt);
  }

  if (!parser_match_token(parser, TOKEN_KIND_ELSE) &&
      !parser_match_token(parser, TOKEN_KIND_END)) {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};

    error_throw(&ctx, "expected 'else' or 'end' to close if block.");
  }

  return (BlockStmt){.stmts = stmts};
}

IfStmt parser_parse_if_stmt(Parser *parser) {
  Token keyword_token = parser_eat_token(parser);
  Expr condition = parser_parse_expr(parser);

  Stmt consequent = {0};
  if (parser_match_token(parser, TOKEN_KIND_ARROW)) {
    parser_eat_token(parser);
    consequent = parser_parse_stmt(parser);
  } else if (parser_match_token(parser, TOKEN_KIND_THEN)) {
    parser_eat_token(parser);

    BlockStmt block_stmt = parser_parse_if_block_stmt(parser);
    consequent.kind = STMT_KIND_BLOCK;
    consequent.as.block = block_stmt;
  } else {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};
    error_throw(&ctx, "expected 'then' or '->', because if statements only "
                      "accept a block or expression consequent.");
  }

  Stmt alternate = {0};
  b8 has_alternate = false;
  if (parser_match_token(parser, TOKEN_KIND_ELSE)) {
    parser_eat_token(parser);
    has_alternate = true;

    if (parser_match_token(parser, TOKEN_KIND_IF)) {
      IfStmt if_stmt = parser_parse_if_stmt(parser);
      alternate.kind = STMT_KIND_IF;
      alternate.as.if_stmt = if_stmt;
      alternate.span = (Span){.line = if_stmt.keyword_token.span.line,
                              .start = if_stmt.keyword_token.span.start,
                              .end = if_stmt.alternate == NULL
                                         ? if_stmt.consequent->span.end
                                         : if_stmt.alternate->span.end};

    } else if (parser_match_token(parser, TOKEN_KIND_ARROW)) {
      parser_eat_token(parser);
      alternate = parser_parse_stmt(parser);
    } else {
      BlockStmt block_stmt = parser_parse_block_stmt(parser, TOKEN_KIND_END);
      alternate.kind = STMT_KIND_BLOCK;
      alternate.as.block = block_stmt;
    }
  }

  if (consequent.kind == STMT_KIND_BLOCK &&
      (!has_alternate || alternate.kind != STMT_KIND_IF)) {
    if (!parser_match_token(parser, TOKEN_KIND_END)) {
      ErrorContext ctx = {.source = parser->source,
                          .span = parser->current_token.span};

      error_throw(&ctx, "expected 'end' keyword to close 'if' block.");
    }
    parser_eat_token(parser);
  }

  IfStmt stmt = {0};
  stmt.keyword_token = keyword_token;
  stmt.condition = malloc(sizeof(Expr));
  stmt.consequent = malloc(sizeof(Stmt));
  stmt.alternate = NULL;
  memcpy(stmt.condition, &condition, sizeof(Expr));
  memcpy(stmt.consequent, &consequent, sizeof(Stmt));

  if (has_alternate) {
    stmt.alternate = malloc(sizeof(Stmt));
    memcpy(stmt.alternate, &alternate, sizeof(Stmt));
  }

  return stmt;
}

WhileStmt parser_parse_while_stmt(Parser *parser) {
  Token keyword_token = parser_eat_token(parser);
  Expr condition = parser_parse_expr(parser);

  Stmt body = {0};
  if (parser_match_token(parser, TOKEN_KIND_DO)) {
    parser_eat_token(parser);
    body.kind = STMT_KIND_BLOCK;
    body.as.block = parser_parse_block_stmt(parser, TOKEN_KIND_END);
    // TODO: add block span when implement multi-line spans.
  } else if (parser_match_token(parser, TOKEN_KIND_ARROW)) {
    parser_eat_token(parser);
    body = parser_parse_stmt(parser);
  } else {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};
    error_throw(&ctx, "expected 'do' or '->', because while statements only "
                      "accept a block or expression body.");
  }

  if (body.kind == STMT_KIND_BLOCK) {
    if (!parser_match_token(parser, TOKEN_KIND_END)) {
      ErrorContext ctx = {.source = parser->source,
                          .span = parser->current_token.span};

      error_throw(&ctx, "expected 'end' keyword to close 'while' block.");
    }
    parser_eat_token(parser);
  }

  WhileStmt while_stmt = {0};
  while_stmt.keyword_token = keyword_token;
  while_stmt.body = malloc(sizeof(Stmt));
  while_stmt.condition = malloc(sizeof(Expr));
  memcpy(while_stmt.body, &body, sizeof(Stmt));
  memcpy(while_stmt.condition, &condition, sizeof(Expr));

  return while_stmt;
}

ForStmt parser_parse_for_stmt(Parser *parser) {
  Token keyword_token = parser_eat_token(parser);
  Stmt init = parser_parse_stmt(parser);
  if (init.kind != STMT_KIND_VARIABLE_DECL) {
    ErrorContext ctx = {.source = parser->source, .span = init.span};
    error_throw(&ctx, "unexpected init statement, 'for' expects an 'variable "
                      "declaration' statement.");
  }

  if (!parser_match_token(parser, TOKEN_KIND_COMMA)) {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};
    error_throw_fmt(&ctx, "expects ',', but received '" SV_FMT "'.",
                    SV_ARG(parser->current_token.lexeme));
  }

  parser_eat_token(parser);
  Expr test = parser_parse_expr(parser);

  if (!parser_match_token(parser, TOKEN_KIND_COMMA)) {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};
    error_throw_fmt(&ctx, "expects ',', but received '" SV_FMT "'.",
                    SV_ARG(parser->current_token.lexeme));
  }

  parser_eat_token(parser);
  Expr update = parser_parse_expr(parser);

  Stmt body = {0};
  if (parser_match_token(parser, TOKEN_KIND_DO)) {
    parser_eat_token(parser);
    body.kind = STMT_KIND_BLOCK;
    body.as.block = parser_parse_block_stmt(parser, TOKEN_KIND_END);
    // TODO: add block span when implement multi-line spans.
  } else if (parser_match_token(parser, TOKEN_KIND_ARROW)) {
    parser_eat_token(parser);
    body = parser_parse_stmt(parser);
  } else {
    ErrorContext ctx = {.source = parser->source,
                        .span = parser->current_token.span};
    error_throw(&ctx, "expected 'do' or '->', because while statements only "
                      "accept a block or expression body.");
  }

  if (body.kind == STMT_KIND_BLOCK) {
    if (!parser_match_token(parser, TOKEN_KIND_END)) {
      ErrorContext ctx = {.source = parser->source,
                          .span = parser->current_token.span};

      error_throw(&ctx, "expected 'end' keyword to close 'for' block.");
    }
    parser_eat_token(parser);
  }

  ForStmt for_stmt = {0};
  for_stmt.keyword_token = keyword_token;
  for_stmt.init = malloc(sizeof(Stmt));
  for_stmt.test = malloc(sizeof(Expr));
  for_stmt.update = malloc(sizeof(Expr));
  for_stmt.body = malloc(sizeof(Stmt));
  memcpy(for_stmt.init, &init, sizeof(Stmt));
  memcpy(for_stmt.test, &test, sizeof(Expr));
  memcpy(for_stmt.update, &update, sizeof(Expr));
  memcpy(for_stmt.body, &body, sizeof(Stmt));

  return for_stmt;
}

ReturnStmt parser_parse_return_stmt(Parser *parser) {
  Token keyword_token = parser_eat_token(parser);
  Expr *expr = NULL;
  if (parser_can_start_expr(parser)) {
    expr = malloc(sizeof(Expr));
    Expr return_expr = parser_parse_expr(parser);
    memcpy(expr, &return_expr, sizeof(Expr));
  }    
  
  return (ReturnStmt) {
    .keyword_token = keyword_token,
    .expr = expr
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
  case TOKEN_KIND_FN: {
    FunctionDeclStmt function_decl = parser_parse_function_decl_stmt(parser);
    Span span = {.line = function_decl.keyword_token.span.line,
                 .start = function_decl.keyword_token.span.start,
                 .end = function_decl.body->span.end};
    stmt.kind = STMT_KIND_FUNCTION_DECL;
    stmt.as.function_decl = function_decl;
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
  case TOKEN_KIND_IF: {
    IfStmt if_stmt = parser_parse_if_stmt(parser);
    Span span = {.line = if_stmt.keyword_token.span.line,
                 .start = if_stmt.keyword_token.span.start,
                 .end = if_stmt.alternate == NULL
                            ? if_stmt.consequent->span.end
                            : if_stmt.alternate->span.end};

    stmt.kind = STMT_KIND_IF;
    stmt.as.if_stmt = if_stmt;
    stmt.span = span;
    break;
  }
  case TOKEN_KIND_WHILE: {
    WhileStmt while_stmt = parser_parse_while_stmt(parser);
    Span span = {.line = while_stmt.keyword_token.span.line,
                 .start = while_stmt.keyword_token.span.start,
                 .end = while_stmt.body->span.end};
    stmt.kind = STMT_KIND_WHILE;
    stmt.as.while_stmt = while_stmt;
    stmt.span = span;
    break;
  }
  case TOKEN_KIND_FOR: {
    ForStmt for_stmt = parser_parse_for_stmt(parser);
    Span span = {.line = for_stmt.keyword_token.span.line,
                 .start = for_stmt.keyword_token.span.start,
                 .end = for_stmt.update->span.end};
    stmt.kind = STMT_KIND_FOR;
    stmt.as.for_stmt = for_stmt;
    stmt.span = span;
    break;
  }
  case TOKEN_KIND_RETURN: {
    ReturnStmt return_stmt = parser_parse_return_stmt(parser);
    Span span = {.line = return_stmt.keyword_token.span.line,
                 .start = return_stmt.keyword_token.span.start,
                 .end = return_stmt.expr == NULL ? return_stmt.keyword_token.span.end : return_stmt.expr->span.end};
    stmt.kind = STMT_KIND_RETURN;
    stmt.as.return_stmt = return_stmt;
    stmt.span = span;
    break;
  }    
  default: {
    ExprStmt expr = parser_parse_expr_stmt(parser);
    stmt.kind = STMT_KIND_EXPR;
    stmt.as.expr = expr;
    stmt.span = expr.expr.span;
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
