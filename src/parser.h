#ifndef BEE_PARSER_H
#define BEE_PARSER_H

#include "ast.h"
#include "lexer.h"

typedef struct {
  StringView source;
  Lexer lexer;  
  Token current_token;
} Parser;

Parser parser_create(StringView source);
Program parser_parse(Parser *parser);

Token parser_eat_token(Parser *parser);
Expr parser_parse_expr(Parser *parser);
Stmt parser_parse_stmt(Parser *parser);

#endif // BEE_PARSER_H
