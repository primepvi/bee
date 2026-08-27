#ifndef BEE_LEXER_H
#define BEE_LEXER_H

#include "libs/source.h"
#include "libs/types.h"

#include "ast.h"

typedef struct {
  u32 cursor;
  u32 row, col;
  Source *source;
} Lexer;

Lexer lexer_create(Source *source);
Token lexer_next_token(Lexer *l);
b8 lexer_has_more_tokens(Lexer *l);

#endif // BEE_LEXER_H
