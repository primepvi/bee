#ifndef BEE_LEXER_H
#define BEE_LEXER_H

#include "libs/string_view.h"
#include "libs/types.h"
#include "ast.h"

typedef struct {
  u32 cursor;
  StringView source;
} Lexer;

Lexer lexer_create(StringView source);
Token lexer_next_token(Lexer *l);
b8 lexer_has_more_tokens(Lexer *l);

#endif // BEE_LEXER_H
