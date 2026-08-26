#ifndef BEE_LEXER_H
#define BEE_LEXER_H

#include "libs/string_view.h"
#include "libs/types.h"

typedef enum {
  TOKEN_KIND_LET,
  TOKEN_KIND_CONST,
  TOKEN_KIND_IDENTIFIER,
  TOKEN_KIND_NUMBER,

  TOKEN_KIND_SEMICOLON,
  TOKEN_KIND_EQUAL,
  
  TOKEN_KIND_EOF
} TokenKind;  

typedef struct {
  TokenKind kind;
  StringView lexeme;
} Token;

typedef struct {
  u32 cursor;
  StringView code;
} Lexer;

Lexer lexer_create(StringView source);
Token lexer_next_token(Lexer *l);
b8 lexer_has_more_tokens(Lexer *l);

#endif // BEE_LEXER_H
