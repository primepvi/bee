#include "lexer.h"
#include "libs/string_view.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

Lexer lexer_create(StringView source) {
  return (Lexer){0, source};
}

Token lexer_next_token(Lexer *l) {
  StringView code = string_view_slice_start(l->code, l->cursor);
  u32 whitespaces = string_view_trim_left(&code);
  
  l->cursor += whitespaces;
  if (!lexer_has_more_tokens(l)) {
    return (Token){TOKEN_KIND_EOF, string_view_from_cstr("\0")};
  }

  StringView number = string_view_slice_while(code, (b8(*)(char))isdigit);
  if (!string_view_is_empty(number)) {
    l->cursor += number.length;
    return (Token){TOKEN_KIND_NUMBER, number};
  }

  StringView name = string_view_slice_while(code, (b8(*)(char))isalnum);
  if (!string_view_is_empty(name)) {
    TokenKind kind = TOKEN_KIND_IDENTIFIER;
    
    if (string_view_is_equal(name, string_view_from_cstr("let"))) {
      kind = TOKEN_KIND_LET;
    } else if (string_view_is_equal(name, string_view_from_cstr("const"))) {
      kind = TOKEN_KIND_CONST;
    }

    l->cursor += name.length;
    return (Token){kind, name};
  }

  TokenKind kind;
  switch (code.data[0]) {
  case '=':
    kind = TOKEN_KIND_EQUAL;
    break;
  case ';':
    kind = TOKEN_KIND_SEMICOLON;
    break;
  default:
    fprintf(stderr, "ERROR: Invalid char found during lexing: %c\n",
            code.data[0]);
    exit(1);
  }

  l->cursor += 1;
  return (Token){kind, string_view_slice(code, 0, 1)};
}

b8 lexer_has_more_tokens(Lexer *l) {
  return l->cursor < l->code.length;
}
