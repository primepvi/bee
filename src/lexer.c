#include "lexer.h"
#include "ast.h"
#include "libs/string_view.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

static b32 lexer_str_predicate(int c) { return c != '"'; }

Lexer lexer_create(StringView source) { return (Lexer){0, source}; }

Token lexer_next_token(Lexer *l) {
  StringView code = string_view_slice_start(l->source, l->cursor);
  StringView whitespaces =
      string_view_slice_while(code, (StringViewPredicate)isspace);
  code = string_view_slice_start(code, whitespaces.length);

  l->cursor += whitespaces.length;
  if (!lexer_has_more_tokens(l)) {
    return (Token){TOKEN_KIND_EOF, SV_LIT("\0")};
  }

  StringView number =
      string_view_slice_while(code, (StringViewPredicate)isdigit);
  if (!string_view_is_empty(number)) {
    l->cursor += number.length;
    return (Token){TOKEN_KIND_NUMBER, number};
  }

  StringView name = string_view_slice_while(code, (StringViewPredicate)isalnum);
  if (!string_view_is_empty(name)) {
    TokenKind kind = TOKEN_KIND_IDENTIFIER;

    if (string_view_is_equal(name, SV_LIT("let"))) {
      kind = TOKEN_KIND_LET;
    } else if (string_view_is_equal(name, SV_LIT("const"))) {
      kind = TOKEN_KIND_CONST;
    }

    l->cursor += name.length;
    return (Token){kind, name};
  }

  if (string_view_starts_with(code, SV_LIT("\""))) {
    StringView lexeme = string_view_slice_while(
        string_view_slice_start(code, 1), lexer_str_predicate);

    if (string_view_at(code, lexeme.length + 1) != '"') {
      fprintf(stderr, "ERROR: unterminated string has found in %u.", l->cursor);
      exit(1);
    }

    l->cursor += lexeme.length + 2;
    return (Token){TOKEN_KIND_STRING, lexeme};
  }

  TokenKind kind;
  switch (code.data[0]) {
  case '=':
    kind = TOKEN_KIND_EQUAL;
    break;
  case ';':
    kind = TOKEN_KIND_SEMICOLON;
    break;
  case '+':
    kind = TOKEN_KIND_PLUS;
    break;    
  case '-':
    kind = TOKEN_KIND_MINUS;
    break;
  case '*':
    kind = TOKEN_KIND_STAR;
    break;
  case '/':
    kind = TOKEN_KIND_SLASH;
    break;
  case '%':
    kind = TOKEN_KIND_PERCENTAGE;
    break;    
  default:
    fprintf(stderr, "ERROR: Invalid char found during lexing: %c\n",
            code.data[0]);
    exit(1);
  }

  l->cursor += 1;
  return (Token){kind, string_view_slice(code, 0, 1)};
}

b8 lexer_has_more_tokens(Lexer *l) { return l->cursor < l->source.length; }
