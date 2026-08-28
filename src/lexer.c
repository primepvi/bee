#include "lexer.h"
#include "ast.h"
#include "libs/error.h"
#include "libs/string_view.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static b32 lexer_str_predicate(int c) { return c != '"'; }

Lexer lexer_create(Source *source) {
  return (Lexer){
      .source = source,
      .row = 1,
      .col = 1,
      .cursor = 0,
  };
}

Span lexer_make_span(Lexer *l, u32 length) {
  return (Span){.start = l->col, .end = l->col + length, .line = l->row};
}

Token lexer_next_token(Lexer *l) {
  StringView source = SV_LIT(l->source->buffer);

  while (lexer_has_more_tokens(l) &&
         string_view_at(source, l->cursor) == '\n') {
    l->row += 1;
    l->col = 1;
    l->cursor += 1;
  }

  if (!lexer_has_more_tokens(l)) {
    return (Token){TOKEN_KIND_EOF, SV_LIT("\0"), lexer_make_span(l, 0)};
  }
  
  StringView code = string_view_slice_start(source, l->cursor);
  StringView whitespaces =
      string_view_slice_while(code, (StringViewPredicate)isspace);
  code = string_view_slice_start(code, whitespaces.length);

  l->cursor += whitespaces.length;
  l->col += whitespaces.length;
  if (!lexer_has_more_tokens(l)) {
    return (Token){TOKEN_KIND_EOF, SV_LIT("\0"), lexer_make_span(l, 0)};
  }

  StringView number =
      string_view_slice_while(code, (StringViewPredicate)isdigit);
  if (!string_view_is_empty(number)) {
    Span span = lexer_make_span(l, number.length);
    l->cursor += number.length;
    l->col += number.length;
    return (Token){TOKEN_KIND_NUMBER, number, span};
  }

  StringView name = string_view_slice_while(code, (StringViewPredicate)isalnum);
  if (!string_view_is_empty(name)) {
    TokenKind kind = TOKEN_KIND_IDENTIFIER;
    Span span = lexer_make_span(l, name.length);

    if (string_view_is_equal(name, SV_LIT("let"))) {
      kind = TOKEN_KIND_LET;
    } else if (string_view_is_equal(name, SV_LIT("const"))) {
      kind = TOKEN_KIND_CONST;
    } else if (string_view_is_equal(name, SV_LIT("echo"))) {
      kind = TOKEN_KIND_ECHO;
    }

    l->cursor += name.length;
    l->col += name.length;
    return (Token){kind, name, span};
  }

  if (string_view_starts_with(code, SV_LIT("\""))) {
    StringView lexeme = string_view_slice_while(
        string_view_slice_start(code, 1), lexer_str_predicate);
    Span span = lexer_make_span(l, lexeme.length + 2);

    if (string_view_at(code, lexeme.length + 1) != '"') {
      ErrorContext ctx = {.source = l->source, .span = span};
      error_throw(&ctx, "unterminated string has found during lexing.");
    }

    l->cursor += lexeme.length + 2;
    l->col += lexeme.length + 2;
    return (Token){TOKEN_KIND_STRING, lexeme, span};
  }

  TokenKind kind;
  Span span = lexer_make_span(l, 1);

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
  case '(':
    kind = TOKEN_KIND_OPEN_PAREN;
    break;
  case ')':
    kind = TOKEN_KIND_CLOSE_PAREN;
    break;
  default: {
    ErrorContext ctx = {.source = l->source, .span = span};
    error_throw_fmt(&ctx, "unexpected char has found: %c", string_view_at(code, 0));
  }
  }

  l->cursor += 1;
  l->col += 1;
  return (Token){kind, string_view_slice(code, 0, 1), span};
}

b8 lexer_has_more_tokens(Lexer *l) {
  return l->cursor < strlen(l->source->buffer);
}
