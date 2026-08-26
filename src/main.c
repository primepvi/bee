#include <stdio.h>

#include "lexer.h"
#include "libs/string_view.h"

int main(void) {
  StringView code = string_view_from_cstr("let age = 18;");
  Lexer lexer = lexer_create(code);

  while (lexer_has_more_tokens(&lexer)) {
    Token current = lexer_next_token(&lexer);
    printf("TOKEN: "SV_FMT"\n", SV_ARG(current.lexeme));
  }
  
  return 0;  
}  
