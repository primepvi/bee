#include "ast.h"
#include "libs/string_view.h"
#include "parser.h"
#include <stdio.h>

int main(void) {
  StringView source = SV_LIT("let a = 10\n"
                             "let b = 20\n"
                             "let result = a + b * 10 / 5\n");

  printf(SV_FMT "\n\n", SV_ARG(source));

  Parser parser = parser_create(source);
  Program program = parser_parse(&parser);
  ast_print(&program);

  return 0;
}
