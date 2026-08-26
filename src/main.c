#include "ast.h"
#include "libs/string_view.h"
#include "parser.h"
#include <stdio.h>

int main(void) {
  StringView source = SV_LIT("const name = \"John\"\n"
                             "name = \"Carlos\"");
  printf(SV_FMT"\n\n", SV_ARG(source));

  Parser parser = parser_create(source);
  Program program = parser_parse(&parser);
  ast_print(&program);

  return 0;
}
