#include "ast.h"
#include "libs/string_view.h"
#include "parser.h"
#include <stdio.h>

int main(void) {
  StringView source = SV_LIT("const name = \"John Doe\";\n"
                             "const age = 18;");

  Parser parser = parser_create(source);
  Program program = parser_parse(&parser);
  ast_print(&program);

  return 0;
}
