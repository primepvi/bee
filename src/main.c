#include "ast.h"
#include "interpreter.h"
#include "libs/array_list.h"
#include "libs/source.h"
#include "libs/string_view.h"
#include "parser.h"
#include <stdio.h>

int main(int argc, char **argv) {
  if (argc < 2) {
    printf("USAGE: bee <filepath>\n");
    return 1;
  }
  
  Source source = source_from_file(argv[1]);
  Parser parser = parser_create(&source);
  Program program = parser_parse(&parser);
  ast_print(&program);

  Interpreter interpreter = interpreter_create(&source, &program);
  interpreter_eval(&interpreter);

  return 0;
}
