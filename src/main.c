#include "ast.h"
#include "interpreter.h"
#include "libs/array_list.h"
#include "libs/source.h"
#include "libs/string_view.h"
#include "parser.h"
#include "typechecker.h"
#include <stdio.h>
#include <string.h>

int main(int argc, char **argv) {
  if (argc < 2) {
    printf("USAGE: bee <filepath>\n");
    return 1;
  }
  
  Source source = source_from_file(argv[1]);
  Parser parser = parser_create(&source);
  Program program = parser_parse(&parser);
  if (argc > 2 && strcmp(argv[2], "--ast") == 0) {
    ast_print(&program);
  }    

  TypeChecker tc = tc_create(&program, &source);
  tc_check(&tc);

  Interpreter interpreter = interpreter_create(&program, &source, tc.symbols);
  interpreter_eval(&interpreter);
  
  return 0;
}
