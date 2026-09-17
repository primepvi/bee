#include <iostream>

#include "bee/Diagnostics.hpp"
#include "bee/Source.hpp"
#include "bee/lexer/Lexer.hpp"

int main(void) {
  const bee::Source source = bee::Source::fromFile("../examples/hello.bee");
  bee::DiagnosticBag bag(source);
  bee::lexer::Lexer lexer(source, bag);

  while (lexer.hasMoreTokens()) {
    const bee::lexer::Token token = lexer.nextToken();
    std::cout << token.toString() << std::endl;
  }

  bag.write(std::cerr);
  
  return 0;
}
