#include <bee/Diagnostics.hpp>
#include <bee/Source.hpp>
#include <bee/lexer/Lexer.hpp>

#include <iostream>

using bee::source::Source;
using bee::diagnostics::DiagnosticBag;
using bee::lexer::Lexer;

int main(void) {
  const Source source = Source::fromFile("../examples/hello.bee");
  DiagnosticBag bag(source);
  Lexer lexer(source, bag);

  while (lexer.hasMoreTokens()) {
    const auto token = lexer.nextToken();
    std::cout << token.toString() << std::endl;
  }

  bag.write(std::cerr);
  
  return 0;
}
