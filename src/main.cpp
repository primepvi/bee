#include <iostream>

#include "bee/Diagnostics.hpp"
#include "bee/Source.hpp"
#include "bee/lexer/Lexer.hpp"
#include "bee/parser/Ast.hpp"
#include "bee/parser/AstDumper.hpp"
#include "bee/parser/Parser.hpp"

using bee::DiagnosticBag;
using bee::Source;
using bee::lexer::Lexer;
using bee::lexer::Token;
using bee::parser::AstDumper;
using bee::parser::Parser;
using bee::parser::Program;

int main(void) {
  const Source source = Source::fromFile("examples/hello.bee");
  DiagnosticBag bag(source);
  Lexer lexer(source, bag);
  std::vector<Token> tokens;

  while (lexer.hasMoreTokens()) {
    Token token = lexer.nextToken();
    tokens.push_back(token);

    std::cout << token.toString() << std::endl;
  }

  Parser parser(source, bag, tokens);
  Program program = parser.parse();

  AstDumper dumper(program, std::cout);
  dumper.dump();

  bag.write(std::cerr);
}
