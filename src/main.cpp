#include <iostream>

#include "bee/Diagnostics.hpp"
#include "bee/Source.hpp"
#include "bee/lexer/Lexer.hpp"
#include "bee/parser/Ast.hpp"
#include "bee/parser/AstDumper.hpp"
#include "bee/parser/Parser.hpp"
#include "bee/typechecker/TypeChecker.hpp"

using bee::DiagnosticBag;
using bee::Source;
using bee::lexer::Lexer;
using bee::lexer::Token;
using bee::parser::AstDumper;
using bee::parser::Parser;
using bee::parser::Program;
using bee::typechecker::TypeChecker;

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

  if (!bag.isEmpty()) {
    bag.write(std::cerr);
    return 1;
  }    

  Parser parser(source, bag, tokens);
  Program program = parser.parse();

  if (!bag.isEmpty()) {
    bag.write(std::cerr);
    return 1;
  }    

  AstDumper dumper(program, std::cout);
  dumper.dump();

  TypeChecker checker(program, bag);
  checker.typecheck();

  if (!bag.isEmpty()) {
    bag.write(std::cerr);
    return 1;
  }    
}
