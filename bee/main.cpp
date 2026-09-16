#include "bee/lexer/Token.hpp"
#include <iostream>

using bee::lexer::Token;
using bee::lexer::TokenKind;

int main(void) {
  Token token(TokenKind::StringLit, "Hello, World!");
  std::cout << token.toString() << std::endl;
  return 0;
}
