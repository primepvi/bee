#ifndef BEE_TOKEN_HPP
#define BEE_TOKEN_HPP

#include <string_view>
#include <cstddef>

namespace bee::lexer {

enum class TokenKind {
  // Keywords
  LetKw,
  ConstKw,
  EchoKw,
  TrueKw,
  FalseKw,
  NullKw,
  AndKw,
  OrKw,
  NotKw,
  ThenKw,
  EndKw,
  IfKw,
  ElseKw,
  WhileKw,
  ForKw,
  DoKw,
  WhenKw,
  OtherwiseKw,
  FnKw,
  ReturnKw,
  Identifier,

  // Literals
  NumberLit,
  StringLit,

  // Symbols,
  ColonSym,
  SemiColonSym,
  QuestionSym,
  EqualSym,
  OpenParenSym,
  CloseParenSym,
  GtSym,
  GteSym,
  LtSym,
  LteSym,
  EqEqSym,
  NeqSym,
  ArrowSym,
  CommaSym,
  PlusSym,
  MinusSym,
  StarSym,
  SlashSym,
  PercentageSym,

  // Specials
  Invalid,
  EndOfFile,
};

class Token {
public:
  Token(TokenKind kind, std::string_view lexeme);
private:
  TokenKind m_Kind;
  std::string_view m_Lexeme;  
};

struct TokenEntry {
  TokenKind kind;
  std::string_view lexeme;
};  

TokenKind getKeywordTokenKind(std::string_view keyword);
TokenKind getSymbolTokenKind(std::string_view symbol);
std::string_view getTokenKindName(TokenKind kind);

std::size_t getUnaryOperatorPriority(TokenKind op);
std::size_t getBinaryOperatorPriority(TokenKind op);

} // namespace bee::lexer

#endif // BEE_TOKEN_HPP
