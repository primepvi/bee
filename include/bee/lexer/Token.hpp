#ifndef BEE_TOKEN_HPP
#define BEE_TOKEN_HPP

#include "bee/Source.hpp"

#include <cstddef>
#include <string_view>

namespace bee::lexer {

enum class TokenKind {
  // Keywords
  LetKw,
  ConstKw,
  LitKw,
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
  FloatLit,
  IntegerLit,
  StringLit,
  CharLit,

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
  DoubleDotSym,
  PipeSym,

  // Specials
  Invalid,
  EndOfFile,
};

class Token {
public:
  Token(TokenKind kind, std::string_view lexeme, bee::SourceSpan m_span);
  std::string toString() const;

  inline TokenKind kind() const { return m_kind; }
  inline std::string_view lexeme() const { return m_lexeme; }
  inline bee::SourceSpan span() const { return m_span; }

private:
  TokenKind m_kind;
  std::string_view m_lexeme;
  bee::SourceSpan m_span;
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
