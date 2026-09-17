#include <algorithm>
#include <array>
#include <bee/lexer/Token.hpp>
#include <cstddef>
#include <format>
#include <string>
#include <string_view>

namespace bee::lexer {

Token::Token(TokenKind kind, std::string_view lexeme, SourceSpan span)
    : m_kind(kind), m_lexeme(lexeme), m_span(span) {}

std::string Token::toString() const {
  return std::format("Token (kind={}, lexeme=\"{}\")", getTokenKindName(m_kind), std::string(m_lexeme));
}

constexpr std::array keywordsEntries = std::to_array<TokenEntry>({
    {TokenKind::LetKw, "let"},     {TokenKind::ConstKw, "const"},
    {TokenKind::EchoKw, "echo"},   {TokenKind::TrueKw, "true"},
    {TokenKind::FalseKw, "false"}, {TokenKind::NullKw, "null"},
    {TokenKind::AndKw, "and"},     {TokenKind::OrKw, "or"},
    {TokenKind::NotKw, "not"},     {TokenKind::ThenKw, "then"},
    {TokenKind::EndKw, "end"},     {TokenKind::IfKw, "if"},
    {TokenKind::ElseKw, "else"},   {TokenKind::WhileKw, "while"},
    {TokenKind::ForKw, "for"},     {TokenKind::DoKw, "do"},
    {TokenKind::WhenKw, "when"},   {TokenKind::OtherwiseKw, "otherwise"},
    {TokenKind::FnKw, "fn"},       {TokenKind::ReturnKw, "return"},
});

constexpr std::array symbolsEntries = std::to_array<TokenEntry>({
    {TokenKind::ColonSym, ":"},      {TokenKind::SemiColonSym, ";"},
    {TokenKind::QuestionSym, "?"},   {TokenKind::EqualSym, "="},
    {TokenKind::OpenParenSym, "("},  {TokenKind::CloseParenSym, ")"},
    {TokenKind::GtSym, ">"},         {TokenKind::GteSym, ">="},
    {TokenKind::LtSym, "<"},         {TokenKind::LteSym, "<="},
    {TokenKind::EqEqSym, "=="},      {TokenKind::NeqSym, "!="},
    {TokenKind::ArrowSym, "->"},     {TokenKind::CommaSym, ","},
    {TokenKind::PlusSym, "+"},       {TokenKind::MinusSym, "-"},
    {TokenKind::StarSym, "*"},       {TokenKind::SlashSym, "/"},
    {TokenKind::PercentageSym, "%"},
});

constexpr std::array tokenKindNameEntries = std::to_array<TokenEntry>({
    // Keywords
    {TokenKind::LetKw, "Let_Kw"},
    {TokenKind::ConstKw, "Const_Kw"},
    {TokenKind::EchoKw, "Echo_KW"},
    {TokenKind::TrueKw, "True_KW"},
    {TokenKind::FalseKw, "False_KW"},
    {TokenKind::NullKw, "Null_KW"},
    {TokenKind::AndKw, "And_KW"},
    {TokenKind::OrKw, "Or_KW"},
    {TokenKind::NotKw, "Not_KW"},
    {TokenKind::ThenKw, "Then_KW"},
    {TokenKind::EndKw, "End_KW"},
    {TokenKind::IfKw, "If_KW"},
    {TokenKind::ElseKw, "Else_KW"},
    {TokenKind::WhileKw, "While_KW"},
    {TokenKind::ForKw, "For_KW"},
    {TokenKind::DoKw, "Do_KW"},
    {TokenKind::WhenKw, "When_KW"},
    {TokenKind::OtherwiseKw, "Otherwise_KW"},
    {TokenKind::FnKw, "Fn_KW"},
    {TokenKind::ReturnKw, "Return_KW"},
    {TokenKind::Identifier, "Identifier"},

    // Literals
    {TokenKind::NumberLit, "Number_LIT"},
    {TokenKind::StringLit, "String_LIT"},

    // Symbols,
    {TokenKind::ColonSym, "Colon_SYM"},
    {TokenKind::SemiColonSym, "SemiColon_SYM"},
    {TokenKind::QuestionSym, "Question_SYM"},
    {TokenKind::EqualSym, "Equal_SYM"},
    {TokenKind::OpenParenSym, "OpenParen_SYM"},
    {TokenKind::CloseParenSym, "CloseParen_SYM"},
    {TokenKind::GtSym, "Gt_SYM"},
    {TokenKind::GteSym, "Gte_SYM"},
    {TokenKind::LtSym, "Lt_SYM"},
    {TokenKind::LteSym, "Lte_SYM"},
    {TokenKind::EqEqSym, "EqEq_SYM"},
    {TokenKind::NeqSym, "Neq_SYM"},
    {TokenKind::ArrowSym, "Arrow_SYM"},
    {TokenKind::CommaSym, "Comma_SYM"},
    {TokenKind::PlusSym, "Plus_SYM"},
    {TokenKind::MinusSym, "Minus_SYM"},
    {TokenKind::StarSym, "Star_SYM"},
    {TokenKind::SlashSym, "Slash_SYM"},
    {TokenKind::PercentageSym, "Percentage_SYM"},

    // Specials
    {TokenKind::Invalid, "Invalid_SPE"},
    {TokenKind::EndOfFile, "EndOfFile_SPE"},
});

TokenKind getKeywordTokenKind(std::string_view keyword) {
  auto it = std::find_if(
      symbolsEntries.begin(), symbolsEntries.end(),
      [keyword](const TokenEntry &entry) { return entry.lexeme == keyword; });

  return it == symbolsEntries.end() ? TokenKind::Identifier : it->kind;
}

TokenKind getSymbolTokenKind(std::string_view symbol) {
  auto it = std::find_if(
      symbolsEntries.begin(), symbolsEntries.end(),
      [symbol](const TokenEntry &entry) { return entry.lexeme == symbol; });

  return it == symbolsEntries.end() ? TokenKind::Invalid : it->kind;
}

std::string_view getTokenKindName(TokenKind kind) {
  auto it = std::find_if(
      tokenKindNameEntries.begin(), tokenKindNameEntries.end(),
      [kind](const TokenEntry &entry) { return entry.kind == kind; });

  return it == tokenKindNameEntries.end() ? "Invalid_SPE" : it->lexeme;
}

std::size_t getUnaryOperatorPriority(TokenKind op) {
  switch (op) {
  case TokenKind::PlusSym:
  case TokenKind::MinusSym:
  case TokenKind::NotKw:
    return 6;
  default:
    return 0;
  }
}

std::size_t getBinaryOperatorPriority(TokenKind op) {
  switch (op) {
  case TokenKind::StarSym:
  case TokenKind::SlashSym:
  case TokenKind::PercentageSym:
    return 5;
  case TokenKind::PlusSym:
  case TokenKind::MinusSym:
    return 4;
  case TokenKind::GtSym:
  case TokenKind::GteSym:
  case TokenKind::LtSym:
  case TokenKind::LteSym:
  case TokenKind::EqEqSym:
  case TokenKind::NeqSym:
    return 3;
  case TokenKind::OrKw:
    return 2;
  case TokenKind::AndKw:
    return 1;
  default:
    return 0;
  }
}

} // namespace bee::lexer
