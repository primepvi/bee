#include <bee/Diagnostics.hpp>
#include <bee/lexer/Lexer.hpp>
#include <bee/lexer/Token.hpp>
#include <cctype>
#include <format>

namespace bee::lexer {

using bee::diagnostics::DiagnosticCode;
using bee::diagnostics::DiagnosticLevel;

Lexer::Lexer(const Source &source, DiagnosticBag &bag)
    : m_cursor(0), m_line(1), m_col(1), m_source(source), m_bag(bag) {}

Token Lexer::nextToken() {
  this->skipWhitespaces();
  if (!this->hasMoreTokens()) {
    return Token(TokenKind::EndOfFile, "\0", this->span(1));
  }

  char current = this->peek();
  if (isalpha(current))
    return this->lexKeyword();
  if (current == '"')
    return this->lexString();
  if (isdigit(current))
    return this->lexNumber();

  return this->lexSymbol();
}

void Lexer::skipWhitespaces() {
  while (this->hasMoreTokens() && isspace(this->peek())) {
    if (this->peek() == '\n') {
      m_cursor += 1;
      m_line += 1;
      m_col = 1;
    } else {
      this->advance();
    }
  }
}

Token Lexer::lexKeyword() {
  std::size_t start = m_cursor;
  while (this->hasMoreTokens() &&
         (isalpha(this->peek()) || this->peek() == '_')) {
    this->advance();
  }

  std::string_view lexeme =
      std::string_view(m_source.code()).substr(start, m_cursor - start);
  TokenKind kind = bee::lexer::getKeywordTokenKind(lexeme);

  return Token(kind, lexeme, this->span(lexeme.length()));
}

Token Lexer::lexString() {
  this->advance(); // eating first string quote symbol.

  std::size_t start = m_cursor;
  while (this->hasMoreTokens() && this->peek() != '"' && this->peek() != '\n') {
    this->advance();
  }

  std::string_view lexeme =
      std::string_view(m_source.code()).substr(start, m_cursor - start);
  Token token(TokenKind::StringLit, lexeme, this->span(lexeme.length()));

  if (this->peek() != '"') {
    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::UnterminatedString,
                 token.span(), std::make_format_args());
  } else {
    this->advance(); // eating second string quote symbol.
  }

  return token;
}

Token Lexer::lexNumber() {
  std::size_t start = m_cursor;
  while (this->hasMoreTokens() && isdigit(this->peek())) {
    this->advance();
  }

  std::string_view lexeme =
      std::string_view(m_source.code()).substr(start, m_cursor - start);
  return Token(TokenKind::NumberLit, lexeme, this->span(lexeme.length()));
}

Token Lexer::lexSymbol() {
  std::string_view code(m_source.code());

  if (m_cursor + 1 < code.length()) {
    std::string_view lexeme = code.substr(m_cursor, 2);
    TokenKind kind = bee::lexer::getSymbolTokenKind(lexeme);
    if (kind != TokenKind::Invalid) {
      this->advance();
      this->advance();

      return Token(kind, lexeme, this->span(2));
    }
  }

  std::string_view lexeme = code.substr(m_cursor, 1);
  this->advance();

  TokenKind kind = bee::lexer::getSymbolTokenKind(lexeme);
  Token token(kind, lexeme, this->span(1));

  if (kind == TokenKind::Invalid) {
    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::UnexpectedSymbol,
                 token.span(), std::make_format_args(lexeme));
  }

  return token;
}

} // namespace bee::lexer
