#include <cctype>
#include <format>
#include <string_view>

#include "bee/Diagnostics.hpp"
#include "bee/lexer/Lexer.hpp"
#include "bee/lexer/Token.hpp"

#include "utfcpp/utf8.h"

namespace bee::lexer {

Lexer::Lexer(const bee::Source &source, bee::DiagnosticBag &bag)
    : m_cursor(0), m_line(1), m_col(1), m_source(source), m_bag(bag) {}

Token Lexer::nextToken() {
  skipWhitespaces();

  if (!hasMoreTokens()) {
    return Token(TokenKind::EndOfFile, "\0", span(1));
  }

  char current = peek();
  if (isalpha(current))
    return lexKeyword();
  if (current == '"')
    return lexString();
  if (current == '\'')
    return lexChar();
  if (isdigit(current))
    return lexNumber();
  if (current == ':' && isalpha(lookahead()))
    return lexAtom();

  return lexSymbol();
}

void Lexer::skipWhitespaces() {
  while (hasMoreTokens() && isspace(peek())) {
    if (peek() == '\n') {
      m_cursor += 1;
      m_line += 1;
      m_col = 1;
    } else {
      advance();
    }
  }
}

Token Lexer::lexKeyword() {
  std::size_t start = m_cursor;
  while (hasMoreTokens() && (isalnum(peek()) || peek() == '_')) {
    advance();
  }

  std::string_view lexeme =
      std::string_view(m_source.code()).substr(start, m_cursor - start);
  TokenKind kind = getKeywordTokenKind(lexeme);

  return Token(kind, lexeme, span(lexeme.length()));
}

Token Lexer::lexString() {
  this->advance(); // eating first string quote symbol.

  std::size_t start = m_cursor;
  while (hasMoreTokens() && peek() != '"' && peek() != '\n') {
    this->advance();
  }

  std::string_view lexeme =
      std::string_view(m_source.code()).substr(start, m_cursor - start);
  Token token(TokenKind::StringLit, lexeme, span(lexeme.length()));

  if (this->peek() != '"') {
    m_bag.report(bee::DiagnosticLevel::Error,
                 bee::DiagnosticCode::UnterminatedString, token.span(),
                 std::make_format_args());
  } else {
    this->advance(); // eating second string quote symbol.
  }

  return token;
}

Token Lexer::lexChar() {
  this->advance(); // eating first quote symbol

  const std::string &code = m_source.code();
  if (!hasMoreTokens()) {
    m_bag.report(bee::DiagnosticLevel::Error,
                 bee::DiagnosticCode::UnterminatedChar, span(1),
                 std::make_format_args());

    return Token(TokenKind::Invalid, "", span(1));
  }

  auto it = code.begin() + m_cursor;
  auto itStart = it;

  std::size_t start = m_cursor;

  utf8::next(it, code.end());
  std::size_t bytes = it - itStart;
  m_cursor += bytes;

  if (peek() == '\'') {
    advance();
    std::string_view lexeme =
        std::string_view(m_source.code()).substr(start, bytes);

    return Token(TokenKind::CharLit, lexeme, span(bytes));
  }

  while (hasMoreTokens() && peek() != '\'' && peek() != '\n')
    advance();

  if (peek() == '\'')
    advance();

  std::string_view lexeme =
      std::string_view(m_source.code()).substr(start, m_cursor - start);
  Token token(TokenKind::Invalid, lexeme, span(m_cursor - start));

  m_bag.report(DiagnosticLevel::Error, DiagnosticCode::InvalidCharLength,
               token.span(), std::make_format_args());

  return token;
}

Token Lexer::lexNumber() {
  std::size_t start = m_cursor;
  bool isFloat = false;

  while (hasMoreTokens() && isdigit(peek()) || (peek() == '.' && !isFloat)) {
    // double dot sym
    if (peek() == '.' && lookahead() == '.')
      break;    
    else if (peek() == '.')
      isFloat = true;

    advance();
  }

  TokenKind kind = isFloat ? TokenKind::FloatLit : TokenKind::IntegerLit;
  std::string_view lexeme =
      std::string_view(m_source.code()).substr(start, m_cursor - start);
  return Token(kind, lexeme, span(lexeme.length()));
}

Token Lexer::lexSymbol() {
  std::string_view code(m_source.code());

  if (m_cursor + 1 < code.length()) {
    std::string_view lexeme = code.substr(m_cursor, 2);
    TokenKind kind = bee::lexer::getSymbolTokenKind(lexeme);
    if (kind != TokenKind::Invalid) {
      advance();
      advance();

      return Token(kind, lexeme, this->span(2));
    }
  }

  std::string_view lexeme = code.substr(m_cursor, 1);
  advance();

  TokenKind kind = getSymbolTokenKind(lexeme);
  Token token(kind, lexeme, span(1));

  if (kind == TokenKind::Invalid) {
    m_bag.report(bee::DiagnosticLevel::Error,
                 bee::DiagnosticCode::UnexpectedSymbol, token.span(),
                 std::make_format_args(lexeme));
  }

  return token;
}

Token Lexer::lexAtom() {
  std::size_t start = m_cursor;

  advance(); // eating colon  
  while (hasMoreTokens() && (isalpha(peek()) || peek() == '_')) {
    advance();
  }

  std::string_view lexeme =
      std::string_view(m_source.code()).substr(start, m_cursor - start);

  return Token(TokenKind::AtomLit, lexeme, span(lexeme.length()));
}

} // namespace bee::lexer
