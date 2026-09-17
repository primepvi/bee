#ifndef BEE_LEXER_HPP
#define BEE_LEXER_HPP

#include "bee/Diagnostics.hpp"
#include "bee/Source.hpp"
#include "bee/lexer/Token.hpp"
#include <cstddef>

namespace bee::lexer {

using bee::diagnostics::DiagnosticBag;

using bee::lexer::Token;
using bee::lexer::TokenKind;

using bee::source::Source;
using bee::source::SourceSpan;

class Lexer {
public:
  Lexer(const Source &source, DiagnosticBag &bag);
  inline bool hasMoreTokens() const {
    return m_cursor < m_source.code().length();
  }

  Token nextToken();

private:
  std::size_t m_cursor, m_line, m_col;
  const Source &m_source;
  DiagnosticBag &m_bag;

  inline char peek() const { return m_source.code().at(m_cursor); }
  inline char lookahead() const { return m_source.code().at(m_cursor + 1); }

  inline void advance() {
    m_cursor += 1;
    m_col += 1;
  }

  inline SourceSpan span(std::size_t length) const {
    return {.line = m_line,
            .col = m_col - length - 1,
            .start = m_cursor - length,
            .end = m_cursor};
  }

  void skipWhitespaces();
  Token lexKeyword();
  Token lexString();
  Token lexNumber();
  Token lexSymbol();
};

} // namespace bee::lexer

#endif // BEE_LEXER_HPP
