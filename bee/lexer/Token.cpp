#include <array>
#include <bee/lexer/Token.hpp>
#include <string_view>

namespace bee::lexer {
Token::Token(TokenKind kind, std::string_view lexeme)
    : m_Kind(kind), m_Lexeme(lexeme) {}

constexpr auto keywords = std::to_array<TokenEntry>({
    {TokenKind::LetKw, "let"},
    {TokenKind::ConstKw, "const"},
    {TokenKind::EchoKw, "echo"},
    {TokenKind::TrueKw, "true"},
    {TokenKind::FalseKw, "false"},
    {TokenKind::NullKw, "null"},
    {TokenKind::AndKw, "and"},    
});

TokenKind getKeywordTokenKind(std::string_view keyword) {}

} // namespace bee::lexer
