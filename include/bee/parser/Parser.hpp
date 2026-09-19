#ifndef BEE_PARSER_HPP
#define BEE_PARSER_HPP

#include <memory>

#include "bee/Diagnostics.hpp"
#include "bee/Source.hpp"
#include "bee/lexer/Token.hpp"
#include "bee/parser/Ast.hpp"

namespace bee::parser {

class Parser {
public:
  Parser(const bee::Source &source, bee::DiagnosticBag &bag,
         const std::vector<bee::lexer::Token> &tokens);
  
  Program parse();
  
private:
  const bee::Source &m_source;
  bee::DiagnosticBag &m_bag;
  const std::vector<bee::lexer::Token> &m_tokens;
  std::size_t m_cursor = 0;
  bool m_panic = false;

  void synchronize();

  bee::lexer::Token expectToken(bee::lexer::TokenKind kind, std::string name);
  bee::lexer::Token peek() const;
  bee::lexer::Token lookahead() const;
  bee::lexer::Token eat();

  bool hasMoreTokens() const;
  bool canStartExpr() const;

  std::unique_ptr<Expr> parseExpr();
  std::unique_ptr<Expr> parseLiteralExpr();
  std::unique_ptr<Expr> parseIdentifierExpr();
  std::unique_ptr<Expr> parseAssignmentExpr();
  std::unique_ptr<Expr> parseParenthesizedExpr();
  std::unique_ptr<Expr> parseWhenExpr();
  std::unique_ptr<Expr> parseCallExpr();
  std::unique_ptr<Expr> parsePrimaryExpr();
  std::unique_ptr<Expr> parseBinaryExpr(std::size_t precedence);

  TypeAnnotation parseTypeAnnotation();

  std::unique_ptr<Stmt> parseStmt();
  std::unique_ptr<Stmt> parseNextStmt();
  std::unique_ptr<Stmt> parseVariableDeclarationStmt();
  std::unique_ptr<Stmt> parseFunctionDeclarationStmt();
  std::unique_ptr<Stmt> parseReturnStmt();
  std::unique_ptr<Stmt> parseExprStmt();
  std::unique_ptr<Stmt> parseEchoStmt();
  std::unique_ptr<Stmt> parseIfStmt();
  std::unique_ptr<Stmt>
  parseBlockStmt(std::span<bee::lexer::TokenKind> endKinds);
  std::unique_ptr<Stmt> parseWhileStmt();
  std::unique_ptr<Stmt> parseForStmt();
};

} // namespace bee::parser

#endif // BEE_PARSER_HPP
