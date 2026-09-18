#ifndef BEE_AST_HPP
#define BEE_AST_HPP

#include <memory>
#include <vector>

#include "bee/Source.hpp"
#include "bee/lexer/Token.hpp"

namespace bee::parser {
enum class ExprKind {
  Literal,
  Identifier,
  Assignment,
  Binary,
  Unary,
  Parenthesized,
  When,
  Call,
  Invalid,
};

class Expr {
public:
  virtual ~Expr() = default;
  virtual ExprKind kind() const = 0;
  virtual bee::SourceSpan span() const = 0;
};

class LiteralExpr : public Expr {
public:
  LiteralExpr(bee::lexer::Token value);
  
  inline ExprKind kind() const override { return ExprKind::Literal; }
  inline bee::SourceSpan span() const override { return m_span; }
private:
  bee::lexer::Token m_value;
  bee::SourceSpan m_span;
};

class IdentifierExpr : public Expr {
public:
  IdentifierExpr(bee::lexer::Token identifier);
  
  inline ExprKind kind() const override { return ExprKind::Identifier; }
  inline bee::SourceSpan span() const override { return m_span; }  

private:
  bee::lexer::Token m_identifier;
  bee::SourceSpan m_span;
};

class AssignmentExpr : public Expr {
public:
  AssignmentExpr(bee::lexer::Token identifier, bee::lexer::Token equal,
                 std::unique_ptr<Expr> value);
  
  inline ExprKind kind() const override { return ExprKind::Assignment; }
  inline bee::SourceSpan span() const override { return m_span; }  

private:
  bee::lexer::Token m_identifier, m_equal;
  std::unique_ptr<Expr> m_value;
  bee::SourceSpan m_span;
};

class BinaryExpr : public Expr {
public:
  BinaryExpr(std::unique_ptr<Expr> left, bee::lexer::Token op,
             std::unique_ptr<Expr> right);
  
  inline ExprKind kind() const override { return ExprKind::Binary; }
  inline bee::SourceSpan span() const override { return m_span; }  

private:
  std::unique_ptr<Expr> m_left, m_right;
  bee::lexer::Token m_op;
  bee::SourceSpan m_span;
};

class UnaryExpr : public Expr {
public:
  UnaryExpr(bee::lexer::Token op, std::unique_ptr<Expr> operand);

  inline ExprKind kind() const override { return ExprKind::Unary; }
  inline bee::SourceSpan span() const override { return m_span; }

private:
  bee::lexer::Token m_op;
  std::unique_ptr<Expr> m_operand;
  bee::SourceSpan m_span;
};

class ParenthesizedExpr : public Expr {
public:
  ParenthesizedExpr(bee::lexer::Token openParen, std::unique_ptr<Expr> expr,
                    bee::lexer::Token closeParen);

  inline ExprKind kind() const override { return ExprKind::Parenthesized; }
  inline bee::SourceSpan span() const override { return m_span; }

private:
  bee::lexer::Token m_openParen, m_closeParen;
  std::unique_ptr<Expr> m_expr;
  bee::SourceSpan m_span;
};

class WhenExpr : public Expr {
public:
  WhenExpr(bee::lexer::Token when, std::unique_ptr<Expr> condition,
           bee::lexer::Token then, std::unique_ptr<Expr> consequent,
           bee::lexer::Token otherwise, std::unique_ptr<Expr> alternate);

  inline ExprKind kind() const override { return ExprKind::When; }
  inline bee::SourceSpan span() const override { return m_span; }

private:
  bee::lexer::Token m_when, m_then, m_otherwise;
  std::unique_ptr<Expr> m_condition, m_consequent, m_alternate;
  bee::SourceSpan m_span;
};

class CallExpr : public Expr {
public:
  CallExpr(bee::lexer::Token identifier, bee::lexer::Token openParen,
           std::vector<std::unique_ptr<Expr>> arguments, bee::lexer::Token closeParen);

  inline ExprKind kind() const override { return ExprKind::Call; }
  inline bee::SourceSpan span() const override { return m_span; }

private:
  bee::lexer::Token m_identifier, m_openParen, m_closeParen;
  std::vector<std::unique_ptr<Expr>> arguments;
  bee::SourceSpan m_span;
};

class InvalidExpr : public Expr {
public:
  InvalidExpr(bee::SourceSpan span);

  inline ExprKind kind() const override { return ExprKind::Invalid; }
  inline bee::SourceSpan span() const override { return m_span; }
private:
  bee::SourceSpan m_span;
};

}

#endif // BEE_AST_HPP
