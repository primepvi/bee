#ifndef BEE_AST_HPP
#define BEE_AST_HPP

#include <memory>
#include <optional>
#include <vector>

#include "bee/Source.hpp"
#include "bee/lexer/Token.hpp"

namespace bee::parser {

enum class ExprKind {
  Literal,
  Identifier,
  Range,
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

class RangeExpr : public Expr {
public:
  RangeExpr(std::unique_ptr<Expr> start, bee::lexer::Token symbol,
            std::unique_ptr<Expr> end);

  inline ExprKind kind() const override { return ExprKind::Range; }
  inline bee::SourceSpan span() const override { return m_span; }

private:
  std::unique_ptr<Expr> m_start, m_end;
  bee::lexer::Token m_symbol;
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
           std::vector<std::unique_ptr<Expr>> arguments,
           bee::lexer::Token closeParen);

  inline ExprKind kind() const override { return ExprKind::Call; }
  inline bee::SourceSpan span() const override { return m_span; }

private:
  bee::lexer::Token m_identifier, m_openParen, m_closeParen;
  std::vector<std::unique_ptr<Expr>> m_arguments;
  bee::SourceSpan m_span;
};

class InvalidExpr : public Expr {
public:
  InvalidExpr(bee::lexer::Token invalid);

  inline ExprKind kind() const override { return ExprKind::Invalid; }
  inline bee::SourceSpan span() const override { return m_span; }

private:
  bee::lexer::Token m_invalid;
  bee::SourceSpan m_span;
};

enum class StmtKind {
  VariableDeclaration,
  FunctionDeclaration,
  Return,
  Expr,
  Echo,
  If,
  Block,
  While,
  For,
  Invalid,
};

class Stmt {
public:
  virtual ~Stmt() = default;
  virtual StmtKind kind() const = 0;
  virtual bee::SourceSpan span() const = 0;
};

struct TypeAnnotation {
  bee::lexer::Token colon;
  bee::lexer::Token identifier;
  bool nullable;
  bee::SourceSpan span;
};

class VariableDeclarationStmt : public Stmt {
public:
  VariableDeclarationStmt(bee::lexer::Token keyword,
                          bee::lexer::Token identifier,
                          bee::lexer::Token assignment,
                          std::optional<TypeAnnotation> typeAnnotation,
                          std::unique_ptr<Expr> value);

  inline StmtKind kind() const override {
    return StmtKind::VariableDeclaration;
  }

  inline bee::SourceSpan span() const override { return m_span; }

private:
  bee::lexer::Token m_keyword, m_identifier, m_assignment;
  std::optional<TypeAnnotation> m_typeAnnotation;
  std::unique_ptr<Expr> m_value;
  bee::SourceSpan m_span;
};

struct FunctionDeclarationParam {
  bee::lexer::Token identifier;
  TypeAnnotation typeAnnotation;
  bee::SourceSpan m_span;
};

class FunctionDeclarationStmt : public Stmt {
public:
  FunctionDeclarationStmt(bee::lexer::Token keyword,
                          bee::lexer::Token identifier,
                          TypeAnnotation typeAnnotation,
                          std::vector<FunctionDeclarationParam> params,
                          std::unique_ptr<Stmt> body);

  inline StmtKind kind() const override {
    return StmtKind::FunctionDeclaration;
  }

  inline bee::SourceSpan span() const override { return m_span; }

private:
  bee::lexer::Token m_keyword, m_identifier;
  TypeAnnotation m_typeAnnotation;
  std::vector<FunctionDeclarationParam> m_params;
  std::unique_ptr<Stmt> m_body;
  bee::SourceSpan m_span;
};

class ReturnStmt : public Stmt {
public:
  ReturnStmt(bee::lexer::Token keyword, std::unique_ptr<Expr> expr);

  inline StmtKind kind() const override { return StmtKind::Return; }
  inline bee::SourceSpan span() const override { return m_span; }

private:
  bee::lexer::Token m_keyword;
  std::unique_ptr<Expr> m_expr;
  bee::SourceSpan m_span;
};

class ExprStmt : public Stmt {
public:
  ExprStmt(std::unique_ptr<Expr> expr);

  inline StmtKind kind() const override { return StmtKind::Expr; }
  inline bee::SourceSpan span() const override { return m_span; }

private:
  std::unique_ptr<Expr> m_expr;
  bee::SourceSpan m_span;
};

class EchoStmt : public Stmt {
public:
  EchoStmt(bee::lexer::Token keyword, std::unique_ptr<Expr> message);

  inline StmtKind kind() const override { return StmtKind::Echo; }
  inline bee::SourceSpan span() const override { return m_span; }

private:
  bee::lexer::Token m_keyword;
  std::unique_ptr<Expr> m_message;
  bee::SourceSpan m_span;
};

class IfStmt : public Stmt {
public:
  IfStmt(bee::lexer::Token keyword, std::unique_ptr<Expr> condition,
         std::unique_ptr<Stmt> consequent, std::unique_ptr<Stmt> alternate);

  inline StmtKind kind() const override { return StmtKind::If; }
  inline bee::SourceSpan span() const override { return m_span; }

private:
  bee::lexer::Token m_keyword;
  std::unique_ptr<Expr> m_condition;
  std::unique_ptr<Stmt> m_consequent, m_alternate;
  bee::SourceSpan m_span;
};

class WhileStmt : public Stmt {
public:
  WhileStmt(bee::lexer::Token keyword, std::unique_ptr<Expr> condition,
            std::unique_ptr<Stmt> body);

  inline StmtKind kind() const override { return StmtKind::While; }
  inline bee::SourceSpan span() const override { return m_span; }

private:
  bee::lexer::Token m_keyword;
  std::unique_ptr<Expr> m_condition;
  std::unique_ptr<Stmt> m_body;
  bee::SourceSpan m_span;
};

class ForStmt : public Stmt {
public:
  ForStmt(bee::lexer::Token keyword, std::unique_ptr<Expr> range,
          std::unique_ptr<Expr> increment, std::unique_ptr<Stmt> body);

  inline StmtKind kind() const override { return StmtKind::For; }
  inline bee::SourceSpan span() const override { return m_span; }

private:
  bee::lexer::Token m_keyword;
  std::unique_ptr<Expr> m_range, m_increment;
  std::unique_ptr<Stmt> m_body;
  bee::SourceSpan m_span;
};

struct BlockCaptureAnnotation {
  bee::lexer::Token openPipe;
  bee::lexer::Token closePipe;
  std::vector<bee::lexer::Token> identifiers;
};

class BlockStmt : public Stmt {
public:
  BlockStmt(bee::lexer::Token openKeyword,
            std::optional<BlockCaptureAnnotation> captureAnnotation,
            std::vector<std::unique_ptr<Stmt>> stmts,
            bee::lexer::Token closeKeyword);

  inline StmtKind kind() const override { return StmtKind::Block; }
  inline bee::SourceSpan span() const override { return m_span; }

private:
  bee::lexer::Token m_openKeyword, m_closeKeyword;
  std::optional<BlockCaptureAnnotation> captureAnnotation;
  std::vector<std::unique_ptr<Stmt>> m_stmts;
  bee::SourceSpan m_span;
};

class InvalidStmt : public Stmt {
public:
  InvalidStmt(bee::lexer::Token invalid);

  inline StmtKind kind() const override { return StmtKind::Invalid; }
  inline bee::SourceSpan span() const override { return m_span; }

private:
  bee::lexer::Token m_invalid;
  bee::SourceSpan m_span;
};

} // namespace bee::parser

#endif // BEE_AST_HPP
