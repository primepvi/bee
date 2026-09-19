#ifndef BEE_AST_VISITORS_HPP
#define BEE_AST_VISITORS_HPP

#include "bee/parser/Ast.hpp"

namespace bee::parser {

template <typename T = void> class ExprVisitor {
public:
  virtual ~ExprVisitor() = default;

  virtual T visit(const Expr &expr) {
    switch (expr.kind()) {
    case ExprKind::Literal:
      return visitLiteralExpr(expr);
    case ExprKind::Identifier:
      return visitIdentifierExpr(expr);
    case ExprKind::Range:
      return visitRangeExpr(expr);
    case ExprKind::Assignment:
      return visitAssignmentExpr(expr);
    case ExprKind::Binary:
      return visitBinaryExpr(expr);
    case ExprKind::Unary:
      return visitUnaryExpr(expr);
    case ExprKind::Parenthesized:
      return visitParenthesizedExpr(expr);
    case ExprKind::When:
      return visitWhenExpr(expr);
    case ExprKind::Call:
      return visitCallExpr(expr);
    case ExprKind::Invalid:
      return visitInvalidExpr(expr);
    }
  }

  virtual T visitLiteralExpr(const LiteralExpr &expr) = 0;
  virtual T visitIdentifierExpr(const IdentifierExpr &expr) = 0;
  virtual T visitRangeExpr(const RangeExpr &expr) = 0;
  virtual T visitAssignmentExpr(const AssignmentExpr &expr) = 0;
  virtual T visitBinaryExpr(const BinaryExpr &expr) = 0;
  virtual T visitUnaryExpr(const UnaryExpr &expr) = 0;
  virtual T visitParenthesizedExpr(const ParenthesizedExpr &expr) = 0;
  virtual T visitWhenExpr(const WhenExpr &expr) = 0;
  virtual T visitCallExpr(const CallExpr &expr) = 0;
  virtual T visitInvalidExpr(const InvalidExpr &expr) = 0;
};

template <typename T = void> class StmtVisitor {
public:
  virtual ~StmtVisitor() = default;

  virtual T visit(const Stmt &stmt) {
    switch (stmt.kind()) {
    case StmtKind::VariableDeclaration:
      return visitVariableDeclarationStmt(stmt);
    case StmtKind::FunctionDeclaration:
      return visitFunctionDeclarationStmt(stmt);
    case StmtKind::Return:
      return visitReturnStmt(stmt);
    case StmtKind::Expr:
      return visitExprStmt(stmt);
    case StmtKind::Echo:
      return visitEchoStmt(stmt);
    case StmtKind::If:
      return visitIfStmt(stmt);
    case StmtKind::Block:
      return visitBlockStmt(stmt);
    case StmtKind::While:
      return visitWhileStmt(stmt);
    case StmtKind::For:
      return visitForStmt(stmt);
    case StmtKind::Invalid:
      return visitInvalidStmt(stmt);
    }
  }

  virtual T
  visitVariableDeclarationStmt(const VariableDeclarationStmt &stmt) = 0;
  virtual T
  visitFunctionDeclarationStmt(const FunctionDeclarationStmt &stmt) = 0;
  virtual T visitReturnStmt(const ReturnStmt &stmt) = 0;
  virtual T visitExprStmt(const ExprStmt &stmt) = 0;
  virtual T visitEchoStmt(const EchoStmt &stmt) = 0;
  virtual T visitIfStmt(const IfStmt &stmt) = 0;
  virtual T visitBlockStmt(const BlockStmt &stmt) = 0;
  virtual T visitWhileStmt(const WhileStmt &stmt) = 0;
  virtual T visitForStmt(const ForStmt &stmt) = 0;
  virtual T visitInvalidStmt(const InvalidStmt &stmt) = 0;
};

} // namespace bee::parser

#endif // BEE_AST_VISITORS_HPP
