#ifndef BEE_AST_VISITORS_HPP
#define BEE_AST_VISITORS_HPP

#include "bee/parser/Ast.hpp"

namespace bee::parser {

template <typename T> class ExprVisitor {
public:
  virtual ~ExprVisitor() = default;

  virtual T visitExpr(const Expr &expr) {
    switch (expr.kind()) {
    case ExprKind::Literal:
      return visitLiteralExpr(static_cast<const LiteralExpr &>(expr));
    case ExprKind::Identifier:
      return visitIdentifierExpr(static_cast<const IdentifierExpr &>(expr));
    case ExprKind::Assignment:
      return visitAssignmentExpr(static_cast<const AssignmentExpr &>(expr));
    case ExprKind::Binary:
      return visitBinaryExpr(static_cast<const BinaryExpr &>(expr));
    case ExprKind::Unary:
      return visitUnaryExpr(static_cast<const UnaryExpr &>(expr));
    case ExprKind::Parenthesized:
      return visitParenthesizedExpr(
          static_cast<const ParenthesizedExpr &>(expr));
    case ExprKind::When:
      return visitWhenExpr(static_cast<const WhenExpr &>(expr));
    case ExprKind::Call:
      return visitCallExpr(static_cast<const CallExpr &>(expr));
    case ExprKind::Invalid:
      return visitInvalidExpr(static_cast<const InvalidExpr &>(expr));
    }
  }

  virtual T visitLiteralExpr(const LiteralExpr &expr) = 0;
  virtual T visitIdentifierExpr(const IdentifierExpr &expr) = 0;
  virtual T visitAssignmentExpr(const AssignmentExpr &expr) = 0;
  virtual T visitBinaryExpr(const BinaryExpr &expr) = 0;
  virtual T visitUnaryExpr(const UnaryExpr &expr) = 0;
  virtual T visitParenthesizedExpr(const ParenthesizedExpr &expr) = 0;
  virtual T visitWhenExpr(const WhenExpr &expr) = 0;
  virtual T visitCallExpr(const CallExpr &expr) = 0;
  virtual T visitInvalidExpr(const InvalidExpr &expr) = 0;
};

template <typename T> class StmtVisitor {
public:
  virtual ~StmtVisitor() = default;

  virtual T visitStmt(const Stmt &stmt) {
    switch (stmt.kind()) {
    case StmtKind::VariableDeclaration:
      return visitVariableDeclarationStmt(
          static_cast<const VariableDeclarationStmt &>(stmt));
    case StmtKind::FunctionDeclaration:
      return visitFunctionDeclarationStmt(
          static_cast<const FunctionDeclarationStmt &>(stmt));
    case StmtKind::Return:
      return visitReturnStmt(static_cast<const ReturnStmt &>(stmt));
    case StmtKind::Expr:
      return visitExprStmt(static_cast<const ExprStmt &>(stmt));
    case StmtKind::Echo:
      return visitEchoStmt(static_cast<const EchoStmt &>(stmt));
    case StmtKind::If:
      return visitIfStmt(static_cast<const IfStmt &>(stmt));
    case StmtKind::Block:
      return visitBlockStmt(static_cast<const BlockStmt &>(stmt));
    case StmtKind::While:
      return visitWhileStmt(static_cast<const WhileStmt &>(stmt));
    case StmtKind::For:
      return visitForStmt(static_cast<const ForStmt &>(stmt));
    case StmtKind::Invalid:
      return visitInvalidStmt(static_cast<const InvalidStmt &>(stmt));
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
