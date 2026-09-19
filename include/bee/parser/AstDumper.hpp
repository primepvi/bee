#ifndef BEE_AST_DUMPER_HPP
#define BEE_AST_DUMPER_HPP

#define BEE_DUMP_RESET_COLOR "\x1b[0m"
#define BEE_DUMP_BRANCH_COLOR "\x1b[38;5;243m"
#define BEE_DUMP_STATEMENT_COLOR "\x1b[38;5;109m"
#define BEE_DUMP_EXPRESSION_COLOR "\x1b[38;5;139m"
#define BEE_DUMP_PROPERTY_COLOR "\x1b[38;5;179m"
#define BEE_DUMP_VALUE_COLOR "\x1b[38;5;108m"

#include <iostream>

#include "bee/parser/Ast.hpp"
#include "bee/parser/AstVisitors.hpp"

namespace bee::parser {

class AstDumper : public ExprVisitor<void>, public StmtVisitor<void> {
public:
  AstDumper(const Program &program, std::ostream &output);
  void dump();

  void
  visitVariableDeclarationStmt(const VariableDeclarationStmt &stmt) override;
  void
  visitFunctionDeclarationStmt(const FunctionDeclarationStmt &stmt) override;
  void visitReturnStmt(const ReturnStmt &stmt) override;
  void visitExprStmt(const ExprStmt &stmt) override;
  void visitEchoStmt(const EchoStmt &stmt) override;
  void visitIfStmt(const IfStmt &stmt) override;
  void visitBlockStmt(const BlockStmt &stmt) override;
  void visitWhileStmt(const WhileStmt &stmt) override;
  void visitForStmt(const ForStmt &stmt) override;
  void visitInvalidStmt(const InvalidStmt &stmt) override;

  void visitLiteralExpr(const LiteralExpr &expr) override;
  void visitIdentifierExpr(const IdentifierExpr &expr) override;
  void visitRangeExpr(const RangeExpr &expr) override;
  void visitAssignmentExpr(const AssignmentExpr &expr) override;
  void visitBinaryExpr(const BinaryExpr &expr) override;
  void visitUnaryExpr(const UnaryExpr &expr) override;
  void visitParenthesizedExpr(const ParenthesizedExpr &expr) override;
  void visitWhenExpr(const WhenExpr &expr) override;
  void visitCallExpr(const CallExpr &expr) override;
  void visitInvalidExpr(const InvalidExpr &expr) override;

  std::string makeDumpPrefix(std::string current, bool isLast) const;
  std::string makeDumpStmt(std::string prefix, std::string symbol,
                           std::string name) const;
  std::string makeDumpExpr(std::string prefix, std::string symbol,
                           std::string name) const;
  std::string makeDumpProperty(std::string prefix, std::string symbol,
                               std::string name) const;
  std::string makeDumpValue(std::string value) const;

private:
  const Program &m_program;
  std::ostream &m_output;
  std::string m_prefix;
  bool m_isLast = false;

  void dumpExpr(const Expr &expr, const std::string &prefix, bool isLast);
  void dumpStmt(const Stmt &stmt, const std::string &prefix, bool isLast);
};

} // namespace bee::parser

#endif // BEE_AST_DUMPER_HPP
