#ifndef BEE_TYPE_CHECKER_HPP
#define BEE_TYPE_CHECKER_HPP

#include "bee/Diagnostics.hpp"
#include "bee/parser/Ast.hpp"
#include "bee/parser/AstVisitors.hpp"
#include "bee/typechecker/TypeEnvironment.hpp"

namespace bee::typechecker {

struct TypeFlow {
  bool canContinue;
};

class TypeChecker : public bee::parser::ExprVisitor<std::unique_ptr<Type>>,
                    public bee::parser::StmtVisitor<TypeFlow> {
public:
  TypeChecker(const bee::parser::Program &program, bee::DiagnosticBag &bag);
  void typecheck();

  TypeFlow visitVariableDeclarationStmt(
      const bee::parser::VariableDeclarationStmt &stmt) override;
  TypeFlow visitFunctionDeclarationStmt(
      const bee::parser::FunctionDeclarationStmt &stmt) override;
  TypeFlow visitReturnStmt(const bee::parser::ReturnStmt &stmt) override;
  TypeFlow visitExprStmt(const bee::parser::ExprStmt &stmt) override;
  TypeFlow visitEchoStmt(const bee::parser::EchoStmt &stmt) override;
  TypeFlow visitIfStmt(const bee::parser::IfStmt &stmt) override;
  TypeFlow visitBlockStmt(const bee::parser::BlockStmt &stmt) override;
  TypeFlow visitWhileStmt(const bee::parser::WhileStmt &stmt) override;
  TypeFlow visitForStmt(const bee::parser::ForStmt &stmt) override;
  TypeFlow visitInvalidStmt(const bee::parser::InvalidStmt &stmt) override;

  std::unique_ptr<Type>
  visitLiteralExpr(const bee::parser::LiteralExpr &expr) override;
  std::unique_ptr<Type>
  visitIdentifierExpr(const bee::parser::IdentifierExpr &expr) override;
  std::unique_ptr<Type>
  visitAssignmentExpr(const bee::parser::AssignmentExpr &expr) override;
  std::unique_ptr<Type>
  visitBinaryExpr(const bee::parser::BinaryExpr &expr) override;
  std::unique_ptr<Type>
  visitUnaryExpr(const bee::parser::UnaryExpr &expr) override;
  std::unique_ptr<Type>
  visitParenthesizedExpr(const bee::parser::ParenthesizedExpr &expr) override;
  std::unique_ptr<Type>
  visitWhenExpr(const bee::parser::WhenExpr &expr) override;
  std::unique_ptr<Type>
  visitCallExpr(const bee::parser::CallExpr &expr) override;
  std::unique_ptr<Type>
  visitInvalidExpr(const bee::parser::InvalidExpr &expr) override;

private:
  const bee::parser::Program &m_program;
  bee::DiagnosticBag &m_bag;
  std::shared_ptr<TypeEnvironment> m_env;
  Type m_scopeReturnType;
};

} // namespace bee::typechecker

#endif // BEE_TYPE_CHECKER_HPP
