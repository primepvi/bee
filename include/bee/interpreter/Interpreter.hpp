#ifndef BEE_INTERPRETER_HPP
#define BEE_INTERPRETER_HPP

#include <memory>
#include <optional>

#include "bee/interpreter/ValueEnvironment.hpp"
#include "bee/parser/Ast.hpp"
#include "bee/parser/AstVisitors.hpp"

namespace bee::interpreter {

struct Result {
  bool isReturn;
  std::optional<Value> value;
};

class Interpreter : public bee::parser::ExprVisitor<Value>,
                    public bee::parser::StmtVisitor<Result> {
public:
  Interpreter(const bee::parser::Program &program);
  void interpret();

  Result visitVariableDeclarationStmt(
      const bee::parser::VariableDeclarationStmt &stmt) override;
  Result visitFunctionDeclarationStmt(
      const bee::parser::FunctionDeclarationStmt &stmt) override;
  Result visitReturnStmt(const bee::parser::ReturnStmt &stmt) override;
  Result visitExprStmt(const bee::parser::ExprStmt &stmt) override;
  Result visitEchoStmt(const bee::parser::EchoStmt &stmt) override;
  Result visitIfStmt(const bee::parser::IfStmt &stmt) override;
  Result visitBlockStmt(const bee::parser::BlockStmt &stmt) override;
  Result visitWhileStmt(const bee::parser::WhileStmt &stmt) override;
  Result visitForStmt(const bee::parser::ForStmt &stmt) override;
  Result visitInvalidStmt(const bee::parser::InvalidStmt &stmt) override;

  Value visitLiteralExpr(const bee::parser::LiteralExpr &expr) override;
  Value visitIdentifierExpr(const bee::parser::IdentifierExpr &expr) override;
  Value visitAssignmentExpr(const bee::parser::AssignmentExpr &expr) override;
  Value visitBinaryExpr(const bee::parser::BinaryExpr &expr) override;
  Value visitUnaryExpr(const bee::parser::UnaryExpr &expr) override;
  Value
  visitParenthesizedExpr(const bee::parser::ParenthesizedExpr &expr) override;
  Value visitWhenExpr(const bee::parser::WhenExpr &expr) override;
  Value visitCallExpr(const bee::parser::CallExpr &expr) override;
  Value visitInvalidExpr(const bee::parser::InvalidExpr &expr) override;

private:
  std::shared_ptr<ValueEnvironment> m_env;
  const bee::parser::Program &m_program;
};

} // namespace bee::interpreter

#endif // BEE_INTERPRETER_HPP
