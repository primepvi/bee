#ifndef BEE_INTERPRETER_HPP
#define BEE_INTERPRETER_HPP

#include <memory>
#include <optional>
#include <unordered_map>

#include "bee/interpreter/ValueEnvironment.hpp"
#include "bee/parser/Ast.hpp"
#include "bee/parser/AstVisitors.hpp"

namespace bee::interpreter {

struct Result {
  bool isReturn;
  std::optional<std::unique_ptr<Value>> value;
};

class Interpreter : public bee::parser::ExprVisitor<std::unique_ptr<Value>>,
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

  std::unique_ptr<Value>
  visitLiteralExpr(const bee::parser::LiteralExpr &expr) override;
  std::unique_ptr<Value>
  visitIdentifierExpr(const bee::parser::IdentifierExpr &expr) override;
  std::unique_ptr<Value>
  visitAssignmentExpr(const bee::parser::AssignmentExpr &expr) override;
  std::unique_ptr<Value>
  visitBinaryExpr(const bee::parser::BinaryExpr &expr) override;
  std::unique_ptr<Value>
  visitUnaryExpr(const bee::parser::UnaryExpr &expr) override;
  std::unique_ptr<Value>
  visitParenthesizedExpr(const bee::parser::ParenthesizedExpr &expr) override;
  std::unique_ptr<Value>
  visitWhenExpr(const bee::parser::WhenExpr &expr) override;
  std::unique_ptr<Value>
  visitCallExpr(const bee::parser::CallExpr &expr) override;
  std::unique_ptr<Value>
  visitInvalidExpr(const bee::parser::InvalidExpr &expr) override;

private:
  std::shared_ptr<ValueEnvironment> m_env;
  std::unordered_map<std::string, std::size_t> m_atoms;
  const bee::parser::Program &m_program;
};

} // namespace bee::interpreter

#endif // BEE_INTERPRETER_HPP
