#include "bee/interpreter/Interpreter.hpp"
#include "bee/interpreter/ValueEnvironment.hpp"
#include "bee/lexer/Token.hpp"
#include "bee/parser/Ast.hpp"
#include <iostream>
#include <memory>
#include <optional>
#include <variant>

namespace bee::interpreter {

using bee::lexer::TokenKind;

Interpreter::Interpreter(const bee::parser::Program &program)
    : m_program(program) {}

void Interpreter::interpret() {
  for (const auto &stmt : m_program)
    visitStmt(*stmt);
}

Result Interpreter::visitVariableDeclarationStmt(
    const bee::parser::VariableDeclarationStmt &stmt) {
  Value value = visitExpr(*stmt.value());
  m_env->putValue(stmt.identifier().lexeme(), value);

  return Result{.isReturn = false, .value = std::nullopt};
}

Result Interpreter::visitFunctionDeclarationStmt(
    const bee::parser::FunctionDeclarationStmt &stmt) {

  std::vector<std::string_view> params;
  for (const auto &param : stmt.params())
    params.push_back(param.identifier.lexeme());

  FunctionValue functionValue = {
      .identifier = stmt.identifier().lexeme(),
      .params = params,
      .body = stmt.body(),
  };
  m_env->putValue(stmt.identifier().lexeme(), functionValue);

  return Result{.isReturn = false, .value = std::nullopt};
}

Result Interpreter::visitReturnStmt(const bee::parser::ReturnStmt &stmt) {
  Value value =
      stmt.expr() == nullptr ? std::monostate{} : visitExpr(*stmt.expr());

  return Result{.isReturn = true, .value = value};
}

Result Interpreter::visitExprStmt(const bee::parser::ExprStmt &stmt) {
  visitExpr(*stmt.expr());
  return Result{.isReturn = true, .value = std::nullopt};
}

Result Interpreter::visitEchoStmt(const bee::parser::EchoStmt &stmt) {
  Value message = visitExpr(*stmt.message());

  std::visit(
      [](const auto &value) {
        using T = std::decay_t<decltype(value)>;

        if constexpr (std::is_same_v<T, FunctionValue>) {
          std::cout << "Function#<" << value.identifier << ">\n";
        } else if constexpr (std::is_same_v<T, RangeValue>) {
          std::cout << "Range#<" << value.start << "," << value.end << ">\n";
        } else if constexpr (std::is_same_v<T, std::int64_t>) {
          std::cout << value << '\n';
        } else if constexpr (std::is_same_v<T, bool>) {
          std::cout << (value ? "true" : "false") << '\n';
        } else if constexpr (std::is_same_v<T, std::string>) {
          std::cout << value << '\n';
        } else if constexpr (std::is_same_v<T, std::monostate>) {
          std::cout << "null\n";
        }
      },
      message);

  return Result{.isReturn = false, .value = std::nullopt};
}

Result Interpreter::visitIfStmt(const bee::parser::IfStmt &stmt) {
  Value condition = visitExpr(*stmt.condition());
  Result result = {.isReturn = false, .value = std::nullopt};

  if (std::get<bool>(condition)) {
    return visitStmt(*stmt.consequent());
  } else if (stmt.alternate() != nullptr) {
    return visitStmt(*stmt.alternate());
  }

  return result;
}

Result Interpreter::visitBlockStmt(const bee::parser::BlockStmt &stmt) {}

Result Interpreter::visitWhileStmt(const bee::parser::WhileStmt &stmt) {
  ValueEnvironment whileEnv(ValueScopeKind::Block, m_env);
  std::shared_ptr<ValueEnvironment> prevEnv = std::move(m_env);
  m_env = std::make_shared<ValueEnvironment>(whileEnv);

  while (std::get<bool>(visitExpr(*stmt.condition()))) {
    Result result = visitStmt(*stmt.body());
    if (result.isReturn) {
      m_env = std::move(prevEnv);
      return result;
    }
  }

  m_env = std::move(prevEnv);
  return Result{.isReturn = false, .value = std::nullopt};
}

Result Interpreter::visitForStmt(const bee::parser::ForStmt &stmt) {}

Result Interpreter::visitInvalidStmt(const bee::parser::InvalidStmt &stmt) {
  return Result{.isReturn = false, .value = std::nullopt};
}

Value Interpreter::visitLiteralExpr(const bee::parser::LiteralExpr &expr) {
  bee::lexer::Token token = expr.value();
  switch (token.kind()) {
  case TokenKind::NumberLit: {
    std::int64_t value = std::stoll(std::string(token.lexeme()));
    return value;
  }
  case TokenKind::StringLit: {
    std::string value = std::string(token.lexeme());
    return value;
  }
  case TokenKind::TrueKw:
  case TokenKind::FalseKw: {
    bool value = token.lexeme() == "true";
    return value;
  }

  default:
    return std::monostate{};
  }
}

Value Interpreter::visitIdentifierExpr(
    const bee::parser::IdentifierExpr &expr) {
  return m_env->getValue(expr.identifier().lexeme());
}

Value Interpreter::visitAssignmentExpr(
    const bee::parser::AssignmentExpr &expr) {
  Value value = visitExpr(*expr.value());
  m_env->putValue(expr.identifier().lexeme(), value);

  return value;
}

Value Interpreter::visitBinaryExpr(const bee::parser::BinaryExpr &expr) {}

Value Interpreter::visitUnaryExpr(const bee::parser::UnaryExpr &expr) {}

Value Interpreter::visitParenthesizedExpr(
    const bee::parser::ParenthesizedExpr &expr) {}
Value Interpreter::visitWhenExpr(const bee::parser::WhenExpr &expr) {}
Value Interpreter::visitCallExpr(const bee::parser::CallExpr &expr) {}
Value Interpreter::visitInvalidExpr(const bee::parser::InvalidExpr &expr) {}

} // namespace bee::interpreter
