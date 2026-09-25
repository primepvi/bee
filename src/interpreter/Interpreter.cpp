#include "bee/interpreter/Interpreter.hpp"
#include "bee/interpreter/Value.hpp"
#include "bee/interpreter/ValueEnvironment.hpp"
#include "bee/lexer/Token.hpp"
#include "bee/parser/Ast.hpp"
#include <iostream>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>

#include "utfcpp/utf8/checked.h"

namespace bee::interpreter {

using bee::lexer::TokenKind;

Interpreter::Interpreter(const bee::parser::Program &program)
    : m_program(program) {
  m_env = std::make_shared<ValueEnvironment>(ValueScopeKind::Global, nullptr);
}

void Interpreter::interpret() {
  for (const auto &stmt : m_program)
    visitStmt(*stmt);
}

Result Interpreter::visitVariableDeclarationStmt(
    const bee::parser::VariableDeclarationStmt &stmt) {
  std::unique_ptr<Value> value = visitExpr(*stmt.value());
  m_env->putValue(stmt.identifier().lexeme(), std::move(value));

  return Result{.isReturn = false, .value = std::nullopt};
}

Result Interpreter::visitFunctionDeclarationStmt(
    const bee::parser::FunctionDeclarationStmt &stmt) {

  std::vector<std::string_view> params;
  for (const auto &param : stmt.params())
    params.push_back(param.identifier.lexeme());

  FunctionValue functionValue(stmt.identifier().lexeme(), params, stmt);
  m_env->putValue(stmt.identifier().lexeme(),
                  std::make_unique<FunctionValue>(std::move(functionValue)));

  return Result{.isReturn = false, .value = std::nullopt};
}

Result Interpreter::visitReturnStmt(const bee::parser::ReturnStmt &stmt) {
  std::unique_ptr<Value> value = stmt.expr() == nullptr
                                     ? std::make_unique<NullValue>()
                                     : visitExpr(*stmt.expr());

  return Result{.isReturn = true, .value = std::move(value)};
}

Result Interpreter::visitExprStmt(const bee::parser::ExprStmt &stmt) {
  visitExpr(*stmt.expr());
  return Result{.isReturn = true, .value = std::nullopt};
}

Result Interpreter::visitEchoStmt(const bee::parser::EchoStmt &stmt) {
  std::unique_ptr<Value> message = visitExpr(*stmt.message());
  std::cout << message->toString() << "\n";

  return Result{.isReturn = false, .value = std::nullopt};
}

Result Interpreter::visitIfStmt(const bee::parser::IfStmt &stmt) {
  std::unique_ptr<Value> rawCondition = visitExpr(*stmt.condition());
  Result result = {.isReturn = false, .value = std::nullopt};

  const BoolValue &condition = static_cast<const BoolValue &>(*rawCondition);
  if (condition.value()) {
    return visitStmt(*stmt.consequent());
  } else if (stmt.alternate() != nullptr) {
    return visitStmt(*stmt.alternate());
  }

  return result;
}

Result Interpreter::visitBlockStmt(const bee::parser::BlockStmt &stmt) {
  ValueEnvironment blockEnv(ValueScopeKind::Block, m_env);

  if (stmt.captureAnnotation() != std::nullopt) {
    const auto &annotation = stmt.captureAnnotation().value();

    if (m_env->scopeKind() == ValueScopeKind::ForLoop) {
      std::array forCapturables = std::to_array<std::string_view>({
          "$FOR_ITERATION",
      });

      for (std::size_t i = 0; i < annotation.captures.size(); i++) {
        std::shared_ptr<Value> capturableValue =
            m_env->getValue(forCapturables[i]);

        std::string_view identifier = annotation.captures[i].lexeme();
        blockEnv.putValue(identifier, capturableValue->clone());
      }
    }
  }

  std::shared_ptr<ValueEnvironment> prevEnv = std::move(m_env);
  m_env = std::make_shared<ValueEnvironment>(blockEnv);

  for (const auto &stmt : stmt.stmts()) {
    Result result = visitStmt(*stmt);
    if (result.isReturn)
      return result;
  }

  m_env = std::move(prevEnv);

  return Result{.isReturn = false, .value = std::nullopt};
}

Result Interpreter::visitWhileStmt(const bee::parser::WhileStmt &stmt) {
  ValueEnvironment whileEnv(ValueScopeKind::Block, m_env);
  std::shared_ptr<ValueEnvironment> prevEnv = std::move(m_env);
  m_env = std::make_shared<ValueEnvironment>(whileEnv);

  std::unique_ptr<Value> rawCondition = visitExpr(*stmt.condition());
  BoolValue &condition = static_cast<BoolValue &>(*rawCondition);

  while (condition.value()) {
    Result result = visitStmt(*stmt.body());
    if (result.isReturn) {
      m_env = std::move(prevEnv);
      return result;
    }

    rawCondition = visitExpr(*stmt.condition());
    condition = static_cast<BoolValue &>(*rawCondition);
  }

  m_env = std::move(prevEnv);
  return Result{.isReturn = false, .value = std::nullopt};
}

Result Interpreter::visitForStmt(const bee::parser::ForStmt &stmt) {
  std::unique_ptr<Value> iteratorValue = visitExpr(*stmt.iterator());
  std::unique_ptr<Value> stepValue = stmt.step() == nullptr
                                         ? std::make_unique<IntValue>(1)
                                         : visitExpr(*stmt.step());

  RangeValue &iterator = static_cast<RangeValue &>(*iteratorValue);
  IntValue &step = static_cast<IntValue &>(*stepValue);

  ValueEnvironment forEnv(ValueScopeKind::ForLoop, m_env);
  std::shared_ptr<ValueEnvironment> prevEnv = std::move(m_env);
  m_env = std::make_shared<ValueEnvironment>(std::move(forEnv));

  if (iterator.start() > iterator.end()) {
    for (std::int64_t i = iterator.start(); i > iterator.end();
         i -= step.value()) {
      std::unique_ptr<IntValue> iterationValue = std::make_unique<IntValue>(i);
      m_env->putValue("$FOR_ITERATION", std::move(iterationValue));

      Result result = visitStmt(*stmt.body());
      if (result.isReturn) {
        return result;
      }
    }
  } else {
    for (std::int64_t i = iterator.start(); i < iterator.end();
         i += step.value()) {
      std::unique_ptr<IntValue> iterationValue = std::make_unique<IntValue>(i);
      m_env->putValue("$FOR_ITERATION", std::move(iterationValue));

      Result result = visitStmt(*stmt.body());
      if (result.isReturn) {
        return result;
      }
    }
  }

  m_env = std::move(prevEnv);

  return Result{.isReturn = false, .value = std::nullopt};
}

Result Interpreter::visitInvalidStmt(const bee::parser::InvalidStmt &stmt) {
  return Result{.isReturn = false, .value = std::nullopt};
}

std::unique_ptr<Value>
Interpreter::visitLiteralExpr(const bee::parser::LiteralExpr &expr) {
  bee::lexer::Token token = expr.value();

  switch (token.kind()) {
  case TokenKind::IntegerLit: {
    std::int64_t value = std::stoll(std::string(token.lexeme()));
    return std::make_unique<IntValue>(value);
  }
  case TokenKind::FloatLit: {
    double value = std::stod(std::string(token.lexeme()));
    return std::make_unique<FloatValue>(value);
  }
  case TokenKind::StringLit: {
    std::string value = std::string(token.lexeme());
    return std::make_unique<StringValue>(value);
  }
  case TokenKind::TrueKw:
  case TokenKind::FalseKw: {
    bool value = token.lexeme() == "true";
    return std::make_unique<BoolValue>(value);
  }

  case TokenKind::CharLit: {
    std::string lexeme = std::string(token.lexeme());
    auto it = lexeme.begin();
    char32_t value = utf8::next(it, lexeme.end());
    return std::make_unique<CharValue>(value);
  }

  case TokenKind::AtomLit: {
    std::string name = std::string(token.lexeme().substr(1));
    if (!m_atoms.contains(name)) {
      m_atoms.insert_or_assign(name, m_atoms.size() + 1);
    }

    return std::make_unique<AtomValue>(name, m_atoms.at(name));    
  }    

  case TokenKind::NullKw:
    return std::make_unique<NullValue>();

  default:
    throw std::runtime_error("Unreachable (Interpreter::visitLiteralExpr).");
  }
}

std::unique_ptr<Value>
Interpreter::visitIdentifierExpr(const bee::parser::IdentifierExpr &expr) {
  std::shared_ptr<Value> rawValue = m_env->getValue(expr.identifier().lexeme());
  switch (rawValue->kind()) {
  case ValueKind::Range: {
    RangeValue &value = static_cast<RangeValue &>(*rawValue);
    return std::make_unique<RangeValue>(value);
  }

  case ValueKind::Function: {
    FunctionValue &value = static_cast<FunctionValue &>(*rawValue);
    return std::make_unique<FunctionValue>(value);
  }

  default:
    return rawValue->clone();
  }
}

std::unique_ptr<Value>
Interpreter::visitAssignmentExpr(const bee::parser::AssignmentExpr &expr) {
  std::unique_ptr<Value> value = visitExpr(*expr.value());
  m_env->putValue(expr.identifier().lexeme(), value->clone());

  return value;
}

std::unique_ptr<Value>
Interpreter::visitBinaryExpr(const bee::parser::BinaryExpr &expr) {
  if (expr.op().kind() == TokenKind::AndKw) {

    std::unique_ptr<Value> leftValue = visitExpr(*expr.left());
    BoolValue &left = static_cast<BoolValue &>(*leftValue);
    if (!left.value())
      return std::make_unique<BoolValue>(false);

    std::unique_ptr<Value> rightValue = visitExpr(*expr.right());
    BoolValue &right = static_cast<BoolValue &>(*rightValue);

    return std::make_unique<BoolValue>(left.value() && right.value());
  } else if (expr.op().kind() == TokenKind::OrKw) {

    std::unique_ptr<Value> leftValue = visitExpr(*expr.left());
    BoolValue &left = static_cast<BoolValue &>(*leftValue);
    if (left.value())
      return std::make_unique<BoolValue>(true);

    std::unique_ptr<Value> rightValue = visitExpr(*expr.right());
    BoolValue &right = static_cast<BoolValue &>(*rightValue);

    return std::make_unique<BoolValue>(left.value() || right.value());
  }

  std::unique_ptr<Value> leftValue = visitExpr(*expr.left());
  std::unique_ptr<Value> rightValue = visitExpr(*expr.right());

  switch (expr.op().kind()) {
  case TokenKind::PlusSym:
    return evalAdd(*leftValue, *rightValue);
  case TokenKind::MinusSym:
    return evalSub(*leftValue, *rightValue);
  case TokenKind::StarSym:
    return evalMulti(*leftValue, *rightValue);
  case TokenKind::SlashSym:
    return evalDiv(*leftValue, *rightValue);
  case TokenKind::PercentageSym:
    return evalMod(*leftValue, *rightValue);
  case TokenKind::DoubleDotSym:
    return evalRange(*leftValue, *rightValue);

  case TokenKind::LtSym: {
    bool result = evalLt(*leftValue, *rightValue);
    return std::make_unique<BoolValue>(result);
  }

  case TokenKind::LteSym: {
    bool result = evalLte(*leftValue, *rightValue);
    return std::make_unique<BoolValue>(result);
  }

  case TokenKind::GtSym: {
    bool result = evalGt(*leftValue, *rightValue);
    return std::make_unique<BoolValue>(result);
  }

  case TokenKind::GteSym: {
    bool result = evalGte(*leftValue, *rightValue);
    return std::make_unique<BoolValue>(result);
  }

  case TokenKind::EqEqSym:
    return std::make_unique<BoolValue>(leftValue->equals(*rightValue));
  case TokenKind::NeqSym:
    return std::make_unique<BoolValue>(!leftValue->equals(*rightValue));

  default:
    throw std::runtime_error("Unreachable (Interpreter::visitBinaryExpr).");
  }
}

std::unique_ptr<Value>
Interpreter::visitUnaryExpr(const bee::parser::UnaryExpr &expr) {
  std::unique_ptr<Value> operandValue = visitExpr(*expr.operand());

  switch (expr.op().kind()) {
  case TokenKind::MinusSym:
    return evalMinus(*operandValue);

  case TokenKind::PlusSym:
    return operandValue;

  case TokenKind::NotKw: {
    bool result = evalNegation(*operandValue);
    return std::make_unique<BoolValue>(result);
  }    

  default:
    throw std::runtime_error("Unreachable (Interpreter::visitUnaryExpr).");
  }
}

std::unique_ptr<Value> Interpreter::visitParenthesizedExpr(
    const bee::parser::ParenthesizedExpr &expr) {
  return visitExpr(*expr.expr());
}

std::unique_ptr<Value>
Interpreter::visitWhenExpr(const bee::parser::WhenExpr &expr) {
  std::unique_ptr<Value> conditionValue = visitExpr(*expr.condition());
  BoolValue &condition = static_cast<BoolValue &>(*conditionValue);

  return condition.value() ? visitExpr(*expr.consequent())
                           : visitExpr(*expr.alternate());
}

std::unique_ptr<Value>
Interpreter::visitCallExpr(const bee::parser::CallExpr &expr) {
  std::shared_ptr<Value> functionValue =
      m_env->getValue(expr.identifier().lexeme());
  FunctionValue &function = static_cast<FunctionValue &>(*functionValue);

  ValueEnvironment callEnv(ValueScopeKind::Function, m_env);

  const auto &decl = function.body();

  const auto &params = decl.params();
  const auto &arguments = expr.arguments();

  for (std::size_t i = 0; i < params.size(); i++) {
    const auto &param = params[i];
    const auto &argument = arguments[i];

    std::unique_ptr<Value> argumentValue = visitExpr(*argument);
    callEnv.putValue(param.identifier.lexeme(), std::move(argumentValue));
  }

  std::shared_ptr<ValueEnvironment> prevEnv = std::move(m_env);
  m_env = std::make_shared<ValueEnvironment>(std::move(callEnv));

  std::unique_ptr<Value> returnValue = nullptr;
  const auto &body = decl.body();

  if (body->kind() == bee::parser::StmtKind::Expr) {
    const auto &expr = static_cast<const bee::parser::ExprStmt &>(*body);
    returnValue = visitExpr(*expr.expr());

  } else {
    Result result = visitStmt(*decl.body());
    returnValue = result.isReturn ? std::move(result.value.value())
                                  : std::make_unique<NullValue>();
  }

  m_env = std::move(prevEnv);
  return returnValue;
}

std::unique_ptr<Value>
Interpreter::visitInvalidExpr(const bee::parser::InvalidExpr &expr) {
  return std::make_unique<NullValue>();
}

} // namespace bee::interpreter
