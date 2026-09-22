#include "bee/typechecker/TypeChecker.hpp"
#include "bee/lexer/Token.hpp"
#include "bee/parser/Ast.hpp"
#include "bee/typechecker/Type.hpp"
#include "bee/typechecker/TypeEnvironment.hpp"
#include "bee/typechecker/TypeSymbol.hpp"
#include <memory>

namespace bee::typechecker {

TypeChecker::TypeChecker(const bee::parser::Program &program,
                         bee::DiagnosticBag bag)
    : m_program(program), m_bag(bag),
      m_scopeReturnType(Type(TypeKind::Void, false)) {

  TypeEnvironment globalEnv(TypeScopeKind::Global, nullptr);

  m_env = std::make_shared<TypeEnvironment>(std::move(globalEnv));
}

TypeFlow TypeChecker::visitVariableDeclarationStmt(
    const bee::parser::VariableDeclarationStmt &stmt) {

  std::string_view variableName = stmt.identifier().lexeme();
  if (m_env->scopeHasSymbol(variableName)) {
    // TODO: add identifier already declared error diagnostic.
    return TypeFlow{.canContinue = true};
  }

  std::unique_ptr<Type> valueType = visitExpr(*stmt.value());
  std::unique_ptr<Type> variableType = nullptr;

  if (stmt.typeAnnotation() != std::nullopt) {
    const bee::parser::TypeAnnotation annotation =
        stmt.typeAnnotation().value();
    Type annotationType = Type::fromAnnotation(annotation);

    if (annotationType.isEmpty() || annotationType.isInvalid()) {
      // TODO: add invalid type annotation error diagnostic.
      variableType = std::make_unique<Type>(Type::invalid());
    } else if (!valueType->isAssignableTo(annotationType)) {
      // TODO: add type mismatch error diagnostic.
      variableType = std::make_unique<Type>(Type::invalid());
    } else {
      variableType = std::make_unique<Type>(std::move(annotationType));
    }
  } else {
    variableType = std::move(valueType);
  }

  if (variableType->isEmpty()) {
    // TODO: add invalid void type usage.
    variableType = std::make_unique<Type>(Type::invalid());
  }

  bool isConstant = stmt.keyword().kind() == bee::lexer::TokenKind::ConstKw;
  VariableSymbol variable(variableName, isConstant, std::move(*variableType));

  m_env->putSymbol(std::move(variable));
  return TypeFlow{.canContinue = true};
}

TypeFlow TypeChecker::visitFunctionDeclarationStmt(
    const bee::parser::FunctionDeclarationStmt &stmt) {
  if (m_env->scopeKind() != TypeScopeKind::Global) {
    // TODO: add non-global scope function declaration error diagnostic.
  }

  std::string_view functionName = stmt.identifier().lexeme();
  if (m_env->scopeHasSymbol(functionName)) {
    // TODO: add identifier already declared error diagnostic.
    return TypeFlow{.canContinue = true};
  }

  std::vector<Type> paramTypes;
  TypeEnvironment functionEnv(TypeScopeKind::Function, m_env);
  for (const auto &param : stmt.params()) {
    Type paramType = Type::fromAnnotation(param.typeAnnotation);
    if (paramType.isEmpty() || paramType.isInvalid()) {
      // TODO: add invalid type annotation error diagnostic.
    }

    VariableSymbol paramSymbol(param.identifier.lexeme(), true,
                               std::move(paramType));
    functionEnv.putSymbol(paramSymbol);
  }

  Type returnType = Type::fromAnnotation(stmt.typeAnnotation());
  if (returnType.isInvalid()) {
    // TODO: add invalid type annotation error diagnostic.
  }

  Type functionType =
      Type::function(std::move(paramTypes), std::move(returnType));
  FunctionSymbol functionSymbol(functionName, stmt.params().size(),
                                std::move(functionType));
  functionEnv.putSymbol(functionSymbol);
  m_env->putSymbol(functionSymbol);

  Type prevScopeReturnType = std::move(m_scopeReturnType);
  m_scopeReturnType = std::move(returnType);

  std::shared_ptr<TypeEnvironment> prevEnv = std::move(m_env);
  m_env = std::make_shared<TypeEnvironment>(std::move(functionEnv));

  TypeFlow flow = visitStmt(*stmt.body());
  m_env = std::move(prevEnv);
  m_scopeReturnType = std::move(prevScopeReturnType);

  if (!returnType.isEmpty() && flow.canContinue) {
    // TODO: add void control paths diagnostic error.
  }

  return TypeFlow{.canContinue = true};
}

TypeFlow TypeChecker::visitReturnStmt(const bee::parser::ReturnStmt &stmt) {
  std::shared_ptr<TypeEnvironment> functionScope = m_env;
  while (functionScope->parent() != nullptr &&
         functionScope->scopeKind() != TypeScopeKind::Function) {
    functionScope = functionScope->parent();
  }

  if (functionScope->scopeKind() != TypeScopeKind::Function) {
    // TODO: add invalid return usage error diagnostic.
    return TypeFlow{.canContinue = true};
  }

  std::unique_ptr<Type> returnType = stmt.expr() == nullptr
                                         ? std::make_unique<Type>(Type::empty())
                                         : visitExpr(*stmt.expr());
  if (!returnType->isAssignableTo(m_scopeReturnType)) {
    // TODO: add type mismatch error diagnostic.
  }

  return TypeFlow{.canContinue = false};
}

TypeFlow TypeChecker::visitExprStmt(const bee::parser::ExprStmt &stmt) {}

TypeFlow TypeChecker::visitEchoStmt(const bee::parser::EchoStmt &stmt) {}
TypeFlow TypeChecker::visitIfStmt(const bee::parser::IfStmt &stmt) {}
TypeFlow TypeChecker::visitBlockStmt(const bee::parser::BlockStmt &stmt) {}
TypeFlow TypeChecker::visitWhileStmt(const bee::parser::WhileStmt &stmt) {}
TypeFlow TypeChecker::visitForStmt(const bee::parser::ForStmt &stmt) {}
TypeFlow TypeChecker::visitInvalidStmt(const bee::parser::InvalidStmt &stmt) {}

std::unique_ptr<Type>
TypeChecker::visitLiteralExpr(const bee::parser::LiteralExpr &expr) {}
std::unique_ptr<Type>
TypeChecker::visitIdentifierExpr(const bee::parser::IdentifierExpr &expr) {}
std::unique_ptr<Type>
TypeChecker::visitAssignmentExpr(const bee::parser::AssignmentExpr &expr) {}
std::unique_ptr<Type>
TypeChecker::visitBinaryExpr(const bee::parser::BinaryExpr &expr) {}
std::unique_ptr<Type>
TypeChecker::visitUnaryExpr(const bee::parser::UnaryExpr &expr) {}
std::unique_ptr<Type> TypeChecker::visitParenthesizedExpr(
    const bee::parser::ParenthesizedExpr &expr) {}
std::unique_ptr<Type>
TypeChecker::visitWhenExpr(const bee::parser::WhenExpr &expr) {}
std::unique_ptr<Type>
TypeChecker::visitCallExpr(const bee::parser::CallExpr &expr) {}
std::unique_ptr<Type>
TypeChecker::visitInvalidExpr(const bee::parser::InvalidExpr &expr) {}

} // namespace bee::typechecker
