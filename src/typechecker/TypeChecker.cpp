#include "bee/typechecker/TypeChecker.hpp"
#include "bee/lexer/Token.hpp"
#include "bee/parser/Ast.hpp"

namespace bee::typechecker {

using bee::lexer::TokenKind;

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

TypeFlow TypeChecker::visitExprStmt(const bee::parser::ExprStmt &stmt) {
  visitExpr(*stmt.expr());
  return TypeFlow{.canContinue = true};
}

TypeFlow TypeChecker::visitEchoStmt(const bee::parser::EchoStmt &stmt) {
  std::unique_ptr<Type> messageType = visitExpr(*stmt.message());
  if (messageType->isEmpty()) {
    // TODO: add invalid void usage error diagnostic.
  }

  return TypeFlow{.canContinue = true};
}

TypeFlow TypeChecker::visitIfStmt(const bee::parser::IfStmt &stmt) {
  std::unique_ptr<Type> conditionType = visitExpr(*stmt.condition());
  if (conditionType->kind() != TypeKind::Bool) {
    // TODO: add type mismatch error diagnostic.
  }

  TypeFlow consequentFlow = visitStmt(*stmt.consequent());
  TypeFlow alternateFlow = stmt.alternate() == nullptr
                               ? TypeFlow{.canContinue = true}
                               : visitStmt(*stmt.alternate());

  return TypeFlow{.canContinue =
                      consequentFlow.canContinue || alternateFlow.canContinue};
}

TypeFlow TypeChecker::visitBlockStmt(const bee::parser::BlockStmt &stmt) {
  TypeEnvironment blockEnv(TypeScopeKind::Block, m_env);
  std::shared_ptr<TypeEnvironment> prevEnv = std::move(m_env);
  m_env = std::make_shared<TypeEnvironment>(std::move(blockEnv));

  TypeFlow flow = {.canContinue = true};
  for (const auto &innerStmt : stmt.stmts()) {
    if (!flow.canContinue)
      break;

    flow = visitStmt(*innerStmt);
  }

  m_env = std::move(prevEnv);
  return flow;
}

TypeFlow TypeChecker::visitWhileStmt(const bee::parser::WhileStmt &stmt) {
  std::unique_ptr<Type> conditionType = visitExpr(*stmt.condition());
  if (conditionType->kind() != TypeKind::Bool) {
    // TODO: add type mismatch error diagnostic.
  }

  TypeEnvironment loopScope(TypeScopeKind::Block, m_env);
  std::shared_ptr<TypeEnvironment> prevEnv = std::move(m_env);
  m_env = std::make_shared<TypeEnvironment>(std::move(loopScope));

  TypeFlow flow = visitStmt(*stmt.body());
  m_env = std::move(prevEnv);

  return flow;
}

TypeFlow TypeChecker::visitForStmt(const bee::parser::ForStmt &stmt) {
  std::unique_ptr<Type> iteratorType = visitExpr(*stmt.iterator());
  if (iteratorType->kind() != TypeKind::Range) {
    // TODO: add type mismatch error diagnostic.
  }

  if (stmt.step() != nullptr) {
    std::unique_ptr<Type> stepType = visitExpr(*stmt.step());
    if (stepType->kind() != TypeKind::Int) {
      // TODO: add type mismatch error diagnostic.
    }
  }

  TypeEnvironment loopScope(TypeScopeKind::Block, m_env);
  std::shared_ptr<TypeEnvironment> prevEnv = std::move(m_env);
  m_env = std::make_shared<TypeEnvironment>(std::move(loopScope));

  TypeFlow flow = visitStmt(*stmt.body());
  m_env = std::move(prevEnv);

  return flow;
}

TypeFlow TypeChecker::visitInvalidStmt(const bee::parser::InvalidStmt &stmt) {
  return TypeFlow{.canContinue = true};
}

std::unique_ptr<Type>
TypeChecker::visitLiteralExpr(const bee::parser::LiteralExpr &expr) {
  switch (expr.value().kind()) {
  case TokenKind::NumberLit:
    return std::make_unique<Type>(Type::fromLexeme("int"));
  case TokenKind::StringLit:
    return std::make_unique<Type>(Type::fromLexeme("string"));
  case TokenKind::TrueKw:
  case TokenKind::FalseKw:
    return std::make_unique<Type>(Type::fromLexeme("bool"));
  case TokenKind::NullKw:
    return std::make_unique<Type>(Type::fromLexeme("null"));
  default:
    return std::make_unique<Type>(Type::invalid());
  }
}

std::unique_ptr<Type>
TypeChecker::visitIdentifierExpr(const bee::parser::IdentifierExpr &expr) {
  std::string_view name = expr.identifier().lexeme();
  if (!m_env->hasSymbol(name)) {
    // TODO: add undefined identifier error diagnostic.
    return std::make_unique<Type>(Type::invalid());
  }

  TypeSymbol symbol = m_env->getSymbol(expr.identifier().lexeme());
  return std::make_unique<Type>(std::move(symbol.type()));
}

std::unique_ptr<Type>
TypeChecker::visitAssignmentExpr(const bee::parser::AssignmentExpr &expr) {
  std::unique_ptr<Type> valueType = visitExpr(*expr.value());
  std::string_view name = expr.identifier().lexeme();
  if (!m_env->hasSymbol(name)) {
    // TODO: add undefined identifier error diagnostic.
    return valueType;
  }

  TypeSymbol symbol = m_env->getSymbol(name);
  if (symbol.kind() != TypeSymbolKind::Variable) {
    // TODO: add invalid assignment error diagnostic.
    return valueType;
  }

  VariableSymbol &variableSymbol = static_cast<VariableSymbol &>(symbol);
  if (variableSymbol.isConst()) {
    // TODO: add invalid assignment error diagnostic.
    return valueType;
  }

  if (!valueType->isAssignableTo(variableSymbol.type())) {
    // TODO: add type mismatch error diagnostic.
  }

  return valueType;
}

std::unique_ptr<Type>
TypeChecker::visitBinaryExpr(const bee::parser::BinaryExpr &expr) {
  std::unique_ptr<Type> leftType = visitExpr(*expr.left());
  std::unique_ptr<Type> rightType = visitExpr(*expr.right());
  if (!Type::isValidBinaryOperation(*leftType, expr.op().kind(), *rightType)) {
    // TODO: add unsuported binary operation error diagnostic.
    return std::make_unique<Type>(Type::invalid());
  }

  switch (expr.op().kind()) {
  case TokenKind::MinusSym:
  case TokenKind::PlusSym:
  case TokenKind::StarSym:
  case TokenKind::SlashSym:
  case TokenKind::PercentageSym:
    return std::make_unique<Type>(Type::fromLexeme("int"));

  case TokenKind::LtSym:
  case TokenKind::LteSym:
  case TokenKind::GtSym:
  case TokenKind::GteSym:
  case TokenKind::EqEqSym:
  case TokenKind::NeqSym:
  case TokenKind::AndKw:
  case TokenKind::OrKw:
    return std::make_unique<Type>(Type::fromLexeme("bool"));

  case TokenKind::DoubleDotSym:
    return std::make_unique<Type>(Type::range(std::move(*leftType)));

  default:
    return std::make_unique<Type>(Type::invalid());
  }
}

std::unique_ptr<Type>
TypeChecker::visitUnaryExpr(const bee::parser::UnaryExpr &expr) {
  std::unique_ptr<Type> operandType = visitExpr(*expr.operand());
  if (!Type::isValidUnaryOperation(expr.op().kind(), *operandType)) {
    // TODO: add unsupported unary operation error diagnostic.
  }

  return operandType;
}

std::unique_ptr<Type> TypeChecker::visitParenthesizedExpr(
    const bee::parser::ParenthesizedExpr &expr) {
  return visitExpr(*expr.expr());
}

std::unique_ptr<Type>
TypeChecker::visitWhenExpr(const bee::parser::WhenExpr &expr) {
  std::unique_ptr<Type> conditionType = visitExpr(*expr.condition());
  if (conditionType->kind() != TypeKind::Bool) {
    // TODO: add type mismatch error diagnostic.
  }

  std::unique_ptr<Type> consequentType = visitExpr(*expr.consequent());
  std::unique_ptr<Type> alternateType = visitExpr(*expr.alternate());

  if (!alternateType->isAssignableTo(*consequentType)) {
    // TODO: add type mismatch error diagnostic.
  }

  return consequentType;
}

std::unique_ptr<Type>
TypeChecker::visitCallExpr(const bee::parser::CallExpr &expr) {
  std::string_view functionName = expr.identifier().lexeme();
  if (!m_env->hasSymbol(functionName)) {
    // TODO: add undefined identifier error diagnostic.
    return std::make_unique<Type>(Type::invalid());
  }

  TypeSymbol symbol = m_env->getSymbol(functionName);
  if (symbol.kind() != TypeSymbolKind::Function) {
    // TODO: add non function call error diagnostic.
    return std::make_unique<Type>(Type::invalid());
  }

  FunctionSymbol &functionSymbol = static_cast<FunctionSymbol &>(symbol);
  Type &functionType = functionSymbol.type();
  FunctionInfo &functionInfo =
      const_cast<FunctionInfo &>(std::get<FunctionInfo>(functionType.info()));

  std::unique_ptr<Type> returnType = std::move(functionInfo.returnType);
  const std::vector<std::unique_ptr<bee::parser::Expr>> &arguments =
      expr.arguments();
  
  if (functionSymbol.arity() != arguments.size()) {
    // TODO: add invalid function arity error diagnostic.
    return returnType;
  }


  const std::vector<Type> &paramsTypes = functionInfo.params;
  for (std::size_t i = 0; i < functionSymbol.arity(); i++) {
    const Type &paramType = paramsTypes[i];
    const std::unique_ptr<bee::parser::Expr> &argumentExpr = arguments[i];
    std::unique_ptr<Type> argumentType = visitExpr(*argumentExpr);
    if (!argumentType->isAssignableTo(paramType)) {
      // TODO: add type mismatch error diagnostic.
    }
  }

  return returnType;  
}

std::unique_ptr<Type>
TypeChecker::visitInvalidExpr(const bee::parser::InvalidExpr &expr) {
  return std::make_unique<Type>(Type::invalid());
}

} // namespace bee::typechecker
