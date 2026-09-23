#include "bee/typechecker/TypeChecker.hpp"
#include "bee/Diagnostics.hpp"
#include "bee/lexer/Token.hpp"
#include "bee/parser/Ast.hpp"
#include "bee/typechecker/TypeEnvironment.hpp"
#include "bee/typechecker/TypeSymbol.hpp"

#include <array>
#include <format>
#include <memory>

namespace bee::typechecker {

using bee::DiagnosticCode;
using bee::DiagnosticLevel;
using bee::lexer::TokenKind;

TypeChecker::TypeChecker(const bee::parser::Program &program,
                         bee::DiagnosticBag &bag)
    : m_program(program), m_bag(bag),
      m_scopeReturnType(Type(TypeKind::Void, false)) {

  TypeEnvironment globalEnv(TypeScopeKind::Global, nullptr);

  m_env = std::make_shared<TypeEnvironment>(std::move(globalEnv));
}

void TypeChecker::typecheck() {
  for (const auto &stmt : m_program) {
    visitStmt(*stmt);
  }
}

TypeFlow TypeChecker::visitVariableDeclarationStmt(
    const bee::parser::VariableDeclarationStmt &stmt) {

  std::string_view variableName = stmt.identifier().lexeme();
  if (m_env->scopeHasSymbol(variableName)) {
    m_bag.report(DiagnosticLevel::Error,
                 DiagnosticCode::IdentifierAlreadyDeclared,
                 stmt.identifier().span(), std::make_format_args(variableName));
    return TypeFlow{.canContinue = true};
  }

  std::unique_ptr<Type> valueType = visitExpr(*stmt.value());
  std::unique_ptr<Type> variableType = std::make_unique<Type>(Type::invalid());

  if (stmt.typeAnnotation() != std::nullopt) {
    const bee::parser::TypeAnnotation annotation =
        stmt.typeAnnotation().value();
    Type annotationType = Type::fromAnnotation(annotation);

    if (annotationType.isEmpty() || annotationType.isInvalid()) {

      m_bag.report(DiagnosticLevel::Error,
                   DiagnosticCode::InvalidTypeAnnotation, annotation.span,
                   std::make_format_args());
    } else if (!valueType->isAssignableTo(annotationType)) {
      std::string annotationTypeString = annotationType.toString();
      std::string valueTypeString = valueType->toString();

      m_bag.report(
          DiagnosticLevel::Error, DiagnosticCode::TypeMismatch,
          stmt.value()->span(),
          std::make_format_args(annotationTypeString, valueTypeString));
    } else {
      variableType = std::make_unique<Type>(std::move(annotationType));
    }
  } else {
    variableType = std::move(valueType);
  }

  if (variableType->isEmpty()) {
    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::InvalidVoidUsage,
                 stmt.span(), std::make_format_args());
    variableType = std::make_unique<Type>(Type::invalid());
  }

  bool isConstant = stmt.keyword().kind() == bee::lexer::TokenKind::ConstKw;
  VariableSymbol variable(variableName, isConstant, std::move(*variableType));

  m_env->putSymbol(std::make_unique<VariableSymbol>(std::move(variable)));
  return TypeFlow{.canContinue = true};
}

TypeFlow TypeChecker::visitFunctionDeclarationStmt(
    const bee::parser::FunctionDeclarationStmt &stmt) {
  if (m_env->scopeKind() != TypeScopeKind::Global) {
    m_bag.report(DiagnosticLevel::Error,
                 DiagnosticCode::InvalidFunctionDeclaration, stmt.span(),
                 std::make_format_args());
  }

  std::string_view functionName = stmt.identifier().lexeme();
  if (m_env->scopeHasSymbol(functionName)) {
    m_bag.report(DiagnosticLevel::Error,
                 DiagnosticCode::IdentifierAlreadyDeclared,
                 stmt.identifier().span(), std::make_format_args(functionName));
    return TypeFlow{.canContinue = true};
  }

  std::vector<Type> paramTypes;
  TypeEnvironment functionEnv(TypeScopeKind::Function, m_env);
  for (const auto &param : stmt.params()) {
    Type paramType = Type::fromAnnotation(param.typeAnnotation);
    if (paramType.isEmpty() || paramType.isInvalid()) {
      m_bag.report(DiagnosticLevel::Error,
                   DiagnosticCode::InvalidTypeAnnotation,
                   param.typeAnnotation.span, std::make_format_args());
    }

    paramTypes.push_back(paramType);
    VariableSymbol paramSymbol(param.identifier.lexeme(), true,
                               std::move(paramType));
    functionEnv.putSymbol(
        std::make_unique<VariableSymbol>(std::move(paramSymbol)));
  }

  Type returnType = Type::fromAnnotation(stmt.typeAnnotation());
  if (returnType.isInvalid()) {
    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::InvalidTypeAnnotation,
                 stmt.typeAnnotation().span, std::make_format_args());
  }

  Type functionType = Type::function(std::move(paramTypes), returnType);
  FunctionSymbol functionSymbol(functionName, stmt.params().size(),
                                std::move(functionType));
  functionEnv.putSymbol(std::make_unique<FunctionSymbol>(functionSymbol));
  m_env->putSymbol(std::make_unique<FunctionSymbol>(std::move(functionSymbol)));

  Type prevScopeReturnType = std::move(m_scopeReturnType);
  m_scopeReturnType = returnType;

  std::shared_ptr<TypeEnvironment> prevEnv = std::move(m_env);
  m_env = std::make_shared<TypeEnvironment>(std::move(functionEnv));

  if (stmt.body()->kind() == bee::parser::StmtKind::Block) {
    TypeFlow flow = visitStmt(*stmt.body());

    if (!returnType.isEmpty() && flow.canContinue) {
      m_bag.report(DiagnosticLevel::Error, DiagnosticCode::VoidControlPaths,
                   stmt.span(), std::make_format_args());
    }
  } else {
    auto &body = static_cast<bee::parser::ExprStmt &>(*stmt.body());
    std::unique_ptr<Type> exprType = visitExpr(*body.expr());

    if (!exprType->isAssignableTo(returnType)) {
      std::string returnTypeString = returnType.toString();
      std::string exprTypeString = exprType->toString();

      m_bag.report(DiagnosticLevel::Error, DiagnosticCode::TypeMismatch,
                   stmt.span(),
                   std::make_format_args(returnTypeString, exprTypeString));
    }
  }

  m_env = std::move(prevEnv);
  m_scopeReturnType = std::move(prevScopeReturnType);

  return TypeFlow{.canContinue = true};
}

TypeFlow TypeChecker::visitReturnStmt(const bee::parser::ReturnStmt &stmt) {
  std::shared_ptr<TypeEnvironment> functionScope = m_env;
  while (functionScope->parent() != nullptr &&
         functionScope->scopeKind() != TypeScopeKind::Function) {
    functionScope = functionScope->parent();
  }

  if (functionScope->scopeKind() != TypeScopeKind::Function) {
    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::InvalidReturnUsage,
                 stmt.span(), std::make_format_args());
    return TypeFlow{.canContinue = true};
  }

  std::unique_ptr<Type> returnType = stmt.expr() == nullptr
                                         ? std::make_unique<Type>(Type::empty())
                                         : visitExpr(*stmt.expr());
  if (!returnType->isAssignableTo(m_scopeReturnType)) {
    std::string scopeReturnTypeString = m_scopeReturnType.toString();
    std::string returnTypeString = returnType->toString();

    m_bag.report(
        DiagnosticLevel::Error, DiagnosticCode::TypeMismatch, stmt.span(),
        std::make_format_args(scopeReturnTypeString, returnTypeString));
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
    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::InvalidVoidUsage,
                 stmt.message()->span(), std::make_format_args());
  }

  return TypeFlow{.canContinue = true};
}

TypeFlow TypeChecker::visitIfStmt(const bee::parser::IfStmt &stmt) {
  std::unique_ptr<Type> conditionType = visitExpr(*stmt.condition());
  if (conditionType->kind() != TypeKind::Bool) {
    std::string conditionTypeString = conditionType->toString();

    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::TypeMismatch,
                 stmt.condition()->span(),
                 std::make_format_args("bool", conditionTypeString));
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
  if (stmt.captureAnnotation() != std::nullopt) {
    bee::parser::BlockCaptureAnnotation annotation =
        stmt.captureAnnotation().value();

    if (m_env->scopeKind() == TypeScopeKind::ForLoop) {
      auto forCapturables = std::to_array<Type>({
          Type::fromLexeme("int"),
      });

      if (annotation.captures.size() > forCapturables.size()) {
	std::size_t capturablesSize = forCapturables.size();
        std::size_t capturesSize = annotation.captures.size();
	
	m_bag.report(DiagnosticLevel::Error, DiagnosticCode::InvalidBlockCaptureCount, stmt.span(), std::make_format_args("for statement scope", capturablesSize, capturesSize));
      }

      for (std::size_t i = 0; i < annotation.captures.size(); i++) {
        bee::lexer::Token captureToken = annotation.captures[i];
        Type capturableType =
            i < forCapturables.size() ? forCapturables[i] : Type::invalid();
        
        VariableSymbol captureSymbol(captureToken.lexeme(), true,
                                     capturableType);
        blockEnv.putSymbol(
            std::make_unique<VariableSymbol>(std::move(captureSymbol)));
      }
    } else {
      m_bag.report(DiagnosticLevel::Error,
                   DiagnosticCode::InvalidBlockCaptureScope, stmt.span(),
                   std::make_format_args());
      
      for (std::size_t i = 0; i < annotation.captures.size(); i++) {
        bee::lexer::Token captureToken = annotation.captures[i];
        VariableSymbol captureSymbol(captureToken.lexeme(), true,
                                     Type::invalid());
        blockEnv.putSymbol(
            std::make_unique<VariableSymbol>(std::move(captureSymbol)));
      }
    }
  }

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
    std::string conditionTypeString = conditionType->toString();

    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::TypeMismatch,
                 stmt.condition()->span(),
                 std::make_format_args("bool", conditionTypeString));
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
    std::string iteratorTypeString = iteratorType->toString();

    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::TypeMismatch,
                 stmt.iterator()->span(),
                 std::make_format_args("range<int>", iteratorTypeString));
  }

  if (stmt.step() != nullptr) {
    std::unique_ptr<Type> stepType = visitExpr(*stmt.step());
    if (stepType->kind() != TypeKind::Int) {
      std::string stepTypeString = stepType->toString();

      m_bag.report(DiagnosticLevel::Error, DiagnosticCode::TypeMismatch,
                   stmt.step()->span(),
                   std::make_format_args("int", stepTypeString));
    }
  }

  TypeEnvironment loopScope(TypeScopeKind::ForLoop, m_env);
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
  case TokenKind::CharLit:
    return std::make_unique<Type>(Type::fromLexeme("char"));
  default:
    return std::make_unique<Type>(Type::invalid());
  }
}

std::unique_ptr<Type>
TypeChecker::visitIdentifierExpr(const bee::parser::IdentifierExpr &expr) {
  std::string_view name = expr.identifier().lexeme();
  if (!m_env->hasSymbol(name)) {
    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::UndefinedIdentifier,
                 expr.identifier().span(), std::make_format_args(name));
    return std::make_unique<Type>(Type::invalid());
  }

  std::shared_ptr<TypeSymbol> symbol =
      m_env->getSymbol(expr.identifier().lexeme());
  return std::make_unique<Type>(std::move(symbol->type()));
}

std::unique_ptr<Type>
TypeChecker::visitAssignmentExpr(const bee::parser::AssignmentExpr &expr) {
  std::unique_ptr<Type> valueType = visitExpr(*expr.value());
  std::string_view name = expr.identifier().lexeme();

  if (!m_env->hasSymbol(name)) {
    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::UndefinedIdentifier,
                 expr.identifier().span(), std::make_format_args(name));
    return valueType;
  }

  std::shared_ptr<TypeSymbol> symbol = m_env->getSymbol(name);
  if (symbol->kind() != TypeSymbolKind::Variable) {
    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::InvalidAssignment,
                 expr.span(), std::make_format_args("non-variable identifier"));
    return valueType;
  }

  VariableSymbol &variableSymbol = static_cast<VariableSymbol &>(*symbol);
  if (variableSymbol.isConst()) {
    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::InvalidAssignment,
                 expr.span(), std::make_format_args("constant variable"));
    return valueType;
  }

  if (!valueType->isAssignableTo(variableSymbol.type())) {
    std::string variableTypeString = variableSymbol.type().toString();
    std::string valueTypeString = valueType->toString();

    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::TypeMismatch,
                 expr.value()->span(),
                 std::make_format_args(variableTypeString, valueTypeString));
  }

  return valueType;
}

std::unique_ptr<Type>
TypeChecker::visitBinaryExpr(const bee::parser::BinaryExpr &expr) {
  std::unique_ptr<Type> leftType = visitExpr(*expr.left());
  std::unique_ptr<Type> rightType = visitExpr(*expr.right());

  if (leftType->isInvalid() || rightType->isInvalid())
    return std::make_unique<Type>(Type::invalid());

  if (!Type::isValidBinaryOperation(*leftType, expr.op().kind(), *rightType)) {
    std::string leftTypeString = leftType->toString();
    std::string rightTypeString = rightType->toString();
    std::string_view op = expr.op().lexeme();

    m_bag.report(DiagnosticLevel::Error,
                 DiagnosticCode::UnsupporetedBinaryOperation, expr.span(),
                 std::make_format_args(op, leftTypeString, rightTypeString));

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
    std::string_view op = expr.op().lexeme();
    std::string operandTypeString = operandType->toString();

    m_bag.report(DiagnosticLevel::Error,
                 DiagnosticCode::UnsupportedUnaryOperation, expr.span(),
                 std::make_format_args(op, operandTypeString));
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
    std::string conditionTypeString = conditionType->toString();

    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::TypeMismatch,
                 expr.condition()->span(),
                 std::make_format_args("bool", conditionTypeString));
  }

  std::unique_ptr<Type> consequentType = visitExpr(*expr.consequent());
  std::unique_ptr<Type> alternateType = visitExpr(*expr.alternate());

  if (!alternateType->isAssignableTo(*consequentType)) {
    std::string consequentTypeString = consequentType->toString();
    std::string alternateTypeString = alternateType->toString();

    m_bag.report(
        DiagnosticLevel::Error, DiagnosticCode::TypeMismatch, expr.span(),
        std::make_format_args(consequentTypeString, alternateTypeString));
  }

  return consequentType;
}

std::unique_ptr<Type>
TypeChecker::visitCallExpr(const bee::parser::CallExpr &expr) {
  std::string_view functionName = expr.identifier().lexeme();

  if (!m_env->hasSymbol(functionName)) {
    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::UndefinedIdentifier,
                 expr.identifier().span(), std::make_format_args(functionName));

    return std::make_unique<Type>(Type::invalid());
  }

  std::shared_ptr<TypeSymbol> symbol = m_env->getSymbol(functionName);

  if (symbol->kind() != TypeSymbolKind::Function) {
    m_bag.report(DiagnosticLevel::Error, DiagnosticCode::NonFunctionCall,
                 expr.span(), std::make_format_args());

    return std::make_unique<Type>(Type::invalid());
  }

  FunctionSymbol &functionSymbol = static_cast<FunctionSymbol &>(*symbol);
  Type &functionType = functionSymbol.type();

  FunctionInfo &functionInfo =
      const_cast<FunctionInfo &>(std::get<FunctionInfo>(functionType.info()));

  std::unique_ptr<Type> functionReturnType =
      std::make_unique<Type>(std::move(*functionInfo.returnType));

  const std::vector<std::unique_ptr<bee::parser::Expr>> &arguments =
      expr.arguments();

  if (functionSymbol.arity() != arguments.size()) {
    std::size_t functionArity = functionSymbol.arity();
    std::size_t argumentsSize = arguments.size();

    m_bag.report(
        DiagnosticLevel::Error, DiagnosticCode::InvalidFunctionCallArity,
        expr.span(),
        std::make_format_args(functionName, functionArity, argumentsSize));

    return functionReturnType;
  }

  const std::vector<Type> &paramsTypes = functionInfo.params;
  for (std::size_t i = 0; i < functionSymbol.arity(); i++) {
    const auto &paramType = paramsTypes[i];
    const auto &argumentExpr = arguments[i];

    std::unique_ptr<Type> argumentType = visitExpr(*argumentExpr);
    if (argumentType->isInvalid() || paramType.isInvalid())
      continue;

    if (!argumentType->isAssignableTo(paramType)) {
      std::string paramTypeString = paramType.toString();
      std::string argumentTypeString = argumentType->toString();

      m_bag.report(DiagnosticLevel::Error, DiagnosticCode::TypeMismatch,
                   argumentExpr->span(),
                   std::make_format_args(paramTypeString, argumentTypeString));
    }
  }

  return functionReturnType;
}

std::unique_ptr<Type>
TypeChecker::visitInvalidExpr(const bee::parser::InvalidExpr &expr) {
  return std::make_unique<Type>(Type::invalid());
}

} // namespace bee::typechecker
