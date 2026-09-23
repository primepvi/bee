#include <algorithm>
#include <array>
#include <format>
#include <memory>

#include "bee/lexer/Token.hpp"
#include "bee/parser/Ast.hpp"
#include "bee/typechecker/Type.hpp"

namespace bee::typechecker {

using bee::lexer::TokenKind;
using bee::parser::TypeAnnotation;

constexpr std::array typeEntries = std::to_array<TypeEntry>({
    {"int", TypeKind::Int},
    {"uint", TypeKind::UInt},
    {"byte", TypeKind::Byte},
    {"ubyte", TypeKind::UByte},
    {"float", TypeKind::Float},
    {"bool", TypeKind::Bool},
    {"string", TypeKind::String},
    {"char", TypeKind::Char},
    {"Range", TypeKind::Range},
    {"Function", TypeKind::Function},
    {"void", TypeKind::Void},
    {"null", TypeKind::Null},
    {"invalid", TypeKind::Invalid},
});

Type::Type(TypeKind kind, bool nullable)
    : m_kind(kind), m_nullable(nullable), m_info(std::monostate{}) {}
Type::Type(TypeKind kind, bool nullable, TypeInfo info)
    : m_kind(kind), m_nullable(nullable), m_info(std::move(info)) {}

Type Type::fromLexeme(std::string_view lexeme) {
  auto it = std::find_if(
      typeEntries.begin(), typeEntries.end(),
      [lexeme](const TypeEntry &entry) { return entry.lexeme == lexeme; });
  TypeKind kind = it == typeEntries.end() ? TypeKind::Invalid : it->kind;
  return Type(kind, false);
}

Type Type::fromAnnotation(TypeAnnotation annotation) {
  std::string_view lexeme = annotation.identifier.lexeme();
  auto it = std::find_if(
      typeEntries.begin(), typeEntries.end(),
      [lexeme](const TypeEntry &entry) { return entry.lexeme == lexeme; });

  TypeKind kind = it == typeEntries.end() ? TypeKind::Invalid : it->kind;
  return Type(kind, annotation.nullable);
}

Type Type::function(std::vector<Type> paramsTypes, Type returnType) {
  FunctionInfo info = {
      .params = std::move(paramsTypes),
      .returnType = std::make_unique<Type>(std::move(returnType)),
  };

  return Type(TypeKind::Function, false, std::move(info));
}

Type Type::range(Type type) {
  RangeInfo info = {
      .type = std::make_unique<Type>(std::move(type)),
  };

  return Type(TypeKind::Range, false, std::move(info));
}

Type Type::invalid() { return Type(TypeKind::Invalid, false); }
Type Type::empty() { return Type(TypeKind::Void, false); }

bool Type::isValidBinaryOperation(const Type &left, bee::lexer::TokenKind op,
                                  const Type &right) {
  switch (op) {
  case TokenKind::MinusSym:
  case TokenKind::PlusSym:
  case TokenKind::StarSym:
  case TokenKind::SlashSym:
  case TokenKind::PercentageSym:
  case TokenKind::LtSym:
  case TokenKind::LteSym:
  case TokenKind::GtSym:
  case TokenKind::GteSym:
    return left.isNumeric() && right.isNumeric() && left.kind() == right.kind() &&
           !left.isNullable() && !right.isNullable();
    
  case TokenKind::DoubleDotSym:
    return left.kind() == TypeKind::Int && right.kind() == TypeKind::Int &&
           !left.isNullable() && !right.isNullable();

  case TokenKind::EqEqSym:
  case TokenKind::NeqSym:
    return !(left.isEmpty() || left.isInvalid()) &&
           !(right.isEmpty() || right.isInvalid());

  case TokenKind::AndKw:
  case TokenKind::OrKw:
    return left.kind() == TypeKind::Bool && right.kind() == TypeKind::Bool &&
           !left.isNullable() && !right.isNullable();

  default:
    return false;
  }
}

bool Type::isValidUnaryOperation(TokenKind op, const Type &operand) {
  switch (op) {
  case TokenKind::MinusSym:
  case TokenKind::PlusSym:
    return operand.isNumeric() && !operand.isNullable();

  case TokenKind::NotKw:
    return operand.kind() == TypeKind::Bool && !operand.isNullable();

  default:
    return false;
  }
}

std::string Type::toString() const {
  TypeKind kind = m_kind;
  auto it = std::find_if(
      typeEntries.begin(), typeEntries.end(),
      [kind](const TypeEntry &entry) { return kind == entry.kind; });

  std::string name =
      it == typeEntries.end() ? "invalid" : std::string(it->lexeme);

  if (kind == TypeKind::Function) {
    const FunctionInfo &info = std::get<FunctionInfo>(m_info);
    name += "[";

    std::size_t count = 0;
    for (const auto &param : info.params) {
      name += param.toString();
      count++;

      if (count < info.params.size())
        name += ",";
    }

    name += "]: " + info.returnType->toString();
  } else if (kind == TypeKind::Range) {
    const RangeInfo &info = std::get<RangeInfo>(m_info);
    name += "[" + info.type->toString() + "]";
  }

  std::string suffix = m_nullable ? "?" : "";

  return std::format("{}{}", name, suffix);
}

bool Type::isAssignableTo(const Type &other) const {
  if (m_kind == TypeKind::Null)
    return other.isNullable() || other.kind() == TypeKind::Null;

  if (this->isEmpty() || this->isInvalid() || other.isEmpty() ||
      other.isInvalid())
    return false;

  if (m_kind != other.kind())
    return false;

  if (m_nullable && !other.m_nullable)
    return false;

  switch (m_kind) {
  case TypeKind::Function: {
    const FunctionInfo &info = std::get<FunctionInfo>(m_info);
    const FunctionInfo &otherInfo = std::get<FunctionInfo>(other.info());

    if (info.params.size() != otherInfo.params.size())
      return false;

    if (!info.returnType->isEqual(*otherInfo.returnType))
      return false;

    for (std::size_t i = 0; i < info.params.size(); i++) {
      if (!info.params[i].isEqual(otherInfo.params[i]))
        return false;
    }

    return true;
  }

  case TypeKind::Range: {
    const RangeInfo &info = std::get<RangeInfo>(m_info);
    const RangeInfo &otherInfo = std::get<RangeInfo>(other.info());
    return info.type->isEqual(*otherInfo.type);
  }

  default:
    return true;
  }
}

} // namespace bee::typechecker
