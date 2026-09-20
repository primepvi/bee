#ifndef BEE_TYPE_HPP
#define BEE_TYPE_HPP

#include "bee/lexer/Token.hpp"
#include "bee/parser/Ast.hpp"
#include <string_view>
#include <variant>

namespace bee::typechecker {

enum class TypeKind {
  Int,
  UInt,
  Range,
  String,
  Function,
  Void,
  Null,
  Invalid,
};

class Type;

struct FunctionInfo {
  std::vector<Type> params;
  std::unique_ptr<Type> returnType;
};

struct RangeInfo {
  std::unique_ptr<Type> type;
};

using TypeData = std::variant<std::monostate, RangeInfo, FunctionInfo>;

class Type {
public:
  Type(TypeKind kind, bool nullable);
  static Type fromLexeme(std::string_view lexeme);
  static Type fromAnnotation(bee::parser::TypeAnnotation annotation);

  static Type function(std::vector<Type> paramsTypes, Type returnType);
  static Type invalid();
  static Type empty();

  static bool isValidBinaryOperation(const Type &left, bee::lexer::TokenKind op,
                                     const Type &right);
  static bool isValidUnaryOperation(bee::lexer::TokenKind op,
                                    const Type &operand);

  std::string toString() const;
  const TypeData &data() const;

  bool isEmpty() const;
  bool isInvalid() const;
  bool isEqual(const Type &other) const;
  bool isAssignableTo(const Type &other) const;

private:
  TypeKind m_kind;
  bool m_nullable;
  TypeData m_data;
};

} // namespace bee::typechecker

#endif // BEE_TYPE_HPP
