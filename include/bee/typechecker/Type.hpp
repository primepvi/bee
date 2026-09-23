#ifndef BEE_TYPE_HPP
#define BEE_TYPE_HPP

#include <string_view>
#include <variant>

#include "bee/lexer/Token.hpp"
#include "bee/parser/Ast.hpp"

namespace bee::typechecker {

enum class TypeKind {
  Int,
  UInt,
  Bool,
  String,
  Char,
  Range,
  Function,
  Void,
  Null,
  Invalid,
};

struct TypeEntry {
  std::string_view lexeme;
  TypeKind kind;
};

class Type;

struct FunctionInfo {
  std::vector<Type> params;
  std::shared_ptr<const Type> returnType;
};

struct RangeInfo {
  std::shared_ptr<const Type> type;
};

using TypeInfo = std::variant<std::monostate, RangeInfo, FunctionInfo>;

class Type {
public:
  Type(TypeKind kind, bool nullable);
  Type(TypeKind kind, bool nullable, TypeInfo info);
  static Type fromLexeme(std::string_view lexeme);
  static Type fromAnnotation(bee::parser::TypeAnnotation annotation);

  static Type function(std::vector<Type> paramsTypes, Type returnType);
  static Type range(Type type);
  static Type invalid();
  static Type empty();

  static bool isValidBinaryOperation(const Type &left, bee::lexer::TokenKind op,
                                     const Type &right);
  static bool isValidUnaryOperation(bee::lexer::TokenKind op,
                                    const Type &operand);

  std::string toString() const;
  
  bool isAssignableTo(const Type &other) const;
  
  inline TypeKind kind() const { return m_kind; }
  inline const TypeInfo &info() const { return m_info; }
  inline bool nullable() const { return m_nullable; }
  
  inline bool isEmpty() const { return m_kind == TypeKind::Void; }
  inline bool isInvalid() const { return m_kind == TypeKind::Invalid; }

  inline bool isEqual(const Type &other) const {
    return this->toString() == other.toString();
  }
  
private:
  TypeKind m_kind;
  bool m_nullable;
  TypeInfo m_info;
};

} // namespace bee::typechecker

#endif // BEE_TYPE_HPP
