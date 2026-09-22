#ifndef BEE_TYPE_SYMBOL_HPP
#define BEE_TYPE_SYMBOL_HPP

#include <string_view>
#include "bee/typechecker/Type.hpp"

namespace bee::typechecker {

enum class TypeSymbolKind {
  Variable,
  Function,
};

class TypeSymbol {
public:
  virtual ~TypeSymbol() = default;
  virtual std::string_view name() const = 0;
  virtual TypeSymbolKind kind() const = 0;
  virtual Type &type() = 0;
};

class VariableSymbol : public TypeSymbol {
public:
  VariableSymbol(std::string_view name, bool constant, Type type);

  inline std::string_view name() const override { return m_name; }
  inline TypeSymbolKind kind() const override {
    return TypeSymbolKind::Variable;
  }

  inline Type &type() override { return m_type; }
  inline bool isConst() const { return m_constant; }

private:
  std::string_view m_name;
  bool m_constant;
  Type m_type;
};

class FunctionSymbol : public TypeSymbol {
public:
  FunctionSymbol(std::string_view name, std::size_t arity, Type type);
  
  inline std::string_view name() const override { return m_name; }
  inline TypeSymbolKind kind() const override {
    return TypeSymbolKind::Function;
  }

  inline std::size_t arity() const { return m_arity; }
  inline Type &type() override { return m_type; }

private:
  std::string_view m_name;
  std::size_t m_arity;
  Type m_type;
};

} // namespace bee::typechecker

#endif // BEE_TYPE_SYMBOL_HPP
