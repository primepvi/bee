#ifndef BEE_TYPE_ENVIRONMENT_HPP
#define BEE_TYPE_ENVIRONMENT_HPP

#include "bee/typechecker/TypeSymbol.hpp"
#include <unordered_map>

namespace bee::typechecker {

enum class TypeScopeKind {
  Global,
  Function,
  Block,
};

class TypeEnvironment {
public:
  TypeEnvironment(TypeScopeKind scopeKind,
                  std::unique_ptr<TypeEnvironment> parent);

  bool hasSymbol(std::string_view key) const;
  bool scopeHasSymbol(std::string_view key) const;

  const TypeSymbol &getSymbol(std::string_view key) const;
  const TypeSymbol &scopeGetSymbol(std::string_view key) const;

private:
  TypeScopeKind m_scopeKind;
  std::unique_ptr<TypeEnvironment> m_parent;
  std::unordered_map<std::string_view, TypeSymbol> m_symbols;
};

} // namespace bee::typechecker

#endif // BEE_TYPE_ENVIRONMENT_HPP
