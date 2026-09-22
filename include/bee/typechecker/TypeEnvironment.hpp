#ifndef BEE_TYPE_ENVIRONMENT_HPP
#define BEE_TYPE_ENVIRONMENT_HPP

#include "bee/typechecker/TypeSymbol.hpp"
#include <unordered_map>

namespace bee::typechecker {

enum class TypeScopeKind {
  Global,
  Function,
  Block,
  ForLoop,
};

class TypeEnvironment {
public:
  TypeEnvironment(TypeScopeKind scopeKind,
                  std::shared_ptr<TypeEnvironment> parent);

  bool hasSymbol(std::string_view key) const;
  bool scopeHasSymbol(std::string_view key) const;

  std::shared_ptr<TypeSymbol> getSymbol(std::string_view key) const;
  std::shared_ptr<TypeSymbol> scopeGetSymbol(std::string_view key) const;

  void putSymbol(std::unique_ptr<TypeSymbol> symbol);

  inline TypeScopeKind scopeKind() { return m_scopeKind; }
  inline std::shared_ptr<TypeEnvironment> parent() { return m_parent; }
  
private:
  TypeScopeKind m_scopeKind;
  std::shared_ptr<TypeEnvironment> m_parent;
  std::unordered_map<std::string_view, std::shared_ptr<TypeSymbol>> m_symbols;
};

} // namespace bee::typechecker

#endif // BEE_TYPE_ENVIRONMENT_HPP
