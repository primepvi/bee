#ifndef BEE_SYMBOL_TABLE_HPP
#define BEE_SYMBOL_TABLE_HPP

#include <string>
#include <optional>
#include <vector>

#include "bee/symbols/Scope.hpp"
#include "bee/symbols/Symbol.hpp"

namespace bee::symbols {

class SymbolTable {
public:
  SymbolId putSymbol(ScopeId scopeId, std::string name, SymbolKind kind,
                     SymbolInfo info);
  ScopeId putScope(std::optional<ScopeId> parentId, ScopeKind kind);

  inline Symbol &getSymbol(SymbolId id) { return m_symbols[id]; }
  inline Scope &getScope(ScopeId id) { return m_scopes[id]; }

  std::optional<SymbolId> lookup(ScopeId scopeId,
                                 const std::string &name) const;
private:
  std::vector<Symbol> m_symbols;
  std::vector<Scope> m_scopes;
};

} // namespace bee::symbols

#endif // BEE_SYMBOL_TABLE_HPP
