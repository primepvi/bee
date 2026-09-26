#include "bee/symbols/SymbolTable.hpp"
#include "bee/symbols/Scope.hpp"
#include <optional>

namespace bee::symbols {

SymbolId SymbolTable::putSymbol(ScopeId scopeId, std::string name,
                                SymbolKind kind, SymbolInfo info) {
  SymbolId id = m_symbols.size();
  Symbol symbol = {
    .id = id,
    .kind = kind,
    .info = info,
    .name = name,
  };

  m_symbols.push_back(symbol);

  Scope scope = m_scopes[scopeId];
  scope.symbols.insert_or_assign(symbol.name, symbol);

  return id;
}

ScopeId SymbolTable::putScope(std::optional<ScopeId> parentId, ScopeKind kind) {  
  ScopeId id = m_scopes.size();
  Scope scope = { .id = id, .parentId = parentId, .kind = kind };
  m_scopes.push_back(scope);

  return id;
}

std::optional<SymbolId> SymbolTable::lookup(ScopeId scopeId,
                                            const std::string &name) const {
  Scope scope = m_scopes[scopeId];
  if (scope.symbols.contains(name))
    return scope.symbols.at(name);

  return std::nullopt;
}



} // namespace bee::symbols
