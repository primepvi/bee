#ifndef BEE_SCOPE_HPP
#define BEE_SCOPE_HPP

#include <unordered_map>
#include <string>
#include <optional>

#include "bee/symbols/Symbol.hpp"

namespace bee::symbols {

using ScopeId = std::size_t;

enum class ScopeKind {
  Global,
  Function,
  Block,
  ForLoop,
};

struct Scope {
  ScopeId id;
  std::optional<ScopeId> parentId;
  ScopeKind kind;

  std::unordered_map<std::string, SymbolId> symbols;
};

} // namespace bee::symbols

#endif // BEE_SCOPE_HPP
