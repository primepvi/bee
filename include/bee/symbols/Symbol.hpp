#ifndef BEE_SYMBOL_HPP
#define BEE_SYMBOL_HPP

#include <string>
#include <vector>
#include <variant>

#include "bee/symbols/Value.hpp"
#include "bee/typechecker/Type.hpp"

namespace bee::symbols {

using SymbolId = std::size_t;

enum class SymbolKind {
  Function,
  Variable,
  Param,
  Type,
  Value,
};

struct FunctionSymbolInfo {
  SymbolId typeId;
  std::vector<SymbolId> paramsIds;
};

struct VariableSymbolInfo {
  bool constant;
  bool lit;
  
  SymbolId valueId;
  SymbolId typeId;
};

struct ParamSymbolInfo {
  bool lit;
  SymbolId typeId;
};

struct TypeSymbolInfo {
  bee::typechecker::Type type;
};

struct ValueSymbolInfo {
  Value value;
}; 

using SymbolInfo =
    std::variant<FunctionSymbolInfo, VariableSymbolInfo, ParamSymbolInfo,
                 TypeSymbolInfo, ValueSymbolInfo>;

struct Symbol {
  SymbolId id;
  SymbolKind kind;
  SymbolInfo info;
  std::string name;
};

} // namespace bee::symbols

#endif // BEE_SYMBOL_HPP
