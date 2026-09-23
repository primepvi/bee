#ifndef BEE_VALUE_ENVIRONMENT_HPP
#define BEE_VALUE_ENVIRONMENT_HPP

#include "bee/parser/Ast.hpp"
#include <cstdint>
#include <string_view>
#include <variant>
#include <unordered_map>
#include <memory>

namespace bee::interpreter {

struct FunctionValue {
  std::string_view identifier;
  std::vector<std::string_view> params;
  const std::unique_ptr<bee::parser::Stmt>& body;
};

struct RangeValue {
  std::int64_t start;
  std::int64_t end;
};

using Value = std::variant<std::int64_t, bool, std::string_view, std::monostate,
                           FunctionValue, RangeValue>;

enum class ValueScopeKind {
  Global,
  Function,
  Block,
  ForLoop,
};  

class ValueEnvironment {
public:
    ValueEnvironment(ValueScopeKind scopeKind,
                  std::shared_ptr<ValueEnvironment> parent);

  bool hasValue(std::string_view key) const;
  bool scopeHasValue(std::string_view key) const;

  Value getValue(std::string_view key) const;
  Value scopeGetValue(std::string_view key) const;

  void putValue(std::string_view key, Value value);

  inline ValueScopeKind scopeKind() { return m_scopeKind; }
  inline std::shared_ptr<ValueEnvironment> parent() { return m_parent; }
  
private:
  ValueScopeKind m_scopeKind;
  std::shared_ptr<ValueEnvironment> m_parent;
  std::unordered_map<std::string_view, Value> m_values;
};  

} // namespace bee::interpreter

#endif // BEE_VALUE_ENVIRONMENT_HPP
