#ifndef BEE_VALUE_ENVIRONMENT_HPP
#define BEE_VALUE_ENVIRONMENT_HPP

#include <string_view>
#include <unordered_map>
#include <memory>

#include "bee/interpreter/Value.hpp"

namespace bee::interpreter {

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

  std::shared_ptr<Value> getValue(std::string_view key) const;
  std::shared_ptr<Value> scopeGetValue(std::string_view key) const;

  void putValue(std::string_view key, std::unique_ptr<Value> value);

  inline ValueScopeKind scopeKind() { return m_scopeKind; }
  inline std::shared_ptr<ValueEnvironment> parent() { return m_parent; }
  
private:
  ValueScopeKind m_scopeKind;
  std::shared_ptr<ValueEnvironment> m_parent;
  std::unordered_map<std::string_view, std::shared_ptr<Value>> m_values;
};  

} // namespace bee::interpreter

#endif // BEE_VALUE_ENVIRONMENT_HPP
