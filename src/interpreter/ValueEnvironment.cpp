#include "bee/interpreter/ValueEnvironment.hpp"

namespace bee::interpreter {

ValueEnvironment::ValueEnvironment(ValueScopeKind scopeKind,
                                   std::shared_ptr<ValueEnvironment> parent)
    : m_scopeKind(scopeKind), m_parent(std::move(parent)) {}

bool ValueEnvironment::hasValue(std::string_view key) const {
  if (m_values.contains(key))
    return true;

  return m_parent == nullptr ? false : m_parent->hasValue(key);
}

bool ValueEnvironment::scopeHasValue(std::string_view key) const {
  return m_values.contains(key);
}

Value ValueEnvironment::getValue(std::string_view key) const {
  if (m_values.contains(key))
    return m_values.at(key);

  if (m_parent == nullptr)
    throw std::runtime_error(
        "error: attempt to get an invalid key in value environment.");

  return m_parent->getValue(key);
}

Value ValueEnvironment::scopeGetValue(std::string_view key) const {
  if (!m_values.contains(key))
    throw std::runtime_error(
        "error: attempt to get an invalid key in value environment.");

  return m_values.at(key);
}

void ValueEnvironment::putValue(std::string_view key, Value value) {
  m_values.insert_or_assign(key, value);
}

} // namespace bee::interpreter
