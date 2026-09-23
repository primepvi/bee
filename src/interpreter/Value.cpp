#include "bee/interpreter/Value.hpp"
#include "bee/parser/Ast.hpp"

namespace bee::interpreter {

IntValue::IntValue(std::int64_t value) : m_value(value) {}
bool IntValue::equals(const Value &other) const {
  if (other.kind() != this->kind())
    return false;

  const IntValue &otherValue = static_cast<const IntValue &>(other);
  return m_value == otherValue.value();
}

UIntValue::UIntValue(std::uint64_t value) : m_value(value) {}
bool UIntValue::equals(const Value &other) const {
  if (other.kind() != this->kind())
    return false;

  const UIntValue &otherValue = static_cast<const UIntValue &>(other);
  return m_value == otherValue.value();
}

BoolValue::BoolValue(bool value) : m_value(value) {}
bool BoolValue::equals(const Value &other) const {
  if (other.kind() != this->kind())
    return false;

  const BoolValue &otherValue = static_cast<const BoolValue &>(other);
  return m_value == otherValue.value();
}

StringValue::StringValue(std::string value) : m_value(value) {}
bool StringValue::equals(const Value &other) const {
  if (other.kind() != this->kind())
    return false;

  const StringValue &otherValue = static_cast<const StringValue &>(other);
  return m_value == otherValue.value();
}

CharValue::CharValue(char32_t value) : m_value(value) {}
bool CharValue::equals(const Value &other) const {
  if (other.kind() != this->kind())
    return false;

  const CharValue &otherValue = static_cast<const CharValue &>(other);
  return m_value == otherValue.value();
}  

RangeValue::RangeValue(std::int64_t start, std::int64_t end)
    : m_start(start), m_end(end) {}

bool RangeValue::equals(const Value &other) const {
  if (other.kind() != this->kind())
    return false;

  const RangeValue &otherValue = static_cast<const RangeValue &>(other);
  return m_start == otherValue.start() && m_end == otherValue.end();
}

FunctionValue::FunctionValue(std::string_view identifier,
                             std::vector<std::string_view> params,
                             const bee::parser::FunctionDeclarationStmt &body)
    : m_identifier(identifier), m_params(std::move(params)), m_body(body) {}

bool FunctionValue::equals(const Value &other) const {
  if (other.kind() != this->kind())
    return false;

  const FunctionValue &otherValue = static_cast<const FunctionValue &>(other);
  return this->toString() == other.toString();
}

} // namespace bee::interpreter
