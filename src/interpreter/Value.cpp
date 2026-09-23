#include "bee/interpreter/Value.hpp"
#include "bee/parser/Ast.hpp"

#include <cmath>

namespace bee::interpreter {

std::unique_ptr<Value> evalAdd(const Value &left, const Value &right) {
  if (left.kind() != right.kind())
    throw std::runtime_error("Cannot add values of different types.");

  switch (left.kind()) {
  case ValueKind::Int: {
    const auto &l = static_cast<const IntValue &>(left);
    const auto &r = static_cast<const IntValue &>(right);
    return std::make_unique<IntValue>(l.value() + r.value());
  }

  case ValueKind::UInt: {
    const auto &l = static_cast<const UIntValue &>(left);
    const auto &r = static_cast<const UIntValue &>(right);
    return std::make_unique<UIntValue>(l.value() + r.value());
  }

  case ValueKind::Byte: {
    const auto &l = static_cast<const ByteValue &>(left);
    const auto &r = static_cast<const ByteValue &>(right);
    return std::make_unique<ByteValue>(static_cast<std::int8_t>(
        static_cast<int>(l.value()) + static_cast<int>(r.value())));
  }

  case ValueKind::UByte: {
    const auto &l = static_cast<const UByteValue &>(left);
    const auto &r = static_cast<const UByteValue &>(right);
    return std::make_unique<UByteValue>(
        static_cast<std::uint8_t>(static_cast<unsigned int>(l.value()) +
                                  static_cast<unsigned int>(r.value())));
  }

  case ValueKind::Float: {
    const auto &l = static_cast<const FloatValue &>(left);
    const auto &r = static_cast<const FloatValue &>(right);
    return std::make_unique<FloatValue>(l.value() + r.value());
  }

  default:
    throw std::runtime_error("Invalid operands for addition.");
  }
}

std::unique_ptr<Value> evalSub(const Value &left, const Value &right) {
  if (left.kind() != right.kind())
    throw std::runtime_error("Cannot subtract values of different types.");

  switch (left.kind()) {
  case ValueKind::Int: {
    const auto &l = static_cast<const IntValue &>(left);
    const auto &r = static_cast<const IntValue &>(right);
    return std::make_unique<IntValue>(l.value() - r.value());
  }

  case ValueKind::UInt: {
    const auto &l = static_cast<const UIntValue &>(left);
    const auto &r = static_cast<const UIntValue &>(right);
    return std::make_unique<UIntValue>(l.value() - r.value());
  }

  case ValueKind::Byte: {
    const auto &l = static_cast<const ByteValue &>(left);
    const auto &r = static_cast<const ByteValue &>(right);
    return std::make_unique<ByteValue>(static_cast<std::int8_t>(
        static_cast<int>(l.value()) - static_cast<int>(r.value())));
  }

  case ValueKind::UByte: {
    const auto &l = static_cast<const UByteValue &>(left);
    const auto &r = static_cast<const UByteValue &>(right);
    return std::make_unique<UByteValue>(
        static_cast<std::uint8_t>(static_cast<unsigned int>(l.value()) -
                                  static_cast<unsigned int>(r.value())));
  }

  case ValueKind::Float: {
    const auto &l = static_cast<const FloatValue &>(left);
    const auto &r = static_cast<const FloatValue &>(right);
    return std::make_unique<FloatValue>(l.value() - r.value());
  }

  default:
    throw std::runtime_error("Invalid operands for subtraction.");
  }
}
std::unique_ptr<Value> evalMulti(const Value &left, const Value &right) {
  if (left.kind() != right.kind())
    throw std::runtime_error("Cannot multiply values of different types.");

  switch (left.kind()) {
  case ValueKind::Int: {
    const auto &l = static_cast<const IntValue &>(left);
    const auto &r = static_cast<const IntValue &>(right);
    return std::make_unique<IntValue>(l.value() * r.value());
  }

  case ValueKind::UInt: {
    const auto &l = static_cast<const UIntValue &>(left);
    const auto &r = static_cast<const UIntValue &>(right);
    return std::make_unique<UIntValue>(l.value() * r.value());
  }

  case ValueKind::Byte: {
    const auto &l = static_cast<const ByteValue &>(left);
    const auto &r = static_cast<const ByteValue &>(right);

    return std::make_unique<ByteValue>(static_cast<std::int8_t>(
        static_cast<int>(l.value()) * static_cast<int>(r.value())));
  }

  case ValueKind::UByte: {
    const auto &l = static_cast<const UByteValue &>(left);
    const auto &r = static_cast<const UByteValue &>(right);
    return std::make_unique<UByteValue>(
        static_cast<std::uint8_t>(static_cast<unsigned int>(l.value()) *
                                  static_cast<unsigned int>(r.value())));
  }

  case ValueKind::Float: {
    const auto &l = static_cast<const FloatValue &>(left);
    const auto &r = static_cast<const FloatValue &>(right);
    return std::make_unique<FloatValue>(l.value() * r.value());
  }

  default:
    throw std::runtime_error("Invalid operands for multiplication.");
  }
}

std::unique_ptr<Value> evalDiv(const Value &left, const Value &right) {
  if (left.kind() != right.kind())
    throw std::runtime_error("Cannot divide values of different types.");

  switch (left.kind()) {
  case ValueKind::Int: {
    const auto &l = static_cast<const IntValue &>(left);
    const auto &r = static_cast<const IntValue &>(right);

    if (r.value() == 0)
      throw std::runtime_error("Division by zero.");

    return std::make_unique<IntValue>(l.value() / r.value());
  }

  case ValueKind::UInt: {
    const auto &l = static_cast<const UIntValue &>(left);
    const auto &r = static_cast<const UIntValue &>(right);

    if (r.value() == 0)
      throw std::runtime_error("Division by zero.");

    return std::make_unique<UIntValue>(l.value() / r.value());
  }

  case ValueKind::Byte: {
    const auto &l = static_cast<const ByteValue &>(left);
    const auto &r = static_cast<const ByteValue &>(right);

    if (r.value() == 0)
      throw std::runtime_error("Division by zero.");

    return std::make_unique<ByteValue>(static_cast<std::int8_t>(
        static_cast<int>(l.value()) / static_cast<int>(r.value())));
  }

  case ValueKind::UByte: {
    const auto &l = static_cast<const UByteValue &>(left);
    const auto &r = static_cast<const UByteValue &>(right);

    if (r.value() == 0)
      throw std::runtime_error("Division by zero.");

    return std::make_unique<UByteValue>(
        static_cast<std::uint8_t>(static_cast<unsigned int>(l.value()) /
                                  static_cast<unsigned int>(r.value())));
  }

  case ValueKind::Float: {
    const auto &l = static_cast<const FloatValue &>(left);
    const auto &r = static_cast<const FloatValue &>(right);

    if (r.value() == 0.0)
      throw std::runtime_error("Division by zero.");

    return std::make_unique<FloatValue>(l.value() / r.value());
  }

  default:
    throw std::runtime_error("Invalid operands for division.");
  }
}

std::unique_ptr<Value> evalMod(const Value &left, const Value &right) {
  if (left.kind() != right.kind())
    throw std::runtime_error("Cannot modulo values of different types.");

  switch (left.kind()) {

  case ValueKind::Int: {
    const auto &l = static_cast<const IntValue &>(left);
    const auto &r = static_cast<const IntValue &>(right);

    if (r.value() == 0)
      throw std::runtime_error("Modulo by zero.");

    return std::make_unique<IntValue>(l.value() % r.value());
  }

  case ValueKind::UInt: {
    const auto &l = static_cast<const UIntValue &>(left);
    const auto &r = static_cast<const UIntValue &>(right);

    if (r.value() == 0)
      throw std::runtime_error("Modulo by zero.");

    return std::make_unique<UIntValue>(l.value() % r.value());
  }

  case ValueKind::Byte: {
    const auto &l = static_cast<const ByteValue &>(left);
    const auto &r = static_cast<const ByteValue &>(right);

    if (r.value() == 0)
      throw std::runtime_error("Modulo by zero.");

    return std::make_unique<ByteValue>(static_cast<std::int8_t>(
        static_cast<int>(l.value()) % static_cast<int>(r.value())));
  }

  case ValueKind::UByte: {
    const auto &l = static_cast<const UByteValue &>(left);
    const auto &r = static_cast<const UByteValue &>(right);

    if (r.value() == 0)
      throw std::runtime_error("Modulo by zero.");

    return std::make_unique<UByteValue>(
        static_cast<std::uint8_t>(static_cast<unsigned int>(l.value()) %
                                  static_cast<unsigned int>(r.value())));
  }

  case ValueKind::Float: {
    const auto &l = static_cast<const FloatValue &>(left);
    const auto &r = static_cast<const FloatValue &>(right);

    if (r.value() == 0.0)
      throw std::runtime_error("Modulo by zero.");

    return std::make_unique<FloatValue>(std::fmod(l.value(), r.value()));
  }

  default:
    throw std::runtime_error("Invalid operands for modulo.");
  }
}

std::unique_ptr<Value> evalRange(const Value &left, const Value &right) {
  if (left.kind() != ValueKind::Int || right.kind() != ValueKind::Int) {
    throw std::runtime_error("range requires int operands");
  }

  const auto &lhs = static_cast<const IntValue &>(left);
  const auto &rhs = static_cast<const IntValue &>(right);

  return std::make_unique<RangeValue>(lhs.value(), rhs.value());
}

bool evalLt(const Value &left, const Value &right) {
  if (left.kind() != right.kind()) {
    throw std::runtime_error("comparison requires operands of the same type");
  }

  switch (left.kind()) {
  case ValueKind::Int: {
    const auto &lhs = static_cast<const IntValue &>(left);
    const auto &rhs = static_cast<const IntValue &>(right);
    return lhs.value() < rhs.value();
  }

  case ValueKind::UInt: {
    const auto &lhs = static_cast<const UIntValue &>(left);
    const auto &rhs = static_cast<const UIntValue &>(right);
    return lhs.value() < rhs.value();
  }

  case ValueKind::Byte: {
    const auto &lhs = static_cast<const ByteValue &>(left);
    const auto &rhs = static_cast<const ByteValue &>(right);
    return lhs.value() < rhs.value();
  }

  case ValueKind::UByte: {
    const auto &lhs = static_cast<const UByteValue &>(left);
    const auto &rhs = static_cast<const UByteValue &>(right);
    return lhs.value() < rhs.value();
  }

  case ValueKind::Float: {
    const auto &lhs = static_cast<const FloatValue &>(left);
    const auto &rhs = static_cast<const FloatValue &>(right);
    return lhs.value() < rhs.value();
  }

  default:
    throw std::runtime_error("invalid operands for <");
  }
}

bool evalLte(const Value &left, const Value &right) {
  if (left.kind() != right.kind()) {
    throw std::runtime_error("comparison requires operands of the same type");
  }

  switch (left.kind()) {
  case ValueKind::Int: {
    const auto &lhs = static_cast<const IntValue &>(left);
    const auto &rhs = static_cast<const IntValue &>(right);
    return lhs.value() <= rhs.value();
  }

  case ValueKind::UInt: {
    const auto &lhs = static_cast<const UIntValue &>(left);
    const auto &rhs = static_cast<const UIntValue &>(right);
    return lhs.value() <= rhs.value();
  }

  case ValueKind::Byte: {
    const auto &lhs = static_cast<const ByteValue &>(left);
    const auto &rhs = static_cast<const ByteValue &>(right);
    return lhs.value() <= rhs.value();
  }

  case ValueKind::UByte: {
    const auto &lhs = static_cast<const UByteValue &>(left);
    const auto &rhs = static_cast<const UByteValue &>(right);
    return lhs.value() <= rhs.value();
  }

  case ValueKind::Float: {
    const auto &lhs = static_cast<const FloatValue &>(left);
    const auto &rhs = static_cast<const FloatValue &>(right);
    return lhs.value() <= rhs.value();
  }

  default:
    throw std::runtime_error("invalid operands for <=");
  }
}

bool evalGt(const Value &left, const Value &right) {
  if (left.kind() != right.kind()) {
    throw std::runtime_error("comparison requires operands of the same type");
  }

  switch (left.kind()) {
  case ValueKind::Int: {
    const auto &lhs = static_cast<const IntValue &>(left);
    const auto &rhs = static_cast<const IntValue &>(right);
    return lhs.value() > rhs.value();
  }

  case ValueKind::UInt: {
    const auto &lhs = static_cast<const UIntValue &>(left);
    const auto &rhs = static_cast<const UIntValue &>(right);
    return lhs.value() > rhs.value();
  }

  case ValueKind::Byte: {
    const auto &lhs = static_cast<const ByteValue &>(left);
    const auto &rhs = static_cast<const ByteValue &>(right);
    return lhs.value() > rhs.value();
  }

  case ValueKind::UByte: {
    const auto &lhs = static_cast<const UByteValue &>(left);
    const auto &rhs = static_cast<const UByteValue &>(right);
    return lhs.value() > rhs.value();
  }

  case ValueKind::Float: {
    const auto &lhs = static_cast<const FloatValue &>(left);
    const auto &rhs = static_cast<const FloatValue &>(right);
    return lhs.value() > rhs.value();
  }

  default:
    throw std::runtime_error("invalid operands for >");
  }
}

bool evalGte(const Value &left, const Value &right) {
  if (left.kind() != right.kind()) {
    throw std::runtime_error("comparison requires operands of the same type");
  }

  switch (left.kind()) {
  case ValueKind::Int: {
    const auto &lhs = static_cast<const IntValue &>(left);
    const auto &rhs = static_cast<const IntValue &>(right);
    return lhs.value() >= rhs.value();
  }

  case ValueKind::UInt: {
    const auto &lhs = static_cast<const UIntValue &>(left);
    const auto &rhs = static_cast<const UIntValue &>(right);
    return lhs.value() >= rhs.value();
  }

  case ValueKind::Byte: {
    const auto &lhs = static_cast<const ByteValue &>(left);
    const auto &rhs = static_cast<const ByteValue &>(right);
    return lhs.value() >= rhs.value();
  }

  case ValueKind::UByte: {
    const auto &lhs = static_cast<const UByteValue &>(left);
    const auto &rhs = static_cast<const UByteValue &>(right);
    return lhs.value() >= rhs.value();
  }

  case ValueKind::Float: {
    const auto &lhs = static_cast<const FloatValue &>(left);
    const auto &rhs = static_cast<const FloatValue &>(right);
    return lhs.value() >= rhs.value();
  }

  default:
    throw std::runtime_error("invalid operands for >=");
  }
}

bool evalAnd(const Value &left, const Value &right) {
  if (left.kind() != ValueKind::Bool || right.kind() != ValueKind::Bool) {
    throw std::runtime_error("Operator 'and' requires boolean operands.");
  }

  const auto &l = static_cast<const BoolValue &>(left);
  const auto &r = static_cast<const BoolValue &>(right);
  return l.value() && r.value();
}

bool evalOr(const Value &left, const Value &right) {
  if (left.kind() != ValueKind::Bool || right.kind() != ValueKind::Bool) {
    throw std::runtime_error("Operator 'or' requires boolean operands.");
  }

  const auto &l = static_cast<const BoolValue &>(left);
  const auto &r = static_cast<const BoolValue &>(right);
  return l.value() || r.value();
}

bool evalNegation(const Value &operand) {
  if (operand.kind() != ValueKind::Bool) {
    throw std::runtime_error("Operator 'not' requires boolean operand.");
  }

  const auto &operandValue = static_cast<const BoolValue &>(operand);
  return !operandValue.value();
}

std::unique_ptr<Value> evalMinus(const Value &operand) {
  switch (operand.kind()) {
  case ValueKind::Int: {
    const auto &value = static_cast<const IntValue &>(operand);
    return std::make_unique<IntValue>(-value.value());
  }

  case ValueKind::Byte: {
    const auto &value = static_cast<const ByteValue &>(operand);
    return std::make_unique<ByteValue>(-value.value());
  }

  case ValueKind::Float: {
    const auto &value = static_cast<const FloatValue &>(operand);
    return std::make_unique<FloatValue>(-value.value());
  }

  case ValueKind::UInt:
  case ValueKind::UByte:
    throw std::runtime_error("cannot apply unary '-' to unsigned value");

  default:
    throw std::runtime_error("invalid operand for unary '-'");
  }
}

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

ByteValue::ByteValue(std::int8_t value) : m_value(value) {}
bool ByteValue::equals(const Value &other) const {
  if (other.kind() != this->kind())
    return false;

  const ByteValue &otherValue = static_cast<const ByteValue &>(other);
  return m_value == otherValue.value();
}

UByteValue::UByteValue(std::uint8_t value) : m_value(value) {}
bool UByteValue::equals(const Value &other) const {
  if (other.kind() != this->kind())
    return false;

  const UByteValue &otherValue = static_cast<const UByteValue &>(other);
  return m_value == otherValue.value();
}

FloatValue::FloatValue(double value) : m_value(value) {}
bool FloatValue::equals(const Value &other) const {
  if (other.kind() != this->kind())
    return false;

  const FloatValue &otherValue = static_cast<const FloatValue &>(other);
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
