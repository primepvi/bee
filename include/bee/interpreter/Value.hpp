#ifndef BEE_VALUE_HPP
#define BEE_VALUE_HPP

#include <cstdint>
#include <format>
#include <memory>
#include <string>
#include <string_view>

#include "bee/parser/Ast.hpp"
#include "utfcpp/utf8/checked.h"

namespace bee::interpreter {

enum class ValueKind {
  Int,
  UInt,
  Byte,
  UByte,
  Float,
  Bool,
  String,
  Char,
  Range,
  Function,
  Null,
};

class Value {
public:
  virtual ~Value() = default;
  virtual ValueKind kind() const = 0;
  virtual std::string toString() const = 0;
  virtual std::unique_ptr<Value> clone() const = 0;
  virtual bool equals(const Value &other) const = 0;
};

std::unique_ptr<Value> evalAdd(const Value &left, const Value &right);
std::unique_ptr<Value> evalSub(const Value &left, const Value &right);
std::unique_ptr<Value> evalMulti(const Value &left, const Value &right);
std::unique_ptr<Value> evalDiv(const Value &left, const Value &right);
std::unique_ptr<Value> evalMod(const Value &left, const Value &right);
std::unique_ptr<Value> evalRange(const Value &left, const Value &right);

bool evalLt(const Value &left, const Value &right);
bool evalLte(const Value &left, const Value &right);
bool evalGt(const Value &left, const Value &right);
bool evalGte(const Value &left, const Value &right);
bool evalAnd(const Value &left, const Value &right);
bool evalOr(const Value &left, const Value &right);

std::unique_ptr<Value> evalMinus(const Value &operand);
bool evalNegation(const Value &operand);

class IntValue : public Value {
public:
  IntValue(std::int64_t value);

  inline ValueKind kind() const override { return ValueKind::Int; }
  inline std::string toString() const override {
    return std::to_string(m_value);
  }

  inline std::unique_ptr<Value> clone() const override {
    return std::make_unique<IntValue>(*this);
  }

  bool equals(const Value &other) const override;

  inline std::int64_t value() const { return m_value; }

private:
  std::int64_t m_value;
};

class UIntValue : public Value {
public:
  UIntValue(std::uint64_t value);

  inline ValueKind kind() const override { return ValueKind::UInt; }
  inline std::string toString() const override {
    return std::to_string(m_value);
  }

  inline std::unique_ptr<Value> clone() const override {
    return std::make_unique<UIntValue>(*this);
  }

  bool equals(const Value &other) const override;

  inline std::uint64_t value() const { return m_value; }

private:
  std::uint64_t m_value;
};

class ByteValue : public Value {
public:
  ByteValue(std::int8_t value);

  inline ValueKind kind() const override { return ValueKind::Byte; }
  inline std::string toString() const override {
    return std::to_string(m_value);
  }

  inline std::unique_ptr<Value> clone() const override {
    return std::make_unique<ByteValue>(*this);
  }

  bool equals(const Value &other) const override;

  inline std::int8_t value() const { return m_value; }

private:
  std::int8_t m_value;
};

class UByteValue : public Value {
public:
  UByteValue(std::uint8_t value);

  inline ValueKind kind() const override { return ValueKind::UByte; }
  inline std::string toString() const override {
    return std::to_string(m_value);
  }

  inline std::unique_ptr<Value> clone() const override {
    return std::make_unique<UByteValue>(*this);
  }

  bool equals(const Value &other) const override;

  inline std::uint8_t value() const { return m_value; }

private:
  std::uint8_t m_value;
};

class FloatValue : public Value {
public:
  FloatValue(double value);

  inline ValueKind kind() const override { return ValueKind::Float; }
  inline std::string toString() const override {
    return std::to_string(m_value);
  }

  inline std::unique_ptr<Value> clone() const override {
    return std::make_unique<FloatValue>(*this);
  }

  bool equals(const Value &other) const override;

  inline double value() const { return m_value; }

private:
  double m_value;
};

class BoolValue : public Value {
public:
  BoolValue(bool value);

  inline ValueKind kind() const override { return ValueKind::Bool; }
  inline std::string toString() const override {
    return std::to_string(m_value);
  }

  inline std::unique_ptr<Value> clone() const override {
    return std::make_unique<BoolValue>(*this);
  }

  bool equals(const Value &other) const override;

  inline bool value() const { return m_value; }

private:
  bool m_value;
};

class StringValue : public Value {
public:
  StringValue(std::string value);

  inline ValueKind kind() const override { return ValueKind::String; }
  inline std::string toString() const override { return m_value; }
  inline std::unique_ptr<Value> clone() const override {
    return std::make_unique<StringValue>(*this);
  }

  bool equals(const Value &other) const override;

  inline const std::string &value() const { return m_value; }

private:
  std::string m_value;
};

class CharValue : public Value {
public:
  CharValue(char32_t value);

  inline ValueKind kind() const override { return ValueKind::Char; }
  inline std::string toString() const override {
    std::string result;
    utf8::append(m_value, std::back_inserter(result));
    return result;
  }

  inline std::unique_ptr<Value> clone() const override {
    return std::make_unique<CharValue>(*this);
  }

  bool equals(const Value &other) const override;

  inline char32_t value() const { return m_value; }

private:
  char32_t m_value;
};

class RangeValue : public Value {
public:
  RangeValue(std::int64_t start, std::int64_t end);

  inline ValueKind kind() const override { return ValueKind::Range; }
  inline std::string toString() const override {
    return std::format("@Range(start: {}, end:{})", m_start, m_end);
  }

  inline std::unique_ptr<Value> clone() const override {
    return std::make_unique<RangeValue>(*this);
  }

  bool equals(const Value &other) const override;

  inline std::int64_t start() const { return m_start; }
  inline std::int64_t end() const { return m_end; }

private:
  std::int64_t m_start, m_end;
};

class FunctionValue : public Value {
public:
  FunctionValue(std::string_view identifier,
                std::vector<std::string_view> params,
                const bee::parser::FunctionDeclarationStmt &stmt);

  inline ValueKind kind() const override { return ValueKind::Function; }
  inline std::string toString() const override {
    return std::format("@Function(identifier: {}, arity: {})", m_identifier,
                       m_params.size());
  }

  inline std::unique_ptr<Value> clone() const override {
    return std::make_unique<FunctionValue>(*this);
  }

  bool equals(const Value &other) const override;

  inline std::string_view identifier() const { return m_identifier; }
  inline const std::vector<std::string_view> &params() const {
    return m_params;
  }

  inline const auto &body() { return m_body; }

private:
  std::string_view m_identifier;
  std::vector<std::string_view> m_params;
  const bee::parser::FunctionDeclarationStmt &m_body;
};

class NullValue : public Value {
public:
  inline ValueKind kind() const override { return ValueKind::Null; }
  inline std::string toString() const override { return "null"; }
  inline std::unique_ptr<Value> clone() const override {
    return std::make_unique<NullValue>();
  }
  
  inline bool equals(const Value &other) const override {
    return other.kind() == this->kind();
  }
};

} // namespace bee::interpreter

#endif // BEE_VALUE_HPP
