#ifndef BEE_VALUE_HPP
#define BEE_VALUE_HPP

#include <cstdint>
#include <string>
#include <variant>
#include <vector>

namespace bee::symbols {

struct Value;
struct NumericData;
struct AtomData;
struct IteratorData;
struct FunctionData;

using ValueData =
    std::variant<NumericData, bool, char32_t, AtomData, std::string,
                 IteratorData, FunctionData, std::monostate>;

struct NumericData {
  union {
    std::int64_t as_int;
    std::uint64_t as_uint;
    std::int8_t as_byte;
    std::uint8_t as_ubyte;
    double as_float;
  };
};

struct AtomData {
  const std::string &identifier;
  std::size_t id;
};

struct IteratorData {
  std::vector<Value> values;
  std::size_t start, end, cursor;
};

struct FunctionData {
  std::string identifier;
  std::vector<std::string> params;
};

struct Value {
  ValueData data;
  bool isLit;
};  

} // namespace bee::symbols

#endif // BEE_VALUE_HPP
