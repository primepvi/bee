#ifndef BEE_SOURCE_HPP
#define BEE_SOURCE_HPP

#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

namespace bee::source {

struct SourceSpan {
  std::size_t line;
  std::size_t col;
  std::size_t start;
  std::size_t end;
};

class Source {
public:
  Source(std::string_view name, std::string code);
  static Source fromFile(std::string_view path);

  inline std::string_view name() const { return m_name; }
  inline const std::string& code() const { return m_code; }
  inline const std::vector<SourceSpan> &linesSpan() const {
    return m_linesSpan;
  }

private:
  std::string_view m_name;
  std::string m_code;
  std::vector<SourceSpan> m_linesSpan;
};

} // namespace bee::source

#endif // BEE_SOURCE_HPP
