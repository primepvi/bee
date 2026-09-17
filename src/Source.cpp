#include <fstream>

#include "bee/Source.hpp"

namespace bee {

Source::Source(std::string_view name, std::string code)
    : m_name(name), m_code(code) {
  std::size_t start = 0;
  for (std::size_t cursor = 0; cursor < code.length(); cursor++) {
    if (code.at(cursor) != '\n')
      continue;

    m_linesSpan.push_back({
        .line = m_linesSpan.size() + 1,
        .col = 0,
        .start = start,
        .end = cursor,
    });

    start = cursor + 1;
  }

  if (start < code.length()) {
    m_linesSpan.push_back({
        .line = m_linesSpan.size() + 1,
        .col = 0,
        .start = start,
        .end = code.length(),
    });
  }
}

Source Source::fromFile(std::string_view path) {
  std::string filepath(path);
  std::ifstream file(filepath, std::ios::binary);

  if (!file) {
    throw std::runtime_error("Failed to open source file.");
  }

  std::string code = std::string(std::istreambuf_iterator<char>(file),
                                 std::istreambuf_iterator<char>());

  return Source(path, code);
}

} // namespace bee
