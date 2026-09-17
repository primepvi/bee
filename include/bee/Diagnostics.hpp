#ifndef BEE_DIAGNOSTICS_HPP
#define BEE_DIAGNOSTICS_HPP

#include "bee/Source.hpp"

#include <format>
#include <string>
#include <string_view>
#include <vector>

namespace bee {

enum class DiagnosticLevel { Error, Warning };
std::string_view getDiagnosticLevelName(DiagnosticLevel level);

enum class DiagnosticCode {
  UnexpectedSymbol,
  UnterminatedString,
};

struct Diagnostic {
  const Source& source;
  std::string message;
  DiagnosticLevel level;
  DiagnosticCode code;
  SourceSpan emphasis;
};

struct DiagnosticEntry {
  DiagnosticCode code;
  std::string_view text;
};

class DiagnosticBag {
public:
  DiagnosticBag(const Source& source);

  void report(DiagnosticLevel level, DiagnosticCode code, SourceSpan span, std::format_args args);
  void write(std::ostream &output) const;

private:
  std::vector<Diagnostic> m_diagnostics;
  const Source &m_source;
};

} // namespace bee::diagnostics

#endif // BEE_DIAGNOSTICS_HPP
