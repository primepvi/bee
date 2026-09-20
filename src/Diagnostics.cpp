#include <algorithm>
#include <array>
#include <format>
#include <ostream>

#include "bee/Diagnostics.hpp"

namespace bee {

constexpr std::array diagnosticsMessageEntries =
    std::to_array<DiagnosticEntry>({
        {DiagnosticCode::UnexpectedSymbol,
         "Unexpected symbol has received: {0}."},
        {DiagnosticCode::UnterminatedString, "Unterminated string has found."},
        {DiagnosticCode::ExpectedToken, "Expected '{0}', but received '{1}'."},
        {DiagnosticCode::UnterminatedBlock, "Unterminated block has found."},
	  {DiagnosticCode::InvalidExpression, "Invalid expression has found."},
    });

std::string_view getDiagnosticLevelName(DiagnosticLevel level) {
  switch (level) {
  case DiagnosticLevel::Error:
    return "error";
  case DiagnosticLevel::Warning:
    return "warning";
  default:
    return "invalid";
  }
}

DiagnosticBag::DiagnosticBag(const Source &source) : m_source(source) {}

void DiagnosticBag::report(DiagnosticLevel level, DiagnosticCode code,
                           SourceSpan span, std::format_args args) {
  const DiagnosticEntry *entry = std::find_if(
      diagnosticsMessageEntries.begin(), diagnosticsMessageEntries.end(),
      [code](const DiagnosticEntry &entry) { return entry.code == code; });

  if (entry == diagnosticsMessageEntries.end()) {
    throw std::runtime_error("Unreachable (DiagnosticBag::report).");
  }

  std::string_view format = entry->text;
  std::string formated = std::vformat(format, args);

  Diagnostic diagnostic = {
      .source = m_source,
      .message = formated,
      .level = level,
      .code = code,
      .emphasis = span,
  };

  m_diagnostics.push_back(diagnostic);
}

void DiagnosticBag::write(std::ostream &output) const {
  for (const Diagnostic &diagnostic : m_diagnostics) {
    std::string_view levelName = getDiagnosticLevelName(diagnostic.level);
    const auto linesSpan = m_source.linesSpan();
    const auto lineSpan = linesSpan.at(diagnostic.emphasis.line - 1);

    const auto gutterWidth = linesSpan.size();
    std::string_view line =
        std::string_view(m_source.code())
            .substr(lineSpan.start, lineSpan.end - lineSpan.start);

    output << std::format("{} at [{}:{}:{}]: {}\n", levelName,
                          diagnostic.source.name(), diagnostic.emphasis.line,
                          diagnostic.emphasis.col, diagnostic.message);
    output << std::format("{:>{}} | \n", "", gutterWidth);
    output << std::format("{:>{}} | {}\n", diagnostic.emphasis.line,
                          gutterWidth, line);
    output << std::format("{:>{}} | {}{}\n", "", gutterWidth,
                          std::string(diagnostic.emphasis.col, ' '),
                          std::string(diagnostic.emphasis.end -
                                          diagnostic.emphasis.start,
                                      '^'))
           << std::endl;
  }
}

} // namespace bee
