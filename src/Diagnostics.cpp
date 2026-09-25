#include <algorithm>
#include <array>
#include <format>
#include <ostream>

#include "bee/Diagnostics.hpp"

namespace bee {

constexpr std::array diagnosticsMessageEntries =
    std::to_array<DiagnosticEntry>({
        // Lexer
        {DiagnosticCode::UnexpectedSymbol,
         "Unexpected symbol has received: '{0}'."},
        {DiagnosticCode::UnterminatedString, "Unterminated string has found."},
        {DiagnosticCode::UnterminatedChar, "Unterminated char has found."},
        {DiagnosticCode::InvalidCharLength,
         "Attempt to create a char with more than one character."},

        // Parser
        {DiagnosticCode::ExpectedToken, "Expected '{0}', but received '{1}'."},
        {DiagnosticCode::UnterminatedBlock, "Unterminated block has found."},
        {DiagnosticCode::InvalidExpression, "Invalid expression has found."},

        // TypeChecker
        {DiagnosticCode::IdentifierAlreadyDeclared,
         "Identifier '{0}' already declared."},
        {DiagnosticCode::TypeMismatch,
         "Expected type '{0}', but received type '{1}'"},
        {DiagnosticCode::InvalidTypeAnnotation, "Invalid type annotation."},
        {DiagnosticCode::InvalidVoidUsage, "Void type cannot be used here."},
        {DiagnosticCode::InvalidReturnUsage,
         "Attempt to return a value outside a function scope."},
        {DiagnosticCode::InvalidFunctionDeclaration,
         "Attempt to declare a function in non-global scope."},
        {DiagnosticCode::InvalidFunctionCallArity,
         "Function '{0}' expects {1} arguments, but has received only {2} "
         "arguments."},
        {DiagnosticCode::VoidControlPaths,
         "Non-void function has void control paths."},
        {DiagnosticCode::UndefinedIdentifier,
         "Identifier '{0}' is not defined."},
        {DiagnosticCode::InvalidAssignment,
         "Attempt to assign a value to a '{0}'."},
        {DiagnosticCode::UnsupporetedBinaryOperation,
         "The binary operator '{0}' not supports a left type '{1}' and a right "
         "type '{2}'."},
        {DiagnosticCode::UnsupportedUnaryOperation,
         "The unary operator '{0}' not supports an operand of type '{1}'."},
        {DiagnosticCode::NonFunctionCall,
         "Attempt to call a non-function identifier."},
        {DiagnosticCode::InvalidBlockCaptureCount,
         "The '{0}' block provides {1} capturables values, but has received "
         "{2} identifiers."},
        {DiagnosticCode::InvalidBlockCaptureScope,
         "The current scope don't provides capturables values."},
        {DiagnosticCode::InvalidLitVarDeclaration,
         "An lit variable only supports 'lit types'."},
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
