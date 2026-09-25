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
  // Lexer
  UnexpectedSymbol,
  UnterminatedString,
  UnterminatedChar,
  InvalidCharLength,

  // Parser
  ExpectedToken,
  UnterminatedBlock,
  InvalidExpression,

  // TypeChecker
  IdentifierAlreadyDeclared,
  TypeMismatch,
  InvalidTypeAnnotation,
  InvalidVoidUsage,
  InvalidReturnUsage,
  InvalidFunctionDeclaration,
  InvalidFunctionCallArity,
  VoidControlPaths,
  UndefinedIdentifier,
  InvalidAssignment,
  UnsupporetedBinaryOperation,
  UnsupportedUnaryOperation,
  NonFunctionCall,
  InvalidBlockCaptureCount,
  InvalidBlockCaptureScope,
  InvalidLitVarDeclaration,
};

struct Diagnostic {
  const Source &source;
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
  DiagnosticBag(const Source &source);

  void report(DiagnosticLevel level, DiagnosticCode code, SourceSpan span,
              std::format_args args);
  void write(std::ostream &output) const;
  
  inline bool isEmpty() const { return m_diagnostics.size() == 0; }
private:
  std::vector<Diagnostic> m_diagnostics;
  const Source &m_source;
};

} // namespace bee

#endif // BEE_DIAGNOSTICS_HPP
