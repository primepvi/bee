#include "bee/parser/AstDumper.hpp"
#include "bee/parser/Ast.hpp"

#include <memory>
#include <optional>
#include <string>

namespace bee::parser {

static auto COMMON_SYM = BEE_DUMP_BRANCH_COLOR "├──" BEE_DUMP_RESET_COLOR;
static auto LAST_SYM = BEE_DUMP_BRANCH_COLOR "└──" BEE_DUMP_RESET_COLOR;

AstDumper::AstDumper(const Program &program, std::ostream &output)
    : m_program(program), m_output(output) {}

void AstDumper::dump() {
  m_output << BEE_DUMP_STATEMENT_COLOR << "Program" << BEE_DUMP_RESET_COLOR
           << '\n';

  for (std::size_t i = 0; i < m_program.size(); ++i) {
    dumpStmt(*m_program[i], " ", i == m_program.size() - 1);
  }
}

void AstDumper::dumpStmt(const Stmt &stmt, const std::string &prefix,
                         bool isLast) {
  const auto oldPrefix = m_prefix;
  const auto oldIsLast = m_isLast;

  m_prefix = prefix;
  m_isLast = isLast;

  visitStmt(stmt);

  m_prefix = oldPrefix;
  m_isLast = oldIsLast;
}

void AstDumper::dumpExpr(const Expr &expr, const std::string &prefix,
                         bool isLast) {
  const auto oldPrefix = m_prefix;
  const auto oldIsLast = m_isLast;

  m_prefix = prefix;
  m_isLast = isLast;

  visitExpr(expr);

  m_prefix = oldPrefix;
  m_isLast = oldIsLast;
}

void AstDumper::visitVariableDeclarationStmt(
    const VariableDeclarationStmt &stmt) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpStmt(m_prefix, symbol, "Variable Declaration Statement");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Constant:");

  const bool isConst = stmt.keyword().lexeme() == "const";
  m_output << makeDumpValue(isConst ? "true" : "false") << '\n';
  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Identifier:");
  m_output << makeDumpValue(std::string(stmt.identifier().lexeme())) << '\n';

  if (stmt.typeAnnotation()) {
    TypeAnnotation annotation = stmt.typeAnnotation().value();
    m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Type Annotation:");
    m_output << makeDumpValue(std::string(annotation.identifier.lexeme()))
             << '\n';
  }

  m_output << makeDumpProperty(childPrefix, LAST_SYM, "Value:") << '\n';
  dumpExpr(*stmt.value(), makeDumpPrefix(childPrefix, true), true);
}

void AstDumper::visitFunctionDeclarationStmt(
    const FunctionDeclarationStmt &stmt) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpStmt(m_prefix, symbol, "Function Declaration Statement");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Identifier:");
  m_output << makeDumpValue(std::string(stmt.identifier().lexeme())) << '\n';

  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Type Annotation:");
  m_output << makeDumpValue(
                  std::string(stmt.typeAnnotation().identifier.lexeme()))
           << '\n';

  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Params:") << '\n';
  const auto paramsPrefix = makeDumpPrefix(childPrefix, false);

  for (std::size_t i = 0; i < stmt.params().size(); ++i) {
    const bool isLast = i == stmt.params().size() - 1;

    m_output << makeDumpProperty(paramsPrefix, isLast ? LAST_SYM : COMMON_SYM,
                                 std::to_string(i) + ":");
    m_output << makeDumpValue(std::string(stmt.params()[i].identifier.lexeme()))
             << '\n';
  }

  m_output << makeDumpProperty(childPrefix, LAST_SYM, "Body:") << '\n';
  dumpStmt(*stmt.body(), makeDumpPrefix(childPrefix, true), true);
}

void AstDumper::visitReturnStmt(const ReturnStmt &stmt) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpStmt(m_prefix, symbol, "Return Statement");

  if (!stmt.expr()) {
    return;
  }

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, LAST_SYM, "Expression:") << '\n';
  dumpExpr(*stmt.expr(), makeDumpPrefix(childPrefix, true), true);
}

void AstDumper::visitExprStmt(const ExprStmt &stmt) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpStmt(m_prefix, symbol, "Expression Statement");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  dumpExpr(*stmt.expr(), childPrefix, true);
}

void AstDumper::visitEchoStmt(const EchoStmt &stmt) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpStmt(m_prefix, symbol, "Echo Statement");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  dumpExpr(*stmt.message(), childPrefix, true);
}

void AstDumper::visitIfStmt(const IfStmt &stmt) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpStmt(m_prefix, symbol, "If Statement");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Condition:") << '\n';
  dumpExpr(*stmt.condition(), makeDumpPrefix(childPrefix, false), true);

  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Consequent:") << '\n';
  dumpStmt(*stmt.consequent(), makeDumpPrefix(childPrefix, false), true);

  if (stmt.alternate()) {
    m_output << makeDumpProperty(childPrefix, LAST_SYM, "Alternate:") << '\n';
    dumpStmt(*stmt.alternate(), makeDumpPrefix(childPrefix, true), true);
  }
}

void AstDumper::visitBlockStmt(const BlockStmt &stmt) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpStmt(m_prefix, symbol, "Block Statement");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, LAST_SYM, "Statements:") << '\n';

  if (stmt.captureAnnotation()) {
    m_output << makeDumpProperty(childPrefix, LAST_SYM, "Captures:") << '\n';

    BlockCaptureAnnotation annotation = stmt.captureAnnotation().value();
    const auto annotationPrefix = makeDumpPrefix(childPrefix, false);
    for (std::size_t i = 0; i < annotation.captures.size(); ++i) {
      const bool isLast = i == annotation.captures.size() - 1;
      m_output << makeDumpProperty(annotationPrefix,
                                   isLast ? LAST_SYM : COMMON_SYM,
                                   std::string(annotation.captures[i].lexeme())) << '\n';
    }
  }

  const auto statementsPrefix = makeDumpPrefix(childPrefix, true);
  for (std::size_t i = 0; i < stmt.stmts().size(); ++i) {
    const bool isLast = i == stmt.stmts().size() - 1;
    dumpStmt(*stmt.stmts()[i], statementsPrefix, isLast);
  }
}

void AstDumper::visitForStmt(const ForStmt &stmt) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpStmt(m_prefix, symbol, "For Statement");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Iterator:") << '\n';
  dumpExpr(*stmt.iterator(), makeDumpPrefix(childPrefix, false), true);

  if (stmt.step()) {
    m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Step:") << '\n';
    dumpExpr(*stmt.step(), makeDumpPrefix(childPrefix, false), true);
  }

  m_output << makeDumpProperty(childPrefix, LAST_SYM, "Body:") << '\n';
  dumpStmt(*stmt.body(), makeDumpPrefix(childPrefix, true), true);
}

void AstDumper::visitWhileStmt(const WhileStmt &stmt) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpStmt(m_prefix, symbol, "While Statement");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Condition:") << '\n';
  dumpExpr(*stmt.condition(), makeDumpPrefix(childPrefix, false), true);

  m_output << makeDumpProperty(childPrefix, LAST_SYM, "Body:") << '\n';
  dumpStmt(*stmt.body(), makeDumpPrefix(childPrefix, true), true);
}

void AstDumper::visitInvalidStmt(const InvalidStmt &stmt) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpStmt(m_prefix, symbol, "InvalidStmt");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, LAST_SYM, "Invalid:");
  m_output << makeDumpValue(std::string(stmt.invalid().lexeme())) << '\n';
}

void AstDumper::visitLiteralExpr(const LiteralExpr &expr) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpExpr(m_prefix, symbol, "Literal Expression");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, LAST_SYM, "Value:");
  m_output << makeDumpValue(std::string(expr.value().lexeme())) << '\n';
}

void AstDumper::visitIdentifierExpr(const IdentifierExpr &expr) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpExpr(m_prefix, symbol, "Identifier Expression");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, LAST_SYM, "Identifier:");
  m_output << makeDumpValue(std::string(expr.identifier().lexeme())) << '\n';
}

void AstDumper::visitRangeExpr(const RangeExpr &expr) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpExpr(m_prefix, symbol, "Range Expression");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Start:") << '\n';
  dumpExpr(*expr.start(), makeDumpPrefix(childPrefix, false), true);

  m_output << makeDumpProperty(childPrefix, LAST_SYM, "End:") << '\n';
  dumpExpr(*expr.end(), makeDumpPrefix(childPrefix, true), true);
}

void AstDumper::visitAssignmentExpr(const AssignmentExpr &expr) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpExpr(m_prefix, symbol, "Assignment Expression");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Identifier:");
  m_output << makeDumpValue(std::string(expr.identifier().lexeme())) << '\n';

  m_output << makeDumpProperty(childPrefix, LAST_SYM, "Value:") << '\n';
  dumpExpr(*expr.value(), makeDumpPrefix(childPrefix, true), true);
}

void AstDumper::visitBinaryExpr(const BinaryExpr &expr) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpExpr(m_prefix, symbol, "Binary Expression");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Left:") << '\n';
  dumpExpr(*expr.left(), makeDumpPrefix(childPrefix, false), true);

  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Operator:");
  m_output << makeDumpValue(std::string(expr.op().lexeme())) << '\n';

  m_output << makeDumpProperty(childPrefix, LAST_SYM, "Right:") << '\n';
  dumpExpr(*expr.right(), makeDumpPrefix(childPrefix, true), true);
}

void AstDumper::visitUnaryExpr(const UnaryExpr &expr) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpExpr(m_prefix, symbol, "Unary Expression");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Operator:");
  m_output << makeDumpValue(std::string(expr.op().lexeme())) << '\n';

  m_output << makeDumpProperty(childPrefix, LAST_SYM, "Operand:") << '\n';
  dumpExpr(*expr.operand(), makeDumpPrefix(childPrefix, true), true);
}

void AstDumper::visitParenthesizedExpr(const ParenthesizedExpr &expr) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpExpr(m_prefix, symbol, "Parenthesized Expression");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, LAST_SYM, "Expression:") << '\n';
  dumpExpr(*expr.expr(), makeDumpPrefix(childPrefix, true), true);
}

void AstDumper::visitWhenExpr(const WhenExpr &expr) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpExpr(m_prefix, symbol, "When Expression");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Condition:") << '\n';
  dumpExpr(*expr.condition(), makeDumpPrefix(childPrefix, false), true);

  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Alternate:") << '\n';
  dumpExpr(*expr.alternate(), makeDumpPrefix(childPrefix, false), true);

  m_output << makeDumpProperty(childPrefix, LAST_SYM, "Consequent:") << '\n';
  dumpExpr(*expr.consequent(), makeDumpPrefix(childPrefix, true), true);
}

void AstDumper::visitCallExpr(const CallExpr &expr) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpExpr(m_prefix, symbol, "Call Expression");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, COMMON_SYM, "Identifier:");
  m_output << makeDumpValue(std::string(expr.identifier().lexeme())) << '\n';

  m_output << makeDumpProperty(childPrefix, LAST_SYM, "Arguments:") << '\n';
  const auto argumentPrefix = makeDumpPrefix(childPrefix, true);
  for (std::size_t i = 0; i < expr.arguments().size(); ++i) {
    const bool isLast = i == expr.arguments().size() - 1;
    const auto argumentSymbol = isLast ? LAST_SYM : COMMON_SYM;

    m_output << makeDumpProperty(argumentPrefix, argumentSymbol,
                                 std::to_string(i) + ":")
             << '\n';

    dumpExpr(*expr.arguments()[i], makeDumpPrefix(argumentPrefix, isLast),
             true);
  }
}

void AstDumper::visitInvalidExpr(const InvalidExpr &expr) {
  const auto symbol = m_isLast ? LAST_SYM : COMMON_SYM;
  m_output << makeDumpExpr(m_prefix, symbol, "Invalid Expression");

  const auto childPrefix = makeDumpPrefix(m_prefix, m_isLast);
  m_output << makeDumpProperty(childPrefix, LAST_SYM, "Invalid:");
  m_output << makeDumpValue(std::string(expr.invalid().lexeme())) << '\n';
}

std::string AstDumper::makeDumpPrefix(std::string current, bool isLast) const {
  std::string whitespace =
      isLast ? "    " : BEE_DUMP_BRANCH_COLOR "│   " BEE_DUMP_RESET_COLOR;

  return current + whitespace;
}

std::string AstDumper::makeDumpStmt(std::string prefix, std::string symbol,
                                    std::string name) const {

  return prefix + BEE_DUMP_BRANCH_COLOR + symbol + BEE_DUMP_STATEMENT_COLOR +
         name + BEE_DUMP_RESET_COLOR + "\n";
}

std::string AstDumper::makeDumpExpr(std::string prefix, std::string symbol,
                                    std::string name) const {
  return prefix + BEE_DUMP_BRANCH_COLOR + symbol + BEE_DUMP_EXPRESSION_COLOR +
         name + BEE_DUMP_RESET_COLOR + "\n";
}

std::string AstDumper::makeDumpProperty(std::string prefix, std::string symbol,
                                        std::string name) const {
  return prefix + BEE_DUMP_BRANCH_COLOR + symbol + BEE_DUMP_PROPERTY_COLOR +
         name + BEE_DUMP_RESET_COLOR;
}

std::string AstDumper::makeDumpValue(std::string value) const {
  return std::string(" ") + BEE_DUMP_VALUE_COLOR + value + BEE_DUMP_RESET_COLOR;
}

} // namespace bee::parser
