#include "bee/parser/AstVisitors.hpp"
#include "bee/Source.hpp"
#include "bee/lexer/Token.hpp"
#include <unistd.h>

namespace bee::parser {

LiteralExpr::LiteralExpr(bee::lexer::Token value)
    : m_value(value), m_span(value.span()) {}

IdentifierExpr::IdentifierExpr(bee::lexer::Token identifier)
    : m_identifier(identifier), m_span(identifier.span()) {}

RangeExpr::RangeExpr(std::unique_ptr<Expr> start, bee::lexer::Token symbol,
                     std::unique_ptr<Expr> end)
    : m_start(std::move(start)), m_end(std::move(end)), m_symbol(symbol) {
  bee::SourceSpan startSpan = m_start->span();

  m_span = {
      .line = startSpan.line,
      .col = startSpan.col,
      .start = startSpan.start,
      .end = m_end->span().end,
  };
};

AssignmentExpr::AssignmentExpr(bee::lexer::Token identifier,
                               bee::lexer::Token equal,
                               std::unique_ptr<Expr> value)
    : m_identifier(identifier), m_equal(equal), m_value(std::move(value)) {

  bee::SourceSpan identifierSpan = identifier.span();

  m_span = {
      .line = identifierSpan.line,
      .col = identifierSpan.col,
      .start = identifierSpan.start,
      .end = m_value->span().end,
  };
}

BinaryExpr::BinaryExpr(std::unique_ptr<Expr> left, bee::lexer::Token op,
                       std::unique_ptr<Expr> right)
    : m_left(std::move(left)), m_right(std::move(right)), m_op(op) {

  bee::SourceSpan leftSpan = m_left->span();

  m_span = {
      .line = leftSpan.line,
      .col = leftSpan.col,
      .start = leftSpan.start,
      .end = m_right->span().end,
  };
}

UnaryExpr::UnaryExpr(bee::lexer::Token op, std::unique_ptr<Expr> operand)
    : m_op(op), m_operand(std::move(operand)) {
  bee::SourceSpan opSpan = op.span();

  m_span = {
      .line = opSpan.line,
      .col = opSpan.col,
      .start = opSpan.start,
      .end = m_operand->span().end,
  };
}

ParenthesizedExpr::ParenthesizedExpr(bee::lexer::Token openParen,
                                     std::unique_ptr<Expr> expr,
                                     bee::lexer::Token closeParen)
    : m_openParen(openParen), m_closeParen(closeParen),
      m_expr(std::move(expr)) {
  bee::SourceSpan openParenSpan = openParen.span();

  m_span = {
      .line = openParenSpan.line,
      .col = openParenSpan.col,
      .start = openParenSpan.start,
      .end = closeParen.span().end,
  };
}

WhenExpr::WhenExpr(bee::lexer::Token when, std::unique_ptr<Expr> condition,
                   bee::lexer::Token then, std::unique_ptr<Expr> consequent,
                   bee::lexer::Token otherwise, std::unique_ptr<Expr> alternate)
    : m_when(when), m_then(then), m_otherwise(otherwise),
      m_condition(std::move(condition)), m_consequent(std::move(consequent)),
      m_alternate(std::move(alternate)) {

  bee::SourceSpan whenSpan = when.span();

  m_span = {
      .line = whenSpan.line,
      .col = whenSpan.col,
      .start = whenSpan.start,
      .end = m_alternate->span().end,
  };
}

CallExpr::CallExpr(bee::lexer::Token identifier, bee::lexer::Token openParen,
                   std::vector<std::unique_ptr<Expr>> arguments,
                   bee::lexer::Token closeParen)
    : m_identifier(identifier), m_openParen(openParen),
      m_closeParen(closeParen), m_arguments(std::move(arguments)) {

  bee::SourceSpan identifierSpan = identifier.span();

  m_span = {
      .line = identifierSpan.line,
      .col = identifierSpan.col,
      .start = identifierSpan.start,
      .end = closeParen.span().end,
  };
}

InvalidExpr::InvalidExpr(bee::lexer::Token invalid)
    : m_invalid(invalid), m_span(invalid.span()) {}

VariableDeclarationStmt::VariableDeclarationStmt(
    bee::lexer::Token keyword, bee::lexer::Token identifier,
    bee::lexer::Token assignment, std::optional<TypeAnnotation> typeAnnotation,
    std::unique_ptr<Expr> value)
    : m_keyword(keyword), m_identifier(identifier), m_assignment(assignment),
      m_typeAnnotation(typeAnnotation), m_value(std::move(value)) {

  bee::SourceSpan keywordSpan = keyword.span();

  m_span = {
      .line = keywordSpan.line,
      .col = keywordSpan.col,
      .start = keywordSpan.start,
      .end = m_value->span().end,
  };
}

FunctionDeclarationStmt::FunctionDeclarationStmt(
    bee::lexer::Token keyword, bee::lexer::Token identifier,
    TypeAnnotation typeAnnotation, std::vector<FunctionDeclarationParam> params,
    std::unique_ptr<Stmt> body)
    : m_keyword(keyword), m_identifier(identifier),
      m_typeAnnotation(typeAnnotation), m_params(std::move(params)),
      m_body(std::move(body)) {

  bee::SourceSpan keywordSpan = keyword.span();

  m_span = {
      .line = keywordSpan.line,
      .col = keywordSpan.col,
      .start = keywordSpan.start,
      .end = m_body->span().end,
  };
}

ReturnStmt::ReturnStmt(bee::lexer::Token keyword, std::unique_ptr<Expr> expr)
    : m_keyword(keyword), m_expr(std::move(expr)) {
  bee::SourceSpan keywordSpan = keyword.span();

  m_span = {
      .line = keywordSpan.line,
      .col = keywordSpan.col,
      .start = keywordSpan.start,
      .end = m_expr->span().end,
  };
}

ExprStmt::ExprStmt(std::unique_ptr<Expr> expr) : m_expr(std::move(expr)) {
  m_span = m_expr->span();
}

EchoStmt::EchoStmt(bee::lexer::Token keyword, std::unique_ptr<Expr> message)
    : m_keyword(keyword), m_message(std::move(message)) {
  bee::SourceSpan keywordSpan = keyword.span();

  m_span = {
      .line = keywordSpan.line,
      .col = keywordSpan.col,
      .start = keywordSpan.start,
      .end = m_message->span().end,
  };
}

IfStmt::IfStmt(bee::lexer::Token keyword, std::unique_ptr<Expr> condition,
               std::unique_ptr<Stmt> consequent,
               std::unique_ptr<Stmt> alternate)
    : m_keyword(keyword), m_condition(std::move(condition)),
      m_consequent(std::move(consequent)), m_alternate(std::move(alternate)) {

  bee::SourceSpan keywordSpan = keyword.span();
  bee::SourceSpan endSpan = !m_alternate ? m_consequent->span() : m_alternate->span();

  m_span = {
      .line = keywordSpan.line,
      .col = keywordSpan.col,
      .start = keywordSpan.start,
      .end = endSpan.end,
  };
}

WhileStmt::WhileStmt(bee::lexer::Token keyword, std::unique_ptr<Expr> condition,
                     std::unique_ptr<Stmt> body)
    : m_keyword(keyword), m_condition(std::move(condition)),
      m_body(std::move(body)) {
  bee::SourceSpan keywordSpan = keyword.span();

  m_span = {
      .line = keywordSpan.line,
      .col = keywordSpan.col,
      .start = keywordSpan.start,
      .end = m_body->span().end,
  };
}

ForStmt::ForStmt(bee::lexer::Token keyword, std::unique_ptr<Expr> iterator,
                 std::unique_ptr<Expr> step, std::unique_ptr<Stmt> body)
    : m_keyword(keyword), m_iterator(std::move(iterator)),
      m_step(std::move(step)), m_body(std::move(body)) {
  bee::SourceSpan keywordSpan = keyword.span();

  m_span = {
      .line = keywordSpan.line,
      .col = keywordSpan.col,
      .start = keywordSpan.start,
      .end = m_body->span().end,
  };
}

BlockStmt::BlockStmt(bee::lexer::Token openKeyword,
                     std::optional<BlockCaptureAnnotation> captureAnnotation,
                     std::vector<std::unique_ptr<Stmt>> stmts,
                     bee::lexer::Token closeKeyword)

    : m_openKeyword(openKeyword), m_closeKeyword(closeKeyword),
      m_captureAnnotation(captureAnnotation), m_stmts(std::move(stmts)) {

  bee::SourceSpan keywordSpan = openKeyword.span();

  m_span = {
      .line = keywordSpan.line,
      .col = keywordSpan.col,
      .start = keywordSpan.start,
      .end = closeKeyword.span().end,
  };
}

InvalidStmt::InvalidStmt(bee::lexer::Token invalid)
    : m_invalid(invalid), m_span(invalid.span()) {}

} // namespace bee::parser
