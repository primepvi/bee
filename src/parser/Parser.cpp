#include "bee/parser/Parser.hpp"
#include "bee/Diagnostics.hpp"
#include "bee/Source.hpp"
#include "bee/lexer/Token.hpp"
#include "bee/parser/Ast.hpp"
#include <format>
#include <memory>
#include <optional>
#include <span>

namespace bee::parser {

using bee::lexer::Token;
using bee::lexer::TokenKind;

Parser::Parser(const bee::Source &source, bee::DiagnosticBag &bag,
               const std::vector<bee::lexer::Token> &tokens)
    : m_source(source), m_bag(bag), m_tokens(tokens) {}

Program Parser::parse() {
  Program program;
  while (hasMoreTokens()) {
    program.push_back(parseStmt());
  }

  return std::move(program);
}

std::unique_ptr<Expr> Parser::parseExpr() { return parseBinaryExpr(0); }
std::unique_ptr<Expr> Parser::parseLiteralExpr() {
  LiteralExpr literalExpr(eat());
  return std::make_unique<LiteralExpr>(literalExpr);
}

std::unique_ptr<Expr> Parser::parseIdentifierExpr() {
  IdentifierExpr identifierExpr(eat());
  return std::make_unique<IdentifierExpr>(identifierExpr);
}

std::unique_ptr<Expr> Parser::parseAssignmentExpr() {
  Token identifier = eat();
  Token equal = eat();
  std::unique_ptr<Expr> value = parseExpr();

  AssignmentExpr assignmentExpr(identifier, equal, std::move(value));
  return std::make_unique<AssignmentExpr>(std::move(assignmentExpr));
}

std::unique_ptr<Expr> Parser::parseParenthesizedExpr() {
  Token openParen = eat();
  std::unique_ptr<Expr> expr = parseExpr();
  Token closeParen = expectToken(TokenKind::CloseParenSym, ")");

  ParenthesizedExpr parenthesizedExpr(openParen, std::move(expr), closeParen);
  return std::make_unique<ParenthesizedExpr>(std::move(parenthesizedExpr));
}

std::unique_ptr<Expr> Parser::parseWhenExpr() {
  Token when = eat();
  std::unique_ptr<Expr> condition = parseExpr();
  Token then = expectToken(TokenKind::ThenKw, "then");
  std::unique_ptr<Expr> consequent = parseExpr();
  Token otherwise = expectToken(TokenKind::OtherwiseKw, "otherwise");
  std::unique_ptr<Expr> alternate = parseExpr();

  WhenExpr whenExpr(when, std::move(condition), then, std::move(consequent),
                    otherwise, std::move(alternate));
  return std::make_unique<WhenExpr>(std::move(whenExpr));
}

std::unique_ptr<Expr> Parser::parseCallExpr() {
  Token identifier = eat();
  Token openParen = eat();

  std::vector<std::unique_ptr<Expr>> arguments;
  while (hasMoreTokens() && peek().kind() != TokenKind::CloseParenSym) {
    std::unique_ptr<Expr> argument = parseExpr();
    arguments.push_back(std::move(argument));

    if (peek().kind() == TokenKind::CommaSym) {
      eat();
    } else {
      break;
    }
  }

  Token closeParen = expectToken(TokenKind::CloseParenSym, ")");
  CallExpr callExpr(identifier, openParen, std::move(arguments), closeParen);
  return std::make_unique<CallExpr>(std::move(callExpr));
}

std::unique_ptr<Expr> Parser::parsePrimaryExpr() {
  switch (peek().kind()) {
  case TokenKind::NullKw:
  case TokenKind::TrueKw:
  case TokenKind::FalseKw:
  case TokenKind::StringLit:
  case TokenKind::IntegerLit:
  case TokenKind::FloatLit:
  case TokenKind::CharLit:
  case TokenKind::AtomLit:    
    return parseLiteralExpr();

  case TokenKind::OpenParenSym:
    return parseParenthesizedExpr();

  case TokenKind::Identifier: {
    if (lookahead().kind() == TokenKind::EqualSym)
      return parseAssignmentExpr();
    else if (lookahead().kind() == TokenKind::OpenParenSym)
      return parseCallExpr();
    else
      return parseIdentifierExpr();
  }

  case TokenKind::WhenKw:
    return parseWhenExpr();

  default: {
    if (!m_panic) {
      m_bag.report(bee::DiagnosticLevel::Error,
                   bee::DiagnosticCode::InvalidExpression, peek().span(),
                   std::make_format_args());
      m_panic = true;
    }

    Token invalid = peek().kind() == TokenKind::EndOfFile ? peek() : eat();
    InvalidExpr invalidExpr(invalid);
    return std::make_unique<InvalidExpr>(invalidExpr);
  }
  }
}

std::unique_ptr<Expr> Parser::parseBinaryExpr(std::size_t precedence) {
  std::size_t unaryPrecedence =
      bee::lexer::getUnaryOperatorPriority(peek().kind());

  std::unique_ptr<Expr> expr = nullptr;

  if (unaryPrecedence != 0 && unaryPrecedence >= precedence) {
    Token op = eat();
    std::unique_ptr<Expr> operand = parseBinaryExpr(unaryPrecedence);

    UnaryExpr unaryExpr(op, std::move(operand));
    expr = std::make_unique<UnaryExpr>(std::move(unaryExpr));
  } else {
    expr = parsePrimaryExpr();
  }

  while (true) {
    std::size_t binaryOpPrecedence =
        bee::lexer::getBinaryOperatorPriority(peek().kind());

    if (binaryOpPrecedence == 0 || binaryOpPrecedence <= precedence)
      break;

    Token op = eat();
    std::unique_ptr<Expr> right = parseBinaryExpr(binaryOpPrecedence);

    BinaryExpr binaryExpr(std::move(expr), op, std::move(right));
    expr = std::make_unique<BinaryExpr>(std::move(binaryExpr));
  }

  return expr;
}

TypeAnnotation Parser::parseTypeAnnotation() {
  Token colon = eat();

  bool lit = false;
  if (peek().kind() == TokenKind::LitKw) {
    lit = true;
    eat();
  }

  Token identifier = expectToken(TokenKind::Identifier, "type name");

  bool nullable = false;
  if (peek().kind() == TokenKind::QuestionSym) {
    nullable = true;
    eat();
  }

  bee::SourceSpan colonSpan = colon.span();
  bee::SourceSpan annotationSpan = {
      .line = colonSpan.line,
      .col = colonSpan.col,
      .start = colonSpan.start,
      .end = nullable ? identifier.span().end + 1 : identifier.span().end,
  };

  return TypeAnnotation{
      .colon = colon,
      .identifier = identifier,
      .nullable = nullable,
      .lit = lit,
      .span = annotationSpan,
  };
}

std::unique_ptr<Stmt> Parser::parseStmt() {
  std::unique_ptr<Stmt> stmt = parseNextStmt();

  if (m_panic) {
    Token invalid = peek();
    synchronize();

    InvalidStmt invalidStmt(invalid);
    return std::make_unique<InvalidStmt>(invalidStmt);
  }

  return stmt;
}

std::unique_ptr<Stmt> Parser::parseNextStmt() {
  switch (peek().kind()) {
  case TokenKind::ConstKw:
  case TokenKind::LetKw:
  case TokenKind::LitKw:    
    return parseVariableDeclarationStmt();
  case TokenKind::FnKw:
    return parseFunctionDeclarationStmt();
  case TokenKind::EchoKw:
    return parseEchoStmt();
  case TokenKind::IfKw:
    return parseIfStmt();
  case TokenKind::WhileKw:
    return parseWhileStmt();
  case TokenKind::ForKw:
    return parseForStmt();
  case TokenKind::ReturnKw:
    return parseReturnStmt();
  default:
    return parseExprStmt();
  }
}

std::unique_ptr<Stmt> Parser::parseVariableDeclarationStmt() {
  Token keyword = eat();
  Token identifier = expectToken(TokenKind::Identifier, "variable name");

  std::optional<TypeAnnotation> typeAnnotation = std::nullopt;
  if (peek().kind() == TokenKind::ColonSym) {
    typeAnnotation = parseTypeAnnotation();
  }

  Token assignment = expectToken(TokenKind::EqualSym, "=");
  std::unique_ptr<Expr> value = parseExpr();

  VariableDeclarationStmt variableDeclStmt(keyword, identifier, assignment,
                                           typeAnnotation, std::move(value));
  return std::make_unique<VariableDeclarationStmt>(std::move(variableDeclStmt));
}

std::unique_ptr<Stmt> Parser::parseFunctionDeclarationStmt() {
  Token keyword = eat();
  Token identifier = expectToken(TokenKind::Identifier, "function name");
  expectToken(TokenKind::OpenParenSym, "(");

  std::vector<FunctionDeclarationParam> params;
  while (hasMoreTokens() && peek().kind() != TokenKind::CloseParenSym) {
    Token paramIdentifier = expectToken(TokenKind::Identifier, "param name");
    TypeAnnotation paramTypeAnnotation = parseTypeAnnotation();

    bee::SourceSpan paramSpan = paramIdentifier.span();
    paramSpan.end = paramTypeAnnotation.span.end;

    FunctionDeclarationParam param = {
        .identifier = paramIdentifier,
        .typeAnnotation = paramTypeAnnotation,
        .span = paramSpan,
    };

    params.push_back(param);
    if (peek().kind() == TokenKind::CommaSym) {
      eat();
    } else {
      break;
    }
  }

  expectToken(TokenKind::CloseParenSym, ")");

  TypeAnnotation typeAnnotation = parseTypeAnnotation();
  std::unique_ptr<Stmt> body = nullptr;
  if (peek().kind() == TokenKind::ArrowSym) {
    eat();
    body = parseExprStmt();
  } else {
    auto endKinds = std::to_array<TokenKind>({TokenKind::EndKw});
    body = parseBlockStmt(endKinds);
    expectToken(TokenKind::EndKw, "end");
  }

  FunctionDeclarationStmt functionDeclStmt(keyword, identifier, typeAnnotation,
                                           std::move(params), std::move(body));
  return std::make_unique<FunctionDeclarationStmt>(std::move(functionDeclStmt));
}

std::unique_ptr<Stmt> Parser::parseReturnStmt() {
  Token keyword = eat();
  std::unique_ptr<Expr> expr = canStartExpr() ? parseExpr() : nullptr;

  ReturnStmt returnStmt(keyword, std::move(expr));
  return std::make_unique<ReturnStmt>(std::move(returnStmt));
}

std::unique_ptr<Stmt> Parser::parseExprStmt() {
  std::unique_ptr<Expr> expr = parseExpr();

  ExprStmt exprStmt(std::move(expr));
  return std::make_unique<ExprStmt>(std::move(exprStmt));
}

std::unique_ptr<Stmt> Parser::parseEchoStmt() {
  Token keyword = eat();
  std::unique_ptr<Expr> message = parseExpr();

  EchoStmt echoStmt(keyword, std::move(message));
  return std::make_unique<EchoStmt>(std::move(echoStmt));
}

std::unique_ptr<Stmt> Parser::parseIfStmt() {
  Token keyword = eat();
  std::unique_ptr<Expr> condition = parseExpr();

  auto endKinds =
      std::to_array<TokenKind>({TokenKind::EndKw, TokenKind::ElseKw});

  std::unique_ptr<Stmt> consequent = parseBlockStmt(endKinds);
  std::unique_ptr<Stmt> alternate = nullptr;

  if (peek().kind() == TokenKind::ElseKw) {
    eat();

    if (peek().kind() == TokenKind::IfKw) {
      alternate = parseIfStmt();
    } else {
      alternate = parseBlockStmt(endKinds);
      expectToken(TokenKind::EndKw, "end");
    }
  } else {
    expectToken(TokenKind::EndKw, "end");
  }

  IfStmt ifStmt(keyword, std::move(condition), std::move(consequent),
                std::move(alternate));
  return std::make_unique<IfStmt>(std::move(ifStmt));
}

std::unique_ptr<Stmt> Parser::parseBlockStmt(std::span<TokenKind> endKinds) {
  Token openKeyword = expectToken(TokenKind::DoKw, "do");

  std::optional<BlockCaptureAnnotation> captureAnnotation;
  if (peek().kind() == TokenKind::PipeSym) {
    Token openPipe = eat();

    std::vector<Token> captures;
    while (hasMoreTokens() && peek().kind() != TokenKind::PipeSym) {
      Token capture = expectToken(TokenKind::Identifier, "identifier");
      captures.push_back(capture);

      if (peek().kind() == TokenKind::CommaSym) {
        eat();
      } else {
        break;
      }
    }

    Token closePipe = expectToken(TokenKind::PipeSym, "|");
    captureAnnotation = BlockCaptureAnnotation{
        .openPipe = openPipe,
        .closePipe = closePipe,
        .captures = std::move(captures),
    };
  }

  std::vector<std::unique_ptr<Stmt>> stmts;
  while (hasMoreTokens()) {
    Token current = peek();

    auto predicate = [current](const TokenKind &kind) {
      return current.kind() == kind;
    };

    auto it = std::find_if(endKinds.begin(), endKinds.end(), predicate);
    if (it != endKinds.end()) {
      break;
    }

    stmts.push_back(parseStmt());
  }

  if (!hasMoreTokens()) {
    m_bag.report(bee::DiagnosticLevel::Error,
                 bee::DiagnosticCode::UnterminatedBlock, openKeyword.span(),
                 std::make_format_args());
    m_panic = true;
  }

  Token closeKeyword = peek();
  BlockStmt blockStmt(openKeyword, captureAnnotation, std::move(stmts),
                      closeKeyword);

  return std::make_unique<BlockStmt>(std::move(blockStmt));
}

std::unique_ptr<Stmt> Parser::parseWhileStmt() {
  Token keyword = eat();
  std::unique_ptr<Expr> condition = parseExpr();
  std::unique_ptr<Stmt> body = nullptr;

  if (peek().kind() == TokenKind::ArrowSym) {
    eat();
    body = parseStmt();
  } else {
    auto endKinds = std::to_array({TokenKind::EndKw});
    body = parseBlockStmt(endKinds);
    expectToken(TokenKind::EndKw, "end");
  }

  WhileStmt whileStmt(keyword, std::move(condition), std::move(body));
  return std::make_unique<WhileStmt>(std::move(whileStmt));
}

std::unique_ptr<Stmt> Parser::parseForStmt() {
  Token keyword = eat();
  std::unique_ptr<Expr> iterator = parseExpr();
  std::unique_ptr<Expr> step = nullptr;
  if (peek().kind() == TokenKind::CommaSym) {
    eat();
    step = parseExpr();
  }

  std::unique_ptr<Stmt> body = nullptr;
  if (peek().kind() == TokenKind::ArrowSym) {
    eat();
    body = parseStmt();
  } else {
    auto endKinds = std::to_array({TokenKind::EndKw});
    body = parseBlockStmt(endKinds);
    expectToken(TokenKind::EndKw, "end");
  }

  ForStmt forStmt(keyword, std::move(iterator), std::move(step),
                  std::move(body));
  return std::make_unique<ForStmt>(std::move(forStmt));
}

bee::lexer::Token Parser::peek() const {
  return m_cursor < m_tokens.size() ? m_tokens.at(m_cursor)
                                    : m_tokens.at(m_tokens.size() - 1);
}
bee::lexer::Token Parser::lookahead() const {
  return m_cursor + 1 < m_tokens.size() ? m_tokens.at(m_cursor + 1)
                                        : m_tokens.at(m_tokens.size() - 1);
}

bee::lexer::Token Parser::eat() {
  bee::lexer::Token token = peek();
  m_cursor += 1;
  return token;
}

void Parser::synchronize() {
  eat();

  while (hasMoreTokens()) {
    switch (peek().kind()) {
    case TokenKind::ConstKw:
    case TokenKind::LetKw:
    case TokenKind::FnKw:
    case TokenKind::EchoKw:
    case TokenKind::IfKw:
    case TokenKind::WhileKw:
    case TokenKind::ForKw:
    case TokenKind::ReturnKw:
    case TokenKind::EndKw:
      m_panic = false;
      return;
    default:
      eat();
    }
  }

  m_panic = false;
}

Token Parser::expectToken(TokenKind kind, std::string name) {
  if (peek().kind() == kind) {
    return eat();
  }

  if (!m_panic) {
    std::string_view lexeme = peek().lexeme();
    m_bag.report(bee::DiagnosticLevel::Error,
                 bee::DiagnosticCode::ExpectedToken, peek().span(),
                 std::make_format_args(name, lexeme));
    m_panic = true;
  }

  SourceSpan span = peek().span();
  span.col -= 1;
  span.start -= 1;
  span.end -= 1;

  return Token(TokenKind::Invalid, "invalid", span);
}

bool Parser::hasMoreTokens() const {
  return m_tokens.size() > m_cursor && peek().kind() != TokenKind::EndOfFile;
}

bool Parser::canStartExpr() const {
  switch (peek().kind()) {
  case TokenKind::NullKw:
  case TokenKind::TrueKw:
  case TokenKind::FalseKw:
  case TokenKind::FloatLit:
  case TokenKind::IntegerLit:
  case TokenKind::CharLit:
  case TokenKind::StringLit:
  case TokenKind::AtomLit:    
  case TokenKind::OpenParenSym:
  case TokenKind::CloseParenSym:
  case TokenKind::Identifier:
  case TokenKind::WhenKw:
    return true;
  default:
    return false;
  }
}

} // namespace bee::parser
