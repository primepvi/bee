#ifndef BEE_TYPED_AST_HPP
#define BEE_TYPED_AST_HPP

#include <memory>
#include <vector>

#include "bee/symbols/Scope.hpp"
#include "bee/symbols/Symbol.hpp"

namespace bee::typechecker {

enum class TypedExprKind {
  Literal,
  Identifier,
  Assignment,
  Binary,
  Unary,
  When,
  Call,
  Invalid,
};

enum class TypedAstBinaryOperation {
  Addition,
  Subtraction,
  Multiplication,
  Division,
  Module,
  Range,
  And,
  Or,
};

enum class TypedAstUnaryOperation {
  Minus,
  Plus,
  Negation,
};

class TypedExpr {
public:
  virtual ~TypedExpr() = default;
  virtual TypedExprKind kind() const = 0;
  virtual bee::symbols::SymbolId typeId() const = 0;
};

class TypedLiteralExpr : public TypedExpr {
public:
  TypedLiteralExpr(bee::symbols::SymbolId valueId,
                   bee::symbols::SymbolId typeId);

  inline TypedExprKind kind() const override { return TypedExprKind::Literal; }

  inline bee::symbols::SymbolId typeId() const override { return m_typeId; }
  inline bee::symbols::SymbolId valueId() const { return m_valueId; }

private:
  bee::symbols::SymbolId m_valueId, m_typeId;
};

class TypedIdentifierExpr : public TypedExpr {
public:
  TypedIdentifierExpr(bee::symbols::SymbolId variableId,
                      bee::symbols::SymbolId typeId);

  inline TypedExprKind kind() const override {
    return TypedExprKind::Identifier;
  }

  inline bee::symbols::SymbolId variableId() const { return m_variableId; }
  inline bee::symbols::SymbolId typeId() const override { return m_typeId; }

private:
  bee::symbols::SymbolId m_variableId, m_typeId;
};

class TypedAssignmentExpr : public TypedExpr {
public:
  TypedAssignmentExpr(bee::symbols::SymbolId variableId,
                      bee::symbols::SymbolId typeId,
                      std::unique_ptr<TypedExpr> value);

  inline TypedExprKind kind() const override {
    return TypedExprKind::Assignment;
  }

  inline bee::symbols::SymbolId variableId() const { return m_variableId; }
  inline bee::symbols::SymbolId typeId() const override { return m_typeId; }
  inline const std::unique_ptr<TypedExpr> &value() const { return m_value; }

private:
  bee::symbols::SymbolId m_variableId, m_typeId;
  std::unique_ptr<TypedExpr> m_value;
};

class TypedBinaryExpr : public TypedExpr {
public:
  TypedBinaryExpr(bee::symbols::SymbolId typeId,
                  std::unique_ptr<TypedExpr> left,
                  TypedAstBinaryOperation operation,
                  std::unique_ptr<TypedExpr> right);

  inline TypedExprKind kind() const override { return TypedExprKind::Binary; }

  inline bee::symbols::SymbolId typeId() const override { return m_typeId; }
  inline TypedAstBinaryOperation operation() const { return m_operation; }

  inline const std::unique_ptr<TypedExpr> &left() const { return m_left; }
  inline const std::unique_ptr<TypedExpr> &right() const { return m_right; }

private:
  bee::symbols::SymbolId m_typeId;
  TypedAstBinaryOperation m_operation;
  std::unique_ptr<TypedExpr> m_left, m_right;
};

class TypedUnaryExpr : public TypedExpr {
public:
  TypedUnaryExpr(bee::symbols::SymbolId typeId,
                 TypedAstUnaryOperation operation,
                 std::unique_ptr<TypedExpr> operand);

  inline bee::symbols::SymbolId typeId() const override { return m_typeId; }
  inline TypedExprKind kind() const override { return TypedExprKind::Unary; }

  inline TypedAstUnaryOperation operation() const { return m_operation; }
  inline const std::unique_ptr<TypedExpr> &operand() const { return m_operand; }

private:
  bee::symbols::SymbolId m_typeId;
  TypedAstUnaryOperation m_operation;
  std::unique_ptr<TypedExpr> m_operand;
};

class TypedWhenExpr : public TypedExpr {
public:
  TypedWhenExpr(bee::symbols::SymbolId typeId,
                std::unique_ptr<TypedExpr> condition,
                std::unique_ptr<TypedExpr> consequent,
                std::unique_ptr<TypedExpr> alternate);

  inline bee::symbols::SymbolId typeId() const override { return m_typeId; }
  inline TypedExprKind kind() const override { return TypedExprKind::When; }

  inline const std::unique_ptr<TypedExpr> &condition() { return m_condition; }
  inline const std::unique_ptr<TypedExpr> &consequent() { return m_consequent; }
  inline const std::unique_ptr<TypedExpr> &alternate() { return m_alternate; }

private:
  bee::symbols::SymbolId m_typeId;
  std::unique_ptr<TypedExpr> m_condition, m_consequent, m_alternate;
};

class TypedCallExpr : public TypedExpr {
public:
  TypedCallExpr(bee::symbols::SymbolId typeId,
                std::vector<std::unique_ptr<TypedExpr>> args);

  inline bee::symbols::SymbolId typeId() const override { return m_typeId; }
  inline TypedExprKind kind() const override { return TypedExprKind::Call; }

  inline const std::vector<std::unique_ptr<TypedExpr>> &args() const {
    return m_args;
  }

private:
  bee::symbols::SymbolId m_typeId;
  std::vector<std::unique_ptr<TypedExpr>> m_args;
};

class TypedInvalidExpr : public TypedExpr {
public:
  TypedInvalidExpr(bee::symbols::SymbolId typeId);

  inline bee::symbols::SymbolId typeId() const override { return m_typeId; }
  inline TypedExprKind kind() const override { return TypedExprKind::Invalid; }

private:
  bee::symbols::SymbolId m_typeId;
};

enum class TypedStmtKind {
  VariableDeclaration,
  FunctionDeclaration,
  Return,
  Expr,
  Echo,
  If,
  Block,
  While,
  For,
  Invalid,
};

class TypedStmt {
public:
  virtual ~TypedStmt() = default;
  virtual TypedStmtKind kind() const = 0;
};

class TypedVariableDeclarationStmt : public TypedStmt {
public:
  TypedVariableDeclarationStmt(bee::symbols::SymbolId variableId,
                               std::unique_ptr<TypedExpr> value);

  inline TypedStmtKind kind() const override {
    return TypedStmtKind::VariableDeclaration;
  }

  inline bee::symbols::SymbolId variableId() const { return m_variableId; }
  inline const std::unique_ptr<TypedExpr> &value() const { return m_value; }

private:
  bee::symbols::SymbolId m_variableId;
  std::unique_ptr<TypedExpr> m_value;
};

class TypedFunctionDeclarationStmt : public TypedStmt {
public:
  TypedFunctionDeclarationStmt(bee::symbols::SymbolId functionId,
                               std::unique_ptr<TypedStmt> body);

  inline TypedStmtKind kind() const override {
    return TypedStmtKind::FunctionDeclaration;
  }

  inline bee::symbols::SymbolId functionId() const { return m_functionId; }

  inline const std::unique_ptr<TypedStmt> &body() const { return m_body; }

private:
  bee::symbols::SymbolId m_functionId;
  std::unique_ptr<TypedStmt> m_body;
};

class TypedReturnStmt : public TypedStmt {
public:
  TypedReturnStmt(std::unique_ptr<TypedExpr> value);

  inline TypedStmtKind kind() const override { return TypedStmtKind::Return; }
  inline const std::unique_ptr<TypedExpr> &value() { return m_value; }

private:
  std::unique_ptr<TypedExpr> m_value;
};

class TypedExprStmt : public TypedStmt {
public:
  TypedExprStmt(std::unique_ptr<TypedExpr> value);

  inline TypedStmtKind kind() const override { return TypedStmtKind::Expr; }
  inline const std::unique_ptr<TypedExpr> &value() { return m_value; }

private:
  std::unique_ptr<TypedExpr> m_value;
};

class TypedEchoStmt : public TypedStmt {
public:
  TypedEchoStmt(std::unique_ptr<TypedExpr> message);

  inline TypedStmtKind kind() const override { return TypedStmtKind::Echo; }
  inline const std::unique_ptr<TypedExpr> &message() { return m_message; }

private:
  std::unique_ptr<TypedExpr> m_message;
};

class TypedIfStmt : public TypedStmt {
public:
  TypedIfStmt(std::unique_ptr<TypedExpr> condition,
              std::unique_ptr<TypedExpr> consequent,
              std::unique_ptr<TypedExpr> alternate);

  inline TypedStmtKind kind() const override { return TypedStmtKind::If; }

  inline const std::unique_ptr<TypedExpr> &condition() const {
    return m_condition;
  }

  inline const std::unique_ptr<TypedStmt> &consequent() const {
    return m_consequent;
  }

  inline const std::unique_ptr<TypedStmt> &alternate() const {
    return m_alternate;
  }

private:
  std::unique_ptr<TypedExpr> m_condition;
  std::unique_ptr<TypedStmt> m_consequent, m_alternate;
};

class TypedWhileStmt : public TypedStmt {
public:
  TypedWhileStmt(bee::symbols::ScopeId scopeId,
                 std::unique_ptr<TypedExpr> condition,
                 std::unique_ptr<TypedStmt> body);

  inline TypedStmtKind kind() const override { return TypedStmtKind::While; }

  inline bee::symbols::ScopeId scopeId() const { return m_scopeId; }
  inline const std::unique_ptr<TypedExpr> &condition() const {
    return m_condition;
  }
  inline const std::unique_ptr<TypedStmt> &body() const { return m_body; }

private:
  bee::symbols::ScopeId m_scopeId;
  std::unique_ptr<TypedExpr> m_condition;
  std::unique_ptr<TypedStmt> m_body;
};

class TypedForStmt : public TypedStmt {
public:
  TypedForStmt(bee::symbols::ScopeId scopeId,
               std::unique_ptr<TypedExpr> iterator,
               std::unique_ptr<TypedExpr> step,
               std::unique_ptr<TypedStmt> body);

  inline TypedStmtKind kind() const override { return TypedStmtKind::For; }

  inline bee::symbols::ScopeId scopeId() const { return m_scopeId; }
  inline const std::unique_ptr<TypedExpr> &iterator() const {
    return m_iterator;
  }

  inline const std::unique_ptr<TypedExpr> &step() const { return m_step; }
  inline const std::unique_ptr<TypedStmt> &body() const { return m_body; }

private:
  bee::symbols::ScopeId m_scopeId;
  std::unique_ptr<TypedExpr> m_iterator, m_step;
  std::unique_ptr<TypedStmt> m_body;
};

class TypedBlockStmt : public TypedStmt {
public:
  TypedBlockStmt(bee::symbols::ScopeId scopeId,
                 std::vector<bee::symbols::SymbolId> capturesIds,
                 std::vector<std::unique_ptr<TypedStmt>> stmts);

  inline TypedStmtKind kind() const override { return TypedStmtKind::Block; }

  inline bee::symbols::ScopeId scopeId() const { return m_scopeId; }
  inline const std::vector<bee::symbols::SymbolId> &capturesIds() const {
    return m_capturesIds;
  }

  inline const std::vector<std::unique_ptr<TypedStmt>> &stmts() const {
    return m_stmts;
  }

private:
  bee::symbols::ScopeId m_scopeId;
  std::vector<bee::symbols::SymbolId> m_capturesIds;
  std::vector<std::unique_ptr<TypedStmt>> m_stmts;
};

class TypedInvalidStmt : public TypedStmt {
public:
  inline TypedStmtKind kind() const override { return TypedStmtKind::Invalid; }
};

} // namespace bee::typechecker

#endif // BEE_TYPED_AST_HPP
