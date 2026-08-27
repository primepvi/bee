#ifndef BEE_AST_H
#define BEE_AST_H

#include "libs/types.h"
#include "libs/string_view.h"
#include "libs/array_list.h"

typedef struct {
  u32 line;
  u32 start, end;
} Span;

typedef enum {
  TOKEN_KIND_LET,
  TOKEN_KIND_CONST,
  TOKEN_KIND_ECHO,
  TOKEN_KIND_IDENTIFIER,

  TOKEN_KIND_NUMBER,
  TOKEN_KIND_STRING,

  TOKEN_KIND_SEMICOLON,
  TOKEN_KIND_EQUAL,

  TOKEN_KIND_PLUS,
  TOKEN_KIND_MINUS,
  TOKEN_KIND_STAR,
  TOKEN_KIND_SLASH,
  TOKEN_KIND_PERCENTAGE,
  
  TOKEN_KIND_EOF,  
  TOKEN_KIND_KEY_COUNT,
} TokenKind;

typedef struct {
  TokenKind kind;
  StringView lexeme;
  Span span;
} Token;

extern const char *const TOKEN_NAMES[TOKEN_KIND_KEY_COUNT];

typedef struct Expr Expr;
typedef struct Stmt Stmt;

typedef enum {
  EXPR_KIND_LITERAL,
  EXPR_KIND_IDENTIFIER,
  EXPR_KIND_ASSIGNMENT,
  EXPR_KIND_BINARY,
} ExprKind;

typedef struct {
  Token value;
  Span span;
} LiteralExpr;

typedef struct {
  StringView name;
  Span span;
} IdentifierExpr;

typedef struct {
  StringView identifier;
  Expr *value;
  Span span;
} AssignmentExpr;

typedef struct {
  Expr *left, *right;
  Token operator_token;
  Span span;
} BinaryExpr;

typedef struct Expr {
  ExprKind kind;
  union {
    LiteralExpr literal;
    IdentifierExpr identifier;
    AssignmentExpr assignment;
    BinaryExpr binary;
  } as;
} Expr;

typedef enum {
  STMT_KIND_VARIABLE_DECL,
  STMT_KIND_EXPR,
  STMT_KIND_ECHO,
} StmtKind;

typedef struct {
  StringView identifier;
  Expr value;
  b8 is_const;
  Span span;
} VariableDeclStmt;

typedef struct {
  Expr expr;
  Span span;
} ExprStmt;

typedef struct {
  Expr message;
  Span span;
} EchoStmt;

typedef struct Stmt {
  StmtKind kind;
  union {
    VariableDeclStmt variable_decl;
    ExprStmt expr;
    EchoStmt echo;
  } as;
} Stmt;

typedef struct {
  ArrayList *stmts;
} Program;

void ast_print(const Program *program);
u32 ast_binary_operator_priority(TokenKind kind);

#endif // BEE_AST_H
