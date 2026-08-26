#ifndef BEE_AST_H
#define BEE_AST_H

#include "libs/types.h"
#include "libs/string_view.h"
#include "libs/array_list.h"

typedef enum {
  TOKEN_KIND_LET,
  TOKEN_KIND_CONST,
  TOKEN_KIND_IDENTIFIER,
  
  TOKEN_KIND_NUMBER,
  TOKEN_KIND_STRING,

  TOKEN_KIND_SEMICOLON,
  TOKEN_KIND_EQUAL,
  
  TOKEN_KIND_EOF,  
  TOKEN_KIND_KEY_COUNT,
} TokenKind;

typedef struct {
  TokenKind kind;
  StringView lexeme;
} Token;

extern const char *const TOKEN_NAMES[TOKEN_KIND_KEY_COUNT];

typedef struct Expr Expr;
typedef struct Stmt Stmt;

typedef enum {
  EXPR_KIND_LITERAL,
  EXPR_KIND_IDENTIFIER,
  EXPR_KIND_ASSIGNMENT,  
} ExprKind;

typedef enum {
  LITERAL_KIND_INTEGER,
  LITERAL_KIND_STRING,  
} LiteralKind;

typedef struct {
  LiteralKind kind;
  union {
    i64 integer;
    StringView string;
  } as;
} LiteralExpr;

typedef struct {
  StringView name;
} IdentifierExpr;

typedef struct {
  StringView identifier;
  Expr *value;
} AssignmentExpr;

typedef struct Expr {
  ExprKind kind;
  union {
    LiteralExpr literal;
    IdentifierExpr identifier;
    AssignmentExpr assignment;
  } as;
} Expr;

typedef enum {
  STMT_KIND_VARIABLE_DECL,
  STMT_KIND_EXPR,
} StmtKind;

typedef struct {
  StringView identifier;
  Expr value;
  b8 is_const;
} VariableDeclStmt;

typedef struct {
  Expr expr;
} ExprStmt;  

typedef struct Stmt {
  StmtKind kind;
  union {
    VariableDeclStmt variable_decl;
    ExprStmt expr;
  } as;
} Stmt;

typedef struct {
  ArrayList *stmts;
} Program;

void ast_print(const Program *program);

#endif // BEE_AST_H
