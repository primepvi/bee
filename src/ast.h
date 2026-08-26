#ifndef BEE_AST_H
#define BEE_AST_H

#include "libs/types.h"
#include "libs/string_view.h"

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

typedef enum {
  EXPR_KIND_LITERAL,
} ExprKind;

typedef enum {
  LITERAL_KIND_INTEGER,
  LITERAL_KIND_STRING,  
} LiteralKind;

typedef struct {
  LiteralKind kind;
  union {
    i64 integer;
    const char *string;
  } as;
} LiteralExpr;

typedef struct {
  ExprKind kind;
  union {
    LiteralExpr literal;
  } as;
} Expr;

typedef enum {
  STMT_KIND_VARIABLE_DECL,
} StmtKind;

typedef struct {
  StringView identifier;
  Expr value;
  b8 is_const;
} VariableDeclStmt;

typedef struct {
  StmtKind kind;
  union {
    VariableDeclStmt variable_decl;
  } as;
} Stmt;

typedef struct {
  Stmt *stmts;
  u32 stmts_length, stmts_capacity;
} Program;

void ast_print(const Program *program);

#endif // BEE_AST_H
