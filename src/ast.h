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
  TOKEN_KIND_TRUE,
  TOKEN_KIND_FALSE,
  TOKEN_KIND_AND,
  TOKEN_KIND_OR,  
  TOKEN_KIND_NOT,
  TOKEN_KIND_IDENTIFIER,  

  TOKEN_KIND_NUMBER,
  TOKEN_KIND_STRING,

  TOKEN_KIND_SEMICOLON,
  TOKEN_KIND_EQUAL,
  TOKEN_KIND_OPEN_PAREN,
  TOKEN_KIND_CLOSE_PAREN,
  TOKEN_KIND_GT,
  TOKEN_KIND_GTE,
  TOKEN_KIND_LT,
  TOKEN_KIND_LTE,
  TOKEN_KIND_EQEQ,
  TOKEN_KIND_NEQ,

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
  EXPR_KIND_LOGICAL,
  EXPR_KIND_UNARY,
  EXPR_KIND_PARENTHESIZED,
} ExprKind;

typedef struct {
  Token value_token;
} LiteralExpr;

typedef struct {
  Token identifier_token;
} IdentifierExpr;

typedef struct {
  Token identifier_token;
  Token assignment_token;
  Expr *value;
} AssignmentExpr;

typedef struct {
  Expr *left, *right;
  Token operator_token;
} BinaryExpr;

typedef struct {
  Expr *left, *right;
  Token operator_token;
} LogicalExpr;

typedef struct {
  Token operator_token;
  Expr *operand;
} UnaryExpr;

typedef struct {
  Token open_paren_token;
  Token close_paren_token;
  Expr *expr;
} ParenthesizedExpr;

typedef struct Expr {
  ExprKind kind;
  Span span;
  union {
    LiteralExpr literal;
    IdentifierExpr identifier;
    AssignmentExpr assignment;
    BinaryExpr binary;
    LogicalExpr logical;
    UnaryExpr unary;
    ParenthesizedExpr parenthesized;
  } as;
} Expr;

typedef enum {
  STMT_KIND_VARIABLE_DECL,
  STMT_KIND_EXPR,
  STMT_KIND_ECHO,
} StmtKind;

typedef struct {
  Token keyword_token;
  Token identifier_token;
  Token assignment_token;
  Expr value;
} VariableDeclStmt;

typedef struct {
  Expr expr;
} ExprStmt;

typedef struct {
  Token keyword_token;
  Expr message;
} EchoStmt;

typedef struct Stmt {
  StmtKind kind;
  union {
    VariableDeclStmt variable_decl;
    ExprStmt expr;
    EchoStmt echo;
  } as;
  Span span;
} Stmt;

typedef struct {
  ArrayList *stmts;
} Program;

void ast_print(const Program *program);
u32 ast_binary_operator_priority(TokenKind kind);
u32 ast_unary_operator_priority(TokenKind kind);

#endif // BEE_AST_H
