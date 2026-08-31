#ifndef BEE_INTERPRETER_H
#define BEE_INTERPRETER_H

#include "ast.h"
#include "libs/hashmap.h"
#include "libs/symbol_table.h"
#include "libs/types.h"
#include "libs/source.h"

typedef struct {
  Source *source;
  Program *program;
  SymbolTable *symbols;
} Interpreter;

typedef enum {
  RESULT_KIND_NORMAL,
  RESULT_KIND_RETURN,
} ResultKind;

typedef struct {
  ResultKind kind;
  Value value;
} Result;

#define RESULT_NORMAL()                                                        \
  ((Result){.kind = (RESULT_KIND_NORMAL)})

#define RESULT_RETURN(value)                                                    \
  ((Result){.kind = (RESULT_KIND_RETURN), .value = (value)})

Interpreter interpreter_create(Program *program, Source *source, SymbolTable *symbols);
void interpreter_eval(Interpreter *interpreter);
Value interpreter_eval_expr(Interpreter *interpreter, Expr *expr);
Result interpreter_eval_stmt(Interpreter *interpreter, Stmt *stmt);

#endif // BEE_INTERPRETER_H
