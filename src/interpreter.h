#ifndef BEE_INTERPRETER_H
#define BEE_INTERPRETER_H

#include "ast.h"
#include "libs/env.h"
#include "libs/types.h"
#include "libs/source.h"

typedef struct {
  Source *source;
  Program *program;
  Env global_env;
} Interpreter;

Interpreter interpreter_create(Source *source, Program *program);
void interpreter_eval(Interpreter *interpreter);
Value interpreter_eval_expr(Interpreter *interpreter, Expr *expr);
void interpreter_eval_stmt(Interpreter *interpreter, Stmt *stmt);

#endif // BEE_INTERPRETER_H
