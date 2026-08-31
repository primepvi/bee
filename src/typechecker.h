#ifndef BEE_TYPECHECKER_H
#define BEE_TYPECHECKER_H

#include "ast.h"
#include "libs/source.h"
#include "libs/string_view.h"
#include "libs/string_builder.h"
#include "libs/hashmap.h"
#include "libs/symbol_table.h"

typedef struct {
  Program *program;
  Source *source;
  SymbolTable *symbols;
  HashMap *type_env;
  Type expected_return_type;
} TypeChecker;

typedef struct {
  b8 can_continue;
} Flow;

#define FLOW_CONTINUE ((Flow){(true)})
#define FLOW_STOP ((Flow){false})
#define FLOW(can_continue) ((Flow){(can_continue)})

TypeChecker tc_create(Program *program, Source *source, SymbolTable *symbols);
void tc_define_type(TypeChecker *tc, Type type);
Type tc_get_type(TypeChecker *tc, TypeAnnotation annotation);
Type tc_get_raw_type(TypeChecker *tc, StringView name);

void tc_check(TypeChecker *tc);
Flow tc_check_stmt(TypeChecker *tc, Stmt *stmt);
Type tc_check_expr(TypeChecker *tc, Expr *expr);

b8 type_supports_unary_op(Type type, TokenKind operator_kind);
b8 type_supports_binary_op(Type left, Type right, TokenKind operator_kind);
b8 type_supports_logical_op(Type left, Type right, TokenKind operator_kind);

b8 type_is(Type type, StringView identifier);
b8 type_is_assignable(Type expected, Type received);
b8 type_is_equal(Type a, Type b);
b8 type_is_numeric(Type type);
b8 type_is_empty(Type type);

StringBuilder type_to_string(Type type);


#endif // BEE_TYPECHECKER_H
