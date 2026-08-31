#ifndef BEE_SYMBOL_TABLE_H
#define BEE_SYMBOL_TABLE_H

#include "types.h"
#include "string_view.h"
#include "hashmap.h"
#include "../ast.h"

typedef struct {
  StringView identifier;
} Type;

typedef enum {
  SYMBOL_KIND_VARIABLE,
  SYMBOL_KIND_FUNCTION,
} SymbolKind;

typedef struct {
  Type type;
  b8 constant;
  Expr *value_expr;
  Value *value;
} SymbolVariable;

typedef struct {
  Type return_type;
  ArrayList *params_variables;
  FunctionDeclStmt *stmt;
  Value *value;
} SymbolFunction; 

typedef struct {
  SymbolKind kind;
  StringView identifier;
  union {
    SymbolVariable variable;
    SymbolFunction func;
  } as;
} Symbol;

typedef enum {
  SYMBOL_TABLE_KIND_GLOBAL,
  SYMBOL_TABLE_KIND_FUNCTION,
  SYMBOL_TABLE_KIND_BLOCK,
} SymbolTableKind;

typedef struct SymbolTable {
  SymbolTableKind kind;
  HashMap *env;
  struct SymbolTable *parent;
} SymbolTable;

SymbolTable symtable_new(SymbolTable *parent, SymbolTableKind kind);
void symtable_destroy(SymbolTable *table);

void symtable_put(SymbolTable *table, Symbol symbol);
b8 symtable_has(SymbolTable *table, StringView identifier);
Symbol *symtable_get(SymbolTable *table, StringView identifier);

b8 symtable_scope_has(SymbolTable *table, StringView identifier);
Symbol *symtable_scope_get(SymbolTable *table, StringView identifier);

#endif // BEE_SYMBOL_TABLE_H
