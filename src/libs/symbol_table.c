#include "symbol_table.h"
#include "hashmap.h"

SymbolTable symtable_new(SymbolTable *parent) {
  SymbolTable table = {0};
  table.env = hashmap_new(32);
  table.parent = parent;
  return table;
}

void symtable_destroy(SymbolTable *table) {
  hashmap_destroy(table->env);
}  

void symtable_put(SymbolTable *table, Symbol symbol) {
  hashmap_put(table->env, symbol.identifier, sizeof(Symbol), &symbol);
}

b8 symtable_has(SymbolTable *table, StringView identifier) {
  if (table == NULL) {
    return false;
  }

  return hashmap_has(table->env, identifier) ||
         symtable_has(table->parent, identifier);
}

Symbol *symtable_get(SymbolTable *table, StringView identifier) {
  if (table == NULL) {
    return NULL;
  }

  return hashmap_has(table->env, identifier) ?
    hashmap_get(table->env, identifier) : symtable_get(table->parent, identifier);
}

b8 symtable_scope_has(SymbolTable *table, StringView identifier) {
  if (table == NULL) {
    return false;
  }

  return hashmap_has(table->env, identifier);
}

Symbol *symtable_scope_get(SymbolTable *table, StringView identifier) {
  if (table == NULL) {
    return NULL;
  }

  return hashmap_get(table->env, identifier);
}  
