#ifndef BEE_ENV_H
#define BEE_ENV_H

#include "string_view.h"
#include "hashmap.h"
#include "types.h"
#include "../ast.h"

typedef enum {
  VALUE_KIND_INTEGER,
  VALUE_KIND_STRING,
  VALUE_KIND_BOOLEAN,  
} ValueKind;  

typedef struct {
  ValueKind kind;
  union {
    i64 integer;
    StringView string;
    b8 boolean;
  } as;
} Value;

typedef enum {
  ENV_ENTRY_KIND_VARIABLE,
} EnvEntryKind;

typedef struct {
  StringView identifier;
  b8 constant;
  Value value;
} EnvVariable;

typedef struct {
  EnvEntryKind kind;
  union {
    EnvVariable variable;
  } as;
} EnvEntry;

typedef struct Env {
  HashMap *map;
  struct Env *parent;
} Env;

Env env_create(Env *parent);
EnvEntry *env_get(Env *env, StringView key);
void env_set(Env *env, StringView key, EnvEntry entry);
b8 env_has(Env *env, StringView key);

#endif // BEE_ENV_H
