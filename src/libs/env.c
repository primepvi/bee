#include "env.h"
#include "hashmap.h"
#include "string_view.h"

Env env_create(Env *parent) {
  Env env = {0};
  env.parent = parent;
  env.map = hashmap_new(32);

  return env;
}

EnvEntry* env_get(Env *env, StringView key) {
  return (EnvEntry *)hashmap_get(env->map, key);
}

void env_set(Env *env, StringView key, EnvEntry entry) {
  hashmap_put(env->map, key, sizeof(EnvEntry), &entry);
}

b8 env_has(Env *env, StringView key) {
  return hashmap_has(env->map, key);
}  
