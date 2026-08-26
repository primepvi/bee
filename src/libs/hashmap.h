#ifndef BEE_HASHMAP_H
#define BEE_HASHMAP_H

#include "string_view.h"
#include "types.h"

typedef enum {
  HASHMAP_ENTRY_EMPTY = 0,
  HASHMAP_ENTRY_OCCUPIED,
  HASHMAP_ENTRY_REMOVED,
} HashMapEntryState;

typedef struct {
  StringView key;
  void *value;
  HashMapEntryState state;
} HashMapEntry;

typedef struct {
  HashMapEntry *entries;
  u32 entries_capacity;
  u32 entries_len;
} HashMap;

HashMap *hashmap_new(u32 capacity);
void hashmap_destroy(HashMap *map);
void hashmap_grow_capacity(HashMap *map, u32 factor);

void hashmap_put(HashMap *map, StringView key, u32 element_size, void *value);
void *hashmap_get(HashMap *map, StringView key);

// must free removed element before use.
void *hashmap_remove(HashMap *map, StringView key);
b8 hashmap_has(HashMap *map, StringView key);

#endif // BEE_HASHMAP_H
