#ifndef BEE_ARRAY_LIST_H
#define BEE_ARRAY_LIST_H

#include "types.h"

typedef struct {
  void *buf;
  u32 buf_length, buf_capacity;
  u32 element_size;
} ArrayList;

typedef b8 (*ArrayListComparePredicate)(void *current, void *expected);
typedef b8 (*ArrayListFindPredicate)(void *current);

ArrayList *array_list_new(u32 buf_capacity, u32 element_size);
ArrayList *array_list_new_ptr(u32 buf_capacity);

void array_list_destroy(ArrayList *array);

u32 array_list_push(ArrayList *array, void *element);
u32 array_list_push_ptr(ArrayList *array, void *ptr);

b8 array_list_replace(ArrayList *array, u32 index, void *element);
void *array_list_pop(ArrayList *array);

void *array_list_at(ArrayList *array, u32 index);
void *array_list_at_ptr(ArrayList *array, u32 index);

i32 array_list_find_index(ArrayList *array, ArrayListComparePredicate predicate, void *expected);
void *array_list_find(ArrayList *array, ArrayListFindPredicate predicate);

u32 array_list_length(ArrayList *array);
u32 array_list_capacity(ArrayList *array);

#endif // BEE_ARRAY_LIST_H
