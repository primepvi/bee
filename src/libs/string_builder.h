#ifndef BEE_STRING_BUILDER_H
#define BEE_STRING_BUILDER_H

#include "types.h"
#include "string_view.h"

typedef struct {
  char *buffer;
  u32 length, capacity;
} StringBuilder;

StringBuilder string_builder_from_cstr(const char *str);
StringBuilder string_builder_from_view(StringView view);

StringBuilder string_builder_new(u32 initial_capacity);
void string_builder_destroy(StringBuilder *sb);

void string_builder_append(StringBuilder *sb, StringView sv);
void string_builder_append_cstr(StringBuilder *sb, const char *str);
void string_builder_append_char(StringBuilder *sb, char c);

void string_builder_reserve(StringBuilder *sb, u32 size);
StringView string_builder_to_view(StringBuilder *sb);

#endif
