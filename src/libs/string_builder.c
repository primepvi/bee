#include "string_builder.h"
#include "string_view.h"
#include <string.h>
#include <stdlib.h>

StringBuilder string_builder_from_cstr(const char *str) {
  StringBuilder sb = {0};
  sb.length = strlen(str);
  sb.capacity = sb.length + 1;
  sb.buffer = strdup(str);

  return sb;
}

StringBuilder string_builder_from_view(StringView view) {
  StringBuilder sb = {0};
  sb.length = view.length;
  sb.capacity = view.length + 1;
  sb.buffer = string_view_to_cstr(view);

  return sb;
}

StringBuilder string_builder_new(u32 initial_capacity) {
  StringBuilder sb = {0};
  sb.length = 0;
  sb.capacity = initial_capacity;
  sb.buffer = malloc(initial_capacity);
  sb.buffer[0] = '\0';

  return sb;
}

void string_builder_destroy(StringBuilder *sb) {
  free(sb->buffer);
  free(sb);
}

void string_builder_append(StringBuilder *sb, StringView sv) {
    u32 required = sb->length + sv.length + 1;

    if (required > sb->capacity) {
        string_builder_reserve(sb, required * 2);
    }

    memcpy(sb->buffer + sb->length, sv.data, sv.length);

    sb->length += sv.length;
    sb->buffer[sb->length] = '\0';
}

void string_builder_append_cstr(StringBuilder *sb, const char *str) {
    u32 str_length = strlen(str);
    u32 required = sb->length + str_length + 1;

    if (required > sb->capacity) {
        string_builder_reserve(sb, required * 2);
    }

    memcpy(sb->buffer + sb->length, str, str_length);

    sb->length += str_length;
    sb->buffer[sb->length] = '\0';
}

void string_builder_append_char(StringBuilder *sb, char c) {
    u32 required = sb->length + 2;

    if (required > sb->capacity) {
        string_builder_reserve(sb, required * 2);
    }

    sb->buffer[sb->length++] = c;
    sb->buffer[sb->length] = '\0';
}

void string_builder_reserve(StringBuilder *sb, u32 capacity) {
    if (capacity <= sb->capacity)
        return;

    sb->buffer = realloc(sb->buffer, capacity);
    sb->capacity = capacity;
}

StringView string_builder_to_view(StringBuilder *sb) {
  return string_view_create(sb->buffer, sb->length);
}
