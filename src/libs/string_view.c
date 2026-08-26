#include "string_view.h"
#include <stdlib.h>
#include <string.h>

StringView string_view_create(const char *data, u32 length) {
  return (StringView){data, length};
}

StringView string_view_slice(StringView source, u32 start, u32 end) {
  return string_view_create(source.data + start, end);
}

StringView string_view_slice_while(StringView source,
                                   StringViewPredicate predicate) {
  u32 cursor = 0;
  while (cursor < source.length && predicate(source.data[cursor]))
    cursor++;

  return string_view_slice(source, 0, cursor);
}

StringView string_view_slice_start(StringView source, u32 count) {
  return string_view_create(source.data + count, source.length - count);
}

char *string_view_to_cstr(StringView view) {
  char *ptr = malloc(view.length + 1);
  strncpy(ptr, view.data, view.length);
  ptr[view.length] = '\0';

  return ptr;
}

char string_view_at(StringView view, u32 index) {
  return index > view.length ? '\0' : view.data[index];
}

b8 string_view_is_empty(StringView view) {
  string_view_trim(&view);
  return view.length == 0;
}

b8 string_view_is_equal(StringView a, StringView b) {
  return a.length == b.length && memcmp(a.data, b.data, a.length) == 0;
}

b8 string_view_starts_with(StringView source, StringView prefix) {
  return source.length >= prefix.length &&
         memcmp(source.data, prefix.data, prefix.length) == 0;
}

b8 string_view_ends_with(StringView source, StringView suffix) {
  return source.length >= suffix.length &&
         memcmp(source.data + source.length - suffix.length, suffix.data,
                suffix.length) == 0;
}

u32 string_view_trim_left(StringView *view) {
  u32 cursor = 0;
  while (cursor < view->length && view->data[cursor] == ' ')
    cursor++;

  view->data += cursor;
  view->length -= cursor;

  return cursor;
}

u32 string_view_trim_right(StringView *view) {
  u32 cursor = 0;
  while (cursor < view->length && view->data[view->length - cursor - 1] == ' ')
    cursor++;

  view->length -= cursor;

  return cursor;
}

u32 string_view_trim(StringView *view) {
  return string_view_trim_left(view) + string_view_trim_right(view);
}

u32 string_view_chop_while(StringView *view, StringViewPredicate predicate) {
  u32 cursor = 0;
  while (cursor < view->length && predicate(view->data[cursor]))
    cursor++;

  view->data += cursor;
  view->length -= cursor;

  return cursor;
}

u32 string_view_chop_left(StringView *view, u32 count) {
  if (count >= view->length) {
    count = view->length;
  }

  view->data += count;
  view->length -= count;

  return count;
}

u32 string_view_chop_right(StringView *view, u32 count) {
  if (count >= view->length) {
    count = view->length;
  }

  view->length -= count;

  return count;
}
