#ifndef BEE_STRING_VIEW_H
#define BEE_STRING_VIEW_H

#include "types.h"

typedef struct {
  const char *data;
  u32 length;
} StringView;

#define SV_FMT "%.*s"
#define SV_ARG(sv) (sv).length, (sv).data

typedef b8 (*StringViewPredicate)(char cur);

StringView string_view_create(const char *data, u32 length);
StringView string_view_from_cstr(const char *data);
StringView string_view_slice(StringView source, u32 start, u32 end);
StringView string_view_slice_while(StringView source,
                                   StringViewPredicate predicate);
StringView string_view_slice_start(StringView source, u32 count);

const char *string_view_to_cstr(StringView view);

b8 string_view_is_empty(StringView view);
b8 string_view_is_equal(StringView a, StringView b);
b8 string_view_starts_with(StringView source, StringView prefix);
b8 string_view_ends_with(StringView source, StringView suffix);

u32 string_view_trim_left(StringView *view);
u32 string_view_trim_right(StringView *view);
u32 string_view_trim(StringView *view);

u32 string_view_chop_while(StringView *view, StringViewPredicate predicate);
u32 string_view_chop_left(StringView *view, u32 count);
u32 string_view_chop_right(StringView *view, u32 count);

#endif // BEE_STRING_VIEW_H
