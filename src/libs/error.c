#include "error.h"
#include "string_view.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

void error_throw_fmt(ErrorContext *ctx, const char *format, ...) {
  va_list args, args_copy;
  va_start(args, format);
  va_copy(args_copy, args);

  i32 length = vsnprintf(NULL, 0, format, args_copy);
  va_end(args_copy);
  if (length < 0) {
    va_end(args);
    return;
  }

  char *message = malloc(length + 1);
  vsnprintf(message, length + 1, format, args);
  va_end(args);

  error_throw(ctx, message);
  free(message);
}

void error_throw(ErrorContext *ctx, const char *message) {
  Source *source = ctx->source;
  Span span = ctx->span;

  StringView *line = array_list_at(source->lines, span.line - 1);
  char *line_buffer = string_view_to_cstr(*line);

  fprintf(stderr, "%s:%u:%u: error: %s\n\n %u | %s\n", source->name, span.line,
          span.start, message, span.line, line_buffer);

  u32 gutter_width = snprintf(NULL, 0, "%u", span.line);

  for (u32 i = 0; i < gutter_width + 3 + span.start; i++)
    fputc(' ', stderr);

  for (u32 i = span.start; i < span.end; i++)
    fputc('^', stderr);

  fputc('\n', stderr);
  exit(1);
}
