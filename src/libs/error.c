#include "error.h"
#include "string_view.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#define ANSI_RESET      "\033[0m"
#define ANSI_BOLD       "\033[1m"
#define ANSI_DIM        "\033[2m"
#define ANSI_RED        "\033[31m"
#define ANSI_BRIGHT_RED "\033[91m"
#define ANSI_CYAN       "\033[36m"
#define ANSI_WHITE "\033[37m"
#define ANSI_YELLOW  "\033[33m"
#define ANSI_BRIGHT_WHITE "\033[97m"

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

  u32 gutter_width = snprintf(NULL, 0, "%u", span.line);

  // error[main.lang:12:8]: message
  fprintf(stderr,
          "%s%serror%s"
          "%s[%s:%u:%u]%s: "
          "%s%s%s\n",
          ANSI_BOLD,
          ANSI_BRIGHT_WHITE,
          ANSI_RESET,
          ANSI_CYAN,
          source->name,
          span.line,
          span.start,
          ANSI_RESET,
          ANSI_RED,
          message,
          ANSI_RESET);

  // empty previous line
  fprintf(stderr,
          "%*s %s|%s\n",
          gutter_width,
          "",
          ANSI_DIM,
          ANSI_RESET);

  // code line
  fprintf(stderr,
          "%s%*u%s %s|%s %s\n",
          ANSI_BOLD,
          gutter_width,
          span.line,
          ANSI_RESET,
          ANSI_DIM,
          ANSI_RESET,
          line_buffer);

  // error emphasis
  fprintf(stderr,
          "%*s %s|%s ",
          gutter_width,
          "",
          ANSI_DIM,
          ANSI_RESET);

  for (u32 i = 1; i < span.start; i++)
    fputc(' ', stderr);

  fprintf(stderr, "%s%s", ANSI_BOLD, ANSI_BRIGHT_RED);

  u32 length = span.end > span.start
             ? span.end - span.start
             : 1;

  for (u32 i = 0; i < length; i++)
    fputc('^', stderr);

  fprintf(stderr, "%s\n", ANSI_RESET);

  free(line_buffer);
  exit(1);
}
