#ifndef BEE_ERROR_H
#define BEE_ERROR_H

#include "source.h"
#include "string_view.h"
#include "../ast.h"

typedef struct {
  Source *source;
  Span span;
} ErrorContext;

void error_throw_fmt(ErrorContext *ctx, const char *format, ...);
void error_throw(ErrorContext *ctx, const char *message);

#endif // BEE_ERROR_H
