#ifndef BEE_SOURCE_H
#define BEE_SOURCE_H

#include "array_list.h"
#include "string_view.h"

typedef struct {
  const char *name;
  char *buffer;
  ArrayList *lines;
  b8 is_file;
} Source;

Source source_from_string(const char *name, char *buffer);
Source source_from_file(const char *path);
void source_free(Source *source);

#endif // BEE_SOURCE_H
