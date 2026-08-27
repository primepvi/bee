#include "source.h"
#include "array_list.h"
#include "string_view.h"
#include <stdio.h>
#include <stdlib.h>

Source source_from_file(const char *path) {
  FILE *file = fopen(path, "r");
  if (file == NULL) {
    fprintf(stderr, "ERROR: cannot read file in path: %s\n", path);
    exit(1);
  }

  fseek(file, 0, SEEK_END);
  u64 size = ftell(file);
  rewind(file);

  char *data = malloc(size + 1);
  if (data == NULL) {
    fclose(file);
    fprintf(stderr, "ERROR: cannot alloc buffer for file in path: %s\n", path);
    exit(1);
  }

  fread(data, 1, size, file);
  data[size] = '\0';

  fclose(file);

  return (Source){.buffer = data,
                  .name = path,
                  .is_file = true,
                  .lines = string_view_split_by_char(SV_LIT(data), '\n')};
}

Source source_from_string(const char *name, char *buffer) {
  return (Source){.buffer = (char*)buffer,
                  .name = name,
                  .is_file = false,
                  .lines = string_view_split_by_char(SV_LIT(buffer), '\n')};
}

void source_free(Source *source) {
  if (source->is_file) {
    free(source->buffer);
  }    
  
  array_list_destroy(source->lines);
}
