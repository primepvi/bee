CC := gcc
CFLAGS := -Wall -Wextra -g -O0

SRCS := $(shell find src -name "*.c")
OBJS := $(patsubst src/%.c,build/%.o,$(SRCS))
TARGET := build/bee

.PHONY: all build clean

all: $(TARGET)

$(TARGET): $(OBJS) 
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(OBJS) -o $(TARGET)

build/%.o: src/%.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf build
