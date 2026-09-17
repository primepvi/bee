# 🐝 bee

A tiny interpreted programming language written in C++.

## Example

```bee
fn sum(a: int, b: int) -> a + b

const a = 10
const b = 10

echo sum(a, b)
```

## Build

Configure the project with CMake:
```console
$ cmake -S . -B build -G Ninja
```

Build with Ninja:
```console
$ ninja -C build
```

The executable will be generated at:
```text
build/bee
```

## Run
Run the interpreter:
```console
$ ./build/bee
```

## Status
BEE is currently under development. The language syntax, standard library, and runtime are subject to change.
