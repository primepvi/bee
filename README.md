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
$ ninja -C build
```

The executable will be generated at `build/bee`
