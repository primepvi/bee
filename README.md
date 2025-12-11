# 🐝 **bee**

A small experimental programming language with static typing, static scoping,
and LLVM-based code generation.

## **Example**

More examples can be found on [examples](./examples/) folder

```typescript
const message = "Hello, World";
const response: @static string = "Hello, Bee";
var fvalue: float32;

begin :a
  var age: @static int32 = 18;
  var year: int32 = 2025;
  
  begin
    var age: int32 = 20;
    var other_age = age;
    age = 18;
    fvalue = -15.5;
  end 
end
```

## **Build**

Requires Rust and LLVM.

```console
$ cargo build --release
```

## **Usage**

Run (JIT):

```console
$ bee examples/block.bee
```

Compile to LLVM IR:

```console
$ bee -c examples/block.bee -o examples/block.ll
```

## **Status**

Early development. Includes lexer, parser, semantic analysis, and basic LLVM IR
generation.
