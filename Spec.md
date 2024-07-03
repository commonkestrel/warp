# Warp Specification

Warp is a language designed for a very specific purpose, that being controlling the [f8ful](https://github.com/commonkestrel/fateful) CPU.
As such, it is a very simple language essentially designed to be a very low level abstraction over assembly.
It is similar in concept to C, except for the fact that it has a little nicer syntax and package management.

## Variables

Variables are defined with the basic syntax `{modifier} {identifier}: {type} = {value}`
Similar to Javascript, there are two keywords used to instanciate a variable: `let` and `mut`.
`let` denotes an immutable variable that cannot be changed after initialization.
`mut` denotes a mutable variable that can be mutated many times.

### Types

Warp has a very limited number of types in order to stay clean and uncluttered (also because I didn't want to deal with type resolution).

#### Integers

There are of course the basic integer types;
Similar to Rust and Zig, these are `u8`, `u16`, `u24`, `u32`, `u64` for the unsigned integers,
and `i8`, `i16`, `i24`, `i32`, `i64` for the signed integers.

Integer literals can be in the form of a decimal number, a hexadecimal number denoted by `0x` prepended,
an octal number denoted by `0o` prepended, or a binary number denoted by `0b` prepended.
The `u8` type can also be in the form of an ASCII character denoted by single-quotes.

#### Arrays

The language also includes arrays, denoted by the `{type}[]` syntax.
Arrays are initialized with bracket syntax: `[{contents}]`.
Arrays can be indexed with bracket syntax as well: `{identifier}[{index}]`
Arrays essentially act as a pointer with easier initialization and better readability.

Strings are another form of array, in that `u8[]` can be initialized with a ASCII string literal,
signified by surrounding double-quotes.

#### Pointers

Pointers in warp act very similarly to C, except for the fact that they are `const` by default.
This means that you are unable to write to `*T`; You are instead only able to write to `*mut T`.
Pointers are 16 bits wide, and are able to do arithmetic with `u8`, `u16`, `i8` and `i16`.
Pointers can be indexed just like arrays; `ptr[i]` returns `*(ptr + i*sizeof T)`, where ptr is of type `*T`.

## Constants

There are 3 types on constants in Warp; These are denoted with the `const`, `static`, and `progmem` keywords.
`const` works similarly to `const` in Go, or `#define` in C, where the value is just copied wherever it is used.
You cannot mutate a constant, and cannot hold a reference of it.
`static` is a global value stored in memory. Statics can be mutated 
