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

#### Tuples

Tuples act similarly to Rust; They are denoted with parentheses and can contain any type.
They allow the return of multiple values from functions.
An empty tuple can be used to denote the unit type, and is equivalent to the `void` type.

## Constants

There are 3 types on constants in Warp; These are denoted with the `const`, `static`, and `progmem` keywords.

`const` works similarly to `const` in Go, or `#define` in C, where the value is just copied wherever it is used.
You cannot mutate a constant, and cannot hold a reference of it.
Constants do not need types.

A `static` is a global value stored in memory. Statics can be mutated and references can be taken.


`progmem` is short for "program memory", meaning that the value is stored along with the program itself.
Since program memory constants are stored on a seperate memory chip, references cannot be taken.
Program memory constants should be used when large amounts of data need to be dynamically referenced,
such as arrays or strings.

### When to use each type

Each type of constant has its own unique use case.
This table will help compare the pros and cons of each type:

|    Type   | Location | References | Mutable | Duplicated |
|-----------|:--------:|:----------:|:-------:|:----------:|
| `static`  |   RAM    |     ✅     |    ✅   |      ❌    |
| `const`   |   ROM    |     ✅     |    ✅   |      ✅    |
| `progmem` |   ROM    |     ❌     |    ❌   |      ❌    |

## Functions

Functions 

## Namespaces

Namespaces work similarly to modules in Rust.
Inline namespaces are created with `namespace {identifier} {}`.
If instead of braces and your namespace contents, you place a semicolon,
Warp will search for a namespace file in the current namespace's subfolder.

## Visibility
