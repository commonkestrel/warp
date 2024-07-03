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

The language also includes arrays, denoted by the `{type}[{length}]` syntax.

