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

|    Type   | Location | References | Mutable | Duplicated | Type Required |
|-----------|:--------:|:----------:|:-------:|:----------:|:-------------:|
| `static`  |   RAM    |     ✅     |    ✅   |     ❌    |       ✅      |
| `const`   |   ROM    |     ❌     |    ❌   |     ✅    |       ❌      |
| `progmem` |   ROM    |     ❌     |    ❌   |     ❌    |       ✅      |

## Functions

Functions are a way to minimise repeated blocks of code.
They accept parameters and can be called anywhere in your code, and will always return to the location it was called.
Functions can return values, and must always contain a return statement.
If the function's return type is `()` or `void`, you can return without a value.

## Conditionals & Loops

The only conditional in Warp is the `if` statement.
These statements allow you to only run code if a specific condition is satisfied.
If statements can also contain an `else` clause,
which specifies code to run if the condition is not satisfied.

If statements have the following syntax:
```rs
if (/* boolean condition */) {
    /* true content */
} else {
    /* false content */
}
```

These conditionals are used in loops, those being the `for` and `while` loops.
While loops allow a block of code to run over and over for as long as a condition is satisfied.
Their syntax is the following:
```rs
while (/* boolean condition */) {
    /* while true content */
}
```

For loops are similar to `while` loops, but offer a more convenient way to run a loop a specific number of times.
For loops have three components, and initializer, a condition, and an update.
The initializer is run before the loop starts, and is used to initialize the loop's state.
The condition is a boolean condition that, similar to a while loop, is checked before each run of the loop.
The update is a block that runs after each iteration of the loop, before the condition is checked.

The syntax of a `for` loop is the following: 
```rs
for (/* initializer */; /* condition */; /* update */) {
    /* content */
}
```

This is equivalent to the following `while` loop:
```rs
/* initializer */
while (/* condition */) {
    /* content */
    /* update */
}
```

## Subspaces

Subspaces work similarly to modules in Rust.
Inline subspaces are created with `subspace {identifier} {}`.
If instead of braces and your subspace contents, you place a semicolon,
Warp will search for a subspace file in the current subspace's subfolder.

subspace subfolders allow for a sense of hierarchy in the file tree.
The project's subfolder is the project root, and all subspaces' subfolders are the identifier of the subspace.

For example, let's imagine a project called `main`,
where `main.warp` contains `subspace child;`, and `child.warp` contains `subspace grandchild`.

The file tree would look something like so:

```
main/
├─ child/
│  ├─ grandchild.warp
├─ child.warp
├─ main.warp
```

## Imports

Packages allow items to be used in your code that you did not create or do not have locally.
Instead of a package file, such as `Cargo.toml` or `go.mod`,
packages are imported through comments.
For example, Warp's standard library is imported like so:
``` rs
//! lib std = https://github.com/commonkestrel/warp-std
```
The Warp compiler will cache the git repository and include it as a package for your file.

While by default the HEAD branch (`main`, `master`, `trunk`, etc...) and the most recent commit is used,
you can specify these values like so:
```rs
//! lib std = git(url = https://github.com/commonkestrel/warp-std, branch = main, commit = 5a6bb6d)
```

You are also able to import local packages via that `path` argument:
```rs
//! lib std = path(./my-standard)
```

All of this allows projects to be completely stored within a single file.
This significantly improves the readability and compactness of projects.

To create a library, all you need is a directory or repository with a file titled `lib.warp` at the project root.
When linking your library, this is treated as the project root by the compiler.
Anything marking `pub` in this file can be accessed by external projects.

## Visibility

Constants and functions can either be private, protected, or public.
Private means that the item can only be accessed within the same file.
Protected means that only files in the same project can access the item.
Public means that any file is able to access or import the item.

Items are private by default, and they can be specified either protected or public with the modifiers `prot` and `pub` respectively.
