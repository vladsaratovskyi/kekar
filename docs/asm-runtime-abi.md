# Kek ASM Runtime ABI (Stage-0)

This document defines the current runtime ABI used by the ASM backend.

## Register and value model
- Scalar `Num`, `Bool`, `Char`: immediate 64-bit value in registers/stack slots.
- Heap values (`struct`, `class`, arrays): raw pointer to heap object.
- `String`: same runtime layout as `Array<Char>` (heap pointer with `len + elements` slots).

## Calling convention
- System V x86_64 register arguments.
- Function args: `rdi`, `rsi`, `rdx`, `rcx`, `r8`, `r9`.
- Return value: `rax`.
- Methods declared inline in `struct`/`class` or in `impl` blocks are lowered to static labels `<Type>__<method>`.
- Method call ABI: receiver (`this`) in `rdi`, then explicit args in `rsi..r9`.

## Heap allocator
- Runtime defines a bump allocator `__kek_alloc` over a static 1 MiB buffer in `.bss`.
- `__kek_runtime_init` initializes `__kek_heap_ptr` at startup.
- `__kek_alloc` input: `rdi = bytes`, output: `rax = pointer`.

## Layouts
- Struct/class object layout: contiguous 8-byte fields in declaration order.
  - Field offset = `index * 8`.
- Array layout (used by both `Array<T>` and `T[]` source types):
  - `[0]` = length (`i64`)
  - `[8 + i*8]` = element `i`

## Array runtime helpers
- `__kek_array_len(arr) -> Num`
- `__kek_array_is_empty(arr) -> Bool`
- `__kek_array_push(arr, value) -> arr'`
- `__kek_array_pop(arr) -> value`

Semantics:
- `push` returns a newly allocated appended array.
- `pop` returns last element and decrements in-place length (`0` when empty).

## Constructor lowering
- `TypeName(...)` lowers to object allocation plus field initialization.
- If class has `init`, constructor allocates object then calls `<Type>__init`.

## String runtime helpers (compatibility)
- `__kek_string_len(text: String) -> Num`
- `__kek_string_concat(left: String, right: String) -> String`
- `__kek_string_eq(left: String, right: String) -> Bool`
- `__kek_string_is_empty(text: String) -> Bool`
- `__kek_string_starts_with(text: String, prefix: String) -> Bool`
- `__kek_string_char_at(text: String, index: Num) -> Char`

Semantics:
- `concat` allocates a new `Array<Char>` payload and copies left + right.
- `char_at` returns `0` when `index` is out of bounds.

## Control flow
- `while` and `for` loops emit concrete labels for `break` and `continue`.
- `for item[, index] in arr` iterates array runtime layout (`len + elements`).
