# Assembler: X86-64

Source: **Introduction to Compilers and Language Design** by Douglas Thain

## Elements

Regardless of CPU architecture, assembly code has three different kinds of
elements.

1. Directives
2. Labels
3. Instructions

### Directives

Begin with a dot and indicate structural information useful to the assembler,
linker, or debugger.

- `.file`: the name of the original source file.
- `.data`: the start of the data segment of a program.
- `.text`: the start of the program segment.
- `.string`: a string constant within the data section.
- `.global main`: names the global `main` that can be accessed by other code
  modules.

### Labels

End with a colon and indicate by their position the association between names
and locations. The label `main:`, for example, indicates that the following
instruction is the first instruction of the main function. Labels do not become
part of the machine code, but they are present in the resulting object code for
the purposes of linking and debugging.

### Instructions

The actual assembly code. Usually indented to distinguish it from directives and
labels. Instructions are usually not case-sensitive but are formatted
fully-uppercase for consistency.

## Registers and Data Types

These registers are almost general purpose because earlier versions of the
processors intended for each register to be used for a specific purpose, and not
all instructions could be applied to every register. The names of the lower
eight registers indicate the purpose for which each was intended. `%rax`, for
example, is the accumulator.

### Almost General Purpose 64-bit Registers

1. `%rax`
2. `%rbx`
3. `%rcx`
4. `%rdx`
5. `%rsi`
6. `%rdi`
7. `%rbp`
8. `%rsp`
9. `%r8`
10. `%r9`
11. `%r10`
12. `%r11`
13. `%r12`
14. `%r13`
15. `%r14`
16. `%r15`

### Syntax

With AT&T syntax, the source is first and the destination is second. Intel
syntax dispenses with percent signs and reverses the order of the arguments.

| AT&T              | Intel           |
| ----------------- | --------------- |
| `MOVQ %rsp, %rbp` | `MOVQ rbp, rsp` |

### Restricted Registers

| name              | purpose           |
| ----------------- | ----------------- |
| `%rsi` and `%rdi` | string processors |
| `%rsp`            | stack pointer     |
| `%rbp`            | base pointer      |

### Register Structure

| name            | size        |
| --------------- | ----------- |
| `%al` and `%ah` | 8 bits each |
| `%ax`           | 16 bits     |
| `%eax`          | 32 bits     |
| `%rax`          | 64 bits     |

| name   | size    |
| ------ | ------- |
| `%r8b` | 8 bits  |
| `%r8w` | 16 bits |
| `%r8d` | 32 bits |
| `%r8`  | 64 bits |

### Addressing Modes

A single letter suffix determines the size of data to be moved.

| suffix | name     | size    |
| ------ | -------- | ------- |
| `B`    | BYTE     | 1 byte  |
| `W`    | WORD     | 2 bytes |
| `L`    | LONG     | 4 bytes |
| `Q`    | QUADWORD | 8 bytes |

| Mode          | Example                         |
| ------------- | ------------------------------- |
| Global Symbol | `MOVQ x, %rax`                  |
| Immediate     | `MOVQ $56, %rax`                |
| Register      | `MOVQ %rbx, %rax`               |
| Indirect      | `MOVQ (%rsp), %rax`             |
| Base-Relative | `MOVQ -8(%rbp), %rax`           |
| Complex       | `MOVQ -16(%rbx, %rcx, 8), %rax` |

### Load Effective Address

Loads the address of a variable instead of its value.

| Mode          | Example                         |
| ------------- | ------------------------------- |
| Global Symbol | `LEAQ x, %rax`                  |
| Base-Relative | `LEAQ -8(%rbp), %rax`           |
| Complex       | `LEAQ -16(%rbx, %rcx, 8), %rax` |

## Basic Arithmetic

### `ADD` and `SUB`

Have two operands: a source and a destructive target.

`ADDQ %rbx, %rax` adds `%rbx` to `%rax` and places the result in `%rax`,
overwriting the previous value.

```asm
; === expression ===

; c = a + b + b

; === assembly ===

MOVQ    a, %rax
MOVQ    b, %rbx
ADDQ %rbx, %rax
ADDQ %rbx, %rax
MOVQ %rax, c
```

### `IMUL`

Because multiplying two 64-bit integers results in a 128-bit integer, `IMUL`
takes a single argument, multiplies it by the contents of `%rax` and then places
the low 64 bits of the result in `%rax` and then, implicitly, places the high 64
bits in `%rdx`.

```asm
; === expression ===

; c = b * (b + a)

; === assembly ===

MOVQ     a, %rax
MOVQ     b, %rbx
ADDQ  %rbx, %rax
IMULQ %rbx
MOVQ  %rax, c
```

### `IDIV`

Computes the same as `IMUL` but in reverse. The quotient is placed in `%rax` and
the remainder in `%rdx`. To set up division, `%rax` must be sign-extended into
`%rdx`.

```asm
MOVQ a, %rax    ; set the low 64 bits of the dividend
CQO             ; sign-extend %rax into %rdx
IDIVQ $5        ; divide %rdx:%rax by 5, leaving the result in %rax
```

## Comparisons and Jumps

```asm
; === infinite loop ===

      MOVQ $0, %rax
loop: INCQ %rax
      JMP  loop
```

| instruction   | meaning                  |
| ------------- | ------------------------ |
| `JE`          | jump if equal            |
| `JNE`         | jump if not equal        |
| `JL`          | jump if less             |
| `JLE`         | jump if less or equal    |
| `JG`          | jump if greater          |
| `JGE`         | jump if greater or equal |

```asm
; === count 0 through 5 ===

      MOVQ  x, %rax
loop: INCQ %rax
      CMPQ $5, %rax
      JLE  loop
```

## Stack

The stack is an auxiliary data structure used primarily to record the function
call history of the program along with local variables that do not fit in
registers. By convention, the stack grows downward from high values to low
values. The `%rsp` register is known as the **stack pointer** and keeps track of
the bottom-most item on the stack.

### Push, Pop, and Drop

```asm
; === push ===
SUBQ   $8, %rsp
MOVQ %rax, (%rsp)

; === pop ===
MOVQ (%rsp), %rax
ADDQ     $8, %rsp

; === drop ===
ADDQ $8, %rsp

; === dedicated 64-bit instructions ===
PUSHQ %rax
POPQ  %rax
```

## Function Calling Conventions

Prior to the 64-bit architectures, a simple call stack convention was used:
arguments were pushed onto the stack in reverse order, then the function was
invoked with `CALL`. The function returned its result in `%eax` and the caller
then removed the arguments from the stack.

64-bit code, however, uses a register-based calling convention: **System V ABI**.

- The first six integer arguments (including pointers and other types that can
  be stored as integers) are placed in registers
  `%rdi, %rsi, %rdx, %rcx, %r8, and %r9` in that order.
- The first eight floating point arguments are placed in the registers
  `%xmm0-%xmm7` in that order.
- Arguments in excess of those registers are pushed onto the stack.
- If the function takes a variable number of arguments (like `printf`) then the
  `%rax` register must be set to the number of floating point arguments.
- The return value of the function is placed in `%rax`.

### System V ABI Register Assignments

| Register | Purpose       | Who Saves?   |
| -------- | ------------- | ------------ |
| `%rax`   | result        | not saved    |
| `%rbx`   | scratch       | callee saves |
| `%rcx`   | argument 4    | not saved    |
| `%rdx`   | argument 3    | not saved    |
| `%rsi`   | argument 2    | not saved    |
| `%rdi`   | argument 1    | not saved    |
| `%rbp`   | base pointer  | callee saves |
| `%rsp`   | stack pointer | callee saves |
| `%r8`    | argument 5    | not saved    |
| `%r9`    | argument 6    | not saved    |
| `%r10`   | scratch       | CALLER saves |
| `%r11`   | scratch       | CALLER saves |
| `%r12`   | scratch       | callee saves |
| `%r13`   | scratch       | callee saves |
| `%r14`   | scratch       | callee saves |
| `%r15`   | scratch       | callee saves |

### Defining Complex Functions

A complex function must be able to invoke other functions and compute
expressions of arbritrary complexity, and then return to the caller with the
original state intact.

The space between `%rbp` and `%rsp` is called the **stack frame**.

```asm
; Three-argument function that uses two local variables.
.global func
func:
    pushq %rbp      ; save the base pointer
    movq %rsp, %rbp ; set new base pointer
    pushq %rdi      ; save first argument on the stack
    pushq %rsi      ; save second argument on the stack
    pushq %rdx      ; save third argument on the stack
    subq $16, %rsp  ; allocate two more local variables
    pushq %rbx      ; save callee-saved registers
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15

    ; === body of function goes here ===

    popq %r15       ; restore callee-saved registers
    popq %r14
    popq %r13
    popq %r12
    popq %rbx
    movq %rbp, %rsp ; reset stack pointer
    popq %rbp       ; recover previous base pointer
    ret             ; return to the caller
```

| Contents              | Address     | Pointers              |
| --------------------- | ----------- | --------------------- |
| old `%rip` register   | `8(%rbp)`   |                       |
| old `%rbp` register   | `(%rbp`     | <- `%rbp` points here |
| argument 0            | `-8(%rbp)`  |                       |
| argument 1            | `-16(%rbp)` |                       |
| argument 2            | `-24(%rbp)` |                       |
| local variable 0      | `-32(%rbp)` |                       |
| local variable 1      | `-40(%rbp)` |                       |
| saved register `%rbx` | `-48(%rbp)` |                       |
| saved register `%r12` | `-56(%rbp)` |                       |
| saved register `%r13` | `-64(%rbp)` |                       |
| saved register `%r14` | `-72(%rbp)` |                       |
| saved register `%r15` | `-80(%rbp)` | <- `%rsp` points here |
