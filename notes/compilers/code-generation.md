# Code Generation

## Example 1

```
=== expression ===

c = a + 3 - b

=== Directed Acyclic Graph ===

assign
 /  \
c   isub
    / \
 iadd  b
  / \
 a   3

=== Intermediate Representation ===

1. MOVQ  a, R0
2. MOVQ $3, R1
3. ADDQ R0, R1
4. MOVQ  b, R0
5. SUBQ R0, R1
6. MOVQ R1, c

=== Assembly ===

MOVQ    a, %rbx
MOVQ   $3, %r10
ADDQ %r10, %rbx
MOVQ    b, %rbx
SUBQ %rbx, %r10
MOVQ %r10, c
```

## Peculiarities of Assembly

In the X86 architecture, `IMUL` takes only one argument because the first argument is
always `%rax`. The result is always placed in `%rax` with the overflow in `%rdx`.

```
=== expression ===

(x * 10)

=== assembly ===

MOV   $10, %rbx
MOV     x, %r10
MOV  %rbx, %rax
IMUL %r10
MOV  %rax, %r11
```

## Example 2

```
=== expression ===

a = f(10, b + c)

=== Directed Acyclic Graph ===

assign
 /  \
a   call
    / \
   f  arg
      / \
    10  arg
        / \
     iadd (null)
      / \
     b   c

=== assembly ===

MOV  $10, %rbx
MOVQ b, %r10
MOVQ c, %r11
ADDQ %r10, %r11
MOVQ %r11, %rsi
MOVQ %rbx, %rdi
PUSHQ %r10
PUSHQ %r11
CALL f
POPQ %r11
POPQ %r10
MOVQ %rax, %rbx
MOVQ %rbx, a
```

## Generating Control Flow

```
=== if-else ===

if ([expression]) {
    [true-statements]
} else {
    [false-statements]
}

--- outputs --->

    [expression]
    CMP $0, register
    JE false-label
    [true-statements]
    JMP done-label
false-label:
    [false-statements]
done-label:

=== for-loop ==

for ([init-expression]; [expression]; [next-expression]) {
    [body-statements]
}

--- outputs --->

    [init-expression]
top-label:
    [expression]
    CMP $0, register
    JE done-label
    [body-statements]
    [next-expression]
    JMP top-label
done-label:
```

## Generating Global Declarations

```
i: integer = 10;
s: string  = "hello";
b: array [4] boolean = {true, false, true, false};

--- outputs --->

.data
i: .quad 10
s: .string "hello"
b: .quad 1, 0, 1, 0
```
