# Intermediate Representation

## Expressions -> ASTs -> DAGs

```
=== Expression ===
 
x = (a + 10) * (a + 10)

=== Abstract Syntax Tree ===

assign
 /  \
x   mul
    / \
  add add
  /\   /\
 a 10 a 10

=== Directed Acyclic Graph ===

assign
 /  \
x   fmul
     ||
    fadd
    /  \
   a   itof
        |
        10

=== Expression ===

x = a[i]

=== Abstract Syntax Tree ===

assign
 /  \
x  lookup
     /\
    a  i

=== Directed Acyclic Graph ===

assign
 /  \
x  deref
     |
    iadd
    / \
   a  imul
       /\
      i  4 <- size of data in array

--- frame-pointer address computations ---

assign
 /  \
x  deref
     |
    iadd
    / \
 deref imul
   |   |  \
iadd deref 4
  /|   |
fp 16 iadd
      / \
    fp   20

--- portable, external representation ---

(ASSIGN x (DEREF (IADD (DEREF (IADD FP 16))
                       (IMUL (DEREF (IADD FP 20)) 4))))
```

## Constant Folding

```
=== Expression ===

seconds = days * 24 * 60 * 60

--- fold --->

   assign
   /    \
seconds imul
        / \
     days imul
          / \
        imul 24
        / \
       60 60

--- fold --->

   assign
   /    \
seconds imul
        / \
     days imul
          / \
        3600 24

--- fold --->

   assign
   /    \
seconds imul
        / \
     days 86400
```

## Linear IR

An idealized assembly language.

```
=== expression ===

x = (a + 10) * (a + 10)

=== Linear IR ===

1. LOAD a        -> %r1
2. LOAD $10      -> %r2
3. ITOF %r2      -> %r3
4. FADD %r1, %r3 -> %r4
5. FMUL %r4, %r4 -> %r5
6. STOR %r5      -> x

=== Lifetimes ===

1. LOAD a        -> %r1   live: %r1
2. LOAD $10      -> %r2   live: %r1 %r2
3. ITOF %r2      -> %r3   live: %r1 %r2 %r3
4. FADD %r1, %r3 -> %r4   live: %r1 %r3 %r4
5. FMUL %r4, %r4 -> %r5   live: %r4 %r5
6. STOR %r5      -> x     live: %r5
```

## Stack Machine IR

```
PUSH a
PUSH 10
ITOF
FADD
DUP
FMUL
POP x
```
