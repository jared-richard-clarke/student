# Compiler Overview

Source: **Introduction to Compilers and Language Design** by Douglas Thain

## Source Text

Most high-level programs begin as encoded bytes, usually text.

```text
height = (width + 56) * factor(foo);
```

## Scanner

Reads source code character by character, identifies boundaries between
symbols, and emits a series of tokens. Each token is a data structure
that describes the nature and contents of each symbol.

```text
[id:height] [=] [(] [id:width] [+] [int:56] [)] [*] [id:factor] [(] [id:foo] [)] [;]
```

## Parser

Every language has a grammar. Each line of a grammar is called a rule.
Each time a rule is applied, the parser creates a node and weaves that
node and others into an abstract syntax tree (AST). The AST shows the
structural relationships between each symbol.

Semantic routines traverse the AST and derive additional meaning by
relating parts of the program to each other and the definition of
the programming language. An important component of this process
is type checking.

```text
      ASSIGN
   +-----|-----+
   ID         MUL
height   +-----|-----+
        ADD         CALL
      +--|--+     +--|--+
      ID   INT    ID    ID
    width  56   factor  foo
```

## Intermediate Representation (IR)

Post-order traversal of the AST generates intermediate instructions
for each node in the tree. Typical IR looks like abstract assembly language
with load/store instructions, arithmetic operations, and an infinite number
of registers.

Most forms of optimization occur here. Dead code is removed, common operations
are combined, and code is generally simplified to consume fewer resources and
run more quickly.

```text
LOAD $56    -> r1
LOAD width  -> r2
IADD r1, r2 -> r3
ARG  foo
CALL factor -> r4
IMUL r3, r4 -> r5
STOR r5     -> height
```

## Assembly

Intermediate code is then converted into the desired assembly.

```text
MOVQ  width, %rax     # load width into rax
ADDQ  $56, %rax       # add 56 to rax
MOVQ  %rax, -8(%rbp)  # save sum in temporary
MOVQ  foo, %edi       # load foo into arg 0 register
CALL  factor          # invoke factor, result in rax
MOVQ  -8(%rbp), %rbx  # load sum into rbx
IMULQ %rbx            # multiply rbx by rax
MOVQ  %rax, height    # store result into height
```
