# Compiler Overview

**Introduction to Compilers and Language Design** by Douglas Thain

## Typical Compiler Toolchain

1.  Preprocessor (`cpp`): sources (`program.c`), headers (`<stdio.h>`) -> preprocessed source
2.  Compiler (`cc1`): preprocessed source -> assembly (`program.s`)
3.  Assembler (`as`): assembly (`program.s`) -> object code (`program.o`)
4.  Static Linker (`ld`): object code (`program.o`), libraries (`libc.a`) -> executable (program)
5.  Dynamic Linker (`ld.so`): executable (program), dynamic libraries (`libc.so`) ->
    running process

In Unix-like operating systems, the user-visible program `cc` invokes each element
of the compiler toolchain to produce the final executable.

## Stages of a Unix Compiler

1.  Scanner: character stream -> tokens
2.  Parser: tokens -> Abstract Syntax Tree
3.  Semantic Routines: AST -> Intermediate Representation
4.  Optimizers: IR -> IR
5.  Code generator: IR -> Assembly

## Source Text

Most high-level programs begin as encoded bytes, usually text.

```
    height = (width + 56) * factor(foo);
```

## Scanner

Reads source code character by character, identifies boundaries between
symbols, and emits a series of tokens. Each token is a data structure
that describes the nature and contents of each symbol.

```
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

```
            ASSIGN
      +-------|-------+
    ID:height        MUL
              +-------|-------+
             ADD             CALL
      +-------|---+       +---|-------+
    ID:width   INT:56   ID:factor   ID:foo
```

## Intermediate Representation (IR)

Post-order traversal of the AST generates intermediate instructions
for each node in the tree. Typical IR looks like abstract assembly language
with load/store instructions, arithmetic operations, and an infinite number
of registers.

Most forms of optimization occur here. Dead code is removed, common operations
are combined, and code is generally simplified to consume fewer resources and
run more quickly.

```
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

```
MOVQ  width, %rax     # load width into rax
ADDQ  $56, %rax       # add 56 to rax
MOVQ  %rax, -8(%rbp)  # save sum in temporary
MOVQ  foo, %edi       # load foo into arg 0 register
CALL  factor          # invoke factor, result in rax
MOVQ  -8(%rbp), %rbx  # load sum into rbx
IMULQ %rbx            # multiply rbx by rax
MOVQ  %rax, height    # store result into height
```
