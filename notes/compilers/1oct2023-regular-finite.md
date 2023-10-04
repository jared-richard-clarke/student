# Regular Expressions and Finite Automata

## Regular Expressions

*s* = string
*L(s)* = language of *s*, a set of strings drawn from alphabet *Σ*.

### Inductive Definition

- If *a ∈ Σ* then *a* is a regular expression and *L(a) = {a}*.

- *ε* is a regular expression and *L(ε)* contains only the empty string.

Then for regular expressions *s* and *t*:

1. *s|t* is a RE such that *L(s|t) = L(s) ∪ L(t)*.

2. *st* is RE such that *L(st)* contains all strings formed by the concatenation
   of a string in *L(s)* followed by a string in *L(t)*.

3. *s\** is a RE such that *L(s\*) = L(s)* concatenated zero or more times.

Rule #3 is known as the **Kleene closure** and has the highest precedence. Rule #2
is known as **concatenation**. Rule #1 has the lowest precedence and is known as
**alternation**. Parentheses can be added to adjust the order of operations in
the usual way.

### Examples

| Regular Expression  | Language L(s)               |
| ------------------- | --------------------------- |
| hello               | { hello }                   |
| d(o\|i)g            | { dog, dig }                |
| moo*                | { mo, moo, mooo, ... }      |
| (moo)*              | { ε, moo, moomoo, ... }     |
| a(b\|a)*a           | { aa, aaa, aba, aaaa, ... } |

### Optional Syntax

| syntax | definition  | alternate      |
| ------ | ----------- | -------------- |
| s?     | optional    | (s\|ε)         |
| s+     | one or more | ss*            |
| [a-z]  | one of      | (a\|b\|...\|z) |
| [^x]   | any except  | Σ - x          |

### Algebraic Properties

| property      | value                 |
| ------------- | --------------------- |
| Associativity | a\|(b\|c) = (a\|b)\|c |
| Commutativity | a\|b = b\|a           |
| Distribution  | a(b\|c) = ab\|ac      |
| Idempotency   | a** = a*              |

## Finite Automata

A **finite automaton** is an abstract machine consisting of a number of states
and a number of edges between those states. Each edge is labeled with one or
more symbols drawn from alphabet Σ. For each input symbol, FA moves to the state
connected by the matching edge.


**Deterministic finite automaton** are a special case of FA where every state
has no more than one outgoing edge for a given symbol. DFAs have no ambiguities.

The transitions between states are represented by a matrix *M[s, i]*, which
encodes the next state, given the current state and input symbol. If the transition
is not allowed, we mark it with *E* to indicate an error. For each symbol, we
compute *c = M[s, i]* until all the input is consumed or an error state is reached.

Every RE can be written as an FA and vice versa.

### FA: *for*

```
-->(0)-f->(1)-o->(2)-r->((3))
```

### RE to FA: *identifier*

```
RE: [a-z][a-z0-9]+
                            -a-z0-9->
                            <--------
FA: -->(0)-a-z->(1)-a-z0-9->((2))
```

### FA: *numbers*

```
                 -0-9->
                 <-----
      ((1))-0-9->((2))
 1-9 /
--(0)
   0 \
      ((3))
```
