# Joy Programming

Original programs by Manfred von Thun, **Introduction to Joy**

`program = data = memory`

1. Joy is a purely functional language.
2. Joy programs denote unary functions from stacks to stacks.
3. Joy uses concatenative notation, so the concatenation of programs denotes
   the composition of the functions.
4. Definitions do not use formal parameters, so there is no environment of
   associations.
5. Combinators do the work of higher-order functions. They take quoted programs
   as parameters.
6. Lists are a special case of quotations, and both can be manipulated in the
   same way. As in LISP, **program = data**.

## Unique Properties

- Unlike **lambda calculus** in that it has no variables.
- Unlike **combinatory calculus** in that it does not use application.
- Unlike **categorical languages** in its uniform use of an untyped stack as argument and value of composed functions.

## A Few Combinators

```text
[B] [A] swap == [A] [B]
    [A] dup  == [A] [A]
    [A] pop  ==

[B] [A] cat  == [B A]
[B] [A] cons == [[B] A]
    [A] unit == [[A]]

    [A] i    == A
[B] [A] dip  == A [B]
```

## Questions about Joy

### What is a functional language?

The programs of functional languages are expressions. Imperative languages have
statements to assign values to variables. The values connected to those
variables can be changed.

Expressions always denote or have values.

### What is an applicative language?

Most functional languages, at least internally, use a binary application
operator. The expression `f @ x` is read as the function `f` applied to
the argument `x`. Most programming language syntax denotes this as `f(x)`.
Apply, `@`, is a higher-order function.

### Do functional languages need application?

The higher-order application operator makes it possible to define other
higher-order operations. One example is the composition operation, `∘`.

```text
=== applicative order notation ===
(f ∘ g) @ x == f @ (g @ x)
```

### What might a compositional functional language be?

If literal expressions could denote unary functions, the expression
`(square ∘ size ∘ rest) @ [ 11 22 33 ]` could be written as
`square ∘ size ∘ rest ∘ [ 11 22 33 ]` where the entire expression
is applied to an implied argument. This argument can be a stack.
The expression `[ 11 22 33 ]` is not a list but a function that pushes
a list onto a stack.

Function composition notation can be reversed to reflect the order of execution:
`square ∘ size ∘ rest ∘ [ 11 22 33 ]` becomes `[ 11 22 33 ] |> rest |> size |> square`.

A functional language that completely replaces application with implicit `∘`
or `|>` might be called a compositional language.

### What is an environment?

Using lambda abstraction, the squaring function can be defined as such:
`square(x) == x * x` or `square == Lx : x * x`.

In the example below, `square @ 2` sets up an environment where `x` is given
the value `2`.

```text
=== with an environment ===
square @ 2
(Lx : x * x) @ 2
2 * 2
4

=== without an environment ===
[ 11 22 33 ] rest size square
        [ 22 33 ] size square
                     2 square
                     2 dup *
                     2 2 *
                     4
```

### How does one think about Joy programs?

1. Imperative: programs are commands to modify the stack.
    - "5" means push the number 5.
    - "+" means add the top two numbers
2. Semantic: programs denote unary functions from stacks to stacks.
    - "5" denotes the function that yields a new stack with the number 5 on top.
    - "+" denotes the function that yields a new stack with the top two numbers
      replaced by their sum.
3. Syntactic: programs are text to be evaluated by rewriting.
    - "5" can be only part of an expression to be rewritten.
    - "+" is used in rewriting "5 3 +" to "8".


## Definitions

```text
ATOM == PROGRAM
```

`==` means defines the atom on the left to cause the execution of the program on the right.

## Basic Stack Manipulation

- `pop`: removes the top item.
- `dup`: creates a copy of the top item.
- `swap`: replaces the top item with the second and the second item with the first.
- `dip`: takes a quoted program and below that a further item. The item is saved,
         the quoted program is executed, and the saved item is then restored.

## Stack Manipulations Using `dip`

```joy
popd  == [ pop ] dip
dupd  == [ dup ] dip
swapd == [ swap ] dip
```

## Ordering the Stack

- `rollup`: moves items `3 2 1` to `1 3 2`
- `rolldown`: moves items `3 2 1` to `2 1 3`
- `rotate`: moves items `3 2 1` to `1 2 3`

```joy
rollup   == swap [ swap ] dip
rolldown == [ swap ] dip swap
rotate   == swap [ swap ] dip swap
```

## Unit Operators

Build aggregates by consing an element onto its unit type. `unitset` requires a small number,
`unitstring` a character, and `unitlist` anything. The action of all three is reversed by `first`.

```joy
unitset    == {} cons
unitstring == "" cons
unitlist   == [] cons
```

Aggregating two items can accomplished by applying `cons` twice to the appropriate unit type.

```joy
pairset    == {} cons cons
pairstring == "" cons cons
pairlist   == [] cons cons

unpair == uncons uncons pop
(* or *)
unpair == uncons first
```

## Non-Empty Set Operations

- `first`: returns the first item of a set.
- `rest`: removes the first item of a set and returns the remaining items.

```joy
second == rest first
third  == rest rest first
```

## Accumulation

- `step`: applies a program to each element of a sequence.
- `shunt`: takes two sequences as parameters and, starting at the front of the topmost sequence,
           moves all items onto the front of the second sequence.

```joy
swons == swap cons
shunt == [ swons ] step

reverselist   == [] swap shunt
reversestring == "" swap shunt

reverse == [ [] ] [ "" ] iflist swap shunt
```

- `infra`: uses list as a temporary stack, which is then pushed onto the actual stack as a list.

```joy
reverselist == [] swap infra
```

## Output

- `put`: prints a single value of any type.
- `putch`: prints a single character unquoted.

```joy
putchars == [ putch ] step
newline  == '\n put
```

## Stack

**Side Note**: a library declaration begins with the word `LIBRA`.

```joy
LIBRA (* stack *)

HIDE
    error == "non-empty stack needed for " putchars putchars newline abort;
IN
    st-new  == [];
    st-push == swons;
    st-null == dup null;
    st-top  == [ null ] [ "st-top"  error ] [ dup first ] ifte;
    st-pop  == [ null ] [ "st-pop"  error ] [ rest      ] ifte;
    st-pull == [ null ] [ "st-pull" error ] [ unswons   ] ifte.st-new  == []
END.
```

## Queue

```joy
LIBRA (* queue *)

HIDE
    error   == "non_empty queue needed for " putchars putchars newline abort;
    prepare == [ null ] [ swap reverse ] [] ifte
IN
    q-new   == [] [];
    q-add   == swap [ swons ] dip;
    q-addl  == swap [ shunt ] dip;
    q-null  == prepare dup null;
    q-front == prepare [ null ] [ "q-front" error ] [ dup first ] ifte;
    q-rem   == prepare [ null ] [ "q-rem "  error ] [ unswons   ] ifte
END.
```

# The Y Combinator

The second definition expects a program on top of the stack from which it will construct another
program that will duplicate itself if ever called by a combinator such as `i`.

```joy
# === recursive ===

Y == dup [[Y] cons] dip i

# === non-recursive ===

Y == [dup cons] swap concat dup cons i
```
