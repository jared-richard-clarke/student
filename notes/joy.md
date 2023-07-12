# Joy Programming

Original programs by Manfred von Thun, **Introduction to Joy**

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

- `rollup`: moves items `3 2 1` -> `1 3 2`
- `rolldown`: moves items `3 2 1` -> `2 1 3`
- `rotate`: moves items `3 2 1` -> `1 2 3`

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
