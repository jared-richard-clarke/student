# The Factor Programming Language

## Stack Shuffling

```factor
drop ( x -- )
dup ( x -- x x )
over ( x y -- x y x )
swap ( x y -- y x )
```

### Data Flow Combinators

Factor provides a set of combinators that encapsulate common data-flow patterns.
Their purpose is to reduce stack shuffling or *stack noise*.

```factor
! cleave ( x sequence -- )
6 { [ 1 + ] [ 2 - ] } cleave ! -> 7 4

! spread ( objects... sequence -- )
"X" "y" { [ >lower ] [ >upper ] } spread ! -> "x" "Y"

! napply ( quote n -- )
"X" "Y" [ >lower ] 2 napply ! -> "x" "y"
```
