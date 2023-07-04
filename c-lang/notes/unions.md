# C: Unions

A `union` is like a `struct` except all fields overlap in memory.

> "If you’re familiar with a language in the ML family, structs and unions in C roughly mirror
>  the difference between product and sum types, between tuples and algebraic data types."
>
> Robert Nystrom, **Crafting Interpreters**

```c
struct {
  bool boolean;
  double number;
};

// struct
// | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
// ^---^ <--------------------------------- 1-byte bool
//     ^-------------------------------^ <- 8-byte double

union {
  bool boolean;
  double number;
};

// union
// | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
// ^---^ <----------------------------- 1-byte bool
// ^-------------------------------^ <- 8-byte double
```

> "Using a union to interpret bits as different types is the quintessence of C.
>  It opens up a number of clever optimizations and lets you slice and dice each
>  byte of memory in ways that memory-safe languages disallow. But it is also
>  wildly unsafe and will happily saw your fingers off if you don’t watch out."
>
> Robert Nystrom, **Crafting Interpreters**
