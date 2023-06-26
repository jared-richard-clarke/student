# C: Unions

A `union` is like a `struct` except all fields overlap in memory.

```c
union data {
  bool boolean;
  double number;
};

// union
// | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
// ^-------------------------------^ <- 8-byte double
// ^---^ <----------------------------- 1-byte bool
```

> "Using a union to interpret bits as different types is the quintessence of C.
>  It opens up a number of clever optimizations and lets you slice and dice each
>  byte of memory in ways that memory-safe languages disallow. But it is also
>  wildly unsafe and will happily saw your fingers off if you don’t watch out."
>
> Robert Nystrom, **Crafting Interpreters**
