# Summary of Base Type Classes

```
 ----      ------      ------
| Eq |    | Show |    | Read |
 ----      ------      ------
   |
 -----         -----     ---------
| Ord |       | Num |   | Bounded |
 -----         -----     ---------
        \       |   \
 ------    ------   ------------
| Enum |  | Real | | Fractional |
 ------    ------   ------------
   |      /    |     /       |
 ----------   ----------   ----------
| Integral | | RealFrac | | Floating |
 ----------   ----------   ----------
                    \         /
     ---------      -----------
    | Functor |    | RealFloat |
     ---------      -----------
     |         \
 -------------  \        ----------
| Applicative |  \      | Foldable |
 -------------    \      ----------
     |             \        /
  -------        -------------
 | Monad |      | Traversable |
  -------        -------------
```