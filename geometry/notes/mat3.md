# 3 x 3 Affine Transformation Matrix

Usually implemented as a 6-part array. Parameters are described in column-major
order. Constants are implied.

Provides two-dimensional transformations and transformation composition.

## Matrix Notation

```
| a c e |
| b d f |
| 0 0 1 |
```

## Array Notation

```
[ A B C D <--- linear transformations
  E F ] <----- translations

       |-------|-------|---- implied constants
[ a b (0) c d (0) e f (1) ]
  0 1     2 3     4 5
```

## Parameters

- `a`: scale-x
- `b`: skew-y
- `c`: skew-x
- `d`: scale-y
- `e`: translate-x
- `f`: translate-y

## Transform

### Identity

```
| 1 0 0 |
| 0 1 0 |
| 0 0 1 |
```

### Translate

```
| 1 0 x |
| 0 1 y |
| 0 0 1 |
```

### Scale

```
| x 0 0 |
| 0 y 0 |
| 0 0 1 |
```

### Rotate

```
| cosθ -sinθ 0 |
| sinθ  cosθ 0 |
| 0     0    1 |
```
