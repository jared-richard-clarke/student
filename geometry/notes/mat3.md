# 3 x 3 Affine Transformation Matrix

Usually implemented as a 6-part array. Parameters are described in column-major
order. Constants are implied.

Provides two-dimensional transformations and transformation composition.

## Column-Major Matrix Notation

$$
\begin{bmatrix}
a & c & e \\
b & d & f \\
0 & 0 & 1
\end{bmatrix}
$$

## Array Notation

```
[ a b c d <--- linear transformations
  e f ] <----- translations

       |-------|-------|---- implied constants
[ a b (0) c d (0) e f (1) ]
  0 1     2 3     4 5
```

## Parameters

- $a$: scale-x
- $b$: skew-y
- $c$: skew-x
- $d$: scale-y
- $e$: translate-x
- $f$: translate-y

## Transformation of a Point

$$x' = ax + cy + e$$

$$y' = bx + dy + f$$

## Transformation of a Vector

$$x' = ax + cy$$

$$y' = bx + dy$$

## Transformations

### Identity

$$
\begin{bmatrix}
1 & 0 & 0 \\
0 & 1 & 0 \\
0 & 0 & 1
\end{bmatrix}
$$

### Translate

$$
\begin{bmatrix}
1 & 0 & x \\
0 & 1 & y \\
0 & 0 & 1
\end{bmatrix}
$$

### Scale

$$
\begin{bmatrix}
x & 0 & 0 \\
0 & y & 0 \\
0 & 0 & 1
\end{bmatrix}
$$

### Rotate

$$
\begin{bmatrix}
\cos\theta & −\sin\theta & 0 \\
\sin\theta & \cos\theta & 0 \\
0 & 0 & 1
\end{bmatrix}
$$
