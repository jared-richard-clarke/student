# C Declaration Signatures

## Reading Declarations

1. Start at the variable name or innermost construct if no identifier is present.
2. Look right without jumping over a right parenthesis. Read type.
3. Look left without jumping over a parenthesis. Read type.
4. Jump out a level of parenthesis.
5. Repeat steps 1 through 4 until entire declaration is read.

## Example Declarations

### `argv`: pointer to `char` pointer

```c
char **argv
```

### `daytab`: pointer to `array[13]` of `int`

```c
int (*daytab)[13]
```

### `daytab`: `array[13]` of pointer to `int`

```c
int *daytab[13]
```

### `comp`: function returning pointer to `void`

```c
void *comp()
```

### `comp`: pointer to function returning `void`

```c
void (*comp)()
```

### `x`: function returning pointer to `array[]` of pointer to function returning `char`

```c
char (*(*x())[])()
```

### `x`: `array[3]` of pointer to function returning pointer to `array[5]` of `char`

```c
char (*(*x[3])())[5]
```
