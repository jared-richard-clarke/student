# C Declaration Signatures

## Reading Declarations

1. Start at the variable name or innermost construct if no identifier is present.
2. Look right without jumping over a right parenthesis. Read type.
3. Look left without jumping over a parenthesis. Read type.
4. Jump out a level of parentheses.
5. Repeat steps 1 through 4 until entire declaration is read.

## Example Declarations

### `argv`: pointer to a `char` pointer

Declaring an array variable without a size is equivalent to declaring a pointer. This emphasizes
that the pointer variable will be used in a manner equivalent to an array. 

```c
char **argv
// - equivalent ->
char *argv[]
```

### `daytab`: pointer to 13-part array of `int`

```c
int (*daytab)[13]
```

### `daytab`: 13-part array of pointers to `int`

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

### `x`: function returning pointer to array of pointers to functions returning `char`

```c
char (*(*x())[])()
```

### `x`: 3-part array of pointers to function returning pointer to 5-part array of `char`

```c
char (*(*x[3])())[5]
```
