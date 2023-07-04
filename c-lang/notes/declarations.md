# Declaration Signatures

Examples of declaration signatures in C.

## `argv`: pointer to `char`

```c
char **argv
```

## `daytab`: pointer to `array[13]` of `int`

```c
int (*daytab)[13]
```

## `daytab`: `array[13]` of pointer to `int`

```c
int *daytab[13]
```

## `comp`: function returning pointer to `void`

```c
void *comp()
```

## `comp`: pointer to function returning `void`

```c
void (*comp)()
```

## `x`: function returning pointer to `array[]` of pointer to function returning `char`

```c
char (*(*x())[])()
```

## `x`: `array[3]` of pointer to function returning pointer to `array[5]` of `char`

```c
char (*(*x[3])())[5]
```
