# C Pointers

## Example #1

```c
void main()
{
    int *x, *y;

    x = (int *)malloc(sizeof(int));
    y = x;
  
    *x = 7;
    *y = 11;
}

// 1. x  y | 2. x  y | 3. x  y | 4. x  y | 5. x  y
//         |    |    |    | /  |    | /  |    | /
//         |    ?    |    ?    |    7    |   11
```

## Example #2

```c
void main()
{
    int *x, *y;

    x = (int *)malloc(sizeof(int));
    y = (int *)malloc(sizeof(int));
  
    *x = 7;
    *y = 11;
  
    *x = *y;
}

// 1. x  y | 2. x  y | 3. x  y | 4. x  y | 5. x  y  | 6. x  y
//         |    |    |    |  | |    |  | |    |  |  |    |  |
//         |    ?    |    ?  ? |    7  ? |    7  11 |   11  11
```

## Example #3

C is a call-by-value language. Pointers allow functions to change variables within the code that called them.

```c
void swap(int *px, int *py)
{
    int temp;

    temp = *px;
    *px = *py;
    *py = temp;
}

void main()
{
    swap(&a, &b);
}

// caller:
// b <-----+
// a <---+ |
//       | |
// swap: | |
// px *----+
// py *--+
```

## Arrays and Pointers

The value of a pointer is not adjusted by the integer amount, but is adjusted by the amount multiplied by the size of
the type to which the pointer refers in bytes. `pointer + x` is equivalent to `pointer + (x * sizeof(*type))`

```c
// array indexing
array[i]
// pointer arithmetic
*(array+i)
```
