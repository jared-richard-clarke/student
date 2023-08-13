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

## Strings and Pointers

```c
// statically declared string with implicit null
static const char *text = "Hello, World!";

// static array of 14 characters, including an explicit null
static const char text[] = {'H', 'e', 'l', 'l', 'o', ',', ' ', 'W', 'o', 'r', 'l', 'd', '!', '\0'};

// static array of strings
static const char *colors[] = {"red", "yellow", "green", "cyan", "blue", "magenta"};

//
static char *text = "Over the wintry"
                    "forest, winds howl in rage"
                    "with no leaves to blow."
```
