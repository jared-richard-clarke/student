# C: Pointers

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
