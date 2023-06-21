# The Clockwise/Spiral Rule

The author David Anderson postulates that any C program declaration can be parsed
using a technique called the **clockwise/spiral rule**. Examples 1-3 are pulled
directly from his article.

## Example 1: Simple Declaration

`str` is an array of ten pointers to `char`.

```c
//   +-------+
//   | +-+   |
//   | ^ |   |
char *str[10];
//^  ^   |   |
//|  +---+   |
//+----------+
```

## Example 2: Pointer to Function Declaration

`fp` is a pointer to a function passing an `int` and a pointer to float returning
a pointer to `char`.

```c
//   +-------------------+
//   | +---+             |
//   | |+-+|             |
//   | |^ ||             |
char *(*fp)(int, float *);
//^  ^ ^  ||             |
//|  | +--+|             |
//|  +-----+             |
//+----------------------+
```

## Example 3: The Ultimate

`signal` is a function passing an `(int` and a pointer to a function passing an
`int` returning nothing (`void`) returning a pointer to a function passing an
`int` returning nothing (`void`).

```c
//    +-----------------------------+
//    |                  +---+      |
//    |  +---+           |+-+|      |
//    |  ^   |           |^ ||      |
void (*signal(int, void (*fp)(int)))(int);
//^   ^      |      ^    ^  ||      |
//|   +------+      |    +--+|      |
//|                 +--------+      |
//+---------------------------------+
```
