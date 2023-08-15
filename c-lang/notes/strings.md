# Strings in C

Unlike many higher-level programming languages, C does not feature an
explicit string type. While C does allow string literals, strings in
C are strictly represented as character arrays terminated with a null byte,
`\0` or `NUL`.

**Side Note**: `NUL` is the notation for ASCII character code `\0`.
`NULL` is a macro defined in `stddef` for null pointers.

C strings are a special case of character arrays. Not all character
arrays are considered strings, but any contiguous and null-terminated
buffer of characters is guaranteed to behave like a string.

## String Declarations

```c
// Set a char pointer to point to a string literal in read-only memory.
char* s = "Hello";

 // Initialize a char array on the stack using a string literal
char s[6] = "Hello";

// Initialize a char array on the stack using an array literal.
char s[6] = {'H', 'e', 'l', 'l', 'o', '\0'};

// Dynamically allocate memory for a string then write a string literal
// to that memory.
char* s = malloc(6);
strcpy(s, "Hello");

// Same as using malloc and strcpy
char* s = strdup("Hello");

// === string in memory ===
//
// | H | e | l | l | o | \0 |
//   0   1   2   3   4   5
```
