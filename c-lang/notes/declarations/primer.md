# C Declarations: A Short Primer

by Greg Comeau

## C Declaration Syntax

The syntax of a C declaration is of the form:

   `storage-class type qualifier declarator = initializer;`

where `storage-class` is only one of the following:

- `typedef`
- `extern`
- `static`
- `auto`
- `register`

A `type` could be one or more of the following:

- `void`
- `char`
- `short`, `int`, `long`
- `float`, `double`
- `signed`, `unsigned`
- `struct` ...
- `union` ...
- `typedef type`

A `qualifier` could be one or more of the following:

- `const`
- `volatile`

A `declarator` contains an identifier and one or more, or none at all, of the following
in a variety of combinations:

- `*`
- `()`
- `[]`

possibly grouped within parentheses to create different bindings.

## Rules for Reading C Declarations

1. Parenthesize declarations as if they were expressions.
2. Locate the innermost parentheses.
3. Say "identifier is" where the identifier is the name of the variable.
   - Say "an array of X" if you see [X].
   - Say "a pointer to" if you see `*`.
   - Say "a function returning" if you see ().
4. Move to the next set of parentheses.
5. If more, go back to 3.
6. Else, say "type" for the remaining type left (such as char, int, etc.).
