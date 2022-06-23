# Abstract Syntax Tree

## JavaScript

```javascript

const ast_1 = {
    operator: "+",
    first: 1,
    second: {
        operator: "*",
        first: 2,
        second: 3,
    },
};

const ast_2 = {
    operator: "+",
    first: {
        operator: "+",
        first: 1,
        second: 2,
    },
    second: 3,
};

```

## Scheme

```scheme

(define ast-1
  '(+ 1
      (* 2
         3)))

(define ast-2
  '(+ (+ 1
         2)
      3))

```
