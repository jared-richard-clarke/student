# Pratt Parser: Abstract Syntax Tree

## JavaScript

```javascript

const ast_1 = {
    operator: "+",
    one: 1,
    two: {
        operator: "*",
        one: 2,
        two: 3,
    },
};

const ast_2 = {
    operator: "+",
    one: {
        operator: "+",
        one: 1,
        two: 2,
    },
    two: 3,
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
