# Pratt Parsing and Syntax

[Syntaxation](https://www.youtube.com/watch?v=Nlqv6NtBXcA)
by Douglas Crockford

## Top Down Operator Precedence

- Easy to build parsers with it.
- Really fast because it does almost nothing.
- Fast enough to use as an interpreter.
- Dynamic: Build DSLs with it.
- Extensible languages.
- No more reserved words.

## Example One

### Expression

```javascript
a = b + c
```

### Abstract Syntax Tree

```javascript
{
    id: "=",
    arity: "binary",
    first: {id: "a", arity: "word"},
    second: {
        id: "+",
        arity: "binary",
        first: {id: "b", arity: "word"},
        second: {id: "c", arity: "word"}
    }
}
```

### Stack Trace

```
statements()
    statement()
        expression(0)
            a.nud()
            while 0 < =.lbp
            =.led(a)
                expression(10)
                    b.nud()
                    while 10 < +.lbp
                    +.led(b)
                        expression(60)
                        c.nud()
```

## Example Two

### Expression

```javascript
a.b = c
```

### Abstract Syntax Tree

```javascript
{
    id: "=",
    arity: "binary",
    first: {
        id: ".",
        arity: "binary",
        first: {id: "a", arity: "word"},
        second: {id: "b", arity: "word"}
    },
    second: {id: "c", arity: "word"}
    
}
```

### Stack Trace

```
statements()
    statement()
        expression(0)
            a.nud()
            while 0 < ..lbp
            ..led(a)
                expression(90)
                    b.nud()
                    while 90 < = .lbp
            while 0 < =.lbp
            =.led(a.b)
                expression(60)
                c.nud()
```
