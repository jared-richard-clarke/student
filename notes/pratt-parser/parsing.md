# Precedence Parsing

[Top Down Operator Precedence](https://www.crockford.com/javascript/tdop/tdop.html)
by Douglas Crockford

[Top-Down Operator Precedence Parsing](https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing)
by Eli Bendersky

Given an operand between two operators, is the operand bound to the left operator or the right? 
Resolving this ambiguity is what makes parsing infix expressions complex.

> d Ⓐ e Ⓑ f
>
> (d Ⓐ e) Ⓑ f or d Ⓐ (e Ⓑ f)

## Expression

> "`expression` calls the `nud` method of the token...Then as long as the right binding power is less than 
> the left binding power of the next token, the `led` method is invoked on the following token (after that)." 
> 
> — Douglas Crockford

### JavaScript ( Crockford )

```javascript

let expression = function (rbp) {
    let left;
    let t = token;
    advance();
    left = t.nud();
    while (rbp < token.lbp) {
        t = token;
        advance();
        left = t.led(left);
    }
    return left;
}

```

### Python ( Bendersky )

```python

def expression(rbp=0):
    global token
    t = token
    token = next()
    left = t.nud()
    while rbp < token.lbp:
        t = token
        token = next()
        left = t.led(left)
    return left

```
