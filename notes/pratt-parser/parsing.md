# Precedence Parsing

6 June 2022

[Top Down Operator Precedence](https://www.crockford.com/javascript/tdop/tdop.html)
by Douglas Crockford

[Top-Down Operator Precedence Parsing](https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing)
by Eli Bendersky

---

The basic precedence problem is this: Given an operand between two operators, is
the operand bound to the left operator or the right? The complexity in the
process of parsing comes down to the resolution of this ambiguity.

> d Ⓐ e Ⓑ f
>
> (d Ⓐ e) Ⓑ f or d Ⓐ (e Ⓑ f)

## NUD and LED

`nud`, null denotation, returns the value of its token.

`led`, left denotation, chooses functionality according to the token left of the token it is operating on.

## Expression

The core of Pratt's technique is the `expression` function. It takes a right
binding power that controls how aggressively it binds to tokens on its right.
The process is recursive because `nud` and `led` can call `expression`.

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
