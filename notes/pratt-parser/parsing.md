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

## Functions and Bindings

| function | description                                                                                                 |
| -------- | ----------------------------------------------------------------------------------------------------------- |
| `nud`    | Null denotation. The prefix handler.                                                                        |
| `led`    | Left denotation. The infix/postfix handler.                                                                 |
| `lbp`    | Left binding power of infix operator. It tells how strongly the operator binds to the argument at its left. |
| `rbp`    | Right binding power.                                                                                        |

## Process

> "`expression` calls the `nud` method of the token...Then as long as the right binding power is less than 
> the left binding power of the next token, the `led` method is invoked on the following token." 
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

### C ( Nystrom )

```c
static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expect expression.");
        return;
    }

    prefixRule();
    while (precedence <= getRule(parser.current.type)->precedence) {
       advance();
       ParseFn infixRule = getRule(parser.previous.type)->infix;
       infixRule();
    }
}
```

> Here’s how the entire function works: At the beginning of `parsePrecedence()`, we look up a prefix parser for the current token.
> The first token is always going to belong to some kind of prefix expression, by definition. It may turn out to be nested as an
> operand inside one or more infix expressions, but as you read the code from left to right, the first token you hit always belongs
> to a prefix expression.
>
> After parsing that, which may consume more tokens, the prefix expression is done. Now we look for an infix parser for the next token.
> If we find one, it means the prefix expression we already compiled might be an operand for it. But only if the call to `parsePrecedence()`
> has a precedence that is low enough to permit that infix operator.
>
> If the next token is too low precedence, or isn’t an infix operator at all, we’re done. We’ve parsed as much expression as we can.
> Otherwise, we consume the operator and hand off control to the infix parser we found. It consumes whatever other tokens it needs
> (usually the right operand) and returns back to `parsePrecedence()`. Then we loop back around and see if the next token is also a
> valid infix operator that can take the entire preceding expression as its operand. We keep looping like that, crunching through
> infix operators and their operands until we hit a token that isn’t an infix operator or is too low precedence and stop.
>
> — Bob Nystrom, **Crafting Interpreters**

