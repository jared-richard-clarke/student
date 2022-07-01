# Precedence Parsing

[Top Down Operator Precedence](https://www.crockford.com/javascript/tdop/tdop.html)
by Douglas Crockford

[JSLint](https://www.jslint.com/)
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

### JavaScript ( JSLint by Crockford)

```javascript
function parse_expression(rbp, initial) {
    let left;
    let the_symbol;

    if (!initial) {
        advance();
    }

    the_symbol = syntax_dict[token_now.id];

    if (the_symbol !== undefined && the_symbol.nud_prefix !== undefined) {
        test_cause("symbol");
        left = the_symbol.nud_prefix();
    } else if (token_now.identifier) {
        test_cause("identifier");
        left = token_now;
        left.arity = "variable";
    } else {
        return stop("unexpected_a", token_now);
    }

    while (true) {
        the_symbol = syntax_dict[token_nxt.id];
        if (
            the_symbol === undefined ||
            the_symbol.led_infix === undefined ||
            the_symbol.lbp <= rbp
        ) {
            break;
        }
        advance();
        left = the_symbol.led_infix(left);
    }
    return left;
}
```
