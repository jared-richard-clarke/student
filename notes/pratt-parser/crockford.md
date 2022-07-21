# Pratt Parser

[Top Down Operator Precedence](https://www.crockford.com/javascript/tdop/tdop.html)
by Douglas Crockford

> "`expression` calls the `nud` method of the token...Then as long as the right binding power is less than 
> the left binding power of the next token, the `led` method is invoked on the following token." 
> 
> — Douglas Crockford

## Original Lexer

This is the lexer/tokenizer Crockford created prior to using regular expressions.

```javascript
// tokens.js
// http://crockford.com/javascript/tdop/tdop.html
// https://github.com/douglascrockford/TDOP
// Douglas Crockford
// 2021-05-04
// Public Domain

// Produce an array of simple token objects from a string.
// A simple token object contains these members:
//      type: "name", "string", "number", "operator"
//      value: string or number value of the token
//      from: index of first character of the token
//      to: index of the last character + 1

// Comments of the // type are ignored.

// Operators are by default single characters. Multicharacter
// operators can be made by supplying a string of prefix and
// suffix characters.
// characters. For example,
//      "<>+-&", "=>&:"
// will match any of these:
//      <=  >>  >>>  <>  >=  +: -: &: &&: &&

// This program augments 'String.prototype'
// because I wrote it when I was young and foolish.

String.prototype.tokens = function (prefix, suffix) {
    "use strict";
    var c;                      // The current character.
    var from;                   // The index of the start of the token.
    var i = 0;                  // The index of the current character.
    var length = this.length;
    var n;                      // The number value.
    var q;                      // The quote character.
    var str;                    // The string value.

    var result = [];            // An array to hold the results.

    var make = function (type, value) {

// Make a token object.

        return {
            type: type,
            value: value,
            from: from,
            to: i
        };
    };

// Begin tokenization. If the source string is empty, return nothing.

    if (!this) {
        return;
    }

// If prefix and suffix strings are not provided, supply defaults.

    if (typeof prefix !== "string") {
        prefix = "<>+-&";
    }
    if (typeof suffix !== "string") {
        suffix = "=>&:";
    }


// Loop through this text, one character at a time.

    c = this.charAt(i);
    while (c) {
        from = i;

// Ignore whitespace.

        if (c <= " ") {
            i += 1;
            c = this.charAt(i);

// name.

        } else if ((c >= "a" && c <= "z") || (c >= "A" && c <= "Z")) {
            str = c;
            i += 1;
            while (true) {
                c = this.charAt(i);
                if (
                    (c >= "a" && c <= "z")
                    || (c >= "A" && c <= "Z")
                    || (c >= "0" && c <= "9")
                    || c === "_"
                ) {
                    str += c;
                    i += 1;
                } else {
                    break;
                }
            }
            result.push(make("name", str));

// number.

// A number cannot start with a decimal point. It must start with a digit,
// possibly "0".

        } else if (c >= "0" && c <= "9") {
            str = c;
            i += 1;

// Look for more digits.

            while (true) {
                c = this.charAt(i);
                if (c < "0" || c > "9") {
                    break;
                }
                i += 1;
                str += c;
            }

// Look for a decimal fraction part.

            if (c === ".") {
                i += 1;
                str += c;
                while (true) {
                    c = this.charAt(i);
                    if (c < "0" || c > "9") {
                        break;
                    }
                    i += 1;
                    str += c;
                }
            }

// Look for an exponent part.

            if (c === "e" || c === "E") {
                i += 1;
                str += c;
                c = this.charAt(i);
                if (c === "-" || c === "+") {
                    i += 1;
                    str += c;
                    c = this.charAt(i);
                }
                if (c < "0" || c > "9") {
                    make("number", str).error("Bad exponent");
                }
                do {
                    i += 1;
                    str += c;
                    c = this.charAt(i);
                } while (c >= "0" && c <= "9");
            }

// Make sure the next character is not a letter.

            if (c >= "a" && c <= "z") {
                str += c;
                i += 1;
                make("number", str).error("Bad number");
            }

// Convert the string value to a number. If it is finite, then it is a good
// token.

            n = +str;
            if (isFinite(n)) {
                result.push(make("number", n));
            } else {
                make("number", str).error("Bad number");
            }

// string

        } else if (c === "\"" || c === "'") {
            str = "";
            q = c;
            i += 1;
            while (true) {
                c = this.charAt(i);
                if (c < " ") {
                    make("string", str).error(
                        (
                            (c === "\n" || c === "\r" || c === "")
                            ? "Unterminated string."
                            : "Control character in string."
                        ),
                        make("", str)
                    );
                }

// Look for the closing quote.

                if (c === q) {
                    break;
                }

// Look for escapement.

                if (c === "\\") {
                    i += 1;
                    if (i >= length) {
                        make("string", str).error("Unterminated string");
                    }
                    c = this.charAt(i);
                    switch (c) {
                    case "b":
                        c = "\b";
                        break;
                    case "f":
                        c = "\f";
                        break;
                    case "n":
                        c = "\n";
                        break;
                    case "r":
                        c = "\r";
                        break;
                    case "t":
                        c = "\t";
                        break;
                    case "u":
                        if (i >= length) {
                            make("string", str).error("Unterminated string");
                        }
                        c = parseInt(this.substr(i + 1, 4), 16);
                        if (!isFinite(c) || c < 0) {
                            make("string", str).error("Unterminated string");
                        }
                        c = String.fromCharCode(c);
                        i += 4;
                        break;
                    }
                }
                str += c;
                i += 1;
            }
            i += 1;
            result.push(make("string", str));
            c = this.charAt(i);

// comment.

        } else if (c === "/" && this.charAt(i + 1) === "/") {
            i += 1;
            while (true) {
                c = this.charAt(i);
                if (c === "\n" || c === "\r" || c === "") {
                    break;
                }
                i += 1;
            }

// combining

        } else if (prefix.indexOf(c) >= 0) {
            str = c;
            i += 1;
            while (true) {
                c = this.charAt(i);
                if (i >= length || suffix.indexOf(c) < 0) {
                    break;
                }
                str += c;
                i += 1;
            }
            result.push(make("operator", str));

// single-character operator

        } else {
            i += 1;
            result.push(make("operator", c));
            c = this.charAt(i);
        }
    }
    return result;
};
```

## Simple JavaScript Lexer and Parser

```javascript

// Transform a token object into an exception object and throw it.

Object.prototype.error = function (message, t) {
    t = t || this;
    t.name = "SyntaxError";
    t.message = message;
    throw t;
};

// tokens.js
// 2011-01-04

// (c) 2006 Douglas Crockford

// Produce an array of simple token objects from a string.
// A simple token object contains these members:
//      type: 'name', 'string', 'number', 'punctuator'
//      value: string or number value of the token
//      from: index of first character of the token
//      to: index of the last character + 1

// Comments of the // type are ignored.

const rx_crlf = /\n|\r\n?/;

// const rx_crlf = /
//      \n
// |
//      \r \n?
// /;

function tokenize(source) {
    // tokenize takes a source and produces from it an array of token objects.
    // If the source is not an array, then it is split into lines at the
    // carriage return/linefeed.

    const lines = Array.isArray(source) ? source : source.split(rx_crlf);
    const result = [];

    lines.forEach(function (line, line_nr) {
        const rx_token =
            /(\u0020+)|(\/\/.*)|([a-zA-Z][a-zA-Z_0-9]*)|(\d+(?:\.\d+)?(?:[eE][+\-]?\d+)?)|("(?:[^"\\]|\\(?:[nr"\\]|u[0-9a-fA-F]{4}))*")|([(){}\[\]?.,:;~*\/]|&&?|\|\|?|[+\-<>]=?|[!=](?:==)?)/y;

        // const rx_token = /
        //     ( \u0020+ )
        // |
        //     ( \/\/ .* )
        // |
        //     (
        //         [ a-z A-Z ]
        //         [ a-z A-Z _ 0-9 ]*
        //     )
        // |
        //     (
        //         \d+
        //         (?: \. \d+ )?
        //         (?: [ e E ] [ + \- ]? \d+ )?
        //     )
        // |
        //     (
        //         "
        //         (?:
        //             [^ " \\ ]
        //         |
        //             \\
        //             (?:
        //                 [ n r " \\ ]
        //             |
        //                 u [ 0-9 a-f A-F ]{4}
        //             )
        //         )*
        //         "
        //     )
        // |
        //     (
        //         [ ( ) { } \[ \] ? . , : ; ~ * \/ ]
        //     |
        //         & &?
        //     |
        //         \| \|?
        //     |
        //         [ + \-  < > ] =?
        //     |
        //         [ ! = ] (?: == )?
        //     )
        // /y;

        // Capture Group
        // [1]  Whitespace
        // [2]  Comment
        // [3]  Name
        // [4]  Number
        // [5]  String
        // [6]  Punctuator

        let column_nr = 0;
        let make = function (type, value) {
            // Make a token object and append it to the result.

            result.push({
                type,
                value,
                line_nr,
                column_nr,
            });
        };

        while (column_nr < line.length) {
            let captives = rx_token.exec(line);
            if (!captives) {
                throw new SyntaxError(
                    "line " + line_nr + " column " + column_nr
                );
            } else if (captives[1]) {
            } else if (captives[2]) {
            } else if (captives[3]) {
                make("name", captives[3]);
            } else if (captives[4]) {
                let number = Number(captives[4]);
                if (Number.isFinite(number)) {
                    make("number", number);
                } else {
                    throw new TypeError(
                        "line " + line_nr + " column " + column_nr
                    );
                }
            } else if (captives[5]) {
                make("string", JSON.parse(captives[5]));
            } else if (captives[6]) {
                make("punctuator", captives[6]);
            }
            column_nr = rx_token.lastIndex;
        }
    });
    return result;
}

// parse.js
// Parser for Simplified JavaScript written in Simplified JavaScript
// From Top Down Operator Precedence
// http://javascript.crockford.com/tdop/index.html
// Douglas Crockford
// 2010-06-26

let make_parse = function () {
    let scope;
    let symbol_table = {};
    let token;
    let tokens;
    let token_nr;

    let itself = function () {
        return this;
    };

    let original_scope = {
        define: function (n) {
            let t = this.def[n.value];
            if (typeof t === "object") {
                n.error(t.reserved ? "Already reserved." : "Already defined.");
            }
            this.def[n.value] = n;
            n.reserved = false;
            n.nud = itself;
            n.led = null;
            n.std = null;
            n.lbp = 0;
            n.scope = scope;
            return n;
        },
        find: function (n) {
            let e = this,
                o;
            while (true) {
                o = e.def[n];
                if (o && typeof o !== "function") {
                    return e.def[n];
                }
                e = e.parent;
                if (!e) {
                    o = symbol_table[n];
                    return o && typeof o !== "function"
                        ? o
                        : symbol_table["(name)"];
                }
            }
        },
        pop: function () {
            scope = this.parent;
        },
        reserve: function (n) {
            if (n.arity !== "name" || n.reserved) {
                return;
            }
            let t = this.def[n.value];
            if (t) {
                if (t.reserved) {
                    return;
                }
                if (t.arity === "name") {
                    n.error("Already defined.");
                }
            }
            this.def[n.value] = n;
            n.reserved = true;
        },
    };

    let new_scope = function () {
        let s = scope;
        scope = Object.create(original_scope);
        scope.def = {};
        scope.parent = s;
        return scope;
    };

    let advance = function (id) {
        let a, o, t, v;
        if (id && token.id !== id) {
            token.error("Expected '" + id + "'.");
        }
        if (token_nr >= tokens.length) {
            token = symbol_table["(end)"];
            return;
        }
        t = tokens[token_nr];
        token_nr += 1;
        v = t.value;
        a = t.type;
        if (a === "name") {
            o = scope.find(v);
        } else if (a === "punctuator") {
            o = symbol_table[v];
            if (!o) {
                t.error("Unknown operator.");
            }
        } else if (a === "string" || a === "number") {
            o = symbol_table["(literal)"];
            a = "literal";
        } else {
            t.error("Unexpected token.");
        }
        token = Object.create(o);
        token.line_nr = t.line_nr;
        token.column_nr = t.column_nr;
        token.value = v;
        token.arity = a;
        return token;
    };

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
    };

    let statement = function () {
        let n = token,
            v;

        if (n.std) {
            advance();
            scope.reserve(n);
            return n.std();
        }
        v = expression(0);
        if (!v.assignment && v.id !== "(") {
            v.error("Bad expression statement.");
        }
        advance(";");
        return v;
    };

    let statements = function () {
        let a = [],
            s;
        while (true) {
            if (token.id === "}" || token.id === "(end)") {
                break;
            }
            s = statement();
            if (s) {
                a.push(s);
            }
        }
        return a.length === 0 ? null : a.length === 1 ? a[0] : a;
    };

    let block = function () {
        let t = token;
        advance("{");
        return t.std();
    };

    let original_symbol = {
        nud: function () {
            this.error("Undefined.");
        },
        led: function (left) {
            this.error("Missing operator.");
        },
    };

    let symbol = function (id, bp) {
        let s = symbol_table[id];
        bp = bp || 0;
        if (s) {
            if (bp >= s.lbp) {
                s.lbp = bp;
            }
        } else {
            s = Object.create(original_symbol);
            s.id = s.value = id;
            s.lbp = bp;
            symbol_table[id] = s;
        }
        return s;
    };

    let constant = function (s, v) {
        let x = symbol(s);
        x.nud = function () {
            scope.reserve(this);
            this.value = symbol_table[this.id].value;
            this.arity = "literal";
            return this;
        };
        x.value = v;
        return x;
    };

    let infix = function (id, bp, led) {
        let s = symbol(id, bp);
        s.led =
            led ||
            function (left) {
                this.first = left;
                this.second = expression(bp);
                this.arity = "binary";
                return this;
            };
        return s;
    };

    let infixr = function (id, bp, led) {
        let s = symbol(id, bp);
        s.led =
            led ||
            function (left) {
                this.first = left;
                this.second = expression(bp - 1);
                this.arity = "binary";
                return this;
            };
        return s;
    };

    let assignment = function (id) {
        return infixr(id, 10, function (left) {
            if (left.id !== "." && left.id !== "[" && left.arity !== "name") {
                left.error("Bad lvalue.");
            }
            this.first = left;
            this.second = expression(9);
            this.assignment = true;
            this.arity = "binary";
            return this;
        });
    };

    let prefix = function (id, nud) {
        let s = symbol(id);
        s.nud =
            nud ||
            function () {
                scope.reserve(this);
                this.first = expression(70);
                this.arity = "unary";
                return this;
            };
        return s;
    };

    let stmt = function (s, f) {
        let x = symbol(s);
        x.std = f;
        return x;
    };

    symbol("(end)");
    symbol("(name)");
    symbol(":");
    symbol(";");
    symbol(")");
    symbol("]");
    symbol("}");
    symbol(",");
    symbol("else");

    constant("true", true);
    constant("false", false);
    constant("null", null);
    constant("pi", 3.141592653589793);
    constant("Object", {});
    constant("Array", []);

    symbol("(literal)").nud = itself;

    symbol("this").nud = function () {
        scope.reserve(this);
        this.arity = "this";
        return this;
    };

    assignment("=");
    assignment("+=");
    assignment("-=");

    infix("?", 20, function (left) {
        this.first = left;
        this.second = expression(0);
        advance(":");
        this.third = expression(0);
        this.arity = "ternary";
        return this;
    });

    infixr("&&", 30);
    infixr("||", 30);

    infixr("===", 40);
    infixr("!==", 40);
    infixr("<", 40);
    infixr("<=", 40);
    infixr(">", 40);
    infixr(">=", 40);

    infix("+", 50);
    infix("-", 50);

    infix("*", 60);
    infix("/", 60);

    infix(".", 80, function (left) {
        this.first = left;
        if (token.arity !== "name") {
            token.error("Expected a property name.");
        }
        token.arity = "literal";
        this.second = token;
        this.arity = "binary";
        advance();
        return this;
    });

    infix("[", 80, function (left) {
        this.first = left;
        this.second = expression(0);
        this.arity = "binary";
        advance("]");
        return this;
    });

    infix("(", 80, function (left) {
        let a = [];
        if (left.id === "." || left.id === "[") {
            this.arity = "ternary";
            this.first = left.first;
            this.second = left.second;
            this.third = a;
        } else {
            this.arity = "binary";
            this.first = left;
            this.second = a;
            if (
                (left.arity !== "unary" || left.id !== "function") &&
                left.arity !== "name" &&
                left.id !== "(" &&
                left.id !== "&&" &&
                left.id !== "||" &&
                left.id !== "?"
            ) {
                left.error("Expected a variable name.");
            }
        }
        if (token.id !== ")") {
            while (true) {
                a.push(expression(0));
                if (token.id !== ",") {
                    break;
                }
                advance(",");
            }
        }
        advance(")");
        return this;
    });

    prefix("!");
    prefix("-");
    prefix("typeof");

    prefix("(", function () {
        let e = expression(0);
        advance(")");
        return e;
    });

    prefix("function", function () {
        let a = [];
        new_scope();
        if (token.arity === "name") {
            scope.define(token);
            this.name = token.value;
            advance();
        }
        advance("(");
        if (token.id !== ")") {
            while (true) {
                if (token.arity !== "name") {
                    token.error("Expected a parameter name.");
                }
                scope.define(token);
                a.push(token);
                advance();
                if (token.id !== ",") {
                    break;
                }
                advance(",");
            }
        }
        this.first = a;
        advance(")");
        advance("{");
        this.second = statements();
        advance("}");
        this.arity = "function";
        scope.pop();
        return this;
    });

    prefix("[", function () {
        let a = [];
        if (token.id !== "]") {
            while (true) {
                a.push(expression(0));
                if (token.id !== ",") {
                    break;
                }
                advance(",");
            }
        }
        advance("]");
        this.first = a;
        this.arity = "unary";
        return this;
    });

    prefix("{", function () {
        let a = [],
            n,
            v;
        if (token.id !== "}") {
            while (true) {
                n = token;
                if (n.arity !== "name" && n.arity !== "literal") {
                    token.error("Bad property name.");
                }
                advance();
                advance(":");
                v = expression(0);
                v.key = n.value;
                a.push(v);
                if (token.id !== ",") {
                    break;
                }
                advance(",");
            }
        }
        advance("}");
        this.first = a;
        this.arity = "unary";
        return this;
    });

    stmt("{", function () {
        new_scope();
        let a = statements();
        advance("}");
        scope.pop();
        return a;
    });

    stmt("let", function () {
        let a = [],
            n,
            t;
        while (true) {
            n = token;
            if (n.arity !== "name") {
                n.error("Expected a new variable name.");
            }
            scope.define(n);
            advance();
            if (token.id === "=") {
                t = token;
                advance("=");
                t.first = n;
                t.second = expression(0);
                t.arity = "binary";
                a.push(t);
            }
            if (token.id !== ",") {
                break;
            }
            advance(",");
        }
        advance(";");
        return a.length === 0 ? null : a.length === 1 ? a[0] : a;
    });

    stmt("if", function () {
        advance("(");
        this.first = expression(0);
        advance(")");
        this.second = block();
        if (token.id === "else") {
            scope.reserve(token);
            advance("else");
            this.third = token.id === "if" ? statement() : block();
        } else {
            this.third = null;
        }
        this.arity = "statement";
        return this;
    });

    stmt("return", function () {
        if (token.id !== ";") {
            this.first = expression(0);
        }
        advance(";");
        if (token.id !== "}") {
            token.error("Unreachable statement.");
        }
        this.arity = "statement";
        return this;
    });

    stmt("break", function () {
        advance(";");
        if (token.id !== "}") {
            token.error("Unreachable statement.");
        }
        this.arity = "statement";
        return this;
    });

    stmt("while", function () {
        advance("(");
        this.first = expression(0);
        advance(")");
        this.second = block();
        this.arity = "statement";
        return this;
    });

    return function (array_of_tokens) {
        tokens = array_of_tokens;
        token_nr = 0;
        new_scope();
        advance();
        let s = statements();
        advance("(end)");
        scope.pop();
        return s;
    };
};

```
