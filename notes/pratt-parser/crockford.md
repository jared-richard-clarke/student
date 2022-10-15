# Pratt Parser

[Top Down Operator Precedence](https://www.crockford.com/javascript/tdop/tdop.html)
by Douglas Crockford

> "`expression` calls the `nud` method of the token...Then as long as the right binding power is less than 
> the left binding power of the next token, the `led` method is invoked on the following token." 
> 
> — Douglas Crockford

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

    // "In a more sophisticated language, we would want to have scope,
    // giving the programmer convenient control over the lifespan and
    // visibility of a variable."

    let scope;

    // "Every token, such as an operator or identifier, will inherit
    // from a symbol. We will keep all of our symbols (which determine
    // the types of tokens in our language) in a symbol_table object."

    let symbol_table = {};

    // "We assume that the source text has been transformed into an array
    // of simple token objects (tokens), each containing a type member
    // ("name", "string", "number", or "operator"), and a value member,
    // which is a string or number.
    // 
    // The token variable always contains the current token".

    let token;
    let tokens;
    let token_nr;

    let itself = function () {
        return this;
    };

    // "The original_scope is the prototype for all scope objects. It contains
    // a define method that is used to define new variables in the scope.
    // The define method transforms a name token into a variable token.
    // It produces an error if the variable has already been defined in
    // the scope or if the name has already been used as a reserved word."

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
        // The find method is used to find the definition of a name. It starts with
        // the current scope and seeks, if necessary, back through the chain of parent
        // scopes and ultimately to the symbol table. It returns symbol_table["(name)"]
        // if it cannot find a definition.
        //
        // ...tests the values it finds to determine that they are not undefined
        // (which would indicate an undeclared name) and not a function (which would
        // indicate a collision with an inherited method).""
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
        // "The pop method closes a scope, giving focus back to the parent."
        pop: function () {
            scope = this.parent;
        },
        // "The reserve method is used to indicate that a name has been used
        // as a reserved word in the current scope.
        //
        // [I]n any function, any name may be used as a structure word or as a
        // variable, but not as both. We will reserve words locally only after
        // they are used as reserved words."
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

    // "[T]o establish a new scope for a function or a block we call the new_scope
    // function, which makes a new instance of the original scope prototype."

    let new_scope = function () {
        let s = scope;
        scope = Object.create(original_scope);
        scope.def = {};
        scope.parent = s;
        return scope;
    };

    // "The advance function makes a new token object from the next simple token
    // in the array and assigns it to the token variable. It can take an optional
    // id parameter which it can check against the id of the previous token.
    // The new token object's prototype is a (name) token in the current scope
    // or a symbol from the symbol table. The new token's arity is "name",
    // "literal", or "operator". Its arity may be changed later to "binary",
    // "unary", or "statement" when we know more about the token's role in
    // the program."

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

    // "Tokens are objects that bear methods allowing them to make
    // precedence decisions, match other tokens, and build trees.
    //
    // The heart of Pratt's technique is the expression function.
    // It takes a right binding power that controls how aggressively
    // it binds to tokens on its right.
    //
    // expression calls the nud method of the token. The nud is used
    // to process literals, variables, and prefix operators. Then as
    // long as the right binding power is less than the left binding
    // power of the next token, the led method is invoked on the
    // following token. The led is used to process infix and suffix
    // operators. This process can be recursive because the nud and
    // led methods can call expression."

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

    // "The statement function parses one statement. If the current token has
    // an std method, the token is reserved and the std is invoked.
    // Otherwise,we assume an expression statement terminated with a semi-colon.
    // For reliability, we will reject an expression statement that is not an
    // assignment or invocation."

    let statement = function () {
        let n = token,
            v;

        if (n.std) {
            advance();
            scope.reserve(n);
            return n.std();
        }
        v = expression(0);
        // In this parser, assignments and invocations are the only
        // expressions allowed in statement positions.
        if (!v.assignment && v.id !== "(") {
            v.error("Bad expression statement.");
        }
        advance(";");
        return v;
    };

    // "The statements function parses statements until it sees (end) or } which
    // signals the end of a block. The function returns a statement, an array
    // of statements, or null if there were no statements present."

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

    // "The block statement wraps a pair of curly braces around a list of
    // statements, giving them a new scope. (JavaScript does not have
    // block scope. Simplified JavaScript corrects that.)"

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

    // "[Makes] a symbol id and an optional binding power that defaults to 0
    // and returns a symbol object for that id. If the symbol already exists
    // in the symbol_table, the function returns that symbol object.
    // Otherwise, it makes a new symbol object that inherits from the
    // original_symbol, stores it in the symbol table, and returns it."

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

    // "The constant function builds constants into the language.
    // The nud mutates a name token into a literal token."

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

    // "[An infix operator] has a led method that weaves the token object
    // into a tree whose two branches (first and second) are the operand
    // to the left of the [operator] and the operand to the right. The left 
    // operand is passed into the led, which then obtains the right operand
    // by calling the expression function."

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

    // "We could use infixr to define our assignment operators, but we will make
    // a specialized assignment function because we want it to do two extra bits
    // of business: examine the left operand to make sure that it is a proper
    // lvalue, and set an assignment member so that we can later quickly identify
    // assignment statements."

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

    // "The code we used for right associative infix operators can be adapted
    // for prefix operators. Prefix operators are right associative. A prefix
    // does not have a left binding power because it does not bind to the left.
    // Prefix operators can also sometimes be reserved words."

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

    // "The stmt function is used to add statement symbols to the symbol table.
    // It takes a statement id and an std function."

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

    // "Functions are executable object values. A function has an optional name
    // (so that it can call itself recursively), a list of parameter names
    // wrapped in parens, and a body that is a list of statements wrapped
    // in curly braces. A function has its own scope."

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

    // "An array literal is a set of square brackets around zero or more
    // comma-separated expressions. Each of the expressions is evaluated,
    // and the results are collected into a new array."

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

    // "An object literal is a set of curly braces around zero or more
    // comma-separated pairs. A pair is a key/expression pair separated
    // by a colon (:). The key is a literal or a name which is treated
    // as a literal."

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
