// My interpreter for the Egg programming language.
// Egg was created by Marijn Haverbeke for "Eloquent Javascript" (https://eloquentjavascript.net/12_language.html) 
// This implmentation closely follows Haverbeke's original implementation. However,
// I made my code more modular. I also added extensive inline documentation.

// interpret(string) -> number | string | boolean | void
// parses and evaluates template string as Egg program.
// interpret(`+(1, 2)`) -> 3
const interpret = (function () {
    // parse(string) -> object
    // parse() wraps parseExpression and parseApply — mutually-recursive functions that parse
    // nested expressions and procedure applications. parse() consumes program string, returns syntax object.
    // parse(`+(1, 2)`) ->
    // { type: "apply",
    //   operator: { type: "word", name: "+" },
    //   args: [ {type: "number", value: 1 },
    //           {type: "number", value: 2 } ]
    // }
    const parse = (function () {
        // trim(string) -> string
        // helper function: removes whitespace from the beginning string.
        // trim(`  +(1, 2)`) -> "+(1, 2)"
        function trim(program) {
            const firstChar = program.search(/\S/);
            if (firstChar === -1) {
                program = "";
            } else {
                program = program.slice(firstChar);
            }
            return program;
        }
        function parseExpression(program) {
            program = trim(program);
            let expr, val, match;
            match = /^"([^"]*)"/.exec(program);
            if (match) {
                val = match[1];
                expr = {
                    type: "string",
                    value: val,
                };
                return parseApply(expr, program.slice(match[0].length));
            }
            match = /^\d+\b/.exec(program);
            if (match) {
                val = match[0];
                expr = {
                    type: "number",
                    value: val,
                };
                return parseApply(expr, program.slice(match[0].length));
            }
            match = /^[^\s(),"]+/.exec(program);
            if (match) {
                val = match[0];
                expr = {
                    type: "word",
                    value: val,
                };
                return parseApply(expr, program.slice(match[0].length));
            }
            throw new SyntaxError("Unexpected syntax: " + program);
        }

        function parseApply(expr, program) {
            program = trim(program);
            if (program[0] !== "(") {
                return {
                    expr: expr,
                    rest: program,
                };
            }
            program = trim(program.slice(1));
            expr = {
                type: "apply",
                operator: expr,
                args: [],
            };
            while (program[0] !== ")") {
                let arg = parseExpression(program);
                expr.args.push(arg.expr);
                program = trim(arg.rest);
                if (program[0] === ",") {
                    program = trim(program.slice(1));
                } else if (program[0] !== ")") {
                    throw new SyntaxError("Expected ',' or ')'");
                }
            }
            return parseApply(expr, program.slice(1));
        }
        // === parse: interface ===
        return function (program) {
            const result = parseExpression(program);
            if (trim(result.rest).length > 0) {
                throw new SyntaxError("Unexpected text after program");
            }
            return result.expr;
        };
    })();

    // evaluate(object, object) -> number | string | boolean | void
    // evaluates syntax and environment objects and returns value.
    // evaluate({ type: "number", value: 11 }, env) -> 11
    function evaluate(expr, env) {
        switch (expr.type) {
            case "string":
                return expr.value;
            case "number":
                return Number(expr.value);
            case "word":
                if (expr.value in env) {
                    return env[expr.value];
                } else {
                    throw new ReferenceError(
                        "Undefined variable: " + expr.value
                    );
                }
            case "apply":
                if (
                    expr.operator.type == "word" &&
                    expr.operator.value in specials
                ) {
                    return specials[expr.operator.value](expr.args, env);
                }
                let operator = evaluate(expr.operator, env);
                if (typeof operator !== "function") {
                    throw new TypeError("Applying a non-function");
                }
                return operator.apply(
                    null,
                    expr.args.map((arg) => {
                        return evaluate(arg, env);
                    })
                );
        }
    }
    // === special forms ===
    const specials = Object.create(null);

    specials["if"] = function (args, env) {
        if (args.length !== 3) {
            throw new SyntaxError("Bad number of args to if");
        }
        if (evaluate(args[0], env) !== false) {
            return evaluate(args[1], env);
        } else {
            return evaluate(args[2], env);
        }
    };
    specials["while"] = function (args, env) {
        if (args.length !== 2) {
            throw new SyntaxError("Bad number of args to while");
        }
        while (evaluate(args[0], env) !== false) {
            evaluate(args[1], env);
        }
        return false;
    };
    specials["do"] = function (args, env) {
        let value = false;
        args.forEach((arg) => {
            value = evaluate(arg, env);
        });
        return value;
    };
    specials["define"] = function (args, env) {
        if (args.length !== 2 || args[0].type !== "word") {
            throw new SyntaxError("Bad use of define");
        }
        let value = evaluate(args[1], env);
        env[args[0].value] = value;
        return value;
    };
    specials["func"] = function (args, env) {
        if (!args.length) {
            throw new SyntaxError("Functions need a body");
        }
        let argNames = args.slice(0, args.length - 1).map((expr) => {
            if (expr.type !== "word") {
                throw new SyntaxError("Arg names must be a word");
            }
            return expr.value;
        });
        let body = args[args.length - 1];

        return function () {
            if (arguments.length !== argNames.length) {
                throw new TypeError("Wrong number of arguments");
            }
            const localEnv = Object.create(env);
            for (let i = 0; i < arguments.length; i += 1) {
                localEnv[argNames[i]] = arguments[i];
            }
            return evaluate(body, localEnv);
        };
    };
    specials["array"] = function (args) {
        let arr = [];
        args.forEach((arg) => {
            arr.push(arg.value);
        });
        return arr;
    };

    // === top-level environment ===
    const env = Object.create(null);
    env["true"] = true;
    env["false"] = false;
    ["+", "-", "/", "*", "%", "===", "!==", ">", "<", ">=", "<="].forEach(
        function (operator) {
            env[operator] = new Function(
                "x",
                "y",
                "return x " + operator + " y;"
            );
        }
    );
    env["print"] = function (value) {
        console.log(value);
        return value;
    };
    env["length"] = function (arg) {
        return arg.length;
    };
    env["nth"] = function (i, arg) {
        return arg[i];
    };
    // === interpret: interface ===
    function interpreter(program) {
        return evaluate(parse(program), env);
    }
    return interpreter;
})();
