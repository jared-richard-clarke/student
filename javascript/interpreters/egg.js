// My interpreter for the Egg programming language.
// Egg was created by Marijn Haverbeke for "Eloquent Javascript" (https://eloquentjavascript.net/12_language.html)
// This implmentation closely follows Haverbeke's original implementation.
// I made my code more modular. I also added extensive inline documentation.

// interpret(string) -> number | string | boolean | void
// parses and evaluates template string as Egg program.
// interpret(`+(1, 2)`) -> 3
const interpret = (function () {
    // parse(string) -> object
    // wraps parse_expression and parse_application — mutually-recursive functions that parse
    // nested expressions and procedure applications.
    // parse(`+(1, 2)`) ->
    // { 
    //   expr: { 
    //     type: "apply",
    //     operator: { type: "word", value: "+" },
    //     args: [ { type: "number", value: 1 },
    //             { type: "number", value: 2 } ]
    //     },
    //   rest: ""
    // }
    const parse = (function () {
        // trim(string) -> string
        // helper function: removes whitespace from the beginning string.
        // trim(`  +(1, 2)`) -> "+(1, 2)"
        function trim(program) {
            const first_char = program.search(/\S/);
            if (first_char === -1) {
                program = "";
            } else {
                program = program.slice(first_char);
            }
            return program;
        }
        function parse_expression(program) {
            program = trim(program);
            let expr, val, match;
            match = /^"([^"]*)"/.exec(program);
            if (match) {
                val = match[1];
                expr = {
                    type: "string",
                    value: val,
                };
                return parse_application(expr, program.slice(match[0].length));
            }
            match = /^\d+\b/.exec(program);
            if (match) {
                val = match[0];
                expr = {
                    type: "number",
                    value: val,
                };
                return parse_application(expr, program.slice(match[0].length));
            }
            match = /^[^\s(),"]+/.exec(program);
            if (match) {
                val = match[0];
                expr = {
                    type: "word",
                    value: val,
                };
                return parse_application(expr, program.slice(match[0].length));
            }
            throw new SyntaxError("Unexpected syntax: " + program);
        }

        function parse_application(expr, program) {
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
                const arg = parse_expression(program);
                expr.args.push(arg.expr);
                program = trim(arg.rest);
                if (program[0] === ",") {
                    program = trim(program.slice(1));
                } else if (program[0] !== ")") {
                    throw new SyntaxError("Expected ',' or ')'");
                }
            }
            return parse_application(expr, program.slice(1));
        }
        // === parse: interface ===
        return function (program) {
            const result = parse_expression(program);
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
                const { operator, args } = expr;
                if (
                    operator.type == "word" &&
                    operator.value in special_forms
                ) {
                    return special_forms[operator.value](args, env);
                }
                const operation = evaluate(operator, env);
                if (typeof operation !== "function") {
                    throw new TypeError("Applying a non-function");
                }
                return operation(
                    ...args.map(function (arg) {
                        return evaluate(arg, env);
                    })
                );
        }
    }
    // === special forms ===
    const special_forms = Object.create(null);

    special_forms["if"] = function (args, env) {
        if (args.length !== 3) {
            throw new SyntaxError("Incorrect number of args to if");
        }
        if (evaluate(args[0], env) !== false) {
            return evaluate(args[1], env);
        } else {
            return evaluate(args[2], env);
        }
    };
    special_forms["while"] = function (args, env) {
        if (args.length !== 2) {
            throw new SyntaxError("Incorrect number of args to while");
        }
        while (evaluate(args[0], env) !== false) {
            evaluate(args[1], env);
        }
        return false;
    };
    special_forms["begin"] = function (args, env) {
        let value = false;
        args.forEach((arg) => {
            value = evaluate(arg, env);
        });
        return value;
    };
    special_forms["define"] = function (args, env) {
        if (args.length !== 2 || args[0].type !== "word") {
            throw new SyntaxError("Incorrect use of define");
        }
        let value = evaluate(args[1], env);
        env[args[0].value] = value;
        return value;
    };
    special_forms["fun"] = function (args, env) {
        if (!args.length) {
            throw new SyntaxError("Functions need a body");
        }
        const parameters = args.slice(0, args.length - 1).map(function (expr) {
            if (expr.type !== "word") {
                throw new SyntaxError("Parameter names must be words");
            }
            return expr.value;
        });
        const body = args[args.length - 1];

        return function () {
            if (arguments.length !== parameters.length) {
                throw new TypeError("Incorrect number of arguments");
            }
            const local_env = Object.create(env);
            for (let i = 0; i < arguments.length; i += 1) {
                local_env[parameters[i]] = arguments[i];
            }
            return evaluate(body, local_env);
        };
    };

    // === top-level environment ===
    const top_env = Object.create(null);
    top_env["true"] = true;
    top_env["false"] = false;
    ["+", "-", "/", "*", "%", "===", "!==", ">", "<", ">=", "<="].forEach(
        function (operator) {
            top_env[operator] = Function(
                "x",
                "y",
                "return x " + operator + " y;"
            );
        }
    );
    top_env["print"] = function (value) {
        console.log(value);
        return value;
    };

    // === interpret: interface ===
    function interpreter(program) {
        return evaluate(parse(program), top_env);
    }
    return interpreter;
})();
