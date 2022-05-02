// pipe(value, ...function) -> value
// Transforms a value through a series of single-argument functions.
// pipe("  hEllO, worLd   ", (text) => text.toLowerCase(), (text) => text.trim()) -> "hello, world"

function pipe(input, ...actions) {
    return actions.reduce(
        (accum, action) => action(accum),
        input
    );
}

// compose_pipe(...function) -> function -> value
// Composes a series of single-argument functions into a single function expression.
// const edit = compose_pipe((text) => text.toLowerCase(), (text) => text.trim()) -> edit("  hEllO, worLd   ") -> "hello, world"

function compose_pipe(...actions) {
    return function (input) {
        return actions.reduce(
            (accum, action) => action(accum), 
            input
        );
    };
}
