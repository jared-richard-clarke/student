// pipe(value, ...function) -> value
// Transforms a value through a series of single-argument functions.
// pipe("  hEllO, worLd   ", (text) => text.toLowerCase(), (text) => text.trim()) -> "hello, world"

function pipe(input, ...actions) {
    return actions.reduce(
        (accum, action) => action(accum),
        input
    );
}

// compose(...function) -> function -> value
// Composes a series of single-argument functions into a single function expression.
// const edit = compose((text) => text.toLowerCase(), (text) => text.trim()) -> edit("  hEllO, worLd   ") -> "hello, world"

function compose(...actions) {
    return function (input) {
        return actions.reduce(
            (accum, action) => action(accum), 
            input
        );
    };
}
