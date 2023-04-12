// pipe(any, ...function) -> any
// Transforms a value through a series of single-argument functions.
// Evaluates left to right.
// pipe("  hEllO, worLd   ", (text) => text.toLowerCase(), (text) => text.trim()) -> "hello, world"

function pipe(input, ...actions) {
    return actions.reduce(
        (accum, action) => action(accum),
        input
    );
}

// compose(...function) -> function(any) -> any
// Composes a series of single-argument functions into a single function expression.
// Evaluates right to left.
// const edit = compose((text) => text.toLowerCase(), (text) => text.trim()) -> edit("  hEllO, worLd   ") -> "hello, world"

function compose(...actions) {
    return function (input) {
        return actions.reduceRight(
            (accum, action) => action(accum), 
            input
        );
    };
}
