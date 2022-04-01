// pipe(...function) -> function -> value
// Composes a series of pure functions into a single function expression.
// Functions must take the same number of arguments as the values returned by the previous function.
// const edit = pipe((text) => text.toLowerCase(), (text) => text.trim()) -> edit("  hEllO, worLd   ") -> "hello, world"

function pipe(...actions) {
    return function (input) {
        return actions.reduce(
            (accum, action) => action(accum), 
            input
        );
    };
}
