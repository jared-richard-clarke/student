// partial(function, ...expr) -> function(...expr) -> value
// Fixes a number of arguments to a function, producing another function with a smaller arity.
// Applies function to combined arguments.
// const add3 = partial(sum, 1, 2) -> add3(2, 2) -> 7

function partial(fn, ...x) {
    return function (...y) {
        return fn(...x, ...y);
    };
}

// Curried functions using arrow notation.
const f2 = (fn) => (x) => (y) => fn(x, y)
const f3 = (fn) => (x) => (y) => (z) => fn(x, y, z);
