// memoize (function) -> function -> value
// Wraps function in a function that stores previously-computed values. Eliminates redundant computation.
// Works only with pure functions. Mutable values cannot be cached.
// const add = memoize((x, y) => x + y)
// add(1, 2) -> caches then returns 7

function memoize(func) {
    const cache = Object.create(null);
    return function(...args) {
        if (args in cache) {
            return cache[args];
        } else {
            const result = func(...args);
            cache[args] = result;
            return result;
        }
    }
}
