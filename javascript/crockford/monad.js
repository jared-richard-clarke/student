// A monad factory function as implemented by Douglas Crockford for his
// lecture [Monads and Gonads](https://www.youtube.com/watch?v=b0EF0VTs9Dc)

function MONAD(modifier) {
    const prototype = Object.create(null);
    function unit(value) {
        const monad = Object.create(prototype);
        monad.bind = function (func, args) {
            return func(value, ...args);
        };
        if (typeof modifier === "function") {
            modifier(monad, value);
        }
        return monad;
    }
    return unit;
}

const maybe = MONAD(function (monad, value) {
    if (value === null || value === undefined) {
        monad.is_null = true;
        monad.bind = function () {
            return monad;
        };
    }
});

const monad = maybe(null);
