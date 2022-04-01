// type_check(string, function) -> function -> value or type error
// Wraps single-argument function in a function that checks the type of its argument.
// const add1 = type_check("number", (value) => value + 1) -> add1(10) -> 11

function type_check(type, action) {
    return function (value) {
        if (typeof value !== type) {
            throw new TypeError("Argument must be of type " + type);
        }
        return action(value);
    };
}
