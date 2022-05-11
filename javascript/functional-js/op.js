// module op: provides a set of variadic arithmetic functions.

function operator(operation, base) {
    return function (...numbers) {
        if (numbers.length === 0) {
            return base;
        } else {
            return numbers.reduce(
                (accum, number) => operation(accum, number)
            );
        }
    };
}

export default Object.freeze({
    add: operator((x, y) => x + y, 0),
    sub: operator((x, y) => x - y, 0),
    mul: operator((x, y) => x * y, 1),
    div: operator((x, y) => x / y, 1),
});
