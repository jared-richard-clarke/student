// operator(function, number) -> function(...number) -> number
// Produces an arithmetic function that can operate on a variable number of operands.
// const add = operator((x, y) => x + y, 0) -> add(1, 2, 3, 4) -> 10

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

const add = operator((x, y) => x + y, 0);
const sub = operator((x, y) => x - y, 0);
const mul = operator((x, y) => x * y, 1);
const div = operator((x, y) => x / y, 1);
